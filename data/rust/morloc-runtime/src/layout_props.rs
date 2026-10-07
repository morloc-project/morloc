//! Randomized round trips of pointer-bearing values through every stream
//! read path.
//!
//! A value's voidstar layout places relative pointers in many positions:
//! string and array headers, optional slots, variant payload slots, bignum
//! limbs, path-form stream handles, and through recursive declarations. Each reader that copies a
//! stream sub-packet out of its file must relocate every one of them. This
//! module generates schemas across all of those kinds, generates values
//! biased toward the cases where a record owns no sub-allocation (empty
//! strings and arrays, absent optionals, nullary arms), writes them through
//! an OStream, reads them back through each reader, and compares the JSON
//! rendering against the original.
//!
//! The generator is a seeded xorshift, so a failure names its seed and
//! replays exactly. `MORLOC_LAYOUT_PROP_CASES` raises the case count and
//! `MORLOC_LAYOUT_PROP_SEED` runs one seed. A reader that dereferences a
//! stale pointer can crash the process rather than return an error, so each
//! case is announced on stderr before it runs (`--nocapture` shows it).

use crate::error::MorlocError;
use crate::intrinsics::IFileWalkArg;
use crate::json::{read_json_with_schema, voidstar_to_json_string};
use crate::shm;
use crate::stream::{
    shared_close_handle, shared_discard_handle, shared_flush_buffer,
    shared_ifile_walk, shared_load_stream_file_as_array, shared_next_frame,
    shared_open_ifile, shared_open_istream, shared_open_ostream_with_schema,
    shared_write_subpacket,
};
use morloc_runtime_types::schema::{parse_schema, Schema};

/// `deep_tests::Gen` with a scrambled seed (a zero state never leaves
/// zero) and the index-sized draws this generator uses.
struct Rng(crate::deep_tests::Gen);

impl Rng {
    fn new(seed: u64) -> Self {
        Rng(crate::deep_tests::Gen(seed.wrapping_mul(0x9E37_79B9_7F4A_7C15) | 1))
    }
    fn next(&mut self) -> u64 {
        self.0.next()
    }
    fn below(&mut self, n: usize) -> usize {
        self.0.below(n as u64) as usize
    }
    fn chance(&mut self, num: u64, den: u64) -> bool {
        self.0.below(den) < num
    }
}

/// A generated element type. `Rec` is a back-reference to the enclosing
/// recursive declaration, which is always a variant whose arm 0 is nullary.
#[derive(Clone)]
enum G {
    Bool,
    Sint(u8),
    Uint(u8),
    F32,
    F64,
    Big,
    Str,
    Handle,
    Enum(usize),
    Arr(Box<G>),
    Opt(Box<G>),
    Tup(Vec<G>),
    Record(Vec<G>),
    Var(Vec<Vec<G>>),
    RecVar(Vec<Vec<G>>),
    Rec,
}

const REC_NAME: &str = "Node";

fn gen_type(r: &mut Rng, depth: usize, in_rec: bool, rec_allowed: bool) -> G {
    if r.chance(1, 12) {
        return G::Handle;
    }
    let leaf = depth == 0;
    let pick = if leaf { r.below(8) } else { r.below(15) };
    match pick {
        0 => G::Bool,
        1 => G::Sint([1, 2, 4, 8][r.below(4)]),
        2 => G::Uint([1, 2, 4, 8][r.below(4)]),
        3 => G::F32,
        4 => G::F64,
        5 => G::Big,
        6 | 7 => G::Str,
        8 => G::Enum(1 + r.below(3)),
        9 => G::Arr(Box::new(gen_type(r, depth - 1, in_rec, rec_allowed))),
        10 => G::Opt(Box::new(gen_type(r, depth - 1, in_rec, rec_allowed))),
        11 => G::Tup((0..1 + r.below(3)).map(|_| gen_type(r, depth - 1, in_rec, rec_allowed)).collect()),
        12 => G::Record((0..1 + r.below(3)).map(|_| gen_type(r, depth - 1, in_rec, rec_allowed)).collect()),
        13 => G::Var(gen_arms(r, depth, in_rec, rec_allowed)),
        _ => {
            if rec_allowed && !in_rec {
                let mut arms = vec![Vec::new()];
                for _ in 0..1 + r.below(2) {
                    let mut fields: Vec<G> =
                        (0..r.below(3)).map(|_| gen_type(r, depth - 1, true, false)).collect();
                    let at = r.below(fields.len() + 1);
                    fields.insert(at, G::Rec);
                    arms.push(fields);
                }
                G::RecVar(arms)
            } else {
                G::Var(gen_arms(r, depth, in_rec, rec_allowed))
            }
        }
    }
}

fn gen_arms(r: &mut Rng, depth: usize, in_rec: bool, rec_allowed: bool) -> Vec<Vec<G>> {
    (0..1 + r.below(3))
        .map(|_| (0..r.below(3)).map(|_| gen_type(r, depth - 1, in_rec, rec_allowed)).collect())
        .collect()
}

fn schema_str(g: &G) -> String {
    match g {
        G::Bool => "b".into(),
        G::Sint(w) => format!("i{w}"),
        G::Uint(w) => format!("u{w}"),
        G::F32 => "f4".into(),
        G::F64 => "f8".into(),
        G::Big => "j".into(),
        G::Str => "s".into(),
        G::Handle => "F".into(),
        G::Enum(n) => {
            let mut s = format!("e{n}");
            for i in 0..*n {
                s += &format!("2E{i}");
            }
            s
        }
        G::Arr(t) => format!("a{}", schema_str(t)),
        G::Opt(t) => format!("?{}", schema_str(t)),
        G::Tup(ts) => {
            let mut s = format!("t{}", ts.len());
            for t in ts {
                s += &schema_str(t);
            }
            s
        }
        G::Record(ts) => {
            let mut s = format!("m{}", ts.len());
            for (i, t) in ts.iter().enumerate() {
                s += &format!("2k{i}{}", schema_str(t));
            }
            s
        }
        G::Var(arms) => variant_str(arms),
        G::RecVar(arms) => format!("&{}{}{}", REC_NAME.len(), REC_NAME, variant_str(arms)),
        G::Rec => format!("^{}{}", REC_NAME.len(), REC_NAME),
    }
}

fn variant_str(arms: &[Vec<G>]) -> String {
    let mut s = format!("v{}", arms.len());
    for (i, fields) in arms.iter().enumerate() {
        s += &format!("2A{i}{}", fields.len());
        for f in fields {
            s += &schema_str(f);
        }
    }
    s
}

/// Render a JSON value for `g`. `minimal` asks for the smallest value the
/// type has: no sub-allocation wherever the type allows one to be absent.
fn gen_value(r: &mut Rng, g: &G, rec: Option<&[Vec<G>]>, depth: usize, minimal: bool) -> String {
    match g {
        G::Bool => (if r.chance(1, 2) { "true" } else { "false" }).into(),
        G::Sint(w) => {
            let bound = 1i64 << (8 * *w as u32 - 2).min(40);
            format!("{}", (r.next() as i64).rem_euclid(2 * bound) - bound)
        }
        G::Uint(w) => format!("{}", r.next() % (1u64 << (8 * *w as u32 - 1).min(40))),
        G::F32 | G::F64 => format!("{}", (r.below(4000) as f64 - 2000.0) / 4.0),
        G::Big => {
            if minimal || r.chance(1, 2) {
                format!("{}", r.below(1000))
            } else {
                // Several limbs, so the value is held behind a pointer.
                let mut s = format!("{}", 1 + r.below(9));
                for _ in 0..30 + r.below(20) {
                    s.push(char::from(b'0' + r.below(10) as u8));
                }
                if r.chance(1, 2) { format!("-{s}") } else { s }
            }
        }
        G::Str => {
            if minimal {
                "\"\"".into()
            } else {
                let n = r.below(12);
                let s: String = (0..n).map(|_| char::from(b'a' + r.below(26) as u8)).collect();
                format!("\"{s}\"")
            }
        }
        G::Handle => {
            // An empty path has no path bytes; any other is laid out after
            // the field, 8-aligned.
            if minimal {
                "\"\"".into()
            } else {
                let n = 1 + r.below(20);
                let p: String = (0..n).map(|_| char::from(b'a' + r.below(26) as u8)).collect();
                format!("\"/tmp/{p}\"")
            }
        }
        G::Enum(n) => format!("\"E{}\"", r.below(*n)),
        G::Arr(t) => {
            let n = if minimal { 0 } else { r.below(4) };
            let items: Vec<String> =
                (0..n).map(|_| gen_value(r, t, rec, depth, false)).collect();
            format!("[{}]", items.join(","))
        }
        G::Opt(t) => {
            if minimal || r.chance(1, 3) {
                "null".into()
            } else {
                gen_value(r, t, rec, depth, false)
            }
        }
        G::Tup(ts) => {
            let items: Vec<String> =
                ts.iter().map(|t| gen_value(r, t, rec, depth, minimal)).collect();
            format!("[{}]", items.join(","))
        }
        G::Record(ts) => {
            let items: Vec<String> = ts
                .iter()
                .enumerate()
                .map(|(i, t)| format!("\"k{i}\":{}", gen_value(r, t, rec, depth, minimal)))
                .collect();
            format!("{{{}}}", items.join(","))
        }
        G::Var(arms) => arm_value(r, arms, rec, depth, minimal),
        G::RecVar(arms) => arm_value(r, arms, Some(arms), depth, minimal),
        G::Rec => {
            let arms = rec.expect("back-reference outside its declaration");
            arm_value(r, arms, rec, depth, minimal)
        }
    }
}

fn arm_value(
    r: &mut Rng,
    arms: &[Vec<G>],
    rec: Option<&[Vec<G>]>,
    depth: usize,
    minimal: bool,
) -> String {
    let nullary = arms.iter().position(|a| a.is_empty());
    let k = match (minimal || depth == 0, nullary) {
        (true, Some(k)) => k,
        _ => r.below(arms.len()),
    };
    let fields = &arms[k];
    if fields.is_empty() {
        return format!("\"A{k}\"");
    }
    let next = depth.saturating_sub(1);
    let items: Vec<String> =
        fields.iter().map(|f| gen_value(r, f, rec, next, minimal)).collect();
    format!("{{\"A{k}\":[{}]}}", items.join(","))
}

/// How the elements are written.
#[derive(Clone, Copy, Debug)]
enum Write {
    /// One `@write` at level 0: one uncompressed sub-packet.
    Plain,
    /// One `@write` at level 3: one compressed sub-packet.
    Compressed,
    /// Two `@write` calls with no flush: coalesced in the write buffer.
    Coalesced,
    /// Two `@write` calls with a flush between: two sub-packets.
    Flushed,
}

const WRITES: [Write; 4] = [Write::Plain, Write::Compressed, Write::Coalesced, Write::Flushed];

fn json_value(s: &str) -> serde_json::Value {
    serde_json::from_str(s).unwrap_or_else(|e| panic!("renderer produced bad JSON {s:?}: {e}"))
}

fn render_and_free(ptr: crate::shm_types::AbsPtr, schema: &Schema) -> Result<serde_json::Value, MorlocError> {
    let s = voidstar_to_json_string(ptr, schema);
    let _ = shm::shfree(ptr);
    Ok(json_value(&s?))
}


/// Write `halves` through `mode`, then check every reader. Returns one line
/// per mismatch or error.
fn check_case(
    path: &str,
    list_schema: &Schema,
    elem_schema: &Schema,
    list_schema_str: &str,
    halves: &[String; 2],
    mode: Write,
) -> Result<Vec<String>, MorlocError> {
    let mut bad = Vec::new();
    let _ = std::fs::remove_file(path);

    let parts: Vec<&String> = match mode {
        Write::Plain | Write::Compressed => vec![&halves[0]],
        Write::Coalesced | Write::Flushed => vec![&halves[0], &halves[1]],
    };
    let level = if matches!(mode, Write::Compressed) { 3 } else { 0 };

    // The expected value is the renderer's view of what was written, so the
    // comparison never depends on how JSON text spells a number.
    let mut expected: Vec<serde_json::Value> = Vec::new();
    let out = shared_open_ostream_with_schema(path, list_schema_str)?;
    for (i, part) in parts.iter().enumerate() {
        let ptr = read_json_with_schema(part, list_schema)?;
        match json_value(&voidstar_to_json_string(ptr, list_schema)?) {
            serde_json::Value::Array(xs) => expected.extend(xs),
            other => panic!("list rendered as {other}"),
        }
        shared_write_subpacket(out, crate::compression::CompressionLevel::from_u8(level)?, ptr)?;
        let _ = shm::shfree(ptr);
        if matches!(mode, Write::Flushed) && i == 0 {
            shared_flush_buffer(out)?;
        }
    }
    shared_close_handle(out)?;
    let n = expected.len();

    // IStream: drain every frame.
    let mut got = Vec::new();
    let reader = shared_open_istream(path)?;
    let drained = (|| -> Result<(), MorlocError> {
        while let Some(frame) = shared_next_frame(reader)? {
            match render_and_free(frame, list_schema)? {
                serde_json::Value::Array(xs) => got.extend(xs),
                other => panic!("frame rendered as {other}"),
            }
        }
        Ok(())
    })();
    let _ = shared_discard_handle(reader);
    match drained {
        Ok(()) if got == expected => {}
        Ok(()) => bad.push(format!("{mode:?} @next: got {got:?}")),
        Err(e) => bad.push(format!("{mode:?} @next: error {e}")),
    }

    // @load of the whole stream file.
    match shared_load_stream_file_as_array(path, Some(list_schema)).and_then(|p| render_and_free(p, list_schema)) {
        Ok(serde_json::Value::Array(xs)) if xs == expected => {}
        Ok(v) => bad.push(format!("{mode:?} @load: got {v}")),
        Err(e) => bad.push(format!("{mode:?} @load: error {e}")),
    }

    // IFile: every index and every forward slice.
    let file = shared_open_ifile(path)?;
    for i in 0..n {
        let r = shared_ifile_walk(file, ".[]", &[IFileWalkArg::opt(Some(i as i64))])
            .and_then(|p| render_and_free(p, elem_schema));
        match r {
            Ok(v) if v == expected[i] => {}
            Ok(v) => bad.push(format!("{mode:?} .[{i}]: got {v}")),
            Err(e) => bad.push(format!("{mode:?} .[{i}]: error {e}")),
        }
    }
    for i in 0..n {
        for j in i + 1..=n {
            let args = [IFileWalkArg::opt(Some(i as i64)), IFileWalkArg::opt(Some(j as i64)), IFileWalkArg::opt(None)];
            let r = shared_ifile_walk(file, ".[:]", &args)
                .and_then(|p| render_and_free(p, list_schema));
            match r {
                Ok(serde_json::Value::Array(xs)) if xs[..] == expected[i..j] => {}
                Ok(v) => bad.push(format!("{mode:?} .[{i}:{j}]: got {v}")),
                Err(e) => bad.push(format!("{mode:?} .[{i}:{j}]: error {e}")),
            }
        }
    }
    let _ = shared_discard_handle(file);
    let _ = std::fs::remove_file(path);
    Ok(bad)
}

fn gen_list(r: &mut Rng, g: &G, empty_first: bool) -> String {
    let n = 1 + r.below(4);
    let items: Vec<String> = (0..n)
        .map(|k| gen_value(r, g, None, 3, empty_first && k == 0))
        .collect();
    format!("[{}]", items.join(","))
}

/// Run every write mode for one seed; returns one line per failure.
fn run_seed(seed: u64, path: &str) -> Vec<String> {
    let mut r = Rng::new(seed);
    let g = gen_type(&mut r, 3, false, true);
    let elem_str = schema_str(&g);
    let list_str = format!("a{elem_str}");
    let elem_schema = parse_schema(&elem_str)
        .unwrap_or_else(|e| panic!("seed {seed}: generated bad schema {elem_str}: {e}"));
    let list_schema = parse_schema(&list_str).unwrap();
    let empty_first = seed % 2 == 0;
    let halves = [gen_list(&mut r, &g, empty_first), gen_list(&mut r, &g, empty_first)];
    let mut failures = Vec::new();
    for mode in WRITES {
        eprintln!("layout_props: seed {seed} {mode:?} schema {elem_str} values {halves:?}");
        match check_case(path, &list_schema, &elem_schema, &list_str, &halves, mode) {
            Ok(bad) => {
                for b in bad {
                    failures.push(format!("seed {seed} schema {elem_str} values {halves:?}: {b}"));
                }
            }
            Err(e) => failures.push(format!(
                "seed {seed} schema {elem_str} values {halves:?}: {mode:?} write failed: {e}"
            )),
        }
    }
    failures
}

const SEED_ENV: &str = "MORLOC_LAYOUT_PROP_SEED";

/// A child that crashes never tears down its test arena, so the parent
/// removes the child's SHM volumes and its file-backed fallback directory.
fn remove_child_arena(pid: u32) {
    let tmp = std::env::temp_dir();
    crate::remove_marked_dir(&tmp.join(format!("morloc_test_{pid}")));
    crate::remove_marked_dir(&tmp.join(format!("morloc_layout_props_{pid}")));
}

/// One seed, run in its own process by `stream_readers_preserve_pointer_layouts`.
/// Ignored when run directly without a seed.
#[test]
#[ignore]
fn layout_props_one_seed() {
    let Some(seed) = std::env::var(SEED_ENV).ok().and_then(|s| s.parse::<u64>().ok()) else {
        return;
    };
    let _shm = crate::own_test_registry();
    let dir = std::env::temp_dir().join(format!("morloc_layout_props_{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("case.stream");
    let failures = run_seed(seed, path.to_str().unwrap());
    let _ = std::fs::remove_dir_all(&dir);
    for f in &failures {
        println!("LAYOUT_FAIL {f}");
    }
    assert!(failures.is_empty(), "seed {seed}: {} failures", failures.len());
}

/// Every generated shape must survive every writer and every reader. Each
/// seed runs in a child process so a reader that crashes on a stale pointer
/// is reported as a failure of that seed instead of ending the run.
#[test]
fn stream_readers_preserve_pointer_layouts() {
    let cases: u64 = std::env::var("MORLOC_LAYOUT_PROP_CASES")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(200);
    let exe = std::env::current_exe().unwrap();
    let mut failures = Vec::new();
    for seed in 0..cases {
        let child = std::process::Command::new(&exe)
            .args(["--exact", "layout_props::layout_props_one_seed", "--ignored", "--nocapture", "--test-threads=1"])
            .env(SEED_ENV, seed.to_string())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .spawn()
            .expect("spawn test child");
        let pid = child.id();
        let out = child.wait_with_output().expect("wait for test child");
        remove_child_arena(pid);
        if out.status.success() {
            continue;
        }
        let stdout = String::from_utf8_lossy(&out.stdout);
        // libtest's "test <name> ... " prefix has no newline, so a report
        // can start mid-line.
        let lines: Vec<&str> =
            stdout.lines().filter_map(|l| l.find("LAYOUT_FAIL ").map(|at| &l[at..])).collect();
        if lines.is_empty() {
            let stderr = String::from_utf8_lossy(&out.stderr);
            let last = stderr.lines().filter(|l| l.starts_with("layout_props: ")).last().unwrap_or("");
            failures.push(format!("seed {seed}: child died ({}) during: {last}", out.status));
        } else {
            failures.extend(lines.iter().map(|l| l.to_string()));
        }
    }
    assert!(
        failures.is_empty(),
        "{} failures over {} seeds; first 25:\n{}",
        failures.len(),
        cases,
        failures.iter().take(25).cloned().collect::<Vec<_>>().join("\n"),
    );
}

/// Writes that overflow the write buffer many times over, read back
/// across sub-packet boundaries. A 4 KiB buffer (the minimum) splits a
/// few thousand elements into many sub-packets, so a slice spans several.
#[test]
fn many_subpacket_round_trips() {
    let _shm = crate::own_test_registry();
    let saved = std::env::var("MORLOC_WRITE_BUFFER_BYTES").ok();
    std::env::set_var("MORLOC_WRITE_BUFFER_BYTES", "4096");
    let dir = std::env::temp_dir().join(format!("morloc_layout_many_{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("many.stream");
    let path = path.to_str().unwrap();

    let cases: [(&str, Box<dyn Fn(usize) -> String>); 3] = [
        ("i2", Box::new(|k| format!("{}", (k as i64 * 7919) % 30000 - 15000))),
        ("t2u1f8", Box::new(|k| format!("[{},{}.5]", k % 256, k))),
        ("s", Box::new(|k| if k % 5 == 0 { "\"\"".into() } else { format!("\"s{k}\"") })),
    ];
    let mut failures = Vec::new();
    for (elem, item) in &cases {
        let list_str = format!("a{elem}");
        let list = parse_schema(&list_str).unwrap();
        let elem_schema = parse_schema(elem).unwrap();
        let part = |a: usize, b: usize| {
            format!("[{}]", (a..b).map(|k| item(k)).collect::<Vec<_>>().join(","))
        };
        // Two writes, the second continuing into a partly filled buffer.
        let (n1, n2) = (6000usize, 1234usize);
        let _ = std::fs::remove_file(path);
        let out = shared_open_ostream_with_schema(path, &list_str).unwrap();
        let mut expected = Vec::new();
        for (a, b) in [(0, n1), (n1, n1 + n2)] {
            let ptr = read_json_with_schema(&part(a, b), &list).unwrap();
            match json_value(&voidstar_to_json_string(ptr, &list).unwrap()) {
                serde_json::Value::Array(xs) => expected.extend(xs),
                v => panic!("list rendered as {v}"),
            }
            shared_write_subpacket(out, crate::compression::CompressionLevel::NONE, ptr).unwrap();
            let _ = shm::shfree(ptr);
        }
        shared_close_handle(out).unwrap();
        let n = expected.len();

        let r = shared_open_istream(path).unwrap();
        let (mut frames, mut got) = (0, Vec::new());
        while let Some(p) = shared_next_frame(r).unwrap() {
            frames += 1;
            match render_and_free(p, &list).unwrap() {
                serde_json::Value::Array(xs) => got.extend(xs),
                v => panic!("frame rendered as {v}"),
            }
        }
        let _ = shared_discard_handle(r);
        if frames < 4 {
            failures.push(format!("{elem}: only {frames} sub-packets; the test needs many"));
        }
        if got != expected {
            failures.push(format!("{elem}: @next differs"));
        }
        match shared_load_stream_file_as_array(path, Some(&list)).and_then(|p| render_and_free(p, &list)) {
            Ok(serde_json::Value::Array(xs)) if xs == expected => {}
            other => failures.push(format!("{elem}: @load differs: {:?}", other.map(|_| ()))),
        }
        let f = shared_open_ifile(path).unwrap();
        for (i, j) in [(0, n), (1, n - 1), (n / 3, 2 * n / 3), (4000, 4001), (n - 7, n)] {
            let args = [IFileWalkArg::opt(Some(i as i64)), IFileWalkArg::opt(Some(j as i64)), IFileWalkArg::opt(None)];
            match shared_ifile_walk(f, ".[:]", &args).and_then(|p| render_and_free(p, &list)) {
                Ok(serde_json::Value::Array(xs)) if xs[..] == expected[i..j] => {}
                Ok(_) => failures.push(format!("{elem}: .[{i}:{j}] differs")),
                Err(e) => failures.push(format!("{elem}: .[{i}:{j}] error {e}")),
            }
        }
        match shared_ifile_walk(f, ".[]", &[IFileWalkArg::opt(Some(n as i64 - 1))])
            .and_then(|p| render_and_free(p, &elem_schema))
        {
            Ok(v) if v == expected[n - 1] => {}
            other => failures.push(format!("{elem}: last index differs: {other:?}")),
        }
        let _ = shared_discard_handle(f);
    }
    match saved {
        Some(v) => std::env::set_var("MORLOC_WRITE_BUFFER_BYTES", v),
        None => std::env::remove_var("MORLOC_WRITE_BUFFER_BYTES"),
    }
    let _ = std::fs::remove_dir_all(&dir);
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}

/// Handle-form stream fields written to a stream land in path form, each
/// path among its own element's bytes: a one-element slice then copies a
/// few bytes, not every path that follows it in the sub-packet.
#[test]
fn handle_paths_are_written_in_place() {
    use morloc_runtime_types::stream_handle as sh;
    let _shm = crate::own_test_registry();
    let dir = std::env::temp_dir().join(format!("morloc_layout_handles_{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();

    // Two small stream files for the handles to name.
    let targets: Vec<String> = (0..2).map(|k| dir.join(format!("t{k}.stream")).to_str().unwrap().to_string()).collect();
    for t in &targets {
        let ints = parse_schema("ai8").unwrap();
        let v = read_json_with_schema("[1,2,3]", &ints).unwrap();
        let h = shared_open_ostream_with_schema(t, "ai8").unwrap();
        shared_write_subpacket(h, crate::compression::CompressionLevel::NONE, v).unwrap();
        shared_close_handle(h).unwrap();
        let _ = shm::shfree(v);
    }
    let handles: Vec<i64> = targets.iter().map(|t| shared_open_ifile(t).unwrap()).collect();

    // 200 records (name, handle); the handles go in by slot id.
    let n = 200usize;
    let list = parse_schema("at2sF").unwrap();
    let elem = parse_schema("t2sF").unwrap();
    let json = format!("[{}]", (0..n).map(|k| format!(r#"["n{k}", "{}"]"#, targets[k % 2])).collect::<Vec<_>>().join(","));
    let value = read_json_with_schema(&json, &list).unwrap();
    unsafe {
        let arr = &*(value as *const crate::shm_types::Array);
        let data = shm::rel2abs(arr.data).unwrap();
        for k in 0..n {
            sh::write_field(data.add(k * elem.width + elem.offsets[1]), sh::TAG_HANDLE, sh::handle_payload(handles[k % 2]));
        }
    }
    let path = dir.join("records.stream");
    let path = path.to_str().unwrap();
    let out = shared_open_ostream_with_schema(path, "at2sF").unwrap();
    shared_write_subpacket(out, crate::compression::CompressionLevel::NONE, value).unwrap();
    shared_close_handle(out).unwrap();
    let _ = shm::shfree(value);

    let f = shared_open_ifile(path).unwrap();
    for k in [0usize, 1, 100, n - 1] {
        let args = [IFileWalkArg::opt(Some(k as i64)), IFileWalkArg::opt(Some(k as i64 + 1)), IFileWalkArg::opt(None)];
        let p = shared_ifile_walk(f, ".[:]", &args).unwrap();
        let size = unsafe { shm::shm_block_size(p) }.unwrap();
        let got = render_and_free(p, &list).unwrap();
        let want = json_value(&format!(r#"[["n{k}", "{}"]]"#, targets[k % 2]));
        assert_eq!(got, want, "element {k}");
        assert!(size < 512, "a one-element slice copied a {size}-byte block");
    }
    let _ = shared_discard_handle(f);
    for h in handles {
        let _ = shared_discard_handle(h);
    }
    let _ = std::fs::remove_dir_all(&dir);
}
