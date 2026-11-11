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
        G::Handle => "F2aj".into(),
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
    crate::stream::set_test_write_buffer_bytes(Some(4096));
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
    crate::stream::set_test_write_buffer_bytes(None);
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
    let list = parse_schema("at2sF3ai8").unwrap();
    let elem = parse_schema("t2sF3ai8").unwrap();
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
    let out = shared_open_ostream_with_schema(path, "at2sF3ai8").unwrap();
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

/// Records `(name, handle)` with names of `name_len(k)` bytes; element
/// `k` names `handles[k % len]`, and the element `bad`, if any, names
/// `closed`.
fn handle_records(n: usize, name_len: impl Fn(usize) -> usize, targets: &[String], handles: &[i64], bad: Option<(usize, i64)>) -> crate::shm_types::AbsPtr {
    use morloc_runtime_types::stream_handle as sh;
    let list = parse_schema("at2sF3ai8").unwrap();
    let elem = parse_schema("t2sF3ai8").unwrap();
    let json = format!(
        "[{}]",
        (0..n).map(|k| format!(r#"["{}", "{}"]"#, "n".repeat(name_len(k)), targets[k % targets.len()])).collect::<Vec<_>>().join(",")
    );
    let value = read_json_with_schema(&json, &list).unwrap();
    unsafe {
        let arr = &*(value as *const crate::shm_types::Array);
        let data = shm::rel2abs(arr.data).unwrap();
        for k in 0..n {
            let h = match bad {
                Some((at, closed)) if at == k => closed,
                _ => handles[k % handles.len()],
            };
            sh::write_field(data.add(k * elem.width + elem.offsets[1]), sh::TAG_HANDLE, sh::handle_payload(h));
        }
    }
    value
}

fn refused_for_its_handle(r: Result<(), MorlocError>, k: usize) {
    match r {
        Err(e) if e.to_string().contains("rewrite handle") => {}
        r => panic!("element {k} names a closed stream, yet the write gave {r:?}"),
    }
}

/// A `@write` batch is whole or absent: when one element names a closed
/// stream, no element of the batch reaches the file or the channel, though
/// the batch spans several sub-packets and holds an oversize element.
#[test]
fn a_batch_with_a_failing_element_leaves_no_trace() {
    let _shm = crate::own_test_registry();
    let dir = std::env::temp_dir().join(format!("morloc_layout_whole_batch_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    let _restore = buffer_bytes(4096);

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
    let closed = shared_open_ifile(&targets[0]).unwrap();
    shared_discard_handle(closed).unwrap();

    let n = 21usize;
    let name_len = |k: usize| if k == 12 { 5000 } else { 400 };
    let level = crate::compression::CompressionLevel::NONE;
    let list = parse_schema("at2sF3ai8").unwrap();
    let write = |path: &str, failing: Option<usize>| -> (usize, serde_json::Value) {
        let out = shared_open_ostream_with_schema(path, "at2sF3ai8").unwrap();
        let before = handle_records(3, |_| 400, &targets, &handles, None);
        shared_write_subpacket(out, level, before).unwrap();
        if let Some(k) = failing {
            let batch = handle_records(n, name_len, &targets, &handles, Some((k, closed)));
            refused_for_its_handle(shared_write_subpacket(out, level, batch), k);
            let _ = shm::shfree(batch);
        }
        let after = handle_records(2, |_| 400, &targets, &handles, None);
        shared_write_subpacket(out, level, after).unwrap();
        shared_close_handle(out).unwrap();
        let _ = shm::shfree(before);
        let _ = shm::shfree(after);
        let len = std::fs::metadata(path).unwrap().len() as usize;
        (len, render_and_free(shared_load_stream_file_as_array(path, Some(&list)).unwrap(), &list).unwrap())
    };
    let reference = write(dir.join("ref.stream").to_str().unwrap(), None);
    let mut failures = Vec::new();
    for k in 0..n {
        let got = write(dir.join(format!("bad{k}.stream")).to_str().unwrap(), Some(k));
        if got != reference {
            failures.push(format!("file: element {k} failing leaves {} bytes and {} elements, want {} and {}",
                got.0, got.1.as_array().map_or(0, |a| a.len()), reference.0, reference.1.as_array().map_or(0, |a| a.len())));
        }
    }

    // The failed batch asks for another level than the batch after it, so a
    // level it pinned would refuse that batch.
    let three = crate::compression::CompressionLevel::from_u8(3).unwrap();
    let channel = |failing: Option<usize>| -> Vec<serde_json::Value> {
        let ch = crate::stream::shared_open_channel("at2sF3ai8").unwrap();
        let reader = std::thread::spawn(move || {
            let list = parse_schema("at2sF3ai8").unwrap();
            let mut frames = Vec::new();
            while let Some(p) = shared_next_frame(ch).unwrap() {
                frames.push(render_and_free(p, &list).unwrap());
            }
            frames
        });
        if let Some(k) = failing {
            let batch = handle_records(n, name_len, &targets, &handles, Some((k, closed)));
            refused_for_its_handle(shared_write_subpacket(ch, three, batch), k);
            let _ = shm::shfree(batch);
        }
        let after = handle_records(2, |_| 400, &targets, &handles, None);
        shared_write_subpacket(ch, level, after).unwrap();
        shared_flush_buffer(ch).unwrap();
        shared_close_handle(ch).unwrap();
        let _ = shm::shfree(after);
        let frames = reader.join().unwrap();
        crate::stream::shared_settle_channel(ch).unwrap();
        frames
    };
    let reference = channel(None);
    for k in [0usize, 9, 12, n - 1] {
        let frames = channel(Some(k));
        if frames != reference {
            failures.push(format!("channel: element {k} failing, the reader saw {} frames, want {}", frames.len(), reference.len()));
        }
    }

    // A staged stream writes a batch as one sub-packet at its own level: a
    // failed first batch pins no level.
    let staged = |path: &str, failing: Option<usize>| -> (usize, serde_json::Value) {
        let out = shared_open_ostream_with_schema(path, "at2sF3ai8").unwrap();
        crate::stream::slot_ref(crate::stream::unpack_handle(out).1).unwrap().staged.store(1, std::sync::atomic::Ordering::SeqCst);
        if let Some(k) = failing {
            let batch = handle_records(n, name_len, &targets, &handles, Some((k, closed)));
            refused_for_its_handle(shared_write_subpacket(out, three, batch), k);
            let _ = shm::shfree(batch);
        }
        let after = handle_records(2, |_| 400, &targets, &handles, None);
        let r = shared_write_subpacket(out, level, after);
        shared_close_handle(out).unwrap();
        let _ = shm::shfree(after);
        r.unwrap();
        let len = std::fs::metadata(path).unwrap().len() as usize;
        (len, render_and_free(shared_load_stream_file_as_array(path, Some(&list)).unwrap(), &list).unwrap())
    };
    let reference = staged(dir.join("staged-ref.stream").to_str().unwrap(), None);
    for k in [0usize, 12, n - 1] {
        let got = staged(dir.join(format!("staged{k}.stream")).to_str().unwrap(), Some(k));
        if got != reference {
            failures.push(format!("staged: element {k} failing leaves {} bytes, want {}", got.0, reference.0));
        }
    }

    for h in handles {
        let _ = shared_discard_handle(h);
    }
    let _ = std::fs::remove_dir_all(&dir);
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}

/// A `@write` batch that fails while it is placed or handed on is whole or
/// absent: before any of its elements is handed on it leaves no trace and a
/// failed first batch pins no level; after, it fails the stream, which then
/// refuses writes and never gets a final footer, and a channel's reader gets
/// the failure.
#[test]
fn a_batch_failing_in_placement_is_absent_or_fails_the_stream() {
    use crate::stream::{FAIL_HAND_ON_AT, FAIL_PLACE_AT};
    struct Restore;
    impl Drop for Restore {
        fn drop(&mut self) {
            crate::stream::set_test_write_buffer_bytes(None);
            FAIL_PLACE_AT.with(|f| f.set(None));
            FAIL_HAND_ON_AT.with(|f| f.set(None));
        }
    }
    struct Discard(i64);
    impl Drop for Discard {
        fn drop(&mut self) {
            let _ = shared_discard_handle(self.0);
        }
    }
    #[derive(Clone, Copy, Debug)]
    enum Fault {
        Place,
        HandOn,
    }
    let inject = |fault: Fault, k: Option<usize>| match fault {
        Fault::Place => FAIL_PLACE_AT.with(|f| f.set(k.map(|k| k as u64))),
        Fault::HandOn => FAIL_HAND_ON_AT.with(|f| f.set(k.map(|k| k as u64))),
    };
    let injected = |r: &Result<(), MorlocError>| matches!(r, Err(e) if e.to_string().contains("failed to place") || e.to_string().contains("handing on before"));
    let _shm = crate::own_test_registry();
    let dir = std::env::temp_dir().join(format!("morloc_layout_placement_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    let _restore = buffer_bytes(4096);
    let _faults = Restore;
    let none = crate::compression::CompressionLevel::NONE;
    let three = crate::compression::CompressionLevel::from_u8(3).unwrap();

    type Item = Box<dyn Fn(usize) -> String>;
    let cases: [(&str, usize, Item); 3] = [
        ("as", 21, Box::new(|k| format!("\"{}\"", "s".repeat(if k == 12 { 5000 } else { 400 })))),
        ("ai4", 3000, Box::new(|k| format!("{k}"))),
        ("aj", 21, Box::new(|k| if k % 5 == 4 { format!("{}", 1u128 << 100) } else { format!("{k}") })),
    ];
    let mut failures = Vec::new();
    for (schema, n, item) in &cases {
        let n = *n;
        let list = parse_schema(schema).unwrap();
        let value = |len: usize, from: usize| {
            let json = format!("[{}]", (from..from + len).map(|k| item(k)).collect::<Vec<_>>().join(","));
            read_json_with_schema(&json, &list).unwrap()
        };
        // `Ok(None)`: the batch failed the stream; `Ok(Some(..))`: the file;
        // `Err("")`: no hand-off came at element k.
        let write = |path: &str, failing: Option<(Fault, usize)>, level| -> Result<Option<(usize, serde_json::Value)>, String> {
            let out = shared_open_ostream_with_schema(path, schema).unwrap();
            let _open = Discard(out);
            // A failed first batch pins no level; later ones carry the pinned one.
            let first = matches!(failing, Some((_, 0)));
            if !first {
                let before = value(3, 100);
                shared_write_subpacket(out, level, before).unwrap();
                let _ = shm::shfree(before);
            }
            if let Some((fault, k)) = failing {
                let batch = value(n, 0);
                inject(fault, Some(k));
                let r = shared_write_subpacket(out, level, batch);
                inject(fault, None);
                let _ = shm::shfree(batch);
                if matches!(fault, Fault::HandOn) && r.is_ok() {
                    let _ = shared_close_handle(out);
                    return Err(String::new());
                }
                if !injected(&r) {
                    return Err(format!("the failing write gave {r:?}"));
                }
            }
            let after = value(2, 200);
            let r = shared_write_subpacket(out, if first { none } else { level }, after);
            let _ = shm::shfree(after);
            let counted = crate::stream::slot_ref(crate::stream::unpack_handle(out).1).unwrap().element_count.load(std::sync::atomic::Ordering::SeqCst);
            let closed = shared_close_handle(out);
            match (r, closed) {
                (Ok(()), Ok(())) => {
                    let len = std::fs::metadata(path).unwrap().len() as usize;
                    Ok(Some((len, render_and_free(shared_load_stream_file_as_array(path, Some(&list)).unwrap(), &list).unwrap())))
                }
                (Err(e), Err(_)) => {
                    if !e.to_string().contains("handed on") {
                        return Err(format!("a write after the stream failed was refused with {e}, not the failure"));
                    }
                    let (mp, sz) = crate::stream_format::mmap_file_readonly(path).unwrap();
                    let footer = crate::stream_format::try_read_footer(mp, sz);
                    unsafe { libc::munmap(mp as *mut libc::c_void, sz as usize) };
                    let in_file = match footer {
                        Ok(Some(f)) if !f.final_footer => f.element_count,
                        _ => return Err("a stream whose batch failed after a hand-off got a final footer".into()),
                    };
                    if in_file != counted {
                        return Err(format!("the failed stream counts {counted} elements and its file holds {in_file}"));
                    }
                    Ok(None)
                }
                (r, c) => Err(format!("after the failed batch, a write gave {r:?} and the close {c:?}")),
            }
        };
        let reference = write(dir.join(format!("{schema}-ref.stream")).to_str().unwrap(), None, none).unwrap();
        for fault in [Fault::Place, Fault::HandOn] {
            let first = write(dir.join(format!("{schema}-{fault:?}-first.stream")).to_str().unwrap(), Some((fault, 0)), three);
            match first {
                Ok(Some((_, got))) if got.as_array().map(|a| a.len()) == Some(2) => {}
                Err(e) if e.is_empty() => {}
                r => failures.push(format!("{schema} {fault:?}: a failed first batch at level 3, then a batch at level 0, gave {r:?}")),
            }
            let (mut absent, mut failed) = (0, 0);
            let ks: Vec<usize> = if n > 100 { vec![1, 500, 1019, 1020, 1021, 2047, 2048, n - 1] } else { (1..n).collect() };
            for &k in &ks {
                match write(dir.join(format!("{schema}-{fault:?}-bad{k}.stream")).to_str().unwrap(), Some((fault, k)), none) {
                    Ok(None) => failed += 1,
                    Ok(got) if got == reference => {
                        if failed > 0 {
                            failures.push(format!("{schema} {fault:?}: element {k} left no trace after an earlier one failed the stream"));
                        }
                        absent += 1
                    }
                    Ok(got) => failures.push(format!("{schema} {fault:?}: element {k} failing leaves {got:?}, want {reference:?}")),
                    Err(e) if e.is_empty() => {}
                    Err(e) => failures.push(format!("{schema} {fault:?}: element {k}: {e}")),
                }
            }
            if absent == 0 || failed == 0 {
                failures.push(format!("{schema}: {absent} failures left no trace and {failed} failed the stream; want both"));
            }
        }
    }

    // A channel's reader sees a batch that failed before a hand-off not at
    // all, and one that failed after as the stream's failure.
    let list = parse_schema("as").unwrap();
    let strs = |len: usize| {
        let json = format!("[{}]", (0..len).map(|_| format!("\"{}\"", "s".repeat(400))).collect::<Vec<_>>().join(","));
        read_json_with_schema(&json, &list).unwrap()
    };
    let channel = |failing: Option<(Fault, usize)>| -> Option<(Vec<serde_json::Value>, bool)> {
        let ch = crate::stream::shared_open_channel("as").unwrap();
        let reader = std::thread::spawn(move || {
            let list = parse_schema("as").unwrap();
            let mut frames = Vec::new();
            loop {
                match shared_next_frame(ch) {
                    Ok(Some(p)) => frames.push(render_and_free(p, &list).unwrap()),
                    Ok(None) => return (frames, false),
                    Err(_) => return (frames, true),
                }
            }
        });
        if let Some((fault, k)) = failing {
            let batch = strs(21);
            inject(fault, Some(k));
            let r = shared_write_subpacket(ch, none, batch);
            inject(fault, None);
            let _ = shm::shfree(batch);
            if r.is_ok() && matches!(fault, Fault::HandOn) {
                let _ = shared_close_handle(ch);
                let _ = reader.join();
                let _ = crate::stream::shared_settle_channel(ch);
                return None;
            }
            assert!(injected(&r), "the failing channel write gave {r:?}");
        }
        let after = strs(2);
        if let Err(e) = shared_write_subpacket(ch, none, after) {
            assert!(e.to_string().contains("handed on"), "a write after the channel failed was refused with {e}, not the failure");
        }
        let _ = shared_flush_buffer(ch);
        let closed = shared_close_handle(ch);
        let _ = shm::shfree(after);
        let got = reader.join().unwrap();
        assert_eq!(got.1, closed.is_err(), "the reader saw a failure {} but the close gave {closed:?}", got.1);
        let _ = crate::stream::shared_settle_channel(ch);
        Some(got)
    };
    let reference = channel(None).unwrap();
    for fault in [Fault::Place, Fault::HandOn] {
        let mut failed = 0;
        for k in 1..21 {
            let Some((frames, err)) = channel(Some((fault, k))) else { continue };
            if err {
                failed += 1;
            } else if (frames.clone(), err) != reference {
                failures.push(format!("channel {fault:?}: element {k} failing, the reader saw {} frames and no failure, want {}", frames.len(), reference.0.len()));
            } else if failed > 0 {
                failures.push(format!("channel {fault:?}: element {k} left no trace after an earlier one failed the channel"));
            }
        }
        if failed == 0 {
            failures.push(format!("channel {fault:?}: no failure reached the reader"));
        }
    }
    let _ = std::fs::remove_dir_all(&dir);
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}

/// Write `value` as a binding writes in place: each element is sized, given
/// its room, and encoded there by the runtime's own flatten, of which only
/// the nonzero bytes are stored, as an encoder that writes fields alone
/// would; `runs` places fixed-width elements in runs. True if in place.
pub(crate) fn write_in_place(
    h: i64,
    level: crate::compression::CompressionLevel,
    value: crate::shm_types::AbsPtr,
    elem: &Schema,
    runs: bool,
    give_up_at: Option<usize>,
) -> Result<bool, MorlocError> {
    use crate::stream::{write_abort, write_begin, write_commit, write_place, write_place_run, Begun};
    use morloc_runtime_types::schema::SerialType;
    if let Begun::Fallback = write_begin(h, level)? {
        return shared_write_subpacket(h, level, value).map(|()| false);
    }
    let encode = || -> Result<(), MorlocError> {
        let arr = unsafe { &*(value as *const crate::shm_types::Array) };
        let (n, w) = (arr.size, elem.width);
        let base = if n == 0 { std::ptr::null_mut() } else { shm::rel2abs(arr.data)? };
        let res = crate::recur::Resolver::new(elem);
        let mut reached: *const u8 = std::ptr::null();
        let mut i = 0;
        while i < n {
            if give_up_at.is_some_and(|k| i >= k) {
                return Err(MorlocError::Other(format!("the encoder gave up at element {i}")));
            }
            let src = unsafe { base.add(i * w) };
            if runs && elem.is_fixed_width() && w > 0 {
                let want = give_up_at.map_or(n, |k| k.min(n)) - i;
                let (slots, k) = write_place_run(want, reached)?;
                for o in 0..k * w {
                    let b = unsafe { *(src as *const u8).add(o) };
                    if b != 0 {
                        unsafe { *slots.add(o) = b };
                    }
                }
                reached = unsafe { slots.add(k * w) };
                i += k;
                continue;
            }
            let inline_int = elem.serial_type == SerialType::Int && unsafe { *(src as *const usize) } <= 1;
            let size = if inline_int { w } else { crate::ffi::calc_voidstar_size_portable_with(src, elem, &res)? };
            let room = write_place(size, reached)?;
            let at = |p: *mut u8| p as usize - room.base as usize;
            let mut scratch = vec![0u64; at(room.end).div_ceil(8)];
            let sp = scratch.as_mut_ptr() as *mut u8;
            if inline_int {
                unsafe { std::ptr::copy_nonoverlapping(src as *const u8, sp.add(at(room.slot)), w) };
            } else {
                unsafe { crate::voidstar::flatten_portable_in_place(sp, at(room.slot), at(room.data), size, src, elem, &res)? };
            }
            for range in [at(room.slot)..at(room.slot) + w, at(room.data)..at(room.end)] {
                for o in range {
                    let b = unsafe { *sp.add(o) };
                    if b != 0 {
                        unsafe { *room.base.add(o) = b };
                    }
                }
            }
            reached = room.end;
            i += 1;
        }
        write_commit(reached)
    };
    encode().inspect_err(|_| write_abort()).map(|()| true)
}

/// An in-place batch is laid out byte for byte as the copy path lays out
/// the same batch, sub-packet cuts included (SLOT-36).
#[test]
fn an_in_place_batch_writes_the_bytes_the_copy_path_writes() {
    let _shm = crate::own_test_registry();
    let _restore = buffer_bytes(4096);
    let dir = std::env::temp_dir().join(format!("morloc_layout_in_place_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    let cases: u64 = std::env::var("MORLOC_LAYOUT_PROP_CASES").ok().and_then(|s| s.parse().ok()).unwrap_or(200);
    let mut failures = Vec::new();
    let in_place = std::cell::Cell::new(0usize);
    let seeds: Vec<u64> = match std::env::var(SEED_ENV).ok().and_then(|s| s.parse().ok()) {
        Some(one) => vec![one],
        None => (0..cases).collect(),
    };
    for &seed in &seeds {
        let mut r = Rng::new(seed ^ 0x5eed);
        let g = gen_type(&mut r, 3, false, true);
        let elem_str = schema_str(&g);
        let list = parse_schema(&format!("a{elem_str}")).unwrap();
        let elem = parse_schema(&elem_str).unwrap();
        let mut lists: Vec<String> = (0..6).map(|k| if k == 3 { "[]".into() } else { gen_list(&mut r, &g, k == 0) }).collect();
        let long: Vec<String> = (0..400).map(|k| gen_value(&mut r, &g, None, 3, k % 7 == 0)).collect();
        lists.push(format!("[{}]", long.join(",")));
        for buf in [4096usize, 1 << 16] {
            crate::stream::set_test_write_buffer_bytes(Some(buf));
            for level in [0u8, 3] {
                let level = crate::compression::CompressionLevel::from_u8(level).unwrap();
                for runs in [false, true] {
                    eprintln!("in_place: seed {seed} schema {elem_str} buf {buf} level {level:?} runs {runs}");
                    let write = |tag: &str, placing: bool| -> Result<Vec<u8>, MorlocError> {
                        let path = dir.join(format!("{seed}-{tag}.stream"));
                        let h = shared_open_ostream_with_schema(path.to_str().unwrap(), &format!("a{elem_str}"))?;
                        for _ in 0..8 {
                            for l in &lists {
                                let v = read_json_with_schema(l, &list)?;
                                let w = if placing {
                                    write_in_place(h, level, v, &elem, runs, None).map(|p| in_place.set(in_place.get() + p as usize))
                                } else {
                                    shared_write_subpacket(h, level, v)
                                };
                                let _ = shm::shfree(v);
                                w?;
                            }
                        }
                        shared_close_handle(h)?;
                        let (mp, sz) = crate::stream_format::mmap_file_readonly(path.to_str().unwrap())?;
                        let footer = crate::stream_format::try_read_footer(mp, sz);
                        unsafe { libc::munmap(mp as *mut libc::c_void, sz as usize) };
                        let footer = footer?.ok_or_else(|| MorlocError::Other("no footer".into()))?;
                        let mut bytes = std::fs::read(&path).unwrap();
                        bytes.truncate(footer.footer_start as usize);
                        bytes.extend(format!("{:?} {}", footer.subpacket_entries, footer.element_count).into_bytes());
                        Ok(bytes)
                    };
                    match (write("copy", false), write("place", true)) {
                        (Ok(a), Ok(b)) if a == b => {}
                        (Ok(a), Ok(b)) => {
                            let at = a.iter().zip(&b).position(|(x, y)| x != y).unwrap_or(a.len().min(b.len()));
                            failures.push(format!("seed {seed} {elem_str} buf {buf} {level:?} runs {runs}: files differ at byte {at} ({} vs {} bytes)", a.len(), b.len()));
                        }
                        (a, b) => failures.push(format!("seed {seed} {elem_str} buf {buf} {level:?} runs {runs}: copy {:?} in place {:?}", a.err(), b.err())),
                    }
                }
            }
        }
    }
    let _ = std::fs::remove_dir_all(&dir);
    assert!(in_place.get() > seeds.len() * 8 * 30, "only {} batches went in place", in_place.get());
    assert!(failures.is_empty(), "{} failures; first 20:\n{}", failures.len(), failures.iter().take(20).cloned().collect::<Vec<_>>().join("\n"));
}

/// An in-place batch that fails before its commit leaves no trace and pins
/// no level, whatever block it failed in; a commit refused at its first
/// push does the same, and one refused later fails the stream, whose file
/// keeps its temporary footer. No block outlives the stream (SLOT-36).
#[test]
fn an_in_place_batch_that_fails_is_absent_or_fails_its_stream() {
    use crate::stream::{FAIL_COMMIT_PUSH_AT, FAIL_PLACE_AT};
    struct Restore;
    impl Drop for Restore {
        fn drop(&mut self) {
            crate::stream::set_test_write_buffer_bytes(None);
            FAIL_PLACE_AT.with(|f| f.set(None));
            FAIL_COMMIT_PUSH_AT.with(|f| f.set(None));
        }
    }
    #[derive(Clone, Copy, Debug, PartialEq)]
    enum Fault {
        Place,
        GiveUp,
        Push,
    }
    let _shm = crate::own_test_registry();
    let dir = std::env::temp_dir().join(format!("morloc_layout_in_place_fail_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    let _restore = buffer_bytes(4096);
    let _faults = Restore;
    let none = crate::compression::CompressionLevel::NONE;
    let three = crate::compression::CompressionLevel::from_u8(3).unwrap();
    let live = || {
        let mut hist = [0usize; 40];
        shm::live_block_stats(&mut hist).0
    };

    type Item = Box<dyn Fn(usize) -> String>;
    // A refused push of a block holding only earlier batches' elements
    // leaves the batch absent: the "as" batch of 3 seals the buffered
    // elements alone before its oversize first element.
    let cases: [(&str, usize, bool, Item); 6] = [
        ("as", 21, false, Box::new(|k| format!("\"{}\"", "s".repeat(if k == 12 { 5000 } else { 400 })))),
        ("as", 4, false, Box::new(|k| format!("\"{}\"", "s".repeat(if k == 0 { 5000 } else { 10 })))),
        ("as", 3, false, Box::new(|k| format!("\"{}\"", "s".repeat(if k == 0 { 5000 } else { 10 })))),
        ("ai4", 3000, false, Box::new(|k| format!("{k}"))),
        ("ai4", 3000, true, Box::new(|k| format!("{k}"))),
        ("aj", 21, false, Box::new(|k| if k % 5 == 4 { format!("{}", 1u128 << 100) } else { format!("{k}") })),
    ];
    let mut failures = Vec::new();
    for (schema, n, runs, item) in &cases {
        let (n, runs) = (*n, *runs);
        let list = parse_schema(schema).unwrap();
        let elem = list.parameters[0].clone();
        let value = |len: usize, from: usize| {
            let json = format!("[{}]", (from..from + len).map(|k| item(k)).collect::<Vec<_>>().join(","));
            read_json_with_schema(&json, &list).unwrap()
        };
        // `Ok(None)`: the stream failed; `Ok(Some(..))`: what the file holds.
        let write = |path: &str, failing: Option<(Fault, usize)>, level| -> Result<Option<serde_json::Value>, String> {
            let blocks = live();
            let out = shared_open_ostream_with_schema(path, schema).unwrap();
            let slot = crate::stream::slot_ref(crate::stream::unpack_handle(out).1).unwrap();
            // SLOT-10: a slot's first stream allocates the queue it keeps.
            let blocks = blocks + (crate::custody::queue_at(crate::stream::SlotField::get(&slot.custody)).unwrap().epoch() == 1) as usize;
            let first = matches!(failing, Some((_, 0)));
            if !first {
                let before = value(3, 100);
                write_in_place(out, level, before, &elem, runs, None).unwrap();
                let _ = shm::shfree(before);
                if schema == &"as" && n == 4 {
                    shared_flush_buffer(out).unwrap();
                }
            }
            let mut may_fail = false;
            if let Some((fault, k)) = failing {
                let batch = value(n, 0);
                match fault {
                    Fault::Place => FAIL_PLACE_AT.with(|f| f.set(Some(k as u64))),
                    Fault::Push => FAIL_COMMIT_PUSH_AT.with(|f| f.set(Some(k))),
                    Fault::GiveUp => {}
                }
                let r = write_in_place(out, level, batch, &elem, runs, (fault == Fault::GiveUp).then_some(k));
                FAIL_PLACE_AT.with(|f| f.set(None));
                FAIL_COMMIT_PUSH_AT.with(|f| f.set(None));
                let _ = shm::shfree(batch);
                match r {
                    Err(e) if fault == Fault::Push && k > 0 => {
                        if !e.to_string().contains("pushing block") {
                            return Err(format!("the refused commit gave {e}"));
                        }
                        may_fail = true;
                    }
                    Err(_) => {}
                    Ok(_) if fault == Fault::Push => {
                        let _ = shared_close_handle(out);
                        return Err(String::new());
                    }
                    Ok(_) => return Err("the failing batch was written".into()),
                }
            }
            let after = value(2, 200);
            let r = write_in_place(out, if first { none } else { level }, after, &elem, runs, None);
            let _ = shm::shfree(after);
            let counted = slot.element_count.load(std::sync::atomic::Ordering::SeqCst);
            let closed = shared_close_handle(out);
            if live() != blocks {
                return Err(format!("{} blocks were live before the stream and {} after", blocks, live()));
            }
            match (r, closed, may_fail) {
                (Ok(_), Ok(()), _) => {
                    Ok(Some(render_and_free(shared_load_stream_file_as_array(path, Some(&list)).unwrap(), &list).unwrap()))
                }
                (Err(e), Err(_), true) => {
                    if !e.to_string().contains("queued") {
                        return Err(format!("a write after the stream failed was refused with {e}, not the failure"));
                    }
                    let (mp, sz) = crate::stream_format::mmap_file_readonly(path).unwrap();
                    let footer = crate::stream_format::try_read_footer(mp, sz);
                    unsafe { libc::munmap(mp as *mut libc::c_void, sz as usize) };
                    match footer {
                        Ok(Some(f)) if !f.final_footer && f.element_count == counted => Ok(None),
                        f => Err(format!("the failed stream counts {counted} elements and has footer {f:?}")),
                    }
                }
                (r, c, may_fail) => Err(format!("after the failed batch (may fail the stream: {may_fail}), a write gave {r:?} and the close {c:?}")),
            }
        };
        let tag = format!("{schema}-{n}-{runs}");
        let reference = write(dir.join(format!("{tag}-ref.stream")).to_str().unwrap(), None, none).unwrap().unwrap();
        for fault in [Fault::Place, Fault::GiveUp, Fault::Push] {
            match write(dir.join(format!("{tag}-{fault:?}-first.stream")).to_str().unwrap(), Some((fault, 0)), three) {
                Ok(Some(got)) if got.as_array().map(|a| a.len()) == Some(2) => {}
                r => failures.push(format!("{tag} {fault:?}: a failed first batch at level 3, then a batch at level 0, gave {r:?}")),
            }
            let ks: Vec<usize> = match fault {
                Fault::Push => (1..8).collect(),
                _ if n > 100 => vec![1, 500, 1019, 1020, 1021, 2047, 2048, n - 1],
                _ => (1..n).collect(),
            };
            let mut failed = 0;
            for &k in &ks {
                let absent_only = fault == Fault::Push && n == 3 && k == 1;
                match write(dir.join(format!("{tag}-{fault:?}-bad{k}.stream")).to_str().unwrap(), Some((fault, k)), none) {
                    Ok(None) if absent_only => failures.push(format!("{tag}: a refused push of earlier elements alone failed the stream")),
                    Ok(None) if fault == Fault::Push => failed += 1,
                    Ok(Some(got)) if got == reference => {}
                    Ok(got) => failures.push(format!("{tag} {fault:?}: failing at {k} leaves {got:?}, want {reference:?}")),
                    Err(e) if e.is_empty() => {}
                    Err(e) => failures.push(format!("{tag} {fault:?}: failing at {k}: {e}")),
                }
            }
            if fault == Fault::Push && n > 4 && failed == 0 {
                failures.push(format!("{tag}: no refused commit failed the stream"));
            }
        }
    }
    let _ = std::fs::remove_dir_all(&dir);
    assert!(failures.is_empty(), "{} failures:\n{}", failures.len(), failures.join("\n"));
}

fn strs_value(n: usize, len: usize, tag: usize) -> crate::shm_types::AbsPtr {
    let list = parse_schema("as").unwrap();
    let json = format!("[{}]", (0..n).map(|k| format!("\"{tag}-{k}-{}\"", "s".repeat(len))).collect::<Vec<_>>().join(","));
    read_json_with_schema(&json, &list).unwrap()
}

fn read_back(path: &std::path::Path) -> serde_json::Value {
    let list = parse_schema("as").unwrap();
    render_and_free(shared_load_stream_file_as_array(path.to_str().unwrap(), Some(&list)).unwrap(), &list).unwrap()
}

/// A thread with an in-place batch open does no other stream operation, so
/// encoder code that writes another stream cannot deadlock two writers
/// (SLOT-36).
#[test]
fn a_thread_encoding_in_place_does_no_other_stream_operation() {
    use crate::stream::{write_abort, write_begin, write_commit, Begun};
    let _shm = crate::own_test_registry();
    let dir = std::env::temp_dir().join(format!("morloc_in_place_refuse_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    let none = crate::compression::CompressionLevel::NONE;
    let s1 = shared_open_ostream_with_schema(dir.join("1.stream").to_str().unwrap(), "as").unwrap();
    let s2 = shared_open_ostream_with_schema(dir.join("2.stream").to_str().unwrap(), "as").unwrap();
    let s3 = shared_open_ostream_with_schema(dir.join("3.stream").to_str().unwrap(), "as").unwrap();
    let refused = |r: Result<(), MorlocError>, what: &str| {
        let e = r.expect_err(what).to_string();
        assert!(e.contains("encoding a @write batch") || e.contains("already open"), "{what}: {e}");
    };
    assert!(matches!(write_begin(s1, none).unwrap(), Begun::Open));
    refused(write_begin(s2, none).map(|_| ()), "a second batch");
    let v = strs_value(3, 4, 0);
    refused(shared_write_subpacket(s2, none, v), "a copy write to another stream");
    refused(shared_write_subpacket(s1, none, v), "a copy write to its own stream");
    refused(shared_flush_buffer(s1), "a flush");
    shared_close_handle(s3).unwrap();
    let s3_slot = crate::stream::slot_ref(crate::stream::unpack_handle(s3).1).unwrap();
    let open = || s3_slot.generation.load(std::sync::atomic::Ordering::Acquire) & crate::stream::GENERATION_MASK == crate::stream::unpack_handle(s3).0;
    assert!(open(), "a close inside the batch ran before the batch ended");
    write_commit(std::ptr::null()).unwrap();
    assert!(!open(), "a close inside the batch never ran");
    assert!(read_back(&dir.join("3.stream")).as_array().unwrap().is_empty());
    shared_write_subpacket(s2, none, v).unwrap();

    let (tx, rx) = std::sync::mpsc::channel();
    let barrier = std::sync::Arc::new(std::sync::Barrier::new(2));
    for (mine, other) in [(s1, s2), (s2, s1)] {
        let (tx, barrier) = (tx.clone(), barrier.clone());
        let v = v as usize;
        std::thread::spawn(move || {
            assert!(matches!(write_begin(mine, none).unwrap(), Begun::Open));
            barrier.wait();
            let r = shared_write_subpacket(other, none, v as crate::shm_types::AbsPtr);
            write_abort();
            tx.send(r.is_err()).unwrap();
        });
    }
    for _ in 0..2 {
        let refused = rx.recv_timeout(std::time::Duration::from_secs(20)).expect("two encoders writing each other's stream hung");
        assert!(refused, "an encoder wrote another stream inside its batch");
    }
    let _ = shm::shfree(v);
    shared_close_handle(s1).unwrap();
    shared_close_handle(s2).unwrap();
    assert_eq!(read_back(&dir.join("2.stream")).as_array().unwrap().len(), 3);
    let _ = std::fs::remove_dir_all(&dir);
}

/// Blocks an in-place batch holds are the slot's: an eval arena rewound
/// between placements frees none of them (SLOT-36, SHM-8).
#[test]
fn an_arena_rewound_inside_an_in_place_batch_keeps_its_blocks() {
    let _shm = crate::own_test_registry();
    let _restore = buffer_bytes(4096);
    let dir = std::env::temp_dir().join(format!("morloc_in_place_arena_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("a.stream");
    let none = crate::compression::CompressionLevel::NONE;
    let h = shared_open_ostream_with_schema(path.to_str().unwrap(), "as").unwrap();
    let list = parse_schema("as").unwrap();
    let elem = list.parameters[0].clone();
    let lens = [10usize, 300, 2000, 40, 700, 5000, 3];
    let json = format!("[{}]", lens.iter().map(|&n| format!("\"{}\"", "x".repeat(n))).collect::<Vec<_>>().join(","));
    let v = read_json_with_schema(&json, &list).unwrap();
    {
        let _arena = crate::eval_arena::enter().unwrap();
        let cp = crate::eval_arena::checkpoint();
        assert!(matches!(crate::stream::write_begin(h, none).unwrap(), crate::stream::Begun::Open));
        let arr = unsafe { &*(v as *const crate::shm_types::Array) };
        let base = shm::rel2abs(arr.data).unwrap();
        let res = crate::recur::Resolver::new(&elem);
        let mut reached: *const u8 = std::ptr::null();
        for i in 0..lens.len() {
            let src = unsafe { base.add(i * elem.width) };
            let size = crate::ffi::calc_voidstar_size_portable_with(src, &elem, &res).unwrap();
            let room = crate::stream::write_place(size, reached).unwrap();
            let at = |p: *mut u8| p as usize - room.base as usize;
            unsafe { crate::voidstar::flatten_portable_in_place(room.base, at(room.slot), at(room.data), size, src, &elem, &res).unwrap() };
            reached = room.end;
            crate::eval_arena::rollback_to(cp);
        }
        crate::stream::write_commit(reached).unwrap();
    }
    let _ = shm::shfree(v);
    shared_close_handle(h).unwrap();
    let got = read_back(&path);
    let want: Vec<serde_json::Value> = lens.iter().map(|&n| serde_json::Value::String("x".repeat(n))).collect();
    assert_eq!(got, serde_json::Value::Array(want));
    let _ = std::fs::remove_dir_all(&dir);
}

/// An element whose encoding ends anywhere but where its size said is a
/// defect: the call panics and the stream is left poisoned (SLOT-36, SLOT-14).
#[test]
fn an_in_place_element_that_overruns_its_room_poisons_the_stream() {
    let _shm = crate::own_test_registry();
    let dir = std::env::temp_dir().join(format!("morloc_in_place_overrun_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    let none = crate::compression::CompressionLevel::NONE;
    let h = shared_open_ostream_with_schema(dir.join("o.stream").to_str().unwrap(), "as").unwrap();
    let panicked = std::thread::spawn(move || {
        assert!(matches!(crate::stream::write_begin(h, none).unwrap(), crate::stream::Begun::Open));
        let room = crate::stream::write_place(16 + 5, std::ptr::null()).unwrap();
        let _ = crate::stream::write_place(16 + 5, unsafe { room.end.add(8) });
    })
    .join();
    assert!(panicked.is_err(), "an overrun was not a defect");
    let v = strs_value(1, 1, 0);
    let e = shared_write_subpacket(h, none, v).expect_err("a poisoned stream took a write").to_string();
    assert!(e.contains("died"), "{e}");
    let _ = shm::shfree(v);
    let _ = shared_discard_handle(h);
    let _ = std::fs::remove_dir_all(&dir);
}

/// Two threads writing one stream in place each leave their batches whole
/// and in one piece (SLOT-36, SLOT-11).
#[test]
fn two_threads_writing_in_place_leave_their_batches_whole() {
    let _shm = crate::own_test_registry();
    let _restore = buffer_bytes(4096);
    let dir = std::env::temp_dir().join(format!("morloc_in_place_two_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("t.stream");
    let h = shared_open_ostream_with_schema(path.to_str().unwrap(), "as").unwrap();
    let threads: Vec<_> = (0..2)
        .map(|t| {
            std::thread::spawn(move || {
                let elem = parse_schema("s").unwrap();
                for b in 0..40 {
                    let v = strs_value(25, 30, t * 1000 + b);
                    write_in_place(h, crate::compression::CompressionLevel::NONE, v, &elem, false, None).unwrap();
                    let _ = shm::shfree(v);
                }
            })
        })
        .collect();
    for t in threads {
        t.join().unwrap();
    }
    shared_close_handle(h).unwrap();
    let got = read_back(&path);
    let got: Vec<&str> = got.as_array().unwrap().iter().map(|s| s.as_str().unwrap()).collect();
    assert_eq!(got.len(), 2 * 40 * 25);
    for batch in got.chunks(25) {
        let tag = batch[0].split('-').next().unwrap();
        for (k, s) in batch.iter().enumerate() {
            assert!(s.starts_with(&format!("{tag}-{k}-")), "batch {tag} is not whole: {batch:?}");
        }
    }
    let _ = std::fs::remove_dir_all(&dir);
}

/// A forked child forgets the batch its parent's thread had open; a thread
/// that ends with one open ends its process, which leaves the stream to
/// the death path (SLOT-36, SLOT-14, FORK-1).
#[test]
fn an_in_place_batch_does_not_survive_a_fork_or_its_thread() {
    let _shm = crate::own_test_registry();
    let dir = std::env::temp_dir().join(format!("morloc_in_place_fork_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    let none = crate::compression::CompressionLevel::NONE;
    let p1 = dir.join("1.stream");
    let s1 = shared_open_ostream_with_schema(p1.to_str().unwrap(), "as").unwrap();
    let s2 = shared_open_ostream_with_schema(dir.join("2.stream").to_str().unwrap(), "as").unwrap();
    let wait = |pid: libc::pid_t| {
        let mut status = 0;
        assert_eq!(unsafe { libc::waitpid(pid, &mut status, 0) }, pid);
        status
    };

    assert!(matches!(crate::stream::write_begin(s1, none).unwrap(), crate::stream::Begun::Open));
    let pid = unsafe { libc::fork() };
    if pid == 0 {
        let forgot = crate::stream::write_place(24, std::ptr::null()).is_err();
        let v = strs_value(2, 3, 9);
        let wrote = shared_write_subpacket(s2, none, v).is_ok();
        unsafe { libc::_exit(if forgot && wrote { 0 } else { 1 }) };
    }
    let status = wait(pid);
    assert!(libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0, "child status {status}");
    let v = strs_value(2, 3, 1);
    crate::stream::write_abort();
    shared_write_subpacket(s1, none, v).unwrap();

    let pid = unsafe { libc::fork() };
    if pid == 0 {
        let none_core = libc::rlimit { rlim_cur: 0, rlim_max: 0 };
        unsafe { libc::setrlimit(libc::RLIMIT_CORE, &none_core) };
        let _ = std::thread::spawn(move || {
            let _ = crate::stream::write_begin(s1, none);
        })
        .join();
        unsafe { libc::_exit(0) };
    }
    let status = wait(pid);
    assert!(!(libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0), "a thread ended inside a batch and its process went on");
    let e = shared_write_subpacket(s1, none, v).expect_err("the stream outlived its writer's death").to_string();
    assert!(e.contains("died"), "{e}");
    let _ = shm::shfree(v);
    let _ = shared_discard_handle(s1);
    shared_close_handle(s2).unwrap();
    assert_eq!(read_back(&dir.join("2.stream")).as_array().unwrap().len(), 2);
    let _ = std::fs::remove_dir_all(&dir);
}

/// Runs and single rooms mix in one batch, and a run's slots read as zero
/// though the buffer once held other values (SLOT-36).
#[test]
fn in_place_runs_read_as_zero_and_mix_with_single_rooms() {
    use crate::stream::{write_begin, write_commit, write_place, write_place_run, Begun};
    let _shm = crate::own_test_registry();
    let _restore = buffer_bytes(4096);
    let dir = std::env::temp_dir().join(format!("morloc_in_place_runs_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("r.stream");
    let none = crate::compression::CompressionLevel::NONE;
    let h = shared_open_ostream_with_schema(path.to_str().unwrap(), "ai4").unwrap();
    let list = parse_schema("ai4").unwrap();
    let elem = list.parameters[0].clone();
    let ones = format!("[{}]", vec!["16843009"; 3000].join(","));
    let zeros = format!("[{}]", vec!["0"; 3000].join(","));
    for json in [&ones, &ones, &zeros, &zeros] {
        let v = read_json_with_schema(json, &list).unwrap();
        assert!(write_in_place(h, none, v, &elem, true, None).unwrap());
        let _ = shm::shfree(v);
        shared_flush_buffer(h).unwrap();
    }
    assert!(matches!(write_begin(h, none).unwrap(), Begun::Open));
    let room = write_place(4, std::ptr::null()).unwrap();
    unsafe { *(room.slot as *mut i32) = 7 };
    let (slots, k) = write_place_run(3, room.end).unwrap();
    assert_eq!(k, 3);
    for j in 0..k {
        unsafe { *(slots as *mut i32).add(j) = 8 + j as i32 };
    }
    let room = write_place(4, unsafe { slots.add(k * 4) }).unwrap();
    unsafe { *(room.slot as *mut i32) = 11 };
    let (_, none_placed) = write_place_run(0, room.end).unwrap();
    assert_eq!(none_placed, 0);
    write_commit(std::ptr::null()).unwrap();
    shared_close_handle(h).unwrap();
    let got = render_and_free(shared_load_stream_file_as_array(path.to_str().unwrap(), Some(&list)).unwrap(), &list).unwrap();
    let got: Vec<i64> = got.as_array().unwrap().iter().map(|v| v.as_i64().unwrap()).collect();
    let mut want = vec![16843009i64; 6000];
    want.extend(vec![0; 6000]);
    want.extend([7, 8, 9, 10, 11]);
    assert_eq!(got, want);
    let _ = std::fs::remove_dir_all(&dir);
}

/// A stream an eval arena discards while an in-place batch is open on the
/// thread is discarded when the batch ends (SLOT-36).
#[test]
fn an_arena_ended_inside_an_in_place_batch_discards_its_streams_after() {
    use crate::stream::{write_begin, write_commit, Begun};
    let _shm = crate::own_test_registry();
    let dir = std::env::temp_dir().join(format!("morloc_in_place_discard_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    let none = crate::compression::CompressionLevel::NONE;
    let out = shared_open_ostream_with_schema(dir.join("out.stream").to_str().unwrap(), "as").unwrap();
    let owned;
    {
        let _arena = crate::eval_arena::enter().unwrap();
        let cp = crate::eval_arena::checkpoint();
        owned = shared_open_ostream_with_schema(dir.join("owned.stream").to_str().unwrap(), "as").unwrap();
        crate::eval_arena::record_slot_if_active(owned);
        assert!(matches!(write_begin(out, none).unwrap(), Begun::Open));
        crate::eval_arena::rollback_to(cp);
        let slot = crate::stream::slot_ref(crate::stream::unpack_handle(owned).1).unwrap();
        assert_eq!(
            slot.generation.load(std::sync::atomic::Ordering::Acquire) & crate::stream::GENERATION_MASK,
            crate::stream::unpack_handle(owned).0,
            "the arena's stream was discarded inside the batch",
        );
        write_commit(std::ptr::null()).unwrap();
    }
    assert!(shared_close_handle(owned).is_err(), "the arena's stream outlived the batch");
    shared_close_handle(out).unwrap();
    let _ = std::fs::remove_dir_all(&dir);
}

/// A run of a fixed-width type wider than a sub-packet places one element
/// at a time, each in a sub-packet of its own, as the copy path writes it
/// (SLOT-36).
#[test]
fn a_run_of_elements_wider_than_a_sub_packet_is_placed_one_by_one() {
    let _shm = crate::own_test_registry();
    let _restore = buffer_bytes(4096);
    let dir = std::env::temp_dir().join(format!("morloc_in_place_wide_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    let elem_str = format!("t8{}", format!("t8{}", format!("t8{}", "f8".repeat(8)).repeat(8)).repeat(8));
    let list = parse_schema(&format!("a{elem_str}")).unwrap();
    let elem = list.parameters[0].clone();
    let row = |k: usize| format!("[{}]", (0..8).map(|j| format!("{}.5", k * 8 + j)).collect::<Vec<_>>().join(","));
    let block = |k: usize| format!("[{}]", (0..8).map(|i| row(k * 8 + i)).collect::<Vec<_>>().join(","));
    let elem_json = |k: usize| format!("[{}]", (0..8).map(|i| block(k * 8 + i)).collect::<Vec<_>>().join(","));
    let json = format!("[{}]", (0..3).map(elem_json).collect::<Vec<_>>().join(","));
    let mut files = Vec::new();
    for (tag, placing) in [("copy", false), ("place", true)] {
        let path = dir.join(format!("{tag}.stream"));
        let h = shared_open_ostream_with_schema(path.to_str().unwrap(), &format!("a{elem_str}")).unwrap();
        let v = read_json_with_schema(&json, &list).unwrap();
        if placing {
            assert!(write_in_place(h, crate::compression::CompressionLevel::NONE, v, &elem, true, None).unwrap());
        } else {
            shared_write_subpacket(h, crate::compression::CompressionLevel::NONE, v).unwrap();
        }
        let _ = shm::shfree(v);
        shared_close_handle(h).unwrap();
        let (mp, sz) = crate::stream_format::mmap_file_readonly(path.to_str().unwrap()).unwrap();
        let footer = crate::stream_format::try_read_footer(mp, sz).unwrap().unwrap();
        unsafe { libc::munmap(mp as *mut libc::c_void, sz as usize) };
        let mut bytes = std::fs::read(&path).unwrap();
        bytes.truncate(footer.footer_start as usize);
        files.push((bytes, format!("{:?}", footer.subpacket_entries)));
    }
    assert_eq!(files[0], files[1]);
    let _ = std::fs::remove_dir_all(&dir);
}

/// Sub-packets of `n` bytes until the guard drops.
fn buffer_bytes(n: usize) -> impl Drop {
    struct Restore;
    impl Drop for Restore {
        fn drop(&mut self) {
            crate::stream::set_test_write_buffer_bytes(None);
        }
    }
    crate::stream::set_test_write_buffer_bytes(Some(n));
    Restore
}

/// A batch still open when its call ends is a defect: it is aborted, so the
/// stream goes on without it, and the call panics (SLOT-36, PANIC-1).
#[test]
fn a_batch_left_open_at_the_end_of_its_call_is_aborted() {
    use crate::stream::{write_begin, write_place, Begun};
    let _shm = crate::own_test_registry();
    let dir = std::env::temp_dir().join(format!("morloc_in_place_leak_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    let none = crate::compression::CompressionLevel::NONE;
    let path = dir.join("l.stream");
    let h = shared_open_ostream_with_schema(path.to_str().unwrap(), "as").unwrap();
    let ended = std::thread::spawn(move || {
        assert!(matches!(write_begin(h, none).unwrap(), Begun::Open));
        let _ = write_place(16 + 3, std::ptr::null()).unwrap();
        crate::stream::pool_reclaim_stdio_after_dispatch();
    })
    .join();
    assert!(ended.is_err(), "a batch left open was not a defect");
    let v = strs_value(2, 3, 5);
    shared_write_subpacket(h, none, v).unwrap();
    let _ = shm::shfree(v);
    shared_close_handle(h).unwrap();
    assert_eq!(read_back(&path).as_array().unwrap().len(), 2);
    let _ = std::fs::remove_dir_all(&dir);
}
