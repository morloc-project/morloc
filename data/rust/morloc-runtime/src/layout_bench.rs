//! Timing of the stream write and read paths over values with different
//! pointer layouts. Ignored by default; run in release mode:
//!
//!   cargo test --release -p morloc-runtime --lib layout_bench -- \
//!       --ignored --nocapture --test-threads=1
//!
//! Each line reports the minimum and median of three runs in milliseconds.
//! `MORLOC_BENCH_SCALE` divides every element count (default 1) for a quick
//! run; `MORLOC_BENCH_ONLY` is a comma-separated list of case names to run.
//! Pointer-bearing values put a non-empty element first.

use crate::intrinsics::IFileWalkArg;
use crate::json::read_json_with_schema;
use crate::shm;
use crate::shm_types::AbsPtr;
use crate::stream::{
    shared_close_handle, shared_discard_handle, shared_ifile_walk,
    shared_load_stream_file_as_array, shared_next_frame, shared_open_ifile,
    shared_open_istream, shared_open_ostream_with_schema, shared_write_subpacket,
};
use morloc_runtime_types::schema::parse_schema;
use std::time::Instant;

const REPS: usize = 3;

fn scale() -> usize {
    std::env::var("MORLOC_BENCH_SCALE").ok().and_then(|s| s.parse().ok()).unwrap_or(1).max(1)
}

fn time<F: FnMut()>(mut f: F) -> (f64, f64) {
    let mut ms: Vec<f64> = (0..REPS)
        .map(|_| {
            let t = Instant::now();
            f();
            t.elapsed().as_secs_f64() * 1e3
        })
        .collect();
    ms.sort_by(|a, b| a.partial_cmp(b).unwrap());
    (ms[0], ms[REPS / 2])
}

fn report(case: &str, op: &str, (min, med): (f64, f64)) {
    println!("BENCH {case:<14} {op:<8} min {min:>10.2} ms   median {med:>10.2} ms");
}

/// A `[T]` of `n` fixed-width elements of `width` bytes, each byte `fill`.
fn flat_array(n: usize, width: usize, fill: u8) -> AbsPtr {
    let (block, data) = crate::stream::alloc_array_block(n * width).unwrap();
    unsafe {
        std::ptr::write_bytes(data, fill, n * width);
        let arr = &mut *(block as *mut crate::shm_types::Array);
        arr.size = n;
        arr.data = shm::abs2rel(data).unwrap();
    }
    block
}

fn selected(case: &str) -> bool {
    match std::env::var("MORLOC_BENCH_ONLY") {
        Ok(only) => only.split(',').any(|c| c == case),
        Err(_) => true,
    }
}

/// Write `value` once, then time every reader over the file.
fn bench_case(case: &str, list_schema_str: &str, value: AbsPtr, n: usize, dir: &std::path::Path) {
    let path = dir.join(format!("{case}.stream"));
    let path = path.to_str().unwrap();

    report(case, "write", time(|| {
        let _ = std::fs::remove_file(path);
        let h = shared_open_ostream_with_schema(path, list_schema_str).unwrap();
        shared_write_subpacket(h, 0, value).unwrap();
        shared_close_handle(h).unwrap();
    }));

    report(case, "@next", time(|| {
        let h = shared_open_istream(path).unwrap();
        while let Some(p) = shared_next_frame(h).unwrap() {
            shm::shfree(p).unwrap();
        }
        shared_discard_handle(h).unwrap();
    }));

    report(case, "@load", time(|| {
        let p = shared_load_stream_file_as_array(path).unwrap();
        shm::shfree(p).unwrap();
    }));

    let f = shared_open_ifile(path).unwrap();
    let (i, j) = (n as i64 / 4, 3 * n as i64 / 4);
    report(case, "slice", time(|| {
        let args = [IFileWalkArg::opt(Some(i)), IFileWalkArg::opt(Some(j)), IFileWalkArg::opt(None)];
        let p = shared_ifile_walk(f, ".[:]", &args).unwrap();
        shm::shfree(p).unwrap();
    }));
    report(case, "index", time(|| {
        let p = shared_ifile_walk(f, ".[]", &[IFileWalkArg::opt(Some(n as i64 / 2))]).unwrap();
        shm::shfree(p).unwrap();
    }));
    shared_discard_handle(f).unwrap();
    let _ = std::fs::remove_file(path);
}

fn json_case(case: &str, elem: &str, n: usize, item: impl Fn(usize) -> String, dir: &std::path::Path) {
    if !selected(case) {
        return;
    }
    let list = format!("a{elem}");
    let schema = parse_schema(&list).unwrap();
    let mut json = String::from("[");
    for k in 0..n {
        if k > 0 {
            json.push(',');
        }
        json.push_str(&item(k));
    }
    json.push(']');
    let value = read_json_with_schema(&json, &schema).unwrap();
    drop(json);
    bench_case(case, &list, value, n, dir);
    shm::shfree(value).unwrap();
}

fn word(k: usize, len: usize) -> String {
    (0..len).map(|i| char::from(b'a' + ((k * 7 + i * 13) % 26) as u8)).collect()
}

#[test]
#[ignore]
fn layout_bench() {
    let _shm = crate::own_test_registry();
    let s = scale();
    let dir = std::env::temp_dir().join(format!("morloc_layout_bench_{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();

    // Flat element types: the whole sub-packet should move as one memcpy.
    if selected("flat-bool") {
        let n = (128 << 20) / s;
        let v = flat_array(n, 1, 1);
        bench_case("flat-bool", "ab", v, n, &dir);
        shm::shfree(v).unwrap();
    }
    if selected("flat-real") {
        let n = (16 << 20) / s;
        let v = flat_array(n, 8, 0);
        bench_case("flat-real", "af8", v, n, &dir);
        shm::shfree(v).unwrap();
    }

    // Strings: one pointer per element. 1% empty, never the first.
    json_case("str", "s", 500_000 / s, |k| {
        if k % 100 == 99 { "\"\"".into() } else { format!("\"{}\"", word(k, 16 + k % 17)) }
    }, &dir);

    // The record shape the bulk slice path was written for.
    json_case("rec-s-u8-u8", "t3sau1au1", 250_000 / s, |k| {
        format!("[\"{}\",[{},{},{}],[{}]]", word(k, 12), k % 256, (k + 1) % 256, (k + 2) % 256, k % 7)
    }, &dir);

    // Variants: a pointer to a payload per element.
    let shape = "v26Circle1f84Rect2f8f8";
    json_case("shape", shape, 250_000 / s, |k| {
        if k % 2 == 0 { format!("{{\"Circle\":[{}.5]}}", k % 100) }
        else { format!("{{\"Rect\":[{}.0,{}.25]}}", k % 50, k % 30) }
    }, &dir);
    json_case("int-shape", &format!("t2i8{shape}"), 250_000 / s, |k| {
        format!("[{k},{{\"Circle\":[{}.5]}}]", k % 100)
    }, &dir);

    // A recursive variant: full binary trees of depth 4.
    fn tree(d: usize, k: usize) -> String {
        if d == 0 { "\"Leaf\"".into() }
        else { format!("{{\"Node\":[{},{},{}]}}", k, tree(d - 1, k * 2), tree(d - 1, k * 2 + 1)) }
    }
    json_case("tree", "&4Treev24Leaf04Node3i8^4Tree^4Tree", 50_000 / s, |k| tree(4, k), &dir);

    let _ = std::fs::remove_dir_all(&dir);
}
