// The consumer receives closures reflected by the Rust member as
// `FnValueN` proxies and applies them with `.call1(&x)` (each call RPCs
// back to the Python home pool). A user-mapped `record Rust => Ops = "Ops"` needs
// its struct here; do NOT `use rustmorloc::MorlocFnN` (the generated pool already
// imports it) -- refer to the trait fully qualified.

// Clone is derived because `crossListOfRecords` places records in a Vec (list
// elements are cloned); the FnValueN fields are themselves Clone.
#[derive(Clone)]
pub struct Ops {
    pub inc: rustmorloc::FnValue1<i64, i64>,
    pub scale: rustmorloc::FnValue1<i64, i64>,
}

pub fn use_ops(ops: &Ops, x: i64) -> i64 {
    ops.inc.call1(&x) + ops.scale.call1(&x)
}

pub fn use_list(fs: &Vec<rustmorloc::FnValue1<i64, i64>>, x: i64) -> i64 {
    fs[0].call1(&x) + fs[1].call1(&x)
}

pub fn use_tuple(
    t: &(rustmorloc::FnValue1<i64, i64>, rustmorloc::FnValue1<i64, i64>),
    x: i64,
) -> i64 {
    t.0.call1(&x) + t.1.call1(&x)
}

pub fn use_opt(f: &Option<rustmorloc::FnValue1<i64, i64>>, x: i64) -> i64 {
    match f {
        Some(g) => g.call1(&x),
        None => x,
    }
}

pub fn use_list_of_records(l: &Vec<Ops>, x: i64) -> i64 {
    l[0].inc.call1(&x) + l[1].scale.call1(&x)
}
