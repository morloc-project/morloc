// The consumer receives closures reflected by the Rust member as
// `Arc<dyn Fn>` proxies and calls them as `f(&x)` (each call RPCs
// back to the Python home pool). A user-mapped `record Rust => Ops = "Ops"` needs
// its struct here.

// Clone is derived because `crossListOfRecords` places records in a Vec (list
// elements are cloned); the function fields are themselves Clone.
#[derive(Clone)]
pub struct Ops {
    pub inc: std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>,
    pub scale: std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>,
}

pub fn use_ops(ops: &Ops, x: i64) -> i64 {
    (ops.inc)(&x) + (ops.scale)(&x)
}

pub fn use_list(fs: &Vec<std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>>, x: i64) -> i64 {
    fs[0](&x) + fs[1](&x)
}

pub fn use_tuple(
    t: &(std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>, std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>),
    x: i64,
) -> i64 {
    (t.0)(&x) + (t.1)(&x)
}

pub fn use_opt(f: &Option<std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>>, x: i64) -> i64 {
    match f {
        Some(g) => g(&x),
        None => x,
    }
}

pub fn use_list_of_records(l: &Vec<Ops>, x: i64) -> i64 {
    (l[0].inc)(&x) + (l[1].scale)(&x)
}
