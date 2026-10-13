#[derive(Clone)]
pub struct Ops {
    pub inc: std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>,
    pub same: std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>,
}

pub fn rs_apply_fn(f: impl Fn(&i64) -> i64, n: i64) -> i64 {
    f(&n)
}

pub fn rs_use_ops(ops: &Ops, n: i64) -> i64 {
    (ops.inc)(&n) + (ops.same)(&n)
}
