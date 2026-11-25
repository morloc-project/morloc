#[derive(Clone)]
pub struct Ops {
    pub inc: std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>,
    pub total: std::sync::Arc<dyn Fn() -> i64 + Send + Sync>,
}

pub fn rs_double(x: i64) -> i64 {
    2 * x
}

pub fn rs_tick(k: i64) -> i64 {
    100 + k
}

pub fn rs_use_ops(ops: &Ops, n: i64) -> i64 {
    (ops.inc)(&n) + (ops.total)()
}
