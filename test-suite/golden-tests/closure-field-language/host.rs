#[derive(Clone)]
pub struct Ops {
    pub inc: rustmorloc::FnValue1<i64, i64>,
    pub total: rustmorloc::FnValue0<i64>,
}

pub fn rs_double(x: i64) -> i64 {
    2 * x
}

pub fn rs_tick(k: i64) -> i64 {
    100 + k
}

pub fn rs_use_ops(ops: &Ops, n: i64) -> i64 {
    ops.inc.call1(&n) + ops.total.call0()
}
