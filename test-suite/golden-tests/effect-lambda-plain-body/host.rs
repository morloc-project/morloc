#[derive(Clone)]
pub struct Ops {
    pub inc: std::rc::Rc<dyn rustmorloc::MorlocFn1<i64, i64>>,
    pub same: std::rc::Rc<dyn rustmorloc::MorlocFn1<i64, i64>>,
}

pub fn rs_apply_fn(f: impl rustmorloc::MorlocFn1<i64, i64>, n: i64) -> i64 {
    f.call1(&n)
}

pub fn rs_use_ops(ops: &Ops, n: i64) -> i64 {
    ops.inc.call1(&n) + ops.same.call1(&n)
}
