#[derive(Clone)]
pub struct Ops {
    pub inc: std::rc::Rc<dyn rustmorloc::MorlocFn1<i64, i64>>,
    pub scale: std::rc::Rc<dyn rustmorloc::MorlocFn1<i64, i64>>,
}

pub fn rs_use_ops(ops: &Ops, n: i64) -> i64 {
    ops.inc.call1(&n) + ops.scale.call1(&n)
}

pub fn rs_use_list(fs: &Vec<std::rc::Rc<dyn rustmorloc::MorlocFn1<i64, i64>>>, n: i64) -> i64 {
    fs[0].call1(&n) + fs[1].call1(&n)
}
