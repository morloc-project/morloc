// The consumer receives closures reflected by the Rust member as
// `Rc<dyn MorlocFnN>` proxies and applies them with `.call1(&x)` (each call RPCs
// back to the C++ home pool). A user-mapped `record Rust => Ops = "Ops"` needs its
// struct here; do NOT `use rustmorloc::MorlocFnN` (the generated pool already
// imports it) -- refer to the trait fully qualified.

pub struct Ops {
    pub inc: std::rc::Rc<dyn rustmorloc::MorlocFn1<i64, i64>>,
    pub scale: std::rc::Rc<dyn rustmorloc::MorlocFn1<i64, i64>>,
}

pub fn use_ops(ops: &Ops, x: i64) -> i64 {
    ops.inc.call1(&x) + ops.scale.call1(&x)
}

pub fn use_list(fs: &Vec<std::rc::Rc<dyn rustmorloc::MorlocFn1<i64, i64>>>, x: i64) -> i64 {
    fs[0].call1(&x) + fs[1].call1(&x)
}

pub fn use_tuple(
    t: &(std::rc::Rc<dyn rustmorloc::MorlocFn1<i64, i64>>, std::rc::Rc<dyn rustmorloc::MorlocFn1<i64, i64>>),
    x: i64,
) -> i64 {
    t.0.call1(&x) + t.1.call1(&x)
}

pub fn use_opt(f: &Option<std::rc::Rc<dyn rustmorloc::MorlocFn1<i64, i64>>>, x: i64) -> i64 {
    match f {
        Some(g) => g.call1(&x),
        None => x,
    }
}
