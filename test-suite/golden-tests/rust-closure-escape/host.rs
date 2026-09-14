#[derive(Clone)]
pub struct Ops {
    pub inc: std::rc::Rc<dyn rustmorloc::MorlocFn1<i64, i64>>,
}

pub fn mark(s: &String) -> String {
    format!("{}!", s)
}

pub fn str_len(s: &String) -> i64 {
    s.len() as i64
}

pub fn use_ops(ops: &Ops, x: i64) -> i64 {
    ops.inc.call1(&x)
}

pub fn use_list(fs: &Vec<std::rc::Rc<dyn rustmorloc::MorlocFn1<i64, i64>>>, x: i64) -> i64 {
    fs[0].call1(&x) + fs[1].call1(&x)
}

pub fn apply_twice(f: impl rustmorloc::MorlocFn1<i64, i64>, x: i64) -> i64 {
    f.call1(&f.call1(&x))
}

pub fn tick(x: i64) -> i64 {
    x + 1
}
