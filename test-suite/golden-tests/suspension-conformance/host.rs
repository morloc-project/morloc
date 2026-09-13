#[derive(Clone)]
pub struct Holder {
    pub run: std::rc::Rc<dyn rustmorloc::MorlocFn0<i64>>,
}

#[derive(Clone)]
pub struct Caller {
    pub inc: std::rc::Rc<dyn rustmorloc::MorlocFn1<i64, i64>>,
}

pub fn tick(path: &String) -> i64 {
    use std::io::Write;
    let mut f = std::fs::OpenOptions::new()
        .append(true)
        .create(true)
        .open(path)
        .unwrap();
    f.write_all(b"x").unwrap();
    std::fs::metadata(path).unwrap().len() as i64
}

pub fn mark(s: &String) -> String {
    s.clone()
}

pub fn ident(x: i64) -> i64 {
    x
}

pub fn take_thunk(t: impl rustmorloc::MorlocFn0<i64>) -> i64 {
    t.call0();
    t.call0();
    t.call0()
}

pub fn use_record(h: &Holder) -> i64 {
    h.run.call0();
    h.run.call0();
    h.run.call0()
}

pub fn use_list(xs: &Vec<std::rc::Rc<dyn rustmorloc::MorlocFn0<i64>>>) -> i64 {
    xs[0].call0();
    xs[0].call0();
    xs[0].call0()
}

pub fn use_callback(f: impl rustmorloc::MorlocFn1<i64, i64>, x: i64) -> i64 {
    f.call1(&x);
    f.call1(&x);
    f.call1(&x)
}

pub fn use_pinned(c: &Caller, x: i64) -> i64 {
    c.inc.call1(&x);
    c.inc.call1(&x);
    c.inc.call1(&x)
}
