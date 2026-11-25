#[derive(Clone)]
pub struct Holder {
    pub run: std::sync::Arc<dyn Fn() -> i64 + Send + Sync>,
}

#[derive(Clone)]
pub struct Caller {
    pub inc: std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>,
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

pub fn take_thunk(t: impl Fn() -> i64) -> i64 {
    t();
    t();
    t()
}

pub fn use_record(h: &Holder) -> i64 {
    (h.run)();
    (h.run)();
    (h.run)()
}

pub fn use_list(xs: &Vec<std::sync::Arc<dyn Fn() -> i64 + Send + Sync>>) -> i64 {
    xs[0]();
    xs[0]();
    xs[0]()
}

pub fn use_callback(f: impl Fn(&i64) -> i64, x: i64) -> i64 {
    f(&x);
    f(&x);
    f(&x)
}

pub fn use_pinned(c: &Caller, x: i64) -> i64 {
    (c.inc)(&x);
    (c.inc)(&x);
    (c.inc)(&x)
}
