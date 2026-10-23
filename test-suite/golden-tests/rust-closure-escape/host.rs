#[derive(Clone)]
pub struct Ops {
    pub inc: std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>,
}

pub fn mark(s: &String) -> String {
    format!("{}!", s)
}

pub fn str_len(s: &String) -> i64 {
    s.len() as i64
}

pub fn use_ops(ops: &Ops, x: i64) -> i64 {
    (ops.inc)(&x)
}

pub fn use_list(fs: &Vec<std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>>, x: i64) -> i64 {
    fs[0](&x) + fs[1](&x)
}

pub fn apply_twice(f: impl Fn(&i64) -> i64, x: i64) -> i64 {
    f(&f(&x))
}

pub fn tick(x: i64) -> i64 {
    x + 1
}
