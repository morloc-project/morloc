use std::sync::Arc;

#[derive(Clone)]
pub struct Ops {
    pub run: Arc<dyn Fn(&i64) -> i64 + Send + Sync>,
    pub later: Arc<dyn Fn() -> i64 + Send + Sync>,
}

pub fn make_adder(k: i64) -> impl Fn(&i64) -> i64 + Send + Sync + 'static {
    move |x| x + k
}

pub fn use_ops(ops: &Ops, x: i64) -> i64 {
    let ops = ops.clone();
    std::thread::spawn(move || (ops.run)(&x) + (ops.later)()).join().unwrap()
}

pub fn relay(fs: &Vec<Arc<dyn Fn(&i64) -> i64 + Send + Sync>>) -> Vec<Arc<dyn Fn(&i64) -> i64 + Send + Sync>> {
    fs.iter().rev().cloned().collect()
}

pub fn on_thread<F: Fn(&i64) -> i64 + Send + Sync + 'static>(f: F, x: i64) -> i64 {
    std::thread::spawn(move || f(&x)).join().unwrap()
}

pub fn apply_twice(f: impl Fn(&i64) -> i64, x: i64) -> i64 {
    f(&f(&x))
}

pub fn checked(x: i64) -> i64 {
    if x > 100 {
        panic!("input {x} is over 100");
    }
    x
}
