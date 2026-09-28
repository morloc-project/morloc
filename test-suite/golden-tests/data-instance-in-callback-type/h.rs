pub fn note(x: i64) -> i64 {
    x
}

pub fn call_on<R, F: Fn(&i64) -> R>(f: F, x: i64) -> i64 {
    f(&x);
    x
}
