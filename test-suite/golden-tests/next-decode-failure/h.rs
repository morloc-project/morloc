pub fn apply_once<F: Fn(&i64) -> i64>(f: F, x: i64) -> i64 {
    f(&x)
}
