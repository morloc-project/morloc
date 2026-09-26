pub fn rs_apply<F: Fn(&i64) -> i64>(f: F, x: i64) -> i64 { f(&x) }
pub fn rs_inc(x: i64) -> i64 { x + 1 }
