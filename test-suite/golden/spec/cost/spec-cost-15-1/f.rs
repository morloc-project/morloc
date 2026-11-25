pub fn rust_f(x: i64) -> i64 { x * 3 }
pub fn rust_map(f: impl Fn(&i64) -> i64, xs: &Vec<i64>) -> Vec<i64> { xs.iter().map(|x| f(x)).collect() }
pub fn rust_sum(xs: &Vec<i64>) -> i64 { xs.iter().sum() }
