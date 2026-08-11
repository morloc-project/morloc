pub fn vsum(xs: &Vec<i64>) -> i64 {
    xs.iter().sum()
}

pub fn add_i(a: i64, b: i64) -> i64 {
    a + b
}

pub fn sub_i(a: i64, b: i64) -> i64 {
    a - b
}

pub fn eq_i(a: i64, b: i64) -> bool {
    a == b
}

pub fn is_even(a: i64) -> bool {
    a % 2 == 0
}

pub fn half(a: i64) -> i64 {
    a / 2
}

pub fn triple1(a: i64) -> i64 {
    3 * a + 1
}
