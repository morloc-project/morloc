pub fn scale(k: i64, xs: &Vec<i64>) -> Vec<i64> {
    xs.iter().map(|x| k * x).collect()
}

pub fn scale_r(k: i64) -> impl Fn(&Vec<i64>) -> Vec<i64> {
    move |xs: &Vec<i64>| scale(k, xs)
}

pub fn mk3(a: i64, b: i64, c: i64) -> i64 {
    a * 100 + b * 10 + c
}

pub fn mk_r(a: i64) -> impl Fn(&i64, &i64) -> i64 {
    move |b: &i64, c: &i64| 1000 + a * 100 + b * 10 + c
}
