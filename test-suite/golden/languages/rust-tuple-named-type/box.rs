#[derive(Clone)]
pub struct R {
    pub b: Vec<i64>,
    pub n: i64,
}

pub fn mkbox(n: i64) -> Vec<i64> {
    (0..n).collect()
}

pub fn boxlen(b: &Vec<i64>) -> i64 {
    b.len() as i64
}

pub fn mkrec(n: i64) -> R {
    R { b: (0..n).collect(), n }
}
