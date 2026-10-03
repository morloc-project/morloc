pub fn readings_rust(i: i64) -> Vec<i64> {
    (i..i + 3000).collect()
}

pub fn total_rust(xs: &Vec<i64>) -> i64 {
    xs.iter().sum()
}
