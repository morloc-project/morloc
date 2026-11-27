pub fn readings_rust(i: i64) -> Vec<i64> {
    (i..i + 3000).collect()
}

pub fn add_total_rust(xs: &Vec<i64>, x: i64) -> i64 {
    xs.iter().sum::<i64>() + x
}

pub fn id_rust(xs: &Vec<i64>) -> Vec<i64> {
    xs.clone()
}
