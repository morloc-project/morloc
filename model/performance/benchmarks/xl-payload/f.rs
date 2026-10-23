pub fn mk_reals(n: i64) -> Vec<f64> {
    (0..n).map(|i| i as f64).collect()
}

pub fn len_reals(xs: &Vec<f64>) -> i64 {
    xs.len() as i64
}

pub fn mk_str(n: i64) -> String {
    "x".repeat(n as usize)
}

pub fn len_str(s: &String) -> i64 {
    s.len() as i64
}

pub fn mk_strs(n: i64) -> Vec<String> {
    vec!["abcdefgh".to_string(); n as usize]
}

pub fn len_strs(xs: &Vec<String>) -> i64 {
    xs.len() as i64
}
