pub fn inc(x: i64) -> i64 {
    x + 1
}

pub fn add(x: i64, y: i64) -> i64 {
    x + y
}

pub fn join2(x: &String, y: &String) -> String {
    format!("{}{}", x, y)
}

pub fn count2(xs: &Vec<i64>) -> i64 {
    xs.len() as i64
}

pub fn ident(xs: &Vec<i64>) -> Vec<i64> {
    xs.clone()
}

pub fn mknames(n: i64) -> Vec<String> {
    (0..n).map(|i| format!("n{}", i)).collect()
}
