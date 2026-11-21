pub fn produce<F: Fn(&Vec<i64>)>(s: i64, b: i64, sink: F) {
    for i in 0..b {
        sink(&(0..s).map(|k| i * s + k).collect());
    }
}

pub fn empty() -> Vec<i64> {
    Vec::new()
}

pub fn step_val(mut acc: Vec<i64>, xs: &Vec<i64>) -> Vec<i64> {
    acc.extend_from_slice(xs);
    acc
}

pub fn cat(mut a: Vec<i64>, b: Vec<i64>) -> Vec<i64> {
    a.extend(b);
    a
}

pub fn len_of(a: &Vec<i64>) -> String {
    a.len().to_string()
}
