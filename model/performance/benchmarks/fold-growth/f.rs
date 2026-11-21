pub fn produce<F: Fn(&Vec<i64>)>(s: i64, b: i64, sink: F) {
    for i in 0..b {
        sink(&(0..s).map(|k| i * s + k).collect());
    }
}

pub fn batches(s: i64, b: i64) -> Vec<Vec<i64>> {
    (0..b).map(|i| (0..s).map(|k| i * s + k).collect()).collect()
}

pub fn empty() -> Vec<i64> {
    Vec::new()
}

pub fn step_ref(acc: &Vec<i64>, xs: &Vec<i64>) -> Vec<i64> {
    let mut a = acc.clone();
    a.extend_from_slice(xs);
    a
}

pub fn cat(a: &Vec<i64>, b: &Vec<i64>) -> Vec<i64> {
    let mut c = a.clone();
    c.extend_from_slice(b);
    c
}

pub fn len_of(a: &Vec<i64>) -> String {
    a.len().to_string()
}
