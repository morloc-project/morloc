// A recoverable failure is raised with `morloc_throw`; a panic is a bug.
pub fn read_csv(path: &String) -> Vec<i64> {
    let text = std::fs::read_to_string(path)
        .unwrap_or_else(|e| rustmorloc::morloc_throw(format!("cannot open {}: {}", path, e)));
    text.trim()
        .split(',')
        .map(|f| f.parse::<i64>().unwrap_or_else(|_| rustmorloc::morloc_throw(format!("not a number: {}", f))))
        .collect()
}

pub fn produce<S: Fn(&Vec<i64>)>(path: &String, sink: S) {
    let text = std::fs::read_to_string(path).unwrap_or_else(|e| panic!("cannot open {}: {}", path, e));
    let mut batch = Vec::new();
    for line in text.lines() {
        batch.push(line.trim().parse::<i64>().unwrap());
        if batch.len() == 2 {
            sink(&batch);
            batch.clear();
        }
    }
    if !batch.is_empty() {
        sink(&batch);
    }
}

pub fn sum_ints(xs: &Vec<i64>) -> i64 {
    xs.iter().sum()
}

pub fn size(xs: &Vec<i64>) -> i64 {
    xs.len() as i64
}
