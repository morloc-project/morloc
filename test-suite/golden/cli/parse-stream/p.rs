pub fn produce<S: Fn(&Vec<i64>)>(path: &String, sink: S) {
    let text = std::fs::read_to_string(path)
        .unwrap_or_else(|e| rustmorloc::morloc_throw(format!("cannot open {}: {}", path, e)));
    let mut batch = Vec::new();
    for line in text.lines() {
        if line == "bad" {
            rustmorloc::morloc_throw(format!("bad line in {}", path));
        }
        batch.push(line.trim().parse::<i64>().unwrap());
        if batch.len() == 2 {
            sink(&batch);
            batch.clear();
        }
    }
    if !batch.is_empty() {
        sink(&batch);
    }
    std::fs::write(format!("{}.done", path), "done\n").unwrap();
}

pub fn done(path: &String) -> bool {
    std::path::Path::new(&format!("{}.done", path)).exists()
}

pub fn sum_ints(xs: &Vec<i64>) -> i64 {
    xs.iter().sum()
}
