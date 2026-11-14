fn gather_files(dir: &std::path::Path) -> i64 {
    let Ok(rd) = std::fs::read_dir(dir) else { return 0 };
    let mut n = 0;
    for e in rd.flatten() {
        let p = e.path();
        if p.is_dir() {
            n += gather_files(&p);
        } else if e.file_name().to_string_lossy().starts_with("morloc-gather-") {
            n += 1;
        }
    }
    n
}

pub fn produce_route<F: Fn(&Vec<i64>)>(sink: F) {
    sink(&vec![0, 1]);
    sink(&vec![]);
    sink(&vec![2, 3, 4]);
    let n = match std::env::var("MORLOC_TMPDIR") {
        Ok(d) => gather_files(std::path::Path::new(&d)),
        Err(_) => -1,
    };
    sink(&vec![1000 + n]);
}

pub fn show_ints(xs: &Vec<i64>) -> String {
    let mut out = String::from("rust");
    for x in xs {
        out.push_str(&format!(" {}", x));
    }
    out.push('\n');
    out
}

pub fn add_batch(acc: i64, xs: &Vec<i64>) -> i64 {
    acc + xs.len() as i64
}

pub fn merge(a: i64, b: i64) -> i64 {
    a + b
}

pub fn show_n(n: i64) -> String {
    format!("rust n={}\n", n)
}
