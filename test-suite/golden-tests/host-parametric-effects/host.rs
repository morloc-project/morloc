pub fn tick(path: &String) -> i64 {
    use std::io::Write;
    let mut f = std::fs::OpenOptions::new()
        .append(true)
        .create(true)
        .open(path)
        .unwrap();
    f.write_all(b"x").unwrap();
    std::fs::metadata(path).unwrap().len() as i64
}

pub fn count(path: &String) -> i64 {
    std::fs::metadata(path).map(|m| m.len() as i64).unwrap_or(0)
}

pub fn h_pass<A, B>(f: impl rustmorloc::MorlocFn1<A, B>, x: &A) -> B {
    f.call1(x)
}
