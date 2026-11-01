use std::io::Write;

pub fn apply2<F: Fn(&i64) -> i64>(f: F, x: i64) -> i64 {
    f(&x) + f(&x)
}

pub fn tick(x: i64) -> i64 {
    let path = std::env::var("TICK_LOG").unwrap();
    let mut f = std::fs::OpenOptions::new().create(true).append(true).open(path).unwrap();
    f.write_all(b"tick\n").unwrap();
    x
}

pub fn ticks(_x: i64) -> i64 {
    let path = std::env::var("TICK_LOG").unwrap();
    std::fs::read_to_string(path).map(|s| s.lines().count() as i64).unwrap_or(0)
}
