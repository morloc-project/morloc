pub fn re_emit(x: i64) -> i64 {
    eprintln!("emit {}", x);
    x
}

pub fn re_nop() {}

// Appends one byte and returns the file's size, so a caller can count how many
// times a suspension was actually run.
pub fn re_tick(path: &String) -> i64 {
    use std::io::Write;
    let mut f = std::fs::OpenOptions::new()
        .append(true).create(true).open(path).unwrap();
    f.write_all(b"x").unwrap();
    std::fs::metadata(path).unwrap().len() as i64
}
