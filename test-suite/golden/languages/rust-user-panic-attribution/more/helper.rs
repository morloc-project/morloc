pub fn helper_panics(x: i64) -> i64 {
    let v: Vec<i64> = Vec::new();
    v[x as usize]
}
