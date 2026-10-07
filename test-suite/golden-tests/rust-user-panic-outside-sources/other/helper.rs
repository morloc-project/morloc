pub fn helper_panics(x: i64) -> i64 {
    let v: Vec<i64> = vec![10, 20];
    v[x as usize]
}
