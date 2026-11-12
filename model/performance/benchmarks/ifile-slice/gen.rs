pub fn gen_reals(n: i64) -> Vec<f64> {
    (0..n).map(|i| (i + 1) as f64 * 0.5).collect()
}

pub fn gen_strs(n: i64) -> Vec<String> {
    (0..n).map(|i| ["A", "C", "G", "T"][(i % 4) as usize].repeat(8 + (i % 17) as usize)).collect()
}
