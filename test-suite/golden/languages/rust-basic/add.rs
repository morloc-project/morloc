// Sourced Rust functions for the rust-basic golden. Exercises the v1 wire
// surface: scalar Int, Vec<f64>, tuple result, and Option result. Argument
// passing is idiomatic: Copy scalars by value (`i64`), non-Copy read-only
// arguments by shared reference (`&[T]`) so a value can fan out with no copy.
pub fn add_i(x: i64, y: i64) -> i64 {
    x + y
}

pub fn mean_real(xs: &[f64]) -> f64 {
    if xs.is_empty() {
        0.0
    } else {
        xs.iter().sum::<f64>() / xs.len() as f64
    }
}

pub fn minmax(xs: &[i64]) -> (i64, i64) {
    let lo = *xs.iter().min().unwrap_or(&0);
    let hi = *xs.iter().max().unwrap_or(&0);
    (lo, hi)
}

pub fn safe_head(xs: &[i64]) -> Option<i64> {
    xs.first().copied()
}

pub fn append(a: &[i64], b: &[i64]) -> Vec<i64> {
    let mut v = a.to_vec();
    v.extend_from_slice(b);
    v
}
