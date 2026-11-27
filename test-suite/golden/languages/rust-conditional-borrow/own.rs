// Sourced Rust functions for the rust-conditional-borrow golden. `is_long` is
// the conditional guard (forcing Rust realization of the arm); `cat` forces
// `pick` to be used more than once so it stays a nested manifold whose [Int]
// parameters arrive borrowed.
pub fn is_long(xs: &[i64]) -> bool {
    xs.len() > 2
}

pub fn cat(a: &[i64], b: &[i64]) -> Vec<i64> {
    let mut v = a.to_vec();
    v.extend_from_slice(b);
    v
}
