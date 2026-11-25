// Concatenate two Int vectors; the closure body applies it with the captured
// `pivot` as the left argument.
pub fn cat(a: &[i64], b: &[i64]) -> Vec<i64> {
    let mut v = a.to_vec();
    v.extend_from_slice(b);
    v
}
