// Sourced Rust function for the rust-optional-widen golden. Returns a plain
// Vec; the morloc signature widens the result to an optional, exercising the
// CoerceToOptional lowering in the Rust member.
pub fn scale(xs: &[i64]) -> Vec<i64> {
    xs.iter().map(|x| x * 2).collect()
}
