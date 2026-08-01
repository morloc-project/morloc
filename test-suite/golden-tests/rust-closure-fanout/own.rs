// Sourced Rust function for the rust-closure-fanout golden: the body of the
// shared closure that is passed to `map` at two sites.
pub fn inc(x: i64) -> i64 {
    x + 1
}
