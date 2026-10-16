// black_box keeps the optimizer from turning `1 + depth(n - 1)` into a loop,
// so every level is a real frame at -O2.
pub fn depth(n: i64) -> i64 {
    if n == 0 { 0 } else { 1 + std::hint::black_box(depth(n - 1)) }
}
