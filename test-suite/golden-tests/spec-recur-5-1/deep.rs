// Plain recursion, one frame per level, as a native program would write it.
#[inline(never)]
pub fn depth(n: i64) -> i64 {
    if n == 0 {
        return 0;
    }
    std::hint::black_box(1 + depth(n - 1))
}
