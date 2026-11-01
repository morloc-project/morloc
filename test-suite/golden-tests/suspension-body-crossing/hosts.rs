pub fn take_thunk(t: impl Fn() -> i64) -> i64 {
    t()
}
