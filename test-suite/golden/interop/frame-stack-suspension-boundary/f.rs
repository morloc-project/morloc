pub fn boom(x: i64) -> i64 {
    if x == 0 {
        panic!("boom");
    }
    x
}

// Runs the suspension and discards its failure.
pub fn attempt(thunk: impl Fn() -> i64) -> i64 {
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| thunk())) {
        Ok(v) => v,
        Err(_) => -1,
    }
}

// Runs the suspension on a thread of its own and raises its failure here.
pub fn on_thread(thunk: impl Fn() -> i64 + Send + Sync) -> i64 {
    std::thread::scope(|s| match s.spawn(|| thunk()).join() {
        Ok(v) => v,
        Err(p) => std::panic::resume_unwind(p),
    })
}
