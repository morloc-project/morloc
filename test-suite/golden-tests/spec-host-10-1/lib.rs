use std::sync::Arc;

pub fn run_cb(f: &Arc<dyn Fn(&i64) -> i64 + Send + Sync>) -> i64 {
    f(&1) + 1
}

pub fn tick(x: i64) -> i64 {
    x + 40
}
