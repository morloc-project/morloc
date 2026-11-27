use std::sync::Arc;

pub fn make_adder(n: i64, x: i64) -> i64 {
    n + x
}

pub fn make_adder1(n: i64) -> Arc<dyn Fn(&i64) -> i64 + Send + Sync> {
    Arc::new(move |x: &i64| n + x)
}
