pub fn make_and_apply<G: Fn(&i64) -> std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>>(g: G, x: i64) -> i64 {
    g(&x)(&x)
}

pub fn make_and_apply3<
    G: Fn(&i64) -> std::sync::Arc<dyn Fn(&i64) -> std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync> + Send + Sync>,
>(g: G, x: i64) -> i64 {
    let h = g(&x);
    let k = h(&x);
    k(&x)
}
