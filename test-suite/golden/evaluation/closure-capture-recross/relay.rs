pub fn forward_fn<F>(f: F) -> std::sync::Arc<dyn Fn(&i64) -> String + Send + Sync>
where
    F: Fn(&i64) -> String + Send + Sync + 'static,
{
    std::sync::Arc::new(f)
}
