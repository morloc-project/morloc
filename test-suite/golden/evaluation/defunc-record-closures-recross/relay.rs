pub fn forward(fs: &Vec<std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>>) -> Vec<std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>> {
    fs.clone()
}
