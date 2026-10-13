// A user-mapped record `record Rust => Ops = "Ops"` whose fields are runtime
// closures, stored in the fat trait object morloc uses for a function value.
pub struct Ops {
    pub inc: std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>,
    pub scale: std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>,
}

pub fn tick() -> i64 {
    0
}

pub fn add(a: i64, b: i64) -> i64 {
    a + b
}

pub fn mul(a: i64, b: i64) -> i64 {
    a * b
}
