// A user-mapped record `record Rust => Ops = "Ops"` whose fields are runtime
// closures. Each field is the fat trait object morloc stores a function value in
// (`Arc<dyn Fn>`); morloc builds the struct literal and reifies each field
// on serialization.
pub struct Ops {
    pub inc: std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>,
    pub scale: std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>,
}

// The effectful tick plus the arithmetic whose partial applications
// (`add a`, `mul b`) become the closures crossing to Python.
pub fn tick() -> i64 {
    0
}

pub fn add(a: i64, b: i64) -> i64 {
    a + b
}

pub fn mul(a: i64, b: i64) -> i64 {
    a * b
}
