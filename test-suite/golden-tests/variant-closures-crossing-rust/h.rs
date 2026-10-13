#[derive(Clone)]
pub struct Ops {
    pub inc: rustmorloc::FnValue1<i64, i64>,
    pub scale: rustmorloc::FnValue1<i64, i64>,
}

pub fn ident<T: Clone>(x: &T) -> T {
    x.clone()
}

pub fn add(a: i64, b: i64) -> i64 {
    a + b
}

pub fn mul(a: i64, b: i64) -> i64 {
    a * b
}
