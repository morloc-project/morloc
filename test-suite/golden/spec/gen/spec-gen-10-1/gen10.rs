#[derive(Clone)]
pub struct Point { pub x: i64, pub y: i64 }
pub fn mkPoint(n: i64) -> Point { Point { x: n, y: n + 1 } }
pub fn idn<T: Clone>(x: &T) -> T { x.clone() }
