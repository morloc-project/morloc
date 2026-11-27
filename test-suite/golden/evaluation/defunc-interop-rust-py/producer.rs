// The effectful tick and the multiply whose partial application (`mul base`)
// becomes the closure crossing to Python.
pub fn tick() -> i64 {
    0
}

pub fn mul(a: i64, b: i64) -> i64 {
    a * b
}
