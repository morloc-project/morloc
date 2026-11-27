// The effectful tick, plus the aggregate whose partial application
// (`add (vsum xs)`) becomes the closure crossing to Python. `xs` (a Vec) is the
// captured value the reify path must serialize self-contained.
pub fn tick() -> i64 {
    0
}

pub fn add(a: i64, b: i64) -> i64 {
    a + b
}

pub fn vsum(xs: &Vec<i64>) -> i64 {
    xs.iter().sum()
}
