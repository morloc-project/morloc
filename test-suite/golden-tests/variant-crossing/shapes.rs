#[derive(Clone)]
pub enum Shape {
    Circle(Box<(f64,)>),
    Rect(Box<(f64, f64)>),
    Dot,
}

pub fn area(s: &Shape) -> f64 {
    match s {
        Shape::Circle(b) => 3.0 * b.0 * b.0,
        Shape::Rect(b) => b.0 * b.1,
        Shape::Dot => 0.0,
    }
}
