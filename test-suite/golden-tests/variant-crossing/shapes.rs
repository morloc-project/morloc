#[derive(Clone)]
pub enum Shape {
    Circle(f64),
    Rect(f64, f64),
    Dot,
}

pub fn area(s: &Shape) -> f64 {
    match s {
        Shape::Circle(r) => 3.0 * r * r,
        Shape::Rect(w, h) => w * h,
        Shape::Dot => 0.0,
    }
}
