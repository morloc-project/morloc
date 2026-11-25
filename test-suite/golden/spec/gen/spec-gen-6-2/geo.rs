use crate::mlc::types::_local::main::{Person, Shape};
pub fn person_age(p: &Person) -> i64 { p.age }
pub fn shape_area(s: &Shape) -> f64 {
  match s { Shape::Circle(c) => 3.0 * c.0 * c.0, Shape::Rect(r) => r.0 * r.1 }
}
