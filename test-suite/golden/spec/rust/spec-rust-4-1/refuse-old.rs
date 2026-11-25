pub enum Sh { Circle(Box<(f64,)>), Dot }
pub fn mk(n: i64) -> Sh { Sh::Circle(Box::new((n as f64,))) }
