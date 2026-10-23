pub struct Bx { pub v: i64 }
pub fn mki(x: i64) -> Bx { Bx { v: x } }
pub fn mks(x: i64) -> Bx { Bx { v: x } }
pub fn both(a: &Bx, b: &Bx) -> i64 { a.v + b.v }
