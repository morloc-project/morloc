pub fn n0(x: i64) -> i64 { x + 1 }
pub fn n1(x: i64) -> i64 { x + 10 }
pub fn n2(x: i64) -> i64 { x + 100 }
pub fn n3(x: i64) -> i64 { x + 1000 }
pub fn n4(x: i64) -> i64 { x + 10000 }
pub fn s0(x: i64) -> i64 { x + 100000 }
pub fn s1(x: i64) -> i64 { x + 1000000 }
pub fn helper0(x: i64) -> i64 { x + 10000000 }
pub fn twice(f: impl Fn(&i64) -> i64, x: i64) -> i64 { f(&f(&x)) }
pub fn __e(x: i64) -> i64 { x + 1 }
pub fn __v(x: i64) -> i64 { x + 10 }
pub fn i2(x: i64) -> i64 { x + 100 }
#[derive(Clone, Debug)]
pub struct MyW(pub i64);
pub fn i1(x: i64) -> MyW { MyW(x * 2) }
pub fn i0(w: &MyW) -> i64 { w.0 + 1 }
pub fn wsum(ws: &Vec<MyW>) -> i64 { ws.iter().map(|w| w.0).sum() }
pub fn wback(xs: &Vec<i64>) -> Vec<MyW> { xs.iter().map(|x| MyW(*x)).collect() }
