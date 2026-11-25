use crate::mlc::types::_local::main::G2;
#[derive(Clone)]
pub struct UT<A> { pub x: A, pub y: G2<A> }
pub fn packTenths(x: i64) -> f64 { x as f64 / 10.0 }
pub fn unpackTenths(x: f64) -> i64 { (x * 10.0).round() as i64 }
pub fn mkU(n: i64) -> UT<f64> { UT { x: n as f64 / 10.0, y: G2 { z: (n + 1) as f64 / 10.0 } } }
pub fn readU(u: &UT<f64>) -> i64 { (u.x * 100.0).round() as i64 + (u.y.z * 10.0).round() as i64 }
pub fn same<A: Clone>(a: &A) -> A { a.clone() }
pub fn bumpU(u: &UT<f64>) -> UT<f64> { UT { x: u.x + 0.1, y: G2 { z: u.y.z + 0.2 } } }
