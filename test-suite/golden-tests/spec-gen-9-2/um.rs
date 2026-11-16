#[derive(Clone)]
pub struct PairT<A> { pub fst: A, pub snd: A }
pub fn packTenths(x: i64) -> f64 { x as f64 / 10.0 }
pub fn unpackTenths(x: f64) -> i64 { (x * 10.0) as i64 }
pub fn tenthsOf(x: i64) -> f64 { x as f64 / 10.0 }
pub fn dupU<A: Clone>(x: &A) -> PairT<A> { PairT { fst: x.clone(), snd: x.clone() } }
