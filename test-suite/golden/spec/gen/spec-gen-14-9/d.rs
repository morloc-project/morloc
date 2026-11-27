#[derive(Clone)]
pub struct DateT { pub y: i64, pub m: i64, pub d: i64 }
#[derive(Clone)]
pub struct PairT<A> { pub fst: A, pub snd: A }
pub fn packDate(s: &str) -> DateT {
  let v: Vec<i64> = s.split('-').map(|x| x.parse().unwrap_or(0)).collect();
  DateT { y: v[0], m: v[1], d: v[2] }
}
pub fn unpackDate(x: &DateT) -> String { format!("{:04}-{:02}-{:02}", x.y, x.m, x.d) }
pub fn packTenths(x: i64) -> f64 { x as f64 / 10.0 }
pub fn unpackTenths(x: f64) -> i64 { (x * 10.0 + 0.5) as i64 }
pub fn same<A: Clone>(a: &A) -> A { a.clone() }
