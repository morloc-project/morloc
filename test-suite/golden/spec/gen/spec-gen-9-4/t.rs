use crate::mlc::types::_local::main::{Choice, R};
pub fn same<A: Clone>(x: &A) -> A { x.clone() }
pub fn isPicked(r: &R<i64>) -> bool { let c: &Choice<i64> = &r.d; matches!(c, Choice::Pick(_)) }
#[derive(Clone)]
pub struct MlcDate { pub v: i64 }
pub fn packDate(s: &String) -> MlcDate { MlcDate { v: s.parse().unwrap() } }
pub fn unpackDate(d: &MlcDate) -> String { d.v.to_string() }
pub fn outerPicked(c: &Choice<Choice<i64>>) -> bool { matches!(c, Choice::Pick(_)) }
pub fn one<T>(_x: &T) -> i64 { 1 }
