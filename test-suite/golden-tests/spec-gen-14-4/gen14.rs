use crate::mlc::types::_local::main::Pair;
#[derive(Clone)]
pub struct MlcDate { pub v: i64 }
pub fn packDate(s: &String) -> MlcDate { MlcDate { v: s.parse().unwrap() } }
pub fn unpackDate(d: &MlcDate) -> String { d.v.to_string() }
pub fn dateOf(x: i64) -> MlcDate { MlcDate { v: x * 10 } }
pub fn dupP<A: Clone>(x: &A) -> Pair<A> { Pair { fst: x.clone(), snd: x.clone() } }
pub fn firstDay(p: &Pair<MlcDate>) -> i64 { p.fst.v }
use crate::mlc::types::_local::main::Bin;
pub fn boxOf<A: Clone>(x: &A) -> Bin<A> { Bin { v: x.clone(), n: 2 } }
