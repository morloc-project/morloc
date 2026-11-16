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
pub fn sumDays(xs: &Vec<Pair<MlcDate>>) -> i64 { xs.iter().map(|p| p.fst.v).sum() }
pub fn producePairs<F: Fn(&Vec<Pair<MlcDate>>)>(sink: F) { for i in 1..=2i64 { let b: Vec<Pair<MlcDate>> = (0..i).map(|j| dupP(&dateOf(i * 10 + j))).collect(); sink(&b); } }
pub fn addPairs(acc: i64, xs: &Vec<Pair<MlcDate>>) -> i64 { acc + sumDays(xs) }
pub fn mergeSums(a: i64, b: i64) -> i64 { a + b }
pub fn showSum(n: i64) -> String { format!("sum={}\n", n) }
