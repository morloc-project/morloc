use crate::mlc::types::_local::main::Pair;

#[derive(Clone)]
pub struct MyDate(pub i64);

pub fn packDate(s: &String) -> MyDate { MyDate(s.parse().unwrap()) }
pub fn unpackDate(d: &MyDate) -> String { d.0.to_string() }
pub fn dateOf(x: i64) -> MyDate { MyDate(x * 10) }

pub fn dupP<A: Clone>(x: &A) -> Pair<A> { Pair { fst: x.clone(), snd: x.clone() } }
pub fn swapP<A: Clone>(p: &Pair<A>) -> Pair<A> { Pair { fst: p.snd.clone(), snd: p.fst.clone() } }
pub fn firstP<A: Clone>(p: &Pair<A>) -> A { p.fst.clone() }

pub fn ints(n: i64) -> Vec<i64> { (0..n).collect() }
pub fn total(p: &Pair<Vec<i64>>) -> i64 { (p.fst.iter().sum::<i64>() + p.snd.iter().sum::<i64>()) % 1000003 }
