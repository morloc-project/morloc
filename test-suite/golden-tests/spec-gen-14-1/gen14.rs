use std::sync::atomic::{AtomicI64, Ordering};

static UNPACKS: AtomicI64 = AtomicI64::new(0);

#[derive(Clone)]
pub struct MlcDate { pub v: i64, pub bad: bool }

pub fn packDate(s: &String) -> MlcDate { MlcDate { v: s.parse().unwrap(), bad: false } }
pub fn unpackDate(d: &MlcDate) -> String {
    if d.bad { panic!("this date cannot be written"); }
    UNPACKS.fetch_add(1, Ordering::SeqCst);
    d.v.to_string()
}
pub fn dateOf(x: i64) -> MlcDate { MlcDate { v: x * 10, bad: false } }
pub fn unpacksAfter<T>(_: &T) -> i64 { UNPACKS.load(Ordering::SeqCst) }
pub fn badDate(x: i64) -> MlcDate { MlcDate { v: x, bad: true } }

#[derive(Clone)]
pub struct MlcBag<T> { pub items: Vec<T> }
pub fn packBag<T: Clone>(xs: &Vec<T>) -> MlcBag<T> { MlcBag { items: xs.clone() } }
pub fn unpackBag<T: Clone>(b: &MlcBag<T>) -> Vec<T> { b.items.clone() }
pub fn bagOf<T: Clone>(xs: &Vec<T>) -> MlcBag<T> { MlcBag { items: xs.clone() } }
pub fn toI64(x: i64) -> i64 { x }
