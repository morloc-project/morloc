use std::sync::atomic::{AtomicI64, Ordering};
use crate::mlc::types::_local::main::R;

static UNPACKS: AtomicI64 = AtomicI64::new(0);

pub fn packTenths(x: i64) -> f64 { x as f64 / 10.0 }
pub fn unpackTenths(x: f64) -> i64 { UNPACKS.fetch_add(1, Ordering::SeqCst); (x * 10.0).round() as i64 }
pub fn packHalf(x: f64) -> i32 { (x * 2.0) as i32 }
pub fn unpackHalf(x: i32) -> f64 { UNPACKS.fetch_add(1, Ordering::SeqCst); x as f64 / 2.0 }

pub fn mk(k: i64) -> Vec<R> { (0..k).map(|i| R { t: i as f64 / 10.0, h: (3 * i) as i32, n: i }).collect() }
pub fn unpacks(_: i64) -> i64 { UNPACKS.load(Ordering::SeqCst) }
pub fn summary(xs: &Vec<R>) -> Vec<i64> {
    let t: i64 = xs.iter().map(|r| (r.t * 10.0).round() as i64).sum();
    let h: i64 = xs.iter().map(|r| r.h as i64).sum();
    let n: i64 = xs.iter().map(|r| r.n).sum();
    vec![xs.len() as i64, t, h, n]
}
