use std::sync::atomic::{AtomicI64, Ordering};

static TICKS: AtomicI64 = AtomicI64::new(0);

pub fn tick(x: i64) -> i64 { TICKS.fetch_add(1, Ordering::SeqCst); x }
pub fn boom(x: i64) -> i64 { panic!("boom {}", x) }
pub fn ticks(x: i64) -> (i64, i64) { (x, TICKS.load(Ordering::SeqCst)) }
pub fn bump(x: i64) -> i64 { TICKS.fetch_add(1, Ordering::SeqCst); x }
