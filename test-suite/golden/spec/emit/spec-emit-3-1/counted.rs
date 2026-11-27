use std::sync::atomic::{AtomicI64, Ordering};

static CLONES: AtomicI64 = AtomicI64::new(0);

// Counts every clone of a Counted; a Rust move is never a clone.
#[derive(Default)]
pub struct Counted { pub v: i64 }

impl Clone for Counted {
    fn clone(&self) -> Counted {
        CLONES.fetch_add(1, Ordering::SeqCst);
        Counted { v: self.v }
    }
}

pub fn make(x: i64) -> Counted { Counted { v: x } }
pub fn bump(c: &Counted) -> Counted { Counted { v: c.v + 1 } }
pub fn tally(c: &Counted) -> Vec<i64> { vec![c.v, CLONES.load(Ordering::SeqCst)] }
pub fn pack_counted(x: i64) -> Counted { Counted { v: x } }
pub fn unpack_counted(c: &Counted) -> i64 { c.v }
