use std::sync::atomic::{AtomicI64, Ordering};

static CLONES: AtomicI64 = AtomicI64::new(0);

#[derive(Default)]
pub struct Counted { pub v: i64 }

impl Clone for Counted {
    fn clone(&self) -> Counted {
        CLONES.fetch_add(1, Ordering::SeqCst);
        Counted { v: self.v }
    }
}

pub fn make(x: i64) -> Counted { Counted { v: x } }
pub fn bump_val(mut c: Counted) -> Counted { c.v += 1; c }
pub fn bump_ref(c: &Counted) -> Counted { Counted { v: c.v + 1 } }
pub fn tally(c: &Counted) -> Vec<i64> { vec![c.v, CLONES.load(Ordering::SeqCst)] }
pub fn tally2(a: &Counted, b: &Counted) -> Vec<i64> { vec![a.v + b.v, CLONES.load(Ordering::SeqCst)] }
pub fn twice(a: Counted, b: &Counted) -> Vec<i64> { vec![a.v + b.v, CLONES.load(Ordering::SeqCst)] }
pub fn view_str(s: &str) -> i64 { s.len() as i64 }
pub fn view_list(xs: &[i64]) -> i64 { xs.iter().sum() }
pub fn pack_counted(x: i64) -> Counted { Counted { v: x } }
pub fn unpack_counted(c: &Counted) -> i64 { c.v }
