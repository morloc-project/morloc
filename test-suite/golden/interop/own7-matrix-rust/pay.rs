// A payload that checks how generated code handles it: every value has an
// identity in a live set; a clone is counted and gets a new identity; a
// read of a dropped value and a second drop each abort.
use std::collections::HashSet;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Mutex;

static NEXT: AtomicI64 = AtomicI64::new(1);
static CLONES: AtomicI64 = AtomicI64::new(0);
static LIVE: Mutex<Option<HashSet<i64>>> = Mutex::new(None);

fn die(what: &str, id: i64) -> ! {
    eprintln!("PAY VIOLATION: {what} (id {id})");
    std::process::exit(99);
}

fn fresh() -> i64 {
    let id = NEXT.fetch_add(1, Ordering::SeqCst);
    LIVE.lock().unwrap().get_or_insert_with(HashSet::new).insert(id);
    id
}

fn check(id: i64, what: &str) {
    let live = LIVE.lock().unwrap();
    if !live.as_ref().map_or(false, |s| s.contains(&id)) {
        drop(live);
        die(what, id);
    }
}

pub struct Pay {
    key: i64,
    id: i64,
}

impl Default for Pay {
    fn default() -> Pay { Pay { key: 0, id: fresh() } }
}

impl Clone for Pay {
    fn clone(&self) -> Pay {
        check(self.id, "clone of a dropped payload");
        CLONES.fetch_add(1, Ordering::SeqCst);
        Pay { key: self.key, id: fresh() }
    }
}

impl Drop for Pay {
    fn drop(&mut self) {
        let removed = LIVE.lock().unwrap().as_mut().map_or(false, |s| s.remove(&self.id));
        if !removed { die("dropped twice", self.id); }
    }
}

impl PartialEq for Pay {
    fn eq(&self, o: &Pay) -> bool { self.key == o.key }
}

pub fn pay_pack(k: i64) -> Pay { Pay { key: k, id: fresh() } }
pub fn pay_unpack(p: &Pay) -> i64 { keyp(p) }
pub fn mkp(k: i64) -> Pay { pay_pack(k) }
pub fn keyp(p: &Pay) -> i64 {
    check(p.id, "read of a dropped payload");
    p.key
}
pub fn census(v: i64) -> (i64, i64) { (v, CLONES.load(Ordering::SeqCst)) }
