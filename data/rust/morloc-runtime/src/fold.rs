//! Native fold slots (fold.md): one slot per thread that puts, held in a
//! fork-side entry (side.md) whose native pointer is the slot table.

use std::ffi::{c_char, c_void};
use std::ptr;
use std::thread::ThreadId;

use crate::error::MorlocError;
use crate::fork_side;

// FOLD-1
const MAX_SLOTS: usize = 4096;

#[derive(Clone, Copy, PartialEq)]
enum Mark {
    Free,
    Busy,
    Failed,
}

struct Slot {
    thread: ThreadId,
    mark: Mark,
    value: *mut c_void,
}

pub type FreshFn = unsafe extern "C" fn(ctx: *mut c_void) -> *mut c_void;

struct Table {
    ctx: *mut c_void,
    fresh: FreshFn,
    slots: Vec<Slot>,
}

fn err(msg: &str) -> MorlocError {
    MorlocError::Other(msg.to_string())
}

/// FOLD-1: a value checked out, or the side route.
pub enum Checkout {
    Value { value: *mut c_void, fresh: bool },
    Side,
}

/// A fold's entry.
pub fn new(ctx: *mut c_void, fresh: FreshFn) -> Result<i64, MorlocError> {
    let t = Box::into_raw(Box::new(Table { ctx, fresh, slots: Vec::new() }));
    fork_side::new(t as *mut c_void, fork_side::Kind::Fold).inspect_err(|_| drop(unsafe { Box::from_raw(t) }))
}

fn table<'a>(native: *mut c_void) -> &'a mut Table {
    // SAFETY: FOLD-1: a fold entry's native pointer is its table, used only under the entry lock.
    unsafe { &mut *(native as *mut Table) }
}

/// FOLD-1, FOLD-5, FOLD-6
pub fn checkout(id: i64) -> Result<Checkout, MorlocError> {
    let me = std::thread::current().id();
    let got = fork_side::with_native(id, fork_side::Kind::Fold, |n| {
        let t = table(n);
        let full = t.slots.len() >= MAX_SLOTS;
        match t.slots.iter_mut().find(|s| s.thread == me) {
            Some(s) => match s.mark {
                Mark::Busy => Err(err("@fold: re-entrant fold put: this thread's accumulator is already being stepped")),
                Mark::Failed => Err(err("@fold: an earlier step of this fold failed on this thread")),
                Mark::Free => {
                    s.mark = Mark::Busy;
                    Ok((std::mem::replace(&mut s.value, ptr::null_mut()), false))
                }
            },
            None if full => Err(MorlocError::Other(format!(
                "@fold: more than {MAX_SLOTS} accumulators in one fold. A folding handler keeps one per \
                 thread that has folded into it, for as long as the fold runs, so more than {MAX_SLOTS} \
                 distinct threads called its sink: either the producer starts a thread per batch, or the \
                 fold ran long enough for the pool's worker threads to retire and be replaced many times \
                 (as when batches arrive through callbacks from another pool)."
            ))),
            None => {
                // SAFETY: FOLD-6: `fresh` was given with `ctx`, which the binding keeps until the seal.
                let v = unsafe { (t.fresh)(t.ctx) };
                t.slots.push(Slot { thread: me, mark: Mark::Busy, value: ptr::null_mut() });
                Ok((v, true))
            }
        }
    })?;
    match got {
        Some(r) => r.map(|(value, fresh)| Checkout::Value { value, fresh }),
        None => Ok(Checkout::Side),
    }
}

/// FOLD-1: on error the caller keeps `value`.
pub fn store(id: i64, value: *mut c_void) -> Result<(), MorlocError> {
    let me = std::thread::current().id();
    #[cfg(test)]
    tests::hook(&tests::BEFORE_STORE);
    let stored = fork_side::with_native(id, fork_side::Kind::Fold, |n| match table(n).slots.iter_mut().find(|s| s.thread == me) {
        Some(s) if s.mark == Mark::Busy => {
            s.value = value;
            s.mark = Mark::Free;
            true
        }
        _ => false,
    })?;
    match stored {
        Some(true) => Ok(()),
        _ => Err(err("@fold: a value was stored with no put checked out on this thread")),
    }
}

/// FOLD-4
pub fn failed(id: i64) {
    let me = std::thread::current().id();
    let _ = fork_side::with_native(id, fork_side::Kind::Fold, |n| {
        if let Some(s) = table(n).slots.iter_mut().find(|s| s.thread == me && s.mark == Mark::Busy) {
            s.mark = Mark::Failed;
        }
    });
}

/// What a take or drop hands back: every stored value, the caller's.
pub struct Taken {
    pub values: Vec<*mut c_void>,
    pub records: Result<fork_side::Reader, MorlocError>,
}

fn reclaim(native: *mut c_void) -> (Vec<*mut c_void>, Option<Mark>) {
    if native.is_null() {
        return (Vec::new(), None);
    }
    // SAFETY: FOLD-2: the seal cleared the entry's pointer, so the table is the taker's alone.
    let t = unsafe { Box::from_raw(native as *mut Table) };
    let worst = t.slots.iter().map(|s| s.mark).find(|&m| m != Mark::Free);
    (t.slots.iter().filter(|s| s.mark == Mark::Free).map(|s| s.value).collect(), worst)
}

/// FOLD-2, FOLD-3, FOLD-4
pub fn take(id: i64) -> Taken {
    let (native, records) = fork_side::finish(id, fork_side::Kind::Fold);
    let (values, worst) = reclaim(native);
    let records = match worst {
        Some(Mark::Failed) => Err(err("@fold: a step of this fold failed, so batches its put reported were lost")),
        Some(_) => Err(err(
            "@fold: a put was still stepping when the fold finished; the producer let a thread outlive its call",
        )),
        None => records,
    };
    Taken { values, records }
}

pub fn drop_unread(id: i64) -> Vec<*mut c_void> {
    reclaim(fork_side::drop_native(id, fork_side::Kind::Fold)).0
}

mod c_abi {
    use super::*;

    unsafe fn out_values(values: Vec<*mut c_void>, out: *mut *mut *mut c_void, n: *mut usize) {
        *n = values.len();
        *out = ptr::null_mut();
        if values.is_empty() {
            return;
        }
        let bytes = values.len() * std::mem::size_of::<*mut c_void>();
        let p = libc::malloc(bytes) as *mut *mut c_void;
        if p.is_null() {
            morloc_runtime_types::panic::fatal("morloc: out of memory returning a fold's values");
        }
        ptr::copy_nonoverlapping(values.as_ptr(), p, values.len());
        *out = p;
    }

    /// # Safety
    /// `fresh` accepts `ctx` while the entry is unsealed; `errmsg` is a valid out-pointer.
    #[no_mangle]
    pub unsafe extern "C" fn mlc_fold_new(ctx: *mut c_void, fresh: FreshFn, errmsg: *mut *mut c_char) -> i64 {
        crate::error::clear_errmsg(errmsg);
        match new(ctx, fresh) {
            Ok(id) => id,
            Err(e) => {
                crate::error::set_errmsg(errmsg, &e);
                -1
            }
        }
    }

    /// # Safety
    /// The out-pointers are valid.
    #[no_mangle]
    pub unsafe extern "C" fn mlc_fold_checkout(id: i64, value: *mut *mut c_void, fresh: *mut i32, errmsg: *mut *mut c_char) -> i32 {
        crate::error::clear_errmsg(errmsg);
        *value = ptr::null_mut();
        *fresh = 0;
        match checkout(id) {
            Ok(Checkout::Value { value: v, fresh: f }) => {
                *value = v;
                *fresh = f as i32;
                1
            }
            Ok(Checkout::Side) => 0,
            Err(e) => {
                crate::error::set_errmsg(errmsg, &e);
                -1
            }
        }
    }

    /// # Safety
    /// `errmsg` is a valid out-pointer.
    #[no_mangle]
    pub unsafe extern "C" fn mlc_fold_store(id: i64, value: *mut c_void, errmsg: *mut *mut c_char) -> i32 {
        crate::error::clear_errmsg(errmsg);
        match store(id, value) {
            Ok(()) => 0,
            Err(e) => {
                crate::error::set_errmsg(errmsg, &e);
                -1
            }
        }
    }

    #[no_mangle]
    pub extern "C" fn mlc_fold_failed(id: i64) {
        failed(id);
    }

    /// # Safety
    /// The out-pointers are valid.
    #[no_mangle]
    pub unsafe extern "C" fn mlc_fold_take(id: i64, values: *mut *mut *mut c_void, n: *mut usize, errmsg: *mut *mut c_char) -> *mut c_void {
        crate::error::clear_errmsg(errmsg);
        let t = take(id);
        out_values(t.values, values, n);
        match t.records {
            Ok(r) => Box::into_raw(Box::new(r)) as *mut c_void,
            Err(e) => {
                crate::error::set_errmsg(errmsg, &e);
                ptr::null_mut()
            }
        }
    }

    /// # Safety
    /// The out-pointers are valid.
    #[no_mangle]
    pub unsafe extern "C" fn mlc_fold_drop(id: i64, values: *mut *mut *mut c_void, n: *mut usize) {
        out_values(drop_unread(id), values, n);
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use std::sync::atomic::{AtomicI64, AtomicUsize, Ordering};
    use std::sync::Mutex;

    pub(crate) static BEFORE_STORE: Mutex<Option<fn()>> = Mutex::new(None);

    pub(crate) fn hook(at: &Mutex<Option<fn()>>) {
        let f = at.lock().unwrap_or_else(|p| p.into_inner()).take();
        if let Some(f) = f {
            f();
        }
    }

    static FRESH: AtomicUsize = AtomicUsize::new(0);
    static ID: AtomicI64 = AtomicI64::new(0);

    unsafe extern "C" fn fresh(ctx: *mut c_void) -> *mut c_void {
        FRESH.fetch_add(1, Ordering::SeqCst);
        Box::into_raw(Box::new(*(ctx as *const u64))) as *mut c_void
    }

    static INIT: u64 = 0;

    fn fold() -> i64 {
        new(&INIT as *const u64 as *mut c_void, fresh).unwrap()
    }

    fn get(v: *mut c_void) -> u64 {
        unsafe { *Box::from_raw(v as *mut u64) }
    }

    fn put(id: i64, add: u64) -> Result<(), MorlocError> {
        let Checkout::Value { value, .. } = checkout(id)? else { return Err(err("side")) };
        let v = get(value) + add;
        let b = Box::into_raw(Box::new(v)) as *mut c_void;
        store(id, b).inspect_err(|_| { get(b); })
    }

    fn sum(t: Taken) -> Result<u64, MorlocError> {
        let s = t.values.into_iter().map(get).sum();
        t.records.map(|_| s)
    }

    // Each test forks, as fork_side's do (SIDE-9).
    fn in_child(f: fn() -> bool) {
        let _shm = crate::own_test_registry();
        let pid = unsafe { libc::fork() };
        assert!(pid >= 0);
        if pid == 0 {
            let ok = std::panic::catch_unwind(f).unwrap_or(false);
            unsafe { libc::_exit(if ok { 0 } else { 1 }) };
        }
        let mut st = 0;
        unsafe { libc::waitpid(pid, &mut st, 0) };
        assert!(libc::WIFEXITED(st) && libc::WEXITSTATUS(st) == 0, "status {st}");
    }

    #[test]
    fn an_untouched_fold_takes_no_values() {
        in_child(|| {
            let id = fold();
            let t = take(id);
            t.values.is_empty() && t.records.is_ok() && FRESH.load(Ordering::SeqCst) == 0
        });
    }

    #[test]
    fn each_thread_folds_into_its_own_fresh_slot() {
        in_child(|| {
            let id = fold();
            ID.store(id, Ordering::SeqCst);
            put(id, 1).unwrap();
            put(id, 2).unwrap();
            let other = std::thread::spawn(|| put(ID.load(Ordering::SeqCst), 10)).join().unwrap();
            let t = take(id);
            other.is_ok() && t.values.len() == 2 && FRESH.load(Ordering::SeqCst) == 2 && sum(t).unwrap() == 13
        });
    }

    // tla:ForkSide_fold_store_ignores_seal.bug
    #[test]
    fn a_store_after_the_seal_fails_and_the_fold_fails() {
        in_child(|| {
            let id = fold();
            ID.store(id, Ordering::SeqCst);
            fn take_meanwhile() {
                let id = ID.load(Ordering::SeqCst);
                let t = std::thread::spawn(move || sum(take(id))).join().unwrap();
                RESULT.lock().unwrap().replace(t.is_err());
            }
            *BEFORE_STORE.lock().unwrap() = Some(take_meanwhile);
            let late = put(id, 1).is_err();
            late && RESULT.lock().unwrap().take() == Some(true)
        });
    }

    static RESULT: Mutex<Option<bool>> = Mutex::new(None);

    // tla:ForkSide_fold_reduce_ignores_busy.bug, tla:ForkSide_fold_replay_over_busy.bug
    #[test]
    fn a_busy_slot_at_take_fails_the_fold_even_with_side_records() {
        in_child(|| {
            let id = fold();
            put(id, 1).unwrap();
            let Ok(Checkout::Value { value, .. }) = checkout(id) else { return false };
            get(value);
            let e = fork_side::tests::side_with_record(id);
            let t = take(id);
            let busy = t.records.as_ref().err().is_some_and(|e| format!("{e}").contains("still stepping"));
            let freed: Vec<u64> = t.values.into_iter().map(get).collect();
            e && busy && freed.is_empty()
        });
    }

    #[test]
    fn a_failed_step_fails_later_puts_and_the_fold() {
        in_child(|| {
            let id = fold();
            let Ok(Checkout::Value { value, .. }) = checkout(id) else { return false };
            get(value);
            failed(id);
            let again = put(id, 1).err().is_some_and(|e| format!("{e}").contains("earlier step"));
            let t = take(id);
            again && t.records.err().is_some_and(|e| format!("{e}").contains("failed"))
        });
    }

    // tla:ForkSide_fold_reentry_no_busy.bug
    #[test]
    fn a_reentrant_put_fails_and_changes_nothing() {
        in_child(|| {
            let id = fold();
            put(id, 5).unwrap();
            let Ok(Checkout::Value { value, .. }) = checkout(id) else { return false };
            let inner = put(id, 100).err().is_some_and(|e| format!("{e}").contains("re-entrant"));
            let v = get(value) + 1;
            store(id, Box::into_raw(Box::new(v)) as *mut c_void).unwrap();
            inner && sum(take(id)).unwrap() == 6
        });
    }

    // tla:ForkSide_fold_slot_after_take.bug
    #[test]
    fn a_thread_that_puts_while_take_starts_is_counted_or_late() {
        in_child(|| {
            let id = fold();
            ID.store(id, Ordering::SeqCst);
            fn put_from_a_new_thread() {
                let id = ID.load(Ordering::SeqCst);
                let r = std::thread::spawn(move || put(id, 7)).join().unwrap();
                RESULT.lock().unwrap().replace(r.is_ok());
            }
            *fork_side::tests::AFTER_TAKE_LOAD.lock().unwrap() = Some(put_from_a_new_thread);
            let total = sum(take(id)).unwrap();
            let returned = RESULT.lock().unwrap().take() == Some(true);
            let after = std::thread::spawn(move || put(id, 1)).join().unwrap().is_err();
            returned && total == 7 && after
        });
    }

    // tla:ForkSide_fold_replay_skipped.bug: a child's batch reaches the take.
    #[test]
    fn a_forked_childs_put_goes_to_the_side_file() {
        in_child(|| {
            let id = fold();
            let pid = unsafe { libc::fork() };
            if pid == 0 {
                let side = matches!(checkout(id), Ok(Checkout::Side))
                    && fork_side::tests::write_side(id, b"child");
                unsafe { libc::_exit(if side { 0 } else { 1 }) };
            }
            let mut st = 0;
            unsafe { libc::waitpid(pid, &mut st, 0) };
            let child_ok = libc::WIFEXITED(st) && libc::WEXITSTATUS(st) == 0;
            put(id, 2).unwrap();
            let t = take(id);
            let mut r = t.records.unwrap();
            let rec = r.next().unwrap();
            let total: u64 = t.values.into_iter().map(get).sum();
            child_ok && rec == Some(b"child".to_vec()) && total == 2
        });
    }

    fn names_the_fold<T>(r: Result<T, MorlocError>) -> bool {
        r.err().is_some_and(|e| {
            let m = format!("{e}");
            m.contains("fold") && !m.contains("gather")
        })
    }

    // FOLD-4: an error names what failed.
    #[test]
    fn a_folds_errors_name_the_fold() {
        in_child(|| {
            let id = fold();
            let pid = unsafe { libc::fork() };
            if pid == 0 {
                let ok = names_the_fold(take(id).records);
                unsafe { libc::_exit(if ok { 0 } else { 1 }) };
            }
            let mut st = 0;
            unsafe { libc::waitpid(pid, &mut st, 0) };
            let child = libc::WIFEXITED(st) && libc::WEXITSTATUS(st) == 0;
            let first = take(id).records.is_ok();
            child && first && names_the_fold(take(id).records) && names_the_fold(put(id, 1))
        });
    }

    #[test]
    fn a_dropped_fold_returns_its_values_and_refuses_puts() {
        in_child(|| {
            let id = fold();
            put(id, 3).unwrap();
            let vs: Vec<u64> = drop_unread(id).into_iter().map(get).collect();
            vs == vec![3] && put(id, 1).is_err()
        });
    }
}
