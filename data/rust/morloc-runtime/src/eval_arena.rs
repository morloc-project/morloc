//! Per-call shared-memory arena (SHM-7, SHM-14).

use std::cell::RefCell;
use morloc_runtime_types::hash::AddrMap;
use std::path::PathBuf;

use crate::error::MorlocError;
use crate::shm::{self, AbsPtr};

#[derive(Clone, Copy)]
struct Entry {
    ptr: AbsPtr,
    generation: u16,
    seq: u64,
}

struct ArenaState {
    // SHM-14: in recording order, so a checkpoint is a seq.
    entries: Vec<Entry>,
    // SHM-14: the one entry each recorded block has.
    index: AddrMap<usize>,
    recorded: u64,
    dropped: usize,
    files: Vec<PathBuf>,
    slots: Vec<i64>,
}

const COMPACT_MIN: usize = if cfg!(test) { 4 } else { 64 };

impl ArenaState {
    fn record(&mut self, ptr: AbsPtr) {
        // SAFETY: SHM-14: `ptr` heads a block this thread was just handed.
        let generation = unsafe { shm::block_generation(ptr) }.unwrap_or(0);
        let at = self.entries.len();
        self.entries.push(Entry { ptr, generation, seq: self.recorded });
        self.recorded += 1;
        if let Some(old) = self.index.insert(ptr as usize, at) {
            // SHM-14: the earlier entry's reference was released without the arena.
            self.entries[old].ptr = std::ptr::null_mut();
            self.dropped += 1;
            stale(ptr, "was released behind its arena entry");
        }
    }

    fn take(&mut self, ptr: AbsPtr) -> bool {
        let Some(at) = self.index.remove(&(ptr as usize)) else {
            return false;
        };
        self.entries[at].ptr = std::ptr::null_mut();
        self.dropped += 1;
        while self.entries.last().is_some_and(|e| e.ptr.is_null()) {
            self.entries.pop();
            self.dropped -= 1;
        }
        if self.dropped > COMPACT_MIN && self.dropped * 2 > self.entries.len() {
            self.compact();
        }
        true
    }

    fn compact(&mut self) {
        self.entries.retain(|e| !e.ptr.is_null());
        for (at, e) in self.entries.iter().enumerate() {
            self.index.insert(e.ptr as usize, at);
        }
        self.dropped = 0;
    }

    fn drain_from(&mut self, seq: u64) -> Vec<Entry> {
        let start = self.entries.partition_point(|e| e.seq < seq);
        let drained = self.entries.len() - start;
        let later: Vec<Entry> = self.entries.drain(start..).filter(|e| !e.ptr.is_null()).collect();
        for e in &later {
            self.index.remove(&(e.ptr as usize));
        }
        self.dropped -= drained - later.len();
        later
    }
}

#[cfg(test)]
thread_local! {
    static BROKEN: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

// SHM-14: reported, never acted on.
fn stale(ptr: AbsPtr, what: &str) {
    eprintln!("eval_arena: block {:p} {}", ptr, what);
    #[cfg(test)]
    BROKEN.with(|n| n.set(n.get() + 1));
}

#[cfg(test)]
pub(crate) fn broken_rules_seen() -> usize {
    BROKEN.with(|n| n.get())
}

// SHM-14: the arena's own reference, refused once the block was released and claimed again.
fn release_entry(e: Entry) {
    // SAFETY: SHM-14: a recorded block lies in a volume this process mapped.
    match unsafe { shm::block_generation(e.ptr) } {
        Some(g) if g == e.generation && unsafe { shm::reference_count(e.ptr) }.is_some_and(|n| n > 0) => {
            if let Err(err) = shm::release(e.ptr) {
                eprintln!("eval_arena: release failed for {:p}: {:?}", e.ptr, err);
            }
        }
        Some(_) => stale(e.ptr, "was released behind its arena entry"),
        None => {}
    }
}

fn release_all(blocks: Vec<Entry>, files: Vec<PathBuf>, slots: Vec<i64>) {
    for e in blocks {
        release_entry(e);
    }
    for path in files {
        if let Err(e) = std::fs::remove_file(&path) {
            if e.kind() != std::io::ErrorKind::NotFound {
                eprintln!("eval_arena: remove_file failed for {}: {}", path.display(), e);
            }
        }
    }
    for handle in slots {
        if let Err(e) = crate::stream::discard_handle(handle) {
            eprintln!("eval_arena: discard_handle({}) failed: {:?}", handle, e);
        }
    }
}

thread_local! {
    static ARENA: RefCell<Option<ArenaState>> = const { RefCell::new(None) };
}

fn with_arena<R>(f: impl FnOnce(&mut Option<ArenaState>) -> R) -> Option<R> {
    ARENA.try_with(|cell| f(&mut cell.borrow_mut())).ok()
}

#[must_use = "ArenaGuard must be bound (e.g. `let _g = enter()?;`); \
              dropping it inline immediately frees the arena"]
pub struct ArenaGuard {
    _priv: (),
}

impl Drop for ArenaGuard {
    fn drop(&mut self) {
        if let Some(s) = with_arena(|slot| slot.take()).flatten() {
            let blocks = s.entries.into_iter().filter(|e| !e.ptr.is_null()).collect();
            release_all(blocks, s.files, s.slots);
        }
    }
}

pub fn enter() -> Result<ArenaGuard, MorlocError> {
    let opened = with_arena(|slot| {
        if slot.is_some() {
            return Err(MorlocError::Other(
                "eval_arena::enter called while an arena is already active on this thread".into(),
            ));
        }
        *slot = Some(ArenaState {
            entries: Vec::with_capacity(64),
            index: AddrMap::default(),
            recorded: 0,
            dropped: 0,
            files: Vec::new(),
            slots: Vec::new(),
        });
        Ok(())
    });
    match opened {
        Some(Ok(())) => Ok(ArenaGuard { _priv: () }),
        Some(Err(e)) => Err(e),
        None => Err(MorlocError::Other(
            "eval_arena::enter called while the thread is shutting down".into(),
        )),
    }
}

// SHM-14
#[inline]
pub fn record_if_active(ptr: AbsPtr) {
    if ptr.is_null() {
        return;
    }
    with_arena(|slot| {
        if let Some(s) = slot.as_mut() {
            s.record(ptr);
        }
    });
}

// SHM-14: false when no arena is active.
pub fn keep_if_active(ptr: AbsPtr) -> bool {
    if ptr.is_null() {
        return false;
    }
    with_arena(|slot| {
        slot.as_mut().map(|s| {
            if !s.index.contains_key(&(ptr as usize)) {
                s.record(ptr);
                return;
            }
            // SAFETY: SHM-14: a recorded block lies in a volume this process mapped.
            if unsafe { shm::reference_count(ptr) } != Some(1) {
                stale(ptr, "was given to its arena while another holder kept it");
            }
        })
    })
    .flatten()
    .is_some()
}

// SHM-14
#[inline]
pub fn record_file_if_active(path: PathBuf) {
    with_arena(|slot| {
        if let Some(s) = slot.as_mut() {
            s.files.push(path);
        }
    });
}

// SHM-14
#[inline]
pub fn forget_if_active(ptr: AbsPtr) -> bool {
    if ptr.is_null() {
        return false;
    }
    with_arena(|slot| slot.as_mut().is_some_and(|s| s.take(ptr))).unwrap_or(false)
}

// SHM-14
#[inline]
pub fn record_slot_if_active(handle: i64) {
    if handle < 0 {
        return;
    }
    with_arena(|slot| {
        if let Some(s) = slot.as_mut() {
            s.slots.push(handle);
        }
    });
}

// SHM-14
#[derive(Debug, Clone, Copy)]
pub struct ArenaCheckpoint {
    seq: u64,
    files: usize,
    slots: usize,
}

pub fn checkpoint() -> ArenaCheckpoint {
    with_arena(|slot| {
        slot.as_ref().map(|s| ArenaCheckpoint { seq: s.recorded, files: s.files.len(), slots: s.slots.len() })
    })
    .flatten()
    .unwrap_or(ArenaCheckpoint { seq: 0, files: 0, slots: 0 })
}

// SHM-14
pub fn rollback_to(cp: ArenaCheckpoint) {
    let drained = with_arena(|slot| {
        slot.as_mut().map(|s| {
            let blocks = s.drain_from(cp.seq);
            let files = if s.files.len() > cp.files { s.files.split_off(cp.files) } else { Vec::new() };
            let slots = if s.slots.len() > cp.slots { s.slots.split_off(cp.slots) } else { Vec::new() };
            (blocks, files, slots)
        })
    })
    .flatten();
    if let Some((blocks, files, slots)) = drained {
        release_all(blocks, files, slots);
    }
}

#[cfg(test)]
pub fn is_active() -> bool {
    with_arena(|slot| slot.is_some()).unwrap_or(false)
}

#[cfg(test)]
pub(crate) fn recorded_blocks() -> usize {
    with_arena(|slot| slot.as_ref().map_or(0, |s| s.index.len())).unwrap_or(0)
}

#[cfg(test)]
pub(crate) fn log_len() -> usize {
    with_arena(|slot| slot.as_ref().map_or(0, |s| s.entries.len())).unwrap_or(0)
}

#[cfg(test)]
pub(crate) fn has_entry(ptr: AbsPtr) -> bool {
    with_arena(|slot| slot.as_ref().is_some_and(|s| s.index.contains_key(&(ptr as usize)))).unwrap_or(false)
}

pub(crate) unsafe fn morloc_eval_arena_enter(
    errmsg: *mut *mut std::ffi::c_char,
) -> *mut std::ffi::c_void {
    crate::error::clear_errmsg(errmsg);
    match enter() {
        Ok(guard) => Box::into_raw(Box::new(guard)) as *mut std::ffi::c_void,
        Err(e) => {
            crate::error::set_errmsg(errmsg, &e);
            std::ptr::null_mut()
        }
    }
}

pub(crate) unsafe fn morloc_eval_arena_exit(handle: *mut std::ffi::c_void) {
    if !handle.is_null() {
        drop(Box::from_raw(handle as *mut ArenaGuard));
    }
}

#[cfg(test)]
mod tests {
    //! These tests exercise the arena's bookkeeping primitives directly
    //! (`record_if_active` / `forget_if_active`). They do not depend on
    //! `shm::shmalloc` auto-registering with the arena -- that wiring is
    //! verified separately in `shm.rs`'s test suite once it's in place.

    use super::*;

    #[must_use]
    fn ensure_shm() -> crate::ArenaShared {
        crate::init_test_shm()
    }

    fn arena_len() -> usize {
        recorded_blocks()
    }

    // A thread-local whose destructor reads the arena, which is what
    // `ShmTracker` does through `shfree`. Registered before the arena is
    // touched, so glibc destroys it last and the arena first -- the ordering
    // that reaches a destroyed thread-local.
    //
    // Failure is an abort, not an assertion: the read panics, the panic has
    // to leave `shfree`, and `shfree` is `extern "C"` and cannot unwind. That
    // is exactly the shape of the bug, so the test reproduces it faithfully
    // and takes the harness down with it when it regresses.
    struct TeardownProbe;

    impl Drop for TeardownProbe {
        fn drop(&mut self) {
            forget_if_active(0x1 as AbsPtr);
            let _ = is_active();
            let _ = checkpoint();
        }
    }

    thread_local! {
        static TEARDOWN_PROBE: TeardownProbe = const { TeardownProbe };
    }

    #[test]
    fn arena_reads_survive_thread_teardown() {
        let _shm = ensure_shm();
        std::thread::spawn(|| {
            TEARDOWN_PROBE.with(|_| {});
            let guard = enter().unwrap();
            drop(guard);
        })
        .join()
        .expect("the worker thread must exit cleanly");
    }

    fn arena_contains(p: AbsPtr) -> bool {
        has_entry(p)
    }

    #[test]
    fn enter_records_and_drop_frees() {
        let _shm = ensure_shm();
        assert!(!is_active());

        let p1 = shm::shmalloc(64).unwrap();
        let p2 = shm::shmalloc(128).unwrap();
        let p3 = shm::shcalloc(4, 16).unwrap();

        {
            let _g = enter().unwrap();
            assert!(is_active());

            // Manually record (simulating what shmalloc will do once
            // wired). Each block is now owned by the arena.
            record_if_active(p1);
            record_if_active(p2);
            record_if_active(p3);

            assert!(arena_contains(p1));
            assert!(arena_contains(p2));
            assert!(arena_contains(p3));
            assert_eq!(arena_len(), 3);
            // Guard drops -> all three freed by the arena.
        }

        assert!(!is_active());
    }

    #[test]
    fn explicit_shfree_during_arena_does_not_double_free() {
        let _shm = ensure_shm();
        let p = shm::shmalloc(32).unwrap();
        let _g = enter().unwrap();

        // Simulate the auto-record that shmalloc will do once wired.
        record_if_active(p);
        assert_eq!(arena_len(), 1);

        // Simulate the auto-forget that shfree will do once wired,
        // followed by the actual free.
        forget_if_active(p);
        shm::shfree(p).unwrap();

        assert_eq!(arena_len(), 0);
        // Guard drops -> nothing left to free, no error.
    }

    fn count(p: AbsPtr) -> u32 {
        unsafe { shm::reference_count(p) }.unwrap()
    }

    // SHM-7: a holder's own reference is released without touching the
    // arena's, so the arena still releases the allocation's at its end.
    #[test]
    fn a_holders_release_leaves_the_arenas_reference() {
        let _shm = ensure_shm();
        let g = enter().unwrap();
        let p = shm::shmalloc(48).unwrap();
        unsafe { shm::shincref(p) }.unwrap();
        unsafe { shm::shincref(p) }.unwrap();
        shm::release(p).unwrap();
        drop(g);
        let left = count(p);
        shm::release(p).unwrap();
        assert_eq!(left, 1, "only the observing reference should remain");
    }

    // SHM-7: freeing an entry recorded before a checkpoint leaves the ones
    // recorded after it in the range a rollback releases.
    #[test]
    fn a_free_before_the_checkpoint_keeps_later_entries_in_the_rollback() {
        let _shm = ensure_shm();
        let _g = enter().unwrap();
        let before = shm::shmalloc(48).unwrap();
        let cp = checkpoint();
        let after = shm::shmalloc(48).unwrap();
        unsafe { shm::shincref(after) }.unwrap();
        shm::shfree(before).unwrap();
        rollback_to(cp);
        let left = count(after);
        shm::release(after).unwrap();
        assert_eq!(left, 1, "the rollback did not release the entry made after the checkpoint");
    }

    // SHM-14: temporary files recorded after a checkpoint go at the
    // rollback, the rest when the arena ends.
    #[test]
    fn files_go_at_the_rollback_after_them_and_at_the_end() {
        let _shm = ensure_shm();
        let dir = std::env::temp_dir().join(format!("morloc_arena_files_{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let make = |n: &str| {
            let p = dir.join(n);
            std::fs::write(&p, b"x").unwrap();
            record_file_if_active(p.clone());
            p
        };
        let g = enter().unwrap();
        let early = make("early");
        let cp = checkpoint();
        let late = make("late");
        rollback_to(cp);
        let after_rollback = (early.exists(), late.exists());
        drop(g);
        let after_end = early.exists();
        let _ = std::fs::remove_dir_all(&dir);
        assert_eq!(after_rollback, (true, false));
        assert!(!after_end);
    }

    // SHM-14: a block released behind its entry is reported, not released
    // again.
    #[test]
    fn a_block_released_behind_its_entry_is_not_released_again() {
        let _shm = ensure_shm();
        let seen = broken_rules_seen();
        let g = enter().unwrap();
        let p = shm::shmalloc(24).unwrap();
        shm::release(p).unwrap();
        drop(g);
        assert_eq!(broken_rules_seen(), seen + 1);
    }

    #[test]
    fn reentrant_enter_returns_err() {
        let _shm = ensure_shm();
        let _g = enter().unwrap();
        let result = enter();
        assert!(result.is_err(), "second enter() must error");
    }

    #[test]
    fn no_arena_means_no_recording() {
        let _shm = ensure_shm();
        assert!(!is_active());

        let p = shm::shmalloc(16).unwrap();
        // No arena active -> record_if_active is a no-op.
        record_if_active(p);
        assert!(!is_active());

        shm::shfree(p).unwrap();
    }

    #[test]
    fn forget_unknown_pointer_is_noop() {
        let _shm = ensure_shm();
        let p = shm::shmalloc(16).unwrap();
        let other = shm::shmalloc(16).unwrap();

        let _g = enter().unwrap();
        record_if_active(p);
        record_if_active(other);

        forget_if_active(other);

        assert!(arena_contains(p));
        assert!(!arena_contains(other));

        // Free `other` manually since we forgot it; `p` freed at guard drop.
        shm::shfree(other).unwrap();
    }

    #[test]
    fn forget_when_arena_inactive_is_noop() {
        let _shm = ensure_shm();
        let p = shm::shmalloc(16).unwrap();
        // No panic, no error.
        forget_if_active(p);
        shm::shfree(p).unwrap();
    }

    #[test]
    fn record_null_is_ignored() {
        let _shm = ensure_shm();
        let _g = enter().unwrap();
        record_if_active(std::ptr::null_mut());
        assert_eq!(arena_len(), 0);
    }

    #[test]
    fn shmalloc_auto_records_and_drop_frees() {
        // Integration: now that shm::shmalloc/shfree are wired to the
        // arena, allocations under an active arena should round-trip
        // through the bookkeeping vector and be released at guard drop.
        let _shm = ensure_shm();
        assert!(!is_active());

        {
            let _g = enter().unwrap();
            let p1 = shm::shmalloc(64).unwrap();
            let p2 = shm::shcalloc(8, 8).unwrap();
            assert!(arena_contains(p1));
            assert!(arena_contains(p2));
            assert_eq!(arena_len(), 2);
            // Guard drops -> both freed automatically.
        }

        // Subsequent shmalloc may reuse the freed slots; we don't assert
        // pointer equality, but we do assert that requesting another
        // block of the same size succeeds (volume not exhausted).
        let p3 = shm::shmalloc(64).unwrap();
        shm::shfree(p3).unwrap();
    }

    #[test]
    fn shfree_auto_forgets_during_arena() {
        // Integration: explicit shm::shfree under an active arena must
        // remove the pointer before freeing, so guard drop doesn't try
        // to free the same block twice (double-free would corrupt the
        // free list / fail BLK_MAGIC check).
        let _shm = ensure_shm();
        let _g = enter().unwrap();

        let p = shm::shmalloc(48).unwrap();
        assert!(arena_contains(p));

        shm::shfree(p).unwrap();
        assert!(!arena_contains(p));
        assert_eq!(arena_len(), 0);
        // Guard drops -> nothing to free, no error.
    }

    #[test]
    fn threads_have_independent_arenas() {
        let _shm = ensure_shm();
        let _g = enter().unwrap();
        assert!(is_active());

        let handle = std::thread::spawn(|| {
            assert!(!is_active(), "child thread must not see parent's arena");
            let _g = enter().unwrap();
            assert!(is_active());
        });
        handle.join().unwrap();

        // Parent's arena unaffected.
        assert!(is_active());
    }
}

mod c_abi {

    #[no_mangle]
    pub unsafe extern "C" fn morloc_eval_arena_enter(errmsg: *mut *mut std::ffi::c_char) -> *mut std::ffi::c_void {
        super::morloc_eval_arena_enter(errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn morloc_eval_arena_exit(handle: *mut std::ffi::c_void) {
        super::morloc_eval_arena_exit(handle)
    }
}
