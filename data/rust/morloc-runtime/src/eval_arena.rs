//! Per-eval shared-memory arena.
//!
//! Tracks every SHM block allocated while a `morloc_eval` call (or any
//! caller-bracketed equivalent) is active on the current thread, so they
//! can all be released together at scope exit.
//!
//! ## Lifecycle
//!
//! 1. The dispatch path (daemon or nexus) calls [`enter`], obtaining a
//!    [`ArenaGuard`]. The guard's `Drop` impl owns cleanup.
//! 2. While the guard is alive, every successful `shm::shmalloc` (and its
//!    `shcalloc`/`shmemcpy` callers) auto-records the returned pointer
//!    via [`record_if_active`].
//! 3. Any explicit `shm::shfree` during the same window auto-removes the
//!    pointer from the arena via [`forget_if_active`], so the guard's
//!    drop doesn't try to free it a second time.
//! 4. When the guard drops, every still-tracked block is `shfree`'d and
//!    the TLS slot is cleared.
//!
//! ## Thread model
//!
//! State is per-thread (`thread_local!`). Daemon worker threads each have
//! their own arena; pool worker threads (which never call [`enter`]) see
//! `None` on every call and behave as no-ops. Re-entrant [`enter`] on the
//! same thread returns `Err`: `morloc_eval` is not designed to nest from
//! the public boundary, so re-entry is treated as a bug.
//!
//! ## Boundary contract
//!
//! Activate the arena only across the dispatch path that owns the call --
//! argument ingress, eval, and result serialization. Never wrap a code
//! path that allocates SHM intended to outlive the eval (pool result
//! construction, persistent caches). If you allocate SHM during the arena
//! that should outlive it, call [`forget_if_active`] after handing the
//! pointer to its long-lived owner.

use std::cell::RefCell;
use std::path::PathBuf;

use crate::error::MorlocError;
use crate::shm::{self, AbsPtr};

struct ArenaState {
    blocks: Vec<AbsPtr>,
    files: Vec<PathBuf>,
}

thread_local! {
    static ARENA: RefCell<Option<ArenaState>> = const { RefCell::new(None) };
}

/// Initial capacity of the tracking vector. Sized to avoid libc-malloc
/// reallocation for typical eval calls (a few dozen blocks). Growing
/// beyond this is fine -- `Vec::push` reallocates via the system
/// allocator, not via SHM, so there's no reentrancy on `ALLOC_MUTEX`.
const INITIAL_CAPACITY: usize = 64;

/// RAII guard returned by [`enter`]. Drop releases every recorded block.
///
/// The struct is intentionally opaque (no public fields) so callers
/// cannot construct it without going through [`enter`].
#[must_use = "ArenaGuard must be bound (e.g. `let _g = enter()?;`); \
              dropping it inline immediately frees the arena"]
pub struct ArenaGuard {
    _priv: (),
}

impl Drop for ArenaGuard {
    fn drop(&mut self) {
        ARENA.with(|cell| {
            let state = cell.borrow_mut().take();
            if let Some(state) = state {
                for ptr in state.blocks {
                    // Panic-safe: log and continue rather than propagate.
                    // A panic inside Drop while already unwinding aborts
                    // the process, which is strictly worse than leaving
                    // one bad block alone and freeing the rest.
                    if let Err(e) = shm::shfree(ptr) {
                        eprintln!(
                            "eval_arena drop: shfree failed for {:p}: {:?}",
                            ptr, e
                        );
                    }
                }
                for path in state.files {
                    if let Err(e) = std::fs::remove_file(&path) {
                        if e.kind() != std::io::ErrorKind::NotFound {
                            eprintln!(
                                "eval_arena drop: remove_file failed for {}: {}",
                                path.display(), e
                            );
                        }
                    }
                }
            }
        });
    }
}

/// Begin a new arena scope on the current thread. Returns `Err` if a
/// scope is already active (re-entry is treated as a bug).
pub fn enter() -> Result<ArenaGuard, MorlocError> {
    ARENA.with(|cell| {
        let mut slot = cell.borrow_mut();
        if slot.is_some() {
            return Err(MorlocError::Other(
                "eval_arena::enter called while an arena is already active on this thread".into(),
            ));
        }
        *slot = Some(ArenaState {
            blocks: Vec::with_capacity(INITIAL_CAPACITY),
            files: Vec::new(),
        });
        Ok(ArenaGuard { _priv: () })
    })
}

/// Record `ptr` in the active arena. No-op if no arena is active.
///
/// Called from `shm::shmalloc` after a successful allocation. Must not
/// allocate SHM itself (would deadlock on `ALLOC_MUTEX`); `Vec::push`
/// uses the system allocator, which is safe.
#[inline]
pub fn record_if_active(ptr: AbsPtr) {
    if ptr.is_null() {
        return;
    }
    ARENA.with(|cell| {
        if let Some(s) = cell.borrow_mut().as_mut() {
            s.blocks.push(ptr);
        }
    });
}

/// Record a file path that should be unlinked when the arena guard
/// drops. Used by the file-packet producer to register temp files for
/// post-eval cleanup. No-op if no arena is active (the caller is then
/// responsible for cleanup).
#[inline]
pub fn record_file_if_active(path: PathBuf) {
    ARENA.with(|cell| {
        if let Some(s) = cell.borrow_mut().as_mut() {
            s.files.push(path);
        }
    });
}

/// Remove `ptr` from the active arena (if present). No-op if no arena
/// is active or `ptr` is not tracked.
///
/// Called from `shm::shfree` before the actual free, so any explicit
/// `shfree` during the arena window is correctly removed from the
/// tracking list and won't be double-freed at guard drop.
///
/// Linear scan; arena vectors are small (few dozen entries in typical
/// eval calls). `swap_remove` keeps removal O(1) per call. We use
/// `rposition` because the most-recent allocation is the most likely
/// candidate for an immediate explicit free.
#[inline]
pub fn forget_if_active(ptr: AbsPtr) {
    if ptr.is_null() {
        return;
    }
    ARENA.with(|cell| {
        if let Some(s) = cell.borrow_mut().as_mut() {
            if let Some(idx) = s.blocks.iter().rposition(|&p| p == ptr) {
                s.blocks.swap_remove(idx);
            }
        }
    });
}

/// Remove `path` from the active arena's file list. Use when a caller
/// has taken responsibility for unlinking the file itself (or after the
/// receiver has consumed it). No-op if no arena is active or the path
/// is not tracked.
#[inline]
pub fn forget_file_if_active(path: &std::path::Path) {
    ARENA.with(|cell| {
        if let Some(s) = cell.borrow_mut().as_mut() {
            if let Some(idx) = s.files.iter().rposition(|p| p == path) {
                s.files.swap_remove(idx);
            }
        }
    });
}

/// Returns true if an arena is currently active on this thread.
pub fn is_active() -> bool {
    ARENA.with(|cell| cell.borrow().is_some())
}

#[cfg(test)]
mod tests {
    //! These tests exercise the arena's bookkeeping primitives directly
    //! (`record_if_active` / `forget_if_active`). They do not depend on
    //! `shm::shmalloc` auto-registering with the arena -- that wiring is
    //! verified separately in `shm.rs`'s test suite once it's in place.

    use super::*;

    fn ensure_shm() {
        crate::init_test_shm();
    }

    fn arena_len() -> usize {
        ARENA.with(|cell| cell.borrow().as_ref().map(|s| s.blocks.len()).unwrap_or(0))
    }

    fn arena_contains(p: AbsPtr) -> bool {
        ARENA.with(|cell| {
            cell.borrow()
                .as_ref()
                .map(|s| s.blocks.contains(&p))
                .unwrap_or(false)
        })
    }

    #[test]
    fn enter_records_and_drop_frees() {
        ensure_shm();
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
        ensure_shm();
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

    #[test]
    fn reentrant_enter_returns_err() {
        ensure_shm();
        let _g = enter().unwrap();
        let result = enter();
        assert!(result.is_err(), "second enter() must error");
    }

    #[test]
    fn no_arena_means_no_recording() {
        ensure_shm();
        assert!(!is_active());

        let p = shm::shmalloc(16).unwrap();
        // No arena active -> record_if_active is a no-op.
        record_if_active(p);
        assert!(!is_active());

        shm::shfree(p).unwrap();
    }

    #[test]
    fn forget_unknown_pointer_is_noop() {
        ensure_shm();
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
        ensure_shm();
        let p = shm::shmalloc(16).unwrap();
        // No panic, no error.
        forget_if_active(p);
        shm::shfree(p).unwrap();
    }

    #[test]
    fn record_null_is_ignored() {
        ensure_shm();
        let _g = enter().unwrap();
        record_if_active(std::ptr::null_mut());
        assert_eq!(arena_len(), 0);
    }

    #[test]
    fn shmalloc_auto_records_and_drop_frees() {
        // Integration: now that shm::shmalloc/shfree are wired to the
        // arena, allocations under an active arena should round-trip
        // through the bookkeeping vector and be released at guard drop.
        ensure_shm();
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
        ensure_shm();
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
        ensure_shm();
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
