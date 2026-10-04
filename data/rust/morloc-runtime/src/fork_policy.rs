use std::cell::Cell;
use std::mem::ManuallyDrop;
use std::ops::{Deref, DerefMut};
use std::sync::atomic::{AtomicPtr, AtomicU64, AtomicUsize, Ordering};
use std::sync::{Condvar, LockResult, Mutex, MutexGuard, PoisonError};

static GENERATION: AtomicU64 = AtomicU64::new(0);

pub(crate) fn generation() -> u64 {
    GENERATION.load(Ordering::Relaxed)
}

pub(crate) struct ForkLocal<T> {
    generation: u64,
    value: ManuallyDrop<T>,
}

impl<T> ForkLocal<T> {
    pub(crate) fn new(value: T) -> Self {
        ForkLocal { generation: generation(), value: ManuallyDrop::new(value) }
    }

    pub(crate) fn is_inherited(&self) -> bool {
        self.generation != generation()
    }
}

impl<T> Deref for ForkLocal<T> {
    type Target = T;
    fn deref(&self) -> &T {
        assert!(!self.is_inherited(), "a forked child used state its parent owns");
        &self.value
    }
}

impl<T> DerefMut for ForkLocal<T> {
    fn deref_mut(&mut self) -> &mut T {
        assert!(!self.is_inherited(), "a forked child used state its parent owns");
        &mut self.value
    }
}

impl<T> Drop for ForkLocal<T> {
    fn drop(&mut self) {
        if !self.is_inherited() {
            unsafe { ManuallyDrop::drop(&mut self.value) }
        }
    }
}

thread_local! {
    static HELD_RANKS: Cell<u64> = const { Cell::new(0) };
    static RESETS_HELD: Cell<usize> = const { Cell::new(0) };
}

// FORK-7: every thread takes these in ascending rank.
pub(crate) struct Held<T> {
    rank: u32,
    mutex: Mutex<T>,
}

pub(crate) struct HeldGuard<'a, T> {
    guard: Option<MutexGuard<'a, T>>,
    bit: u64,
}

fn rank_violation(rank: u32, held: u64) -> ! {
    panic!("morloc: lock of rank {rank} taken while holding ranks {held:#b}");
}

impl<T> Held<T> {
    pub(crate) const fn new(rank: u32, value: T) -> Self {
        assert!(rank > 0 && rank < 64);
        Held { rank, mutex: Mutex::new(value) }
    }

    pub(crate) fn lock(&self) -> HeldGuard<'_, T> {
        let bit = 1u64 << self.rank;
        let held = HELD_RANKS.with(Cell::get);
        if held >= bit {
            rank_violation(self.rank, held);
        }
        let guard = self.mutex.lock().unwrap_or_else(|p| p.into_inner());
        HELD_RANKS.with(|r| r.set(held | bit));
        HeldGuard { guard: Some(guard), bit }
    }

    pub(crate) fn try_lock(&self) -> Option<HeldGuard<'_, T>> {
        let guard = match self.mutex.try_lock() {
            Ok(g) => g,
            Err(std::sync::TryLockError::Poisoned(p)) => p.into_inner(),
            Err(std::sync::TryLockError::WouldBlock) => return None,
        };
        let bit = 1u64 << self.rank;
        HELD_RANKS.with(|r| r.set(r.get() | bit));
        Some(HeldGuard { guard: Some(guard), bit })
    }
}

impl<T> HeldGuard<'_, T> {
    pub(crate) fn wait(mut self, condvar: &Condvar) -> Self {
        let guard = self.guard.take().unwrap();
        let guard = condvar.wait(guard).unwrap_or_else(|p| p.into_inner());
        self.guard = Some(guard);
        self
    }
}

impl<T> Deref for HeldGuard<'_, T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.guard.as_ref().unwrap()
    }
}

impl<T> DerefMut for HeldGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
        self.guard.as_mut().unwrap()
    }
}

impl<T> Drop for HeldGuard<'_, T> {
    fn drop(&mut self) {
        let bit = self.bit;
        HELD_RANKS.with(|r| r.set(r.get() & !bit));
    }
}

struct ResetBox<T> {
    generation: u64,
    mutex: Mutex<T>,
}

// FORK-8: tla/ResetPublish.tla.
pub(crate) struct Reset<T> {
    current: AtomicPtr<ResetBox<T>>,
    init: fn() -> T,
    _shared_as: std::marker::PhantomData<Mutex<T>>,
}

pub(crate) struct ResetGuard<'a, T> {
    guard: MutexGuard<'a, T>,
}

impl<'a, T> ResetGuard<'a, T> {
    fn new(guard: MutexGuard<'a, T>) -> Self {
        RESETS_HELD.with(|n| n.set(n.get() + 1));
        ResetGuard { guard }
    }
}

impl<T> Deref for ResetGuard<'_, T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.guard
    }
}

impl<T> DerefMut for ResetGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.guard
    }
}

impl<T> Drop for ResetGuard<'_, T> {
    fn drop(&mut self) {
        RESETS_HELD.with(|n| n.set(n.get() - 1));
    }
}

impl<T> Reset<T> {
    pub(crate) const fn new(init: fn() -> T) -> Self {
        Reset { current: AtomicPtr::new(std::ptr::null_mut()), init, _shared_as: std::marker::PhantomData }
    }

    fn mutex(&self) -> &Mutex<T> {
        let generation = generation();
        loop {
            let seen = self.current.load(Ordering::Acquire);
            // SAFETY: FORK-8: a published box is never freed.
            if let Some(b) = unsafe { seen.as_ref() } {
                if b.generation == generation {
                    return &b.mutex;
                }
            }
            let fresh = Box::into_raw(Box::new(ResetBox { generation, mutex: Mutex::new((self.init)()) }));
            match self.current.compare_exchange(seen, fresh, Ordering::AcqRel, Ordering::Acquire) {
                // SAFETY: FORK-8: published, so never freed.
                Ok(_) => return unsafe { &(*fresh).mutex },
                // SAFETY: FORK-8: the loser frees only its own unpublished box.
                Err(_) => drop(unsafe { Box::from_raw(fresh) }),
            }
        }
    }

    pub(crate) fn lock(&self) -> LockResult<ResetGuard<'_, T>> {
        match self.mutex().lock() {
            Ok(g) => Ok(ResetGuard::new(g)),
            Err(p) => Err(PoisonError::new(ResetGuard::new(p.into_inner()))),
        }
    }
}

static INHERITED_DISPATCHES: AtomicUsize = AtomicUsize::new(0);

pub(crate) fn inherited_dispatches() -> usize {
    INHERITED_DISPATCHES.load(Ordering::Relaxed)
}

struct ForkHeld {
    _pass: HeldGuard<'static, ()>,
    map: HeldGuard<'static, Option<std::collections::HashMap<i64, crate::stream::LocalEntry>>>,
    locked_fds: HeldGuard<'static, Vec<libc::c_int>>,
    _alloc: HeldGuard<'static, ()>,
    _volumes: HeldGuard<'static, crate::shm::VolumeTable>,
    _basename: HeldGuard<'static, [u8; crate::shm::MAX_FILENAME_SIZE]>,
    _registry: HeldGuard<'static, Option<crate::shm_companion::CompanionSegment>>,
    _stats: HeldGuard<'static, Option<crate::shm_companion::CompanionSegment>>,
    _fallback: HeldGuard<'static, [u8; crate::shm::MAX_FILENAME_SIZE]>,
    _hooks: HeldGuard<'static, Vec<crate::shm::ShcloseHook>>,
    _tmpdir: HeldGuard<'static, crate::packet::Config>,
    _self_socket: HeldGuard<'static, Option<std::path::PathBuf>>,
    _bench: HeldGuard<'static, Option<std::sync::Arc<std::fs::File>>>,
    _tees: HeldGuard<'static, Option<std::collections::HashMap<String, std::sync::Arc<std::fs::File>>>>,
    _run_tee: HeldGuard<'static, Option<std::sync::Arc<std::fs::File>>>,
    _command: HeldGuard<'static, Option<String>>,
    _error: HeldGuard<'static, Option<String>>,
}

thread_local! {
    static FORK_HELD: std::cell::RefCell<Option<ForkHeld>> = const { std::cell::RefCell::new(None) };
}

extern "C" fn prepare_fork() {
    if HELD_RANKS.with(Cell::get) != 0 || RESETS_HELD.with(Cell::get) != 0 {
        let msg = b"morloc: fork from a thread holding a runtime lock\n";
        unsafe {
            libc::write(2, msg.as_ptr() as *const libc::c_void, msg.len());
            libc::abort();
        }
    }
    let pass = crate::stream::RELEASE_PASS.lock();
    if let Err(e) = crate::stream::drain_before_handoff() {
        eprintln!("morloc: a stream write failed before fork: {e}");
    }
    let held = ForkHeld {
        _pass: pass,
        map: crate::stream::PROCESS_LOCAL_SLOTS.lock(),
        locked_fds: crate::stream::LOCKED_FDS.lock(),
        _alloc: crate::shm::ALLOC_MUTEX.lock(),
        _volumes: crate::shm::VOLUMES.lock(),
        _registry: crate::stream::REGISTRY_SEGMENT.lock(),
        _stats: crate::shm_stats::SEGMENT.lock(),
        _basename: crate::shm::COMMON_BASENAME.lock(),
        _fallback: crate::shm::FALLBACK_DIR.lock(),
        _hooks: crate::shm::SHCLOSE_HOOKS.lock(),
        _tmpdir: crate::packet::TMPDIR.lock(),
        _self_socket: crate::ipc_ffi::SELF_SOCKET.lock(),
        _bench: crate::log::BENCH_FILE.lock(),
        _tees: crate::run::TEE_HANDLES.lock(),
        _run_tee: crate::run::RUN_TEE_HANDLE.lock(),
        _command: crate::run::RUN_COMMAND.lock(),
        _error: crate::run::RUN_ERROR.lock(),
    };
    FORK_HELD.with(|h| *h.borrow_mut() = Some(held));
}

extern "C" fn after_fork_in_parent() {
    FORK_HELD.with(|h| drop(h.borrow_mut().take()));
}

extern "C" fn after_fork_in_child() {
    GENERATION.fetch_add(1, Ordering::Relaxed);
    INHERITED_DISPATCHES.store(crate::intrinsics::dispatch_depth(), Ordering::Relaxed);
    crate::cell::after_fork_in_child();
    FORK_HELD.with(|h| {
        if let Some(mut held) = h.borrow_mut().take() {
            crate::stream::after_fork_in_child(&mut held.map, &mut held.locked_fds);
        }
    });
}

extern "C" fn register_fork_handlers() {
    unsafe {
        libc::pthread_atfork(Some(prepare_fork), Some(after_fork_in_parent), Some(after_fork_in_child));
    }
}

#[used]
#[cfg_attr(any(target_os = "linux", target_os = "android"), link_section = ".init_array")]
#[cfg_attr(target_os = "macos", link_section = "__DATA,__mod_init_func")]
static REGISTER_AT_LOAD: extern "C" fn() = register_fork_handlers;

#[cfg(test)]
pub(crate) fn exits_cleanly_in_a_forked_child(work: impl FnOnce() -> bool) -> bool {
    let pid = unsafe { libc::fork() };
    assert!(pid >= 0);
    if pid == 0 {
        unsafe { libc::alarm(5) };
        let ok = std::panic::catch_unwind(std::panic::AssertUnwindSafe(work)).unwrap_or(false);
        unsafe { libc::_exit(if ok { 0 } else { 1 }) }
    }
    let mut status = 0;
    unsafe { libc::waitpid(pid, &mut status, 0) };
    libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    #[test]
    fn a_value_dropped_in_a_forked_child_is_forgotten() {
        let probe = Arc::new(());
        let local = ForkLocal::new(Arc::clone(&probe));
        let pid = unsafe { libc::fork() };
        assert!(pid >= 0);
        if pid == 0 {
            let inherited = local.is_inherited();
            drop(local);
            let forgotten = Arc::strong_count(&probe) == 2;
            unsafe { libc::_exit(if inherited && forgotten { 0 } else { 1 }) }
        }
        let mut status = 0;
        unsafe { libc::waitpid(pid, &mut status, 0) };
        assert!(libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0);
        assert!(!local.is_inherited());
        drop(local);
        assert_eq!(Arc::strong_count(&probe), 1);
    }

    #[test]
    fn the_exported_generation_changes_in_a_forked_child() {
        let parent = crate::ffi::morloc_fork_generation();
        let pid = unsafe { libc::fork() };
        assert!(pid >= 0);
        if pid == 0 {
            let changed = crate::ffi::morloc_fork_generation() != parent;
            unsafe { libc::_exit(if changed { 0 } else { 1 }) }
        }
        let mut status = 0;
        unsafe { libc::waitpid(pid, &mut status, 0) };
        assert!(libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0);
        assert_eq!(crate::ffi::morloc_fork_generation(), parent);
    }

    static PROBE: Reset<u32> = Reset::new(|| 7);

    #[test]
    fn a_child_forked_while_a_thread_holds_a_reset_lock_gets_a_fresh_one() {
        *PROBE.lock().unwrap() = 8;
        let held = Arc::new(std::sync::Barrier::new(2));
        let (release_tx, release_rx) = std::sync::mpsc::channel::<()>();
        let holder = {
            let held = Arc::clone(&held);
            std::thread::spawn(move || {
                let guard = PROBE.lock().unwrap();
                held.wait();
                let _ = release_rx.recv();
                drop(guard);
            })
        };
        held.wait();
        let ok = exits_cleanly_in_a_forked_child(|| {
            let mut v = PROBE.lock().unwrap();
            let fresh = *v == 7;
            *v = 9;
            fresh
        });
        release_tx.send(()).unwrap();
        holder.join().unwrap();
        assert!(ok, "a forked child blocked on, or saw, its parent's reset value");
        assert_eq!(*PROBE.lock().unwrap(), 8);
    }

    #[test]
    fn a_fork_from_a_thread_holding_a_reset_lock_aborts() {
        let pid = unsafe { libc::fork() };
        assert!(pid >= 0);
        if pid == 0 {
            unsafe { libc::alarm(5) };
            let _probe = PROBE.lock().unwrap();
            let grandchild = unsafe { libc::fork() };
            unsafe { libc::_exit(if grandchild == 0 { 0 } else { 1 }) };
        }
        let mut status = 0;
        unsafe { libc::waitpid(pid, &mut status, 0) };
        assert!(
            libc::WIFSIGNALED(status) && libc::WTERMSIG(status) == libc::SIGABRT,
            "the fork did not abort: status {status}"
        );
    }

    #[test]
    #[should_panic(expected = "lock of rank 4 taken while holding ranks")]
    fn a_lock_taken_below_a_held_rank_is_refused() {
        let _volumes = crate::shm::VOLUMES.lock();
        let _alloc = crate::shm::ALLOC_MUTEX.lock();
    }

    #[test]
    fn a_fork_from_a_thread_holding_a_runtime_lock_aborts() {
        let pid = unsafe { libc::fork() };
        assert!(pid >= 0);
        if pid == 0 {
            unsafe { libc::alarm(5) };
            let _alloc = crate::shm::ALLOC_MUTEX.lock();
            let grandchild = unsafe { libc::fork() };
            unsafe { libc::_exit(if grandchild == 0 { 0 } else { 1 }) };
        }
        let mut status = 0;
        unsafe { libc::waitpid(pid, &mut status, 0) };
        assert!(
            libc::WIFSIGNALED(status) && libc::WTERMSIG(status) == libc::SIGABRT,
            "the fork neither aborted nor completed cleanly: status {status}"
        );
    }
}
