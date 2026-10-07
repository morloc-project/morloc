use std::cell::Cell;
use std::mem::ManuallyDrop;
use std::ops::{Deref, DerefMut};
use std::sync::atomic::{AtomicPtr, AtomicUsize, Ordering};
use std::sync::{Condvar, LockResult, Mutex, MutexGuard, PoisonError};

pub(crate) use morloc_runtime_types::fork_generation::generation;

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
    morloc_runtime_types::panic::fatal(&format!("morloc: lock of rank {rank} taken while holding ranks {held:#b}"))
}

// PANIC-2: a panic holding a runtime lock may have torn what it guards.
pub(crate) fn holds_no_runtime_lock() -> bool {
    HELD_RANKS.try_with(Cell::get).unwrap_or(1) == 0 && RESETS_HELD.try_with(Cell::get).unwrap_or(1) == 0
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
        let guard = self.mutex.lock().unwrap_or_else(|_| morloc_runtime_types::panic::poisoned_lock());
        HELD_RANKS.with(|r| r.set(held | bit));
        HeldGuard { guard: Some(guard), bit }
    }

    pub(crate) fn try_lock(&self) -> Option<HeldGuard<'_, T>> {
        let guard = match self.mutex.try_lock() {
            Ok(g) => g,
            Err(std::sync::TryLockError::Poisoned(_)) => morloc_runtime_types::panic::poisoned_lock(),
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
        let guard = condvar.wait(guard).unwrap_or_else(|_| morloc_runtime_types::panic::poisoned_lock());
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
    views: HeldGuard<'static, Vec<crate::arrow_shm::BorrowEntry>>,
    lease: Option<crate::lease::Staged>,
    lease_rels: Vec<crate::shm::RelPtr>,
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

struct ForkHeldSlot(std::cell::UnsafeCell<Option<ForkHeld>>);

// SAFETY: FORK-5: only the thread holding every held lock touches the slot.
unsafe impl Sync for ForkHeldSlot {}

static FORK_HELD: ForkHeldSlot = ForkHeldSlot(std::cell::UnsafeCell::new(None));

fn take_fork_held() -> Option<ForkHeld> {
    // SAFETY: FORK-5: called by the forking thread, which holds every held lock.
    unsafe { (*FORK_HELD.0.get()).take() }
}

extern "C" fn prepare_fork() {
    // PANIC-2
    morloc_runtime_types::panic::outside_scope(prepare_fork_body)
}

fn prepare_fork_body() {
    if HELD_RANKS.with(Cell::get) != 0 || RESETS_HELD.with(Cell::get) != 0 {
        let msg = b"morloc: fork from a thread holding a runtime lock\n";
        unsafe {
            libc::write(2, msg.as_ptr() as *const libc::c_void, msg.len());
            libc::abort();
        }
    }
    // FORK-15: staged now; the fork holds the locks guarding its paths below,
    // and no file is written while they are held.
    let staged = if crate::arrow_shm::any_views() {
        crate::lease::paths().and_then(|p| crate::lease::stage(&p).ok())
    } else {
        None
    };
    let pass = crate::stream::RELEASE_PASS.lock();
    crate::stream::drain_before_fork();
    let held = ForkHeld {
        _pass: pass,
        map: crate::stream::PROCESS_LOCAL_SLOTS.lock(),
        locked_fds: crate::stream::LOCKED_FDS.lock(),
        views: crate::arrow_shm::BORROWABLE.lock(),
        lease: None,
        lease_rels: Vec::new(),
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
    let mut held = held;
    // FORK-15: the child's views read blocks its lease holds references on.
    let rels = crate::arrow_shm::fork_prepare_views(&held._volumes, &held.views);
    if !rels.is_empty() {
        crate::shm::hand_on_references(rels.len());
        if staged.is_none() {
            let msg = b"morloc: could not record a forked child's view references; they stay held until the run ends\n";
            unsafe { libc::write(2, msg.as_ptr() as *const libc::c_void, msg.len()) };
        }
    }
    held.lease = staged;
    held.lease_rels = rels;
    // SAFETY: FORK-5: this thread now holds every held lock.
    unsafe { *FORK_HELD.0.get() = Some(held) };
}

extern "C" fn after_fork_in_parent() {
    // PANIC-2
    morloc_runtime_types::panic::outside_scope(after_fork_in_parent_body)
}

fn after_fork_in_parent_body() {
    crate::lease::note_forked();
    if let Some(mut held) = take_fork_held() {
        let (lease, rels) = (held.lease.take(), std::mem::take(&mut held.lease_rels));
        drop(held);
        // FORK-15: written and placed once the locks are released; closing
        // this copy leaves the lock to the child.
        if let Some(lease) = lease {
            lease.place(&rels);
        }
    }
}

extern "C" fn after_fork_in_child() {
    // PANIC-2
    morloc_runtime_types::panic::outside_scope(after_fork_in_child_body)
}

fn after_fork_in_child_body() {
    morloc_runtime_types::fork_generation::bump_in_child();
    // FORK-16: the dispatches running anywhere in the parent, none of which
    // the child has.
    INHERITED_DISPATCHES.store(crate::intrinsics::dispatches_in_flight(), Ordering::Relaxed);
    crate::cell::after_fork_in_child();
    crate::shm::forget_held_references();
    if let Some(mut held) = take_fork_held() {
        crate::stream::after_fork_in_child(&mut held.map, &mut held.locked_fds);
        // FORK-15: the child keeps its lease locked for as long as it lives.
        let lease = held.lease.take();
        if let Some(l) = &lease {
            l.mark_child();
        }
        std::mem::forget(lease);
    }
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

// FORK-6
pub fn thread_count() -> Option<usize> {
    // FORK-6: a thread past its join but not yet gone is not counted.
    #[cfg(target_os = "linux")]
    {
        const PF_EXITING: u64 = 0x4;
        let mut live = 0;
        for task in std::fs::read_dir("/proc/self/task").ok()? {
            let Ok(stat) = std::fs::read_to_string(task.ok()?.path().join("stat")) else { continue };
            let Some(rest) = stat.rsplit_once(')').map(|(_, r)| r) else { continue };
            let fields: Vec<&str> = rest.split_whitespace().collect();
            let exiting = matches!(fields.first(), Some(&"Z") | Some(&"X"))
                || fields.get(6).and_then(|f| f.parse::<u64>().ok()).is_some_and(|f| f & PF_EXITING != 0);
            if !exiting {
                live += 1;
            }
        }
        Some(live)
    }
    #[cfg(target_os = "macos")]
    {
        let mut info: libc::proc_taskinfo = unsafe { std::mem::zeroed() };
        let size = std::mem::size_of::<libc::proc_taskinfo>() as libc::c_int;
        let got = unsafe {
            libc::proc_pidinfo(
                libc::getpid(),
                libc::PROC_PIDTASKINFO,
                0,
                &mut info as *mut libc::proc_taskinfo as *mut libc::c_void,
                size,
            )
        };
        (got == size).then_some(info.pti_threadnum as usize)
    }
}

// FORK-6: -1 when the count cannot be read.
pub(crate) fn morloc_thread_count() -> libc::c_long {
    thread_count().map_or(-1, |n| n as libc::c_long)
}

pub const FORK_WORKER_REFUSED: libc::pid_t = -2;

// FORK-6: the parent counts its threads once every prepare handler has run,
// so a library that ends its threads at fork is not counted; the child runs
// only once the count is one. Returns the child's pid, 0 in the child, -1
// with `error` set on a failed fork, or FORK_WORKER_REFUSED with
// `threads_at_fork` set.
pub(crate) unsafe fn morloc_fork_worker(threads_at_fork: *mut libc::c_long, error: *mut libc::c_int) -> libc::pid_t {
    let mut gate = [0i32; 2];
    if morloc_runtime_types::fd::pipe(gate.as_mut_ptr()) != 0 {
        if !error.is_null() {
            *error = crate::utility::errno_val();
        }
        return -1;
    }
    let pid = libc::fork();
    if pid == 0 {
        libc::close(gate[1]);
        let mut go = 0u8;
        let got = loop {
            let n = libc::read(gate[0], &mut go as *mut u8 as *mut libc::c_void, 1);
            if n < 0 && std::io::Error::last_os_error().kind() == std::io::ErrorKind::Interrupted {
                continue;
            }
            break n;
        };
        libc::close(gate[0]);
        if got != 1 {
            libc::_exit(FORK_REFUSED_EXIT);
        }
        return 0;
    }
    libc::close(gate[0]);
    if pid < 0 {
        if !error.is_null() {
            *error = crate::utility::errno_val();
        }
        libc::close(gate[1]);
        return -1;
    }
    let n = thread_count().map_or(-1, |n| n as libc::c_long);
    if !threads_at_fork.is_null() {
        *threads_at_fork = n;
    }
    if n == 1 {
        libc::write(gate[1], &1u8 as *const u8 as *const libc::c_void, 1);
        libc::close(gate[1]);
        return pid;
    }
    libc::close(gate[1]);
    // FORK-6: the child may never reach its gate, or another fork may hold it open.
    libc::kill(pid, libc::SIGKILL);
    let mut st = 0;
    while libc::waitpid(pid, &mut st, 0) < 0
        && std::io::Error::last_os_error().kind() == std::io::ErrorKind::Interrupted
    {}
    FORK_WORKER_REFUSED
}

const FORK_REFUSED_EXIT: libc::c_int = 71;

// Written directly: a forked test process's captured output dies with it.
#[cfg(test)]
fn report_status(what: &str, status: i32) {
    let line = format!("{what} ended with status {status}\n");
    unsafe { libc::write(2, line.as_ptr() as *const libc::c_void, line.len()) };
}

#[cfg(test)]
pub(crate) fn exits_cleanly_in_a_forked_child(work: impl FnOnce() -> bool) -> bool {
    let pid = unsafe { libc::fork() };
    if pid == 0 {
        run_in_child(5, work);
    }
    wait_status(pid) == 0
}

#[cfg(test)]
pub(crate) fn wait_status(pid: libc::pid_t) -> i32 {
    if pid < 0 {
        return 2;
    }
    let mut st = 0;
    loop {
        let r = unsafe { libc::waitpid(pid, &mut st, 0) };
        if r == pid {
            break;
        }
        if r < 0 && std::io::Error::last_os_error().raw_os_error() != Some(libc::EINTR) {
            return 2;
        }
    }
    if libc::WIFEXITED(st) { libc::WEXITSTATUS(st) } else { 100 + libc::WTERMSIG(st) }
}

#[cfg(test)]
extern "C" fn exit_on_alarm(_: libc::c_int) {
    unsafe { libc::_exit(114) };
}

#[cfg(test)]
fn run_in_child(seconds: u32, work: impl FnOnce() -> bool) -> ! {
    unsafe { libc::signal(libc::SIGALRM, exit_on_alarm as *const () as libc::sighandler_t) };
    unsafe { libc::alarm(seconds) };
    let ok = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(work)) {
        Ok(ok) => ok,
        Err(panic) => {
            let what = panic
                .downcast_ref::<String>()
                .map(String::as_str)
                .or_else(|| panic.downcast_ref::<&str>().copied())
                .unwrap_or("a panic");
            let line = format!("a forked test process panicked: {what}\n");
            unsafe { libc::write(2, line.as_ptr() as *const libc::c_void, line.len()) };
            false
        }
    };
    unsafe { libc::_exit(if ok { 0 } else { 1 }) }
}

// FORK-14: runs `body` as pid 1 of new user and pid namespaces; None where unavailable.
#[cfg(all(test, target_os = "linux"))]
pub(crate) fn as_pid_one(body: impl FnOnce() -> bool) -> Option<bool> {
    let uid = unsafe { libc::getuid() };
    let gid = unsafe { libc::getgid() };
    let child = unsafe { libc::fork() };
    assert!(child >= 0);
    if child == 0 {
        if unsafe { libc::unshare(libc::CLONE_NEWUSER | libc::CLONE_NEWPID) } != 0 {
            unsafe { libc::_exit(77) };
        }
        let mapped = std::fs::write("/proc/self/setgroups", "deny").is_ok()
            && std::fs::write("/proc/self/uid_map", format!("0 {uid} 1")).is_ok()
            && std::fs::write("/proc/self/gid_map", format!("0 {gid} 1")).is_ok();
        if !mapped {
            unsafe { libc::_exit(77) };
        }
        let one = unsafe { libc::fork() };
        if one == 0 {
            run_in_child(10, || {
                let one = std::process::id() == 1;
                if !one {
                    report_status("a process meant to be pid one is not; its pid", std::process::id() as i32);
                }
                one && body()
            });
        }
        unsafe { libc::_exit(wait_status(one)) };
    }
    match wait_status(child) {
        77 if std::env::var_os("MORLOC_REQUIRE_NAMESPACES").is_none_or(|v| v.is_empty()) => {
            eprintln!("skipped: unprivileged user and pid namespaces are unavailable");
            None
        }
        code => {
            if code != 0 {
                report_status("the pid-one process", code);
            }
            Some(code == 0)
        }
    }
}

// FORK-14: runs `work` in a descendant that is pid 1 of its own namespace.
#[cfg(all(test, target_os = "linux"))]
pub(crate) fn in_a_descendant_with_the_same_pid(work: impl FnOnce() -> bool) -> bool {
    let me = std::process::id();
    let child = unsafe { libc::fork() };
    assert!(child >= 0);
    if child == 0 {
        if unsafe { libc::unshare(libc::CLONE_NEWPID) } != 0 {
            unsafe { libc::_exit(78) };
        }
        let descendant = unsafe { libc::fork() };
        if descendant == 0 {
            run_in_child(5, || std::process::id() == me && work());
        }
        unsafe { libc::_exit(wait_status(descendant)) };
    }
    // 78: no new pid namespace; 1: the work returned false; 114: timed out;
    // over 100: killed by signal (status - 100).
    let status = wait_status(child);
    if status != 0 {
        report_status("the same-pid descendant", status);
    }
    status == 0
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    #[test]
    fn a_worker_forked_from_a_process_with_another_thread_never_runs() {
        assert!(exits_cleanly_in_a_forked_child(|| {
            let (tx, rx) = std::sync::mpsc::channel::<()>();
            let t = std::thread::spawn(move || {
                let _ = rx.recv();
            });
            let mut ran = [0i32; 2];
            unsafe { morloc_runtime_types::fd::pipe(ran.as_mut_ptr()) };
            let mut threads = 0;
            let pid = unsafe { morloc_fork_worker(&mut threads, std::ptr::null_mut()) };
            if pid == 0 {
                unsafe {
                    libc::write(ran[1], b"x".as_ptr() as *const libc::c_void, 1);
                    libc::_exit(0)
                };
            }
            unsafe { libc::close(ran[1]) };
            let mut b = 0u8;
            let child_wrote = unsafe { libc::read(ran[0], &mut b as *mut u8 as *mut libc::c_void, 1) } == 1;
            drop(tx);
            let _ = t.join();
            pid == FORK_WORKER_REFUSED && threads == 2 && !child_wrote
        }));
    }

    #[test]
    fn a_worker_forked_from_a_lone_thread_runs() {
        assert!(exits_cleanly_in_a_forked_child(|| {
            let pid = unsafe { morloc_fork_worker(std::ptr::null_mut(), std::ptr::null_mut()) };
            if pid == 0 {
                unsafe { libc::_exit(7) };
            }
            pid > 0 && wait_status(pid) == 7
        }));
    }

    #[test]
    fn a_forked_child_counts_only_its_own_threads() {
        assert!(exits_cleanly_in_a_forked_child(|| {
            let alone = thread_count() == Some(1);
            let (tx, rx) = std::sync::mpsc::channel::<()>();
            let t = std::thread::spawn(move || {
                let _ = rx.recv();
            });
            let two = thread_count() == Some(2);
            drop(tx);
            let _ = t.join();
            alone && two
        }));
    }

    #[cfg(target_os = "linux")]
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
    fn a_fork_from_a_thread_local_destructor_completes() {
        fn fork_and_reap() {
            let pid = unsafe { libc::fork() };
            if pid == 0 {
                unsafe { libc::_exit(0) };
            }
            wait_status(pid);
        }
        struct ForksOnDrop;
        impl Drop for ForksOnDrop {
            fn drop(&mut self) {
                fork_and_reap();
            }
        }
        thread_local! {
            static LATE: ForksOnDrop = const { ForksOnDrop };
        }
        let ok = exits_cleanly_in_a_forked_child(|| {
            std::thread::spawn(|| {
                LATE.with(|_| {});
                fork_and_reap();
            })
            .join()
            .is_ok()
        });
        assert!(ok, "a fork from a thread-local destructor did not complete");
    }

    #[test]
    #[should_panic(expected = "lock of rank 5 taken while holding ranks")]
    fn a_lock_taken_below_a_held_rank_is_refused() {
        let outer = Held::new(6, ());
        let inner = Held::new(5, ());
        let _outer = outer.lock();
        let _inner = inner.lock();
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

mod c_abi {

    #[no_mangle]
    pub extern "C" fn morloc_thread_count() -> libc::c_long {
        super::morloc_thread_count()
    }

    #[no_mangle]
    pub unsafe extern "C" fn morloc_fork_worker(threads_at_fork: *mut libc::c_long, error: *mut libc::c_int) -> libc::pid_t {
        super::morloc_fork_worker(threads_at_fork, error)
    }
}
