//! A cross-process lock that survives its holder's death.
//!
//! Unlike `ShmLock`, a death inside this lock does not disable it: the next
//! taker gets the lock and is told the holder died, so it can mark what the
//! lock protects as unusable and still release it. The lock initialises
//! itself on first use, so zeroed shared memory holds valid locks without
//! every page being touched up front.
//!
//! Linux uses a robust, process-shared, error-checking pthread mutex, whose
//! owner death the kernel reports. macOS has no robust mutexes; there the
//! lock word holds the owner's pid and start time, and a waiter takes the
//! lock over once that process is gone.

use std::marker::PhantomData;

use crate::error::MorlocError;

#[cfg(target_os = "linux")]
use std::cell::UnsafeCell;
#[cfg(target_os = "linux")]
use std::sync::atomic::{AtomicU32, Ordering};
#[cfg(not(target_os = "linux"))]
use std::sync::atomic::{AtomicU64, Ordering};

#[repr(C)]
pub struct RecoverableLock {
    /// 0 before first use, `READY` once the mutex is initialised, otherwise
    /// the pid of the process initialising it.
    #[cfg(target_os = "linux")]
    state: AtomicU32,
    #[cfg(target_os = "linux")]
    mutex: UnsafeCell<libc::pthread_mutex_t>,
    /// 0 when free, otherwise the owner's `process_token`.
    #[cfg(not(target_os = "linux"))]
    owner: AtomicU64,
}

// SAFETY: the lock exists to be shared; every access goes through
// pthread_mutex_* (Linux) or atomics (elsewhere).
unsafe impl Sync for RecoverableLock {}

/// Holds the lock until dropped. Not `Send`: a mutex must be released by
/// the thread that took it.
pub struct RecoverableGuard<'a> {
    lock: &'a RecoverableLock,
    _thread_bound: PhantomData<*const ()>,
}

impl Drop for RecoverableGuard<'_> {
    fn drop(&mut self) {
        // SAFETY: a guard exists only while this thread holds the lock.
        unsafe { self.lock.unlock() }
    }
}

/// A taken lock, and whether its previous holder died holding it.
pub struct Acquired<'a> {
    pub guard: RecoverableGuard<'a>,
    pub holder_died: bool,
}

fn relocked() -> MorlocError {
    MorlocError::Other("a lock was taken again by the thread already holding it".into())
}

/// Trylock rounds before a waiter blocks: most holds are a few hundred
/// nanoseconds, and a blocked waiter costs the holder a wake syscall.
#[cfg(target_os = "linux")]
const SPINS: u32 = 128;

#[cfg(target_os = "linux")]
const READY: u32 = u32::MAX;

#[cfg(target_os = "linux")]
impl RecoverableLock {
    pub fn lock(&self) -> Result<Acquired<'_>, MorlocError> {
        self.ensure_ready()?;
        let m = self.mutex.get();
        for _ in 0..SPINS {
            // SAFETY: `ensure_ready` initialised the mutex.
            match unsafe { libc::pthread_mutex_trylock(m) } {
                libc::EBUSY => std::hint::spin_loop(),
                rc => return self.taken(rc),
            }
        }
        // SAFETY: as above.
        let rc = unsafe { libc::pthread_mutex_lock(m) };
        self.taken(rc)
    }

    fn taken(&self, rc: i32) -> Result<Acquired<'_>, MorlocError> {
        let guard = || RecoverableGuard { lock: self, _thread_bound: PhantomData };
        match rc {
            0 => Ok(Acquired { guard: guard(), holder_died: false }),
            libc::EOWNERDEAD => {
                // SAFETY: this thread now owns the mutex.
                let rc = unsafe { libc::pthread_mutex_consistent(self.mutex.get()) };
                if rc != 0 {
                    unsafe { self.unlock() };
                    return Err(MorlocError::Other(format!(
                        "cannot recover a lock whose holder died: \
                         pthread_mutex_consistent returned {rc}"
                    )));
                }
                Ok(Acquired { guard: guard(), holder_died: true })
            }
            libc::EDEADLK => Err(relocked()),
            rc => Err(MorlocError::Other(format!(
                "cannot take a lock: pthread_mutex_lock returned {rc}"
            ))),
        }
    }

    /// Initialise the mutex the first time any process uses the lock.
    fn ensure_ready(&self) -> Result<(), MorlocError> {
        if self.state.load(Ordering::Acquire) == READY {
            return Ok(());
        }
        let me = std::process::id();
        let began = std::time::Instant::now();
        let mut waits: u32 = 0;
        loop {
            let s = self.state.load(Ordering::Acquire);
            if s == READY {
                return Ok(());
            }
            // Initialising takes nanoseconds; an initialiser still at it is
            // stopped, or its pid was reused after it died.
            if waits % 1024 == 1023 && began.elapsed() > std::time::Duration::from_secs(10) {
                return Err(MorlocError::Other(format!(
                    "a shared lock was never initialised: process {s} began and did not finish"
                )));
            }
            // An initialiser that died mid-way leaves its pid behind.
            let claimable = s == 0 || (waits % 1024 == 1023 && !pid_alive(s));
            if claimable
                && self.state.compare_exchange(s, me, Ordering::AcqRel, Ordering::Acquire).is_ok()
            {
                // SAFETY: no process uses the mutex until `READY` is published.
                if let Err(e) = unsafe { init_mutex(self.mutex.get()) } {
                    self.state.store(0, Ordering::Release);
                    return Err(e);
                }
                self.state.store(READY, Ordering::Release);
                return Ok(());
            }
            waits = waits.wrapping_add(1);
            if waits < 64 {
                std::hint::spin_loop();
            } else {
                std::thread::yield_now();
            }
        }
    }

    unsafe fn unlock(&self) {
        if libc::pthread_mutex_unlock(self.mutex.get()) != 0 {
            // The lock would stay held, and the kernel would later report a
            // death that never happened.
            std::process::abort();
        }
    }
}

#[cfg(target_os = "linux")]
unsafe fn init_mutex(m: *mut libc::pthread_mutex_t) -> Result<(), MorlocError> {
    let mut attr: libc::pthread_mutexattr_t = std::mem::zeroed();
    let fail = |what: &str, rc: i32| {
        MorlocError::Other(format!("cannot initialise a shared lock: {what} returned {rc}"))
    };
    let rc = libc::pthread_mutexattr_init(&mut attr);
    if rc != 0 {
        return Err(fail("pthread_mutexattr_init", rc));
    }
    let result = (|| {
        let rc = libc::pthread_mutexattr_setpshared(&mut attr, libc::PTHREAD_PROCESS_SHARED);
        if rc != 0 {
            return Err(fail("pthread_mutexattr_setpshared", rc));
        }
        let rc = libc::pthread_mutexattr_setrobust(&mut attr, libc::PTHREAD_MUTEX_ROBUST);
        if rc != 0 {
            return Err(fail("pthread_mutexattr_setrobust", rc));
        }
        let rc = libc::pthread_mutexattr_settype(&mut attr, libc::PTHREAD_MUTEX_ERRORCHECK);
        if rc != 0 {
            return Err(fail("pthread_mutexattr_settype", rc));
        }
        let rc = libc::pthread_mutex_init(m, &attr);
        if rc != 0 {
            return Err(fail("pthread_mutex_init", rc));
        }
        Ok(())
    })();
    libc::pthread_mutexattr_destroy(&mut attr);
    result
}

#[cfg(target_os = "linux")]
fn pid_alive(pid: u32) -> bool {
    let Ok(pid) = libc::pid_t::try_from(pid) else { return false };
    // SAFETY: signal 0 performs only the existence and permission check.
    unsafe {
        libc::kill(pid, 0) == 0
            || std::io::Error::last_os_error().raw_os_error() != Some(libc::ESRCH)
    }
}

#[cfg(not(target_os = "linux"))]
thread_local! {
    /// The process and the locks this thread holds, to refuse a second
    /// take of one of them: the owner word names a process, not a thread.
    /// A forked child starts with none.
    static HELD: std::cell::RefCell<(u32, Vec<usize>)> =
        const { std::cell::RefCell::new((0, Vec::new())) };
}

#[cfg(not(target_os = "linux"))]
fn with_held<R>(f: impl FnOnce(&mut Vec<usize>) -> R) -> R {
    let pid = std::process::id();
    HELD.with(|h| {
        let mut h = h.borrow_mut();
        if h.0 != pid {
            *h = (pid, Vec::new());
        }
        f(&mut h.1)
    })
}

#[cfg(not(target_os = "linux"))]
impl RecoverableLock {
    pub fn lock(&self) -> Result<Acquired<'_>, MorlocError> {
        let addr = self as *const Self as usize;
        if with_held(|h| h.contains(&addr)) {
            return Err(relocked());
        }
        let me = process_token();
        let mut waits: u32 = 0;
        let holder_died = loop {
            match self.owner.compare_exchange_weak(0, me, Ordering::Acquire, Ordering::Relaxed) {
                Ok(_) => break false,
                Err(0) => continue,
                Err(owner) => {
                    waits = waits.wrapping_add(1);
                    if waits % 64 == 0
                        && !token_alive(owner)
                        && self
                            .owner
                            .compare_exchange(owner, me, Ordering::Acquire, Ordering::Relaxed)
                            .is_ok()
                    {
                        break true;
                    }
                    if waits < 64 {
                        std::hint::spin_loop();
                    } else if waits < 1024 {
                        std::thread::yield_now();
                    } else {
                        std::thread::sleep(std::time::Duration::from_micros(200));
                    }
                }
            }
        };
        with_held(|h| h.push(addr));
        Ok(Acquired {
            guard: RecoverableGuard { lock: self, _thread_bound: PhantomData },
            holder_died,
        })
    }

    unsafe fn unlock(&self) {
        let addr = self as *const Self as usize;
        with_held(|h| h.retain(|&a| a != addr));
        let me = process_token();
        if self.owner.compare_exchange(me, 0, Ordering::Release, Ordering::Relaxed).is_err() {
            // Not ours: a guard inherited over fork, or a lock taken over
            // from a process wrongly judged dead. Releasing it would admit
            // a second holder.
            std::process::abort();
        }
    }
}

/// This process as an owner word: pid in the high half, the low bits of its
/// start time in the low half, so a reused pid does not pass for the owner.
#[cfg(not(target_os = "linux"))]
fn process_token() -> u64 {
    static CACHED: AtomicU64 = AtomicU64::new(0);
    let pid = std::process::id();
    let cached = CACHED.load(Ordering::Relaxed);
    if cached != 0 && (cached >> 32) as u32 == pid {
        return cached;
    }
    let stamp = process_state(pid).map_or(0, |(stamp, _)| stamp);
    let token = ((pid as u64) << 32) | stamp as u64;
    CACHED.store(token, Ordering::Relaxed);
    token
}

/// Whether the owner a token names may still be running. Anything this
/// cannot establish counts as alive: wrongly judging a live owner dead
/// would admit a second holder.
#[cfg(not(target_os = "linux"))]
fn token_alive(token: u64) -> bool {
    let pid = (token >> 32) as u32;
    let Ok(p) = libc::pid_t::try_from(pid) else { return true };
    // SAFETY: signal 0 performs only the existence and permission check.
    let gone = unsafe {
        libc::kill(p, 0) != 0
            && std::io::Error::last_os_error().raw_os_error() == Some(libc::ESRCH)
    };
    if gone {
        return false;
    }
    match process_state(pid) {
        Some((_, true)) => false,
        Some((stamp, false)) => {
            let owner_stamp = token as u32;
            stamp == 0 || owner_stamp == 0 || stamp == owner_stamp
        }
        None => true,
    }
}

/// Whether `pid` has exited and awaits its parent. False when unknown.
#[cfg(not(target_os = "linux"))]
pub fn pid_is_zombie(pid: u32) -> bool {
    process_state(pid).is_some_and(|(_, zombie)| zombie)
}

/// The low 32 bits of a process's start time in microseconds (0 when
/// unknown), and whether it has exited and awaits its parent.
#[cfg(target_vendor = "apple")]
fn process_state(pid: u32) -> Option<(u32, bool)> {
    let mut info: libc::proc_bsdinfo = unsafe { std::mem::zeroed() };
    let size = std::mem::size_of::<libc::proc_bsdinfo>() as libc::c_int;
    // A nonzero arg makes the lookup also find a process awaiting reaping.
    // SAFETY: `info` is a writable buffer of `size` bytes.
    let n = unsafe {
        libc::proc_pidinfo(
            pid as libc::c_int,
            libc::PROC_PIDTBSDINFO,
            1,
            &mut info as *mut libc::proc_bsdinfo as *mut libc::c_void,
            size,
        )
    };
    if n != size {
        return None;
    }
    let stamp = info.pbi_start_tvsec.wrapping_mul(1_000_000).wrapping_add(info.pbi_start_tvusec);
    Some((stamp as u32, info.pbi_status == libc::SZOMB))
}

#[cfg(all(not(target_os = "linux"), not(target_vendor = "apple")))]
fn process_state(_pid: u32) -> Option<(u32, bool)> {
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicU32, Ordering};

    unsafe fn shared_zeroed<T>() -> *mut T {
        let p = libc::mmap(
            std::ptr::null_mut(),
            std::mem::size_of::<T>(),
            libc::PROT_READ | libc::PROT_WRITE,
            libc::MAP_SHARED | libc::MAP_ANONYMOUS,
            -1,
            0,
        );
        assert_ne!(p, libc::MAP_FAILED);
        p as *mut T
    }

    fn exit_status(pid: libc::pid_t) -> i32 {
        let mut status = 0;
        unsafe { libc::waitpid(pid, &mut status, 0) };
        if libc::WIFEXITED(status) { libc::WEXITSTATUS(status) } else { -1 }
    }

    #[repr(C)]
    struct Shared {
        lock: RecoverableLock,
        owner: AtomicU32,
        violations: AtomicU32,
        died: AtomicU32,
    }

    // Several processes take a never-used lock at once, so they also race
    // its first-use initialisation, then hammer it. Any overlap inside the
    // lock is a violation.
    #[test]
    fn the_lock_admits_one_process_at_a_time() {
        const ROUNDS: u32 = 50_000;
        unsafe {
            let sh = &*shared_zeroed::<Shared>();
            let mut kids = Vec::new();
            for _ in 0..4 {
                let pid = libc::fork();
                assert!(pid >= 0);
                if pid == 0 {
                    let me = libc::getpid() as u32;
                    for _ in 0..ROUNDS {
                        let Ok(a) = sh.lock.lock() else { libc::_exit(2) };
                        if sh.owner.load(Ordering::SeqCst) != 0 {
                            sh.violations.fetch_add(1, Ordering::SeqCst);
                        }
                        sh.owner.store(me, Ordering::SeqCst);
                        for _ in 0..20 {
                            std::hint::spin_loop();
                        }
                        if sh.owner.load(Ordering::SeqCst) != me {
                            sh.violations.fetch_add(1, Ordering::SeqCst);
                        }
                        sh.owner.store(0, Ordering::SeqCst);
                        drop(a);
                    }
                    libc::_exit(0);
                }
                kids.push(pid);
            }
            for k in kids {
                assert_eq!(exit_status(k), 0);
            }
            assert_eq!(sh.violations.load(Ordering::SeqCst), 0);
        }
    }

    // A holder that dies hands the lock on, flagged, to the next taker, and
    // the lock keeps working afterwards.
    #[test]
    fn a_dead_holder_hands_the_lock_on() {
        unsafe {
            let sh = &*shared_zeroed::<Shared>();
            let holder = libc::fork();
            assert!(holder >= 0);
            if holder == 0 {
                let Ok(a) = sh.lock.lock() else { libc::_exit(2) };
                std::mem::forget(a);
                libc::_exit(0);
            }
            assert_eq!(exit_status(holder), 0);
            let a = sh.lock.lock().expect("lock after the holder died");
            assert!(a.holder_died, "the holder's death was not reported");
            drop(a);
            let b = sh.lock.lock().expect("lock after recovery");
            assert!(!b.holder_died, "a recovered lock reported a death twice");
        }
    }

    // A holder that has exited but awaits reaping is gone: nothing it held
    // can be released by it, so the lock passes on without the reap.
    #[test]
    fn a_holder_awaiting_reaping_hands_the_lock_on() {
        unsafe {
            let sh = &*shared_zeroed::<Shared>();
            let holder = libc::fork();
            assert!(holder >= 0);
            if holder == 0 {
                let Ok(a) = sh.lock.lock() else { libc::_exit(2) };
                std::mem::forget(a);
                libc::_exit(0);
            }
            let mut info: libc::siginfo_t = std::mem::zeroed();
            libc::waitid(libc::P_PID, holder as libc::id_t, &mut info, libc::WEXITED | libc::WNOWAIT);
            let lock = &sh.lock as *const RecoverableLock as usize;
            let (tx, rx) = std::sync::mpsc::channel();
            std::thread::spawn(move || {
                let lock = &*(lock as *const RecoverableLock);
                let _ = tx.send(lock.lock().map(|a| a.holder_died));
            });
            let got = rx.recv_timeout(std::time::Duration::from_secs(10));
            assert_eq!(exit_status(holder), 0);
            let died = got.expect("lock never passed on from an unreaped holder");
            assert!(died.expect("lock after the holder exited"), "the holder's death was not reported");
        }
    }

    // A waiter already blocked when the holder dies is woken with the lock.
    #[test]
    fn a_blocked_waiter_survives_the_holder() {
        unsafe {
            let sh = &*shared_zeroed::<Shared>();
            let mut ready = [0 as libc::c_int; 2];
            assert_eq!(libc::pipe(ready.as_mut_ptr()), 0);
            let holder = libc::fork();
            assert!(holder >= 0);
            if holder == 0 {
                let Ok(a) = sh.lock.lock() else { libc::_exit(2) };
                std::mem::forget(a);
                libc::write(ready[1], b"x".as_ptr() as *const libc::c_void, 1);
                loop {
                    libc::pause();
                }
            }
            let mut b = 0u8;
            libc::read(ready[0], &mut b as *mut u8 as *mut libc::c_void, 1);
            let waiter = libc::fork();
            assert!(waiter >= 0);
            if waiter == 0 {
                let Ok(a) = sh.lock.lock() else { libc::_exit(2) };
                if a.holder_died {
                    sh.died.store(1, Ordering::SeqCst);
                }
                drop(a);
                libc::_exit(0);
            }
            std::thread::sleep(std::time::Duration::from_millis(200));
            libc::kill(holder, libc::SIGKILL);
            exit_status(holder);
            assert_eq!(exit_status(waiter), 0);
            assert_eq!(sh.died.load(Ordering::SeqCst), 1);
        }
    }

    // A thread taking a lock it already holds gets an error, not a hang.
    #[test]
    fn a_second_take_by_the_holder_is_refused() {
        unsafe {
            let sh = &*shared_zeroed::<Shared>();
            let a = sh.lock.lock().unwrap();
            assert!(sh.lock.lock().is_err());
            drop(a);
            assert!(sh.lock.lock().is_ok());
        }
    }
}

