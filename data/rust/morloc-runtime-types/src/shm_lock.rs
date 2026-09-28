//! The cross-process lock in every SHM volume header.
//!
//! Every process of a program allocates from the same volumes, so the
//! allocator's critical section (find a free block, split it, mark it owned)
//! must admit one process at a time. A process can die inside that section,
//! leaving the block list half-edited. The lock never hands such a section
//! to another process: a torn split cannot be repaired from outside, so the
//! lock is poisoned and every later attempt fails with an error instead of
//! allocating from a corrupt list.
//!
//! Linux uses a process-shared robust pthread mutex, whose death detection
//! the kernel performs. macOS has no robust mutexes; there the lock word
//! holds the owner's pid and a waiter poisons it once that pid is gone.

use crate::error::MorlocError;

#[cfg(target_os = "linux")]
use std::cell::UnsafeCell;
#[cfg(not(target_os = "linux"))]
use std::sync::atomic::{AtomicU32, Ordering};

#[repr(C)]
pub struct ShmLock {
    #[cfg(target_os = "linux")]
    mutex: UnsafeCell<libc::pthread_mutex_t>,
    #[cfg(not(target_os = "linux"))]
    holder: AtomicU32,
}

// SAFETY: the lock exists to be shared; every access goes through
// pthread_mutex_* (Linux) or atomics (elsewhere).
unsafe impl Sync for ShmLock {}

/// Holds the lock until dropped.
pub struct ShmGuard<'a> {
    lock: &'a ShmLock,
}

impl Drop for ShmGuard<'_> {
    fn drop(&mut self) {
        // SAFETY: a guard exists only while this thread holds the lock.
        unsafe { self.lock.unlock() }
    }
}

fn poisoned() -> MorlocError {
    MorlocError::Shm(
        "a process died while allocating from this shared memory volume; \
         its block list may be inconsistent, so the volume can no longer \
         allocate"
            .into(),
    )
}

#[cfg(target_os = "linux")]
impl ShmLock {
    /// Initialise the lock in place.
    ///
    /// # Safety
    /// `this` must point to writable memory shared by every process that
    /// will take the lock, and no process may use the lock until this
    /// returns. Initialising a lock that is in use is undefined behaviour.
    pub unsafe fn init(this: *mut ShmLock) -> Result<(), MorlocError> {
        let mut attr: libc::pthread_mutexattr_t = std::mem::zeroed();
        let fail = |what: &str, rc: i32| {
            MorlocError::Shm(format!("cannot initialise the volume lock: {what} returned {rc}"))
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
            // A thread relocking its own lock gets EDEADLK instead of hanging.
            let rc = libc::pthread_mutexattr_settype(&mut attr, libc::PTHREAD_MUTEX_ERRORCHECK);
            if rc != 0 {
                return Err(fail("pthread_mutexattr_settype", rc));
            }
            let rc = libc::pthread_mutex_init((*this).mutex.get(), &attr);
            if rc != 0 {
                return Err(fail("pthread_mutex_init", rc));
            }
            Ok(())
        })();
        libc::pthread_mutexattr_destroy(&mut attr);
        result
    }

    pub fn lock(&self) -> Result<ShmGuard<'_>, MorlocError> {
        // SAFETY: the mutex was initialised by `init` before the volume
        // became visible.
        match unsafe { libc::pthread_mutex_lock(self.mutex.get()) } {
            0 => Ok(ShmGuard { lock: self }),
            libc::EOWNERDEAD => {
                // Unlocking without marking the mutex consistent makes it
                // permanently unrecoverable: every later lock fails.
                unsafe { libc::pthread_mutex_unlock(self.mutex.get()) };
                Err(poisoned())
            }
            libc::ENOTRECOVERABLE => Err(poisoned()),
            rc => Err(MorlocError::Shm(format!(
                "cannot take the volume lock: pthread_mutex_lock returned {rc}"
            ))),
        }
    }

    unsafe fn unlock(&self) {
        libc::pthread_mutex_unlock(self.mutex.get());
    }
}

#[cfg(not(target_os = "linux"))]
const FREE: u32 = 0;
#[cfg(not(target_os = "linux"))]
const POISONED: u32 = u32::MAX;

#[cfg(not(target_os = "linux"))]
impl ShmLock {
    /// Initialise the lock in place.
    ///
    /// # Safety
    /// As on Linux: shared writable memory, and no user until this returns.
    pub unsafe fn init(this: *mut ShmLock) -> Result<(), MorlocError> {
        std::ptr::addr_of_mut!((*this).holder).write(AtomicU32::new(FREE));
        Ok(())
    }

    /// The pid word cannot tell apart two threads of one process; the
    /// caller serialises its own threads before taking this lock.
    pub fn lock(&self) -> Result<ShmGuard<'_>, MorlocError> {
        let me = std::process::id();
        let mut waits: u32 = 0;
        loop {
            match self.holder.compare_exchange_weak(FREE, me, Ordering::Acquire, Ordering::Relaxed) {
                Ok(_) => return Ok(ShmGuard { lock: self }),
                Err(POISONED) => return Err(poisoned()),
                Err(FREE) => continue,
                Err(owner) => {
                    waits = waits.wrapping_add(1);
                    if waits % 1024 == 0 && !pid_alive(owner) {
                        // Only the waiter that wins this exchange poisons;
                        // any other sees POISONED on its next attempt.
                        let _ = self.holder.compare_exchange(
                            owner, POISONED, Ordering::AcqRel, Ordering::Relaxed,
                        );
                        return Err(poisoned());
                    }
                    if waits < 64 {
                        std::hint::spin_loop();
                    } else {
                        std::thread::yield_now();
                    }
                }
            }
        }
    }

    unsafe fn unlock(&self) {
        self.holder.store(FREE, Ordering::Release);
    }
}

#[cfg(not(target_os = "linux"))]
fn pid_alive(pid: u32) -> bool {
    let Ok(pid) = libc::pid_t::try_from(pid) else { return false };
    // SAFETY: signal 0 performs only the existence and permission check.
    unsafe { libc::kill(pid, 0) == 0 || *libc::__error() != libc::ESRCH }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicU32, Ordering};

    unsafe fn shared_map<T>() -> *mut T {
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

    unsafe fn wait_ok(pid: libc::pid_t) {
        let mut status = 0;
        libc::waitpid(pid, &mut status, 0);
        assert!(libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0, "child failed: {status}");
    }

    #[repr(C)]
    struct Shared {
        lock: ShmLock,
        owner: AtomicU32,
        violations: AtomicU32,
    }

    // Two processes hammer the lock through a shared mapping: each takes it,
    // checks the owner word is free, marks it, spins, checks the mark
    // survived, and releases. Any overlap is a violation.
    #[test]
    fn the_lock_admits_one_process_at_a_time() {
        const ROUNDS: u32 = 200_000;
        unsafe {
            let sh = shared_map::<Shared>();
            ShmLock::init(std::ptr::addr_of_mut!((*sh).lock)).unwrap();
            let sh = &*sh;
            let mut kids = Vec::new();
            for _ in 0..2 {
                let pid = libc::fork();
                assert!(pid >= 0);
                if pid == 0 {
                    let me = libc::getpid() as u32;
                    for _ in 0..ROUNDS {
                        let Ok(_g) = sh.lock.lock() else { libc::_exit(2) };
                        if sh.owner.load(Ordering::SeqCst) != 0 {
                            sh.violations.fetch_add(1, Ordering::SeqCst);
                        }
                        sh.owner.store(me, Ordering::SeqCst);
                        for _ in 0..50 {
                            std::hint::spin_loop();
                        }
                        if sh.owner.load(Ordering::SeqCst) != me {
                            sh.violations.fetch_add(1, Ordering::SeqCst);
                        }
                        sh.owner.store(0, Ordering::SeqCst);
                    }
                    libc::_exit(0);
                }
                kids.push(pid);
            }
            for k in kids {
                wait_ok(k);
            }
            let v = sh.violations.load(Ordering::SeqCst);
            assert_eq!(v, 0, "{v} rounds saw another process inside the lock");
        }
    }

    // A holder that keeps the lock for a long time is waited for, never
    // robbed, however long the wait.
    #[test]
    fn a_slow_holder_is_not_robbed() {
        unsafe {
            let sh = shared_map::<Shared>();
            ShmLock::init(std::ptr::addr_of_mut!((*sh).lock)).unwrap();
            let sh = &*sh;
            let holder = libc::fork();
            assert!(holder >= 0);
            if holder == 0 {
                let Ok(g) = sh.lock.lock() else { libc::_exit(2) };
                sh.owner.store(1, Ordering::SeqCst);
                std::thread::sleep(std::time::Duration::from_millis(6000));
                if sh.owner.load(Ordering::SeqCst) != 1 {
                    libc::_exit(3);
                }
                sh.owner.store(0, Ordering::SeqCst);
                drop(g);
                libc::_exit(0);
            }
            while sh.owner.load(Ordering::SeqCst) != 1 {
                std::thread::yield_now();
            }
            let waiter = libc::fork();
            assert!(waiter >= 0);
            if waiter == 0 {
                let Ok(g) = sh.lock.lock() else { libc::_exit(2) };
                if sh.owner.load(Ordering::SeqCst) != 0 {
                    libc::_exit(4);
                }
                drop(g);
                libc::_exit(0);
            }
            wait_ok(holder);
            wait_ok(waiter);
        }
    }

    // A process that dies holding the lock poisons it: the next taker and
    // every one after gets an error rather than the half-edited section.
    #[test]
    fn a_dead_holder_poisons_the_lock() {
        unsafe {
            let sh = shared_map::<Shared>();
            ShmLock::init(std::ptr::addr_of_mut!((*sh).lock)).unwrap();
            let sh = &*sh;
            let holder = libc::fork();
            assert!(holder >= 0);
            if holder == 0 {
                let g = sh.lock.lock();
                std::mem::forget(g);
                libc::_exit(0);
            }
            wait_ok(holder);
            for _ in 0..3 {
                let err = sh.lock.lock().err().expect("a poisoned lock was taken");
                assert!(err.to_string().contains("died"), "{err}");
            }
        }
    }
}
