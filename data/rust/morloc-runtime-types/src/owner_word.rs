//! A cross-process lock built from one word naming its owner, for platforms
//! without robust mutexes (macOS). The word holds the owner's
//! `process::token`; a waiter takes the lock over once that process is gone
//! and is told so. Built on every platform so its tests run everywhere.

#![cfg_attr(target_os = "linux", allow(dead_code))]

use std::sync::atomic::{AtomicU64, Ordering};

use crate::error::MorlocError;

#[repr(C)]
pub struct OwnerWord {
    /// 0 when free, otherwise the owner's `process::token`.
    owner: AtomicU64,
}

thread_local! {
    /// The process and the words this thread holds, to refuse a second take
    /// of one of them: the word names a process, not a thread. A forked child
    /// starts with none.
    static HELD: std::cell::RefCell<(u32, Vec<usize>)> =
        const { std::cell::RefCell::new((0, Vec::new())) };
}

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

pub fn relocked() -> MorlocError {
    MorlocError::Other("a lock was taken again by the thread already holding it".into())
}

impl OwnerWord {
    pub const fn new() -> Self {
        OwnerWord { owner: AtomicU64::new(0) }
    }

    /// Take the word, waiting while its owner lives. Returns whether the
    /// previous owner died holding it.
    pub fn acquire(&self) -> Result<bool, MorlocError> {
        let addr = self as *const Self as usize;
        if with_held(|h| h.contains(&addr)) {
            return Err(relocked());
        }
        let me = crate::process::token();
        let mut waits: u32 = 0;
        let holder_died = loop {
            match self.owner.compare_exchange_weak(0, me, Ordering::Acquire, Ordering::Relaxed) {
                Ok(_) => break false,
                Err(0) => continue,
                Err(owner) => {
                    waits = waits.wrapping_add(1);
                    if waits % 64 == 0
                        && !crate::process::token_alive(owner)
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
        Ok(holder_died)
    }

    /// Release a word this thread took.
    ///
    /// # Safety
    /// The calling thread must hold the word.
    pub unsafe fn release(&self) {
        let addr = self as *const Self as usize;
        with_held(|h| h.retain(|&a| a != addr));
        let me = crate::process::token();
        if self.owner.compare_exchange(me, 0, Ordering::Release, Ordering::Relaxed).is_err() {
            // Not ours: a hold inherited over fork, or a word taken over
            // from a process wrongly judged dead. Releasing it would admit
            // a second holder.
            std::process::abort();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::AtomicU32;

    #[repr(C)]
    struct Shared {
        word: OwnerWord,
        inside: AtomicU32,
        violations: AtomicU32,
    }

    unsafe fn shared() -> &'static Shared {
        let p = libc::mmap(
            std::ptr::null_mut(),
            std::mem::size_of::<Shared>(),
            libc::PROT_READ | libc::PROT_WRITE,
            libc::MAP_SHARED | libc::MAP_ANONYMOUS,
            -1,
            0,
        );
        assert_ne!(p, libc::MAP_FAILED);
        &*(p as *const Shared)
    }

    fn exit_status(pid: libc::pid_t) -> i32 {
        let mut status = 0;
        unsafe { libc::waitpid(pid, &mut status, 0) };
        if libc::WIFEXITED(status) { libc::WEXITSTATUS(status) } else { -1 }
    }

    /// Fork a child that takes the word and exits holding it.
    unsafe fn dead_holder(sh: &Shared) -> libc::pid_t {
        let pid = libc::fork();
        assert!(pid >= 0);
        if pid == 0 {
            let _ = sh.word.acquire();
            libc::_exit(0);
        }
        pid
    }

    #[test]
    fn the_word_admits_one_process_at_a_time() {
        const ROUNDS: u32 = 20_000;
        unsafe {
            let sh = shared();
            let mut kids = Vec::new();
            for _ in 0..3 {
                let pid = libc::fork();
                assert!(pid >= 0);
                if pid == 0 {
                    for _ in 0..ROUNDS {
                        if !matches!(sh.word.acquire(), Ok(false)) {
                            libc::_exit(2);
                        }
                        if sh.inside.fetch_add(1, Ordering::SeqCst) != 0 {
                            sh.violations.fetch_add(1, Ordering::SeqCst);
                        }
                        std::hint::spin_loop();
                        sh.inside.fetch_sub(1, Ordering::SeqCst);
                        sh.word.release();
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

    #[test]
    fn a_dead_owner_hands_the_word_on() {
        unsafe {
            let sh = shared();
            assert_eq!(exit_status(dead_holder(sh)), 0);
            assert!(sh.word.acquire().unwrap(), "the owner's death was not reported");
            sh.word.release();
            assert!(!sh.word.acquire().unwrap(), "a recovered word reported a death twice");
            sh.word.release();
        }
    }

    #[test]
    fn an_unreaped_dead_owner_hands_the_word_on() {
        unsafe {
            let sh = shared();
            let holder = dead_holder(sh);
            let mut info: libc::siginfo_t = std::mem::zeroed();
            libc::waitid(libc::P_PID, holder as libc::id_t, &mut info, libc::WEXITED | libc::WNOWAIT);
            let addr = sh as *const Shared as usize;
            let (tx, rx) = std::sync::mpsc::channel();
            std::thread::spawn(move || {
                let sh = &*(addr as *const Shared);
                let died = sh.word.acquire();
                if died.is_ok() {
                    sh.word.release();
                }
                let _ = tx.send(died.ok());
            });
            let got = rx.recv_timeout(std::time::Duration::from_secs(10));
            assert_eq!(exit_status(holder), 0);
            let died = got.expect("the word waited on an unreaped dead owner");
            assert_eq!(died, Some(true), "the owner's death was not reported");
        }
    }

    #[test]
    fn a_live_owner_is_waited_for() {
        unsafe {
            let sh = shared();
            let mut ready = [0 as libc::c_int; 2];
            assert_eq!(crate::fd::pipe(ready.as_mut_ptr()), 0);
            let holder = libc::fork();
            assert!(holder >= 0);
            if holder == 0 {
                let _ = sh.word.acquire();
                sh.inside.store(1, Ordering::SeqCst);
                libc::write(ready[1], b"x".as_ptr() as *const libc::c_void, 1);
                std::thread::sleep(std::time::Duration::from_millis(500));
                sh.inside.store(0, Ordering::SeqCst);
                sh.word.release();
                libc::_exit(0);
            }
            let mut b = 0u8;
            libc::read(ready[0], &mut b as *mut u8 as *mut libc::c_void, 1);
            let died = sh.word.acquire().unwrap();
            let inside = sh.inside.load(Ordering::SeqCst);
            sh.word.release();
            assert_eq!(exit_status(holder), 0);
            assert!(!died && inside == 0, "a live owner was robbed");
        }
    }

    #[test]
    fn a_second_take_by_the_holder_is_refused() {
        let w = OwnerWord::new();
        w.acquire().unwrap();
        assert!(w.acquire().is_err());
        unsafe { w.release() };
        w.acquire().unwrap();
        unsafe { w.release() };
    }
}
