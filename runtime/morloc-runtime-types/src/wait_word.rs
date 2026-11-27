//! Waiting for a 32-bit word in shared memory to change, across processes.

use std::sync::atomic::{AtomicU32, Ordering};
use std::time::Duration;

/// Wait until `word` may no longer hold `seen`, or `timeout` passes. May
/// return early; callers re-check.
pub fn wait(word: &AtomicU32, seen: u32, timeout: Duration) {
    if word.load(Ordering::Acquire) != seen {
        return;
    }
    imp::wait(word, seen, timeout);
}

/// Wake every process and thread waiting on `word`.
pub fn wake_all(word: &AtomicU32) {
    imp::wake_all(word);
}

#[cfg(target_os = "linux")]
mod imp {
    use super::*;

    pub fn wait(word: &AtomicU32, seen: u32, timeout: Duration) {
        let ts = libc::timespec {
            tv_sec: timeout.as_secs().min(i64::MAX as u64) as libc::time_t,
            tv_nsec: timeout.subsec_nanos() as libc::c_long,
        };
        // SAFETY: SLOT-12: the word is a valid, aligned u32 for the call.
        unsafe {
            libc::syscall(
                libc::SYS_futex,
                word.as_ptr(),
                libc::FUTEX_WAIT,
                seen,
                &ts as *const libc::timespec,
                std::ptr::null::<u32>(),
                0,
            );
        }
    }

    pub fn wake_all(word: &AtomicU32) {
        // SAFETY: SLOT-12: as above.
        unsafe {
            libc::syscall(
                libc::SYS_futex,
                word.as_ptr(),
                libc::FUTEX_WAKE,
                i32::MAX,
                std::ptr::null::<libc::timespec>(),
                std::ptr::null::<u32>(),
                0,
            );
        }
    }
}

#[cfg(target_vendor = "apple")]
mod imp {
    use super::*;

    extern "C" {
        fn __ulock_wait(operation: u32, addr: *mut libc::c_void, value: u64, timeout_us: u32) -> libc::c_int;
        fn __ulock_wake(operation: u32, addr: *mut libc::c_void, wake_value: u64) -> libc::c_int;
    }

    const UL_COMPARE_AND_WAIT_SHARED: u32 = 3;
    const ULF_WAKE_ALL: u32 = 0x0000_0100;
    const ULF_NO_ERRNO: u32 = 0x0100_0000;

    pub fn wait(word: &AtomicU32, seen: u32, timeout: Duration) {
        let us = timeout.as_micros().clamp(1, u32::MAX as u128) as u32;
        // SAFETY: SLOT-12: as on Linux.
        unsafe {
            __ulock_wait(UL_COMPARE_AND_WAIT_SHARED | ULF_NO_ERRNO, word.as_ptr() as *mut libc::c_void, seen as u64, us);
        }
    }

    pub fn wake_all(word: &AtomicU32) {
        // SAFETY: SLOT-12: as on Linux.
        unsafe {
            __ulock_wake(UL_COMPARE_AND_WAIT_SHARED | ULF_WAKE_ALL | ULF_NO_ERRNO, word.as_ptr() as *mut libc::c_void, 0);
        }
    }
}

#[cfg(not(any(target_os = "linux", target_vendor = "apple")))]
mod imp {
    use super::*;

    pub fn wait(_word: &AtomicU32, _seen: u32, timeout: Duration) {
        std::thread::sleep(timeout.min(Duration::from_millis(1)));
    }

    pub fn wake_all(_word: &AtomicU32) {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Instant;

    #[test]
    fn a_waiter_wakes_when_the_word_changes_on_another_thread() {
        static WORD: AtomicU32 = AtomicU32::new(0);
        let t = std::thread::spawn(|| {
            std::thread::sleep(Duration::from_millis(50));
            WORD.store(1, Ordering::Release);
            wake_all(&WORD);
        });
        let began = Instant::now();
        while WORD.load(Ordering::Acquire) == 0 {
            wait(&WORD, 0, Duration::from_secs(10));
        }
        t.join().unwrap();
        assert!(began.elapsed() < Duration::from_secs(5));
    }

    #[test]
    fn a_waiter_in_another_process_wakes_when_the_shared_word_changes() {
        unsafe {
            let p = libc::mmap(
                std::ptr::null_mut(),
                4096,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_SHARED | libc::MAP_ANONYMOUS,
                -1,
                0,
            );
            assert_ne!(p, libc::MAP_FAILED);
            let word = &*(p as *const AtomicU32);
            let child = libc::fork();
            assert!(child >= 0);
            if child == 0 {
                libc::alarm(10);
                while word.load(Ordering::Acquire) == 0 {
                    wait(word, 0, Duration::from_secs(10));
                }
                libc::_exit(0);
            }
            std::thread::sleep(Duration::from_millis(100));
            let began = Instant::now();
            word.store(1, Ordering::Release);
            wake_all(word);
            let mut status = 0;
            libc::waitpid(child, &mut status, 0);
            assert!(libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0, "status {status}");
            assert!(began.elapsed() < Duration::from_secs(5));
            libc::munmap(p, 4096);
        }
    }

    #[test]
    fn a_wait_on_a_word_that_already_changed_returns_at_once() {
        let word = AtomicU32::new(5);
        let began = Instant::now();
        wait(&word, 4, Duration::from_secs(10));
        assert!(began.elapsed() < Duration::from_secs(1));
    }
}
