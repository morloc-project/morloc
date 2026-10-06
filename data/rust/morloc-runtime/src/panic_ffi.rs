use std::sync::atomic::{AtomicUsize, Ordering};

static EXIT: AtomicUsize = AtomicUsize::new(0);

// PANIC-1
fn panic_exit() -> ! {
    let f = EXIT.load(Ordering::Acquire);
    if f != 0 {
        // SAFETY: PANIC-1: only `install` stores here, and it stores a host's exit function.
        let exit: extern "C" fn() -> ! = unsafe { std::mem::transmute::<usize, extern "C" fn() -> !>(f) };
        exit()
    }
    unsafe { libc::_exit(morloc_runtime_types::panic::PANIC_EXIT_STATUS) }
}

// PANIC-1
pub(crate) fn install(exit: Option<extern "C" fn() -> !>) {
    EXIT.store(exit.map_or(0, |f| f as usize), Ordering::Release);
    morloc_runtime_types::panic::install_hook_unless(panic_exit, crate::fork_policy::holds_no_runtime_lock);
}

#[no_mangle]
pub extern "C" fn morloc_install_panic_hook(exit: Option<extern "C" fn() -> !>) {
    install(exit)
}

#[cfg(test)]
mod tests {
    use super::*;

    const CHILD_ENV: &str = "MORLOC_PANIC_TEST_CHILD";
    const CHILD_OK: i32 = 42;

    fn status_of_child(name: &str) -> i32 {
        let status = std::process::Command::new(std::env::current_exe().unwrap())
            .args([&format!("panic_ffi::tests::{name}"), "--exact", "--ignored", "--test-threads=1"])
            .env(CHILD_ENV, "1")
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .status()
            .unwrap();
        status.code().unwrap_or(-1)
    }

    fn in_child(work: impl FnOnce()) {
        if std::env::var_os(CHILD_ENV).is_none() {
            return;
        }
        install(None);
        work();
        unsafe { libc::_exit(CHILD_OK) };
    }

    #[test]
    fn a_libmorloc_panic_on_a_thread_outside_any_catch_scope_exits_with_the_internal_error_status() {
        assert_eq!(status_of_child("child_panics_on_a_thread"), morloc_runtime_types::panic::PANIC_EXIT_STATUS);
    }

    #[test]
    #[ignore]
    fn child_panics_on_a_thread() {
        in_child(|| {
            let _ = std::thread::spawn(|| panic!("background")).join();
            unsafe { libc::_exit(3) };
        });
    }

    #[test]
    fn a_libmorloc_panic_inside_a_catch_scope_is_caught() {
        assert_eq!(status_of_child("child_panics_inside_a_scope"), CHILD_OK);
    }

    #[test]
    #[ignore]
    fn child_panics_inside_a_scope() {
        in_child(|| {
            if morloc_runtime_types::panic::catch(|| panic!("inside")).is_ok() {
                unsafe { libc::_exit(3) };
            }
        });
    }

    #[test]
    fn a_poisoned_libmorloc_lock_ends_the_process() {
        assert_eq!(status_of_child("child_finds_a_poisoned_lock"), morloc_runtime_types::panic::PANIC_EXIT_STATUS);
    }

    #[test]
    #[ignore]
    fn child_finds_a_poisoned_lock() {
        in_child(|| {
            let lock = std::sync::Mutex::new(0u32);
            let _ = morloc_runtime_types::panic::catch(|| {
                let _g = lock.lock().unwrap();
                panic!("holding");
            });
            if !lock.is_poisoned() {
                unsafe { libc::_exit(4) };
            }
            let _g = lock.lock().unwrap_or_else(|_| morloc_runtime_types::panic::poisoned_lock());
            unsafe { libc::_exit(3) };
        });
    }

    #[test]
    fn a_format_library_panic_is_a_decode_error_and_the_process_goes_on() {
        assert_eq!(status_of_child("child_decodes_malformed_bytes"), CHILD_OK);
    }

    #[test]
    #[ignore]
    fn child_decodes_malformed_bytes() {
        in_child(|| {
            let mut err: *mut std::ffi::c_char = std::ptr::null_mut();
            let r = unsafe { crate::error::guarded(&mut err, 1, || -> i32 { panic!("malformed") }) };
            let msg = unsafe { std::ffi::CStr::from_ptr(err) }.to_string_lossy().into_owned();
            if r != 1 || !msg.contains("malformed") {
                unsafe { libc::_exit(3) };
            }
        });
    }

    #[test]
    fn a_poisoned_lock_inside_a_format_library_call_still_ends_the_process() {
        assert_eq!(status_of_child("child_finds_a_poisoned_lock_while_decoding"), morloc_runtime_types::panic::PANIC_EXIT_STATUS);
    }

    #[test]
    #[ignore]
    fn child_finds_a_poisoned_lock_while_decoding() {
        in_child(|| {
            let mut err: *mut std::ffi::c_char = std::ptr::null_mut();
            let _ = unsafe {
                crate::error::guarded(&mut err, 1, || -> i32 { morloc_runtime_types::panic::poisoned_lock() })
            };
            unsafe { libc::_exit(3) };
        });
    }

    #[test]
    fn a_panic_holding_a_runtime_lock_inside_a_format_library_call_ends_the_process() {
        assert_eq!(status_of_child("child_panics_holding_a_lock_while_decoding"), morloc_runtime_types::panic::PANIC_EXIT_STATUS);
    }

    #[test]
    #[ignore]
    fn child_panics_holding_a_lock_while_decoding() {
        in_child(|| {
            let lock = crate::fork_policy::Held::new(60, 0u32);
            let mut err: *mut std::ffi::c_char = std::ptr::null_mut();
            let _ = unsafe {
                crate::error::guarded(&mut err, 1, || -> i32 {
                    let _g = lock.lock();
                    panic!("inside the allocator")
                })
            };
            unsafe { libc::_exit(3) };
        });
    }
}
