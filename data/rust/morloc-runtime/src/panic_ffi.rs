use std::sync::atomic::{AtomicUsize, Ordering};

static EXIT: AtomicUsize = AtomicUsize::new(0);
static CLASSIFIER: AtomicUsize = AtomicUsize::new(0);

// PANIC-9
fn may_unwind(host: bool, file: &str) -> bool {
    if !crate::fork_policy::holds_no_runtime_lock() {
        return false;
    }
    let c = CLASSIFIER.load(Ordering::Acquire);
    if !host || c == 0 {
        return true;
    }
    // SAFETY: PANIC-9: only `morloc_set_panic_classifier` stores here.
    let is_runtime: extern "C" fn(*const u8, usize) -> bool =
        unsafe { std::mem::transmute::<usize, extern "C" fn(*const u8, usize) -> bool>(c) };
    !is_runtime(file.as_ptr(), file.len())
}

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
    morloc_runtime_types::panic::install_hook_unless(panic_exit, may_unwind);
}

pub(crate) fn morloc_install_panic_hook(exit: Option<extern "C" fn() -> !>) {
    install(exit)
}

// PANIC-1: the hook of another copy of the standard library -- the Rust
// pool's own, where libmorloc does not share it -- asks for this one's
// decision. Returns whether the panic may unwind; otherwise reports `line`
// and ends the process.
pub(crate) unsafe fn morloc_panic_decide(file: *const u8, file_len: usize, line: *const u8, line_len: usize, fatal: bool) -> bool {
    use morloc_runtime_types::panic::{decide, report_text, Outcome};
    let file = std::str::from_utf8(std::slice::from_raw_parts(file, file_len)).unwrap_or("");
    let line = std::slice::from_raw_parts(line, line_len);
    match decide(file, fatal, may_unwind) {
        Outcome::Unwind => {
            report_text(line);
            true
        }
        Outcome::UnwindQuietly => true,
        Outcome::Exit => {
            report_text(line);
            panic_exit()
        }
    }
}

// PANIC-9: the host says whether a panic at a location is its runtime's.
pub(crate) fn morloc_set_panic_classifier(classify: Option<extern "C" fn(*const u8, usize) -> bool>) {
    CLASSIFIER.store(classify.map_or(0, |f| f as usize), Ordering::Release);
}

// PANIC-6: `kind` 2 opens a host's scope around its user code, 0 closes it;
// returns the scope to restore.
pub(crate) fn morloc_catch_scope(kind: u8) -> u8 {
    morloc_runtime_types::panic::set_scope(kind)
}

// PANIC-6
pub(crate) fn morloc_panic_caught() {
    morloc_runtime_types::panic::caught()
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
    fn a_callee_failure_without_a_reason_ends_the_process_even_inside_a_catch_scope() {
        assert_eq!(status_of_child("child_takes_a_missing_reason"), morloc_runtime_types::panic::PANIC_EXIT_STATUS);
    }

    #[test]
    #[ignore]
    fn child_takes_a_missing_reason() {
        in_child(|| {
            let _ = morloc_runtime_types::panic::catch(|| unsafe { crate::error::take_reason(std::ptr::null_mut()) });
            unsafe { libc::_exit(3) };
        });
    }

    #[test]
    fn a_fold_call_with_a_null_value_ends_the_process() {
        assert_eq!(status_of_child("child_puts_a_null_fold_value"), morloc_runtime_types::panic::PANIC_EXIT_STATUS);
    }

    #[test]
    #[ignore]
    fn child_puts_a_null_fold_value() {
        in_child(|| {
            let schema = morloc_runtime_types::schema::parse_schema("i4").unwrap();
            let _ = morloc_runtime_types::panic::catch(|| unsafe { crate::cell::cell_put(0, &schema, std::ptr::null()) });
            unsafe { libc::_exit(3) };
        });
    }

    #[test]
    fn a_panic_below_a_c_abi_function_called_from_rust_reaches_the_catch() {
        assert_eq!(status_of_child("child_panics_below_a_c_abi_function"), CHILD_OK);
    }

    #[test]
    #[ignore]
    fn child_panics_below_a_c_abi_function() {
        in_child(|| {
            let mut err: *mut std::ffi::c_char = std::ptr::null_mut();
            let cs = unsafe { crate::ffi::parse_schema(c"i4".as_ptr(), &mut err) };
            assert!(!cs.is_null());
            unsafe { (*cs).serial_type = 9999 };
            let caught = morloc_runtime_types::panic::catch(|| unsafe { crate::ffi::schema_to_string(cs) });
            if caught.is_err() {
                unsafe { libc::_exit(CHILD_OK) };
            }
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
            let r = crate::error::decode("decoding", || -> i32 { panic!("malformed") });
            if !matches!(r, Err(e) if e.to_string().contains("malformed")) {
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
            let _ = crate::error::decode("decoding", || -> i32 { morloc_runtime_types::panic::poisoned_lock() });
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
            let _ = crate::error::decode("decoding", || -> i32 {
                let _g = lock.lock();
                panic!("inside the allocator")
            });
            unsafe { libc::_exit(3) };
        });
    }

    #[test]
    fn a_panicking_release_callback_ends_the_process_with_70() {
        assert_eq!(status_of_child("child_releases_a_stream_that_panics"), morloc_runtime_types::panic::PANIC_EXIT_STATUS);
    }

    #[test]
    #[ignore]
    fn child_releases_a_stream_that_panics() {
        use arrow_array::ffi::{FFI_ArrowArray, FFI_ArrowSchema};
        use arrow_array::ffi_stream::FFI_ArrowArrayStream;
        unsafe extern "C" fn get_schema(_: *mut FFI_ArrowArrayStream, out: *mut FFI_ArrowSchema) -> libc::c_int {
            let s = arrow_schema::Schema::new(vec![arrow_schema::Field::new("x", arrow_schema::DataType::Int64, true)]);
            std::ptr::write(out, FFI_ArrowSchema::try_from(&s).unwrap());
            0
        }
        unsafe extern "C" fn get_next(_: *mut FFI_ArrowArrayStream, _: *mut FFI_ArrowArray) -> libc::c_int {
            libc::EIO
        }
        unsafe extern "C" fn get_last_error(_: *mut FFI_ArrowArrayStream) -> *const std::ffi::c_char {
            c"the producer failed".as_ptr()
        }
        unsafe extern "C" fn release(s: *mut FFI_ArrowArrayStream) {
            (*s).release = None;
            panic!("the producer's release");
        }
        in_child(|| {
            let mut stream = FFI_ArrowArrayStream {
                get_schema: Some(get_schema),
                get_next: Some(get_next),
                get_last_error: Some(get_last_error),
                release: Some(release),
                private_data: std::ptr::null_mut(),
            };
            let mut err: *mut std::ffi::c_char = std::ptr::null_mut();
            let _ = morloc_runtime_types::panic::catch(|| unsafe {
                crate::arrow_ffi::arrow_stream_to_shm_typed(&mut stream, std::ptr::null(), &mut err)
            });
            unsafe { libc::_exit(3) };
        });
    }

    extern "C" fn panicking_handler(_: libc::c_int) {
        morloc_runtime_types::panic::signal_frame(|| panic!("in a signal handler"))
    }

    #[test]
    fn a_panic_in_a_signal_handler_ends_the_process_whatever_scope_it_interrupts() {
        assert_eq!(status_of_child("child_panics_in_a_handler_inside_a_catch"), morloc_runtime_types::panic::PANIC_EXIT_STATUS);
        assert_eq!(status_of_child("child_panics_in_a_handler_inside_a_host_scope"), morloc_runtime_types::panic::PANIC_EXIT_STATUS);
    }

    #[test]
    #[ignore]
    fn child_panics_in_a_handler_inside_a_catch() {
        in_child(|| {
            unsafe { libc::signal(libc::SIGUSR1, panicking_handler as *const () as libc::sighandler_t) };
            let _ = morloc_runtime_types::panic::catch(|| unsafe { libc::raise(libc::SIGUSR1) });
            unsafe { libc::_exit(3) };
        });
    }

    #[test]
    #[ignore]
    fn child_panics_in_a_handler_inside_a_host_scope() {
        in_child(|| {
            unsafe { libc::signal(libc::SIGUSR1, panicking_handler as *const () as libc::sighandler_t) };
            let outer = morloc_runtime_types::panic::set_scope(morloc_runtime_types::panic::SCOPE_HOST);
            unsafe { libc::raise(libc::SIGUSR1) };
            morloc_runtime_types::panic::set_scope(outer);
            unsafe { libc::_exit(3) };
        });
    }

    #[test]
    fn a_second_user_panic_after_a_user_catch_is_still_the_users() {
        assert_eq!(status_of_child("child_panics_twice_in_a_host_scope"), CHILD_OK);
    }

    #[test]
    #[ignore]
    fn child_panics_twice_in_a_host_scope() {
        in_child(|| {
            let outer = morloc_runtime_types::panic::set_scope(morloc_runtime_types::panic::SCOPE_HOST);
            let first = std::panic::catch_unwind(|| panic!("one")).is_err();
            let second = std::panic::catch_unwind(|| panic!("two")).is_err();
            morloc_runtime_types::panic::set_scope(outer);
            if !(first && second) {
                unsafe { libc::_exit(3) };
            }
        });
    }
}

mod c_abi {

    #[no_mangle]
    pub unsafe extern "C" fn morloc_panic_decide(file: *const u8, file_len: usize, line: *const u8, line_len: usize, fatal: bool) -> bool {
        super::morloc_panic_decide(file, file_len, line, line_len, fatal)
    }

    #[no_mangle]
    pub extern "C" fn morloc_install_panic_hook(exit: Option<extern "C" fn() -> !>) {
        super::morloc_install_panic_hook(exit)
    }

    #[no_mangle]
    pub extern "C" fn morloc_set_panic_classifier(classify: Option<extern "C" fn(*const u8, usize) -> bool>) {
        super::morloc_set_panic_classifier(classify)
    }

    #[no_mangle]
    pub extern "C" fn morloc_catch_scope(kind: u8) -> u8 {
        super::morloc_catch_scope(kind)
    }

    #[no_mangle]
    pub extern "C" fn morloc_panic_caught() {
        super::morloc_panic_caught()
    }
}
