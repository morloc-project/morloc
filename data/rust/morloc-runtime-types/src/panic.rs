use std::cell::Cell;

// PANIC-1
pub const PANIC_EXIT_STATUS: i32 = 70;

thread_local! {
    static IN_SCOPE: Cell<bool> = const { Cell::new(false) };
    static UNWINDING: Cell<bool> = const { Cell::new(false) };
}

pub struct Caught;

// PANIC-2: the only place a panic is caught.
pub fn catch<R>(body: impl FnOnce() -> R) -> Result<R, Caught> {
    let outer = IN_SCOPE.with(|s| s.replace(true));
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(body));
    IN_SCOPE.with(|s| s.set(outer));
    match result {
        Ok(r) => Ok(r),
        Err(_) => {
            UNWINDING.with(|u| u.set(false));
            Err(Caught)
        }
    }
}

// PANIC-2: a panic below an `extern "C"` function cannot unwind to a catch.
pub fn outside_scope<R>(body: impl FnOnce() -> R) -> R {
    struct Restore(bool);
    impl Drop for Restore {
        fn drop(&mut self) {
            let _ = IN_SCOPE.try_with(|s| s.set(self.0));
        }
    }
    let _restore = Restore(IN_SCOPE.with(|s| s.replace(false)));
    body()
}

// PANIC-1
pub fn install_hook(panic_exit: fn() -> !) {
    std::panic::set_hook(Box::new(move |info| {
        report(info);
        let in_scope = IN_SCOPE.try_with(|s| s.get()).unwrap_or(false);
        let unwinding = UNWINDING.try_with(|u| u.replace(true)).unwrap_or(true);
        if in_scope && !unwinding {
            return;
        }
        panic_exit()
    }));
}

struct Report {
    buf: [u8; 2048],
    len: usize,
}

impl std::fmt::Write for Report {
    fn write_str(&mut self, text: &str) -> std::fmt::Result {
        let n = text.len().min(self.buf.len() - self.len);
        self.buf[self.len..self.len + n].copy_from_slice(&text.as_bytes()[..n]);
        self.len += n;
        Ok(())
    }
}

fn report(info: &std::panic::PanicHookInfo<'_>) {
    use std::fmt::Write;
    let mut text = Report { buf: [0; 2048], len: 0 };
    let thread = std::thread::current();
    let _ = writeln!(text, "morloc: internal error: thread '{}' {}", thread.name().unwrap_or("<unnamed>"), info);
    write_stderr(&text.buf[..text.len]);
    let trace = std::backtrace::Backtrace::capture();
    if trace.status() == std::backtrace::BacktraceStatus::Captured {
        write_stderr(format!("{trace}\n").as_bytes());
    }
}

fn write_stderr(mut rest: &[u8]) {
    while !rest.is_empty() {
        let n = unsafe { libc::write(2, rest.as_ptr() as *const libc::c_void, rest.len()) };
        if n > 0 {
            rest = &rest[n as usize..];
        } else if n < 0 && std::io::Error::last_os_error().kind() == std::io::ErrorKind::Interrupted {
            continue;
        } else {
            return;
        }
    }
}
