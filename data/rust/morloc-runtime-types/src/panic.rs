use std::cell::Cell;

// PANIC-1
pub const PANIC_EXIT_STATUS: i32 = 70;

// PANIC-2: a request frame's or a format library call's catch.
pub const SCOPE_RUNTIME: u8 = 1;
// PANIC-6: a host's catch around its user code.
pub const SCOPE_HOST: u8 = 2;

thread_local! {
    static IN_SCOPE: Cell<u8> = const { Cell::new(0) };
    static UNWINDING: Cell<bool> = const { Cell::new(false) };
    static FATAL: Cell<bool> = const { Cell::new(false) };
}

pub struct Caught {
    pub message: String,
}

// PANIC-2: a panic no catch scope may hold.
pub fn fatal(message: &str) -> ! {
    let _ = FATAL.try_with(|f| f.set(true));
    panic!("{message}")
}

// PANIC-4
pub fn poisoned_lock() -> ! {
    fatal("a lock is poisoned: a thread panicked while holding it")
}

// PANIC-2: the only place a panic is caught.
pub fn catch<R>(body: impl FnOnce() -> R) -> Result<R, Caught> {
    let outer = set_scope(SCOPE_RUNTIME);
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(body));
    match result {
        Ok(r) => {
            set_scope(outer);
            Ok(r)
        }
        Err(payload) => {
            set_scope(outer);
            FATAL.with(|f| f.set(false));
            let message = payload
                .downcast_ref::<&str>()
                .map(|s| s.to_string())
                .or_else(|| payload.downcast_ref::<String>().cloned())
                .unwrap_or_else(|| "unknown panic".into());
            Err(Caught { message })
        }
    }
}

const UNWINDING_BIT: u8 = 0x80;

// PANIC-6: opens scope `kind` with no panic unwinding in it; returns what
// to pass back to restore the outer scope and its unwinding state.
pub fn set_scope(kind: u8) -> u8 {
    let unwinding = UNWINDING.with(|u| u.replace(kind & UNWINDING_BIT != 0));
    let outer = IN_SCOPE.with(|s| s.replace(kind & !UNWINDING_BIT));
    outer | if unwinding { UNWINDING_BIT } else { 0 }
}

// PANIC-6: a host's own catch caught the panic.
pub fn caught() {
    UNWINDING.with(|u| u.set(false));
    FATAL.with(|f| f.set(false));
}

// PANIC-2: a panic below an `extern "C"` function cannot unwind to a catch.
pub fn outside_scope<R>(body: impl FnOnce() -> R) -> R {
    struct Restore(u8);
    impl Drop for Restore {
        fn drop(&mut self) {
            let _ = IN_SCOPE.try_with(|s| s.set(self.0));
        }
    }
    let _restore = Restore(IN_SCOPE.with(|s| s.replace(0)));
    body()
}

// PANIC-1
pub fn install_hook(panic_exit: fn() -> !) {
    install_hook_unless(panic_exit, |_, _| true)
}

// PANIC-9: the directory of this crate's sources as panic locations name
// it, and as backtraces name it.
pub fn source_dirs() -> [&'static str; 2] {
    let f = file!();
    [&f[..f.len() - "panic.rs".len()], concat!(env!("CARGO_MANIFEST_DIR"), "/src/")]
}

// PANIC-1: `may_unwind(host, file)` says whether this thread's state lets a
// catch hold the panic; `host` when the innermost scope is a host's user
// code, `file` the panic's location.
pub fn install_hook_unless(panic_exit: fn() -> !, may_unwind: fn(bool, &str) -> bool) {
    std::panic::set_hook(Box::new(move |info| {
        let file = info.location().map(|l| l.file()).unwrap_or("");
        match decide(file, false, may_unwind) {
            Outcome::Unwind => report(info),
            Outcome::UnwindQuietly => {}
            Outcome::Exit => {
                report(info);
                panic_exit()
            }
        }
    }));
}

/// What the hook does with a panic located at `file` on this thread.
pub enum Outcome {
    Unwind,
    /// PANIC-6: a user panic is reported by the call's failure.
    UnwindQuietly,
    Exit,
}

/// PANIC-1: the hook's decision for a panic at `file`; `fatal` adds a fatal
/// mark the panicking code set in another copy of this crate.
pub fn decide(file: &str, fatal: bool, may_unwind: fn(bool, &str) -> bool) -> Outcome {
    let scope = IN_SCOPE.try_with(|s| s.get()).unwrap_or(0);
    let fatal = fatal || FATAL.try_with(|f| f.get()).unwrap_or(true);
    let unwinding = UNWINDING.try_with(|u| u.replace(true)).unwrap_or(true);
    // PANIC-6: user code may catch its own panics, so a host scope
    // cannot tell a second panic from a nested one; Rust aborts a
    // nested one itself.
    let nested = unwinding && scope != SCOPE_HOST;
    if scope != 0 && !nested && !fatal && may_unwind(scope == SCOPE_HOST, file) {
        if scope == SCOPE_HOST { Outcome::UnwindQuietly } else { Outcome::Unwind }
    } else {
        Outcome::Exit
    }
}

/// Whether this copy of the crate marked the panic in flight fatal.
pub fn fatal_marked() -> bool {
    FATAL.try_with(|f| f.get()).unwrap_or(true)
}

/// A panic's report line, formatted into a fixed buffer.
pub struct Report {
    buf: [u8; 2048],
    len: usize,
}

impl Report {
    pub fn of(info: &std::panic::PanicHookInfo<'_>) -> Report {
        use std::fmt::Write;
        let mut text = Report { buf: [0; 2048], len: 0 };
        let thread = std::thread::current();
        let _ = writeln!(text, "morloc: internal error: thread '{}' {}", thread.name().unwrap_or("<unnamed>"), info);
        text
    }

    pub fn as_bytes(&self) -> &[u8] {
        &self.buf[..self.len]
    }
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
    report_text(Report::of(info).as_bytes());
}

/// Write a panic's report line, and a backtrace when one is asked for.
pub fn report_text(line: &[u8]) {
    write_stderr(line);
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_scope_restores_the_outer_unwinding_state() {
        let outer = set_scope(SCOPE_RUNTIME | UNWINDING_BIT);
        let inner = set_scope(SCOPE_HOST);
        assert_eq!(inner, SCOPE_RUNTIME | UNWINDING_BIT);
        assert!(!UNWINDING.with(|u| u.get()));
        let back = set_scope(inner);
        assert_eq!(back, SCOPE_HOST);
        assert!(UNWINDING.with(|u| u.get()));
        set_scope(outer);
        assert!(!UNWINDING.with(|u| u.get()));
    }
}
