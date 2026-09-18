//! Reporting a pool's death by signal.
//!
//! A pool that dies by a signal leaves its caller a closed socket and an
//! exit status that names the signal but not what the pool was doing.
//! Every pool installs this handler at start. On a fatal signal it writes
//! one line naming the pool, the signal and, when the pool supplies one,
//! the manifold the faulting thread was executing, then a native
//! backtrace; then it lets the process die of the signal so the parent
//! still sees it. Inside the handler only calls that are safe there are
//! used: `write`, `backtrace_symbols_fd` (warmed at install), `alarm` and
//! `raise`.

use std::ffi::{c_char, c_int, c_void, CStr};
use std::sync::atomic::{AtomicPtr, AtomicUsize, Ordering};

/// The innermost manifold the calling thread is executing, as a pointer
/// and byte length (not NUL-terminated), or null when the thread is not
/// running one. Supplied by the pool, which keeps the slot in its own
/// binary where a handler can read it without allocating.
pub type FrameFn = Option<unsafe extern "C" fn(*mut usize) -> *const c_char>;

static LANG: AtomicPtr<c_char> = AtomicPtr::new(std::ptr::null_mut());
static FRAME_FN: AtomicUsize = AtomicUsize::new(0);
/// The signal whose handler is running, for the alarm that ends a wedged
/// handler.
static PENDING: AtomicUsize = AtomicUsize::new(0);

const FATAL: [c_int; 5] = [libc::SIGSEGV, libc::SIGBUS, libc::SIGILL, libc::SIGFPE, libc::SIGABRT];

/// Seconds a handler may spend on its backtrace before the process is
/// killed by the signal it was reporting.
const HANDLER_ALARM_SECS: u32 = 5;

/// Install the fatal-signal handler for a pool of language `lang`.
/// `current_frame` may be null.
///
/// # Safety
/// `lang` is a NUL-terminated string; `current_frame`, when given, is safe
/// to call from a signal handler on any thread.
#[no_mangle]
pub unsafe extern "C" fn morloc_install_crash_handler(lang: *const c_char, current_frame: FrameFn) {
    let name = if lang.is_null() { "" } else { CStr::from_ptr(lang).to_str().unwrap_or("") };
    let owned = std::ffi::CString::new(name).unwrap_or_default();
    LANG.store(owned.into_raw(), Ordering::Release);
    FRAME_FN.store(current_frame.map_or(0, |f| f as *const () as usize), Ordering::Release);

    // The first call of backtrace() loads the unwinder and may allocate;
    // done now, while the process is healthy, so the handler's call does
    // not take the malloc lock a corrupted heap may hold.
    let mut warm = [std::ptr::null_mut::<c_void>(); 4];
    libc::backtrace(warm.as_mut_ptr(), warm.len() as c_int);

    // SA_ONSTACK: runs on the thread's alternate stack, so a stack
    // overflow is still reported. SA_RESETHAND: the disposition is back
    // to the default before the handler runs, so a fault inside the
    // handler ends the process with the real signal rather than
    // re-entering. The signal stays masked meanwhile.
    let mut sa: libc::sigaction = std::mem::zeroed();
    sa.sa_sigaction = fatal as *const () as usize;
    sa.sa_flags = libc::SA_ONSTACK | libc::SA_RESETHAND | libc::SA_SIGINFO;
    libc::sigemptyset(&mut sa.sa_mask);
    for sig in FATAL {
        libc::sigaction(sig, &sa, std::ptr::null_mut());
    }
    let mut alarm: libc::sigaction = std::mem::zeroed();
    alarm.sa_sigaction = wedged as *const () as usize;
    alarm.sa_flags = libc::SA_ONSTACK;
    libc::sigemptyset(&mut alarm.sa_mask);
    libc::sigaction(libc::SIGALRM, &alarm, std::ptr::null_mut());
}

/// A handler that has taken too long: end the process with the signal it
/// was reporting.
extern "C" fn wedged(_sig: c_int) {
    let sig = PENDING.load(Ordering::Relaxed) as c_int;
    unsafe {
        libc::signal(sig, libc::SIG_DFL);
        libc::raise(sig);
    }
}

fn signal_name(sig: c_int) -> &'static str {
    match sig {
        libc::SIGSEGV => "SIGSEGV",
        libc::SIGBUS => "SIGBUS",
        libc::SIGILL => "SIGILL",
        libc::SIGFPE => "SIGFPE",
        libc::SIGABRT => "SIGABRT",
        _ => "",
    }
}

/// A fixed buffer written with `write(2)`: no allocation, no locale.
struct Line {
    buf: [u8; 1024],
    len: usize,
}

impl Line {
    fn new() -> Line {
        Line { buf: [0; 1024], len: 0 }
    }
    fn bytes(&mut self, s: &[u8]) {
        let n = s.len().min(self.buf.len() - self.len);
        self.buf[self.len..self.len + n].copy_from_slice(&s[..n]);
        self.len += n;
    }
    fn str(&mut self, s: &str) {
        self.bytes(s.as_bytes());
    }
    fn num(&mut self, mut v: u64) {
        let mut digits = [0u8; 20];
        let mut i = digits.len();
        loop {
            i -= 1;
            digits[i] = b'0' + (v % 10) as u8;
            v /= 10;
            if v == 0 {
                break;
            }
        }
        self.bytes(&digits[i..]);
    }
    fn flush(&mut self) {
        unsafe { libc::write(2, self.buf.as_ptr() as *const c_void, self.len) };
        self.len = 0;
    }
}

extern "C" fn fatal(sig: c_int, info: *mut libc::siginfo_t, _ctx: *mut c_void) {
    PENDING.store(sig as usize, Ordering::Relaxed);
    unsafe { libc::alarm(HANDLER_ALARM_SECS) };

    // The signal line first: everything after it may fault.
    let mut line = Line::new();
    line.str("\nmorloc ");
    let lang = LANG.load(Ordering::Acquire);
    if !lang.is_null() {
        line.bytes(unsafe { CStr::from_ptr(lang) }.to_bytes());
        line.str(" ");
    }
    line.str("pool (pid ");
    line.num(unsafe { libc::getpid() } as u64);
    line.str("): fatal signal ");
    line.num(sig as u64);
    let name = signal_name(sig);
    if !name.is_empty() {
        line.str(" (");
        line.str(name);
        line.str(")");
    }
    line.flush();

    let frame_fn = FRAME_FN.load(Ordering::Acquire);
    if frame_fn != 0 {
        let f: unsafe extern "C" fn(*mut usize) -> *const c_char = unsafe { std::mem::transmute(frame_fn) };
        let mut len = 0usize;
        let p = unsafe { f(&mut len) };
        if !p.is_null() && len > 0 {
            line.str(" while executing ");
            line.bytes(unsafe { std::slice::from_raw_parts(p as *const u8, len) });
        } else {
            line.str(" on a thread running no manifold");
        }
        line.flush();
    }
    line.str("\n");
    if sig == libc::SIGBUS {
        line.str("  (SIGBUS often means the shared-memory filesystem is full; check /dev/shm)\n");
    }
    line.flush();

    let mut frames = [std::ptr::null_mut::<c_void>(); 64];
    let n = unsafe { libc::backtrace(frames.as_mut_ptr(), frames.len() as c_int) };
    unsafe { libc::backtrace_symbols_fd(frames.as_ptr(), n, 2) };

    // A fault the kernel raised is re-executed on return and, with the
    // default disposition restored, ends the process with its own context.
    // A signal sent by a process (raise, kill, abort) would not recur, so
    // it is raised again.
    let kernel_raised = !info.is_null() && unsafe { (*info).si_code } > 0;
    if !kernel_raised {
        unsafe { libc::raise(sig) };
    }
}
