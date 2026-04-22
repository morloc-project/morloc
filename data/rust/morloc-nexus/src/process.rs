//! Pool daemon process management, signal handling, and lifecycle.
//!
//! Replaces the fork/exec, SIGCHLD, SIGTERM, clean_exit logic from nexus.c.

use std::ffi::CString;
use std::path::Path;
use std::sync::atomic::{AtomicBool, AtomicI32, Ordering};
use std::time::Duration;

use crate::manifest::Pool;

pub const MAX_DAEMONS: usize = 32;

const INITIAL_PING_TIMEOUT: Duration = Duration::from_millis(10);
const INITIAL_RETRY_DELAY: Duration = Duration::from_millis(1);
const RETRY_MULTIPLIER: f64 = 1.25;
const MAX_RETRIES: usize = 16;

// ── Global state for signal handlers ───────────────────────────────────────

/// PIDs of spawned pool daemons. 0 = unused, -1 = reaped.
static PIDS: [AtomicI32; MAX_DAEMONS] = {
    const INIT: AtomicI32 = AtomicI32::new(0);
    [INIT; MAX_DAEMONS]
};

/// Process group IDs for cleanup.
static PGIDS: [AtomicI32; MAX_DAEMONS] = {
    const INIT: AtomicI32 = AtomicI32::new(0);
    [INIT; MAX_DAEMONS]
};

/// Exit statuses saved by SIGCHLD handler.
static EXIT_STATUSES: [AtomicI32; MAX_DAEMONS] = {
    const INIT: AtomicI32 = AtomicI32::new(0);
    [INIT; MAX_DAEMONS]
};

/// Re-entrancy guard for clean_exit.
static CLEANING_UP: AtomicBool = AtomicBool::new(false);

/// Global tmpdir path (set once in main, read during cleanup).
static TMPDIR: std::sync::OnceLock<String> = std::sync::OnceLock::new();

/// Socket info for each pool.
///
/// Pool stderr and stdout are intentionally NOT captured or intercepted by
/// the nexus: a core morloc guarantee is that anything a sourced function
/// prints to stderr/stdout is passed through unchanged. Raised exceptions
/// are caught inside each pool's dispatch wrapper (see pool.py/pool.cpp/
/// pool.R/pool.jl) and returned as morloc error packets, which the nexus
/// then annotates with call-site context when bubbling them up.
pub struct PoolSocket {
    pub lang: String,
    pub socket_path: String,
    pub syscmd: Vec<CString>,
    pub pid: i32,
}

// ── Signal handlers (async-signal-safe) ────────────────────────────────────

/// SIGCHLD handler: reap terminated children.
extern "C" fn sigchld_handler(_sig: libc::c_int) {
    #[cfg(target_os = "linux")]
    let saved_errno = unsafe { *libc::__errno_location() };
    #[cfg(target_os = "macos")]
    let saved_errno = unsafe { *libc::__error() };
    loop {
        let mut status: libc::c_int = 0;
        let pid = unsafe { libc::waitpid(-1, &mut status, libc::WNOHANG) };
        if pid <= 0 {
            break;
        }
        for i in 0..MAX_DAEMONS {
            if PIDS[i].load(Ordering::Relaxed) == pid {
                EXIT_STATUSES[i].store(status, Ordering::Relaxed);
                PIDS[i].store(-1, Ordering::Relaxed);
                break;
            }
        }
    }
    #[cfg(target_os = "linux")]
    unsafe { *libc::__errno_location() = saved_errno };
    #[cfg(target_os = "macos")]
    unsafe { *libc::__error() = saved_errno };
}

/// SIGTERM/SIGINT handler: clean shutdown.
extern "C" fn signal_exit_handler(sig: libc::c_int) {
    if CLEANING_UP.load(Ordering::Relaxed) {
        unsafe { libc::_exit(128 + sig) };
    }
    clean_exit(128 + sig);
}

/// Install signal handlers.
pub fn install_signal_handlers() {
    unsafe {
        // SIGCHLD
        let mut sa: libc::sigaction = std::mem::zeroed();
        sa.sa_sigaction = sigchld_handler as *const () as usize;
        libc::sigemptyset(&mut sa.sa_mask);
        sa.sa_flags = libc::SA_RESTART | libc::SA_NOCLDSTOP;
        libc::sigaction(libc::SIGCHLD, &sa, std::ptr::null_mut());

        // SIGTERM and SIGINT
        let mut sa_exit: libc::sigaction = std::mem::zeroed();
        sa_exit.sa_sigaction = signal_exit_handler as *const () as usize;
        libc::sigemptyset(&mut sa_exit.sa_mask);
        sa_exit.sa_flags = 0;
        libc::sigaction(libc::SIGTERM, &sa_exit, std::ptr::null_mut());
        libc::sigaction(libc::SIGINT, &sa_exit, std::ptr::null_mut());
    }
}

/// Set the global tmpdir for cleanup.
pub fn set_tmpdir(path: String) {
    let _ = TMPDIR.set(path);
}

/// Get the tmpdir path.
pub fn get_tmpdir() -> Option<&'static str> {
    TMPDIR.get().map(|s| s.as_str())
}

// ── Clean exit ─────────────────────────────────────────────────────────────

/// Terminate all pool daemons and clean up resources.
///
/// Race condition with stderr output: when a pool process is dying (e.g.,
/// Python printing a traceback), its stderr writes may still be in a pipe
/// buffer or mid-syscall when we send SIGTERM. The pool's signal handler
/// (or SIG_DFL) may kill the process before its output reaches the
/// terminal. We mitigate this by:
/// 1. Flushing the nexus's own stderr first (so our error message is out)
/// 2. Giving pools 200ms after SIGTERM before escalating to SIGKILL
///    (up from the previous 50ms, which was too short for Python's
///    atexit handlers and multiprocessing cleanup to flush buffers)
pub fn clean_exit(exit_code: i32) -> ! {
    CLEANING_UP.store(true, Ordering::SeqCst);

    // Flush nexus stderr so our error messages are visible even if
    // the process is killed by a parent (e.g., shell pipeline).
    unsafe { libc::fsync(2) };

    // Block SIGCHLD during cleanup
    unsafe {
        let mut block_chld: libc::sigset_t = std::mem::zeroed();
        libc::sigemptyset(&mut block_chld);
        libc::sigaddset(&mut block_chld, libc::SIGCHLD);
        libc::sigprocmask(libc::SIG_BLOCK, &block_chld, std::ptr::null_mut());
    }

    // Send SIGTERM to all pool process groups
    for i in 0..MAX_DAEMONS {
        let pgid = PGIDS[i].load(Ordering::Relaxed);
        if pgid > 0 {
            unsafe { libc::kill(-pgid, libc::SIGTERM) };
        }
    }

    // Wait for groups to exit (up to 200ms per group, then SIGKILL).
    // The 200ms window serves two purposes:
    // - Lets pool signal handlers run (Python's signal_handler in pool.py
    //   calls close_daemon and cleans up shared memory)
    // - Lets any pending stderr writes (tracebacks, error messages) drain
    //   to the terminal before the process is force-killed
    for i in 0..MAX_DAEMONS {
        let pgid = PGIDS[i].load(Ordering::Relaxed);
        if pgid <= 0 {
            continue;
        }

        // Reap any available children
        while unsafe { libc::waitpid(-1, std::ptr::null_mut(), libc::WNOHANG) } > 0 {}
        if unsafe { libc::kill(-pgid, 0) } == -1 {
            continue;
        }

        let mut group_dead = false;
        for _ in 0..100 {
            while unsafe { libc::waitpid(-1, std::ptr::null_mut(), libc::WNOHANG) } > 0 {}
            if unsafe { libc::kill(-pgid, 0) } == -1 {
                group_dead = true;
                break;
            }
            std::thread::sleep(Duration::from_millis(2));
        }

        if !group_dead {
            unsafe { libc::kill(-pgid, libc::SIGKILL) };
            std::thread::sleep(Duration::from_millis(50));
        }
    }

    // Final reap
    while unsafe { libc::waitpid(-1, std::ptr::null_mut(), libc::WNOHANG) } > 0 {}

    // Clean up shared memory segments
    extern "C" {
        fn shclose(errmsg: *mut *mut std::ffi::c_char) -> bool;
    }
    unsafe {
        let mut err: *mut std::ffi::c_char = std::ptr::null_mut();
        shclose(&mut err);
        if !err.is_null() {
            libc::free(err as *mut libc::c_void);
        }
    }

    // Clean up tmpdir
    if let Some(dir) = get_tmpdir() {
        let _ = std::fs::remove_dir_all(dir);
    }

    std::process::exit(exit_code);
}

// ── Pool daemon spawning ───────────────────────────────────────────────────

/// Setup socket descriptors for all pools from the manifest.
pub fn setup_sockets(pools: &[Pool], tmpdir: &str, shm_basename: &str) -> Vec<PoolSocket> {
    pools
        .iter()
        .map(|pool| {
            let socket_path = format!("{}/{}", tmpdir, pool.socket);

            // Build syscmd: exec_args... socket_path tmpdir shm_basename
            let mut syscmd: Vec<CString> = pool
                .exec
                .iter()
                .map(|s| CString::new(s.as_str()).unwrap())
                .collect();
            syscmd.push(CString::new(socket_path.as_str()).unwrap());
            syscmd.push(CString::new(tmpdir).unwrap());
            syscmd.push(CString::new(shm_basename).unwrap());

            PoolSocket {
                lang: pool.lang.clone(),
                socket_path,
                syscmd,
                pid: 0,
            }
        })
        .collect()
}

/// Fork and exec a language pool daemon. Returns child PID.
///
/// The child inherits the nexus's stdin/stdout/stderr unchanged: anything a
/// sourced function prints must reach the terminal byte-for-byte without
/// morloc interposing. Runtime errors raised inside the pool are caught by
/// the pool's own dispatch wrapper and returned as morloc error packets.
fn start_language_server(socket: &PoolSocket) -> Result<i32, String> {
    let pid = unsafe { libc::fork() };

    if pid == 0 {
        // Child process
        unsafe { libc::setpgid(0, 0) };

        let argv: Vec<*const libc::c_char> = socket
            .syscmd
            .iter()
            .map(|s| s.as_ptr())
            .chain(std::iter::once(std::ptr::null()))
            .collect();

        unsafe {
            libc::execvp(argv[0], argv.as_ptr());
        }
        // Only reached if exec fails.
        eprintln!(
            "execvp failed for {}: {}",
            socket.lang,
            std::io::Error::last_os_error()
        );
        unsafe { libc::_exit(127) };
    } else if pid > 0 {
        // Parent: ensure child is in its own process group
        unsafe { libc::setpgid(pid, pid) };
        Ok(pid)
    } else {
        Err(format!("fork failed: {}", std::io::Error::last_os_error()))
    }
}

/// Start pool daemons for the given socket indices and wait for them to respond to pings.
pub fn start_daemons(sockets: &mut [PoolSocket], indices: &[usize]) -> Result<(), String> {
    for &idx in indices {
        let pid = start_language_server(&sockets[idx])?;
        sockets[idx].pid = pid;
        PIDS[idx].store(pid, Ordering::Relaxed);
        PGIDS[idx].store(pid, Ordering::Relaxed);
    }

    // Wait for each daemon to respond to pings
    for &idx in indices {
        wait_for_daemon(&sockets[idx], idx)?;
    }

    Ok(())
}

/// Ping a daemon with exponential backoff until it responds.
/// Matches the C nexus behavior: initial delay 1ms, multiplier 1.25,
/// plus socket timeout that doubles from 10ms to ~10s.
fn wait_for_daemon(socket: &PoolSocket, pool_index: usize) -> Result<(), String> {
    use morloc_runtime::packet::PacketHeader;
    use std::os::unix::net::UnixStream;
    use std::io::{Read, Write};

    let ping = PacketHeader::ping();
    let ping_bytes = ping.to_bytes();
    let mut retry_delay = INITIAL_RETRY_DELAY.as_secs_f64();
    let mut ping_timeout = INITIAL_PING_TIMEOUT;

    for attempt in 0..=MAX_RETRIES {
        // Check if child already died. The pool's stderr was inherited
        // directly, so any traceback it printed is already on the user's
        // terminal; the nexus just reports the exit status here.
        if PIDS[pool_index].load(Ordering::Relaxed) == -1 {
            let status = EXIT_STATUSES[pool_index].load(Ordering::Relaxed);
            return Err(format!(
                "Pool process for '{}' died unexpectedly (status: {})",
                socket.lang, status
            ));
        }

        // Try to connect and ping
        match UnixStream::connect(&socket.socket_path) {
            Ok(mut stream) => {
                let _ = stream.set_read_timeout(Some(ping_timeout));
                let _ = stream.set_write_timeout(Some(ping_timeout));

                if stream.write_all(&ping_bytes).is_ok() {
                    let mut resp = [0u8; 32];
                    if stream.read_exact(&mut resp).is_ok() {
                        if let Ok(hdr) = PacketHeader::from_bytes(&resp) {
                            if hdr.is_ping() {
                                return Ok(());
                            }
                        }
                    }
                }
            }
            Err(_) => {}
        }

        if attempt == MAX_RETRIES {
            return Err(format!(
                "Failed to ping pool '{}' at {} after {} retries",
                socket.lang, socket.socket_path, MAX_RETRIES
            ));
        }

        // Sleep with exponential backoff
        // Use the larger of retry_delay or ping_timeout to ensure we wait
        // long enough for slow-starting pools (R, Python)
        let wait = retry_delay.max(ping_timeout.as_secs_f64());
        let secs = wait as u64;
        let nanos = ((wait - secs as f64) * 1e9) as u32;
        std::thread::sleep(Duration::new(secs, nanos));
        retry_delay *= RETRY_MULTIPLIER;
        ping_timeout = ping_timeout * 2;
    }

    unreachable!()
}

/// Return a C-compatible function pointer for pool_is_alive.
pub fn pool_is_alive_ptr() -> *const std::ffi::c_void {
    extern "C" fn pool_alive_c(pool_index: usize) -> bool {
        pool_is_alive(pool_index)
    }
    pool_alive_c as *const std::ffi::c_void
}

/// Check if a pool at given index is alive.
pub fn pool_is_alive(pool_index: usize) -> bool {
    if pool_index >= MAX_DAEMONS {
        return false;
    }
    let pid = PIDS[pool_index].load(Ordering::Relaxed);
    if pid <= 0 {
        return false;
    }
    unsafe { libc::kill(pid, 0) == 0 }
}

/// Get the exit status of a reaped pool, returning signal/exit info.
pub fn pool_death_info(pool_index: usize) -> Option<String> {
    if PIDS[pool_index].load(Ordering::Relaxed) != -1 {
        return None;
    }
    let st = EXIT_STATUSES[pool_index].load(Ordering::Relaxed);
    if libc::WIFSIGNALED(st) {
        let sig = libc::WTERMSIG(st);
        Some(format!("Pool process crashed with signal {sig}"))
    } else if libc::WIFEXITED(st) {
        let code = libc::WEXITSTATUS(st);
        Some(format!("Pool process exited with status {code}"))
    } else {
        Some("Pool process died unexpectedly".into())
    }
}

/// Validate that all pool executables exist.
pub fn validate_pools(pools: &[Pool]) -> Result<(), String> {
    for pool in pools {
        if let Some(exec) = pool.exec.last() {
            if !Path::new(exec).exists() {
                return Err(format!(
                    "Build artifacts missing or stale. Pool file '{}' not found. Re-run `morloc make`.",
                    exec
                ));
            }
        }
    }
    Ok(())
}

/// Create a temporary directory for this nexus session.
pub fn make_tmpdir() -> Result<String, String> {
    let template = CString::new("/tmp/morloc.XXXXXX").unwrap();
    let mut buf = template.into_bytes_with_nul();
    let ptr = buf.as_mut_ptr() as *mut libc::c_char;
    let result = unsafe { libc::mkdtemp(ptr) };
    if result.is_null() {
        return Err(format!(
            "Failed to create temporary directory: {}",
            std::io::Error::last_os_error()
        ));
    }
    let cstr = unsafe { std::ffi::CStr::from_ptr(result) };
    Ok(cstr.to_string_lossy().into_owned())
}

/// Generate a job hash from seed, pid, and timestamps.
pub fn make_job_hash(seed: u64) -> u64 {
    use morloc_runtime::hash::xxh64;

    let pid = std::process::id() as u64;
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default();
    let epoch_ns = now.as_nanos() as u64;

    let data = format!("{}:{}:{}", pid, epoch_ns, seed);
    xxh64(data.as_bytes())
}

/// Become a subreaper so orphaned grandchildren get reparented to us.
/// Only available on Linux; no-op on other platforms.
pub fn set_child_subreaper() {
    #[cfg(target_os = "linux")]
    unsafe {
        libc::prctl(libc::PR_SET_CHILD_SUBREAPER, 1, 0, 0, 0);
    }
}
