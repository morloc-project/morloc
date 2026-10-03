//! Pool server lifecycle: accept connections, dispatch packets, manage workers.
//! Replaces pool.c. Uses std::thread instead of raw pthreads for thread mode.

use std::ffi::{c_char, c_void, CStr};
use std::ptr;

use std::sync::atomic::{AtomicBool, AtomicI32, Ordering};
use std::sync::{Arc, Mutex, Condvar};

// ── C-compatible types matching pool.h ───────────────────────────────────────

pub type PoolDispatchFn = unsafe extern "C" fn(
    mid: u32, args: *mut *const u8, nargs: usize, ctx: *mut c_void,
) -> *mut u8;

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PoolConcurrency {
    Threads = 0,
    Single = 1,
}

#[repr(C)]
pub struct PoolConfig {
    pub local_dispatch: PoolDispatchFn,
    pub remote_dispatch: PoolDispatchFn,
    pub dispatch_ctx: *mut c_void,
    pub concurrency: PoolConcurrency,
    pub initial_workers: i32,
    pub dynamic_scaling: bool,
    /// Run on the worker thread once a dispatch's reply has been sent (or
    /// could not be). The reply carries the caller's own reference to its
    /// value, so a pool releases what the dispatch still holds here.
    pub after_reply: Option<unsafe extern "C" fn()>,
}

// SAFETY: PoolConfig contains function pointers and a *mut c_void dispatch_ctx.
// The function pointers are set once at startup and never mutated.
// dispatch_ctx points to language-runtime state protected by the runtime's
// own synchronization.
// The pool architecture guarantees dispatch_ctx is not concurrently mutated.
unsafe impl Send for PoolConfig {}
unsafe impl Sync for PoolConfig {}

// ── Global state ─────────────────────────────────────────────────────────────

static SHUTTING_DOWN: AtomicBool = AtomicBool::new(false);
static BUSY_COUNT: AtomicI32 = AtomicI32::new(0);
static TOTAL_WORKERS: AtomicI32 = AtomicI32::new(0);

#[no_mangle]
pub extern "C" fn pool_mark_busy() {
    BUSY_COUNT.fetch_add(1, Ordering::Relaxed);
}

#[no_mangle]
pub extern "C" fn pool_mark_idle() {
    BUSY_COUNT.fetch_sub(1, Ordering::Relaxed);
}

extern "C" fn pool_sigterm_handler(_sig: i32) {
    SHUTTING_DOWN.store(true, Ordering::Relaxed);
}

// ── Packet dispatch ──────────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn pool_dispatch_packet(
    packet: *const u8,
    local_dispatch: PoolDispatchFn,
    remote_dispatch: PoolDispatchFn,
    ctx: *mut c_void,
) -> *mut u8 {
    use crate::packet_ffi::make_fail_packet;
    use crate::packet_ffi::packet_is_ping;
    use crate::packet_ffi::return_ping;
    use crate::packet_ffi::packet_is_local_call;
    use crate::packet_ffi::packet_is_remote_call;
    use crate::packet_ffi::read_morloc_call_packet;
    use crate::packet_ffi::free_morloc_call;

    if packet.is_null() {
        return make_fail_packet(b"NULL packet in pool dispatch\0".as_ptr() as *const c_char);
    }

    let mut errmsg: *mut c_char = ptr::null_mut();

    if packet_is_ping(packet, &mut errmsg) {
        if !errmsg.is_null() { return fail_from_errmsg(errmsg); }
        let pong = return_ping(packet, &mut errmsg);
        if !errmsg.is_null() { return fail_from_errmsg(errmsg); }
        return pong;
    }
    if !errmsg.is_null() { return fail_from_errmsg(errmsg); }

    let is_local = packet_is_local_call(packet, &mut errmsg);
    if !errmsg.is_null() { return fail_from_errmsg(errmsg); }
    let is_remote = packet_is_remote_call(packet, &mut errmsg);
    if !errmsg.is_null() { return fail_from_errmsg(errmsg); }

    if is_local || is_remote {
        let call = read_morloc_call_packet(packet, &mut errmsg);
        if !errmsg.is_null() { return fail_from_errmsg(errmsg); }

        let mid = (*call).midx;
        let args = (*call).args as *const *const u8;
        let nargs = (*call).nargs;

        // Tag the dispatch so @tmpfile's registry can tell this call's files
        // from a concurrent call's. The previous id is restored rather than
        // cleared: pool_dispatch_packet can run under a daemon dispatch.
        let (temp_owner, prev_temp_owner) = crate::intrinsics::begin_dispatch();

        let dispatch_fn = if is_local { local_dispatch } else { remote_dispatch };
        let result = dispatch_fn(mid, args.cast_mut(), nargs, ctx);

        free_morloc_call(call);

        // Remove any whole-form gather temp files this call left registered
        // (e.g. a handler that raised before its @close(path)). Success-path
        // temps are already gone via @close(path).
        crate::intrinsics::end_dispatch(temp_owner, prev_temp_owner);

        // Write the stream batches this call left compressing, before the
        // reply lets anyone read or extend those streams.
        if let Err(e) = crate::stream::drain_sealed_batches() {
            if !result.is_null() {
                libc::free(result as *mut c_void);
            }
            let msg = std::ffi::CString::new(e.to_string().replace('\0', " "))
                .unwrap_or_default();
            crate::stream::pool_reclaim_stdio_after_dispatch();
            return make_fail_packet(msg.as_ptr());
        }

        // Reclaim any stdio singleton (@stdout/@stderr/@stdin) this dispatch
        // left open -- e.g. an exception unwound past @close on a broken
        // pipe. Without this, the claim persists in the nexus-scoped SHM
        // registry and every later invocation fails "@stdout already open"
        // while the (daemon-mode) pool stays alive. Cheap on the common
        // no-stdio path: a single thread-local read (see the fn's gate).
        crate::stream::pool_reclaim_stdio_after_dispatch();

        if result.is_null() {
            return make_fail_packet(b"dispatch callback returned NULL\0".as_ptr() as *const c_char);
        }
        return result;
    }

    make_fail_packet(b"Unexpected packet type in pool dispatch\0".as_ptr() as *const c_char)
}

unsafe fn fail_from_errmsg(errmsg: *mut c_char) -> *mut u8 {
    use crate::packet_ffi::make_fail_packet;
    let pkt = make_fail_packet(errmsg);
    libc::free(errmsg as *mut c_void);
    pkt
}

// ── Helpers ──────────────────────────────────────────────────────────────────

unsafe fn try_send_fail(client_fd: i32, msg: *const c_char) {
    use crate::packet_ffi::make_fail_packet;
    use crate::ipc_ffi::send_packet_to_foreign_server;
    let fail = make_fail_packet(if msg.is_null() { b"Unknown error\0".as_ptr() as *const c_char } else { msg });
    if !fail.is_null() {
        let mut err: *mut c_char = ptr::null_mut();
        send_packet_to_foreign_server(client_fd, fail, &mut err);
        libc::free(fail as *mut c_void);
        if !err.is_null() { libc::free(err as *mut c_void); }
    }
}

// ── Thread mode job queue ────────────────────────────────────────────────────

struct JobQueue {
    jobs: Mutex<Vec<i32>>,
    cond: Condvar,
}

// Outcome of a bounded wait for work: a job to run, an idle timeout (the caller
// decides whether to keep waiting or reap the worker), or shutdown.
enum PopResult {
    Job(i32),
    Timeout,
    Shutdown,
}

impl JobQueue {
    fn new() -> Self {
        JobQueue { jobs: Mutex::new(Vec::new()), cond: Condvar::new() }
    }

    fn push(&self, fd: i32) {
        let mut jobs = self.jobs.lock().unwrap();
        jobs.push(fd);
        self.cond.notify_one();
    }

    // Wait up to `wait` for a job. Unlike an unbounded pop, a `Timeout` return
    // lets the worker loop check its idle deadline and exit if it is a surplus
    // (dynamically-spawned) worker.
    fn pop_timeout(&self, wait: std::time::Duration) -> PopResult {
        let mut jobs = self.jobs.lock().unwrap();
        if SHUTTING_DOWN.load(Ordering::Relaxed) { return PopResult::Shutdown; }
        if let Some(fd) = jobs.pop() { return PopResult::Job(fd); }
        let (mut jobs, _) = self.cond.wait_timeout(jobs, wait).unwrap();
        if SHUTTING_DOWN.load(Ordering::Relaxed) { return PopResult::Shutdown; }
        if let Some(fd) = jobs.pop() { return PopResult::Job(fd); }
        PopResult::Timeout
    }
}

// Seconds a surplus worker stays idle before exiting. Mirrors the Python pool's
// WORKER_IDLE_TIMEOUT so a burst of concurrency does not leave threads (and the
// fds/stacks they hold) alive for the rest of the run.
const WORKER_IDLE_TIMEOUT: std::time::Duration = std::time::Duration::from_secs(5);

// ── Worker thread ────────────────────────────────────────────────────────────

// A worker runs on the std default stack (RUST_MIN_STACK overrides it). No
// operation on a value spends a machine frame per level of the value, so
// the stack bounds the recursion of user code only.

// Alternate signal stack of a worker thread. Without one, a stack overflow in
// the worker cannot run the process's SIGSEGV handler at all: the kernel has
// nowhere to push the signal frame and kills the process silently (Linux) or
// with SIGILL (macOS), hiding the backtrace the handler would have printed.
const WORKER_ALTSTACK_SIZE: usize = 256 << 10;

// A worker's alternate signal stack, released when the worker exits.
struct AltStack { base: *mut c_void }

impl AltStack {
    unsafe fn install() -> Option<AltStack> {
        let base = libc::mmap(
            ptr::null_mut(), WORKER_ALTSTACK_SIZE,
            libc::PROT_READ | libc::PROT_WRITE,
            libc::MAP_PRIVATE | libc::MAP_ANON, -1, 0,
        );
        if base == libc::MAP_FAILED { return None; }
        let ss = libc::stack_t { ss_sp: base, ss_size: WORKER_ALTSTACK_SIZE, ss_flags: 0 };
        if libc::sigaltstack(&ss, ptr::null_mut()) != 0 {
            libc::munmap(base, WORKER_ALTSTACK_SIZE);
            return None;
        }
        Some(AltStack { base })
    }
}

impl Drop for AltStack {
    fn drop(&mut self) {
        unsafe {
            let ss = libc::stack_t { ss_sp: ptr::null_mut(), ss_size: 0, ss_flags: libc::SS_DISABLE };
            libc::sigaltstack(&ss, ptr::null_mut());
            libc::munmap(self.base, WORKER_ALTSTACK_SIZE);
        }
    }
}

unsafe fn spawn_worker(queue: &Arc<JobQueue>, config: &PoolConfig) -> std::io::Result<std::thread::JoinHandle<()>> {
    let q = Arc::clone(queue);
    let cfg = ptr::read(config); // Copy config for thread
    std::thread::Builder::new().spawn(move || {
        let _altstack = AltStack::install();
        worker_loop(&q, &cfg);
    })
}

unsafe fn worker_loop(queue: &JobQueue, config: &PoolConfig) {
    use crate::ipc_ffi::stream_from_client;
    use crate::ipc_ffi::send_packet_to_foreign_server;
    use crate::ipc_ffi::close_socket;

    let min_workers = config.initial_workers.max(1);
    let mut last_activity = std::time::Instant::now();

    while !SHUTTING_DOWN.load(Ordering::Relaxed) {
        let client_fd = match queue.pop_timeout(std::time::Duration::from_millis(100)) {
            PopResult::Job(fd) => fd,
            PopResult::Shutdown => break,
            PopResult::Timeout => {
                // Reap this worker if it is surplus (beyond the initial core
                // pool) and has been idle past the timeout. The floor check and
                // the decrement are a single atomic CAS (fetch_update), so
                // concurrent idle workers cannot race past min_workers down to
                // zero; only the worker whose CAS succeeds exits. Keeping at
                // least min_workers alive avoids churn under steady load, and the
                // decrement lets the accept loop spawn again on the next burst.
                if config.dynamic_scaling && last_activity.elapsed() > WORKER_IDLE_TIMEOUT {
                    let reaped = TOTAL_WORKERS
                        .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |t| {
                            if t > min_workers { Some(t - 1) } else { None }
                        })
                        .is_ok();
                    if reaped {
                        return;
                    }
                }
                continue;
            }
        };

        let mut errmsg: *mut c_char = ptr::null_mut();
        let data = stream_from_client(client_fd, &mut errmsg);
        if data.is_null() || !errmsg.is_null() {
            // Log why the request read failed before closing: otherwise the
            // caller only sees a bare "Connection closed by peer" with no cause.
            // eprintln! writes to Rust's unbuffered stderr (a raw fdopen(2)
            // FILE* would be block-buffered and lost if the pool never flushes).
            if !errmsg.is_null() {
                let m = std::ffi::CStr::from_ptr(errmsg).to_string_lossy();
                eprintln!("morloc pool: request read failed: {}", m);
                try_send_fail(client_fd, errmsg);
                libc::free(errmsg as *mut c_void);
            } else {
                eprintln!("morloc pool: request read returned no data");
            }
            libc::free(data as *mut c_void);
            close_socket(client_fd);
            continue;
        }

        // Track busy state so the accept loop can spawn new workers if needed
        pool_mark_busy();
        let result = pool_dispatch_packet(data, config.local_dispatch, config.remote_dispatch, config.dispatch_ctx);
        pool_mark_idle();
        libc::free(data as *mut c_void);

        if !result.is_null() {
            send_packet_to_foreign_server(client_fd, result, &mut errmsg);
            libc::free(result as *mut c_void);
            // A failed response send was previously freed silently, leaving the
            // caller with an unexplained "Connection closed" -- log the cause.
            if !errmsg.is_null() {
                let m = std::ffi::CStr::from_ptr(errmsg).to_string_lossy();
                eprintln!("morloc pool: response send failed: {}", m);
                libc::free(errmsg as *mut c_void);
            }
        } else {
            // pool_dispatch_packet returned null: no response is sent and the
            // socket is closed below, so the caller sees a bare "Connection
            // closed by peer". This should not happen (dispatch always returns
            // at least a fail packet); log it to catch the case if it does.
            eprintln!("morloc pool: dispatch returned null result (no response sent)");
        }
        if let Some(f) = config.after_reply { f(); }

        libc::fflush(ptr::null_mut()); // flush stdout
        close_socket(client_fd);
        last_activity = std::time::Instant::now();
    }
}

// ── Pool main: threads mode ──────────────────────────────────────────────────

unsafe fn pool_main_threads(config: &PoolConfig, socket_path: *const c_char, tmpdir: *const c_char, shm_basename: *const c_char) -> i32 {
    use crate::ipc_ffi::start_daemon;
    use crate::ipc_ffi::close_daemon;
    use crate::ipc_ffi::wait_for_client_with_timeout;

    let mut errmsg: *mut c_char = ptr::null_mut();
    let mut daemon = start_daemon(socket_path, tmpdir, shm_basename, 0xffff, &mut errmsg);
    if !errmsg.is_null() {
        eprintln!("Failed to start language server:\n{}", CStr::from_ptr(errmsg).to_string_lossy());
        libc::free(errmsg as *mut c_void);
        return 1;
    }

    let queue = Arc::new(JobQueue::new());
    let nthreads = config.initial_workers.max(1) as usize;
    TOTAL_WORKERS.store(nthreads as i32, Ordering::Relaxed);

    let mut handles = Vec::with_capacity(nthreads);
    for _ in 0..nthreads {
        handles.push(spawn_worker(&queue, config).expect("failed to spawn pool worker thread"));
    }

    while !SHUTTING_DOWN.load(Ordering::Relaxed) {
        let client_fd = wait_for_client_with_timeout(daemon, 10000, &mut errmsg);
        if !errmsg.is_null() {
            // A dropped accept-loop error can leave an accepted client
            // unserved (the caller then sees a bare "Connection closed"); log it.
            let m = std::ffi::CStr::from_ptr(errmsg).to_string_lossy();
            eprintln!("morloc pool: accept loop error: {}", m);
            libc::free(errmsg as *mut c_void);
            errmsg = ptr::null_mut();
        }
        if client_fd > 0 {
            queue.push(client_fd);
        }

        // Dynamic scaling: spawn a new worker if all are busy
        if config.dynamic_scaling {
            let busy = BUSY_COUNT.load(Ordering::Relaxed);
            let total = TOTAL_WORKERS.load(Ordering::Relaxed);
            if busy >= total {
                match spawn_worker(&queue, config) {
                    Ok(h) => {
                        handles.push(h);
                        TOTAL_WORKERS.fetch_add(1, Ordering::Relaxed);
                    }
                    // The existing workers keep serving the queue.
                    Err(e) => eprintln!("morloc pool: failed to spawn worker thread: {}", e),
                }
            }
        }

        // Drop join handles of workers that exited on idle timeout so the Vec
        // does not grow without bound over a long-running, bursty program.
        handles.retain(|h| !h.is_finished());
    }

    SHUTTING_DOWN.store(true, Ordering::Relaxed);
    queue.cond.notify_all();

    for h in handles { let _ = h.join(); }

    close_daemon(&mut daemon);
    0
}

// ── Pool main: single mode ───────────────────────────────────────────────────

unsafe fn pool_main_single(config: &PoolConfig, socket_path: *const c_char, tmpdir: *const c_char, shm_basename: *const c_char) -> i32 {
    use crate::ipc_ffi::start_daemon;
    use crate::ipc_ffi::close_daemon;
    use crate::ipc_ffi::wait_for_client_with_timeout;
    use crate::ipc_ffi::stream_from_client;
    use crate::ipc_ffi::send_packet_to_foreign_server;
    use crate::ipc_ffi::close_socket;

    let mut errmsg: *mut c_char = ptr::null_mut();
    let mut daemon = start_daemon(socket_path, tmpdir, shm_basename, 0xffff, &mut errmsg);
    if !errmsg.is_null() {
        eprintln!("Failed to start language server:\n{}", CStr::from_ptr(errmsg).to_string_lossy());
        libc::free(errmsg as *mut c_void);
        return 1;
    }

    while !SHUTTING_DOWN.load(Ordering::Relaxed) {
        let client_fd = wait_for_client_with_timeout(daemon, 10000, &mut errmsg);
        if !errmsg.is_null() { libc::free(errmsg as *mut c_void); errmsg = ptr::null_mut(); }
        if client_fd <= 0 { continue; }

        let data = stream_from_client(client_fd, &mut errmsg);
        if data.is_null() || !errmsg.is_null() {
            if !errmsg.is_null() { try_send_fail(client_fd, errmsg); libc::free(errmsg as *mut c_void); errmsg = ptr::null_mut(); }
            libc::free(data as *mut c_void);
            close_socket(client_fd);
            continue;
        }

        let result = pool_dispatch_packet(data, config.local_dispatch, config.remote_dispatch, config.dispatch_ctx);
        libc::free(data as *mut c_void);

        if !result.is_null() {
            send_packet_to_foreign_server(client_fd, result, &mut errmsg);
            libc::free(result as *mut c_void);
            if !errmsg.is_null() { libc::free(errmsg as *mut c_void); errmsg = ptr::null_mut(); }
        }
        if let Some(f) = config.after_reply { f(); }

        libc::fflush(ptr::null_mut());
        close_socket(client_fd);
    }

    close_daemon(&mut daemon);
    0
}

// ── Entry point ──────────────────────────────────────────────────────────────

/// Stop a streaming pool from handing its heap back to the kernel between
/// batches.
///
/// A pass over a stream allocates a batch, works on it, and drops it, over and
/// over at the same sizes. glibc answers that pattern badly out of the box: a
/// batch large enough crosses the mmap threshold and is unmapped on free, and
/// what does live on the heap crosses the trim threshold and is released with
/// `MADV_DONTNEED`. Either way the next batch faults every page back in. On a
/// 1.5 GB FASTA pass that was 2.4 million minor faults and about half the wall
/// clock, and it moved run to run, which made real changes unreadable.
///
/// Keeping both thresholds above a batch turns those faults into free-list
/// hits: the same pass drops to 176 thousand faults and stops varying.
///
/// A dispatch runs on a worker thread, and a thread's arena is not trimmed by
/// the threshold above -- glibc shrinks it by the padding it keeps past the
/// top instead. Left at its 128 KiB default that gives back a page or so on
/// nearly every free, which is a syscall each: 143 thousand of them on a
/// 200 MB gather. Keeping a batch's worth of headroom removes them.
///
/// The glibc environment variables still win. They are read before the first
/// allocation, so a value set there is already in force and this leaves it
/// alone.
#[cfg(target_env = "gnu")]
unsafe fn tune_allocator() {
    if std::env::var_os("MORLOC_MALLOC_TUNING").as_deref() == Some(std::ffi::OsStr::new("off")) {
        return;
    }
    // Comfortably above a default 16 MiB sub-packet, so a batch and the values
    // built from it stay on the heap and are reused rather than remapped.
    const MMAP_THRESHOLD: libc::c_int = 32 * 1024 * 1024;
    const TRIM_THRESHOLD: libc::c_int = 64 * 1024 * 1024;
    const TOP_PAD: libc::c_int = 64 * 1024 * 1024;
    if std::env::var_os("MALLOC_MMAP_THRESHOLD_").is_none() {
        libc::mallopt(libc::M_MMAP_THRESHOLD, MMAP_THRESHOLD);
    }
    if std::env::var_os("MALLOC_TRIM_THRESHOLD_").is_none() {
        libc::mallopt(libc::M_TRIM_THRESHOLD, TRIM_THRESHOLD);
    }
    if std::env::var_os("MALLOC_TOP_PAD_").is_none() {
        libc::mallopt(libc::M_TOP_PAD, TOP_PAD);
    }
}

/// Only glibc exposes these knobs; every other allocator keeps its own policy.
#[cfg(not(target_env = "gnu"))]
unsafe fn tune_allocator() {}

#[no_mangle]
pub unsafe extern "C" fn pool_main(
    argc: i32,
    argv: *mut *mut c_char,
    config: *mut PoolConfig,
) -> i32 {
    tune_allocator();
    if argc != 4 {
        let prog = if argc > 0 { CStr::from_ptr(*argv).to_string_lossy() } else { "pool".into() };
        eprintln!("Usage: {} <socket_path> <tmpdir> <shm_basename>", prog);
        return 1;
    }

    let cfg = &mut *config;
    if cfg.initial_workers <= 0 { cfg.initial_workers = 1; }

    SHUTTING_DOWN.store(false, Ordering::Relaxed);
    BUSY_COUNT.store(0, Ordering::Relaxed);

    // SIGTERM handler
    let mut sa: libc::sigaction = std::mem::zeroed();
    sa.sa_sigaction = pool_sigterm_handler as *const () as usize;
    libc::sigemptyset(&mut sa.sa_mask);
    libc::sigaction(libc::SIGTERM, &sa, ptr::null_mut());

    // Ignore SIGPIPE process-wide. A peer closing the socket mid-write must
    // surface as the EPIPE return the IPC code already handles, not kill the
    // pool. Linux masks this via MSG_NOSIGNAL on every send; macOS has no
    // per-call flag (SEND_NOSIGNAL == 0), so without this a broken pipe
    // terminates the pool and the caller sees "Connection closed by peer".
    libc::signal(libc::SIGPIPE, libc::SIG_IGN);

    let socket_path = *argv.add(1);
    let tmpdir = *argv.add(2);
    let shm_basename = *argv.add(3);
    crate::ipc_ffi::mlc_set_self_socket(socket_path);

    match cfg.concurrency {
        PoolConcurrency::Threads => pool_main_threads(cfg, socket_path, tmpdir, shm_basename),
        PoolConcurrency::Single => pool_main_single(cfg, socket_path, tmpdir, shm_basename),
    }
}
