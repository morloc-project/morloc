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
    /// Start workers as calls need them. Without it the pool keeps
    /// `initial_workers`, which must exceed the depth of calls back into the
    /// pool that can be waiting at once.
    pub dynamic_scaling: bool,
    /// Release what a dispatch still holds. Runs on the worker thread once
    /// the reply holds the caller's own reference to its value and before
    /// the reply is sent (see `send_reply_to_foreign_server`), or after the
    /// dispatch when there is no reply.
    pub release_dispatch: Option<unsafe extern "C" fn()>,
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
// Threads blocked in a call to another pool. Starting workers does not depend
// on it (see JobQueue); it is kept for inspection.
static BUSY_COUNT: AtomicI32 = AtomicI32::new(0);

pub(crate) fn pool_mark_busy() {
    BUSY_COUNT.fetch_add(1, Ordering::Relaxed);
}

pub(crate) fn pool_mark_idle() {
    BUSY_COUNT.fetch_sub(1, Ordering::Relaxed);
}

extern "C" fn pool_sigterm_handler(_sig: i32) {
    SHUTTING_DOWN.store(true, Ordering::Relaxed);
}

// ── Packet dispatch ──────────────────────────────────────────────────────────

pub(crate) unsafe fn pool_dispatch_packet(
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
        let started = crate::fork_policy::generation();
        let result = dispatch_fn(mid, args.cast_mut(), nargs, ctx);
        // FORK-12: before anything of the worker's is touched.
        crate::ipc_ffi::exit_if_forked_since(started);

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

// Every count that decides whether to start a worker, kept under one mutex so
// that a job, a worker becoming free and a worker retiring are seen in a
// single order. A job that finds no worker able to take it starts one: a
// callback into a pool whose workers all wait on other pools therefore always
// finds a worker, and calls that arrive one after another never start a
// second.
struct QueueState {
    jobs: Vec<i32>,
    total: usize,
    // Waiting for a job.
    idle: usize,
    // Started, not yet waiting.
    starting: usize,
    // Replied, on the way back to wait: counted free from just before the
    // reply is sent, since the caller may send its next call the moment the
    // reply arrives.
    finishing: usize,
}

impl QueueState {
    fn free(&self) -> usize {
        self.idle + self.starting + self.finishing
    }
}

struct JobQueue {
    state: Mutex<QueueState>,
    cond: Condvar,
}

// What a worker was doing before it asks for a job; it stops being counted
// as such once it is counted idle.
#[derive(Clone, Copy)]
enum Arriving {
    Starting,
    Finishing,
    Other,
}

enum PopResult {
    Job(i32),
    Retire,
    Shutdown,
}

impl JobQueue {
    fn new(workers: usize) -> Self {
        JobQueue {
            state: Mutex::new(QueueState {
                jobs: Vec::new(),
                total: workers,
                idle: 0,
                starting: workers,
                finishing: 0,
            }),
            cond: Condvar::new(),
        }
    }

    fn push(&self, fd: i32) {
        let mut st = self.state.lock().unwrap();
        st.jobs.push(fd);
        self.cond.notify_one();
    }

    // When some queued job has no free worker to take it, count a worker as
    // starting and return true: the caller must start it.
    fn reserve_start(&self) -> bool {
        let mut st = self.state.lock().unwrap();
        if st.jobs.len() > st.free() {
            st.total += 1;
            st.starting += 1;
            true
        } else {
            false
        }
    }

    // A worker counted by `reserve_start` could not be started.
    fn start_failed(&self) {
        let mut st = self.state.lock().unwrap();
        st.total -= 1;
        st.starting -= 1;
    }

    // Take a queued job that no free worker can take, to fail it. Which job
    // does not matter: any one leaves the rest covered.
    fn take_uncovered(&self) -> Option<i32> {
        let mut st = self.state.lock().unwrap();
        if st.jobs.len() > st.free() {
            st.jobs.pop()
        } else {
            None
        }
    }

    // The worker is about to send its reply.
    fn finishing(&self) {
        self.state.lock().unwrap().finishing += 1;
    }

    // Wait for a job. A surplus worker idle past `retire_after` retires; the
    // decision is taken under the lock, so no job can arrive unseen between
    // it and the worker leaving.
    fn pop(
        &self,
        arriving: Arriving,
        idle_since: std::time::Instant,
        retire_after: Option<std::time::Duration>,
        min_workers: usize,
    ) -> PopResult {
        let mut st = self.state.lock().unwrap();
        match arriving {
            Arriving::Starting => st.starting -= 1,
            Arriving::Finishing => st.finishing -= 1,
            Arriving::Other => {}
        }
        loop {
            if SHUTTING_DOWN.load(Ordering::Relaxed) {
                return PopResult::Shutdown;
            }
            if let Some(fd) = st.jobs.pop() {
                return PopResult::Job(fd);
            }
            if let Some(t) = retire_after {
                if st.total > min_workers && idle_since.elapsed() >= t {
                    st.total -= 1;
                    return PopResult::Retire;
                }
            }
            st.idle += 1;
            st = self.cond.wait_timeout(st, std::time::Duration::from_millis(100)).unwrap().0;
            st.idle -= 1;
        }
    }
}

// How long a surplus worker stays idle before it exits, unless
// MORLOC_POOL_IDLE_TIMEOUT_MS says otherwise. Mirrors the Python pool's
// WORKER_IDLE_TIMEOUT so a burst of concurrency does not leave threads (and
// the fds/stacks they hold) alive for the rest of the run.
// Consecutive passes of the accept loop (about 10 ms apart) on which a worker
// fails to start before the jobs waiting for one are failed.
const START_ATTEMPTS: u32 = 50;

fn worker_idle_timeout() -> std::time::Duration {
    std::env::var("MORLOC_POOL_IDLE_TIMEOUT_MS")
        .ok()
        .and_then(|v| v.trim().parse::<u64>().ok())
        .map(std::time::Duration::from_millis)
        .unwrap_or(std::time::Duration::from_secs(5))
}

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

unsafe fn spawn_worker(
    queue: &Arc<JobQueue>,
    config: &PoolConfig,
    retire_after: Option<std::time::Duration>,
) -> std::io::Result<std::thread::JoinHandle<()>> {
    let q = Arc::clone(queue);
    let cfg = ptr::read(config); // Copy config for thread
    std::thread::Builder::new().spawn(move || {
        let _altstack = AltStack::install();
        worker_loop(&q, &cfg, retire_after);
    })
}

// `retire_after` is how long a worker beyond the initial ones may stay idle
// before it exits; None keeps every worker.
unsafe fn worker_loop(queue: &JobQueue, config: &PoolConfig, retire_after: Option<std::time::Duration>) {
    use crate::ipc_ffi::stream_from_client;
    use crate::ipc_ffi::send_reply_to_foreign_server;
    use crate::ipc_ffi::close_socket;

    let min_workers = config.initial_workers.max(1) as usize;
    let mut idle_since = std::time::Instant::now();
    let mut arriving = Arriving::Starting;

    while !SHUTTING_DOWN.load(Ordering::Relaxed) {
        let client_fd = match queue.pop(arriving, idle_since, retire_after, min_workers) {
            PopResult::Job(fd) => fd,
            PopResult::Shutdown => break,
            PopResult::Retire => return,
        };
        arriving = Arriving::Other;

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

        let result = pool_dispatch_packet(data, config.local_dispatch, config.remote_dispatch, config.dispatch_ctx);
        libc::free(data as *mut c_void);
        libc::fflush(ptr::null_mut()); // flush stdout

        // From here the worker counts as free, so nothing below may wait on
        // another thread of the program: only the send (which waits on the
        // caller, already reading), the dispatch's release and the close.
        queue.finishing();
        arriving = Arriving::Finishing;

        if !result.is_null() {
            send_reply_to_foreign_server(client_fd, result, config.release_dispatch, &mut errmsg);
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
            if let Some(f) = config.release_dispatch { f(); }
        }

        close_socket(client_fd);
        idle_since = std::time::Instant::now();
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

    let nthreads = config.initial_workers.max(1) as usize;
    let queue = Arc::new(JobQueue::new(nthreads));
    let retire_after = if config.dynamic_scaling { Some(worker_idle_timeout()) } else { None };

    let mut handles = Vec::with_capacity(nthreads);
    for _ in 0..nthreads {
        handles.push(spawn_worker(&queue, config, retire_after).expect("failed to spawn pool worker thread"));
    }
    let mut start_failures = 0u32;

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

        // Start a worker for each queued job that no worker can take. A start
        // that fails is retried on the next pass; once it has failed for
        // START_ATTEMPTS passes in a row, the jobs no worker can take are
        // failed, since left queued they could wait on workers that are
        // themselves waiting on them.
        while config.dynamic_scaling && queue.reserve_start() {
            match spawn_worker(&queue, config, retire_after) {
                Ok(h) => {
                    handles.push(h);
                    start_failures = 0;
                }
                Err(e) => {
                    queue.start_failed();
                    if start_failures == 0 {
                        eprintln!("morloc pool: failed to spawn worker thread: {}", e);
                    }
                    start_failures += 1;
                    if start_failures >= START_ATTEMPTS {
                        let msg = std::ffi::CString::new(format!(
                            "morloc pool: could not start a worker thread for this call: {}", e
                        ))
                        .unwrap_or_default();
                        while let Some(fd) = queue.take_uncovered() {
                            try_send_fail(fd, msg.as_ptr());
                            crate::ipc_ffi::close_socket(fd);
                        }
                        start_failures = 0;
                    }
                    break;
                }
            }
        }

        // Drop join handles of workers that exited on idle timeout so the Vec
        // does not grow without bound over a long-running, bursty program.
        handles.retain(|h| !h.is_finished());
    }

    SHUTTING_DOWN.store(true, Ordering::Relaxed);
    {
        let _st = queue.state.lock().unwrap();
        queue.cond.notify_all();
    }

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
    use crate::ipc_ffi::send_reply_to_foreign_server;
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
            send_reply_to_foreign_server(client_fd, result, config.release_dispatch, &mut errmsg);
            libc::free(result as *mut c_void);
            if !errmsg.is_null() { libc::free(errmsg as *mut c_void); errmsg = ptr::null_mut(); }
        } else if let Some(f) = config.release_dispatch {
            f();
        }

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

pub(crate) unsafe fn pool_main(
    argc: i32,
    argv: *mut *mut c_char,
    config: *mut PoolConfig,
) -> i32 {
    crate::panic_ffi::install(None);
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

#[cfg(test)]
mod tests {
    use super::*;

    unsafe extern "C" fn forks_and_returns(_: u32, _: *mut *const u8, _: usize, ctx: *mut c_void) -> *mut u8 {
        let child = libc::fork();
        if child > 0 {
            *(ctx as *mut libc::pid_t) = child;
        }
        crate::packet_ffi::make_fail_packet(c"returned".as_ptr())
    }

    #[test]
    fn a_child_forked_by_user_code_never_returns_from_the_dispatch() {
        let _shm = crate::init_test_shm();
        let mut err: *mut c_char = ptr::null_mut();
        let arg = unsafe { crate::packet_ffi::make_fail_packet(c"arg".as_ptr()) };
        let args = [arg as *const u8];
        let call = unsafe { crate::packet_ffi::make_morloc_local_call_packet(0, args.as_ptr(), 1, &mut err) };
        assert!(!call.is_null());
        let mut child: libc::pid_t = 0;
        let ctx = &mut child as *mut libc::pid_t as *mut c_void;
        let watchdog = unsafe { libc::getpid() };
        let reply = unsafe { pool_dispatch_packet(call, forks_and_returns, forks_and_returns, ctx) };
        if unsafe { libc::getpid() } != watchdog {
            unsafe { libc::_exit(0) };
        }
        assert!(!reply.is_null());
        let mut status = 0;
        unsafe { libc::waitpid(child, &mut status, 0) };
        unsafe {
            libc::free(reply as *mut c_void);
            libc::free(call as *mut c_void);
            libc::free(arg as *mut c_void);
        }
        assert!(libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 1, "the child returned from the dispatch: status {status}");
    }

    // Every queued job has a free worker counted to take it.
    fn covered(q: &JobQueue) -> bool {
        let st = q.state.lock().unwrap();
        st.jobs.len() <= st.free()
    }

    fn take(q: &JobQueue, arriving: Arriving) -> i32 {
        match q.pop(arriving, std::time::Instant::now(), None, 1) {
            PopResult::Job(fd) => fd,
            _ => panic!("expected a job"),
        }
    }

    // A worker fails to start for a new job while another worker, finishing,
    // takes that job: the older job left behind must still be covered or
    // be handed back to fail.
    #[test]
    fn failed_start_never_strands_an_older_job() {
        let q = JobQueue::new(1);
        q.push(10);
        assert!(!q.reserve_start());
        assert_eq!(take(&q, Arriving::Starting), 10);
        q.finishing();
        q.push(11);
        assert!(!q.reserve_start());
        q.push(12);
        assert!(q.reserve_start());
        assert_eq!(take(&q, Arriving::Finishing), 12);
        q.start_failed();
        // Job 11 now has no worker: the next pass starts one, or fails it.
        assert!(q.reserve_start());
        q.start_failed();
        assert_eq!(q.take_uncovered(), Some(11));
        assert!(covered(&q));
        assert_eq!(q.take_uncovered(), None);
    }

    // Calls arriving one at a time, each answered before the next, never
    // need a second worker.
    #[test]
    fn sequential_calls_keep_one_worker() {
        let q = JobQueue::new(1);
        let mut arriving = Arriving::Starting;
        for fd in 0..100 {
            q.push(fd);
            assert!(!q.reserve_start());
            assert_eq!(take(&q, arriving), fd);
            q.finishing();
            arriving = Arriving::Finishing;
        }
        assert_eq!(q.state.lock().unwrap().total, 1);
    }

    // A worker waiting on another pool leaves a callback with nobody to take
    // it, so the callback starts a worker.
    #[test]
    fn callback_to_a_waiting_pool_starts_a_worker() {
        let q = JobQueue::new(1);
        q.push(1);
        assert_eq!(take(&q, Arriving::Starting), 1);
        q.push(2);
        assert!(q.reserve_start());
        assert!(!q.reserve_start());
        assert!(covered(&q));
    }
}

mod c_abi {
    use super::*;

    #[no_mangle]
    pub extern "C" fn pool_mark_busy() {
        super::pool_mark_busy()
    }

    #[no_mangle]
    pub extern "C" fn pool_mark_idle() {
        super::pool_mark_idle()
    }

    #[no_mangle]
    pub unsafe extern "C" fn pool_dispatch_packet(packet: *const u8, local_dispatch: PoolDispatchFn, remote_dispatch: PoolDispatchFn, ctx: *mut c_void) -> *mut u8 {
        super::pool_dispatch_packet(packet, local_dispatch, remote_dispatch, ctx)
    }

    #[no_mangle]
    pub unsafe extern "C" fn pool_main(argc: i32, argv: *mut *mut c_char, config: *mut PoolConfig) -> i32 {
        super::pool_main(argc, argv, config)
    }
}
