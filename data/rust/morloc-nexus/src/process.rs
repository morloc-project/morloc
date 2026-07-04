//! Pool daemon process management, signal handling, and lifecycle.
//!
//! Replaces the fork/exec, SIGCHLD, SIGTERM, clean_exit logic from nexus.c.

use std::ffi::CString;
use std::path::Path;
use std::sync::atomic::{AtomicBool, AtomicI32, AtomicU64, Ordering};
use std::time::Duration;

use morloc_runtime_types::shm_types::MAX_VOLUME_NUMBER;

use crate::manifest::Pool;

pub const MAX_DAEMONS: usize = 32;

/// Pool-crash recovery generation counter. Starts at 0 (initial daemon
/// startup). Each successful coordinated recovery (kill all pools, drop
/// SHM, respawn) increments this. The current generation is encoded into
/// the SHM basename so volumes from a prior generation can never be
/// confused for current-generation volumes -- if a generation 0 file
/// somehow survives the recovery teardown, generation 1's `morloc-<pid>-
/// <hash>-gen1_X` will not collide. The next nexus startup sweep
/// (`cleanup_stale_shm`) catches any stragglers because everything still
/// starts with `morloc-<pid>-`.
pub static RECOVERY_GENERATION: AtomicU64 = AtomicU64::new(0);

/// Compute the SHM basename for a given recovery generation. Generation
/// 0 returns the base unchanged (preserves the current naming for the
/// happy path); generations 1+ append `-gen<N>`.
pub fn basename_for_generation(base: &str, generation: u64) -> String {
    if generation == 0 {
        base.to_string()
    } else {
        format!("{}-gen{}", base, generation)
    }
}

// ── Recovery context ────────────────────────────────────────────────────────

/// State retained across a daemon's lifetime so the recovery callback can
/// rebuild pool syscmds with a fresh basename and respawn pool processes.
/// The C-side MorlocSocket array passed to the callback only carries enough
/// fields to talk to live pools; it doesn't carry the original spawn
/// arguments. We keep the original Rust PoolSocket array here so recovery
/// can mutate the basename embedded in each pool's syscmd in place.
struct RecoveryContext {
    /// The Vec<PoolSocket> originally built by setup_sockets, owned for
    /// the daemon's lifetime. Recovery rewrites each pool's syscmd
    /// argv-tail (the basename slot) on every generation bump.
    sockets: Vec<PoolSocket>,
    /// Pool list from the manifest, retained so we can call
    /// `setup_sockets` again to rebuild syscmds with a fresh basename.
    pools: Vec<Pool>,
    /// tmpdir, retained for setup_sockets re-invocation on recovery.
    tmpdir: String,
    /// The original (gen=0) basename. Each recovery computes
    /// `basename_for_generation(base_basename, RECOVERY_GENERATION)` for
    /// the new namespace.
    base_basename: String,
    /// Recent recovery attempt timestamps, for the loop-guard (max ~5 per
    /// minute before we exit the daemon process).
    recent_attempts: Vec<std::time::Instant>,
}

static RECOVERY_CONTEXT: std::sync::Mutex<Option<RecoveryContext>> =
    std::sync::Mutex::new(None);

/// Install the recovery context. Called by main once, before daemon_run,
/// so the recovery callback has the data it needs to respawn pools.
pub fn install_recovery_context(
    sockets: Vec<PoolSocket>,
    pools: Vec<Pool>,
    tmpdir: String,
    base_basename: String,
) {
    let mut guard = RECOVERY_CONTEXT.lock().unwrap();
    *guard = Some(RecoveryContext {
        sockets,
        pools,
        tmpdir,
        base_basename,
        recent_attempts: Vec::new(),
    });
}

/// Return the C-ABI function pointer for the recovery callback. Pass to
/// the daemon via DaemonConfig.pool_check_fn.
pub fn pool_check_and_recover_ptr() -> *const std::ffi::c_void {
    pool_check_and_recover as *const std::ffi::c_void
}

/// Maximum recovery attempts within `RECOVERY_WINDOW`. If exceeded the
/// daemon exits fatally so an external supervisor can decide what to
/// do (most likely the underlying problem -- e.g. a missing native
/// library so the pool can't even initialize -- is fundamentally
/// outside the daemon's control). The window is wide enough to
/// accommodate stress-test workloads that intentionally kill pools at
/// up to ~1/second; sustained higher rates indicate genuinely broken
/// pools rather than bursty user-driven crashes.
const RECOVERY_MAX_ATTEMPTS: usize = 100;
const RECOVERY_WINDOW: Duration = Duration::from_secs(60);
/// Brief drain wait between marking recovery in progress and tearing
/// down SHM, so any worker mid-request can fail back through its
/// per-eval arena's Drop and release SHM before we munmap.
const RECOVERY_DRAIN: Duration = Duration::from_millis(250);
/// Wait between SIGTERM and SIGKILL on remaining live pools.
const RECOVERY_TERM_GRACE: Duration = Duration::from_millis(250);

extern "C" {
    fn shclose(errmsg: *mut *mut std::ffi::c_char) -> bool;
    fn shinit(
        basename: *const std::ffi::c_char,
        volume_index: usize,
        shm_size: usize,
        errmsg: *mut *mut std::ffi::c_char,
    ) -> *mut std::ffi::c_void;
    // C-ABI bridges to libmorloc.so's daemon-coordination atomics.
    // Used to live as `morloc_runtime::daemon_ffi::*` Rust calls, but
    // those resolved to the rlib's disjoint copy of the atomics --
    // recovery would mark "in progress" in nexus's copy and libmorloc.so
    // would never see it, with the inverse for end_recovery. The C-ABI
    // wrappers in `morloc-runtime::daemon_ffi` flip the cdylib's atomics
    // that the request handlers actually read.
    fn morloc_daemon_is_shutting_down() -> bool;
    fn morloc_daemon_begin_recovery() -> bool;
    fn morloc_daemon_end_recovery();
}

/// C-ABI callback wired into DaemonConfig.pool_check_fn.
///
/// Invoked once per daemon main-loop iteration (~1 s cadence).
/// Detects dead pool processes via `pool_is_alive`. If any are dead,
/// runs the coordinated recovery sequence:
/// 1. Set `RECOVERY_IN_PROGRESS` so request handlers bail out.
/// 2. Brief drain so in-flight workers' arenas release SHM.
/// 3. SIGTERM -> SIGKILL all remaining live pools; reap each.
/// 4. Drop the entire SHM namespace (shclose / reset_all).
/// 5. Bump RECOVERY_GENERATION, compute new basename.
/// 6. Re-shinit and respawn every pool with the new basename.
/// 7. Wait for ping responses.
/// 8. Clear `RECOVERY_IN_PROGRESS` so workers resume serving.
///
/// Loop guard: if more than RECOVERY_MAX_ATTEMPTS recoveries fire
/// within RECOVERY_WINDOW, the daemon exits with a fatal error.
extern "C" fn pool_check_and_recover(
    _sockets: *mut morloc_runtime_types::daemon_socket::MorlocSocket,
    n_pools: usize,
) {
    // First, enqueue PID-sweep requests for any pool that has died
    // since our last visit. The sweeper thread releases the dead
    // pool's slots in the shared SHM registry off this latency path.
    // Cheap on the no-death fast path (one Relaxed load per pool).
    sweep_dead_pools(n_pools);

    // Fast path: if no pool has died, we're done. This runs every
    // poll cycle so it must be cheap.
    let mut any_dead = false;
    for i in 0..n_pools {
        if !pool_is_alive(i) {
            any_dead = true;
            break;
        }
    }
    if !any_dead {
        return;
    }

    // If the daemon is already shutting down, the dead pool we just
    // observed is the result of `clean_exit` SIGTERM'ing the pool
    // group, not a crash. Don't fight the shutdown by respawning.
    if unsafe { morloc_daemon_is_shutting_down() } {
        return;
    }

    // Begin recovery; if another caller got here first, bail.
    if !unsafe { morloc_daemon_begin_recovery() } {
        return;
    }

    // Loop-guard bookkeeping under lock so concurrent (extremely
    // unlikely) recoveries don't all see an empty history.
    {
        let mut guard = RECOVERY_CONTEXT.lock().unwrap();
        if let Some(ref mut ctx) = *guard {
            let now = std::time::Instant::now();
            ctx.recent_attempts
                .retain(|t| now.duration_since(*t) < RECOVERY_WINDOW);
            ctx.recent_attempts.push(now);
            if ctx.recent_attempts.len() > RECOVERY_MAX_ATTEMPTS {
                eprintln!(
                    "morloc daemon: pool crash recovery attempted {} times in {:?}; giving up",
                    ctx.recent_attempts.len(),
                    RECOVERY_WINDOW
                );
                drop(guard);
                std::process::exit(1);
            }
        }
    }

    eprintln!("morloc daemon: pool crash detected; coordinated recovery starting");
    for i in 0..n_pools {
        if let Some(info) = pool_death_info(i) {
            eprintln!("  pool {}: {}", i, info);
        }
    }

    // Step 1: drain. Workers see RECOVERY_IN_PROGRESS at request entry
    // and bail. In-flight workers' socket calls to dying pools fail
    // with ECONNREFUSED; their per-eval arenas drop, releasing SHM.
    std::thread::sleep(RECOVERY_DRAIN);

    // Step 2: SIGTERM remaining live pools; brief grace; SIGKILL the
    // holdouts; reap.
    for i in 0..n_pools {
        let pgid = PGIDS[i].load(Ordering::Relaxed);
        if pgid > 0 && pool_is_alive(i) {
            unsafe { libc::kill(-pgid, libc::SIGTERM); }
        }
    }
    std::thread::sleep(RECOVERY_TERM_GRACE);
    for i in 0..n_pools {
        let pgid = PGIDS[i].load(Ordering::Relaxed);
        if pgid > 0 && pool_is_alive(i) {
            unsafe { libc::kill(-pgid, libc::SIGKILL); }
        }
    }
    while unsafe { libc::waitpid(-1, std::ptr::null_mut(), libc::WNOHANG) } > 0 {}

    // Step 3: tear down all SHM.
    unsafe {
        let mut err: *mut std::ffi::c_char = std::ptr::null_mut();
        shclose(&mut err);
        if !err.is_null() {
            libc::free(err as *mut libc::c_void);
        }
    }

    // Step 4: bump generation, compute new basename.
    let generation = RECOVERY_GENERATION.fetch_add(1, Ordering::AcqRel) + 1;

    // Step 5: rebuild syscmds with the new basename and respawn.
    let respawn_result: Result<(), String> = (|| {
        let mut guard = RECOVERY_CONTEXT.lock().unwrap();
        let ctx = guard
            .as_mut()
            .ok_or_else(|| "recovery context not installed".to_string())?;
        let new_basename = basename_for_generation(&ctx.base_basename, generation);

        // Rebuild Vec<PoolSocket> with fresh syscmd argv-tails.
        let new_sockets = setup_sockets(&ctx.pools, &ctx.tmpdir, &new_basename);
        ctx.sockets = new_sockets;

        // Bootstrap the daemon's allocator with the new basename. This
        // both sets COMMON_BASENAME and creates volume 0 ready for use.
        let basename_c = CString::new(new_basename.as_str()).unwrap();
        let mut err: *mut std::ffi::c_char = std::ptr::null_mut();
        let shm = unsafe { shinit(basename_c.as_ptr(), 0, 0xffff, &mut err) };
        if shm.is_null() {
            let msg = if !err.is_null() {
                let s = unsafe { std::ffi::CStr::from_ptr(err) }.to_string_lossy().into_owned();
                unsafe { libc::free(err as *mut libc::c_void) };
                s
            } else {
                "unknown shinit error".into()
            };
            return Err(format!("shinit({}) after recovery failed: {}", new_basename, msg));
        }
        // Re-bootstrap the stream registry under the fresh basename so
        // pools spawned by `start_daemons` below see the registry as
        // part of their session.
        extern "C" {
            fn stream_registry_init(errmsg: *mut *mut std::ffi::c_char) -> usize;
        }
        let slot_count = unsafe { stream_registry_init(&mut err) };
        if slot_count == usize::MAX {
            let msg = if !err.is_null() {
                let s = unsafe { std::ffi::CStr::from_ptr(err) }.to_string_lossy().into_owned();
                unsafe { libc::free(err as *mut libc::c_void) };
                s
            } else {
                "unknown stream_registry_init error".into()
            };
            return Err(format!(
                "stream_registry_init after recovery failed: {}", msg
            ));
        }
        record_shm_basename(&new_basename);

        // Spawn each pool fresh.
        let indices: Vec<usize> = (0..n_pools).collect();
        start_daemons(&mut ctx.sockets, &indices)
    })();

    match respawn_result {
        Ok(()) => {
            eprintln!(
                "morloc daemon: recovery complete (generation {})",
                generation
            );
        }
        Err(msg) => {
            eprintln!(
                "morloc daemon: recovery failed (generation {}): {}",
                generation, msg
            );
            // Fall through to end_recovery so the next poll cycle can
            // retry. The loop guard above counts every attempt
            // (including failed ones) and exits the daemon if too many
            // pile up in a short window -- that's the correct response
            // to a fundamentally broken pool that can't be respawned.
        }
    }

    // Always clear the gate, even on respawn failure: leaving
    // RECOVERY_IN_PROGRESS pinned would wedge the daemon forever
    // (every subsequent request returns "recovering", and
    // begin_recovery's compare_exchange would refuse to retry).
    unsafe { morloc_daemon_end_recovery() };
}

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

/// Start-times of pool processes, captured at spawn time from
/// `/proc/PID/stat` field 22. Paired with PIDs for the §1.7 PID
/// sweep. Zero entries (unset, or non-Linux fallback) cause the
/// sweeper to fall back to PID-only matching.
static POOL_START_TIMES: [std::sync::atomic::AtomicU64; MAX_DAEMONS] = {
    const INIT: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
    [INIT; MAX_DAEMONS]
};

/// Per-pool flag: has this pool's PID sweep already been enqueued?
/// Prevents duplicate sweeps after the same death is observed across
/// multiple dispatch cycles. Reset when a pool is respawned.
static POOL_SWEPT: [AtomicBool; MAX_DAEMONS] = {
    const INIT: AtomicBool = AtomicBool::new(false);
    [INIT; MAX_DAEMONS]
};

/// Re-entrancy guard for clean_exit.
static CLEANING_UP: AtomicBool = AtomicBool::new(false);

/// Set when we exit due to BrokenPipe on stdout. Read by `clean_exit` to
/// skip the stdout flush -- fd 1 is known dead, and the flush would
/// hit the same EPIPE again.
static BROKEN_PIPE: AtomicBool = AtomicBool::new(false);

/// Global tmpdir path (set once in main, read during cleanup).
static TMPDIR: std::sync::OnceLock<String> = std::sync::OnceLock::new();

/// Async-signal-safe view of the SHM basename prefix (`"<basename>_\0"`)
/// so `signal_exit_handler` can build `<basename>_<idx>` names without
/// allocating or locking. `Mutex`-guarded `COMMON_BASENAME` in shm.rs
/// is unusable from a signal handler. Overwrites leak the previous
/// `CString` -- recovery is rare and each string is ~64 B.
static SHM_BASENAME_PREFIX: std::sync::atomic::AtomicPtr<std::os::raw::c_char> =
    std::sync::atomic::AtomicPtr::new(std::ptr::null_mut());

/// Socket info for each pool.
///
/// Pool stderr and stdout are intentionally NOT captured or intercepted by
/// the nexus: a core morloc guarantee is that anything a sourced function
/// prints to stderr/stdout is passed through unchanged. Raised exceptions
/// are caught inside each pool's dispatch wrapper (see pool.py/pool.cpp/
/// pool.R/pool.jl) and returned as morloc error packets, which the nexus
/// then annotates with call-site context when bubbling them up.
#[derive(Clone)]
pub struct PoolSocket {
    pub lang: String,
    pub socket_path: String,
    pub syscmd: Vec<CString>,
    pub pid: i32,
    /// Process start-time of `pid`, read from `/proc/PID/stat` field
    /// 22 right after the daemon was spawned. Paired with `pid` when
    /// the nexus enqueues a PID sweep on pool death, so the sweeper
    /// rejects PID-reuse false positives. 0 if start_time couldn't
    /// be read (non-Linux fallback; sweep falls back to PID-only).
    pub pid_start_time: u64,
    /// XXH64 fingerprint (16-char hex) of this pool's emitted source +
    /// any declared @hash-include@ files. Exported to the spawned pool
    /// process as @MORLOC_POOL_HASH@ so the runtime cache key changes
    /// whenever source or external dependencies change. Empty for
    /// manifests that predate the field.
    pub pool_hash: CString,
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

/// SIGTERM/SIGINT handler: fast, async-signal-safe shutdown. Any Rust
/// std API would risk a re-entrant lock or a double-borrow if the main
/// thread was mid-`println!` at signal time. The run-log epilogue /
/// `summary.json` are the notable casualties; users who Ctrl-C don't
/// expect them. A second signal skips even the pool-kill loop.
extern "C" fn signal_exit_handler(sig: libc::c_int) {
    if CLEANING_UP.swap(true, Ordering::SeqCst) {
        unsafe { libc::_exit(128 + sig) };
    }
    for i in 0..MAX_DAEMONS {
        let pgid = PGIDS[i].load(Ordering::Relaxed);
        if pgid > 0 {
            unsafe { libc::kill(-pgid, libc::SIGKILL) };
        }
    }
    unsafe { sweep_shm_segments() };
    unsafe { libc::_exit(128 + sig) };
}

/// Sweep every `<basename>_<idx>` segment via `shm_unlink`. Called
/// from `signal_exit_handler`; must stay async-signal-safe (no
/// allocation, no locks). The full 0..MAX_VOLUME_NUMBER walk is
/// required because `pick_free_slot` (shm.rs) places new volumes at
/// random indices, so a smaller conservative bound would leak.
///
/// # Safety
/// Concurrent mutation of `SHM_BASENAME_PREFIX` is impossible: the
/// setter only runs from the main thread during startup and recovery,
/// both strictly before the failing path that delivers a signal.
unsafe fn sweep_shm_segments() {
    let prefix_ptr = SHM_BASENAME_PREFIX.load(Ordering::Acquire);
    if prefix_ptr.is_null() {
        return;
    }
    // strlen is async-signal-safe.
    let prefix_len = libc::strlen(prefix_ptr);

    // 96 B holds any realistic basename (~50 B) + a full u32's 10
    // decimal digits + NUL, with headroom.
    const BUF_LEN: usize = 96;
    let mut buf: [u8; BUF_LEN] = [0; BUF_LEN];
    if prefix_len + 11 > BUF_LEN {
        return;
    }
    std::ptr::copy_nonoverlapping(
        prefix_ptr as *const u8,
        buf.as_mut_ptr(),
        prefix_len,
    );

    for idx in 0..MAX_VOLUME_NUMBER {
        let digits_end = write_decimal(&mut buf, prefix_len, idx as u32);
        buf[digits_end] = 0;
        libc::shm_unlink(buf.as_ptr() as *const std::os::raw::c_char);
    }
}

/// Signal-safe unsigned-decimal writer. Divmod-and-reverse.
fn write_decimal(buf: &mut [u8], start: usize, mut val: u32) -> usize {
    if val == 0 {
        buf[start] = b'0';
        return start + 1;
    }
    let digits_start = start;
    let mut pos = start;
    while val > 0 {
        buf[pos] = b'0' + (val % 10) as u8;
        val /= 10;
        pos += 1;
    }
    let mut lo = digits_start;
    let mut hi = pos - 1;
    while lo < hi {
        buf.swap(lo, hi);
        lo += 1;
        hi -= 1;
    }
    pos
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

/// Publish the current basename with its trailing `_` so the signal
/// handler only has to append decimal digits and a NUL.
fn record_shm_basename(basename: &str) {
    let prefix = CString::new(format!("{}_", basename))
        .expect("shm basename must not contain NUL");
    SHM_BASENAME_PREFIX.store(prefix.into_raw(), Ordering::Release);
}

/// Initialize the per-process SHM segment: make the tmpdir, sweep
/// stale SHM volumes left by prior crashes, and call libmorloc's
/// `shinit`. Returns `(tmpdir, shm_basename)`. Exits the process via
/// [`clean_exit`] on any failure.
pub fn init_shm() -> (String, String) {
    let tmpdir = match make_tmpdir() {
        Ok(t) => t,
        Err(e) => {
            eprintln!("Error: {}", e);
            std::process::exit(1);
        }
    };
    set_tmpdir(tmpdir.clone());
    cleanup_stale_shm();

    let job_hash = make_job_hash(42);
    // PID is embedded so cleanup_stale_shm() can identify orphans
    // without reading SHM headers.
    let shm_basename = format!("morloc-{}-{:016x}", std::process::id(), job_hash);

    extern "C" {
        fn shm_set_fallback_dir(dir: *const std::ffi::c_char);
        fn shinit(
            shm_basename: *const std::ffi::c_char,
            volume_index: usize,
            shm_size: usize,
            errmsg: *mut *mut std::ffi::c_char,
        ) -> *mut std::ffi::c_void;
        fn stream_registry_init(errmsg: *mut *mut std::ffi::c_char) -> usize;
    }

    let tmpdir_c = std::ffi::CString::new(tmpdir.as_str()).unwrap();
    let basename_c = std::ffi::CString::new(shm_basename.as_str()).unwrap();
    let mut errmsg: *mut std::ffi::c_char = std::ptr::null_mut();
    unsafe {
        shm_set_fallback_dir(tmpdir_c.as_ptr());
        let shm = shinit(basename_c.as_ptr(), 0, 0xffff, &mut errmsg);
        if shm.is_null() {
            let msg = if !errmsg.is_null() {
                let s = std::ffi::CStr::from_ptr(errmsg).to_string_lossy().into_owned();
                libc::free(errmsg as *mut std::ffi::c_void);
                s
            } else {
                "unknown error".into()
            };
            eprintln!("Error: failed to initialize shared memory: {}", msg);
            clean_exit(1);
        }
        // After the main SHM allocation pool is up, bootstrap the
        // shared stream registry. This allocates a dedicated volume
        // (`STREAM_REGISTRY_VOLUME`) holding the slot table all pools
        // will share. Subsequent pool processes attach to the same
        // segment via `stream_registry_init`, which is idempotent.
        let slot_count = stream_registry_init(&mut errmsg);
        if slot_count == usize::MAX {
            let msg = if !errmsg.is_null() {
                let s = std::ffi::CStr::from_ptr(errmsg).to_string_lossy().into_owned();
                libc::free(errmsg as *mut std::ffi::c_void);
                s
            } else {
                "unknown error".into()
            };
            eprintln!("Error: failed to initialise stream registry: {}", msg);
            clean_exit(1);
        }
    }
    record_shm_basename(&shm_basename);
    (tmpdir, shm_basename)
}

/// Apply the `-o FILE` redirect by `dup2`ing a freshly-opened file
/// onto STDOUT_FILENO. No-op when `path` is `None`. Exits via
/// [`clean_exit`] on any open/dup2 failure. Used by both `run` and
/// `view` so the redirect covers every output writer uniformly
/// (Rust, libc printf, raw fd writes).
pub fn redirect_stdout_to(path: Option<&str>) {
    let Some(path) = path else { return };
    use std::os::unix::io::AsRawFd;
    match std::fs::File::create(path) {
        Ok(f) => {
            let rc = unsafe { libc::dup2(f.as_raw_fd(), libc::STDOUT_FILENO) };
            if rc == -1 {
                let err = std::io::Error::last_os_error();
                eprintln!("Error: cannot redirect stdout to '{}': {}", path, err);
                clean_exit(1);
            }
            // Drop the File handle so the original fd is closed; the
            // dup2'd fd 1 keeps the file alive.
            drop(f);
        }
        Err(e) => {
            eprintln!("Error: cannot open output file '{}': {}", path, e);
            clean_exit(1);
        }
    }
}

/// Convert a libmorloc-allocated C error string into an owned Rust
/// `String` and `libc::free` the source pointer. Returns `None` if
/// the pointer is null. Centralizes the FFI errmsg-consume pattern
/// every callsite ends up needing.
pub fn take_c_errmsg(p: *mut std::ffi::c_char) -> Option<String> {
    if p.is_null() {
        return None;
    }
    let owned = unsafe { std::ffi::CStr::from_ptr(p) }
        .to_string_lossy()
        .into_owned();
    unsafe { libc::free(p as *mut std::ffi::c_void) };
    Some(owned)
}

/// Convert a filesystem path to a `CString` for FFI. Distinguishes the
/// non-UTF-8 case (rare on Linux, but legal) from the NUL-embedded case
/// so callers can produce meaningful error messages instead of the
/// current `unwrap_or("")` sink that surfaces as "cannot open ''".
pub fn path_to_cstring(p: &Path) -> Result<CString, String> {
    let s = p
        .as_os_str()
        .to_str()
        .ok_or_else(|| format!("path '{}' is not valid UTF-8", p.display()))?;
    CString::new(s).map_err(|_| format!("path '{}' contains NUL", p.display()))
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

    // Flush stdout. Critical when -o redirected fd 1 to a file: Rust
    // and libc both buffer when stdout is not a TTY, and std::process::exit
    // skips destructors so any unflushed bytes would be lost.
    //
    // Skip when the pipe is already broken -- the flush would return
    // EPIPE and (worse, in some libc configs) block trying to write
    // buffered bytes into a dead pipe.
    if !BROKEN_PIPE.load(Ordering::Relaxed) {
        use std::io::Write;
        let _ = std::io::stdout().lock().flush();
        unsafe { libc::fflush(std::ptr::null_mut()) };
    }

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

    // Render and emit the run-scope epilogue BEFORE finalize writes
    // summary.json. The epilogue line lands on stderr (and the rundir
    // tee, when active) so a user grep'ing for "FAILED" sees the
    // result independent of the structured summary. emit_epilogue is
    // idempotent: if some earlier path already emitted, this call is
    // a no-op.
    crate::runlog::emit_epilogue(exit_code);

    // Write summary.json (when --log-dir / --summary opts in) and drop
    // any tee handles still held by the nexus. Pool-side handles were
    // closed when those processes died above; this only touches state
    // owned by the nexus itself.
    extern "C" {
        fn morloc_run_finalize(exit_code: i32);
    }
    unsafe { morloc_run_finalize(exit_code) };

    std::process::exit(exit_code);
}

/// Downstream pipe closed while we were emitting output (e.g., `mtrim
/// | head -20`). Exit fast with the conventional SIGPIPE status (141)
/// after tearing down pools and shared memory. Unlike a raw SIGPIPE,
/// which would kill the nexus without any cleanup, this preserves the
/// full teardown -- pool subprocesses, SHM segments, run log summary.
///
/// The BROKEN_PIPE flag it sets tells `clean_exit` to skip the stdout
/// flush that would otherwise attempt to write into the dead pipe.
pub fn exit_broken_pipe() -> ! {
    BROKEN_PIPE.store(true, Ordering::SeqCst);
    clean_exit(141);
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
                pid_start_time: 0,
                pool_hash: CString::new(pool.pool_hash.as_str())
                    .unwrap_or_else(|_| CString::new("").unwrap()),
            }
        })
        .collect()
}

/// Fork and exec a language pool daemon. Returns child PID.
///
/// The child inherits the nexus's fd 0 / 1 / 2 unchanged. Pool
/// `sys.stdin.read()`, `std::cout << ...`, `print(...)`, `cat(...)`
/// etc. reach the user's terminal byte-for-byte. Morloc does not
/// silently redirect any of the three standard streams.
///
/// A program that opens `@stdin` / `@stdout` / `@stderr` and also
/// touches fd 0 / 1 / 2 from sourced code shares those fds with the
/// nexus's RPC handler. The resulting stream on the wire may be
/// corrupted -- the runtime cannot police fd sharing across pools
/// and the nexus. Documented, not enforced.
fn start_language_server(socket: &PoolSocket) -> Result<i32, String> {
    let pid = unsafe { libc::fork() };

    if pid == 0 {
        // Child process
        unsafe { libc::setpgid(0, 0) };

        // Export this pool's source-fingerprint hash so the runtime
        // cache wrap can mix it into every cache key. The env var is
        // per-pool because each language emits its own pool source and
        // therefore has its own hash; setting it in the child between
        // fork and exec keeps the values isolated.
        unsafe {
            let key = b"MORLOC_POOL_HASH\0".as_ptr() as *const libc::c_char;
            libc::setenv(key, socket.pool_hash.as_ptr(), 1);
        }

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
    extern "C" {
        fn stream_pid_start_time(pid: u32) -> u64;
    }
    for &idx in indices {
        let pid = start_language_server(&sockets[idx])?;
        sockets[idx].pid = pid;
        // Capture the pool's start_time at spawn so the PID-based crash
        // sweep can detect PID reuse correctly. Best-effort: reads
        // /proc/PID/stat. Zero on non-Linux or if the read races a fast
        // exit; sweep falls back to PID-only match in that case.
        let start_time = unsafe { stream_pid_start_time(pid as u32) };
        sockets[idx].pid_start_time = start_time;
        POOL_START_TIMES[idx].store(start_time, Ordering::Release);
        // Fresh incarnation: clear the sweep-done flag so the next
        // death of this pool is detected.
        POOL_SWEPT[idx].store(false, Ordering::Relaxed);
        PIDS[idx].store(pid, Ordering::Relaxed);
        PGIDS[idx].store(pid, Ordering::Relaxed);
    }

    // Wait for each daemon to respond to pings
    for &idx in indices {
        wait_for_daemon(&sockets[idx], idx)?;
    }

    Ok(())
}

/// Walk the per-index PIDs/PGIDs/start-times arrays and enqueue a
/// PID sweep for any pool that has died (`PIDS[i] == -1`) but hasn't
/// been swept yet. Idempotent per pool index until the pool is
/// respawned (which clears `POOL_SWEPT[i]`).
///
/// Called by the daemon poll-cycle hook (alongside
/// `pool_check_and_recover`) so pool clean-exits between dispatches
/// release their slots promptly. The crash-recovery path tears down
/// the SHM registry so any racy sweep enqueue against the new
/// registry is harmless (no slots will match).
///
/// Walks the global `PIDS` / `POOL_START_TIMES` / `POOL_SWEPT`
/// statics rather than a `&[PoolSocket]` so it can be called from
/// any context (including the C-ABI callback from libmorloc.so).
pub fn sweep_dead_pools(n_pools: usize) {
    extern "C" {
        fn stream_sweep_pid(pid: u32, start_time: u64);
    }
    let limit = n_pools.min(MAX_DAEMONS);
    for i in 0..limit {
        if POOL_SWEPT[i].load(Ordering::Relaxed) { continue; }
        let pid = PIDS[i].load(Ordering::Relaxed);
        if pid != -1 { continue; }
        // The PID stored before SIGCHLD cleared PIDS lives in
        // PGIDS (process-group same as pid for the initial fork).
        // We saved that on spawn and never clear it.
        let dead_pid = PGIDS[i].load(Ordering::Relaxed);
        if dead_pid <= 0 { continue; }
        let start_time = POOL_START_TIMES[i].load(Ordering::Acquire);
        if POOL_SWEPT[i]
            .compare_exchange(false, true, Ordering::AcqRel, Ordering::Relaxed)
            .is_err()
        {
            continue;
        }
        unsafe { stream_sweep_pid(dead_pid as u32, start_time) };
    }
}

/// Ping a daemon with exponential backoff until it responds.
/// Matches the C nexus behavior: initial delay 1ms, multiplier 1.25,
/// plus socket timeout that doubles from 10ms to ~10s.
fn wait_for_daemon(socket: &PoolSocket, pool_index: usize) -> Result<(), String> {
    use morloc_runtime_types::packet::PacketHeader;
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
    use morloc_runtime_types::hash::xxh64;

    let pid = std::process::id() as u64;
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default();
    let epoch_ns = now.as_nanos() as u64;

    let data = format!("{}:{}:{}", pid, epoch_ns, seed);
    xxh64(data.as_bytes())
}

/// Sweep /dev/shm for orphaned morloc segments. Two detection strategies:
///
/// 1. New-format basenames `morloc-<pid>-<...>`: parse the PID, check
///    /proc/<pid>; if absent, the creator is gone -- unlink.
/// 2. Old-format basenames `morloc-<hash>` (no embedded PID, predates
///    PR-21) and any segment whose PID parse fails: scan /proc/*/maps
///    once and check whether any live process has the segment mapped.
///    If not mapped anywhere, unlink.
///
/// Called once at nexus startup. Concurrency: the sweep is safe even if
/// other morloc processes are running -- a live PID's segments are
/// skipped, and a segment mapped by any live process is skipped. Live
/// segments owned by other users are skipped silently because shm_unlink
/// returns EACCES; we ignore unlink failures.
pub fn cleanup_stale_shm() {
    let dir = match std::fs::read_dir("/dev/shm") {
        Ok(d) => d,
        Err(_) => return, // /dev/shm not present (e.g. macOS); nothing to do
    };

    // Collect candidate morloc-* names first so we don't read /proc twice.
    let mut candidates: Vec<String> = Vec::new();
    for entry in dir.flatten() {
        let name = match entry.file_name().into_string() {
            Ok(s) => s,
            Err(_) => continue,
        };
        if name.starts_with("morloc-") {
            candidates.push(name);
        }
    }
    if candidates.is_empty() {
        return;
    }

    // First pass: PID-in-name strategy. Move fallback candidates to a
    // bucket for the /proc/maps strategy.
    let mut needs_map_scan: Vec<String> = Vec::new();
    for name in candidates {
        let body = name.strip_prefix("morloc-").unwrap();
        let pid_str = body.split(['-', '_']).next().unwrap_or("");
        let pid_parsed: Option<u32> = pid_str.parse().ok();
        match pid_parsed {
            Some(pid) if std::path::Path::new(&format!("/proc/{}", pid)).exists() => {
                continue; // creator alive; leave alone
            }
            Some(_) => unlink_shm(&name), // PID dead -> orphan
            None => needs_map_scan.push(name), // old format; verify via /proc/*/maps
        }
    }

    if needs_map_scan.is_empty() {
        return;
    }

    // Second pass: build a set of every /dev/shm path mentioned in any
    // live process's memory map, then unlink any candidate not in it.
    let mapped: std::collections::HashSet<String> = collect_mapped_shm_paths();
    for name in needs_map_scan {
        let full = format!("/dev/shm/{}", name);
        if !mapped.contains(&full) {
            unlink_shm(&name);
        }
    }
}

/// Collect every `/dev/shm/...` path mentioned in any live process's
/// `/proc/<pid>/maps`. Used as a fallback liveness check for SHM segments
/// whose names don't embed a PID.
fn collect_mapped_shm_paths() -> std::collections::HashSet<String> {
    let mut out = std::collections::HashSet::new();
    let proc = match std::fs::read_dir("/proc") {
        Ok(d) => d,
        Err(_) => return out,
    };
    for entry in proc.flatten() {
        let pid_name = match entry.file_name().into_string() {
            Ok(s) => s,
            Err(_) => continue,
        };
        if !pid_name.chars().all(|c| c.is_ascii_digit()) {
            continue;
        }
        let maps_path = format!("/proc/{}/maps", pid_name);
        let contents = match std::fs::read_to_string(&maps_path) {
            Ok(s) => s,
            Err(_) => continue, // process gone or no permission
        };
        for line in contents.lines() {
            // /proc/<pid>/maps lines: "<addr> <perms> <off> <dev> <ino> <path>"
            // We only care about the trailing path field.
            if let Some(idx) = line.find("/dev/shm/") {
                let path: String = line[idx..]
                    .trim_end_matches([' ', '\t', '\n'])
                    .to_string();
                out.insert(path);
            }
        }
    }
    out
}

fn unlink_shm(name: &str) {
    // Best-effort: failures (EACCES from foreign user, ENOENT from race,
    // etc.) are silent.
    unsafe {
        let cstr = match std::ffi::CString::new(format!("/{}", name)) {
            Ok(c) => c,
            Err(_) => return,
        };
        libc::shm_unlink(cstr.as_ptr());
    }
}

/// Become a subreaper so orphaned grandchildren get reparented to us.
/// Only available on Linux; no-op on other platforms.
pub fn set_child_subreaper() {
    #[cfg(target_os = "linux")]
    unsafe {
        libc::prctl(libc::PR_SET_CHILD_SUBREAPER, 1, 0, 0, 0);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn digits(val: u32) -> String {
        let mut buf = [0u8; 32];
        let end = write_decimal(&mut buf, 0, val);
        std::str::from_utf8(&buf[..end]).unwrap().to_string()
    }

    #[test]
    fn write_decimal_zero() {
        assert_eq!(digits(0), "0");
    }

    #[test]
    fn write_decimal_single_digit() {
        assert_eq!(digits(7), "7");
    }

    #[test]
    fn write_decimal_multi_digit() {
        assert_eq!(digits(42), "42");
        assert_eq!(digits(1234), "1234");
        // Locks the sweep's per-index name to fit in the 96-byte buffer.
        assert_eq!(MAX_VOLUME_NUMBER, 32768);
        assert_eq!(digits((MAX_VOLUME_NUMBER - 1) as u32), "32767");
    }

    #[test]
    fn write_decimal_appends_after_prefix() {
        let mut buf = [0u8; 64];
        let prefix = b"morloc-1234_";
        buf[..prefix.len()].copy_from_slice(prefix);
        let end = write_decimal(&mut buf, prefix.len(), 42);
        assert_eq!(&buf[..end], b"morloc-1234_42");
    }
}
