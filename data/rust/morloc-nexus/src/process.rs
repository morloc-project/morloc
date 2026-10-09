//! Pool daemon process management, signal handling, and lifecycle.
//!
//! Replaces the fork/exec, SIGCHLD, SIGTERM, clean_exit logic from nexus.c.

use std::ffi::CString;
use std::path::Path;
use std::sync::atomic::{AtomicBool, AtomicI32, AtomicU64, Ordering};
use std::time::Duration;

use morloc_runtime_types::process as proc_info;
use morloc_runtime_types::shm_types::MAX_VOLUME_NUMBER;

use crate::manifest::Pool;

pub const MAX_DAEMONS: usize = 32;

/// Pool-crash recovery generation counter. Starts at 0 (initial daemon
/// startup). Each successful coordinated recovery (kill all pools, drop
/// SHM, respawn) increments this. The current generation is encoded into
/// the SHM basename's 4-hex generation field so volumes from a prior
/// generation can never be confused for current-generation volumes -- if
/// a generation 0 file somehow survives the recovery teardown,
/// generation 1's `/mlc-<pid>-<hash>-0001-<vol>` will not collide. The
/// final exit removes every segment its run directory's markers name,
/// whatever the generation.
pub static RECOVERY_GENERATION: AtomicU64 = AtomicU64::new(0);

/// Compute the SHM basename for a given recovery generation. `base` is
/// the generation-0 basename `/mlc-<pid>-<hash>-0000`; this replaces its
/// trailing 4-hex generation field. Generation 0 reproduces `base`,
/// keeping the fixed four-field shape (pid, hash, gen, vol) so parsing is
/// positional. (The name is ASCII, so byte-slicing off the last field is
/// safe.)
pub fn basename_for_generation(base: &str, generation: u64) -> String {
    // `base` is always a gen-0 basename ending in the 4-hex generation field,
    // so it is at least 4 bytes; saturating_sub keeps a malformed short input
    // from underflow-panicking (it would just drop the whole string).
    debug_assert!(base.len() >= 4, "shm basename too short: {:?}", base);
    // The generation field is 4 hex digits; beyond 0xffff the name would grow
    // past macOS PSHMNAMLEN(31) and shm_open falls back to file-backed. The
    // daemon's recovery loop-guard bounds generations far below this.
    debug_assert!(
        generation <= 0xffff,
        "recovery generation overflows the 4-hex field: {}",
        generation
    );
    let core = &base[..base.len().saturating_sub(4)];
    format!("{}{:04x}", core, generation)
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
const RECOVERY_DRAIN_LIMIT: Duration = Duration::from_secs(60);
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
    fn morloc_daemon_wait_for_requests(timeout_ms: u64) -> bool;
    // Hands a reaped child's exit status to whoever forked it. The daemon
    // forks the compiler to serve an expression and then waits for it; the
    // drains below would otherwise consume the status first and leave that
    // wait with nothing to read.
    fn morloc_note_child_exit(pid: libc::c_int, status: libc::c_int);
    fn morloc_reaped_sequence() -> u64;
    fn morloc_take_noted_child_exit(pid: libc::c_int, since: u64, status: *mut libc::c_int) -> libc::c_int;
    fn morloc_stop_child_groups();
    fn morloc_child_group_leader_exited(pid: libc::c_int);
    fn morloc_refuse_new_segments();
    fn morloc_claim_exit() -> bool;
    fn morloc_daemon_remove_endpoints();
    fn morloc_remove_leases();
    fn morloc_reclaim_all();
}

/// C-ABI callback wired into DaemonConfig.pool_check_fn.
///
/// Invoked once per daemon main-loop iteration (~1 s cadence).
/// Detects dead pool processes via `pool_is_alive`. If any are dead,
/// runs the coordinated recovery sequence:
/// 1. Close the request gate: no new request is admitted.
/// 2. SIGTERM -> SIGKILL all remaining live pools; reap each.
/// 3. Wait for every admitted request to finish; exit if they do not.
/// 4. Drop the entire SHM namespace (shclose / reset_all).
/// 5. Bump RECOVERY_GENERATION, compute new basename.
/// 6. Re-shinit and respawn every pool with the new basename.
/// 7. Wait for ping responses.
/// 8. Clear `RECOVERY_IN_PROGRESS` so workers resume serving.
///
/// Loop guard: if more than RECOVERY_MAX_ATTEMPTS recoveries fire
/// within RECOVERY_WINDOW, the daemon exits with a fatal error.
static POOL_CHECK: std::sync::Mutex<()> = std::sync::Mutex::new(());

extern "C" fn pool_check_and_recover(
    _sockets: *mut morloc_runtime_types::daemon_socket::MorlocSocket,
    n_pools: usize,
) {
    morloc_runtime_types::panic::outside_scope(|| check_and_recover(n_pools))
}

fn check_and_recover(n_pools: usize) {
    let _checking = match POOL_CHECK.try_lock() {
        Ok(g) => g,
        // PANIC-4
        Err(std::sync::TryLockError::Poisoned(_)) => panic!("the pool recovery lock is poisoned"),
        Err(std::sync::TryLockError::WouldBlock) => return,
    };
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
                clean_exit(1);
            }
        }
    }

    eprintln!("morloc daemon: pool crash detected; coordinated recovery starting");
    for i in 0..n_pools {
        if let Some(info) = pool_death_info(i) {
            eprintln!("  pool {}: {}", i, info);
        }
    }

    // Step 2: SIGTERM remaining live pools; brief grace; SIGKILL the
    // holdouts; reap.
    for i in 0..n_pools {
        if pool_is_alive(i) {
            signal_pool_group(i, libc::SIGTERM);
        }
    }
    std::thread::sleep(RECOVERY_TERM_GRACE);
    for i in 0..n_pools {
        signal_pool_group(i, libc::SIGKILL);
        release_pool_group(i);
    }
    reap_noting();

    if !unsafe { morloc_daemon_wait_for_requests(RECOVERY_DRAIN_LIMIT.as_millis() as u64) } {
        eprintln!(
            "morloc daemon: requests still running {:?} into pool crash recovery; \
             exiting rather than unmapping shared memory they may be reading",
            RECOVERY_DRAIN_LIMIT
        );
        exit_leaving_threads(1);
    }
    // DAEMON-6: a shutdown requested during the drain needs no new pools.
    if unsafe { morloc_daemon_is_shutting_down() } {
        unsafe { morloc_daemon_end_recovery() };
        return;
    }

    // Step 3: tear down all SHM.
    // FORK-16: the killed pools' temp directories; FORK-15: the leases name
    // the namespace being discarded.
    unsafe { morloc_reclaim_all() };
    unsafe { morloc_remove_leases() };
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
        // both sets COMMON_BASENAME and creates the primary volume.
        let basename_c = CString::new(new_basename.as_str()).unwrap();
        let mut err: *mut std::ffi::c_char = std::ptr::null_mut();
        let shm = unsafe { shinit(basename_c.as_ptr(), morloc_runtime_types::shm_types::PRIMARY_VOLUME, 0xffff, &mut err) };
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

/// Synchronous pool-crash check + recovery for the MCP server loop.
///
/// The MCP loop has no periodic poll cycle (unlike `daemon_run`), so it calls
/// this after a dispatch fails with an INTERNAL error: if the failure was a
/// pool process dying, [`pool_check_and_recover`] tears down and respawns every
/// pool (a fast per-pool `pool_is_alive` no-op when nothing died). Requires
/// [`install_recovery_context`] to have run. The `sockets` argument is unused
/// by the recovery routine (it walks the global PID tables), so a null pointer
/// is passed.
pub fn mcp_recover_pools(n_pools: usize) {
    pool_check_and_recover(std::ptr::null_mut(), n_pools);
}

// SHM-8: a pool whose worker died ends within its coordinator's next reap.
pub fn mcp_recover_pools_after_failure(n_pools: usize) {
    let until = std::time::Instant::now() + POOL_END_WAIT;
    while (0..n_pools).all(pool_is_alive) && std::time::Instant::now() < until {
        std::thread::sleep(Duration::from_millis(5));
    }
    mcp_recover_pools(n_pools);
}

const POOL_END_WAIT: Duration = Duration::from_millis(200);

const INITIAL_PING_TIMEOUT: Duration = Duration::from_millis(10);
const INITIAL_RETRY_DELAY: Duration = Duration::from_millis(1);
const RETRY_MULTIPLIER: f64 = 1.25;
const MAX_RETRIES: usize = 16;
// DAEMON-6
const PING_REPLY_LIMIT: Duration = Duration::from_secs(5);

// ── Global state for signal handlers ───────────────────────────────────────

/// PIDs of spawned pool daemons. 0 = unused, -1 = reaped.
static PIDS: [AtomicI32; MAX_DAEMONS] = {
    const INIT: AtomicI32 = AtomicI32::new(0);
    [INIT; MAX_DAEMONS]
};

// DAEMON-14: written before the pool's pid is published in PIDS.
static POOL_PGIDS: [AtomicI32; MAX_DAEMONS] = {
    const INIT: AtomicI32 = AtomicI32::new(0);
    [INIT; MAX_DAEMONS]
};

// DAEMON-14
fn kill_pool_group(i: usize) {
    let pgid = POOL_PGIDS[i].load(Ordering::SeqCst);
    if pgid > 0 {
        POOL_GROUPS.kill_group(pgid);
    }
}

/// The name a pool's pin runs under.
pub const POOL_PIN_NAME: &str = "morloc-pool-pin";

// DAEMON-11: each pool's process group, held by a pin until released.
static POOL_GROUPS: morloc_runtime_types::child_group::ChildGroups =
    morloc_runtime_types::child_group::ChildGroups::new();

struct PoolPin {
    group: morloc_runtime_types::child_group::Registered<'static>,
    _pin: std::os::fd::OwnedFd,
}

static POOL_PINS: std::sync::Mutex<[Option<PoolPin>; MAX_DAEMONS]> =
    std::sync::Mutex::new([const { None }; MAX_DAEMONS]);

/// The pid each pool started with, kept after its exit for the crash sweep.
static SPAWNED_PIDS: [AtomicI32; MAX_DAEMONS] = {
    const INIT: AtomicI32 = AtomicI32::new(0);
    [INIT; MAX_DAEMONS]
};

fn pool_pins() -> std::sync::MutexGuard<'static, [Option<PoolPin>; MAX_DAEMONS]> {
    // PANIC-4
    POOL_PINS.lock().unwrap_or_else(|_| morloc_runtime_types::panic::poisoned_lock())
}

// DAEMON-11
fn signal_pool_group(i: usize, sig: libc::c_int) {
    if let Some(p) = pool_pins()[i].as_ref() {
        p.group.signal(sig);
    }
}

// DAEMON-11
fn release_pool_group(i: usize) {
    let released = pool_pins()[i].take();
    drop(released);
}

/// Exit statuses saved by SIGCHLD handler.
static EXIT_STATUSES: [AtomicI32; MAX_DAEMONS] = {
    const INIT: AtomicI32 = AtomicI32::new(0);
    [INIT; MAX_DAEMONS]
};

/// Start stamps of pool processes, captured at spawn. Paired with PIDs
/// for the §1.7 PID sweep. Zero entries (unset or unreadable) cause the
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

/// Per-index pool language label, captured at spawn. Used only for the
/// post-mortem `report_dead_pools` line so a failed run names WHICH pool
/// died (e.g. "pool 2 [cpp]"). Indexed by the global pool index.
static POOL_LANGS: std::sync::Mutex<Vec<String>> = std::sync::Mutex::new(Vec::new());

/// Re-entrancy guard for clean_exit.
static CLEANING_UP: AtomicBool = AtomicBool::new(false);

/// Exit status chosen by the thread that owns the teardown. Read only by a
/// thread parked in `park_until_exit` whose owner never reached `exit`.
static EXIT_CODE: AtomicI32 = AtomicI32::new(0);

/// Upper bound on how long a parked thread waits for the teardown owner to
/// call `exit`. `clean_exit` is itself bounded (about 300 ms to stop the
/// pools), so reaching this means the owner is wedged;
/// staying alive with no thread making progress is worse than exiting with
/// the status the owner already chose.
const PARK_LIMIT: Duration = Duration::from_secs(60);

/// Set when we exit due to BrokenPipe on stdout. Read by `clean_exit` to
/// skip the stdout flush -- fd 1 is known dead, and the flush would
/// hit the same EPIPE again.
static BROKEN_PIPE: AtomicBool = AtomicBool::new(false);

/// Async-signal-safe view of the SHM basename prefix (`"<basename>-\0"`)
/// so `signal_exit_handler` can build `<basename>-<idx>` names without
/// allocating or locking. `Mutex`-guarded `COMMON_BASENAME` in shm.rs
/// is unusable from a signal handler. Overwrites leak the previous
/// `CString` -- recovery is rare and each string is ~64 B.
static SHM_BASENAME_PREFIX: std::sync::atomic::AtomicPtr<std::os::raw::c_char> =
    std::sync::atomic::AtomicPtr::new(std::ptr::null_mut());

// Companion-segment SHM name list lives in `libmorloc.so`
// (`morloc-runtime::shm_companion`) so the runtime code that opens
// segments can register into it directly. The nexus reads it here
// via `extern "C"` linkage and iterates it in `sweep_shm_segments`.
extern "C" {
    static MORLOC_COMPANION_NAMES:
        [std::sync::atomic::AtomicPtr<std::os::raw::c_char>; 16];
}

/// Socket info for each pool.
///
/// Pool stderr and stdout are intentionally NOT captured or intercepted by
/// the nexus: a core morloc guarantee is that anything a sourced function
/// prints to stderr/stdout is passed through unchanged. Raised exceptions
/// are caught inside each pool's dispatch wrapper (see pool.py/pool.cpp/
/// pool.R) and returned as morloc error packets, which the nexus
/// then annotates with call-site context when bubbling them up.
#[derive(Clone)]
pub struct PoolSocket {
    pub lang: String,
    pub socket_path: String,
    pub syscmd: Vec<CString>,
    pub pid: i32,
    /// Process start stamp of `pid`, read right after the daemon was
    /// spawned. Paired with `pid` when the nexus enqueues a PID sweep on
    /// pool death, so the sweeper rejects PID-reuse false positives. 0 if
    /// it couldn't be read (the sweep then falls back to PID-only).
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
    // PANIC-1
    morloc_runtime_types::panic::signal_frame(|| {
        #[cfg(target_os = "linux")]
        let saved_errno = unsafe { *libc::__errno_location() };
        #[cfg(target_os = "macos")]
        let saved_errno = unsafe { *libc::__error() };
        reap_from_handler();
        #[cfg(target_os = "linux")]
        unsafe { *libc::__errno_location() = saved_errno };
        #[cfg(target_os = "macos")]
        unsafe { *libc::__error() = saved_errno };
    })
}

/// SIGTERM/SIGINT handler: fast, async-signal-safe shutdown. Any Rust
/// std API would risk a re-entrant lock or a double-borrow if the main
/// thread was mid-`println!` at signal time. The run-log epilogue /
/// `summary.json` are the notable casualties; users who Ctrl-C don't
/// expect them. A second signal skips even the pool-kill loop.
extern "C" fn signal_exit_handler(sig: libc::c_int) {
    // PANIC-1
    morloc_runtime_types::panic::signal_frame(|| {
        if !CLEANING_UP.swap(true, Ordering::SeqCst) {
            stop_everything();
        }
        unsafe { libc::_exit(128 + sig) };
    })
}

// PANIC-1
pub fn panic_exit() -> ! {
    CLEANING_UP.store(true, Ordering::SeqCst);
    stop_everything();
    unsafe { libc::_exit(morloc_runtime_types::panic::PANIC_EXIT_STATUS) };
}

static NEXUS_GENERATION: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(u64::MAX);

extern "C" {
    fn morloc_fork_generation() -> u64;
}

// PANIC-1: also binds the symbol before any signal handler needs it.
pub fn record_nexus_process() {
    NEXUS_GENERATION.store(unsafe { morloc_fork_generation() }, Ordering::SeqCst);
}

// FORK-11: the environment is written only while the process has one thread;
// a count that cannot be read allows it.
fn alone() {
    extern "C" {
        fn morloc_thread_count() -> libc::c_long;
    }
    let n = unsafe { morloc_thread_count() };
    if n > 1 {
        morloc_runtime_types::panic::fatal(&format!("the environment was written with {n} threads running"));
    }
}

pub(crate) fn set_startup_env(key: &str, value: impl AsRef<std::ffi::OsStr>) {
    alone();
    std::env::set_var(key, value);
}

pub(crate) fn remove_startup_env(key: &str) {
    alone();
    std::env::remove_var(key);
}

// DAEMON-6
fn stop_everything() {
    // PANIC-1: a forked child before exec owns none of these.
    if unsafe { morloc_fork_generation() } != NEXUS_GENERATION.load(Ordering::SeqCst) {
        return;
    }
    POOL_GROUPS.stop_all();
    unsafe {
        // DAEMON-10
        morloc_daemon_remove_endpoints();
        morloc_stop_child_groups();
        crate::stop_frontend_children();
        sweep_shm_segments();
        crate::sigrm::remove_registered();
    }
}

/// Crash handler for fatal program-error signals (SIGSEGV / SIGABRT /
/// SIGBUS / SIGFPE). Unlinks the SHM segments this nexus owns, then
/// re-raises so the default disposition (core dump / termination)
/// still fires and the exit status is preserved. Registered with
/// `SA_RESETHAND`, so the handler runs once and the signal's default
/// takes over on the re-raise.
///
/// Must stay async-signal-safe: it calls only `sweep_shm_segments`
/// (shm_unlink loop over a fixed buffer, no locks/alloc) and
/// `raise`. It deliberately does NOT run `clean_exit` or the
/// pool-kill loop -- those are not async-signal-safe. Orphaned pools
/// are reaped by the nexus poll's dead-pool sweep on the next run.
extern "C" fn crash_cleanup_handler(sig: libc::c_int) {
    // PANIC-1
    morloc_runtime_types::panic::signal_frame(|| {
        unsafe {
            sweep_shm_segments();
            libc::raise(sig);
        }
    })
}

/// Sweep every `<basename>-<idx>` segment via `shm_unlink`. Called
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

    // 96 B holds any realistic basename (~50 B) + the 4-hex volume index
    // + NUL, with headroom.
    const BUF_LEN: usize = 96;
    let mut buf: [u8; BUF_LEN] = [0; BUF_LEN];
    if prefix_len + 5 > BUF_LEN {
        return;
    }
    std::ptr::copy_nonoverlapping(
        prefix_ptr as *const u8,
        buf.as_mut_ptr(),
        prefix_len,
    );

    for idx in 0..MAX_VOLUME_NUMBER {
        let digits_end = write_hex4(&mut buf, prefix_len, idx as u16);
        buf[digits_end] = 0;
        libc::shm_unlink(buf.as_ptr() as *const std::os::raw::c_char);
    }

    // Companion segments (stream registry etc.) live outside the
    // 0..MAX_VOLUME_NUMBER allocator namespace. `libmorloc.so`
    // populates `MORLOC_COMPANION_NAMES` as each companion opens.
    for slot in MORLOC_COMPANION_NAMES.iter() {
        let p = slot.load(Ordering::Acquire);
        if !p.is_null() {
            libc::shm_unlink(p);
        }
    }
}

/// Signal-safe fixed-width (4-digit, zero-padded) lowercase-hex writer.
/// Volume indices are < MAX_VOLUME_NUMBER (32768 = 0x8000), so 4 hex
/// digits are exact. Returns the end offset (`start + 4`).
fn write_hex4(buf: &mut [u8], start: usize, val: u16) -> usize {
    const HEX: &[u8; 16] = b"0123456789abcdef";
    buf[start] = HEX[((val >> 12) & 0xf) as usize];
    buf[start + 1] = HEX[((val >> 8) & 0xf) as usize];
    buf[start + 2] = HEX[((val >> 4) & 0xf) as usize];
    buf[start + 3] = HEX[(val & 0xf) as usize];
    start + 4
}

/// Install signal handlers.
pub fn install_signal_handlers() {
    unsafe {
        // Bind the cross-library symbol the SIGCHLD handler calls before the
        // handler can run. A first call from inside a signal handler would
        // resolve it through the dynamic loader, whose lock the interrupted
        // thread may already hold. A non-positive pid records nothing.
        morloc_note_child_exit(-1, 0);

        // SIGCHLD
        let mut sa: libc::sigaction = std::mem::zeroed();
        sa.sa_sigaction = sigchld_handler as *const () as usize;
        libc::sigemptyset(&mut sa.sa_mask);
        sa.sa_flags = libc::SA_RESTART | libc::SA_NOCLDSTOP;
        libc::sigaction(libc::SIGCHLD, &sa, std::ptr::null_mut());

        // SIGTERM, SIGINT and SIGHUP
        let mut sa_exit: libc::sigaction = std::mem::zeroed();
        sa_exit.sa_sigaction = signal_exit_handler as *const () as usize;
        libc::sigemptyset(&mut sa_exit.sa_mask);
        sa_exit.sa_flags = 0;
        libc::sigaction(libc::SIGTERM, &sa_exit, std::ptr::null_mut());
        libc::sigaction(libc::SIGINT, &sa_exit, std::ptr::null_mut());
        // A closed terminal or dropped ssh session ends the run as cleanly as
        // SIGTERM does, unless the run was started with SIGHUP ignored
        // (nohup), which it keeps.
        let mut hup: libc::sigaction = std::mem::zeroed();
        libc::sigaction(libc::SIGHUP, std::ptr::null(), &mut hup);
        if hup.sa_sigaction != libc::SIG_IGN {
            libc::sigaction(libc::SIGHUP, &sa_exit, std::ptr::null_mut());
        }

        // Fatal program-error signals: unlink SHM segments, then
        // re-raise for the default disposition. SA_RESETHAND ensures
        // the re-raise hits the default handler (core dump / kill)
        // rather than recursing. SIGKILL is intentionally absent --
        // it cannot be caught.
        let mut sa_crash: libc::sigaction = std::mem::zeroed();
        sa_crash.sa_sigaction = crash_cleanup_handler as *const () as usize;
        libc::sigemptyset(&mut sa_crash.sa_mask);
        sa_crash.sa_flags = libc::SA_RESETHAND;
        libc::sigaction(libc::SIGSEGV, &sa_crash, std::ptr::null_mut());
        libc::sigaction(libc::SIGABRT, &sa_crash, std::ptr::null_mut());
        libc::sigaction(libc::SIGBUS, &sa_crash, std::ptr::null_mut());
        libc::sigaction(libc::SIGFPE, &sa_crash, std::ptr::null_mut());
    }
}

/// Remove the run tmpdir when the run ends.
/// This run's temporary directory, once made.
static RUN_TMPDIR: std::sync::OnceLock<String> = std::sync::OnceLock::new();

pub fn set_tmpdir(path: String) {
    if let Err(e) = crate::sigrm::register(&path) {
        eprintln!("Error: {}", e);
        clean_exit(1);
    }
}

/// Publish the current basename with its trailing `-` so the signal
/// handler only has to append the 4-hex volume index and a NUL.
fn record_shm_basename(basename: &str) {
    let prefix = CString::new(format!("{}-", basename))
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
    let _ = RUN_TMPDIR.set(tmpdir.clone());
    match make_temp_root(&tmpdir) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("Error: {}", e);
            std::process::exit(1);
        }
    }

    // Point every pool at this run's benchmark record file. An env var
    // rather than a per-language setter because pools inherit it for
    // free -- Python and R never enter libmorloc's pool_main, so a
    // C-ABI setter would have to be bound four times over. Unset means
    // "not benchmarking", and morloc_bench_record is then a no-op.
    crate::process::set_startup_env(
        "MORLOC_BENCH_RECORDS",
        std::path::Path::new(&tmpdir).join("benchmark.records"),
    );

    cleanup_stale_shm();

    let job_hash = make_job_hash(42);
    // SHM name convention, uniform across platforms. macOS `shm_open`
    // requires a leading '/' and caps the whole name (slash included) at
    // PSHMNAMLEN = 31; Linux/glibc accepts the identical form, so one
    // shape works everywhere and the Linux test suite exercises exactly
    // the names macOS uses. Four dash-separated fixed-width hex fields
    // (always present) keep names short, give a distinctive `mlc-` match
    // prefix, and make parsing positional:
    //   basename  /mlc-<pid:6hex>-<hash:8hex>-<gen:4hex>
    //   volume    <basename>-<vol:4hex>       registry  <basename>.reg
    // The PID (embedded so orphan sweeps need no SHM-header read) uses 6
    // hex = 24 bits, covering every real PID (Linux pid_max <= 2^22,
    // macOS PID_MAX = 99999). The 32-bit hash disambiguates same-PID
    // jobs. The generation field starts at 0000 and is bumped by
    // `basename_for_generation` on pool-crash recovery.
    let shm_basename = format!(
        "/mlc-{:06x}-{:08x}-0000",
        std::process::id(),
        job_hash & 0xffff_ffff
    );

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
        let shm = shinit(basename_c.as_ptr(), morloc_runtime_types::shm_types::PRIMARY_VOLUME, 0xffff, &mut errmsg);
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
        // shared stream registry. Since the registry lives in a
        // CompanionSegment (`<basename>.reg`) it never collides
        // with the allocator's `-<idx>` volumes. Subsequent pool
        // processes attach to the same segment; `stream_registry_init`
        // is idempotent.
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
    teardown(exit_code, true)
}

// DAEMON-6: for a process whose other threads may still be running.
pub fn exit_leaving_threads(exit_code: i32) -> ! {
    teardown(exit_code, false)
}

fn teardown(exit_code: i32, unmap: bool) -> ! {
    // Exactly one thread runs the teardown. A second concurrent caller
    // would repeat the waitpid sweep, the SHM unlink and
    // morloc_run_finalize, and race the owner on the exit status.
    if CLEANING_UP.swap(true, Ordering::SeqCst) {
        park_until_exit();
    }
    EXIT_CODE.store(exit_code, Ordering::SeqCst);
    // DAEMON-10
    unsafe { morloc_daemon_remove_endpoints() };

    // A successful run completes its streamed stdout (a no-op when the
    // result printer already did). A failed run leaves it unterminated so
    // a reader cannot mistake a partial stream for a whole one.
    let mut exit_code = exit_code;
    if exit_code == 0 && !BROKEN_PIPE.load(Ordering::Relaxed) {
        crate::stdio_server::finish_stdout();
    }
    // A stage completes the stream it saved even when stdout broke; the
    // run then reports the broken pipe.
    if exit_code == 0 && crate::stage::active() {
        if let Err(e) = crate::stdio_server::finish_stage() {
            eprintln!("Error: saving the stdout stream: {}", e);
            exit_code = 1;
        } else if crate::stdio_server::stage_stdout_broken() {
            exit_code = 141;
        }
        EXIT_CODE.store(exit_code, Ordering::SeqCst);
    }

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

    stop_pools();
    crate::stop_frontend_children();

    // FORK-15: leases kept outside the run directory.
    unsafe { morloc_remove_leases() };
    // Clean up shared memory segments
    extern "C" {
        fn morloc_shretire(errmsg: *mut *mut std::ffi::c_char) -> bool;
    }
    // DAEMON-5: names only; no thread can find the memory unmapped.
    if unmap {
        unsafe {
            let mut err: *mut std::ffi::c_char = std::ptr::null_mut();
            morloc_shretire(&mut err);
            if !err.is_null() {
                libc::free(err as *mut libc::c_void);
            }
        }
    }
    // DAEMON-5
    unsafe { morloc_refuse_new_segments() };
    // Every segment the run recorded, including those of earlier recovery
    // generations and companions whose own teardown did not run.
    if let Some(dir) = RUN_TMPDIR.get() {
        unlink_marked_segments(Path::new(dir));
    }

    // Aggregate and emit the benchmark summary while the tmpdir still
    // exists -- the records live inside it -- and after the pools are
    // reaped, so every completed call has been written.
    crate::runlog::emit_benchmark_summary();

    // Remove the tmpdir and every other directory the run registered.
    unsafe { crate::sigrm::remove_registered() };

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

    // DAEMON-6: the shutdown watchdog may already be ending the process.
    if !unsafe { morloc_claim_exit() } {
        loop {
            std::thread::sleep(Duration::from_secs(1));
        }
    }
    if !unmap {
        // DAEMON-6: exit handlers would unmap shared memory under the threads.
        unsafe {
            libc::fflush(std::ptr::null_mut());
            libc::_exit(exit_code)
        };
    }
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
static PANICKED: AtomicBool = AtomicBool::new(false);
static IN_FLIGHT: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
// DAEMON-6
const PANIC_GRACE: Duration = Duration::from_secs(5);

pub struct Request(());

impl Drop for Request {
    fn drop(&mut self) {
        IN_FLIGHT.fetch_sub(1, Ordering::SeqCst);
    }
}

// PANIC-3: no new work once a panic was caught.
pub fn begin_request() -> Option<Request> {
    IN_FLIGHT.fetch_add(1, Ordering::SeqCst);
    let request = Request(());
    if PANICKED.load(Ordering::SeqCst) {
        return None;
    }
    Some(request)
}

// PANIC-3
pub fn refuse_new_work() {
    PANICKED.store(true, Ordering::SeqCst);
}

// PANIC-3: the daemon's own shutdown waits for its requests.
pub fn fail_daemon() -> ! {
    extern "C" {
        fn morloc_daemon_fail();
    }
    refuse_new_work();
    unsafe { morloc_daemon_fail() };
    loop {
        std::thread::park();
    }
}

// PANIC-3
pub fn end_after_panic() -> ! {
    refuse_new_work();
    let deadline = std::time::Instant::now() + PANIC_GRACE;
    while IN_FLIGHT.load(Ordering::SeqCst) > 0 && std::time::Instant::now() < deadline {
        std::thread::sleep(Duration::from_millis(10));
    }
    exit_leaving_threads(morloc_runtime_types::panic::PANIC_EXIT_STATUS)
}

pub fn exit_broken_pipe() -> ! {
    BROKEN_PIPE.store(true, Ordering::SeqCst);
    clean_exit(141);
}

/// True once some thread has committed to tearing the nexus down.
///
/// Callers about to report a pool failure use this to tell a real fault
/// from a consequence of the teardown: `clean_exit` SIGTERMs every pool
/// process group, so a pool connection still open at that moment drops,
/// and a thread blocked reading it sees a peer that vanished mid-call.
pub fn teardown_in_progress() -> bool {
    CLEANING_UP.load(Ordering::SeqCst)
}

/// Block a thread that reached an exit path while another thread already
/// owns the teardown. The owner ends in `exit`, which takes the whole
/// process down, so this normally never returns.
pub fn park_until_exit() -> ! {
    let deadline = std::time::Instant::now() + PARK_LIMIT;
    while std::time::Instant::now() < deadline {
        std::thread::sleep(Duration::from_millis(10));
    }
    unsafe { libc::_exit(EXIT_CODE.load(Ordering::SeqCst)) };
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
fn start_language_server(socket: &PoolSocket) -> Result<(i32, libc::pid_t, PoolPin), String> {
    extern "C" {
        fn morloc_lifeline_child_env(read_fd: *mut i32) -> *const libc::c_char;
    }
    let cmd = socket.syscmd.first().ok_or_else(|| format!("pool '{}' has no command", socket.lang))?;
    let program = resolve_program(cmd)
        .ok_or_else(|| format!("cannot start pool '{}': '{}' was not found on PATH", socket.lang, cmd.to_string_lossy()))?;
    let argv: Vec<*const libc::c_char> = socket
        .syscmd
        .iter()
        .map(|s| s.as_ptr())
        .chain(std::iter::once(std::ptr::null()))
        .collect();

    let mut lifeline_fd: i32 = -1;
    let lifeline = unsafe { morloc_lifeline_child_env(&mut lifeline_fd) };
    // The pool's source fingerprint, mixed into its cache keys, and its
    // lifeline go into its environment only.
    let mut env: Vec<CString> = std::env::vars_os()
        .filter(|(k, _)| k != "MORLOC_POOL_HASH" && k != "MORLOC_LIFELINE")
        .filter_map(|(k, v)| {
            let mut kv = k.into_encoded_bytes();
            kv.push(b'=');
            kv.extend(v.into_encoded_bytes());
            CString::new(kv).ok()
        })
        .collect();
    let mut hash = b"MORLOC_POOL_HASH=".to_vec();
    hash.extend_from_slice(socket.pool_hash.as_bytes());
    env.push(CString::new(hash).map_err(|e| e.to_string())?);
    if !lifeline.is_null() {
        env.push(unsafe { std::ffi::CStr::from_ptr(lifeline) }.to_owned());
    }
    let envp: Vec<*const libc::c_char> =
        env.iter().map(|s| s.as_ptr()).chain(std::iter::once(std::ptr::null())).collect();

    let fail = |e: std::io::Error| format!("cannot start pool '{}' running '{}': {e}", socket.lang, program.to_string_lossy());
    let (pin_pid, pin) = start_pin().map_err(fail)?;
    // DAEMON-11: refused, the pin ends with its pipe.
    let group = POOL_GROUPS
        .add(pin_pid)
        .ok_or_else(|| format!("cannot start pool '{}': too many process groups", socket.lang))?;
    let pinned = PoolPin { group, _pin: pin };
    // DAEMON-13: recorded before anything in the group can make shared memory.
    if let Some(dir) = RUN_TMPDIR.get() {
        std::fs::write(std::path::Path::new(dir).join(format!("{GROUP_FILE_PREFIX}{pin_pid}")), "")
            .map_err(|e| format!("cannot start pool '{}': cannot record its process group in {dir}: {e}", socket.lang))?;
    }
    let started = pinned.group.while_held(|mask| {
        let mut spawn = morloc_runtime_types::spawn::Spawn::new()?;
        spawn.join_process_group(pin_pid)?;
        spawn.signal_mask(mask)?;
        if lifeline_fd >= 0 {
            spawn.keep_across_exec(lifeline_fd)?;
        }
        spawn.run(&program, &argv, &envp, false)
    });
    match started {
        Some(Ok(pid)) => Ok((pid, pin_pid, pinned)),
        Some(Err(e)) => Err(fail(e)),
        None => Err(format!("cannot start pool '{}': the pools are stopping", socket.lang)),
    }
}

/// A process leading a new group, which holds the group's id until the
/// returned pipe end closes or it is killed.
// DAEMON-11
fn start_pin() -> std::io::Result<(libc::pid_t, std::os::fd::OwnedFd)> {
    use std::os::fd::FromRawFd;
    let mut fds = [0; 2];
    if unsafe { morloc_runtime_types::fd::pipe(fds.as_mut_ptr()) } != 0 {
        return Err(std::io::Error::last_os_error());
    }
    let (read, write) = unsafe { (std::os::fd::OwnedFd::from_raw_fd(fds[0]), std::os::fd::OwnedFd::from_raw_fd(fds[1])) };
    let sh = CString::new("/bin/sh").unwrap();
    let name = CString::new(POOL_PIN_NAME).unwrap();
    let flag = CString::new("-c").unwrap();
    let script = CString::new("trap '' TERM INT HUP; read _; exit 0").unwrap();
    let argv = [name.as_ptr(), flag.as_ptr(), script.as_ptr(), std::ptr::null()];
    let envp = [std::ptr::null()];
    let mut spawn = morloc_runtime_types::spawn::Spawn::new()?;
    spawn.new_process_group()?;
    // DAEMON-11: immune from birth, before the shell sets its traps.
    let mut mask: libc::sigset_t = unsafe { std::mem::zeroed() };
    unsafe {
        libc::sigemptyset(&mut mask);
        for sig in [libc::SIGTERM, libc::SIGINT, libc::SIGHUP] {
            libc::sigaddset(&mut mask, sig);
        }
    }
    spawn.signal_mask(&mask)?;
    use std::os::fd::AsRawFd;
    spawn.dup2(read.as_raw_fd(), 0)?;
    // DAEMON-11: not a writer of the nexus's output, so no reader of it waits on the pin.
    let null = std::fs::OpenOptions::new().write(true).open("/dev/null")?;
    spawn.dup2(null.as_raw_fd(), 1)?;
    spawn.dup2(null.as_raw_fd(), 2)?;
    let pid = spawn.run(&sh, &argv, &envp, false)?;
    Ok((pid, write))
}

/// The file `cmd` names: itself when it holds a `/`, otherwise the first
/// executable of that name on PATH, as `execvp` would find it.
fn resolve_program(cmd: &CString) -> Option<CString> {
    use std::os::unix::ffi::OsStrExt;
    let bytes = cmd.as_bytes();
    if bytes.contains(&b'/') {
        return Some(cmd.clone());
    }
    let path = std::env::var_os("PATH").unwrap_or_else(|| "/usr/bin:/bin".into());
    std::env::split_paths(&path).find_map(|dir| {
        let dir = if dir.as_os_str().is_empty() { std::path::PathBuf::from(".") } else { dir };
        let candidate = CString::new(dir.join(std::ffi::OsStr::from_bytes(bytes)).into_os_string().into_encoded_bytes()).ok()?;
        let is_file = std::fs::metadata(std::ffi::OsStr::from_bytes(candidate.as_bytes())).is_ok_and(|m| m.is_file());
        (is_file && unsafe { libc::access(candidate.as_ptr(), libc::X_OK) } == 0).then_some(candidate)
    })
}

#[cfg(test)]
static SPAWN_TO_RECORD_DELAY_MS: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

/// Start pool daemons for the given socket indices and wait for them to respond to pings.
pub fn start_daemons(sockets: &mut [PoolSocket], indices: &[usize]) -> Result<(), String> {
    extern "C" {
        fn stream_pid_start_time(pid: u32) -> u64;
    }
    for &idx in indices {
        let since = unsafe { morloc_reaped_sequence() };
        let (pid, pgid, pin) = start_language_server(&sockets[idx])?;
        pool_pins()[idx] = Some(pin);
        POOL_PGIDS[idx].store(pgid, Ordering::SeqCst);
        #[cfg(test)]
        std::thread::sleep(Duration::from_millis(SPAWN_TO_RECORD_DELAY_MS.load(Ordering::Relaxed)));
        sockets[idx].pid = pid;
        // Capture the pool's start stamp at spawn so the PID-based crash
        // sweep can detect PID reuse. Zero if the read races a fast exit;
        // the sweep falls back to a PID-only match in that case.
        let start_time = unsafe { stream_pid_start_time(pid as u32) };
        sockets[idx].pid_start_time = start_time;
        POOL_START_TIMES[idx].store(start_time, Ordering::Release);
        // Fresh incarnation: clear the sweep-done flag so the next
        // death of this pool is detected.
        POOL_SWEPT[idx].store(false, Ordering::Relaxed);
        PIDS[idx].store(pid, Ordering::SeqCst);
        SPAWNED_PIDS[idx].store(pid, Ordering::Relaxed);
        let mut status = 0;
        if unsafe { morloc_take_noted_child_exit(pid, since, &mut status) } == 1 {
            kill_pool_group(idx);
            EXIT_STATUSES[idx].store(status, Ordering::Relaxed);
            PIDS[idx].store(-1, Ordering::SeqCst);
        }
        // Record the lang label for the post-mortem in report_dead_pools.
        {
            let mut langs = POOL_LANGS.lock().unwrap();
            if langs.len() <= idx {
                langs.resize(idx + 1, String::new());
            }
            langs[idx] = sockets[idx].lang.clone();
        }
    }

    // Wait for each daemon to respond to pings
    for &idx in indices {
        wait_for_daemon(&sockets[idx], idx)?;
    }

    Ok(())
}

/// Walk the per-index PIDs/start-times arrays and enqueue a
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
        let dead_pid = SPAWNED_PIDS[i].load(Ordering::Relaxed);
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
// DAEMON-6: false once a daemon shutdown is requested.
fn sleep_unless_shutting_down(d: Duration) -> bool {
    let until = std::time::Instant::now() + d;
    loop {
        if unsafe { morloc_daemon_is_shutting_down() } {
            return false;
        }
        let left = until.saturating_duration_since(std::time::Instant::now());
        if left.is_zero() {
            return true;
        }
        std::thread::sleep(left.min(Duration::from_millis(50)));
    }
}

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
                let _ = stream.set_read_timeout(Some(ping_timeout.min(PING_REPLY_LIMIT)));
                let _ = stream.set_write_timeout(Some(ping_timeout.min(PING_REPLY_LIMIT)));

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
        if !sleep_unless_shutting_down(Duration::new(secs, nanos)) {
            return Err(format!("the daemon is shutting down; pool '{}' was not started", socket.lang));
        }
        retry_delay *= RETRY_MULTIPLIER;
        ping_timeout = ping_timeout * 2;
    }

    unreachable!()
}

/// Stop every pool process group: SIGTERM, up to 200 ms for the pools to
/// exit, then SIGKILL; reap for up to 100 ms more.
pub fn stop_pools() {
    // DAEMON-6
    unsafe { morloc_stop_child_groups() };
    for i in 0..MAX_DAEMONS {
        signal_pool_group(i, libc::SIGTERM);
    }

    // DAEMON-11: the grace ends when the pool processes have exited.
    let until = std::time::Instant::now() + Duration::from_millis(200);
    while (0..MAX_DAEMONS).any(pool_is_alive) && std::time::Instant::now() < until {
        reap_noting();
        std::thread::sleep(Duration::from_millis(2));
    }
    // DAEMON-11: also ends the pins; no signal follows.
    POOL_GROUPS.stop_all();
    let until = std::time::Instant::now() + Duration::from_millis(100);
    loop {
        reap_noting();
        if !has_children() || std::time::Instant::now() >= until {
            break;
        }
        std::thread::sleep(Duration::from_millis(2));
    }
}

fn has_children() -> bool {
    let mut info: libc::siginfo_t = unsafe { std::mem::zeroed() };
    let rc = unsafe { libc::waitid(libc::P_ALL, 0, &mut info, libc::WEXITED | libc::WNOHANG | libc::WNOWAIT) };
    rc == 0 || std::io::Error::last_os_error().raw_os_error() != Some(libc::ECHILD)
}

// DAEMON-6: async-signal-safe, so it ends a teardown whatever locks are held.
pub fn emergency_exit_ptr() -> *const std::ffi::c_void {
    extern "C" fn emergency_exit(code: libc::c_int) {
        stop_everything();
        unsafe { libc::_exit(code) };
    }
    emergency_exit as *const std::ffi::c_void
}

/// Return a C-compatible function pointer for stop_pools.
pub fn stop_pools_ptr() -> *const std::ffi::c_void {
    extern "C" fn stop_pools_c() {
        morloc_runtime_types::panic::outside_scope(stop_pools)
    }
    stop_pools_c as *const std::ffi::c_void
}

/// Return a C-compatible function pointer for pool_is_alive.
pub fn pool_is_alive_ptr() -> *const std::ffi::c_void {
    extern "C" fn pool_alive_c(pool_index: usize) -> bool {
        morloc_runtime_types::panic::outside_scope(|| pool_is_alive(pool_index))
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

static REAPING: AtomicBool = AtomicBool::new(false);

// DAEMON-3: every reaped status is recorded for whoever waits on that child.
fn reap_noting() {
    reap(false)
}

// DAEMON-11: a handler may have interrupted the reaper's own thread, so it never waits for it.
fn reap_from_handler() {
    reap(true)
}

fn reap(in_handler: bool) {
    loop {
        // DAEMON-11: one reaper at a time, so a pid marked exited is never one reused since.
        while REAPING.swap(true, Ordering::SeqCst) {
            if in_handler {
                return;
            }
            std::thread::yield_now();
        }
        while let Some(pid) = exited_child() {
            // DAEMON-11: the group's slot is dead before its leader's id is freed.
            POOL_GROUPS.leader_exited(pid);
            crate::mcp::FRONTEND_EVALS.leader_exited(pid);
            unsafe { morloc_child_group_leader_exited(pid) };
            if let Some(i) = (0..MAX_DAEMONS).find(|&i| PIDS[i].load(Ordering::SeqCst) == pid) {
                kill_pool_group(i);
            }
            let mut status: libc::c_int = 0;
            if unsafe { libc::waitpid(pid, &mut status, libc::WNOHANG) } != pid {
                continue;
            }
            unsafe { morloc_note_child_exit(pid, status) };
            for i in 0..MAX_DAEMONS {
                if PIDS[i].load(Ordering::SeqCst) == pid {
                    EXIT_STATUSES[i].store(status, Ordering::Relaxed);
                    PIDS[i].store(-1, Ordering::Relaxed);
                    break;
                }
            }
        }
        REAPING.store(false, Ordering::SeqCst);
        // DAEMON-11: a reaper turned away meanwhile left its child to this one.
        if exited_child().is_none() {
            return;
        }
    }
}

/// An exited child, left unreaped.
fn exited_child() -> Option<libc::pid_t> {
    loop {
        let mut info: libc::siginfo_t = unsafe { std::mem::zeroed() };
        let rc = unsafe { libc::waitid(libc::P_ALL, 0, &mut info, libc::WEXITED | libc::WNOHANG | libc::WNOWAIT) };
        if rc == 0 {
            let pid = unsafe { info.si_pid() };
            return (pid > 0).then_some(pid);
        }
        if std::io::Error::last_os_error().kind() != std::io::ErrorKind::Interrupted {
            return None;
        }
    }
}

/// A cross-language "Connection closed by peer" only names the caller pool;
/// the callee that actually died is invisible to the caller (they are
/// siblings, both children of this nexus). This nexus is the parent, so
/// `waitpid` gives it the exact disposition. Crucially, a pool killed by an
/// uncatchable SIGKILL -- e.g. the macOS memory-pressure/jetsam OOM killer --
/// leaves NO in-pool backtrace (no crash handler, no faulthandler, no log);
/// only the parent's wait-status reveals it ("signal 9"). Called from the
/// run-failed paths so that signal is surfaced instead of swallowed.
/// Wait up to `limit` for pool `pool_index`, which closed its connection on
/// its way out, to exit and be reaped.
pub fn wait_for_pool_exit(pool_index: usize, limit: Duration) {
    let until = std::time::Instant::now() + limit;
    loop {
        reap_noting();
        if pool_death_info(pool_index).is_some() || std::time::Instant::now() >= until {
            return;
        }
        std::thread::sleep(Duration::from_millis(10));
    }
}

/// `dying` when a pool ended the call without a reply: it closed the
/// connection on its way out, so its exit is waited for, briefly.
pub fn report_dead_pools(dying: bool) {
    // Drain any child exits the SIGCHLD handler has not yet processed, so a
    // pool that died microseconds before the caller observed EOF is still
    // reported (avoids a report/reap race).
    reap_noting();
    let n = POOL_LANGS.lock().unwrap().len().min(MAX_DAEMONS);
    let until = std::time::Instant::now() + Duration::from_millis(500);
    while dying && !(0..n).any(|i| pool_death_info(i).is_some()) && std::time::Instant::now() < until {
        std::thread::sleep(Duration::from_millis(10));
        reap_noting();
    }
    let langs = POOL_LANGS.lock().unwrap();
    for i in 0..langs.len().min(MAX_DAEMONS) {
        if let Some(info) = pool_death_info(i) {
            let lang = &langs[i];
            if lang.is_empty() {
                eprintln!("  pool {}: {}", i, info);
            } else {
                eprintln!("  pool {} [{}]: {}", i, lang, info);
            }
        }
    }
}

/// Resolve a command the way `execvp` will: a path containing '/' is checked
/// directly, a bare name is searched across `$PATH`.
fn command_on_path(cmd: &str) -> bool {
    if cmd.contains('/') {
        return Path::new(cmd).exists();
    }
    // Mirror execvp: an unset PATH falls back to a default search path, so do the
    // same here rather than reporting the interpreter missing.
    let path = std::env::var("PATH").unwrap_or_else(|_| "/bin:/usr/bin".to_string());
    path.split(':')
        .any(|dir| !dir.is_empty() && Path::new(dir).join(cmd).exists())
}

/// Validate that all pool executables exist. Two distinct checks: the pool FILE
/// (the last argv element -- a build-dir-relative path) must exist on disk, and
/// for an interpreted pool the INTERPRETER (argv[0], e.g. Rscript/python3) must
/// resolve on PATH. Without the second check a missing interpreter surfaces only
/// as an opaque fork/exec failure after startup.
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
        // Interpreted pools carry [interpreter, script, ...]; compiled pools
        // carry just [executable] (already checked above).
        if pool.exec.len() > 1 {
            let interpreter = &pool.exec[0];
            if !command_on_path(interpreter) {
                return Err(format!(
                    "The '{}' pool needs the interpreter '{}', which was not found on PATH. \
                     Install it, or run inside an environment that provides it \
                     (e.g. `mim run [--env <name>] -- ...`).",
                    pool.lang, interpreter
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
    let dir = unsafe { std::ffi::CStr::from_ptr(result) }.to_string_lossy().into_owned();
    // Record the owner before anything else lands in the directory, so the
    // startup sweep of a later run can tell a dead run's directory from a
    // live one; written whole by rename.
    let pid = std::process::id();
    let record = format!(
        "{} {} {} {}\n",
        pid,
        proc_info::start_time(pid),
        proc_info::boot_id().unwrap_or_else(|| "?".into()),
        proc_info::pid_namespace().unwrap_or_else(|| "?".into()),
    );
    let path = std::path::Path::new(&dir);
    std::fs::write(path.join(".owner.tmp"), record)
        .and_then(|_| std::fs::rename(path.join(".owner.tmp"), path.join(OWNER_FILE)))
        .map_err(|e| format!("Failed to record the owner of {}: {}", dir, e))?;
    Ok(dir)
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

/// The run directory of a nexus that died without cleaning up still holds
/// what the run made: a marker for each shared-memory object (see the
/// runtime's `shm::marker_path`) and its file-backed volumes. Remove every
/// such directory under /tmp whose owner is gone, with the objects its
/// markers name. Called once at nexus startup.
///
/// A directory is only removed when its `.owner` record proves the owner
/// dead: written under another boot, or naming, in this PID namespace, a
/// process that no longer runs. A directory without a record, of another
/// user, or recorded in another PID namespace (a container sharing /tmp)
/// is left alone.
pub fn cleanup_stale_shm() {
    use std::os::unix::fs::MetadataExt;
    let boot = proc_info::boot_id();
    let ns = proc_info::pid_namespace();
    let euid = unsafe { libc::geteuid() };
    let Ok(entries) = std::fs::read_dir("/tmp") else { return };
    for entry in entries.flatten() {
        let name = entry.file_name();
        let name = name.to_string_lossy();
        if !(name.starts_with("morloc.") && name.len() == "morloc.XXXXXX".len()) {
            continue;
        }
        let dir = entry.path();
        let Ok(meta) = std::fs::symlink_metadata(&dir) else { continue };
        if !meta.is_dir() || meta.uid() != euid || meta.mode() & 0o777 != 0o700 {
            continue;
        }
        sweep_run_if_dead(&dir, boot.as_deref(), ns.as_deref());
    }
}

// DAEMON-13
fn sweep_run_if_dead(dir: &std::path::Path, boot: Option<&str>, ns: Option<&str>) -> bool {
    let Ok(record) = std::fs::read_to_string(dir.join(OWNER_FILE)) else { return false };
    let f: Vec<&str> = record.split_whitespace().collect();
    let [pid, start, owner_boot, owner_ns] = f.as_slice() else { return false };
    let (Ok(pid), Ok(start)) = (pid.parse::<u32>(), start.parse::<u64>()) else { return false };
    if ns != Some(*owner_ns) {
        return false;
    }
    let other_boot = boot.is_some_and(|b| b != *owner_boot);
    if !(other_boot || !proc_info::alive(pid, start)) {
        return false;
    }
    // DAEMON-13: groups of another boot are gone.
    if !other_boot && recorded_group_running(dir) {
        return false;
    }
    unlink_marked_segments(dir);
    // FORK-16: a temp root kept outside the run directory goes with it.
    if let Ok(temps) = std::fs::read_to_string(dir.join(TEMPS_FILE)) {
        let _ = std::fs::remove_dir_all(temps.trim_end());
    }
    let _ = std::fs::remove_dir_all(dir);
    true
}

// DAEMON-13
fn recorded_group_running(dir: &std::path::Path) -> bool {
    let Ok(entries) = std::fs::read_dir(dir) else { return dir.exists() };
    entries.flatten().any(|e| {
        e.file_name()
            .to_str()
            .and_then(|f| f.strip_prefix(GROUP_FILE_PREFIX))
            .and_then(|g| g.parse::<libc::pid_t>().ok())
            .is_some_and(proc_info::group_running)
    })
}

// DAEMON-13
pub fn sweep_dead_run(dir: &std::path::Path) -> bool {
    sweep_run_if_dead(dir, proc_info::boot_id().as_deref(), proc_info::pid_namespace().as_deref())
}

// DAEMON-13
pub fn run_dir_of(nexus_pid: u32, nexus_start: u64) -> Option<std::path::PathBuf> {
    let entries = std::fs::read_dir("/tmp").ok()?;
    entries.flatten().map(|e| e.path()).find(|dir| {
        let name = dir.file_name().map(|n| n.to_string_lossy().into_owned()).unwrap_or_default();
        name.starts_with("morloc.")
            && std::fs::read_to_string(dir.join(OWNER_FILE)).is_ok_and(|record| {
                let f: Vec<&str> = record.split_whitespace().collect();
                f.first().and_then(|p| p.parse::<u32>().ok()) == Some(nexus_pid)
                    && f.get(1).and_then(|t| t.parse::<u64>().ok()) == Some(nexus_start)
            })
    })
}

// DAEMON-13
const GROUP_FILE_PREFIX: &str = ".group-";

// DAEMON-13
pub fn sweep_after(target: libc::pid_t, run_dir: Option<&std::path::Path>) {
    let ended = || {
        if target < 0 {
            !proc_info::group_running(-target)
        } else {
            !proc_info::alive(target as u32, 0)
        }
    };
    let until = std::time::Instant::now() + REAP_LIMIT;
    while !ended() {
        if std::time::Instant::now() >= until {
            return;
        }
        std::thread::sleep(Duration::from_millis(10));
    }
    if let Some(dir) = run_dir {
        sweep_dead_run(dir);
    }
}

// DAEMON-13
const REAP_LIMIT: Duration = Duration::from_secs(10);

/// The run directory's record of its owner: pid, start stamp, boot and PID
/// namespace.
const OWNER_FILE: &str = ".owner";
const TEMPS_FILE: &str = ".temps";

// FORK-16: the run's temp root (the runtime derives the same path), under
// `--tmpdir` when one is given, else in the run directory; one outside the
// run directory is recorded there before it exists, so a later run's sweep
// finds it however this run ends.
fn make_temp_root(run_dir: &str) -> Result<String, String> {
    extern "C" {
        fn morloc_run_temp_root(run_dir: *const libc::c_char, user_tmpdir: *const libc::c_char) -> *mut libc::c_char;
    }
    let user = std::env::var("MORLOC_TMPDIR").ok().filter(|d| !d.is_empty());
    let run_c = CString::new(run_dir).map_err(|e| e.to_string())?;
    let user_c = user.as_deref().map(CString::new).transpose().map_err(|e| e.to_string())?;
    let raw = unsafe { morloc_run_temp_root(run_c.as_ptr(), user_c.as_ref().map_or(std::ptr::null(), |c| c.as_ptr())) };
    if raw.is_null() {
        return Err("Failed to name the run's temp root".into());
    }
    let dir = unsafe { std::ffi::CStr::from_ptr(raw) }.to_string_lossy().into_owned();
    unsafe { libc::free(raw as *mut libc::c_void) };
    if user.is_some() {
        let marker = std::path::Path::new(run_dir);
        std::fs::write(marker.join(".temps.tmp"), &dir)
            .and_then(|_| std::fs::rename(marker.join(".temps.tmp"), marker.join(TEMPS_FILE)))
            .map_err(|e| format!("Failed to record the temp root {}: {}", dir, e))?;
        crate::sigrm::register(&dir)?;
    }
    std::fs::create_dir_all(&dir).map_err(|e| format!("Failed to create {}: {}", dir, e))?;
    Ok(dir)
}

/// Remove every shared-memory object a marker in `dir` names. Only morloc's
/// own names are touched.
pub fn unlink_marked_segments(dir: &std::path::Path) {
    let Ok(entries) = std::fs::read_dir(dir) else { return };
    for entry in entries.flatten() {
        let file = entry.file_name();
        let file = file.to_string_lossy();
        let Some(object) = file.strip_suffix(".shm").filter(|o| o.starts_with("mlc-")) else { continue };
        if let Ok(c) = CString::new(format!("/{object}")) {
            unsafe { libc::shm_unlink(c.as_ptr()) };
        }
        let _ = std::fs::remove_file(entry.path());
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
    #[test]
    fn a_pool_group_outlives_its_pool_until_released() {
        let (pgid, pin) = super::start_pin().unwrap();
        let mut spawn = morloc_runtime_types::spawn::Spawn::new().unwrap();
        spawn.join_process_group(pgid).unwrap();
        let (sh, flag, script) = (CString::new("/bin/sh").unwrap(), CString::new("-c").unwrap(), CString::new("exit 0").unwrap());
        let argv = [sh.as_ptr(), flag.as_ptr(), script.as_ptr(), std::ptr::null()];
        let pool = spawn.run(&sh, &argv, &[std::ptr::null()], false).unwrap();
        let mut status = 0;
        assert_eq!(unsafe { libc::waitpid(pool, &mut status, 0) }, pool);
        assert_eq!(unsafe { libc::kill(-pgid, 0) }, 0, "the group ended with its pool");
        unsafe { libc::kill(-pgid, libc::SIGTERM) };
        std::thread::sleep(Duration::from_millis(100));
        assert_eq!(unsafe { libc::kill(-pgid, 0) }, 0, "the pin ended on SIGTERM");
        drop(pin);
        assert_eq!(unsafe { libc::waitpid(pgid, &mut status, 0) }, pgid);
        assert_eq!(unsafe { libc::kill(-pgid, 0) }, -1, "the group outlived its pin");
    }

    #[test]
    fn a_thread_that_reaps_waits_for_a_reap_in_progress() {
        while REAPING.swap(true, Ordering::SeqCst) {
            std::thread::yield_now();
        }
        let reaper = std::thread::spawn(reap_noting);
        std::thread::sleep(Duration::from_millis(100));
        assert!(!reaper.is_finished(), "returned before the reap in progress ended");
        REAPING.store(false, Ordering::SeqCst);
        reaper.join().unwrap();
    }

    #[test]
    fn a_pin_ignores_sigterm_from_birth() {
        let (pgid, pin) = super::start_pin().unwrap();
        unsafe { libc::kill(-pgid, libc::SIGTERM) };
        std::thread::sleep(Duration::from_millis(100));
        let mut status = 0;
        assert_eq!(unsafe { libc::waitpid(pgid, &mut status, libc::WNOHANG) }, 0, "the pin ended on SIGTERM");
        drop(pin);
        assert_eq!(unsafe { libc::waitpid(pgid, &mut status, 0) }, pgid);
        assert!(libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0);
    }

    use super::*;

    fn hex4(val: u16) -> String {
        let mut buf = [0u8; 8];
        let end = write_hex4(&mut buf, 0, val);
        std::str::from_utf8(&buf[..end]).unwrap().to_string()
    }

    #[test]
    fn write_hex4_zero_is_padded() {
        assert_eq!(hex4(0), "0000");
    }

    #[test]
    fn write_hex4_pads_to_four() {
        assert_eq!(hex4(7), "0007");
        assert_eq!(hex4(0x1234), "1234");
        // The max volume index fits in exactly 4 hex digits.
        assert_eq!(MAX_VOLUME_NUMBER, 32768);
        assert_eq!(hex4((MAX_VOLUME_NUMBER - 1) as u16), "7fff");
    }

    /// A run directory under /tmp, recording `owner` and one shared-memory
    /// object, which is created. Returns the directory and the object name.
    fn fake_run(owner: &str, tag: &str) -> (std::path::PathBuf, CString) {
        let mut tmpl = *b"/tmp/morloc.XXXXXX\0";
        let p = unsafe { libc::mkdtemp(tmpl.as_mut_ptr() as *mut libc::c_char) };
        assert!(!p.is_null());
        let dir = std::path::PathBuf::from(unsafe { std::ffi::CStr::from_ptr(p) }.to_string_lossy().into_owned());
        std::fs::write(dir.join(OWNER_FILE), owner).unwrap();
        let object = format!("mlc-{:06x}-{tag}-0000-0001", std::process::id() & 0xff_ffff);
        std::fs::write(dir.join(format!("{object}.shm")), "").unwrap();
        let name = CString::new(format!("/{object}")).unwrap();
        let fd = unsafe { libc::shm_open(name.as_ptr(), libc::O_RDWR | libc::O_CREAT, 0o600) };
        assert!(fd >= 0);
        unsafe { libc::close(fd) };
        (dir, name)
    }

    fn object_exists(name: &CString) -> bool {
        let fd = unsafe { libc::shm_open(name.as_ptr(), libc::O_RDONLY, 0) };
        if fd >= 0 {
            unsafe { libc::close(fd) };
        }
        fd >= 0
    }

    #[test]
    fn the_startup_sweep_removes_only_dead_runs() {
        let boot = proc_info::boot_id().unwrap();
        let ns = proc_info::pid_namespace().unwrap();
        let me = std::process::id();
        let dead = unsafe {
            let pid = libc::fork();
            if pid == 0 {
                libc::_exit(0);
            }
            libc::waitpid(pid, std::ptr::null_mut(), 0);
            pid as u32
        };
        let live = fake_run(&format!("{me} {} {boot} {ns}", proc_info::start_time(me)), "aaaaaaa1");
        let gone = fake_run(&format!("{dead} 0 {boot} {ns}"), "aaaaaaa2");
        let rebooted = fake_run(&format!("{me} {} other-boot {ns}", proc_info::start_time(me)), "aaaaaaa3");
        let foreign = fake_run(&format!("{dead} 0 {boot} other-ns"), "aaaaaaa4");
        cleanup_stale_shm();
        let kept = |(dir, name): &(std::path::PathBuf, CString)| dir.exists() && object_exists(name);
        let removed = |(dir, name): &(std::path::PathBuf, CString)| !dir.exists() && !object_exists(name);
        let (live_ok, gone_ok, reboot_ok, foreign_ok) = (kept(&live), removed(&gone), removed(&rebooted), kept(&foreign));
        for (dir, name) in [&live, &gone, &rebooted, &foreign] {
            unsafe { libc::shm_unlink(name.as_ptr()) };
            let _ = std::fs::remove_dir_all(dir);
        }
        assert!(live_ok, "a live run's directory was removed");
        assert!(gone_ok, "a dead run's directory or object survived");
        assert!(reboot_ok, "a run from another boot survived");
        assert!(foreign_ok, "a run from another PID namespace was removed");
    }

    fn dead_pid() -> u32 {
        unsafe {
            let pid = libc::fork();
            if pid == 0 {
                libc::_exit(0);
            }
            libc::waitpid(pid, std::ptr::null_mut(), 0);
            pid as u32
        }
    }

    fn dead_run(tag: &str) -> (std::path::PathBuf, CString) {
        let boot = proc_info::boot_id().unwrap();
        let ns = proc_info::pid_namespace().unwrap();
        fake_run(&format!("{} 0 {boot} {ns}", dead_pid()), tag)
    }

    fn start_group(script: &str) -> libc::pid_t {
        let sh = CString::new("/bin/sh").unwrap();
        let flag = CString::new("-c").unwrap();
        let script = CString::new(script).unwrap();
        let argv = [sh.as_ptr(), flag.as_ptr(), script.as_ptr(), std::ptr::null()];
        let mut spawn = morloc_runtime_types::spawn::Spawn::new().unwrap();
        spawn.new_process_group().unwrap();
        spawn.run(&sh, &argv, &[std::ptr::null()], false).unwrap()
    }

    fn record_group(dir: &std::path::Path, pgid: libc::pid_t) {
        std::fs::write(dir.join(format!("{GROUP_FILE_PREFIX}{pgid}")), "").unwrap();
    }

    #[test]
    fn a_run_is_found_by_its_owners_pid_and_start() {
        let (dir, name) = fake_run("4000001 777 boot ns", "aaaaaab3");
        let found = run_dir_of(4000001, 777);
        let other_start = run_dir_of(4000001, 778);
        unsafe { libc::shm_unlink(name.as_ptr()) };
        let _ = std::fs::remove_dir_all(&dir);
        assert_eq!(found, Some(dir));
        assert_eq!(other_start, None, "a run of another process with the pid was found");
    }

    #[test]
    fn a_dead_run_is_swept_only_once_its_recorded_groups_have_ended() {
        let (dir, name) = dead_run("aaaaaab1");
        let pgid = start_group("exec sleep 30");
        record_group(&dir, pgid);
        let swept_while_running = sweep_dead_run(&dir);
        cleanup_stale_shm();
        let kept = dir.exists() && object_exists(&name);
        unsafe {
            libc::kill(-pgid, libc::SIGKILL);
            libc::waitpid(pgid, std::ptr::null_mut(), 0);
        }
        let swept = sweep_dead_run(&dir);
        let removed = !dir.exists() && !object_exists(&name);
        unsafe { libc::shm_unlink(name.as_ptr()) };
        let _ = std::fs::remove_dir_all(&dir);
        assert!(!swept_while_running && kept, "a dead run was swept while a recorded group ran");
        assert!(swept && removed, "a dead run whose groups ended was not swept");
    }

    #[test]
    fn the_sweeper_waits_for_a_group_to_end_then_sweeps_its_dead_run() {
        let (dir, name) = dead_run("aaaaaab2");
        let pgid = start_group("sleep 30 & sleep 30 & wait");
        record_group(&dir, pgid);
        let sweeper = {
            let dir = dir.clone();
            std::thread::spawn(move || sweep_after(-pgid, Some(&dir)))
        };
        std::thread::sleep(Duration::from_millis(200));
        let kept_while_running = dir.exists() && object_exists(&name);
        unsafe { libc::kill(-pgid, libc::SIGKILL) };
        sweeper.join().unwrap();
        let removed = !dir.exists() && !object_exists(&name);
        unsafe {
            libc::waitpid(pgid, std::ptr::null_mut(), 0);
            libc::shm_unlink(name.as_ptr());
        }
        let _ = std::fs::remove_dir_all(&dir);
        assert!(kept_while_running, "the sweeper swept while its group ran");
        assert!(removed, "the sweeper left the dead run's shared memory");
    }

    #[test]
    fn shm_names_fit_macos_pshmnamlen() {
        // macOS PSHMNAMLEN is 31, counting the leading '/'. Assemble the
        // worst case: a full-width PID (>= any real pid_max), the full
        // 32-bit hash field, a high recovery generation, and the maximum
        // volume index / the registry companion suffix.
        const PSHMNAMLEN: usize = 31;
        let base = basename_for_generation(
            &format!("/mlc-{:06x}-{:08x}-0000", 0xff_ffffu32, 0xffff_ffffu32),
            0xffff,
        );
        let volume = format!("{}-{:04x}", base, MAX_VOLUME_NUMBER - 1);
        let registry = format!("{}.reg", base);
        assert!(volume.len() <= PSHMNAMLEN, "volume '{}' is {} chars", volume, volume.len());
        assert!(registry.len() <= PSHMNAMLEN, "registry '{}' is {} chars", registry, registry.len());
    }

    #[test]
    fn pool_death_scenario() {
        if std::env::var_os("MORLOC_POOL_DEATH_SCENARIO").is_none() {
            return;
        }
        SPAWN_TO_RECORD_DELAY_MS.store(300, Ordering::Relaxed);
        install_signal_handlers();
        let mut sockets = [PoolSocket {
            lang: "test".into(),
            socket_path: "/nonexistent/morloc-test.sock".into(),
            syscmd: vec![CString::new("true").unwrap()],
            pid: 0,
            pid_start_time: 0,
            pool_hash: CString::new("").unwrap(),
        }];
        let died = matches!(start_daemons(&mut sockets, &[0]), Err(e) if e.contains("died"));
        std::process::exit(if died { 0 } else { 1 });
    }

    #[test]
    fn a_pool_that_dies_before_its_pid_is_recorded_is_reported_promptly() {
        let mut child = std::process::Command::new(std::env::current_exe().unwrap())
            .args(["--exact", "process::tests::pool_death_scenario", "--test-threads=1"])
            .env("MORLOC_POOL_DEATH_SCENARIO", "1")
            .stdout(std::process::Stdio::null())
            .spawn()
            .unwrap();
        let deadline = std::time::Instant::now() + std::time::Duration::from_secs(20);
        let status = loop {
            if let Some(status) = child.try_wait().unwrap() {
                break status;
            }
            if std::time::Instant::now() > deadline {
                let _ = child.kill();
                let _ = child.wait();
                panic!("a pool that died before its pid was recorded was not reported within 20 s");
            }
            std::thread::sleep(std::time::Duration::from_millis(50));
        };
        assert!(status.success(), "{status}");
    }

    #[test]
    fn write_hex4_appends_after_prefix() {
        // Mirrors the signal-handler sweep: prefix ends with '-', then a
        // 4-hex volume index.
        let mut buf = [0u8; 64];
        let prefix = b"mlc-0004d2-deadbeef-";
        buf[..prefix.len()].copy_from_slice(prefix);
        let end = write_hex4(&mut buf, prefix.len(), 42);
        assert_eq!(&buf[..end], b"mlc-0004d2-deadbeef-002a");
    }
}
