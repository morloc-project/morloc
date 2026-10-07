//! C ABI wrappers for daemon subsystems.
//! Replaces daemon.c. Uses serde_json, HashMap, VecDeque, and std::thread.

use std::collections::HashMap;
use std::collections::VecDeque;
use std::ffi::{c_char, c_void, CStr, CString};
use std::ptr;
use std::sync::atomic::{AtomicBool, AtomicI32, AtomicU64, AtomicU8, Ordering};
use std::sync::{Arc, Condvar, Mutex};

use crate::cschema::CSchema;
use crate::error::{clear_errmsg, set_errmsg, MorlocError};
use crate::hash;
use crate::http_ffi::{DaemonMethod, DaemonRequest, HttpMethod};

// -- Constants ----------------------------------------------------------------

const DEFAULT_XXHASH_SEED: u64 = 0;
const MAX_LP_MESSAGE: u32 = 64 * 1024 * 1024;

// -- Global state -------------------------------------------------------------

static SHUTDOWN_REQUESTED: AtomicBool = AtomicBool::new(false);
static SHUTDOWN_ESCALATED: AtomicBool = AtomicBool::new(false);
static POOLS_STOPPED: AtomicBool = AtomicBool::new(false);
static EXIT_CLAIMED: AtomicBool = AtomicBool::new(false);

// DAEMON-10: the socket and port file this daemon made, by path and identity.
static ENDPOINTS: [Endpoint; 2] = [const { Endpoint::new() }; 2];
const SOCKET_ENDPOINT: usize = 0;
const PORT_FILE_ENDPOINT: usize = 1;

struct Endpoint {
    path: std::sync::atomic::AtomicPtr<c_char>,
    dev: std::sync::atomic::AtomicU64,
    ino: std::sync::atomic::AtomicU64,
}

impl Endpoint {
    const fn new() -> Endpoint {
        Endpoint {
            path: std::sync::atomic::AtomicPtr::new(ptr::null_mut()),
            dev: std::sync::atomic::AtomicU64::new(0),
            ino: std::sync::atomic::AtomicU64::new(0),
        }
    }
}

unsafe fn identity(path: *const c_char) -> Option<(u64, u64)> {
    let mut st: libc::stat = std::mem::zeroed();
    (libc::lstat(path, &mut st) == 0).then_some((st.st_dev as u64, st.st_ino as u64))
}

// DAEMON-10: `path` must outlive the process.
unsafe fn record_endpoint(which: usize, path: *const c_char) {
    if let Some((dev, ino)) = identity(path) {
        let e = &ENDPOINTS[which];
        e.dev.store(dev, Ordering::SeqCst);
        e.ino.store(ino, Ordering::SeqCst);
        e.path.store(path as *mut c_char, Ordering::SeqCst);
    }
}

// DAEMON-10: callable from a signal handler.
pub(crate) unsafe fn morloc_daemon_remove_endpoints() {
    for e in &ENDPOINTS {
        let path = e.path.swap(ptr::null_mut(), Ordering::SeqCst);
        if !path.is_null() && identity(path) == Some((e.dev.load(Ordering::SeqCst), e.ino.load(Ordering::SeqCst))) {
            libc::unlink(path);
        }
    }
}

// DAEMON-6: one of the watchdog and the normal exit ends the process.
pub(crate) fn morloc_claim_exit() -> bool {
    !EXIT_CLAIMED.swap(true, Ordering::SeqCst)
}
static G_EVAL_TIMEOUT: AtomicI32 = AtomicI32::new(30);

// Daemon result-form policy (set in `daemon_run` from `DaemonConfig`).
// `G_DAEMON_OUTPUT_PACKET` selects raw-packet output for `call` results over
// the length-prefixed (Unix socket / TCP) transports; `G_DAEMON_COMPRESSION`
// is the zstd preset applied to those packets. HTTP and control methods are
// unaffected -- see `CURRENT_OUTPUT_PACKET` below.
static G_DAEMON_OUTPUT_PACKET: AtomicBool = AtomicBool::new(false);
static G_DAEMON_COMPRESSION: AtomicU8 = AtomicU8::new(0);

thread_local! {
    /// Per-thread flag: when set, `daemon_dispatch`'s `call` path returns a
    /// raw morloc data packet on `resp.result_bytes` instead of a JSON string
    /// on `resp.result_json`. Set by `handle_lp_connection` (the only caller
    /// that speaks the raw-packet wire) for the duration of one dispatch, and
    /// restored afterward; the HTTP handler and MCP/router callers never set
    /// it, so their dispatches stay JSON even on a packet-configured daemon.
    static CURRENT_OUTPUT_PACKET: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// Read the current thread's packet-output flag.
fn current_output_packet() -> bool {
    CURRENT_OUTPUT_PACKET.with(|c| c.get())
}

/// Set the current thread's packet-output flag, returning the previous value
/// so the caller can restore it after the dispatch completes.
fn set_current_output_packet(new: bool) -> bool {
    CURRENT_OUTPUT_PACKET.with(|c| {
        let old = c.get();
        c.set(new);
        old
    })
}

thread_local! {
    /// Per-thread flag: when set, `daemon_dispatch` does `?render=` output-
    /// projection resolution against the command's terminals (the `@default`
    /// terminal when no render is given). HTTP-only -- LP/MCP callers select
    /// their projection before dispatch, so this stays false for them. See also
    /// `CURRENT_OUTPUT_MEDIA_BYTES`, which governs the raw-bytes response form
    /// and is shared with the in-process MCP server.
    static CURRENT_OUTPUT_HTTP: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };

    /// Per-thread flag: when set, `daemon_dispatch`'s `call` path returns the
    /// raw content bytes of a media-typed (`@mime`) return on
    /// `resp.result_bytes` plus the media type on `resp.mime`, instead of a JSON
    /// string. Set for one dispatch and restored afterward by the two callers
    /// that can consume raw media -- `handle_http_connection` (emits a
    /// `Content-Type`) and the in-process MCP server (base64s the bytes into a
    /// content block, via `daemon_set_output_media_bytes`). Off for everyone
    /// else, so their dispatches stay JSON.
    static CURRENT_OUTPUT_MEDIA_BYTES: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// Read the current thread's HTTP-output flag.
fn current_output_http() -> bool {
    CURRENT_OUTPUT_HTTP.with(|c| c.get())
}

/// Read the current thread's raw-media-output flag.
fn current_output_media_bytes() -> bool {
    CURRENT_OUTPUT_MEDIA_BYTES.with(|c| c.get())
}

/// Set the current thread's raw-media-output flag, returning the previous value.
fn set_current_output_media_bytes(new: bool) -> bool {
    CURRENT_OUTPUT_MEDIA_BYTES.with(|c| {
        let old = c.get();
        c.set(new);
        old
    })
}

/// C-ABI entry point for the in-process MCP server to request the raw-media
/// response form for its next `daemon_dispatch` on this thread (a media-typed
/// `@mime` return comes back as raw content bytes + media type instead of a
/// JSON int array). Returns the previous value so the caller can restore it.
/// The HTTP handler sets the same flag directly. Off by default.
pub(crate) fn daemon_set_output_media_bytes(on: bool) -> bool {
    set_current_output_media_bytes(on)
}

/// Set the current thread's HTTP-output flag, returning the previous value.
fn set_current_output_http(new: bool) -> bool {
    CURRENT_OUTPUT_HTTP.with(|c| {
        let old = c.get();
        c.set(new);
        old
    })
}

/// Resolve the HTTP output projection for a `/call`: the command to actually
/// dispatch given `?render=<flag>`. Null render = fire the `@default` terminal
/// (matching the CLI's no-flag behavior); `"raw"` = the command's own typed
/// value (the `-f json` analog); a flag = that terminal's entry command. `Err`
/// for an unknown render flag.
unsafe fn resolve_render_target<'a>(
    mv: *const crate::manifest_ffi::Manifest,
    cmd: &'a crate::manifest_ffi::ManifestCommand,
    render: *const c_char,
) -> Result<&'a crate::manifest_ffi::ManifestCommand, String> {
    // The terminal the request names, or the `@default` one when it names
    // none; no terminal at all means the command's own typed value.
    let (terminal, named) = if render.is_null() {
        (find_terminal(cmd, None), None)
    } else {
        let r = CStr::from_ptr(render).to_str().unwrap_or("");
        if r == "raw" {
            return Ok(cmd);
        }
        match find_terminal(cmd, Some(r)) {
            Some(t) => (Some(t), Some(r)),
            None => {
                return Err(format!(
                    "unknown render '{}' for command '{}'",
                    r,
                    CStr::from_ptr(cmd.name).to_string_lossy()
                ))
            }
        }
    };
    let Some(t) = terminal else { return Ok(cmd) };
    // An action with no entry applies to the command's whole streamed output,
    // which only the command line saves and replays.
    if t.entry.is_null() {
        let which = match named {
            Some(r) => format!("render '{}'", r),
            None => "the default render".to_string(),
        };
        return Err(format!(
            "{} of command '{}' runs only from the command line; request render=raw \
             for the command's own output",
            which,
            CStr::from_ptr(cmd.name).to_string_lossy()
        ));
    }
    let m: &'a crate::manifest_ffi::Manifest = &*mv;
    m.command_by_name(CStr::from_ptr(t.entry)).ok_or_else(|| {
        format!(
            "render entry '{}' not found",
            CStr::from_ptr(t.entry).to_string_lossy()
        )
    })
}

/// The first terminal whose long flag is `match_long`, or with `None` the
/// `@default` terminal.
unsafe fn find_terminal<'a>(
    cmd: &'a crate::manifest_ffi::ManifestCommand,
    match_long: Option<&str>,
) -> Option<&'a crate::manifest_ffi::ManifestTerminal> {
    (0..cmd.n_terminals).map(|i| &*cmd.terminals.add(i)).find(|t| match match_long {
        Some(l) => !t.long.is_null() && CStr::from_ptr(t.long).to_str().map_or(false, |x| x == l),
        None => t.default,
    })
}
// Eval sandbox policy for served eval/bind. When G_EVAL_SANDBOX is set, the
// forked `morloc eval` runs with `--eval-sandbox` (+ the allow-list), so it
// refuses directly-written IO intrinsics and imports outside the list. Read
// once in the PARENT before fork (never locked in the async-signal-unsafe
// post-fork child).
static G_EVAL_SANDBOX: AtomicBool = AtomicBool::new(false);
static G_EVAL_ALLOWED: Mutex<Option<String>> = Mutex::new(None);

// NET-1
struct HttpAccess {
    address: u32,
    token: Option<String>,
}

static HTTP_ACCESS: Mutex<HttpAccess> = Mutex::new(HttpAccess { address: 0x7f00_0001, token: None });

fn http_access() -> std::sync::MutexGuard<'static, HttpAccess> {
    // PANIC-4
    HTTP_ACCESS.lock().unwrap_or_else(|_| morloc_runtime_types::panic::poisoned_lock())
}

/// The bearer token every HTTP request must carry, if one was set.
pub(crate) fn http_token() -> Option<String> {
    http_access().token.clone()
}

/// Set true while the daemon is performing pool-crash recovery: SIGTERM/KILL
/// pools, drop SHM, respawn, etc. Workers must bail out of any incoming or
/// in-flight request immediately when this is set, returning a "recovering"
/// error to the client rather than touching pool sockets or SHM. Cleared once
/// the new pools are pingable. See `nexus::process::pool_check_and_recover`.
pub static RECOVERY_IN_PROGRESS: AtomicBool = AtomicBool::new(false);

/// True while the daemon is recovering from a pool crash. Public read-only
/// helper for callers that need a one-line guard at the top of a request
/// handler.
#[inline]
pub fn is_recovering() -> bool {
    RECOVERY_IN_PROGRESS.load(Ordering::Acquire)
}

/// Mark recovery as starting. Returns `false` if recovery was already in
/// progress (caller should treat that as "someone else got here first" and
/// skip the recovery sequence).
pub fn begin_recovery() -> bool {
    let _requests = requests_in_flight();
    RECOVERY_IN_PROGRESS
        .compare_exchange(false, true, Ordering::SeqCst, Ordering::SeqCst)
        .is_ok()
}

static REQUESTS_IN_FLIGHT: Mutex<usize> = Mutex::new(0);
static REQUESTS_DRAINED: Condvar = Condvar::new();

fn requests_in_flight() -> std::sync::MutexGuard<'static, usize> {
    REQUESTS_IN_FLIGHT.lock().unwrap_or_else(|_| morloc_runtime_types::panic::poisoned_lock())
}

pub(crate) struct InFlight(());

impl Drop for InFlight {
    fn drop(&mut self) {
        let mut n = requests_in_flight();
        *n -= 1;
        if *n == 0 {
            REQUESTS_DRAINED.notify_all();
        }
    }
}

pub(crate) fn enter_request() -> Option<InFlight> {
    let mut n = requests_in_flight();
    // DAEMON-6
    if RECOVERY_IN_PROGRESS.load(Ordering::SeqCst) || SHUTDOWN_REQUESTED.load(Ordering::SeqCst) {
        return None;
    }
    *n += 1;
    Some(InFlight(()))
}

pub fn wait_for_requests(timeout: std::time::Duration) -> bool {
    let n = requests_in_flight();
    let (n, _) = REQUESTS_DRAINED
        .wait_timeout_while(n, timeout, |n| *n > 0)
        .unwrap_or_else(|_| morloc_runtime_types::panic::poisoned_lock());
    *n == 0
}

/// Mark recovery as complete. Workers will start accepting requests again.
pub fn end_recovery() {
    RECOVERY_IN_PROGRESS.store(false, Ordering::Release);
}

/// True once the daemon has begun graceful shutdown (SIGTERM received,
/// `daemon_run` main loop is exiting). Recovery should bail in this case
/// rather than fight the shutdown by respawning pools the daemon is
/// trying to tear down.
pub fn is_shutting_down() -> bool {
    SHUTDOWN_REQUESTED.load(Ordering::Acquire)
}

// ── C-ABI wrappers for cross-library access ────────────────────────────────
//
// The nexus calls these via `extern "C"` declarations that resolve at
// load time against libmorloc.so (DT_NEEDED). The Rust-side fns
// (is_shutting_down/begin_recovery/end_recovery) cannot be used from
// the nexus because the nexus no longer links morloc-runtime as an
// rlib -- and even when it did, linking the rlib gave the nexus its
// own disjoint copy of RECOVERY_IN_PROGRESS / SHUTDOWN_REQUESTED,
// silently breaking recovery coordination with libmorloc.so's daemon
// loop. Going through the C ABI ensures both ends touch the same
// atomics.

pub(crate) unsafe fn morloc_daemon_is_shutting_down() -> bool {
    is_shutting_down()
}

pub(crate) unsafe fn morloc_daemon_begin_recovery() -> bool {
    begin_recovery()
}

pub(crate) fn morloc_daemon_wait_for_requests(timeout_ms: u64) -> bool {
    wait_for_requests(std::time::Duration::from_millis(timeout_ms))
}

pub(crate) unsafe fn morloc_daemon_end_recovery() {
    end_recovery()
}

type PoolAliveFn = unsafe extern "C" fn(usize) -> bool;
static POOL_STATUS: Mutex<(Option<PoolAliveFn>, usize)> = Mutex::new((None, 0));
static BINDING_STORE: Mutex<Option<BindingStore>> = Mutex::new(None);
static BINDING_FINISHED: Condvar = Condvar::new();

fn binding_store() -> std::sync::MutexGuard<'static, Option<BindingStore>> {
    BINDING_STORE.lock().unwrap_or_else(|_| morloc_runtime_types::panic::poisoned_lock())
}

enum BindClaim {
    NoStore,
    Bound,
    Compile(String),
}

fn claim_binding(hv: u64, name: Option<&str>) -> BindClaim {
    let mut guard = binding_store();
    loop {
        let Some(store) = guard.as_mut() else { return BindClaim::NoStore };
        if store.name_if_bound(hv, name) {
            return BindClaim::Bound;
        }
        if store.compiling.insert(hv) {
            return BindClaim::Compile(store.base_dir.clone());
        }
        guard = BINDING_FINISHED.wait(guard).unwrap_or_else(|_| morloc_runtime_types::panic::poisoned_lock());
    }
}

fn finish_binding(hv: u64, expr: &str, name: Option<&str>, artifact_dir: Option<String>) {
    if let Some(store) = binding_store().as_mut() {
        store.compiling.remove(&hv);
        if let Some(dir) = artifact_dir {
            store.insert(hv, expr, dir, name);
        }
    }
    BINDING_FINISHED.notify_all();
}

// -- C-compatible types -------------------------------------------------------

// Re-export MorlocSocket from the types crate so nexus and libmorloc.so
// share the canonical layout without redefining it on either side.
pub use morloc_runtime_types::daemon_socket::MorlocSocket;

/// Matches daemon_config_t from daemon.h.
///
/// Port sentinel convention:
/// - `tcp_port` / `http_port` == -1 -> listener not configured.
/// - `tcp_port` / `http_port` ==  0 -> bind ephemeral; OS picks a port.
/// - 0..=65535 otherwise -> bind that specific port.
///
/// `port_file_path` is optional; when non-null, daemon_run writes a JSON
/// blob `{"http": N|null, "tcp": N|null, "unix": "PATH"|null}` to that
/// path (atomically via rename) after all listeners are bound. This is
/// the race-free orchestration channel for harnesses spawning many
/// daemons in parallel.
#[repr(C)]
pub struct DaemonConfig {
    pub unix_socket_path: *const c_char,
    pub tcp_port: i32,
    pub http_port: i32,
    pub port_file_path: *const c_char,
    pub pool_check_fn: Option<unsafe extern "C" fn(*mut MorlocSocket, usize)>,
    pub pool_alive_fn: Option<unsafe extern "C" fn(usize) -> bool>,
    pub n_pools: usize,
    pub eval_timeout: i32,
    /// When true, `call` results over the Unix socket / TCP transports are
    /// returned as a raw morloc data packet instead of a JSON envelope.
    pub output_packet: bool,
    /// zstd preset (0..=9) for `output_packet` results; 0 = no compression.
    pub compression_level: u8,
    // DAEMON-6
    pub stop_pools_fn: Option<unsafe extern "C" fn()>,
    // DAEMON-6
    pub emergency_exit_fn: Option<unsafe extern "C" fn(i32)>,
}

/// Error classification for a daemon dispatch failure.
///
/// `success = true` -> `error_kind = OK`. On failure, the kind drives
/// HTTP status mapping in `handle_http_connection` (and in the router's
/// equivalent). Unix/TCP clients keep reading the `success` boolean and
/// the `{"status":"error","error":"..."}` JSON envelope; they don't see
/// the kind on the wire today.
pub const DAEMON_ERROR_OK:          i32 = 0;
pub const DAEMON_ERROR_BAD_REQUEST: i32 = 1;
pub const DAEMON_ERROR_NOT_FOUND:   i32 = 2;
pub const DAEMON_ERROR_TIMEOUT:     i32 = 3;
pub const DAEMON_ERROR_RECOVERING:  i32 = 4;
pub const DAEMON_ERROR_INTERNAL:    i32 = 5;

/// Translate a `DAEMON_ERROR_*` kind to its HTTP status code.
pub fn daemon_error_kind_to_http_status(kind: i32, success: bool) -> i32 {
    if success {
        return 200;
    }
    match kind {
        DAEMON_ERROR_BAD_REQUEST => 400,
        DAEMON_ERROR_NOT_FOUND   => 404,
        DAEMON_ERROR_TIMEOUT     => 408,
        DAEMON_ERROR_RECOVERING  => 503,
        // OK with success=false shouldn't happen, but treat as internal.
        _ => 500,
    }
}

/// Matches daemon_response_t from daemon.h. `error_kind` is one of the
/// `DAEMON_ERROR_*` constants above; see `daemon_error_kind_to_http_status`.
#[repr(C)]
pub struct DaemonResponse {
    pub id: *mut c_char,
    pub success: bool,
    pub error_kind: i32,
    pub result_json: *mut c_char,
    pub error: *mut c_char,
    /// Raw morloc data-packet bytes for the `-f packet` daemon wire. Null in
    /// JSON mode (the default); non-null is the tag that
    /// `handle_lp_connection` writes these bytes verbatim instead of the JSON
    /// envelope. `libc::malloc`'d by the packet normalizer; freed in
    /// `daemon_free_response`. Appended after the original fields so the C ABI
    /// offsets of `id..error` (mirrored by nexus-side readers) are unchanged.
    pub result_bytes: *mut u8,
    pub result_len: usize,
    /// Media type (`@mime`) of a media-typed return, set alongside
    /// `result_bytes` when serving an HTTP request; the HTTP handler emits it as
    /// the `Content-Type`. Null in JSON/packet modes. `libc::strdup`'d; freed in
    /// `daemon_free_response`. Appended last so earlier C-ABI offsets are stable.
    pub mime: *mut c_char,
}

// -- Binding store (replaces linear-probe hash table with HashMap) ------------

struct BindingEntry {
    hash: u64,
    expr: String,
    #[allow(dead_code)]
    artifact_dir: String,
    type_sig: Option<String>,
    names: Vec<String>,
}

pub struct BindingStore {
    entries: HashMap<u64, BindingEntry>,
    /// Index from name -> hash for name-based lookup
    name_index: HashMap<String, u64>,
    base_dir: String,
    compiling: std::collections::HashSet<u64>,
}

impl BindingStore {
    fn new(base_dir: &str) -> Self {
        let _ = std::fs::create_dir_all(base_dir);
        BindingStore {
            entries: HashMap::new(),
            name_index: HashMap::new(),
            base_dir: base_dir.to_string(),
            compiling: std::collections::HashSet::new(),
        }
    }

    fn lookup_hash(&self, hash: u64) -> Option<&BindingEntry> {
        self.entries.get(&hash)
    }

    fn lookup_name(&self, name: &str) -> Option<&BindingEntry> {
        let hash = self.name_index.get(name)?;
        self.entries.get(hash)
    }

    fn add_name(&mut self, hash: u64, name: &str) {
        if let Some(entry) = self.entries.get_mut(&hash) {
            if !entry.names.contains(&name.to_string()) {
                entry.names.push(name.to_string());
            }
        }
        self.name_index.insert(name.to_string(), hash);
    }

    fn name_if_bound(&mut self, hv: u64, name: Option<&str>) -> bool {
        if !self.entries.contains_key(&hv) {
            return false;
        }
        if let Some(n) = name {
            self.add_name(hv, n);
        }
        true
    }

    fn insert(&mut self, hv: u64, expr: &str, artifact_dir: String, name: Option<&str>) {
        self.entries.entry(hv).or_insert_with(|| BindingEntry {
            hash: hv,
            expr: expr.to_string(),
            artifact_dir,
            type_sig: None,
            names: Vec::new(),
        });
        if let Some(n) = name {
            self.add_name(hv, n);
        }
    }

    fn list_json(&self) -> String {
        #[derive(serde::Serialize)]
        struct BindingInfo {
            hash: String,
            expr: String,
            #[serde(skip_serializing_if = "Option::is_none")]
            r#type: Option<String>,
            names: Vec<String>,
        }
        #[derive(serde::Serialize)]
        struct BindingsList {
            bindings: Vec<BindingInfo>,
        }
        let bindings: Vec<BindingInfo> = self
            .entries
            .values()
            .map(|e| BindingInfo {
                hash: format!("{:016x}", e.hash),
                expr: e.expr.clone(),
                r#type: e.type_sig.clone(),
                names: e.names.clone(),
            })
            .collect();
        serde_json::to_string(&BindingsList { bindings }).unwrap_or_default()
    }

    fn unbind(&mut self, name: &str) -> bool {
        let hash = match self.name_index.remove(name) {
            Some(h) => h,
            None => return false,
        };
        if let Some(entry) = self.entries.get_mut(&hash) {
            entry.names.retain(|n| n != name);
        }
        true
    }
}

unsafe fn two_pipes(a: &mut [i32; 2], b: &mut [i32; 2]) -> bool {
    if morloc_runtime_types::fd::pipe(a.as_mut_ptr()) != 0 {
        return false;
    }
    if morloc_runtime_types::fd::pipe(b.as_mut_ptr()) != 0 {
        libc::close(a[0]);
        libc::close(a[1]);
        return false;
    }
    true
}

// DAEMON-6: the leader stays unreaped until its pin closes, so its group id
// stays its own while the group is signalled; it passes on its child's
// end, by status or by signal.
const EVAL_WRAPPER: &str = "trap : TERM
if [ \"$1\" -gt 0 ]; then ulimit -t $(($1 + 5)) && ulimit -S -t \"$1\" || exit 126; fi
shift
\"$@\" 3<&- </dev/null
s=$?
trap '' TERM
exec 1>&- 2>&-
read _ <&3
if [ \"$s\" -gt 128 ] && [ \"$s\" -le 192 ]; then ulimit -c 0; trap - TERM; kill -$((s - 128)) $$; fi
exit \"$s\"";

static EVAL_CHILDREN: morloc_runtime_types::child_group::ChildGroups =
    morloc_runtime_types::child_group::ChildGroups::new();

pub(crate) fn morloc_stop_child_groups() {
    EVAL_CHILDREN.stop_all();
}

// DAEMON-11
pub(crate) fn morloc_child_group_leader_exited(pid: libc::c_int) {
    EVAL_CHILDREN.leader_exited(pid);
}

struct EvalChild {
    pid: libc::pid_t,
    since: u64,
    pin: i32,
    group: Option<morloc_runtime_types::child_group::Registered<'static>>,
}

impl EvalChild {
    fn signal(&self, sig: libc::c_int) {
        if let Some(g) = &self.group {
            g.signal(sig);
        }
    }

    // DAEMON-6: unregistered before the pin closes, so no signal reaches a reaped leader.
    fn release(&mut self) {
        drop(self.group.take());
        if self.pin >= 0 {
            unsafe { libc::close(self.pin) };
            self.pin = -1;
        }
    }

    unsafe fn finish(mut self) -> Option<i32> {
        self.release();
        wait_child(self.pid, self.since)
    }
}

impl Drop for EvalChild {
    fn drop(&mut self) {
        self.release();
    }
}

// DAEMON-6: a source must not be overwritten by an earlier dup2 onto 1, 2 or 3.
unsafe fn above_stdio(fd: i32) -> std::io::Result<i32> {
    let moved = libc::fcntl(fd, libc::F_DUPFD_CLOEXEC, 10);
    if moved < 0 {
        Err(std::io::Error::last_os_error())
    } else {
        Ok(moved)
    }
}

unsafe fn spawn_morloc(
    argv: &[*const c_char],
    stdout_pipe: &[i32; 2],
    stderr_pipe: &[i32; 2],
    cpu_seconds: i32,
) -> std::io::Result<EvalChild> {
    let mut pin = [0i32; 2];
    if morloc_runtime_types::fd::pipe(pin.as_mut_ptr()) != 0 {
        return Err(std::io::Error::last_os_error());
    }
    let (_env, envp) = morloc_runtime_types::spawn::current_environment();
    let since = morloc_reaped_sequence();
    let fixed: Vec<CString> = ["sh", "-c", EVAL_WRAPPER, "sh", &cpu_seconds.max(0).to_string()]
        .iter()
        .map(|a| CString::new(*a).unwrap())
        .collect();
    let sh_argv: Vec<*const c_char> = fixed.iter().map(|a| a.as_ptr()).chain(argv.iter().copied()).collect();
    let mut moved: Vec<i32> = Vec::new();
    let started = (|| {
        for fd in [stdout_pipe[1], stderr_pipe[1], pin[0]] {
            moved.push(above_stdio(fd)?);
        }
        let mut spawn = morloc_runtime_types::spawn::Spawn::new()?;
        spawn.new_process_group()?;
        spawn.dup2(moved[0], libc::STDOUT_FILENO)?;
        spawn.dup2(moved[1], libc::STDERR_FILENO)?;
        spawn.dup2(moved[2], 3)?;
        spawn.run(&CString::new("/bin/sh").unwrap(), &sh_argv, &envp, false)
    })();
    for fd in moved {
        libc::close(fd);
    }
    libc::close(pin[0]);
    let pid = match started {
        Ok(pid) => pid,
        Err(e) => {
            libc::close(pin[1]);
            return Err(e);
        }
    };
    match EVAL_CHILDREN.add(pid) {
        Some(group) => Ok(EvalChild { pid, since, pin: pin[1], group: Some(group) }),
        None => {
            // DAEMON-6: the leader is pinned, so the group is still its own.
            libc::kill(-pid, libc::SIGKILL);
            libc::close(pin[1]);
            let _ = wait_child(pid, since);
            Err(std::io::Error::other("too many forked morloc processes at once"))
        }
    }
}

fn compile_binding(base_dir: &str, hv: u64, expr: &str, eval_timeout: i32) -> Option<String> {
    let hash_hex = format!("{:016x}", hv);
    let artifact_dir = format!("{}/{}", base_dir, hash_hex);
    // Fork morloc eval --save
    unsafe {
        let mut stdout_pipe = [0i32; 2];
        let mut stderr_pipe = [0i32; 2];
        if !two_pipes(&mut stdout_pipe, &mut stderr_pipe) {
            return None;
        }

        // Build argv in the PARENT (the policy read locks a mutex, which
        // is unsafe in the post-fork child). These outlive the fork.
        let cmd = CString::new("morloc").unwrap();
        let arg_eval = CString::new("eval").unwrap();
        let arg_save = CString::new("--save").unwrap();
        let arg_hex = CString::new(hash_hex.as_str()).unwrap();
        // `morloc eval` takes a script file by default; the binding store
        // supplies an inline expression.
        let arg_dash_e = CString::new("-e").unwrap();
        // `expr` is client-supplied; an interior NUL cannot be exec'd.
        // Fail this bind cleanly rather than panicking the worker thread.
        let arg_expr = match CString::new(expr) {
            Ok(c) => c,
            Err(_) => {
                libc::close(stdout_pipe[0]);
                libc::close(stdout_pipe[1]);
                libc::close(stderr_pipe[0]);
                libc::close(stderr_pipe[1]);
                return None;
            }
        };
        let policy = eval_policy_args();
        let rts = eval_rts_args();
        // argv: morloc +RTS <heap> -RTS eval --save <hex> -e <policy...> <expr> NULL.
        let mut argv: Vec<*const c_char> =
            Vec::with_capacity(7 + rts.len() + policy.len());
        argv.push(cmd.as_ptr());
        for p in &rts {
            argv.push(p.as_ptr());
        }
        argv.push(arg_eval.as_ptr());
        argv.push(arg_save.as_ptr());
        argv.push(arg_hex.as_ptr());
        argv.push(arg_dash_e.as_ptr());
        for p in &policy {
            argv.push(p.as_ptr());
        }
        argv.push(arg_expr.as_ptr());
        argv.push(ptr::null());

        let child = match spawn_morloc(&argv, &stdout_pipe, &stderr_pipe, eval_timeout) {
            Ok(child) => child,
            Err(_) => {
                libc::close(stdout_pipe[0]);
                libc::close(stdout_pipe[1]);
                libc::close(stderr_pipe[0]);
                libc::close(stderr_pipe[1]);
                return None;
            }
        };

        // Parent
        libc::close(stdout_pipe[1]);
        libc::close(stderr_pipe[1]);

        let (_, stderr_buf, in_time) = drain_child(&child, stdout_pipe[0], stderr_pipe[0], eval_timeout);
        libc::close(stdout_pipe[0]);
        libc::close(stderr_pipe[0]);

        let ok = child
            .finish()
            .is_some_and(|st| libc::WIFEXITED(st) && libc::WEXITSTATUS(st) == 0);
        if !in_time {
            eprintln!(
                "binding_store_bind: morloc eval --save ran past its time limit ({} s of wall time) and was stopped",
                eval_timeout as u64 * EVAL_WALL_PER_CPU
            );
            return None;
        }
        if !ok {
            let msg = String::from_utf8_lossy(&stderr_buf);
            eprintln!("binding_store_bind: morloc eval --save failed: {}", msg);
            return None;
        }
    }
    Some(artifact_dir)
}

// -- C-exported binding store functions ---------------------------------------

pub(crate) unsafe fn binding_store_init(base_dir: *const c_char) -> *mut BindingStore {
    let dir = CStr::from_ptr(base_dir).to_string_lossy().into_owned();
    Box::into_raw(Box::new(BindingStore::new(&dir)))
}

pub(crate) unsafe fn binding_store_free(store: *mut BindingStore) {
    if !store.is_null() {
        drop(Box::from_raw(store));
    }
}

// -- Request parsing (serde_json) ---------------------------------------------

#[derive(serde::Deserialize)]
struct JsonRequest {
    id: Option<String>,
    method: Option<String>,
    command: Option<String>,
    /// Kept as the text the client sent: it goes to the pool verbatim,
    /// so an integer wider than a double keeps its digits and a value of
    /// any depth passes, neither of which a tree-shaped parse allows.
    args: Option<Box<serde_json::value::RawValue>>,
    expr: Option<String>,
    name: Option<String>,
    #[serde(default)]
    media: Option<bool>,
}

pub(crate) unsafe fn daemon_parse_request(json: *const c_char, len: usize, errmsg: *mut *mut c_char) -> *mut DaemonRequest {
    parse_request(json, len, errmsg)
}

pub(crate) unsafe fn parse_request(
    json: *const c_char,
    len: usize,
    errmsg: *mut *mut c_char,
) -> *mut DaemonRequest {
    clear_errmsg(errmsg);

    let slice = std::slice::from_raw_parts(json as *const u8, len);
    let text = match std::str::from_utf8(slice) {
        Ok(s) => s,
        Err(_) => {
            set_errmsg(errmsg, &MorlocError::Other("Invalid UTF-8 in request".into()));
            return ptr::null_mut();
        }
    };

    let parsed: JsonRequest = match serde_json::from_str(text) {
        Ok(r) => r,
        Err(e) => {
            set_errmsg(
                errmsg,
                &MorlocError::Other(format!("Failed to parse request JSON: {}", e)),
            );
            return ptr::null_mut();
        }
    };

    let req = libc::calloc(1, std::mem::size_of::<DaemonRequest>()) as *mut DaemonRequest;
    if req.is_null() {
        set_errmsg(
            errmsg,
            &MorlocError::Other("Failed to allocate daemon_request_t".into()),
        );
        return ptr::null_mut();
    }

    if let Some(id) = &parsed.id {
        let c = CString::new(id.as_str()).unwrap_or_default();
        (*req).id = libc::strdup(c.as_ptr());
    }

    if let Some(method) = &parsed.method {
        (*req).method = match method.as_str() {
            "call" => DaemonMethod::Call,
            "discover" => DaemonMethod::Discover,
            "health" => DaemonMethod::Health,
            "eval" => DaemonMethod::Eval,
            "typecheck" => DaemonMethod::Typecheck,
            "bind" => DaemonMethod::Bind,
            "bindings" => DaemonMethod::Bindings,
            "unbind" => DaemonMethod::Unbind,
            _ => {
                daemon_free_request(req);
                set_errmsg(
                    errmsg,
                    &MorlocError::Other(format!("Unknown method: {}", method)),
                );
                return ptr::null_mut();
            }
        };
    }

    if let Some(cmd) = &parsed.command {
        let c = CString::new(cmd.as_str()).unwrap_or_default();
        (*req).command = libc::strdup(c.as_ptr());
    }

    if let Some(args) = &parsed.args {
        let c = CString::new(args.get()).unwrap_or_default();
        (*req).args_json = libc::strdup(c.as_ptr());
    }

    if let Some(expr) = &parsed.expr {
        let c = CString::new(expr.as_str()).unwrap_or_default();
        (*req).expr = libc::strdup(c.as_ptr());
    }

    if let Some(name) = &parsed.name {
        let c = CString::new(name.as_str()).unwrap_or_default();
        (*req).name = libc::strdup(c.as_ptr());
    }

    (*req).media = parsed.media.unwrap_or(false);

    req
}

// -- Response parsing (serde_json) --------------------------------------------

#[derive(serde::Deserialize)]
struct JsonResponse {
    id: Option<String>,
    status: Option<String>,
    /// The value as text: it is user data of any depth and width, and
    /// passes through untouched.
    result: Option<Box<serde_json::value::RawValue>>,
    error: Option<String>,
    /// Media (`@mime`) return: base64 content + media type (see
    /// daemon_serialize_response). Reconstructed onto result_bytes/mime.
    result_b64: Option<String>,
    mime: Option<String>,
}

/// The response as written to the socket. The member order is the wire
/// order.
#[derive(serde::Serialize)]
struct WireResponse {
    #[serde(skip_serializing_if = "Option::is_none")]
    id: Option<String>,
    status: &'static str,
    #[serde(skip_serializing_if = "Option::is_none")]
    result_b64: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    mime: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    result: Option<Box<serde_json::value::RawValue>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<String>,
}

pub(crate) unsafe fn daemon_parse_response(
    json: *const c_char,
    len: usize,
    errmsg: *mut *mut c_char,
) -> *mut DaemonResponse {
    clear_errmsg(errmsg);

    let slice = std::slice::from_raw_parts(json as *const u8, len);
    let text = match std::str::from_utf8(slice) {
        Ok(s) => s,
        Err(_) => {
            set_errmsg(errmsg, &MorlocError::Other("Invalid UTF-8 in response".into()));
            return ptr::null_mut();
        }
    };

    let parsed: JsonResponse = match serde_json::from_str(text) {
        Ok(r) => r,
        Err(e) => {
            set_errmsg(
                errmsg,
                &MorlocError::Other(format!("Failed to parse response JSON: {}", e)),
            );
            return ptr::null_mut();
        }
    };

    let resp = libc::calloc(1, std::mem::size_of::<DaemonResponse>()) as *mut DaemonResponse;
    if resp.is_null() {
        set_errmsg(
            errmsg,
            &MorlocError::Other("Failed to allocate daemon_response_t".into()),
        );
        return ptr::null_mut();
    }

    if let Some(id) = &parsed.id {
        let c = CString::new(id.as_str()).unwrap_or_default();
        (*resp).id = libc::strdup(c.as_ptr());
    }

    (*resp).success = parsed
        .status
        .as_deref()
        .map(|s| s == "ok")
        .unwrap_or(false);
    // The wire JSON envelope does not carry error_kind, so a parsed
    // error response defaults to INTERNAL. Callers that received a real
    // dispatch response (not a re-parse) will already have the correct
    // kind set directly on DaemonResponse.
    (*resp).error_kind = if (*resp).success {
        DAEMON_ERROR_OK
    } else {
        DAEMON_ERROR_INTERNAL
    };

    // Media (`@mime`) return: decode base64 back to raw bytes + mime, mirroring
    // the in-process dispatch response so downstream renders identically.
    if let (Some(b64), Some(mime)) = (&parsed.result_b64, &parsed.mime) {
        use base64::Engine;
        // Fail closed: a decode or allocation error must not leave a "success"
        // response with a null payload (which downstream would render as a bare
        // JSON `null`). Convert it into a real error instead.
        let media_err: Option<String> =
            match base64::engine::general_purpose::STANDARD.decode(b64) {
                Ok(bytes) => {
                    let len = bytes.len();
                    let buf = libc::malloc(len.max(1)) as *mut u8;
                    if buf.is_null() {
                        Some("Failed to allocate media result buffer".to_string())
                    } else {
                        ptr::copy_nonoverlapping(bytes.as_ptr(), buf, len);
                        (*resp).result_bytes = buf;
                        (*resp).result_len = len;
                        let c = CString::new(mime.as_str()).unwrap_or_default();
                        (*resp).mime = libc::strdup(c.as_ptr());
                        None
                    }
                }
                Err(e) => Some(format!("Failed to decode media result: {}", e)),
            };
        if let Some(msg) = media_err {
            (*resp).success = false;
            (*resp).error_kind = DAEMON_ERROR_INTERNAL;
            let c = CString::new(msg).unwrap_or_default();
            (*resp).error = libc::strdup(c.as_ptr());
        }
    } else if let Some(result) = &parsed.result {
        let c = CString::new(result.get()).unwrap_or_default();
        (*resp).result_json = libc::strdup(c.as_ptr());
    }

    if let Some(error) = &parsed.error {
        let c = CString::new(error.as_str()).unwrap_or_default();
        (*resp).error = libc::strdup(c.as_ptr());
    }

    resp
}

// -- Free functions -----------------------------------------------------------

pub(crate) unsafe fn daemon_free_request(req: *mut DaemonRequest) {
    if req.is_null() {
        return;
    }
    if !(*req).id.is_null() {
        libc::free((*req).id as *mut c_void);
    }
    if !(*req).command.is_null() {
        libc::free((*req).command as *mut c_void);
    }
    if !(*req).args_json.is_null() {
        libc::free((*req).args_json as *mut c_void);
    }
    if !(*req).expr.is_null() {
        libc::free((*req).expr as *mut c_void);
    }
    if !(*req).name.is_null() {
        libc::free((*req).name as *mut c_void);
    }
    if !(*req).render.is_null() {
        libc::free((*req).render as *mut c_void);
    }
    libc::free(req as *mut c_void);
}

pub(crate) unsafe fn daemon_free_response(resp: *mut DaemonResponse) {
    if resp.is_null() {
        return;
    }
    if !(*resp).id.is_null() {
        libc::free((*resp).id as *mut c_void);
    }
    if !(*resp).result_json.is_null() {
        libc::free((*resp).result_json as *mut c_void);
    }
    if !(*resp).error.is_null() {
        libc::free((*resp).error as *mut c_void);
    }
    if !(*resp).result_bytes.is_null() {
        libc::free((*resp).result_bytes as *mut c_void);
    }
    if !(*resp).mime.is_null() {
        libc::free((*resp).mime as *mut c_void);
    }
    libc::free(resp as *mut c_void);
}

// -- Response serialization (serde_json) --------------------------------------

pub(crate) unsafe fn daemon_serialize_response(response: *mut DaemonResponse, out_len: *mut usize) -> *mut c_char {
    serialize_response(response, out_len)
}

pub(crate) unsafe fn serialize_response(
    response: *mut DaemonResponse,
    out_len: *mut usize,
) -> *mut c_char {
    use serde_json::value::RawValue;
    let owned = |p: *const c_char| (!p.is_null()).then(|| CStr::from_ptr(p).to_string_lossy().into_owned());
    let mut wire = WireResponse {
        id: owned((*response).id),
        status: if (*response).success { "ok" } else { "error" },
        result_b64: None,
        mime: None,
        result: None,
        error: None,
    };

    // A media-typed (`@mime`) return carries raw bytes + a media type instead of
    // a JSON value. Convey them across the socket wire as base64 + mime so the
    // JSON envelope stays valid (the receiver reconstructs result_bytes/mime);
    // reuse this for the front-end forward, which renders the media itself.
    if (*response).success && !(*response).mime.is_null() && !(*response).result_bytes.is_null() {
        use base64::Engine;
        let bytes = std::slice::from_raw_parts((*response).result_bytes, (*response).result_len);
        wire.result_b64 = Some(base64::engine::general_purpose::STANDARD.encode(bytes));
        wire.mime = owned((*response).mime);
    } else if (*response).success && !(*response).result_json.is_null() {
        // The value goes out as the text it is; text that is not JSON is
        // carried as a JSON string.
        let raw = CStr::from_ptr((*response).result_json).to_string_lossy();
        wire.result = match serde_json::from_str::<&RawValue>(&raw) {
            Ok(_) => RawValue::from_string(raw.into_owned()).ok(),
            Err(_) => serde_json::to_string(raw.as_ref()).ok().and_then(|s| RawValue::from_string(s).ok()),
        };
    }

    if !(*response).success {
        wire.error = owned((*response).error);
    }

    let json_str = serde_json::to_string(&wire).unwrap_or_else(|_| "{}".into());
    if !out_len.is_null() {
        *out_len = json_str.len();
    }
    let c = CString::new(json_str).unwrap_or_default();
    libc::strdup(c.as_ptr())
}

// -- Discovery ----------------------------------------------------------------

pub(crate) unsafe fn daemon_build_discovery(manifest: *mut crate::manifest_ffi::Manifest) -> *mut c_char {
    use crate::manifest_ffi::manifest_to_discovery_json;
    manifest_to_discovery_json(manifest)
}

// -- Eval timeout -------------------------------------------------------------

pub(crate) fn daemon_set_eval_timeout(timeout_sec: i32) {
    let t = if timeout_sec > 0 { timeout_sec } else { 30 };
    G_EVAL_TIMEOUT.store(t, Ordering::Relaxed);
}

/// Set the eval sandbox policy applied to forked `morloc eval`/`--save`.
/// `sandbox` enables the sandbox; `allowed` is a NUL-terminated, comma-
/// separated module allow-list (may be null/empty). The nexus calls this
/// once before serving; the global is process-wide, so every serve path
/// (daemon, router, future MCP eval) is covered.
///
/// # Safety
///
/// `allowed` must be null or a NUL-terminated string.
/// Set the HTTP listener's IPv4 address (host order) and the bearer token
/// every HTTP request must carry; a null `token` requires none.
///
/// # Safety
///
/// `token` must be null or a NUL-terminated string.
// NET-1
pub(crate) unsafe fn daemon_set_http_access(address: u32, token: *const c_char) {
    let token = (!token.is_null()).then(|| CStr::from_ptr(token).to_string_lossy().into_owned());
    *http_access() = HttpAccess { address, token };
}

pub(crate) unsafe fn daemon_set_eval_policy(sandbox: bool, allowed: *const c_char) {
    G_EVAL_SANDBOX.store(sandbox, Ordering::Relaxed);
    let list = if allowed.is_null() {
        None
    } else {
        // Safe: `allowed` is a caller-owned C string valid for this call.
        unsafe { CStr::from_ptr(allowed) }
            .to_str()
            .ok()
            .filter(|s| !s.is_empty())
            .map(|s| s.to_string())
    };
    *G_EVAL_ALLOWED.lock().unwrap() = list;
}

/// Heap ceiling for a forked `morloc`, written as a GHC RTS argument.
///
/// The child is a GHC-compiled binary, and `RLIMIT_AS` is the wrong instrument
/// for one. Its runtime reserves roughly a terabyte of address space at startup
/// -- untouched, so it costs no memory -- and under an `RLIMIT_AS` it shrinks
/// that reservation until it fits, which lands it just under the cap. There is
/// then no address space left for the per-capability OS thread stacks it
/// creates next, so the process dies before running any code with "failed to
/// create OS thread" on any host with enough cores. `-M` bounds the live heap
/// instead, which is the quantity actually worth bounding, and overflows
/// cleanly and diagnosably. Mirrored in morloc-nexus's `mcp.rs`.
const EVAL_HEAP_LIMIT: &str = "-M2G";

/// 1 ms polls spent waiting for the SIGCHLD handler to publish a status it
/// has already reaped. The gap is a handful of instructions; the bound only
/// has to outlast a descheduled handler thread.
const NOTED_EXIT_POLLS: usize = 100;

/// The RTS block bounding a forked `morloc`'s heap, as argv entries. The child
/// runtime consumes and strips `+RTS ... -RTS` before the program sees argv, so
/// it can sit anywhere; it goes first, keeping it clear of the expression.
/// Built in the PARENT (it allocates) like the policy args below.
fn eval_rts_args() -> [CString; 3] {
    [
        CString::new("+RTS").unwrap(),
        CString::new(EVAL_HEAP_LIMIT).unwrap(),
        CString::new("-RTS").unwrap(),
    ]
}

/// Build the sandbox policy argv tail (`--eval-sandbox [--eval-allowed-modules
/// <list>]`) for a forked `morloc eval`. MUST be called in the PARENT before
/// fork: it locks G_EVAL_ALLOWED, which is not async-signal-safe to touch in
/// the post-fork child. The returned CStrings outlive the fork (the child
/// shares the parent's address space until exec).
fn eval_policy_args() -> Vec<CString> {
    let mut out = Vec::new();
    if G_EVAL_SANDBOX.load(Ordering::Relaxed) {
        out.push(CString::new("--eval-sandbox").unwrap());
        if let Some(list) = G_EVAL_ALLOWED.lock().unwrap().as_deref() {
            out.push(CString::new("--eval-allowed-modules").unwrap());
            out.push(CString::new(list).unwrap());
        }
    }
    out
}

// -- Reaped-child status exchange ---------------------------------------------
//
// The nexus reaps EVERY child from its SIGCHLD handler (`waitpid(-1)`), so a
// thread that forks its own child and waits for it usually finds the child
// already gone: `waitpid` fails with ECHILD and the exit status is lost.
// Reading an untouched status word then says "exited 0", which turns a
// compiler that refused the caller's expression into a successful run.
//
// The handler deposits every status it reaps here, and the waiter collects
// the one it is owed. A ring rather than a registry because the waiter cannot
// register before the fork (it has no pid yet) and cannot register after
// (the child may already be reaped): depositing unconditionally has no such
// window. Entries for children nobody is waiting on simply age out.

const REAPED_SLOTS: usize = 32;

static REAPED_PID: [AtomicI32; REAPED_SLOTS] = {
    const INIT: AtomicI32 = AtomicI32::new(0);
    [INIT; REAPED_SLOTS]
};
static REAPED_STATUS: [AtomicI32; REAPED_SLOTS] = {
    const INIT: AtomicI32 = AtomicI32::new(0);
    [INIT; REAPED_SLOTS]
};
static REAPED_SEQ: [AtomicU64; REAPED_SLOTS] = {
    const INIT: AtomicU64 = AtomicU64::new(0);
    [INIT; REAPED_SLOTS]
};
static REAPED_NEXT: AtomicU64 = AtomicU64::new(1);

/// Record a child the caller has already reaped, so whoever forked it can
/// still learn how it ended.
///
/// Called from the nexus's SIGCHLD handler and so must stay
/// async-signal-safe: atomics only, no allocation, no locks. The nexus warms
/// the PLT entry for this symbol before installing the handler, so the
/// in-handler call never triggers lazy symbol resolution.
pub(crate) fn morloc_note_child_exit(pid: i32, status: i32) {
    if pid <= 0 {
        return; // PLT warm-up call, or nothing to record
    }
    let seq = REAPED_NEXT.fetch_add(1, Ordering::SeqCst);
    let slot = (seq % REAPED_SLOTS as u64) as usize;
    REAPED_STATUS[slot].store(status, Ordering::Relaxed);
    REAPED_SEQ[slot].store(seq, Ordering::Relaxed);
    REAPED_PID[slot].store(pid, Ordering::SeqCst);
}

pub(crate) fn morloc_reaped_sequence() -> u64 {
    REAPED_NEXT.load(Ordering::SeqCst)
}

pub(crate) unsafe fn morloc_take_noted_child_exit(pid: i32, since: u64, status: *mut i32) -> i32 {
    match take_noted_child_exit(pid, since) {
        Some(s) => {
            if !status.is_null() {
                *status = s;
            }
            1
        }
        None => 0,
    }
}

/// Collect the exit status of `pid` if the SIGCHLD handler reaped it.
/// Consumes the entry so a recycled pid cannot be answered twice.
fn take_noted_child_exit(pid: i32, since: u64) -> Option<i32> {
    for i in 0..REAPED_SLOTS {
        if REAPED_PID[i].load(Ordering::SeqCst) == pid && REAPED_SEQ[i].load(Ordering::Relaxed) >= since {
            let status = REAPED_STATUS[i].load(Ordering::Relaxed);
            REAPED_PID[i].store(0, Ordering::Release);
            return Some(status);
        }
    }
    None
}

/// The exit status of child `pid`, whether this thread reaps it or the
/// SIGCHLD handler already has; `None` if neither yields one.
fn wait_child(pid: i32, since: u64) -> Option<i32> {
    let mut status: i32 = 0;
    loop {
        // SAFETY: waitpid writes only `status`.
        let rc = unsafe { libc::waitpid(pid, &mut status, 0) };
        if rc == pid {
            return Some(status);
        }
        if rc < 0 && std::io::Error::last_os_error().kind() == std::io::ErrorKind::Interrupted {
            continue;
        }
        break;
    }
    // The pipes the caller drained only reach EOF when the child exits, so
    // the handler has usually reaped it already; give it a moment to record
    // the status if the reap and the deposit straddle this point.
    for _ in 0..NOTED_EXIT_POLLS {
        if let Some(s) = take_noted_child_exit(pid, since) {
            return Some(s);
        }
        std::thread::sleep(std::time::Duration::from_millis(1));
    }
    None
}

// -- Fork-based eval/typecheck ------------------------------------------------

/// Fork `morloc <subcmd> <expr>`, capture stdout/stderr, return a DaemonResponse.
unsafe fn fork_morloc_command(subcmd: &str, expr: *const c_char) -> *mut DaemonResponse {
    let resp = libc::calloc(1, std::mem::size_of::<DaemonResponse>()) as *mut DaemonResponse;

    let mut stdout_pipe = [0i32; 2];
    let mut stderr_pipe = [0i32; 2];
    if !two_pipes(&mut stdout_pipe, &mut stderr_pipe) {
        (*resp).success = false;
        (*resp).error_kind = DAEMON_ERROR_INTERNAL;
        let c = CString::new(format!("Failed to create pipes for {}", subcmd)).unwrap_or_default();
        (*resp).error = libc::strdup(c.as_ptr());
        return resp;
    }

    // Build the exec argv in the PARENT (before fork): the sandbox policy
    // read locks a mutex, which is not async-signal-safe in the child. The
    // child shares this address space until exec, so these outlive the fork.
    let cmd = CString::new("morloc").unwrap();
    let arg_subcmd = CString::new(subcmd).unwrap();
    // `morloc eval`/`typecheck` take a script file by default; the daemon
    // always supplies an inline expression, so pass `-e`.
    let dash_e = CString::new("-e").unwrap();
    // The sandbox policy belongs to `eval`, which compiles AND RUNS the
    // caller's expression. `typecheck` infers a type and stops -- it
    // executes nothing and reads no sourced files -- and its parser
    // rejects these flags outright, so handing them over turned every
    // typecheck into an argument error before it ever saw the expression.
    let policy = if subcmd == "eval" { eval_policy_args() } else { Vec::new() };
    let rts = eval_rts_args();
    // argv: morloc +RTS <heap> -RTS <subcmd> -e <policy...> <expr> NULL. Policy
    // flags precede the expr positional so the positional is never read as a
    // flag value.
    let mut argv: Vec<*const c_char> = Vec::with_capacity(5 + rts.len() + policy.len());
    argv.push(cmd.as_ptr());
    for p in &rts {
        argv.push(p.as_ptr());
    }
    argv.push(arg_subcmd.as_ptr());
    argv.push(dash_e.as_ptr());
    for p in &policy {
        argv.push(p.as_ptr());
    }
    argv.push(expr);
    argv.push(ptr::null());

    let child = match spawn_morloc(&argv, &stdout_pipe, &stderr_pipe, G_EVAL_TIMEOUT.load(Ordering::Relaxed)) {
        Ok(child) => child,
        Err(e) => {
            (*resp).success = false;
            (*resp).error_kind = DAEMON_ERROR_INTERNAL;
            let c = CString::new(format!("Failed to start morloc {}: {}", subcmd, e)).unwrap_or_default();
            (*resp).error = libc::strdup(c.as_ptr());
            libc::close(stdout_pipe[0]);
            libc::close(stdout_pipe[1]);
            libc::close(stderr_pipe[0]);
            libc::close(stderr_pipe[1]);
            return resp;
        }
    };

    // Parent
    libc::close(stdout_pipe[1]);
    libc::close(stderr_pipe[1]);

    let eval_timeout = G_EVAL_TIMEOUT.load(Ordering::Relaxed);
    let (stdout_buf, stderr_buf, in_time) = drain_child(&child, stdout_pipe[0], stderr_pipe[0], eval_timeout);
    libc::close(stdout_pipe[0]);
    libc::close(stderr_pipe[0]);
    let status = child.finish();
    if !in_time {
        (*resp).success = false;
        (*resp).error_kind = DAEMON_ERROR_TIMEOUT;
        let c = CString::new(format!(
            "morloc {} ran past its time limit ({} s of wall time) and was stopped",
            subcmd,
            eval_timeout as u64 * EVAL_WALL_PER_CPU,
        ))
        .unwrap_or_default();
        (*resp).error = libc::strdup(c.as_ptr());
        return resp;
    }

    let status = match status {
        Some(st) => st,
        None => {
            (*resp).success = false;
            (*resp).error_kind = DAEMON_ERROR_INTERNAL;
            let c = CString::new(format!(
                "lost the exit status of the forked `morloc {}`; \
                 its result cannot be trusted",
                subcmd,
            ))
            .unwrap_or_default();
            (*resp).error = libc::strdup(c.as_ptr());
            return resp;
        }
    };

    if libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0 {
        let mut out = String::from_utf8_lossy(&stdout_buf).into_owned();
        // Trim trailing newlines
        while out.ends_with('\n') || out.ends_with('\r') {
            out.pop();
        }
        (*resp).success = true;
        let c = CString::new(out).unwrap_or_default();
        (*resp).result_json = libc::strdup(c.as_ptr());
    } else {
        (*resp).success = false;
        // Classify by exit cause. SIGXCPU means the child blew the
        // RLIMIT_CPU budget (the --eval-timeout guard) -> TIMEOUT (408).
        // Other fatal signals (SIGKILL, SIGSEGV, ...) are server-side
        // failures -> INTERNAL (500). EXIT_HEAPOVERFLOW means the child hit
        // the heap ceiling this server imposed, which is a server-side
        // resource decision and NOT a malformed expression -> INTERNAL (500);
        // reporting it as BAD_REQUEST would tell the caller their expression
        // did not compile when it merely needed more memory than we allow.
        // Anything else (non-zero exit, no signal) is "your expression didn't
        // compile / had a runtime error" -> BAD_REQUEST (400). Note: these
        // guards bound /eval and /typecheck (this fork_morloc_command path);
        // /call/<command> dispatches into a pre-compiled pool worker
        // and is subject to neither.
        let (errmsg, kind) = classify_failed_command(status, subcmd, &stdout_buf, &stderr_buf);
        (*resp).error_kind = kind;
        let c = CString::new(errmsg).unwrap_or_default();
        (*resp).error = libc::strdup(c.as_ptr());
    }

    resp
}

fn classify_failed_command(status: libc::c_int, subcmd: &str, stdout_buf: &[u8], stderr_buf: &[u8]) -> (String, i32) {
    use morloc_runtime_types::eval_status::{eval_failure, EvalFailure};
    let stderr = String::from_utf8_lossy(stderr_buf);
    match eval_failure(status) {
        EvalFailure::Timeout => (
            format!("morloc {} exceeded CPU budget ({}s); see --eval-timeout", subcmd, G_EVAL_TIMEOUT.load(Ordering::Relaxed)),
            DAEMON_ERROR_TIMEOUT,
        ),
        // The child's own advice ("use +RTS -M<size>") is useless to an
        // HTTP caller, who cannot set it; say who imposed the ceiling.
        EvalFailure::HeapCeiling => (
            format!("morloc {} exceeded the server's heap ceiling ({})", subcmd, EVAL_HEAP_LIMIT.trim_start_matches("-M")),
            DAEMON_ERROR_INTERNAL,
        ),
        EvalFailure::Internal if libc::WIFSIGNALED(status) => {
            if stderr.is_empty() {
                (format!("morloc {} killed by signal {}", subcmd, libc::WTERMSIG(status)), DAEMON_ERROR_INTERNAL)
            } else {
                (stderr.into_owned(), DAEMON_ERROR_INTERNAL)
            }
        }
        // PANIC-1
        EvalFailure::Internal => (
            format!("morloc {} failed with an internal error: {}", subcmd, stderr.trim()),
            DAEMON_ERROR_INTERNAL,
        ),
        // DAEMON-6: the wrapper could not start `morloc`.
        EvalFailure::CouldNotStart => (format!("Failed to start morloc {}: {}", subcmd, stderr.trim()), DAEMON_ERROR_INTERNAL),
        EvalFailure::Rejected if !stderr.is_empty() => (stderr.into_owned(), DAEMON_ERROR_BAD_REQUEST),
        // A rejected expression is a diagnostic, and the compiler prints
        // its diagnostics on stdout. Handing back the exit code alone
        // would tell the caller their expression failed while withholding
        // the sentence saying why.
        EvalFailure::Rejected if !stdout_buf.is_empty() => {
            (String::from_utf8_lossy(stdout_buf).into_owned(), DAEMON_ERROR_BAD_REQUEST)
        }
        EvalFailure::Rejected => {
            let code = if libc::WIFEXITED(status) { libc::WEXITSTATUS(status) } else { -1 };
            (format!("morloc {} exited with code {}", subcmd, code), DAEMON_ERROR_BAD_REQUEST)
        }
    }
}

/// Read two pipes to end of file at once, so a child that fills one while
/// the other is being read cannot block forever. Returns both contents.
unsafe fn drain_pair(a: i32, b: i32, deadline: Option<std::time::Instant>) -> (Vec<u8>, Vec<u8>, bool) {
    let mut out = (Vec::new(), Vec::new());
    let mut open = [a >= 0, b >= 0];
    let mut tmp = [0u8; 8192];
    while open[0] || open[1] {
        let mut fds = [
            libc::pollfd { fd: if open[0] { a } else { -1 }, events: libc::POLLIN, revents: 0 },
            libc::pollfd { fd: if open[1] { b } else { -1 }, events: libc::POLLIN, revents: 0 },
        ];
        let wait_ms = match deadline {
            None => -1,
            Some(d) => {
                let left = d.saturating_duration_since(std::time::Instant::now());
                if left.is_zero() {
                    return (out.0, out.1, false);
                }
                left.as_millis().clamp(1, i32::MAX as u128) as i32
            }
        };
        if libc::poll(fds.as_mut_ptr(), 2, wait_ms) < 0 {
            if std::io::Error::last_os_error().kind() == std::io::ErrorKind::Interrupted {
                continue;
            }
            break;
        }
        for k in 0..2 {
            if !open[k] || fds[k].revents == 0 {
                continue;
            }
            let n = libc::read(fds[k].fd, tmp.as_mut_ptr() as *mut c_void, tmp.len());
            if n > 0 {
                let dst = if k == 0 { &mut out.0 } else { &mut out.1 };
                dst.extend_from_slice(&tmp[..n as usize]);
            } else if n == 0
                || std::io::Error::last_os_error().kind() != std::io::ErrorKind::Interrupted
            {
                open[k] = false;
            }
        }
    }
    (out.0, out.1, true)
}

// DAEMON-6: a forked `morloc` is limited in CPU time by ulimit, and in wall
// time here, since a child blocked on I/O spends no CPU.
unsafe fn drain_child(child: &EvalChild, a: i32, b: i32, cpu_seconds: i32) -> (Vec<u8>, Vec<u8>, bool) {
    if cpu_seconds <= 0 {
        return drain_pair(a, b, None);
    }
    let limit = std::time::Duration::from_secs(cpu_seconds as u64 * EVAL_WALL_PER_CPU);
    let (mut out, mut err, finished) = drain_pair(a, b, Some(std::time::Instant::now() + limit));
    if finished {
        return (out, err, true);
    }
    for sig in [libc::SIGTERM, libc::SIGKILL] {
        child.signal(sig);
        let (o, e, done) = drain_pair(a, b, Some(std::time::Instant::now() + EVAL_DRAIN_AFTER_KILL));
        out.extend(o);
        err.extend(e);
        if done {
            break;
        }
    }
    (out, err, false)
}

const EVAL_WALL_PER_CPU: u64 = 4;
const EVAL_DRAIN_AFTER_KILL: std::time::Duration = std::time::Duration::from_secs(1);

// -- Packet-mode result serialization -----------------------------------------

/// Mark `resp` as a failed INTERNAL error. If `*err` carries a message it is
/// moved onto `resp.error` and `*err` reset to null (so callers' later
/// null-checks stay correct); otherwise `default_msg` is used.
unsafe fn set_packet_error(
    resp: *mut DaemonResponse,
    err: *mut *mut c_char,
    default_msg: &str,
) {
    (*resp).success = false;
    (*resp).error_kind = DAEMON_ERROR_INTERNAL;
    if (*err).is_null() {
        let c = CString::new(default_msg).unwrap_or_default();
        (*resp).error = libc::strdup(c.as_ptr());
    } else {
        (*resp).error = *err;
        *err = ptr::null_mut();
    }
}

/// Flatten an existing morloc data packet to self-contained bytes (reading SHM
/// for RPTR sources and applying zstd `compression`), storing them on
/// `resp.result_bytes`/`result_len` and marking success. On failure the error
/// fields on `resp` are set instead. The caller owns `packet` and frees it.
unsafe fn packetize_data_packet(
    resp: *mut DaemonResponse,
    packet: *const u8,
    compression: u8,
    err: *mut *mut c_char,
) {
    let sz = crate::packet_ffi::morloc_packet_size(packet, err);
    if !(*err).is_null() {
        set_packet_error(resp, err, "failed to read result packet header");
        return;
    }
    let mut out: *mut u8 = ptr::null_mut();
    let mut outlen: usize = 0;
    let rc = crate::packet_ffi::normalize_data_packet_for_output(
        packet, sz, compression, &mut out, &mut outlen, err,
    );
    if rc != 0 || !(*err).is_null() {
        set_packet_error(resp, err, "failed to serialize result packet");
        return;
    }
    (*resp).success = true;
    (*resp).result_bytes = out;
    (*resp).result_len = outlen;
}

/// Wrap a result voidstar in a morloc data packet and flatten it to bytes via
/// [`packetize_data_packet`]. MUST run while the eval arena is still alive (the
/// voidstar and the RPTR packet reference SHM the arena owns).
unsafe fn packetize_result_voidstar(
    resp: *mut DaemonResponse,
    voidstar: *mut c_void,
    schema: *const CSchema,
    compression: u8,
    err: *mut *mut c_char,
) {
    let pkt = crate::cli::wrap_voidstar_as_packet(voidstar, schema, err);
    if pkt.is_null() || !(*err).is_null() {
        set_packet_error(resp, err, "failed to build result packet");
        if !pkt.is_null() {
            libc::free(pkt as *mut c_void);
        }
        return;
    }
    packetize_data_packet(resp, pkt, compression, err);
    libc::free(pkt as *mut c_void);
}

/// Fill `resp` with the raw content bytes of a media-typed (`@mime`) return:
/// serialize `voidstar` to its `-f raw` bytes and, on success, set
/// `result_bytes`/`result_len` + strdup the media type onto `mime`; on failure
/// set the error fields. `voidstar` is the result value (pure eval) or the
/// pool packet's value (remote). Mirrors the `packetize_*` result helpers.
unsafe fn emit_raw_media(
    resp: *mut DaemonResponse,
    voidstar: *mut c_void,
    schema: *const CSchema,
    mime: *const c_char,
) {
    let mut err: *mut c_char = ptr::null_mut();
    let mut raw_len: usize = 0;
    let raw = crate::json_ffi::voidstar_to_raw_bytes(voidstar, schema, &mut raw_len, &mut err);
    if raw.is_null() || !err.is_null() {
        set_packet_error(resp, &mut err, "failed to serialize media return");
        return;
    }
    (*resp).success = true;
    (*resp).result_bytes = raw;
    (*resp).result_len = raw_len;
    (*resp).mime = libc::strdup(mime);
}

/// # Safety
/// `packet` must be a well-formed morloc packet.
unsafe fn adopt_rptr_result(packet: *const u8) {
    use crate::packet::{PacketHeader, PACKET_SOURCE_RPTR};
    use crate::shm::RelPtr;
    if packet.is_null() {
        return;
    }
    let header = packet as *const PacketHeader;
    if (*header).command.data.source != PACKET_SOURCE_RPTR {
        return;
    }
    let payload_start = 32 + (*header).offset as usize;
    if ((*header).length as usize) < std::mem::size_of::<RelPtr>() {
        return;
    }
    let relptr = *(packet.add(payload_start) as *const RelPtr);
    if let Ok(abs) = crate::shm::rel2abs(relptr) {
        // The producer took a reference on this block before the packet left
        // it, and that reference is ours now. Take ownership rather than a
        // second reference: acquiring here would be too late anyway, since
        // the interval this is meant to cover has already elapsed.
        crate::eval_arena::record_if_active(abs);
    }
}

// -- Dispatch -----------------------------------------------------------------

pub(crate) unsafe fn daemon_dispatch(manifest: *mut crate::manifest_ffi::Manifest, request: *mut DaemonRequest, sockets: *mut MorlocSocket, shm_basename: *const c_char) -> *mut DaemonResponse {
    dispatch(manifest, request, sockets, shm_basename)
}

pub(crate) unsafe fn dispatch(
    manifest: *mut crate::manifest_ffi::Manifest,
    request: *mut DaemonRequest,
    sockets: *mut MorlocSocket,
    shm_basename: *const c_char,
) -> *mut DaemonResponse {
    let resp = dispatch_request(manifest, request, sockets, shm_basename);
    if !(*resp).success && (*resp).error_kind == DAEMON_ERROR_INTERNAL && POOLS_STOPPED.load(Ordering::SeqCst) {
        let cause = if (*resp).error.is_null() {
            String::new()
        } else {
            format!(": {}", CStr::from_ptr((*resp).error).to_string_lossy())
        };
        libc::free((*resp).error as *mut c_void);
        let c = CString::new(format!("the daemon is shutting down and stopped this request{}", cause))
            .unwrap_or_default();
        (*resp).error = libc::strdup(c.as_ptr());
        (*resp).error_kind = DAEMON_ERROR_RECOVERING;
    }
    resp
}

/// The result of a call that reports failure through an error out-pointer.
unsafe fn checked<T>(call: impl FnOnce(*mut *mut c_char) -> T) -> Result<T, *mut c_char> {
    let mut err: *mut c_char = ptr::null_mut();
    let value = call(&mut err);
    if err.is_null() { Ok(value) } else { Err(err) }
}

struct OwnedSchema(*mut CSchema);

impl Drop for OwnedSchema {
    fn drop(&mut self) {
        unsafe { crate::ffi::free_schema(self.0) }
    }
}

fn internal(e: *mut c_char) -> (i32, *mut c_char) {
    (DAEMON_ERROR_INTERNAL, e)
}

fn bad_request(e: *mut c_char) -> (i32, *mut c_char) {
    (DAEMON_ERROR_BAD_REQUEST, e)
}

unsafe fn dispatch_request(
    manifest: *mut crate::manifest_ffi::Manifest,
    request: *mut DaemonRequest,
    sockets: *mut MorlocSocket,
    _shm_basename: *const c_char,
) -> *mut DaemonResponse {
    let resp = libc::calloc(1, std::mem::size_of::<DaemonResponse>()) as *mut DaemonResponse;

    // Echo request id
    if !(*request).id.is_null() {
        (*resp).id = libc::strdup((*request).id);
    }

    // Recovery gate. If a pool process has died and the nexus is currently
    // tearing down + respawning all pools, we cannot safely talk to any
    // pool socket or touch SHM. Every method returns success=false with a
    // clear retryable error so callers (including the health endpoint
    // poller in pool-crash-stress) can distinguish the recovery window
    // from normal operation: the outer JSON wrapper turns success=false
    // into `"status":"error"`, which a polling loop can wait on.
    let Some(_in_flight) = enter_request() else {
        (*resp).success = false;
        (*resp).error_kind = DAEMON_ERROR_RECOVERING;
        let c = CString::new(if is_shutting_down() {
            "Daemon shutting down."
        } else {
            "Daemon recovering from a pool process crash; please retry."
        })
        .unwrap();
        (*resp).error = libc::strdup(c.as_ptr());
        return resp;
    };

    match (*request).method {
        DaemonMethod::Health => {
            (*resp).success = true;
            let (alive, n_pools) = *POOL_STATUS.lock().unwrap_or_else(|_| morloc_runtime_types::panic::poisoned_lock());
            if let Some(alive_fn) = alive {
                let mut arr = Vec::with_capacity(n_pools);
                for i in 0..n_pools {
                    // PANIC-2
                    arr.push(serde_json::Value::Bool(morloc_runtime_types::panic::outside_scope(|| alive_fn(i))));
                }
                // Named, not bare: a list of anonymous booleans tells a
                // caller nothing about what is being reported, and leaves no
                // room to report anything else about the daemon later.
                let json = serde_json::json!({ "pools": arr }).to_string();
                let c = CString::new(json).unwrap_or_default();
                (*resp).result_json = libc::strdup(c.as_ptr());
            }
            return resp;
        }
        DaemonMethod::Discover => {
            (*resp).success = true;
            (*resp).result_json = daemon_build_discovery(manifest);
            return resp;
        }
        DaemonMethod::Eval => {
            if (*request).expr.is_null() {
                (*resp).success = false;
                (*resp).error_kind = DAEMON_ERROR_BAD_REQUEST;
                let c = CString::new("Missing 'expr' field in eval request").unwrap();
                (*resp).error = libc::strdup(c.as_ptr());
                return resp;
            }

            // Check binding store for cached expression
            if let Some(store) = binding_store().as_ref() {
                let expr_str = CStr::from_ptr((*request).expr).to_string_lossy();
                let hv = hash::xxh64_with_seed(expr_str.as_bytes(), DEFAULT_XXHASH_SEED);
                let _cached = store
                    .lookup_hash(hv)
                    .or_else(|| store.lookup_name(&expr_str));
                // TODO: direct binary execution for bound functions
            }

            let eval_resp = fork_morloc_command("eval", (*request).expr);
            if !(*request).id.is_null() {
                (*eval_resp).id = libc::strdup((*request).id);
            }
            libc::free(resp as *mut c_void);
            return eval_resp;
        }
        DaemonMethod::Typecheck => {
            if (*request).expr.is_null() {
                (*resp).success = false;
                (*resp).error_kind = DAEMON_ERROR_BAD_REQUEST;
                let c = CString::new("Missing 'expr' field in typecheck request").unwrap();
                (*resp).error = libc::strdup(c.as_ptr());
                return resp;
            }
            let tc_resp = fork_morloc_command("typecheck", (*request).expr);
            if !(*request).id.is_null() {
                (*tc_resp).id = libc::strdup((*request).id);
            }
            libc::free(resp as *mut c_void);
            return tc_resp;
        }
        DaemonMethod::Bind => {
            if (*request).expr.is_null() {
                (*resp).success = false;
                (*resp).error_kind = DAEMON_ERROR_BAD_REQUEST;
                let c = CString::new("Missing 'expr' field in bind request").unwrap();
                (*resp).error = libc::strdup(c.as_ptr());
                return resp;
            }
            let expr_str = CStr::from_ptr((*request).expr).to_string_lossy().into_owned();
            let name = if (*request).name.is_null() {
                None
            } else {
                Some(CStr::from_ptr((*request).name).to_string_lossy().into_owned())
            };
            let hv = hash::xxh64_with_seed(expr_str.as_bytes(), DEFAULT_XXHASH_SEED);
            let bound = match claim_binding(hv, name.as_deref()) {
                BindClaim::NoStore => {
                    (*resp).success = false;
                    (*resp).error_kind = DAEMON_ERROR_INTERNAL;
                    let c = CString::new("Binding store not initialized").unwrap();
                    (*resp).error = libc::strdup(c.as_ptr());
                    return resp;
                }
                BindClaim::Bound => true,
                BindClaim::Compile(dir) => {
                    // DAEMON-7: waiters are woken even if the compile panics.
                    struct Unfinished(u64);
                    impl Drop for Unfinished {
                        fn drop(&mut self) {
                            finish_binding(self.0, "", None, None);
                        }
                    }
                    let unfinished = Unfinished(hv);
                    let timeout = G_EVAL_TIMEOUT.load(Ordering::Relaxed);
                    let artifact_dir = compile_binding(&dir, hv, &expr_str, timeout);
                    let ok = artifact_dir.is_some();
                    std::mem::forget(unfinished);
                    finish_binding(hv, &expr_str, name.as_deref(), artifact_dir);
                    ok
                }
            };
            match bound {
                true => {
                    let mut map = serde_json::Map::new();
                    map.insert(
                        "hash".into(),
                        serde_json::Value::String(format!("{:016x}", hv)),
                    );
                    map.insert("expr".into(), serde_json::Value::String(expr_str));
                    if let Some(n) = &name {
                        map.insert("name".into(), serde_json::Value::String(n.clone()));
                    }
                    if let Some(entry) = binding_store().as_ref().and_then(|st| st.lookup_hash(hv)) {
                        if let Some(ref ts) = entry.type_sig {
                            map.insert("type".into(), serde_json::Value::String(ts.clone()));
                        }
                    }
                    let json = serde_json::to_string(&map).unwrap_or_default();
                    (*resp).success = true;
                    let c = CString::new(json).unwrap_or_default();
                    (*resp).result_json = libc::strdup(c.as_ptr());
                }
                false => {
                    (*resp).success = false;
                    (*resp).error_kind = DAEMON_ERROR_BAD_REQUEST;
                    let c =
                        CString::new("Failed to compile and bind expression").unwrap_or_default();
                    (*resp).error = libc::strdup(c.as_ptr());
                }
            }
            return resp;
        }
        DaemonMethod::Bindings => {
            (*resp).success = true;
            let json = binding_store()
                .as_ref()
                .map_or_else(|| "{\"bindings\":[]}".to_string(), |st| st.list_json());
            let c = CString::new(json).unwrap_or_default();
            (*resp).result_json = libc::strdup(c.as_ptr());
            return resp;
        }
        DaemonMethod::Unbind => {
            let name_ptr = if !(*request).command.is_null() {
                (*request).command
            } else {
                (*request).name
            };
            if name_ptr.is_null() {
                (*resp).success = false;
                (*resp).error_kind = DAEMON_ERROR_BAD_REQUEST;
                let c = CString::new("Missing binding name").unwrap();
                (*resp).error = libc::strdup(c.as_ptr());
                return resp;
            }
            let name = CStr::from_ptr(name_ptr).to_string_lossy();
            let removed = match binding_store().as_mut() {
                Some(store) => store.unbind(&name),
                None => {
                    (*resp).success = false;
                    (*resp).error_kind = DAEMON_ERROR_INTERNAL;
                    let c = CString::new("Binding store not initialized").unwrap();
                    (*resp).error = libc::strdup(c.as_ptr());
                    return resp;
                }
            };
            if removed {
                (*resp).success = true;
                let c = CString::new("{\"removed\":true}").unwrap();
                (*resp).result_json = libc::strdup(c.as_ptr());
            } else {
                (*resp).success = false;
                (*resp).error_kind = DAEMON_ERROR_NOT_FOUND;
                let c = CString::new(format!("Binding not found: {}", name)).unwrap_or_default();
                (*resp).error = libc::strdup(c.as_ptr());
            }
            return resp;
        }
        DaemonMethod::Call => {
            // Fall through to call dispatch below
        }
    }

    // DAEMON_CALL
    if (*request).command.is_null() {
        (*resp).success = false;
        (*resp).error_kind = DAEMON_ERROR_BAD_REQUEST;
        let c = CString::new("Missing 'command' field in call request").unwrap();
        (*resp).error = libc::strdup(c.as_ptr());
        return resp;
    }

    use crate::ffi::parse_schema;
    use crate::ffi::free_schema;
    use crate::cli::initialize_positional;
    use crate::cli::free_argument_t;
    use crate::cli::parse_cli_data_argument;
    use crate::cli::make_call_packet_from_cli;
    use crate::ipc_ffi::send_and_receive_over_socket;
    use crate::packet_ffi::get_morloc_data_packet_error_message;
    use crate::packet_ffi::get_morloc_data_packet_value;
    use crate::json_ffi::voidstar_to_json_string;
    use crate::arrow_ffi::arrow_to_json_string;
    use crate::eval_ffi::morloc_eval;

    // The manifest is the canonical v2 C struct from manifest_ffi.rs.
    // No local mirror needed -- import the real type and walk it.
    use crate::manifest_ffi::{Manifest as ManifestC, ManifestArgKind};

    let mv = manifest as *const ManifestC;
    let command_name = CStr::from_ptr((*request).command);
    let cmd = match (*mv).command_by_name(command_name).filter(|c| !c.internal) {
        Some(c) => c,
        None => {
            (*resp).success = false;
            (*resp).error_kind = DAEMON_ERROR_NOT_FOUND;
            let msg = format!("Unknown command: {}", command_name.to_string_lossy());
            let c = CString::new(msg).unwrap_or_default();
            (*resp).error = libc::strdup(c.as_ptr());
            return resp;
        }
    };
    // HTTP output selection: `?render=<flag>` (or the command's `@default`
    // terminal when absent) re-points dispatch to that projection's entry
    // command -- same argument shape by construction, so parsing below is
    // unchanged. Gated on the HTTP transport so LP / MCP callers are unaffected:
    // a bare LP `/call` runs the command's own typed value, never the @default
    // projection (LP framing carries no render selector and its clients decode
    // the typed value directly). The @default asymmetry is intentional.
    let cmd = if current_output_http() {
        match resolve_render_target(mv, cmd, (*request).render) {
            Ok(c) => c,
            Err(msg) => {
                (*resp).success = false;
                (*resp).error_kind = DAEMON_ERROR_BAD_REQUEST;
                let ce = CString::new(msg).unwrap_or_default();
                (*resp).error = libc::strdup(ce.as_ptr());
                return resp;
            }
        }
    } else {
        cmd
    };
    let expected_nargs = cmd.n_args;

    // Result-form policy for this dispatch. `want_packet` is a per-thread flag
    // set only by the raw-packet (Unix socket / TCP) wire; the HTTP handler and
    // MCP/router callers leave it false, so they always take the JSON path
    // below. When set, both the pure-eval and remote sub-cases emit a morloc
    // data packet (compressed per `compression`) on `resp.result_bytes`.
    let want_packet = current_output_packet();
    // Raw-media output: when a raw-media consumer (the HTTP handler or the
    // in-process MCP server) is active AND the command's return type carries a
    // `@mime`, emit the raw content bytes + media type instead of JSON. The HTTP
    // handler turns this into a `Content-Type` body; MCP base64s it into a
    // content block. Both skip the wasteful JSON int-array round-trip.
    let want_media_bytes = current_output_media_bytes() && !cmd.ret.mime.is_null();
    let compression = G_DAEMON_COMPRESSION.load(Ordering::Relaxed);

    // Parse JSON args into argument_t** array
    let mut err: *mut c_char = ptr::null_mut();
    let args: *mut *mut crate::cli::ArgumentT;

    if !(*request).args_json.is_null() {
        // Parse the JSON array
        let args_str = CStr::from_ptr((*request).args_json).to_string_lossy();
        let parsed_args: Vec<&serde_json::value::RawValue> = match serde_json::from_str(&args_str) {
            Ok(v) => v,
            Err(e) => {
                (*resp).success = false;
                (*resp).error_kind = DAEMON_ERROR_BAD_REQUEST;
                let c = CString::new(format!("Failed to parse args: {}", e)).unwrap_or_default();
                (*resp).error = libc::strdup(c.as_ptr());
                return resp;
            }
        };

        if parsed_args.len() != expected_nargs {
            (*resp).success = false;
            (*resp).error_kind = DAEMON_ERROR_BAD_REQUEST;
            let c = CString::new(format!(
                "Expected {} arguments, got {}",
                expected_nargs,
                parsed_args.len()
            ))
            .unwrap_or_default();
            (*resp).error = libc::strdup(c.as_ptr());
            return resp;
        }

        // Each argument goes to the pool as the JSON text it arrived in. A
        // NUL inside a JSON string is the escape `\u0000`, and a raw NUL
        // anywhere is a syntax error the parse above rejects, so the
        // CString cannot fail. Done before the allocation below so a
        // failure returns without leaking the argument array.
        let mut arg_texts: Vec<CString> = Vec::with_capacity(expected_nargs);
        for val in parsed_args.iter() {
            let encoded = CString::new(val.get()).map_err(|e| e.to_string());
            match encoded {
                Ok(c) => arg_texts.push(c),
                Err(e) => {
                    (*resp).success = false;
                    (*resp).error_kind = DAEMON_ERROR_BAD_REQUEST;
                    let c = CString::new(format!(
                        "Failed to encode argument {}: {}",
                        arg_texts.len() + 1,
                        e
                    ))
                    .unwrap_or_default();
                    (*resp).error = libc::strdup(c.as_ptr());
                    return resp;
                }
            }
        }

        args = libc::calloc(expected_nargs + 1, std::mem::size_of::<*mut c_void>())
            as *mut *mut crate::cli::ArgumentT;
        for (i, c) in arg_texts.iter().enumerate() {
            let dup = libc::strdup(c.as_ptr());
            *args.add(i) = initialize_positional(dup);
            libc::free(dup as *mut c_void);
        }
        *args.add(expected_nargs) = ptr::null_mut();
    } else {
        if expected_nargs > 0 {
            // Check if any are positional (required)
            // For simplicity, match the C behavior: require args if n_args > 0
            (*resp).success = false;
            (*resp).error_kind = DAEMON_ERROR_BAD_REQUEST;
            let c = CString::new("Missing 'args' field in call request").unwrap();
            (*resp).error = libc::strdup(c.as_ptr());
            return resp;
        }
        args =
            libc::calloc(1, std::mem::size_of::<*mut c_void>()) as *mut *mut crate::cli::ArgumentT;
        *args = ptr::null_mut();
    }

    if cmd.is_pure {
        // Pure command: evaluate expression tree
        let mut nargs: usize = 0;
        while !(*args.add(nargs)).is_null() {
            nargs += 1;
        }

        // v2: schemas live on each ManifestArg. Walk cmd.args in
        // declaration order, INCLUDING flags (they consume an arg
        // slot in the parsed list and need a corresponding schema
        // entry to keep alignment). For flags, fall back to the
        // boolean schema "b".
        static FLAG_SCHEMA: &[u8] = b"b\0";
        let mut arg_schema_strs: Vec<*mut c_char> = Vec::with_capacity(nargs);
        for i in 0..cmd.n_args {
            let a = &*cmd.args.add(i);
            let s = if a.kind == ManifestArgKind::Flag || a.schema.is_null() {
                FLAG_SCHEMA.as_ptr() as *mut c_char
            } else {
                a.schema
            };
            arg_schema_strs.push(s);
        }

        let arg_schemas_arr =
            libc::calloc(nargs, std::mem::size_of::<*mut CSchema>()) as *mut *mut CSchema;
        let arg_packets =
            libc::calloc(nargs, std::mem::size_of::<*mut u8>()) as *mut *mut u8;
        let arg_voidstars =
            libc::calloc(nargs, std::mem::size_of::<*mut u8>()) as *mut *mut u8;

        let mut cleanup_and_fail = false;

        // Open a per-eval SHM arena. Every shm::shmalloc that fires while
        // this guard is alive (CLI arg ingress via msgpack/voidstar
        // unpack, all morloc_eval intermediates, the final result tree)
        // is tracked and released when the guard drops below. The result
        // serializer voidstar_to_json_string reads SHM and writes a libc
        // JSON string -- it MUST run before the guard drops, but the
        // libc-allocated JSON survives the drop and gets stored on resp.
        let arena_guard = match crate::eval_arena::enter() {
            Ok(g) => Some(g),
            Err(e) => {
                (*resp).success = false;
                (*resp).error_kind = DAEMON_ERROR_INTERNAL;
                let msg = CString::new(format!("eval arena error: {}", e))
                    .unwrap_or_default();
                (*resp).error = libc::strdup(msg.as_ptr());
                cleanup_and_fail = true;
                None
            }
        };

        // Tag this dispatch with a unique `call_id` so every @open
        // it issues records the id in its slot. After the response
        // is sent below (`drop(arena_guard)` and the libc cleanup
        // loop), we enqueue a per-call sweep to the dedicated
        // sweeper thread, which discards any slot left open at the
        // call's tail. Stored in TLS for the duration of morloc_eval.
        let call_id = crate::stream::generate_call_id();
        let prev_call_id = crate::stream::set_current_call_id(call_id);

        if !cleanup_and_fail {
            let evaluated = (|| -> Result<(), (i32, *mut c_char)> {
                for i in 0..nargs {
                    let schema_str = arg_schema_strs.get(i).copied().unwrap_or(ptr::null_mut());
                    *arg_schemas_arr.add(i) = checked(|e| parse_schema(schema_str, e)).map_err(internal)?;
                    *arg_packets.add(i) = checked(|e| parse_cli_data_argument(ptr::null_mut(), *args.add(i), *arg_schemas_arr.add(i), e))
                        .map_err(bad_request)?;
                    *arg_voidstars.add(i) = checked(|e| get_morloc_data_packet_value(*arg_packets.add(i), *arg_schemas_arr.add(i), e))
                        .map_err(bad_request)?;
                }
                let owned = OwnedSchema(checked(|e| parse_schema(cmd.ret.schema, e)).map_err(internal)?);
                let return_schema = owned.0;
                let result_abs = checked(|e| morloc_eval(cmd.expr, return_schema, arg_voidstars, arg_schemas_arr, nargs, e))
                    .map_err(internal)?;
                if want_media_bytes {
                    // Raw-media return: raw content bytes + media type, so the
                    // consumer (HTTP `Content-Type` body / MCP content block)
                    // skips the JSON int array.
                    emit_raw_media(resp, result_abs as *mut c_void, return_schema as *const CSchema, cmd.ret.mime);
                } else if want_packet {
                    // Packet mode: wrap the result voidstar in a data packet
                    // and flatten it to self-contained bytes (reading SHM,
                    // applying zstd) BEFORE the arena guard drops below.
                    packetize_result_voidstar(resp, result_abs as *mut c_void, return_schema as *const CSchema, compression, &mut err);
                } else {
                    // A table is an Arrow block, which the generic voidstar
                    // serializer refuses. Render it as the array of row
                    // objects its JSON Schema already advertises, so a
                    // served or MCP caller gets data rather than an error.
                    // CSchema carries the discriminant as a raw u32.
                    let returns_table = (*(return_schema as *const CSchema)).serial_type
                        == morloc_runtime_types::schema::SerialType::Table as u32;
                    let json = checked(|e| {
                        if returns_table {
                            arrow_to_json_string(result_abs as *const c_void, e)
                        } else {
                            voidstar_to_json_string(result_abs as *const c_void, return_schema as *const CSchema, e)
                        }
                    })
                    .map_err(internal)?;
                    (*resp).success = true;
                    (*resp).result_json = json;
                }
                Ok(())
            })();
            if let Err((kind, e)) = evaluated {
                (*resp).success = false;
                (*resp).error_kind = kind;
                (*resp).error = e;
            }
        }

        // Drop the arena guard explicitly here, before the libc cleanup
        // loop below. This is the point at which all SHM blocks allocated
        // for this request -- args, eval intermediates, result tree --
        // are returned to the volume's free list. The libc cleanup that
        // follows is independent (it frees the outer arrays for packets,
        // schemas, and the voidstar pointer array).
        drop(arena_guard);

        // Cleanup
        for i in 0..nargs {
            let s = *arg_schemas_arr.add(i);
            if !s.is_null() {
                free_schema(s);
            }
            let p = *arg_packets.add(i);
            if !p.is_null() {
                libc::free(p as *mut c_void);
            }
        }
        libc::free(arg_schemas_arr as *mut c_void);
        libc::free(arg_packets as *mut c_void);
        libc::free(arg_voidstars as *mut c_void);

        // Clear the per-thread call_id BEFORE enqueueing the sweep
        // so the sweeper sees the slot as belonging to a finished
        // call (TLS restore happens-before the queue push by
        // sequential program order on this thread). The sweep runs
        // on a separate thread and is off the user latency path.
        crate::stream::set_current_call_id(prev_call_id);
        crate::stream::sweeper_enqueue_call(call_id);
    } else {
        // Remote command: send call packet to pool. v2 stores schemas
        // per-arg, but make_call_packet_from_cli wants a NULL-terminated
        // flat array. ManifestCommand exposes a helper that materializes
        // the flat view; the outer pointer array is owned by us and
        // freed below, but the inner C strings remain owned by the
        // ManifestArg objects.

        // NUL-in-Str guard. If the target pool's language does not
        // support embedded NULs (e.g. R), reject the call cleanly here
        // before any pool I/O. The JSON args text is scanned as it
        // stands; text the scanner cannot read is rejected too, since a
        // guard that lets an unreadable argument through is no guard.
        // The check is bypassed when the env var or the program-wide
        // --unsafe-skip-null-check flag is set.
        let target_pool = &*(*mv).pools.add(cmd.pool_index);
        let skip = (*mv).unsafe_skip_null_check
            || crate::null_check::env_skip_null_check();
        if !skip && !target_pool.allow_string_null {
            if !(*request).args_json.is_null() {
                let args_str = CStr::from_ptr((*request).args_json).to_string_lossy();
                let lang = CStr::from_ptr(target_pool.lang).to_string_lossy();
                let rejection = match crate::null_check::first_null_in_json_text(&args_str) {
                    Ok(None) => None,
                    Ok(Some(p)) => Some(format!(
                        "{} does not support embedded NUL bytes in strings (at {})",
                        lang, p
                    )),
                    Err(e) => Some(format!("Failed to scan args for NUL bytes: {}", e)),
                };
                if let Some(msg) = rejection {
                    (*resp).success = false;
                    (*resp).error_kind = DAEMON_ERROR_BAD_REQUEST;
                    let c = CString::new(msg).unwrap_or_default();
                    (*resp).error = libc::strdup(c.as_ptr());
                    // Cleanup the args array allocated above.
                    if !args.is_null() {
                        let mut i = 0;
                        loop {
                            let p = *args.add(i);
                            if p.is_null() {
                                break;
                            }
                            free_argument_t(p);
                            i += 1;
                        }
                        libc::free(args as *mut c_void);
                    }
                    return resp;
                }
            }
        }

        // Open a per-eval SHM arena for the duration of this remote call.
        // make_call_packet_from_cli -> parse_cli_data_argument allocates
        // SHM blocks for non-trivial args (these are referenced by relptr
        // in the call packet shipped to the pool); the pool's arg ingress
        // shincref's any RPTR args it consumes, so we can safely shfree
        // our originals when the arena drops here.
        let _arena = match crate::eval_arena::enter() {
            Ok(g) => Some(g),
            Err(_) => None,  // already active is unexpected here; proceed without
        };
        let arg_schemas_flat = cmd.build_arg_schemas_array();
        let call_packet = make_call_packet_from_cli(
            ptr::null_mut(),
            cmd.mid,
            args,
            arg_schemas_flat,
            &mut err,
        );
        libc::free(arg_schemas_flat as *mut c_void);
        if !err.is_null() {
            (*resp).success = false;
            (*resp).error_kind = DAEMON_ERROR_BAD_REQUEST;
            (*resp).error = err;
        } else {
            let socket_path = (*sockets.add(cmd.pool_index)).socket_filename;
            let result_packet =
                send_and_receive_over_socket(socket_path, call_packet, &mut err);
            libc::free(call_packet as *mut c_void);

            if !err.is_null() {
                (*resp).success = false;
                (*resp).error_kind = DAEMON_ERROR_INTERNAL;
                (*resp).error = err;
            } else {
                adopt_rptr_result(result_packet);

                let packet_error =
                    get_morloc_data_packet_error_message(result_packet, &mut err);
                if !packet_error.is_null() {
                    (*resp).success = false;
                    (*resp).error_kind = DAEMON_ERROR_INTERNAL;
                    (*resp).error = libc::strdup(packet_error);
                    libc::free(result_packet as *mut c_void);
                } else if !err.is_null() {
                    (*resp).success = false;
                    (*resp).error_kind = DAEMON_ERROR_INTERNAL;
                    (*resp).error = err;
                    libc::free(result_packet as *mut c_void);
                } else if want_packet {
                    // Packet mode: forward the pool's result packet as
                    // self-contained bytes -- no JSON round-trip, no schema
                    // parse. A pool FAIL packet was already turned into
                    // `resp.error` by the `packet_error` check above.
                    packetize_data_packet(resp, result_packet, compression, &mut err);
                    libc::free(result_packet as *mut c_void);
                } else {
                    let return_schema = parse_schema(cmd.ret.schema, &mut err);
                    if !err.is_null() {
                        (*resp).success = false;
                        (*resp).error_kind = DAEMON_ERROR_INTERNAL;
                        (*resp).error = err;
                        libc::free(result_packet as *mut c_void);
                    } else {
                        let packet_value = get_morloc_data_packet_value(
                            result_packet,
                            return_schema as *const CSchema,
                            &mut err,
                        );
                        if !err.is_null() {
                            (*resp).success = false;
                            (*resp).error_kind = DAEMON_ERROR_INTERNAL;
                            (*resp).error = err;
                        } else if want_media_bytes {
                            // Raw-media return: raw content bytes + media type
                            // from the pool's result value.
                            emit_raw_media(
                                resp,
                                packet_value as *mut c_void,
                                return_schema as *const CSchema,
                                cmd.ret.mime,
                            );
                        } else {
                            // Same reason as the eval path above: a table is
                            // an Arrow block, not a generic voidstar.
                            let returns_table =
                                (*(return_schema as *const CSchema)).serial_type
                                    == morloc_runtime_types::schema::SerialType::Table as u32;
                            let json = if returns_table {
                                arrow_to_json_string(packet_value as *const c_void, &mut err)
                            } else {
                                voidstar_to_json_string(
                                    packet_value as *const c_void,
                                    return_schema as *const CSchema,
                                    &mut err,
                                )
                            };
                            if !err.is_null() {
                                (*resp).success = false;
                                (*resp).error_kind = DAEMON_ERROR_INTERNAL;
                                (*resp).error = err;
                            } else {
                                (*resp).success = true;
                                (*resp).result_json = json;
                            }
                        }
                        free_schema(return_schema);
                        libc::free(result_packet as *mut c_void);
                    }
                }
            }
        }
    }

    // Free args
    let mut i = 0;
    while !(*args.add(i)).is_null() {
        free_argument_t(*args.add(i));
        i += 1;
    }
    libc::free(args as *mut c_void);

    resp
}

// -- Length-prefixed message protocol -----------------------------------------

unsafe fn recv_exact(fd: i32, buf: *mut u8, len: usize) -> Result<(), usize> {
    let mut total = 0;
    while total < len {
        let n = libc::recv(fd, buf.add(total) as *mut c_void, len - total, 0);
        if n > 0 {
            total += n as usize;
        } else if n < 0 && crate::utility::errno_val() == libc::EINTR {
            continue;
        } else {
            return Err(total);
        }
    }
    Ok(())
}

unsafe fn read_lp_message(
    fd: i32,
    out_len: *mut usize,
    errmsg: *mut *mut c_char,
) -> *mut c_char {
    clear_errmsg(errmsg);

    let mut len_buf = [0u8; 4];
    if recv_exact(fd, len_buf.as_mut_ptr(), 4).is_err() {
        set_errmsg(
            errmsg,
            &MorlocError::Other("Failed to read message length prefix".into()),
        );
        return ptr::null_mut();
    }

    let msg_len = ((len_buf[0] as u32) << 24)
        | ((len_buf[1] as u32) << 16)
        | ((len_buf[2] as u32) << 8)
        | (len_buf[3] as u32);

    if msg_len > MAX_LP_MESSAGE {
        set_errmsg(
            errmsg,
            &MorlocError::Other(format!("Message too large: {} bytes", msg_len)),
        );
        return ptr::null_mut();
    }

    let msg = libc::malloc(msg_len as usize + 1) as *mut c_char;
    if msg.is_null() {
        set_errmsg(
            errmsg,
            &MorlocError::Other("Failed to allocate message buffer".into()),
        );
        return ptr::null_mut();
    }

    if let Err(total) = recv_exact(fd, msg as *mut u8, msg_len as usize) {
        libc::free(msg as *mut c_void);
        set_errmsg(
            errmsg,
            &MorlocError::Other(format!(
                "Failed to read message body (got {} of {} bytes)",
                total, msg_len
            )),
        );
        return ptr::null_mut();
    }
    *msg.add(msg_len as usize) = 0;

    if !out_len.is_null() {
        *out_len = msg_len as usize;
    }
    msg
}

unsafe fn write_lp_message(
    fd: i32,
    data: *const c_char,
    len: usize,
    errmsg: *mut *mut c_char,
) -> bool {
    clear_errmsg(errmsg);
    note_reply_started();

    let len_buf: [u8; 4] = [
        ((len >> 24) & 0xFF) as u8,
        ((len >> 16) & 0xFF) as u8,
        ((len >> 8) & 0xFF) as u8,
        (len & 0xFF) as u8,
    ];

    // send_all retries on EAGAIN (non-blocking client fd on macOS) rather than
    // treating a full send buffer as a fatal error mid-message.
    if !crate::ipc_ffi::send_all(fd, len_buf.as_ptr(), 4) {
        set_errmsg(
            errmsg,
            &MorlocError::Other("Failed to write message length prefix".into()),
        );
        return false;
    }

    if !crate::ipc_ffi::send_all(fd, data as *const u8, len) {
        set_errmsg(
            errmsg,
            &MorlocError::Other("Failed to write message body".into()),
        );
        return false;
    }

    true
}

// -- Connection handlers ------------------------------------------------------

unsafe fn handle_lp_connection(
    client_fd: i32,
    manifest: *mut c_void,
    sockets: *mut MorlocSocket,
    shm_basename: *const c_char,
) {
    let mut errmsg: *mut c_char = ptr::null_mut();
    let mut msg_len: usize = 0;

    // Peek to distinguish a probe connection (immediate EOF) from a real
    // client.  The router's readiness check connects then closes without
    // sending data; silently ignore those.
    let mut peek_buf = [0u8; 1];
    let peek_n = libc::recv(client_fd, peek_buf.as_mut_ptr() as *mut c_void, 1, libc::MSG_PEEK);
    if peek_n == 0 {
        // Clean EOF — probe connection, silently close.
        libc::close(client_fd);
        return;
    }

    let msg = read_lp_message(client_fd, &mut msg_len, &mut errmsg);
    if !errmsg.is_null() {
        let err_str = CStr::from_ptr(errmsg).to_string_lossy();
        eprintln!("morloc-daemon: read error: {}", err_str);
        libc::free(errmsg as *mut c_void);
        libc::close(client_fd);
        return;
    }

    let req = parse_request(msg, msg_len, &mut errmsg);
    libc::free(msg as *mut c_void);
    if !errmsg.is_null() {
        let mut err_resp: DaemonResponse = std::mem::zeroed();
        err_resp.success = false;
        err_resp.error_kind = DAEMON_ERROR_BAD_REQUEST;
        err_resp.error = errmsg;
        let mut resp_len: usize = 0;
        let resp_json = serialize_response(&mut err_resp, &mut resp_len);
        let mut write_err: *mut c_char = ptr::null_mut();
        write_lp_message(client_fd, resp_json, resp_len, &mut write_err);
        libc::free(resp_json as *mut c_void);
        if !write_err.is_null() {
            libc::free(write_err as *mut c_void);
        }
        libc::free(errmsg as *mut c_void);
        libc::close(client_fd);
        return;
    }

    // Packet output applies only to `call` results on this raw-packet (Unix
    // socket / TCP) wire. Scoped per-thread around the dispatch so the shared
    // `daemon_dispatch` stays JSON for HTTP and control methods.
    let want_packet = G_DAEMON_OUTPUT_PACKET.load(Ordering::Relaxed)
        && (*req).method == DaemonMethod::Call;
    REPLY_IS_PACKET.with(|r| r.set(want_packet));
    let prev = set_current_output_packet(want_packet);
    // The raw-media form (an `@mime` return as result_bytes+mime, conveyed as
    // base64 in the JSON envelope) is requested per-request by the serving
    // front-end's forward (`media:true`). Direct length-prefixed clients leave
    // it off and keep the JSON `result`. Packet mode already returns raw bytes.
    let want_media = (*req).media && !want_packet;
    let prev_media = set_current_output_media_bytes(want_media);
    let resp = dispatch(manifest as *mut crate::manifest_ffi::Manifest, req, sockets, shm_basename);
    set_current_output_media_bytes(prev_media);
    set_current_output_packet(prev);

    let mut write_err: *mut c_char = ptr::null_mut();
    if want_packet && !(*resp).result_bytes.is_null() {
        // Success (or a forwarded FAIL packet): raw bytes, no JSON envelope.
        write_lp_message(
            client_fd,
            (*resp).result_bytes as *const c_char,
            (*resp).result_len,
            &mut write_err,
        );
    } else if want_packet {
        // An error was raised before packet construction; synthesize a FAIL
        // packet so a packet-mode client always reads exactly one packet.
        let msg = if !(*resp).error.is_null() {
            CStr::from_ptr((*resp).error).to_string_lossy().into_owned()
        } else {
            "unknown daemon error".to_string()
        };
        let fail = morloc_runtime_types::packet::make_fail_packet_bytes(&msg);
        write_lp_message(
            client_fd,
            fail.as_ptr() as *const c_char,
            fail.len(),
            &mut write_err,
        );
    } else {
        let mut resp_len: usize = 0;
        let resp_json = serialize_response(resp, &mut resp_len);
        write_lp_message(client_fd, resp_json, resp_len, &mut write_err);
        libc::free(resp_json as *mut c_void);
    }
    if !write_err.is_null() {
        let err_str = CStr::from_ptr(write_err).to_string_lossy();
        eprintln!("morloc-daemon: write error: {}", err_str);
        libc::free(write_err as *mut c_void);
    }

    daemon_free_request(req);
    daemon_free_response(resp);
    libc::close(client_fd);
}

unsafe fn handle_http_connection(
    client_fd: i32,
    manifest: *mut c_void,
    sockets: *mut MorlocSocket,
    shm_basename: *const c_char,
) {
    use crate::http_ffi::http_parse_request;
    use crate::http_ffi::http_free_request;
    use crate::http_ffi::http_to_daemon_request;

    let mut errmsg: *mut c_char = ptr::null_mut();
    let http_req = http_parse_request(client_fd, &mut errmsg);
    if !errmsg.is_null() {
        let body = b"{\"status\":\"error\",\"error\":\"Bad request\"}\0";
        let ct = b"application/json\0";
        crate::http_ffi::write_response(
            client_fd,
            400,
            ct.as_ptr() as *const c_char,
            body.as_ptr() as *const c_char,
            body.len() - 1,
        );
        libc::free(errmsg as *mut c_void);
        libc::close(client_fd);
        return;
    }

    // CORS preflight short-circuit. Browser-issued OPTIONS requests
    // should get 204 No Content with the standard CORS headers, never
    // reaching daemon_dispatch (which would otherwise process them
    // through the Health pipeline -- including the recovery gate).
    // NET-1
    if (*http_req).method != HttpMethod::Options && !(*http_req).authorized {
        let body = b"{\"status\":\"error\",\"error\":\"unauthorized\"}";
        crate::http_ffi::write_response_ex(
            client_fd,
            401,
            c"application/json".as_ptr(),
            body.as_ptr() as *const c_char,
            body.len(),
            c"WWW-Authenticate: Bearer\r\n".as_ptr(),
        );
        http_free_request(http_req);
        libc::close(client_fd);
        return;
    }

    if (*http_req).method == HttpMethod::Options {
        let ct = b"application/json\0";
        crate::http_ffi::write_response(
            client_fd,
            204,
            ct.as_ptr() as *const c_char,
            ptr::null(),
            0,
        );
        http_free_request(http_req);
        libc::close(client_fd);
        return;
    }

    let mut route_kind: i32 = DAEMON_ERROR_BAD_REQUEST;
    let req = http_to_daemon_request(http_req, &mut errmsg, &mut route_kind);
    if !errmsg.is_null() {
        // Build the error body from the actual errmsg so unknown-endpoint
        // and missing-field cases get distinguishable messages, and use
        // the kind that http_to_daemon_request returned so 404 vs 400
        // routes correctly.
        let err_str = CStr::from_ptr(errmsg).to_string_lossy();
        let body = format!(
            "{{\"status\":\"error\",\"error\":{}}}\n",
            serde_json::Value::String(err_str.into_owned())
        );
        let body_c = CString::new(body.as_str()).unwrap_or_default();
        let status = daemon_error_kind_to_http_status(route_kind, false);
        let ct = b"application/json\0";
        crate::http_ffi::write_response(
            client_fd,
            status,
            ct.as_ptr() as *const c_char,
            body_c.as_ptr(),
            body.len(),
        );
        http_free_request(http_req);
        libc::free(errmsg as *mut c_void);
        libc::close(client_fd);
        return;
    }
    http_free_request(http_req);

    // For the duration of this dispatch, enable HTTP `?render=` resolution and
    // the raw-media response form: a return whose type carries a `@mime` comes
    // back as raw bytes + media type rather than JSON, so we can set
    // `Content-Type` below.
    let prev_http = set_current_output_http(true);
    let prev_media = set_current_output_media_bytes(true);
    let resp = dispatch(manifest as *mut crate::manifest_ffi::Manifest, req, sockets, shm_basename);
    set_current_output_media_bytes(prev_media);
    set_current_output_http(prev_http);

    let status = daemon_error_kind_to_http_status(
        (*resp).error_kind, (*resp).success,
    );

    if (*resp).success && !(*resp).mime.is_null() && !(*resp).result_bytes.is_null() {
        // Media-typed return (`@mime`): send the raw content bytes with the
        // declared Content-Type instead of the JSON envelope, so an HTTP client
        // gets a real image/PDF/... it can save. Errors still go out as JSON.
        crate::http_ffi::write_response(
            client_fd,
            status,
            (*resp).mime,
            (*resp).result_bytes as *const c_char,
            (*resp).result_len,
        );
    } else {
        let mut resp_len: usize = 0;
        let resp_json = serialize_response(resp, &mut resp_len);

        // Append newline for terminal-friendly output
        let resp_body = libc::malloc(resp_len + 2) as *mut u8;
        ptr::copy_nonoverlapping(resp_json as *const u8, resp_body, resp_len);
        *resp_body.add(resp_len) = b'\n';
        *resp_body.add(resp_len + 1) = 0;

        let ct = b"application/json\0";
        // 503 carries Retry-After: 1 so HTTP clients with automatic-retry
        // middleware (curl --retry, axios-retry, etc.) back off appropriately
        // during the brief pool-crash recovery window.
        if status == 503 {
            let extra = b"Retry-After: 1\r\n\0";
            crate::http_ffi::write_response_ex(
                client_fd,
                status,
                ct.as_ptr() as *const c_char,
                resp_body as *const c_char,
                resp_len + 1,
                extra.as_ptr() as *const c_char,
            );
        } else {
            crate::http_ffi::write_response(
                client_fd,
                status,
                ct.as_ptr() as *const c_char,
                resp_body as *const c_char,
                resp_len + 1,
            );
        }

        libc::free(resp_body as *mut c_void);
        libc::free(resp_json as *mut c_void);
    }

    daemon_free_request(req);
    daemon_free_response(resp);
    libc::close(client_fd);
}

// -- Thread pool (VecDeque + Condvar instead of linked list + pthread) --------

#[derive(Clone, Copy)]
struct DaemonJob {
    client_fd: i32,
    conn_type: i32, // 0 = length-prefixed (unix/tcp), 2 = http
}

struct JobQueue {
    jobs: VecDeque<DaemonJob>,
}

struct WorkerContext {
    queue: Mutex<JobQueue>,
    cond: Condvar,
    manifest: *mut c_void,
    sockets: *mut MorlocSocket,
    shm_basename: *const c_char,
}

// SAFETY: WorkerContext is shared between threads but all raw pointers
// within it point to read-only or thread-safe C data.
unsafe impl Send for WorkerContext {}
unsafe impl Sync for WorkerContext {}

fn set_socket_timeouts(fd: i32, timeout_sec: i32) {
    unsafe {
        let tv = libc::timeval {
            tv_sec: timeout_sec as _,
            tv_usec: 0,
        };
        libc::setsockopt(
            fd,
            libc::SOL_SOCKET,
            libc::SO_RCVTIMEO,
            &tv as *const libc::timeval as *const c_void,
            std::mem::size_of::<libc::timeval>() as libc::socklen_t,
        );
        libc::setsockopt(
            fd,
            libc::SOL_SOCKET,
            libc::SO_SNDTIMEO,
            &tv as *const libc::timeval as *const c_void,
            std::mem::size_of::<libc::timeval>() as libc::socklen_t,
        );
    }
}

// -- Main daemon event loop ---------------------------------------------------

const MAX_LISTENERS: usize = 3;

/// Read the port a TCP socket was bound to. Necessary when the caller
/// passed port 0 (bind ephemeral; OS picks the port). Returns None on
/// any getsockname or socket-family mismatch.
unsafe fn getsockname_port(fd: i32) -> Option<u16> {
    let mut addr: libc::sockaddr_in = std::mem::zeroed();
    let mut len = std::mem::size_of::<libc::sockaddr_in>() as libc::socklen_t;
    if libc::getsockname(
        fd,
        &mut addr as *mut libc::sockaddr_in as *mut libc::sockaddr,
        &mut len,
    ) < 0
    {
        return None;
    }
    if addr.sin_family != libc::AF_INET as libc::sa_family_t {
        return None;
    }
    Some(u16::from_be(addr.sin_port))
}

/// Render the port-file JSON and write it atomically (tmp + rename).
/// Always emits all three keys; missing listeners are null. Path string
/// escaping is intentionally minimal -- the unix socket path is a
/// filesystem path under the caller's control, and we only need to
/// escape `\` and `"` to keep the JSON well-formed.
fn write_port_file_atomic(
    path: &str,
    http_port: Option<u16>,
    tcp_port: Option<u16>,
    unix_path: Option<&str>,
) -> std::io::Result<()> {
    use std::io::Write;
    let mut body = String::new();
    body.push('{');
    body.push_str("\"http\":");
    match http_port {
        Some(p) => body.push_str(&p.to_string()),
        None => body.push_str("null"),
    }
    body.push_str(",\"tcp\":");
    match tcp_port {
        Some(p) => body.push_str(&p.to_string()),
        None => body.push_str("null"),
    }
    body.push_str(",\"unix\":");
    match unix_path {
        Some(s) => {
            body.push('"');
            for c in s.chars() {
                match c {
                    '\\' => body.push_str("\\\\"),
                    '"' => body.push_str("\\\""),
                    _ => body.push(c),
                }
            }
            body.push('"');
        }
        None => body.push_str("null"),
    }
    body.push('}');
    body.push('\n');

    let tmp = format!("{}.tmp", path);
    {
        let mut f = std::fs::File::create(&tmp)?;
        f.write_all(body.as_bytes())?;
        f.sync_all()?;
    }
    std::fs::rename(&tmp, path)
}

pub(crate) unsafe fn daemon_run(
    config: *mut DaemonConfig,
    manifest: *mut crate::manifest_ffi::Manifest,
    sockets: *mut MorlocSocket,
    n_pools: usize,
    shm_basename: *const c_char,
) -> bool {
    // Widen the open-file ceiling: this process accepts and fans out to every
    // pool, so it can hold the most fds. poll() tolerates fds >= 1024 but only
    // if the soft limit permits them to exist.
    crate::utility::raise_nofile_limit();

    // Set globals
    *POOL_STATUS.lock().unwrap_or_else(|_| morloc_runtime_types::panic::poisoned_lock()) = ((*config).pool_alive_fn, n_pools);
    let timeout = if (*config).eval_timeout > 0 {
        (*config).eval_timeout
    } else {
        30
    };
    G_EVAL_TIMEOUT.store(timeout, Ordering::Relaxed);
    G_DAEMON_OUTPUT_PACKET.store((*config).output_packet, Ordering::Relaxed);
    G_DAEMON_COMPRESSION.store((*config).compression_level, Ordering::Relaxed);

    // Initialize binding store
    binding_store().get_or_insert_with(|| BindingStore::new("/tmp/morloc-bindings"));

    // Install signal handlers
    begin_serving();
    let handler: libc::sighandler_t =
        std::mem::transmute::<extern "C" fn(i32), libc::sighandler_t>(daemon_signal_handler_fn);
    libc::signal(libc::SIGTERM, handler);
    libc::signal(libc::SIGINT, handler);

    let mut fds = [libc::pollfd {
        fd: -1,
        events: 0,
        revents: 0,
    }; MAX_LISTENERS];
    let mut fd_types = [0i32; MAX_LISTENERS]; // 0=unix, 1=tcp, 2=http
    let mut nfds: usize = 0;

    // Bound listener metadata collected during bind, used to print one
    // stderr ready line per listener and to render the optional
    // --port-file JSON.
    let mut bound_unix_path: Option<String> = None;
    let mut bound_tcp_port: Option<u16> = None;
    let mut bound_http_port: Option<u16> = None;

    // Unix socket
    if !(*config).unix_socket_path.is_null() {
        let sock_fd = morloc_runtime_types::fd::socket(libc::AF_UNIX, libc::SOCK_STREAM, 0);
        if sock_fd < 0 {
            eprintln!("morloc-daemon: failed to create unix socket");
            return true;
        }
        let addr = match crate::utility::unix_socket_addr(CStr::from_ptr((*config).unix_socket_path).to_bytes()) {
            Ok(a) => a,
            Err(e) => {
                eprintln!("morloc-daemon: {}", e);
                libc::close(sock_fd);
                return true;
            }
        };
        libc::unlink((*config).unix_socket_path);
        if libc::bind(
            sock_fd,
            &addr as *const libc::sockaddr_un as *const libc::sockaddr,
            std::mem::size_of::<libc::sockaddr_un>() as libc::socklen_t,
        ) < 0
        {
            eprintln!("morloc-daemon: failed to bind unix socket");
            libc::close(sock_fd);
            return true;
        }
        libc::listen(sock_fd, libc::SOMAXCONN);
        record_endpoint(SOCKET_ENDPOINT, (*config).unix_socket_path);
        fds[nfds].fd = sock_fd;
        fds[nfds].events = libc::POLLIN as i16;
        fd_types[nfds] = 0;
        nfds += 1;
        let unix_path = CStr::from_ptr((*config).unix_socket_path)
            .to_string_lossy()
            .into_owned();
        eprintln!("morloc-daemon: listening on unix://{}", unix_path);
        bound_unix_path = Some(unix_path);
    }

    // TCP. Configured iff tcp_port >= 0. tcp_port == 0 means "bind
    // ephemeral; OS picks the port"; getsockname() reads it back.
    if (*config).tcp_port >= 0 {
        let requested = (*config).tcp_port as u16;
        let tcp_fd = morloc_runtime_types::fd::socket(libc::AF_INET, libc::SOCK_STREAM, 0);
        if tcp_fd < 0 {
            eprintln!("morloc-daemon: failed to create tcp socket");
            return true;
        }
        let opt: i32 = 1;
        libc::setsockopt(
            tcp_fd,
            libc::SOL_SOCKET,
            libc::SO_REUSEADDR,
            &opt as *const i32 as *const c_void,
            std::mem::size_of::<i32>() as libc::socklen_t,
        );
        let mut addr: libc::sockaddr_in = std::mem::zeroed();
        addr.sin_family = libc::AF_INET as libc::sa_family_t;
        addr.sin_addr.s_addr = u32::from_be(0x7f000001); // INADDR_LOOPBACK
        addr.sin_port = requested.to_be();
        if libc::bind(
            tcp_fd,
            &addr as *const libc::sockaddr_in as *const libc::sockaddr,
            std::mem::size_of::<libc::sockaddr_in>() as libc::socklen_t,
        ) < 0
        {
            eprintln!("morloc-daemon: failed to bind tcp port {}", requested);
            libc::close(tcp_fd);
            return true;
        }
        let actual = getsockname_port(tcp_fd).unwrap_or(requested);
        libc::listen(tcp_fd, libc::SOMAXCONN);
        fds[nfds].fd = tcp_fd;
        fds[nfds].events = libc::POLLIN as i16;
        fd_types[nfds] = 1;
        nfds += 1;
        eprintln!("morloc-daemon: listening on tcp://127.0.0.1:{}", actual);
        bound_tcp_port = Some(actual);
    }

    // HTTP. Same sentinel convention as TCP.
    if (*config).http_port >= 0 {
        let requested = (*config).http_port as u16;
        let http_fd = morloc_runtime_types::fd::socket(libc::AF_INET, libc::SOCK_STREAM, 0);
        if http_fd < 0 {
            eprintln!("morloc-daemon: failed to create http socket");
            return true;
        }
        let opt: i32 = 1;
        libc::setsockopt(
            http_fd,
            libc::SOL_SOCKET,
            libc::SO_REUSEADDR,
            &opt as *const i32 as *const c_void,
            std::mem::size_of::<i32>() as libc::socklen_t,
        );
        let mut addr: libc::sockaddr_in = std::mem::zeroed();
        addr.sin_family = libc::AF_INET as libc::sa_family_t;
        // NET-1
        let address = http_access().address;
        addr.sin_addr.s_addr = address.to_be();
        addr.sin_port = requested.to_be();
        if libc::bind(
            http_fd,
            &addr as *const libc::sockaddr_in as *const libc::sockaddr,
            std::mem::size_of::<libc::sockaddr_in>() as libc::socklen_t,
        ) < 0
        {
            eprintln!("morloc-daemon: failed to bind http port {}", requested);
            libc::close(http_fd);
            return true;
        }
        let actual = getsockname_port(http_fd).unwrap_or(requested);
        libc::listen(http_fd, libc::SOMAXCONN);
        fds[nfds].fd = http_fd;
        fds[nfds].events = libc::POLLIN as i16;
        fd_types[nfds] = 2;
        nfds += 1;
        eprintln!("morloc-daemon: listening on http://{}:{}", std::net::Ipv4Addr::from(address), actual);
        bound_http_port = Some(actual);
    }

    // Optional port-file output. Written atomically via rename so a
    // stat-waiting orchestrator never sees a partial file. Schema is
    // fixed: every key is always present; missing listeners are null.
    if !(*config).port_file_path.is_null() {
        let path = CStr::from_ptr((*config).port_file_path)
            .to_string_lossy()
            .into_owned();
        match write_port_file_atomic(&path, bound_http_port, bound_tcp_port, bound_unix_path.as_deref()) {
            Ok(()) => record_endpoint(PORT_FILE_ENDPOINT, (*config).port_file_path),
            Err(e) => eprintln!("morloc-daemon: failed to write port file {}: {}", path, e),
        }
    }

    if nfds == 0 {
        eprintln!("morloc-daemon: no listeners configured, exiting");
        return true;
    }

    // Start worker thread pool
    let ctx = Arc::new(WorkerContext {
        queue: Mutex::new(JobQueue {
            jobs: VecDeque::new(),
        }),
        cond: Condvar::new(),
        manifest: manifest as *mut c_void,
        sockets,
        shm_basename,
    });

    let n_workers = n_pools.saturating_add(4).clamp(4, 32);
    let mut workers = Vec::with_capacity(n_workers);
    for _ in 0..n_workers {
        let ctx = Arc::clone(&ctx);
        workers.push(std::thread::spawn(move || {
            daemon_worker_fn(ctx);
        }));
    }

    // Main event loop
    while !SHUTDOWN_REQUESTED.load(Ordering::Relaxed) {
        let ready = libc::poll(fds.as_mut_ptr(), nfds as libc::nfds_t, 1000);
        if ready < 0 {
            if crate::utility::errno_val() == libc::EINTR {
                continue;
            }
            eprintln!("morloc-daemon: poll error");
            // DAEMON-6
            SHUTDOWN_REQUESTED.store(true, Ordering::SeqCst);
            break;
        }

        // Check and restart crashed pools
        if let Some(check_fn) = (*config).pool_check_fn {
            check_fn(sockets, n_pools);
        }

        if ready == 0 {
            continue;
        }

        for i in 0..nfds {
            if fds[i].revents & libc::POLLIN as i16 == 0 {
                continue;
            }
            let client_fd = morloc_runtime_types::fd::accept(fds[i].fd, ptr::null_mut(), ptr::null_mut());
            if client_fd < 0 {
                if crate::utility::errno_val() == libc::EINTR
                    || crate::utility::errno_val() == libc::EAGAIN
                {
                    continue;
                }
                eprintln!("morloc-daemon: accept error");
                continue;
            }
            crate::utility::set_nosigpipe(client_fd);
            set_socket_timeouts(client_fd, 30);

            let job = DaemonJob {
                client_fd,
                conn_type: fd_types[i],
            };
            let mut q = ctx.queue.lock().unwrap_or_else(|_| morloc_runtime_types::panic::poisoned_lock());
            q.jobs.push_back(job);
            ctx.cond.notify_one();
        }
    }

    // DAEMON-6: shutdown is bounded even if a worker holds a lock teardown needs.
    let emergency = (*config).emergency_exit_fn;
    let watchdog = std::thread::Builder::new().spawn(move || {
        let until = std::time::Instant::now() + SHUTDOWN_WATCHDOG;
        while std::time::Instant::now() < until && !SHUTDOWN_ESCALATED.load(Ordering::SeqCst) {
            std::thread::sleep(std::time::Duration::from_millis(50));
        }
        if !morloc_claim_exit() {
            return;
        }
        morloc_daemon_remove_endpoints();
        let code = if WORKER_PANICKED.load(Ordering::SeqCst) { morloc_runtime_types::panic::PANIC_EXIT_STATUS } else { 128 + libc::SIGTERM };
        match emergency {
            Some(exit) => exit(code),
            None => libc::_exit(code),
        }
    });
    if watchdog.is_err() {
        say("morloc-daemon: could not start the shutdown watchdog\n");
    }
    for i in 0..nfds {
        libc::close(fds[i].fd);
    }
    morloc_daemon_remove_endpoints();
    // DAEMON-6: a queued request is refused, never started.
    {
        let mut q = ctx.queue.lock().unwrap_or_else(|_| morloc_runtime_types::panic::poisoned_lock());
        while let Some(job) = q.jobs.pop_front() {
            libc::close(job.client_fd);
        }
    }
    // DAEMON-6: a worker inside a call to a wedged pool returns only once
    // the pools are stopped.
    ctx.cond.notify_all();
    let workers = join_within(workers, SHUTDOWN_GRACE);
    let workers = if workers.is_empty() {
        workers
    } else {
        say(&format!(
            "morloc-daemon: {} request(s) still running {:?} after shutdown was requested; stopping the pools\n",
            workers.len(),
            SHUTDOWN_GRACE
        ));
        // DAEMON-6
        POOLS_STOPPED.store(true, Ordering::SeqCst);
        if let Some(stop) = (*config).stop_pools_fn {
            stop();
        }
        join_within(workers, SHUTDOWN_AFTER_STOP)
    };
    let all_returned = workers.is_empty();
    if !all_returned {
        say(&format!("morloc-daemon: {} worker(s) did not return; exiting without them\n", workers.len()));
    }

    all_returned
}

const SHUTDOWN_GRACE: std::time::Duration = std::time::Duration::from_secs(5);
const SHUTDOWN_AFTER_STOP: std::time::Duration = std::time::Duration::from_secs(2);
const SHUTDOWN_WATCHDOG: std::time::Duration = std::time::Duration::from_secs(30);

// DAEMON-6: no stdio lock, which a running worker may hold.
fn say(line: &str) {
    unsafe { libc::write(2, line.as_ptr() as *const c_void, line.len()) };
}

fn join_within(
    mut workers: Vec<std::thread::JoinHandle<()>>,
    limit: std::time::Duration,
) -> Vec<std::thread::JoinHandle<()>> {
    let began = std::time::Instant::now();
    loop {
        let (done, running): (Vec<_>, Vec<_>) = workers.into_iter().partition(|w| w.is_finished());
        for w in done {
            let _ = w.join();
        }
        if running.is_empty() || began.elapsed() >= limit {
            return running;
        }
        workers = running;
        std::thread::sleep(std::time::Duration::from_millis(20));
    }
}

fn daemon_worker_fn(ctx: Arc<WorkerContext>) {
    loop {
        if SHUTDOWN_REQUESTED.load(Ordering::Relaxed) {
            break;
        }

        let job = {
            let mut q = ctx.queue.lock().unwrap_or_else(|_| morloc_runtime_types::panic::poisoned_lock());
            loop {
                // DAEMON-6
                if SHUTDOWN_REQUESTED.load(Ordering::Relaxed) {
                    break None;
                }
                if let Some(job) = q.jobs.pop_front() {
                    break Some(job);
                }
                // Wait with timeout so we recheck shutdown
                let (guard, _timeout) = ctx
                    .cond
                    .wait_timeout(q, std::time::Duration::from_millis(100))
                    .unwrap_or_else(|_| morloc_runtime_types::panic::poisoned_lock());
                q = guard;
            }
        };

        let job = match job {
            Some(j) => j,
            None => continue,
        };

        let panicked = serve_job(job, |fd| unsafe {
            if job.conn_type == 2 {
                handle_http_connection(fd, ctx.manifest, ctx.sockets, ctx.shm_basename);
            } else {
                handle_lp_connection(fd, ctx.manifest, ctx.sockets, ctx.shm_basename);
            }
        });
        if panicked {
            return;
        }
    }
}

// DAEMON-7: set once a worker panicked; the daemon then shuts down and
// exits as failed.
static WORKER_PANICKED: AtomicBool = AtomicBool::new(false);

thread_local! {
    // DAEMON-7: this thread's request: whether its reply is a packet, and
    // whether any of a reply has been written.
    static REPLY_IS_PACKET: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
    static REPLY_STARTED: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

pub(crate) fn note_reply_started() {
    REPLY_STARTED.with(|r| r.set(true));
}

fn begin_serving() {
    // DAEMON-7: a failure recorded before serving began still ends it.
    SHUTDOWN_REQUESTED.store(false, Ordering::SeqCst);
    if WORKER_PANICKED.load(Ordering::SeqCst) {
        SHUTDOWN_REQUESTED.store(true, Ordering::SeqCst);
    }
}

// DAEMON-7: a panic the nexus caught while serving the daemon.
pub(crate) fn morloc_daemon_fail() {
    WORKER_PANICKED.store(true, Ordering::SeqCst);
    SHUTDOWN_REQUESTED.store(true, Ordering::SeqCst);
}

pub(crate) fn morloc_daemon_worker_panicked() -> bool {
    WORKER_PANICKED.load(Ordering::SeqCst)
}

// DAEMON-7: the handler gets its own descriptor for the connection, so a
// panic at any point leaves this one open to answer on and close. Returns
// whether the handler panicked.
fn serve_job(job: DaemonJob, handle: impl FnOnce(i32)) -> bool {
    let own = unsafe { libc::fcntl(job.client_fd, libc::F_DUPFD_CLOEXEC, 0) };
    if own < 0 {
        // DAEMON-7: no spare descriptor to serve guarded with; the client
        // reads end of file, an error in either protocol.
        say("morloc-daemon: out of file descriptors; closing a connection unserved\n");
        unsafe { libc::close(job.client_fd) };
        return false;
    }
    REPLY_IS_PACKET.with(|r| r.set(false));
    REPLY_STARTED.with(|r| r.set(false));
    // PANIC-2
    let outcome = morloc_runtime_types::panic::catch(|| handle(own));
    if outcome.is_ok() {
        unsafe { libc::close(job.client_fd) };
        return false;
    }
    say("morloc-daemon: a request handler panicked; shutting down\n");
    // DAEMON-7: a reply already begun is not followed by a second.
    if !REPLY_STARTED.with(|r| r.get()) {
        unsafe { answer_panicked_request(job) };
    }
    // DAEMON-7: ends the connection however the handler left its own
    // descriptor, which is never touched here: it may already be closed and
    // its number reused.
    unsafe {
        libc::shutdown(job.client_fd, libc::SHUT_RDWR);
        libc::close(job.client_fd);
    }
    WORKER_PANICKED.store(true, Ordering::SeqCst);
    SHUTDOWN_REQUESTED.store(true, Ordering::SeqCst);
    true
}

unsafe fn answer_panicked_request(job: DaemonJob) {
    let message = "the daemon failed while serving this request and is restarting";
    if job.conn_type != 2 && REPLY_IS_PACKET.with(|r| r.get()) {
        let fail = morloc_runtime_types::packet::make_fail_packet_bytes(message);
        let mut err: *mut c_char = ptr::null_mut();
        write_lp_message(job.client_fd, fail.as_ptr() as *const c_char, fail.len(), &mut err);
        if !err.is_null() {
            libc::free(err as *mut c_void);
        }
    } else if job.conn_type == 2 {
        let body = format!("{{\"status\":\"error\",\"error\":\"{}\"}}", message);
        let ct = b"application/json\0";
        crate::http_ffi::write_response(
            job.client_fd,
            500,
            ct.as_ptr() as *const c_char,
            body.as_ptr() as *const c_char,
            body.len(),
        );
    } else {
        let mut resp: DaemonResponse = std::mem::zeroed();
        resp.success = false;
        resp.error_kind = DAEMON_ERROR_INTERNAL;
        let c = CString::new(message).unwrap_or_default();
        resp.error = libc::strdup(c.as_ptr());
        let mut len: usize = 0;
        let json = serialize_response(&mut resp, &mut len);
        let mut err: *mut c_char = ptr::null_mut();
        write_lp_message(job.client_fd, json, len, &mut err);
        libc::free(json as *mut c_void);
        libc::free(resp.error as *mut c_void);
        if !err.is_null() {
            libc::free(err as *mut c_void);
        }
    }
}

// Signal handler (must be async-signal-safe)
extern "C" fn daemon_signal_handler_fn(_sig: i32) {
    // DAEMON-6: a second signal ends the shutdown at once.
    if SHUTDOWN_REQUESTED.swap(true, Ordering::SeqCst) {
        SHUTDOWN_ESCALATED.store(true, Ordering::SeqCst);
    }
}

#[cfg(test)]
mod endpoint_tests {
    use super::*;

    #[test]
    fn a_daemon_removes_only_the_endpoint_files_it_made() {
        let dir = std::env::temp_dir().join(format!("morloc-endpoints-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let ours = dir.join("ours.sock");
        let taken = dir.join("taken.port");
        std::fs::write(&ours, "").unwrap();
        std::fs::write(&taken, "first").unwrap();
        let path = |p: &std::path::Path| -> *const c_char {
            Box::leak(std::ffi::CString::new(p.to_str().unwrap()).unwrap().into_boxed_c_str()).as_ptr()
        };
        unsafe {
            record_endpoint(SOCKET_ENDPOINT, path(&ours));
            record_endpoint(PORT_FILE_ENDPOINT, path(&taken));
        }
        let replacement = dir.join("replacement");
        std::fs::write(&replacement, "another daemon's").unwrap();
        std::fs::rename(&replacement, &taken).unwrap();
        unsafe { morloc_daemon_remove_endpoints() };
        assert!(!ours.exists());
        assert_eq!(std::fs::read_to_string(&taken).unwrap(), "another daemon's");
        let _ = std::fs::remove_dir_all(&dir);
    }
}

#[cfg(test)]
mod request_parse_tests {
    use super::*;

    fn parse(text: &str) -> (*mut DaemonRequest, Option<String>) {
        let mut err: *mut c_char = ptr::null_mut();
        let req = unsafe { parse_request(text.as_ptr() as *const c_char, text.len(), &mut err) };
        let msg = if err.is_null() {
            None
        } else {
            let m = unsafe { CStr::from_ptr(err) }.to_string_lossy().into_owned();
            unsafe { libc::free(err as *mut c_void) };
            Some(m)
        };
        (req, msg)
    }

    // The args travel to the pool as the text the client sent: an integer
    // past the range of a double keeps every digit, and a value nested
    // deeper than a tree-shaped JSON parser allows still parses.
    #[test]
    fn args_pass_through_as_text() {
        let deep = format!("{}1{}", "[".repeat(400), "]".repeat(400));
        let text = format!(
            "{{\"method\":\"call\",\"command\":\"f\",\"args\":[18446744073709551617, {deep}, \"s\"]}}"
        );
        let (req, err) = parse(&text);
        assert_eq!(err, None);
        assert!(!req.is_null());
        let args = unsafe { CStr::from_ptr((*req).args_json) }.to_string_lossy().into_owned();
        assert_eq!(args, format!("[18446744073709551617, {deep}, \"s\"]"));
        unsafe { daemon_free_request(req) };
    }

    #[test]
    fn malformed_args_are_rejected() {
        let (req, err) = parse("{\"method\":\"call\",\"args\":[1,}");
        assert!(req.is_null());
        assert!(err.unwrap().contains("Failed to parse request JSON"));
    }
}

#[cfg(test)]
mod media_wire_tests {
    use super::*;

    // A JSON result crosses the socket wire as the text the pool produced.
    #[test]
    fn json_result_round_trips_as_text() {
        unsafe {
            let deep = format!("{}1{}", "[".repeat(400), "]".repeat(400));
            let value = format!("[18446744073709551617, {deep}]");
            let mut resp: DaemonResponse = std::mem::zeroed();
            resp.success = true;
            let v = CString::new(value.as_str()).unwrap();
            resp.result_json = libc::strdup(v.as_ptr());
            let mut len: usize = 0;
            let json = serialize_response(&mut resp, &mut len);
            let text = CStr::from_ptr(json).to_str().unwrap();
            assert_eq!(text, format!("{{\"status\":\"ok\",\"result\":{value}}}"));
            let mut err: *mut c_char = ptr::null_mut();
            let parsed = daemon_parse_response(json, len, &mut err);
            assert!(err.is_null());
            assert!((*parsed).success);
            assert_eq!(CStr::from_ptr((*parsed).result_json).to_str().unwrap(), value);
            libc::free(json as *mut c_void);
            libc::free(resp.result_json as *mut c_void);
            daemon_free_response(parsed);
        }
    }

    // A media (@mime) return must survive the daemon response wire: serialize
    // sets result_b64+mime; parse reconstructs the raw bytes + mime byte-for-byte.
    // This is what carries @mime across the front-end forward.
    #[test]
    fn media_response_round_trips() {
        unsafe {
            let mut resp: DaemonResponse = std::mem::zeroed();
            resp.success = true;
            let bytes: &[u8] = b"\x89PNG\r\n\x1a\n\x00\xff\x01binary";
            resp.result_bytes = libc::malloc(bytes.len()) as *mut u8;
            ptr::copy_nonoverlapping(bytes.as_ptr(), resp.result_bytes, bytes.len());
            resp.result_len = bytes.len();
            let mime_c = CString::new("image/png").unwrap();
            resp.mime = libc::strdup(mime_c.as_ptr());

            let mut len: usize = 0;
            let json = serialize_response(&mut resp, &mut len);
            assert!(!json.is_null());

            let mut err: *mut c_char = ptr::null_mut();
            let parsed = daemon_parse_response(json, len, &mut err);
            assert!(err.is_null());
            assert!(!parsed.is_null());
            assert!((*parsed).success);
            assert!(!(*parsed).mime.is_null());
            assert!(!(*parsed).result_bytes.is_null());
            assert_eq!((*parsed).result_len, bytes.len());
            let out = std::slice::from_raw_parts((*parsed).result_bytes, (*parsed).result_len);
            assert_eq!(out, bytes);
            let mime = CStr::from_ptr((*parsed).mime).to_string_lossy();
            assert_eq!(mime, "image/png");

            libc::free(json as *mut c_void);
            libc::free(resp.result_bytes as *mut c_void);
            libc::free(resp.mime as *mut c_void);
            daemon_free_response(parsed);
        }
    }
}

#[cfg(test)]
mod recovery_gate_tests {
    use super::*;
    use std::time::Duration;

    #[test]
    fn recovery_waits_for_requests_already_running_and_admits_none() {
        let (inside_tx, inside_rx) = std::sync::mpsc::channel();
        let (leave_tx, leave_rx) = std::sync::mpsc::channel::<()>();
        let request = std::thread::spawn(move || {
            let guard = enter_request().expect("admitted before recovery");
            inside_tx.send(()).unwrap();
            leave_rx.recv().unwrap();
            drop(guard);
        });
        inside_rx.recv().unwrap();
        assert!(begin_recovery());
        let admitted_during = enter_request().is_some();
        let drained_while_running = wait_for_requests(Duration::from_millis(200));
        leave_tx.send(()).unwrap();
        let drained_after = wait_for_requests(Duration::from_secs(5));
        request.join().unwrap();
        end_recovery();
        let admitted_after = enter_request().is_some();
        assert!(!admitted_during, "a request was admitted during recovery");
        assert!(!drained_while_running, "recovery saw no requests while one was running");
        assert!(drained_after, "recovery never saw the running request finish");
        assert!(admitted_after, "requests were refused after recovery ended");
    }
}

#[cfg(test)]
mod reaped_ring_tests {
    use super::*;

    #[test]
    fn an_exit_recorded_before_a_spawn_is_not_the_new_childs() {
        let pid = 0x3fff_fff1;
        morloc_note_child_exit(pid, 7);
        let since = morloc_reaped_sequence();
        assert_eq!(take_noted_child_exit(pid, since), None, "a recycled pid was answered with a stale exit");
        morloc_note_child_exit(pid, 9);
        assert_eq!(take_noted_child_exit(pid, since), Some(9));
        let _ = take_noted_child_exit(pid, 0);
    }
}

#[cfg(test)]
mod binding_tests {
    use super::*;

    #[test]
    fn a_second_bind_of_one_expression_waits_for_the_first() {
        let dir = std::env::temp_dir().join(format!("morloc_bind_test_{}", std::process::id()));
        binding_store().get_or_insert_with(|| BindingStore::new(dir.to_str().unwrap()));
        let hv = 0x5eed_0001;
        assert!(matches!(claim_binding(hv, None), BindClaim::Compile(_)));
        let (tx, rx) = std::sync::mpsc::channel();
        let second = std::thread::spawn(move || {
            let bound = matches!(claim_binding(hv, Some("again")), BindClaim::Bound);
            tx.send(bound).unwrap();
        });
        let early = rx.recv_timeout(std::time::Duration::from_millis(200));
        finish_binding(hv, "expr", None, Some("artifact".into()));
        let late = rx.recv_timeout(std::time::Duration::from_secs(5));
        second.join().unwrap();
        let _ = std::fs::remove_dir_all(&dir);
        assert!(early.is_err(), "a second bind ran while the first was compiling");
        assert_eq!(late, Ok(true), "the second bind did not see the first's result");
    }
}

#[cfg(test)]
mod lp_message_tests {
    use super::*;

    extern "C" fn ignore(_: i32) {}

    #[test]
    fn a_signal_during_a_read_does_not_drop_the_message() {
        unsafe {
            let mut sa: libc::sigaction = std::mem::zeroed();
            sa.sa_sigaction = ignore as *const () as usize;
            sa.sa_flags = libc::SA_RESTART;
            libc::sigemptyset(&mut sa.sa_mask);
            libc::sigaction(libc::SIGUSR2, &sa, ptr::null_mut());

            let mut fds = [0i32; 2];
            assert_eq!(morloc_runtime_types::fd::socketpair(libc::AF_UNIX, libc::SOCK_STREAM, 0, fds.as_mut_ptr()), 0);
            let (reader, writer) = (fds[0], fds[1]);
            let tv = libc::timeval { tv_sec: 10, tv_usec: 0 };
            libc::setsockopt(
                reader,
                libc::SOL_SOCKET,
                libc::SO_RCVTIMEO,
                &tv as *const _ as *const c_void,
                std::mem::size_of::<libc::timeval>() as libc::socklen_t,
            );

            let (tid_tx, tid_rx) = std::sync::mpsc::channel::<libc::pthread_t>();
            let read = std::thread::spawn(move || {
                tid_tx.send(libc::pthread_self()).unwrap();
                let mut len = 0usize;
                let mut err: *mut c_char = ptr::null_mut();
                let msg = read_lp_message(reader, &mut len, &mut err);
                let ok = !msg.is_null() && err.is_null() && len == 5
                    && std::slice::from_raw_parts(msg as *const u8, 5) == b"hello";
                if !msg.is_null() {
                    libc::free(msg as *mut c_void);
                }
                ok
            });
            let tid = tid_rx.recv().unwrap();
            libc::send(writer, [0u8, 0, 0].as_ptr() as *const c_void, 3, 0);
            std::thread::sleep(std::time::Duration::from_millis(100));
            libc::pthread_kill(tid, libc::SIGUSR2);
            std::thread::sleep(std::time::Duration::from_millis(100));
            libc::send(writer, [5u8, b'h', b'e'].as_ptr() as *const c_void, 3, 0);
            std::thread::sleep(std::time::Duration::from_millis(100));
            libc::pthread_kill(tid, libc::SIGUSR2);
            std::thread::sleep(std::time::Duration::from_millis(100));
            libc::send(writer, b"llo".as_ptr() as *const c_void, 3, 0);
            let ok = read.join().unwrap();
            libc::close(reader);
            libc::close(writer);
            assert!(ok, "a signal during the read dropped the message");
        }
    }
}

#[cfg(test)]
mod panic_tests {
    use super::*;

    #[test]
    fn a_command_that_exits_with_the_internal_error_status_is_an_internal_error() {
        let status = morloc_runtime_types::panic::PANIC_EXIT_STATUS << 8;
        let (_, kind) = classify_failed_command(status, "eval", b"", b"morloc: internal error");
        assert_eq!(kind, DAEMON_ERROR_INTERNAL);
        let (_, kind) = classify_failed_command(1 << 8, "eval", b"", b"type error");
        assert_eq!(kind, DAEMON_ERROR_BAD_REQUEST);
    }

    fn pair() -> [i32; 2] {
        use std::os::fd::IntoRawFd;
        let (a, b) = std::os::unix::net::UnixStream::pair().unwrap();
        [a.into_raw_fd(), b.into_raw_fd()]
    }

    fn read_all(fd: i32) -> Vec<u8> {
        let mut out = Vec::new();
        let mut buf = [0u8; 4096];
        loop {
            let n = unsafe { libc::read(fd, buf.as_mut_ptr() as *mut c_void, buf.len()) };
            if n <= 0 {
                return out;
            }
            out.extend_from_slice(&buf[..n as usize]);
        }
    }

    #[test]
    fn a_panicking_request_is_answered_500_and_the_daemon_shuts_down_as_failed() {
        assert!(crate::fork_policy::exits_cleanly_in_a_forked_child(|| {
            let sv = pair();
            let job = DaemonJob { client_fd: sv[0], conn_type: 2 };
            let panicked = serve_job(job, |_| panic!("a bug in a handler"));
            let reply = String::from_utf8_lossy(&read_all(sv[1])).into_owned();
            let ok = panicked
                && reply.starts_with("HTTP/1.1 500")
                && WORKER_PANICKED.load(Ordering::SeqCst)
                && SHUTDOWN_REQUESTED.load(Ordering::SeqCst);
            if !ok {
                eprintln!("panicked {panicked}, reply {reply:?}");
            }
            ok
        }));
    }

    fn lp_frame(body: &[u8]) -> Vec<u8> {
        let mut out = (body.len() as u32).to_be_bytes().to_vec();
        out.extend_from_slice(body);
        out
    }

    #[test]
    fn a_packet_request_that_panics_is_answered_with_a_fail_packet() {
        assert!(crate::fork_policy::exits_cleanly_in_a_forked_child(|| {
            let sv = pair();
            let job = DaemonJob { client_fd: sv[0], conn_type: 0 };
            let panicked = serve_job(job, |_| {
                REPLY_IS_PACKET.with(|r| r.set(true));
                panic!("a bug in a handler")
            });
            let reply = read_all(sv[1]);
            let want = lp_frame(&morloc_runtime_types::packet::make_fail_packet_bytes(
                "the daemon failed while serving this request and is restarting",
            ));
            panicked && reply == want
        }));
    }

    #[test]
    fn a_handler_that_panics_after_replying_gets_no_second_reply() {
        assert!(crate::fork_policy::exits_cleanly_in_a_forked_child(|| {
            let sv = pair();
            let job = DaemonJob { client_fd: sv[0], conn_type: 0 };
            let panicked = serve_job(job, |fd| unsafe {
                let mut err: *mut c_char = ptr::null_mut();
                write_lp_message(fd, b"done".as_ptr() as *const c_char, 4, &mut err);
                panic!("a bug while cleaning up")
            });
            let reply = read_all(sv[1]);
            panicked && reply == lp_frame(b"done")
        }));
    }

    #[test]
    fn a_connection_with_no_descriptor_to_spare_is_closed_unserved() {
        assert!(crate::fork_policy::exits_cleanly_in_a_forked_child(|| {
            let sv = pair();
            let highest = (0..4096).filter(|fd| unsafe { libc::fcntl(*fd, libc::F_GETFD) } >= 0).max().unwrap_or(0);
            let limit = libc::rlimit { rlim_cur: (highest + 1) as libc::rlim_t, rlim_max: libc::RLIM_INFINITY };
            unsafe { libc::setrlimit(libc::RLIMIT_NOFILE, &limit) };
            // Every free number below the limit taken, so no dup can succeed.
            while unsafe { libc::fcntl(0, libc::F_DUPFD_CLOEXEC, 0) } >= 0 {}
            let job = DaemonJob { client_fd: sv[0], conn_type: 2 };
            let ran = std::cell::Cell::new(false);
            let panicked = serve_job(job, |_| ran.set(true));
            let reply = read_all(sv[1]);
            !panicked && !ran.get() && reply.is_empty()
        }));
    }

    #[test]
    fn a_failure_before_serving_starts_is_not_forgotten() {
        assert!(crate::fork_policy::exits_cleanly_in_a_forked_child(|| {
            morloc_daemon_fail();
            begin_serving();
            SHUTDOWN_REQUESTED.load(Ordering::SeqCst)
        }));
    }

    #[test]
    fn a_request_that_does_not_panic_is_closed_after_its_handler() {
        assert!(crate::fork_policy::exits_cleanly_in_a_forked_child(|| {
            let sv = pair();
            let job = DaemonJob { client_fd: sv[0], conn_type: 2 };
            let panicked = serve_job(job, |fd| unsafe {
                libc::write(fd, b"ok".as_ptr() as *const c_void, 2);
                libc::close(fd);
            });
            let reply = read_all(sv[1]);
            !panicked && reply == b"ok" && !WORKER_PANICKED.load(Ordering::SeqCst)
        }));
    }
}

#[cfg(test)]
mod child_output_tests {
    use super::*;

    fn write_all(fd: i32, byte: u8, n: usize) -> bool {
        let buf = vec![byte; n];
        let mut off = 0;
        while off < n {
            let w = unsafe { libc::write(fd, buf.as_ptr().add(off) as *const c_void, n - off) };
            if w <= 0 {
                return false;
            }
            off += w as usize;
        }
        true
    }

    /// A child that writes more than a pipe holds to each of its two outputs
    /// is drained to the end of both, whatever order it writes in.
    #[test]
    fn both_outputs_of_a_child_are_drained() {
        const N: usize = 1 << 20;
        unsafe {
            let mut o = [0 as libc::c_int; 2];
            let mut e = [0 as libc::c_int; 2];
            assert_eq!(morloc_runtime_types::fd::pipe(o.as_mut_ptr()), 0);
            assert_eq!(morloc_runtime_types::fd::pipe(e.as_mut_ptr()), 0);
            let since = morloc_reaped_sequence();
            let pid = libc::fork();
            assert!(pid >= 0);
            if pid == 0 {
                libc::close(o[0]);
                libc::close(e[0]);
                let ok = write_all(e[1], b'e', N) && write_all(o[1], b'o', N);
                libc::_exit(if ok { 0 } else { 1 });
            }
            libc::close(o[1]);
            libc::close(e[1]);
            let (out, err, finished) = drain_pair(o[0], e[0], None);
            assert!(finished);
            libc::close(o[0]);
            libc::close(e[0]);
            assert_eq!(wait_child(pid, since), Some(0));
            assert_eq!((out.len(), err.len()), (N, N));
            assert!(out.iter().all(|&b| b == b'o') && err.iter().all(|&b| b == b'e'));
        }
    }

    #[test]
    fn draining_a_writer_that_never_closes_stops_at_the_deadline() {
        let mut p = [0 as libc::c_int; 2];
        assert_eq!(unsafe { morloc_runtime_types::fd::pipe(p.as_mut_ptr()) }, 0);
        let began = std::time::Instant::now();
        let (_, _, finished) = unsafe {
            drain_pair(p[0], -1, Some(began + std::time::Duration::from_millis(200)))
        };
        let waited = began.elapsed();
        unsafe {
            libc::close(p[0]);
            libc::close(p[1]);
        }
        assert!(!finished);
        assert!(waited < std::time::Duration::from_secs(5), "drained for {waited:?}");
    }

    #[test]
    fn an_eval_leader_outlives_a_term_until_it_is_released() {
        let mut out_pipe = [0i32; 2];
        let mut err_pipe = [0i32; 2];
        assert!(unsafe { two_pipes(&mut out_pipe, &mut err_pipe) });
        let (sh, dash_c, script) =
            (CString::new("sh").unwrap(), CString::new("-c").unwrap(), CString::new("exit 3").unwrap());
        let argv = [sh.as_ptr(), dash_c.as_ptr(), script.as_ptr(), ptr::null()];
        let child = unsafe { spawn_morloc(&argv, &out_pipe, &err_pipe, 0) }.expect("spawn");
        unsafe {
            libc::close(out_pipe[1]);
            libc::close(err_pipe[1]);
        }
        let (_, _, finished) = unsafe { drain_child(&child, out_pipe[0], err_pipe[0], 0) };
        assert!(finished);
        child.signal(libc::SIGTERM);
        std::thread::sleep(std::time::Duration::from_millis(300));
        let mut st = 0;
        let alive = unsafe { libc::waitpid(child.pid, &mut st, libc::WNOHANG) } == 0;
        let status = unsafe { child.finish() };
        unsafe {
            libc::close(out_pipe[0]);
            libc::close(err_pipe[0]);
        }
        assert!(alive, "the leader ended while registered");
        assert!(status.is_some_and(|st| libc::WIFEXITED(st) && libc::WEXITSTATUS(st) == 3), "status {status:?}");
    }

    #[test]
    fn an_eval_past_its_wall_limit_is_stopped_with_everything_it_started() {
        let mut out_pipe = [0i32; 2];
        let mut err_pipe = [0i32; 2];
        assert!(unsafe { two_pipes(&mut out_pipe, &mut err_pipe) });
        let script = CString::new("sleep 600 & echo $!; exec sleep 600").unwrap();
        let (sh, dash_c) = (CString::new("sh").unwrap(), CString::new("-c").unwrap());
        let argv = [sh.as_ptr(), dash_c.as_ptr(), script.as_ptr(), ptr::null()];
        let child = unsafe { spawn_morloc(&argv, &out_pipe, &err_pipe, 1) }.expect("spawn");
        unsafe {
            libc::close(out_pipe[1]);
            libc::close(err_pipe[1]);
        }
        let began = std::time::Instant::now();
        let (out, _, finished) = unsafe { drain_child(&child, out_pipe[0], err_pipe[0], 1) };
        let waited = began.elapsed();
        unsafe {
            libc::close(out_pipe[0]);
            libc::close(err_pipe[0]);
        }
        let status = unsafe { child.finish() };
        assert!(status.is_some_and(|st| libc::WIFSIGNALED(st)), "status {status:?}");
        let grandchild: i32 = String::from_utf8_lossy(&out).trim().parse().expect("grandchild pid");
        let gone = (0..200).any(|_| {
            if unsafe { libc::kill(grandchild, 0) } == -1 {
                return true;
            }
            std::thread::sleep(std::time::Duration::from_millis(10));
            false
        });
        if !gone {
            unsafe { libc::kill(grandchild, libc::SIGKILL) };
        }
        assert!(!finished);
        assert!(gone, "process {grandchild} started by the eval outlived its limit");
        assert!(waited < std::time::Duration::from_millis(5500), "stopping took {waited:?}");
    }
}

mod c_abi {
    use super::*;

    #[no_mangle]
    pub unsafe extern "C" fn morloc_daemon_remove_endpoints() {
        super::morloc_daemon_remove_endpoints()
    }

    #[no_mangle]
    pub extern "C" fn morloc_claim_exit() -> bool {
        super::morloc_claim_exit()
    }

    #[no_mangle]
    pub extern "C" fn daemon_set_output_media_bytes(on: bool) -> bool {
        super::daemon_set_output_media_bytes(on)
    }

    #[no_mangle]
    pub unsafe extern "C" fn morloc_daemon_is_shutting_down() -> bool {
        super::morloc_daemon_is_shutting_down()
    }

    #[no_mangle]
    pub unsafe extern "C" fn morloc_daemon_begin_recovery() -> bool {
        super::morloc_daemon_begin_recovery()
    }

    #[no_mangle]
    pub extern "C" fn morloc_daemon_wait_for_requests(timeout_ms: u64) -> bool {
        super::morloc_daemon_wait_for_requests(timeout_ms)
    }

    #[no_mangle]
    pub unsafe extern "C" fn morloc_daemon_end_recovery() {
        super::morloc_daemon_end_recovery()
    }

    #[no_mangle]
    pub extern "C" fn morloc_stop_child_groups() {
        super::morloc_stop_child_groups()
    }

    #[no_mangle]
    pub extern "C" fn morloc_child_group_leader_exited(pid: libc::c_int) {
        super::morloc_child_group_leader_exited(pid)
    }

    #[no_mangle]
    pub unsafe extern "C" fn binding_store_init(base_dir: *const c_char) -> *mut BindingStore {
        super::binding_store_init(base_dir)
    }

    #[no_mangle]
    pub unsafe extern "C" fn binding_store_free(store: *mut BindingStore) {
        super::binding_store_free(store)
    }

    #[no_mangle]
    pub unsafe extern "C" fn daemon_parse_request(json: *const c_char, len: usize, errmsg: *mut *mut c_char) -> *mut DaemonRequest {
        super::daemon_parse_request(json, len, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn daemon_parse_response(json: *const c_char, len: usize, errmsg: *mut *mut c_char) -> *mut DaemonResponse {
        super::daemon_parse_response(json, len, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn daemon_free_request(req: *mut DaemonRequest) {
        super::daemon_free_request(req)
    }

    #[no_mangle]
    pub unsafe extern "C" fn daemon_free_response(resp: *mut DaemonResponse) {
        super::daemon_free_response(resp)
    }

    #[no_mangle]
    pub unsafe extern "C" fn daemon_serialize_response(response: *mut DaemonResponse, out_len: *mut usize) -> *mut c_char {
        super::daemon_serialize_response(response, out_len)
    }

    #[no_mangle]
    pub unsafe extern "C" fn daemon_build_discovery(manifest: *mut crate::manifest_ffi::Manifest) -> *mut c_char {
        super::daemon_build_discovery(manifest)
    }

    #[no_mangle]
    pub extern "C" fn daemon_set_eval_timeout(timeout_sec: i32) {
        super::daemon_set_eval_timeout(timeout_sec)
    }

    #[no_mangle]
    pub unsafe extern "C" fn daemon_set_eval_policy(sandbox: bool, allowed: *const c_char) {
        super::daemon_set_eval_policy(sandbox, allowed)
    }

    #[no_mangle]
    pub unsafe extern "C" fn daemon_set_http_access(address: u32, token: *const c_char) {
        super::daemon_set_http_access(address, token)
    }

    #[no_mangle]
    pub extern "C" fn morloc_note_child_exit(pid: i32, status: i32) {
        super::morloc_note_child_exit(pid, status)
    }

    #[no_mangle]
    pub extern "C" fn morloc_reaped_sequence() -> u64 {
        super::morloc_reaped_sequence()
    }

    #[no_mangle]
    pub unsafe extern "C" fn morloc_take_noted_child_exit(pid: i32, since: u64, status: *mut i32) -> i32 {
        super::morloc_take_noted_child_exit(pid, since, status)
    }

    #[no_mangle]
    pub unsafe extern "C" fn daemon_dispatch(manifest: *mut crate::manifest_ffi::Manifest, request: *mut DaemonRequest, sockets: *mut MorlocSocket, shm_basename: *const c_char) -> *mut DaemonResponse {
        super::daemon_dispatch(manifest, request, sockets, shm_basename)
    }

    #[no_mangle]
    pub unsafe extern "C" fn daemon_run(config: *mut DaemonConfig, manifest: *mut crate::manifest_ffi::Manifest, sockets: *mut MorlocSocket, n_pools: usize, shm_basename: *const c_char) -> bool {
        super::daemon_run(config, manifest, sockets, n_pools, shm_basename)
    }

    #[no_mangle]
    pub extern "C" fn morloc_daemon_fail() {
        super::morloc_daemon_fail()
    }

    #[no_mangle]
    pub extern "C" fn morloc_daemon_worker_panicked() -> bool {
        super::morloc_daemon_worker_panicked()
    }
}
