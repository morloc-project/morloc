//! Per-run identity and per-labeled-step workdir management.
//!
//! A morloc invocation only materializes a run directory when the
//! operator opts in via `--log-dir` (or `MORLOC_LOG_DIR`). Without the
//! opt-in:
//!
//!   * No `MORLOC_RUN_DIR` is published to children.
//!   * [`get_run`] returns `None`, so [`ensure_run_dir`] returns `None`,
//!     so the per-label log tee is a no-op.
//!   * `summary.json` is not written by [`write_summary_json`] unless
//!     `MORLOC_SUMMARY` was set to an explicit path.
//!
//! When activated, every invocation corresponds to one "run":
//!
//!   * A run id: `{utc-iso8601-second}-{8-hex-random}` (lexically sortable).
//!   * A base directory: `$MORLOC_LOG_DIR` (the operator's choice).
//!   * A run directory: `$base/$id`.
//!
//! The directory is created lazily on first need (logging tee,
//! prologue write, cache or SLURM artifacts). A `start.json` is
//! written on first materialization; a structured `summary.json`
//! is written on clean exit by the nexus's `clean_exit`.
//!
//! ## Nested vs top-level invocations
//!
//! Nested morloc programs (a morloc-built binary launching another)
//! share the parent's run dir so the user's `tail`/`grep` tooling sees
//! the full workflow as one entity. Inheritance is recognized via two
//! env vars set by every activated parent on startup:
//!
//!   * `MORLOC_RUN_DIR` -- the absolute path of the run dir.
//!   * `MORLOC_RUN_PARENT_PID` -- the PID that set the above.
//!
//! A process inherits only if `MORLOC_RUN_PARENT_PID` names one of its
//! ancestors. This defeats the case of a stale `MORLOC_RUN_DIR` left
//! lingering in a shell environment from a previous run -- that
//! publisher is not in the ancestry, so the process generates its own
//! run id.
//!
//! The relation is ancestry rather than parentage because a pool may
//! dispatch by forking a worker: the process that emits a log line is
//! then a grandchild of the nexus, and testing only the immediate
//! parent would split one invocation across two run dirs.

use std::collections::HashMap;
use std::ffi::CStr;
use std::fs::OpenOptions;
use std::io::Write;
use std::path::PathBuf;
use std::sync::Arc;
use morloc_runtime_types::publish_once::PublishOnce;
use crate::fork_policy::Held;
use std::time::Instant;

struct Run {
    base: PathBuf,
    id: String,
    dir: PathBuf,
    started_here: bool,
}

static RUN: PublishOnce<Option<Run>> = PublishOnce::new();

/// Per-label append handles for the log tee. Opened on first emission,
/// kept open for the process lifetime, line-flushed so a crash can lose
/// at most the in-flight line.
pub(crate) static TEE_HANDLES: Held<Option<HashMap<String, Arc<std::fs::File>>>> = Held::new(14, None);

/// Top-level (non-per-label) tee handle. Used for the prologue, the
/// epilogue, and any future run-scope events the nexus emits. Written
/// to `$MORLOC_RUN_DIR/log` so a single `tail -f` follows the whole
/// run instead of having to glob per-label sub-files.
pub(crate) static RUN_TEE_HANDLE: Held<Option<Arc<std::fs::File>>> = Held::new(15, None);

/// Nexus-owned scratchpad consulted by [`morloc_run_finalize`] when
/// writing `summary.json`. Pools never write here.
struct RunContext {
    started_at: Instant,
    started_at_iso: String,
}

pub(crate) static RUN_COMMAND: Held<Option<String>> = Held::new(16, None);
pub(crate) static RUN_ERROR: Held<Option<String>> = Held::new(17, None);

// FORK-10: one write(2) outside any lock; O_APPEND keeps lines whole.
fn append_line(file: &std::fs::File, text: &str) {
    let mut line = String::with_capacity(text.len() + 1);
    line.push_str(text);
    line.push('\n');
    let _ = (&*file).write_all(line.as_bytes());
}

static CONTEXT: PublishOnce<RunContext> = PublishOnce::new();

fn get_run() -> Option<&'static Run> {
    RUN.get_or_init_then(init_run, publish_run).as_ref()
}

fn parent_of(pid: i32) -> Option<i32> {
    let p = morloc_runtime_types::process::parent_of(u32::try_from(pid).ok()?)?;
    i32::try_from(p).ok()
}

/// Whether `ancestor` is this process's parent or a further ancestor.
///
/// A pool that dispatches by forking a worker puts the process that emits the
/// log two levels below the nexus, so testing only the immediate parent misses
/// a legitimate descendant. Where `parent_of` cannot answer, this degrades to
/// exactly that immediate-parent test -- correct for pool models that dispatch
/// on threads rather than forking below the pool process.
pub(crate) fn descends_from(ancestor: i32) -> bool {
    let mut pid = unsafe { libc::getppid() };
    // Bounded so a `/proc` inconsistency cannot spin at startup. A morloc
    // process tree is a handful of levels deep.
    for _ in 0..32 {
        if pid == ancestor {
            return true;
        }
        if pid <= 1 {
            return false;
        }
        match parent_of(pid) {
            Some(p) => pid = p,
            None => return false,
        }
    }
    false
}

fn init_run() -> Option<Run> {
    use std::env;

    // Inheritance: trust MORLOC_RUN_DIR only if MORLOC_RUN_PARENT_PID names a
    // process we descend from. Defeats stale shell-exported values, whose
    // publisher is not in our ancestry.
    let dir = env::var("MORLOC_RUN_DIR").ok();
    let ppid = env::var("MORLOC_RUN_PARENT_PID")
        .ok()
        .and_then(|s| s.parse::<i32>().ok());
    if let (Some(d), Some(p)) = (dir, ppid) {
        if descends_from(p) {
            let path = PathBuf::from(&d);
            let id = path
                .file_name()
                .and_then(|n| n.to_str())
                .map(str::to_string)
                .unwrap_or_default();
            let base = path.parent().map(PathBuf::from).unwrap_or_default();
            // No env republish: pools see the inherited values already.
            return Some(Run { base, id, dir: path, started_here: false });
        }
    }

    // Top-level activation. Without --log-dir / MORLOC_LOG_DIR the
    // process leaves no filesystem footprint.
    let base = match env::var("MORLOC_LOG_DIR").ok() {
        Some(d) if !d.is_empty() => PathBuf::from(d),
        _ => return None,
    };
    Some(new_run(base))
}

// INIT-3: only the run that won publishes itself to the environment.
fn publish_run(run: &Option<Run>) {
    if let Some(run) = run.as_ref().filter(|r| r.started_here) {
        std::env::set_var("MORLOC_RUN_DIR", &run.dir);
        std::env::set_var("MORLOC_RUN_BASE", &run.base);
        std::env::set_var("MORLOC_RUN_PARENT_PID", std::process::id().to_string());
    }
}

fn new_run(base: PathBuf) -> Run {
    let id = gen_id();
    let dir = base.join(&id);
    Run { base, id, dir, started_here: true }
}

fn gen_id() -> String {
    use chrono::Utc;
    let ts = Utc::now().format("%Y%m%dT%H%M%SZ").to_string();
    let mut buf = [0u8; 4];
    if let Ok(mut f) = std::fs::File::open("/dev/urandom") {
        use std::io::Read;
        let _ = f.read_exact(&mut buf);
    }
    if buf == [0u8; 4] {
        // /dev/urandom unavailable: fall back to clock nanos + pid.
        use std::time::{SystemTime, UNIX_EPOCH};
        let n = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.subsec_nanos())
            .unwrap_or(0)
            ^ std::process::id();
        buf = n.to_le_bytes();
    }
    let suffix: String = buf.iter().map(|b| format!("{:02x}", b)).collect();
    format!("{}-{}", ts, suffix)
}

/// Idempotently materialize `$run_dir` and (if `label` is `Some` and
/// non-empty) `$run_dir/$label`. Returns the deepest created path on
/// success, `None` if anything failed (the caller treats this as
/// "log to stderr only") or if the run is inactive.
pub fn ensure_run_dir(label: Option<&str>) -> Option<PathBuf> {
    let run = get_run()?;
    if std::fs::create_dir_all(&run.dir).is_err() {
        return None;
    }
    match label.filter(|s| !s.is_empty()) {
        None => Some(run.dir.clone()),
        Some(g) => {
            let p = run.dir.join(g);
            if std::fs::create_dir_all(&p).is_err() {
                return None;
            }
            Some(p)
        }
    }
}

/// Best-effort wrapper around `gethostname(2)`. Returns an empty
/// string on failure. Exposed so the nexus (and any other consumer
/// that wants to keep template logic in one place) can resolve the
/// `{hostname}` placeholder without rebuilding the buffer dance.
pub fn hostname() -> String {
    let mut buf = [0u8; 256];
    let rc = unsafe { libc::gethostname(buf.as_mut_ptr() as *mut libc::c_char, buf.len()) };
    if rc != 0 {
        return String::new();
    }
    let len = buf.iter().position(|&b| b == 0).unwrap_or(buf.len());
    String::from_utf8_lossy(&buf[..len]).into_owned()
}

/// Append a plain-text line to `$run_dir/$label/log`. The handle is
/// opened on first call per label and reused. Silently returns on any
/// I/O error so a permission-denied or disk-full condition never
/// crashes an otherwise-good run.
pub fn tee_log_line(label: &str, plain_text: &str) {
    if label.is_empty() {
        return;
    }
    let dir = match ensure_run_dir(Some(label)) {
        Some(d) => d,
        None => return,
    };
    let path = dir.join("log");
    let existing = TEE_HANDLES.lock().as_ref().and_then(|m| m.get(label).cloned());
    let file = match existing {
        Some(f) => f,
        None => {
            // INIT-2: opened without the lock.
            let Ok(opened) = OpenOptions::new().append(true).create(true).open(&path) else { return };
            let opened = Arc::new(opened);
            let installed = TEE_HANDLES
                .lock()
                .get_or_insert_with(HashMap::new)
                .entry(label.to_string())
                .or_insert_with(|| opened.clone())
                .clone();
            // FORK-10: a losing open is closed after the lock is released.
            drop(opened);
            installed
        }
    };
    append_line(&file, plain_text);
}

/// Append a plain-text line to the run-level `log` file (no per-label
/// subdir). Used for prologue / epilogue / any future run-scope event.
/// Silently no-op when the run dir hasn't been materialized.
pub fn tee_run_log_line(plain_text: &str) {
    let dir = match ensure_run_dir(None) {
        Some(d) => d,
        None => return,
    };
    let path = dir.join("log");
    if let Some(file) = crate::log::shared_append_file(&RUN_TEE_HANDLE, || Some(path)) {
        append_line(&file, plain_text);
    }
}

/// Resolve the path where `summary.json` should be written.
///
/// Resolution order, highest precedence first:
///   1. `MORLOC_SUMMARY` env var (set by `--summary=PATH`). Wins even
///      when no rundir was materialized. The directory must already
///      exist; we don't create parents.
///   2. `$MORLOC_RUN_DIR/summary.json` when the rundir is active. Even
///      a `--quiet` run with `--log-dir` still gets a sentinel; we
///      materialize the rundir lazily here so it's ready to receive
///      the file.
///   3. `None` -- nothing to write.
fn resolve_summary_path() -> Option<PathBuf> {
    if let Ok(p) = std::env::var("MORLOC_SUMMARY") {
        if !p.is_empty() {
            return Some(PathBuf::from(p));
        }
    }
    // Materialize on demand so the summary fires for --log-dir runs
    // that didn't emit any log lines (--quiet, no labels, etc.).
    let dir = ensure_run_dir(None)?;
    Some(dir.join("summary.json"))
}

/// Write the structured `summary.json` to the resolved sentinel path
/// (see [`resolve_summary_path`]). Atomic via `crate::utility::write_atomic_path`.
fn write_summary_json(exit_code: i32) {
    let target = match resolve_summary_path() {
        Some(p) => p,
        None => return,
    };

    let ctx = CONTEXT.get();
    let started_at = ctx.map(|c| c.started_at_iso.clone()).unwrap_or_default();
    let wall_ms = ctx
        .map(|c| c.started_at.elapsed().as_millis() as u64)
        .unwrap_or(0);
    let command = ctx.and(RUN_COMMAND.lock().clone());
    let recorded_error = ctx.and(RUN_ERROR.lock().clone());
    let run_id = get_run().map(|r| r.id.clone()).unwrap_or_default();

    let (status, error_field) = if exit_code == 0 {
        ("ok", serde_json::Value::Null)
    } else {
        let msg = recorded_error
            .unwrap_or_else(|| format!("nexus exited with status {}", exit_code));
        ("fail", serde_json::Value::String(msg))
    };

    let body = serde_json::json!({
        "status": status,
        "exit_code": exit_code,
        "command": command,
        "run_id": run_id,
        "started_at": started_at,
        "finished_at": chrono::Utc::now().to_rfc3339(),
        "wall_ms": wall_ms,
        "morloc_version": env!("CARGO_PKG_VERSION"),
        "error": error_field,
    });
    let payload = serde_json::to_vec_pretty(&body).unwrap_or_default();
    let _ = crate::utility::write_atomic_path(&target, &payload);
}

// ── C-ABI bridge for nexus and pool processes ─────────────────────────

/// Force the lazy run-id resolution + env publication. The nexus calls
/// this once at startup so children inherit a fully-published env. Pool
/// processes never call it directly; they hit `get_run` lazily via the
/// first log emission. Also seeds the [`RunContext`] used by the
/// summary writer (started-at fields).
#[no_mangle]
pub extern "C" fn morloc_run_init() {
    let _ = get_run();
    let _ = CONTEXT.get_or_init(|| RunContext {
        started_at: Instant::now(),
        started_at_iso: chrono::Utc::now().to_rfc3339(),
    });
}

/// Copy a C string into a `Mutex<Option<String>>` field of the
/// [`RunContext`], or no-op if the context isn't ready or the input
/// is NULL / non-UTF-8.
unsafe fn store_ctx_field(field: &Held<Option<String>>, cstr: *const libc::c_char) {
    if cstr.is_null() {
        return;
    }
    let s = match CStr::from_ptr(cstr).to_str() {
        Ok(s) => s.to_string(),
        Err(_) => return,
    };
    if CONTEXT.get().is_some() {
        *field.lock() = Some(s);
    }
}

/// Record the dispatched subcommand name. Becomes `command` in
/// `summary.json` and `{name}` in prologue/epilogue templates.
///
/// Safety: `name` must be a NUL-terminated UTF-8 string or NULL.
#[no_mangle]
pub unsafe extern "C" fn morloc_run_record_command(name: *const libc::c_char) {
    store_ctx_field(&RUN_COMMAND, name);
}

/// Record the most recent error message verbatim (multi-line OK).
/// Becomes `error` in `summary.json` and `{error}` in the fail
/// epilogue.
///
/// Safety: `msg` must be a NUL-terminated UTF-8 string or NULL.
#[no_mangle]
pub unsafe extern "C" fn morloc_run_record_error(msg: *const libc::c_char) {
    store_ctx_field(&RUN_ERROR, msg);
}

/// Copy the current run id into `buf` (NUL-terminated, truncated to
/// `len`). Returns the length written excluding the NUL, or 0 when
/// no rundir is active. Used by the nexus to render `{run_id}` in
/// templates without reparsing `MORLOC_RUN_DIR`.
///
/// Safety: `buf` must be writable for at least `len` bytes.
#[no_mangle]
pub unsafe extern "C" fn morloc_run_id(buf: *mut libc::c_char, len: usize) -> usize {
    if buf.is_null() || len == 0 {
        return 0;
    }
    let id = match get_run() {
        Some(r) => &r.id,
        None => return 0,
    };
    let bytes = id.as_bytes();
    let n = bytes.len().min(len - 1);
    std::ptr::copy_nonoverlapping(bytes.as_ptr(), buf as *mut u8, n);
    *buf.add(n) = 0;
    n
}

/// Copy the host name into `buf` (NUL-terminated, truncated to `len`).
/// Returns the length written excluding the NUL. Exposed so the nexus
/// can render `{hostname}` without an independent gethostname dance.
///
/// Safety: `buf` must be writable for at least `len` bytes.
#[no_mangle]
pub unsafe extern "C" fn morloc_hostname(buf: *mut libc::c_char, len: usize) -> usize {
    if buf.is_null() || len == 0 {
        return 0;
    }
    let h = hostname();
    let bytes = h.as_bytes();
    let n = bytes.len().min(len - 1);
    std::ptr::copy_nonoverlapping(bytes.as_ptr(), buf as *mut u8, n);
    *buf.add(n) = 0;
    n
}

/// Write `summary.json`, drop any input spooled off a pipe, and flush all
/// tee handles. Called from the nexus's `clean_exit` after pools have been
/// torn down so any in-flight log lines they wrote also land in the
/// per-label files.
#[no_mangle]
pub extern "C" fn morloc_run_finalize(exit_code: i32) {
    write_summary_json(exit_code);
    crate::cli::remove_spooled_inputs();
    // FORK-10: the files are closed after the locks are released.
    let tees = TEE_HANDLES.lock().take();
    let run_tee = RUN_TEE_HANDLE.lock().take();
    drop((tees, run_tee));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn id_format_is_sortable_and_unique() {
        let a = gen_id();
        let b = gen_id();
        assert_ne!(a, b);
        // Both should start with "20" (year 20xx) and contain T...Z-.
        assert!(a.contains('T') && a.contains('Z'));
    }

    #[test]
    fn descends_from_accepts_a_grandparent() {
        // A pool's forked worker sits two levels below the nexus.
        let grandparent = unsafe { libc::getppid() };
        unsafe {
            let child = libc::fork();
            assert!(child >= 0);
            if child == 0 {
                libc::_exit(if descends_from(grandparent) { 0 } else { 1 });
            }
            let mut status = 0;
            libc::waitpid(child, &mut status, 0);
            assert!(libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0, "a grandchild denied its grandparent");
        }
    }

    #[test]
    fn descends_from_accepts_the_immediate_parent() {
        // The direct-parent case must keep working when `parent_of` cannot answer.
        let ppid = unsafe { libc::getppid() };
        assert!(descends_from(ppid));
    }

    #[test]
    fn descends_from_rejects_a_stale_publisher() {
        // A pid that is not in our ancestry stands for the stale value a shell
        // exported from an earlier run.
        assert!(!descends_from(-1));
    }
}
