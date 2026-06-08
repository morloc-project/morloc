//! Per-run identity and per-labeled-step workdir management.
//!
//! Every invocation of a morloc-built program corresponds to exactly one
//! "run". The run has:
//!
//!   * A run id: `{utc-iso8601-second}-{8-hex-random}` (lexically sortable).
//!   * A base directory: `$MORLOC_RUN_BASE` or `$XDG_CACHE_HOME/morloc/runs`.
//!   * A run directory: `$base/$id`.
//!
//! The directory is created lazily on first need (logging tee, future
//! cache or SLURM artifacts). A run that never logs and never caches
//! leaves no filesystem footprint. On materialization a `start.json`
//! manifest is written; on clean exit a `summary.json` is written by
//! [`write_summary_json`] (called from the nexus's `clean_exit`).
//!
//! ## Nested vs top-level invocations
//!
//! Nested morloc programs (a morloc-built binary launching another)
//! share the parent's run dir so the user's `tail`/`grep` tooling sees
//! the full workflow as one entity. Inheritance is recognized via two
//! env vars set by every morloc nexus on startup:
//!
//!   * `MORLOC_RUN_DIR` -- the absolute path of the run dir.
//!   * `MORLOC_RUN_PARENT_PID` -- the PID that set the above.
//!
//! A child only inherits if `MORLOC_RUN_PARENT_PID == getppid()`. This
//! defeats the case of a stale `MORLOC_RUN_DIR` left lingering in a
//! shell environment from a previous run -- the PID won't match, the
//! child generates its own run id.
//!
//! Pool processes are technically children of the nexus, so they hit
//! the inherit branch and reuse the nexus's run dir -- which is exactly
//! what we want; one invocation, one dir.

use std::collections::HashMap;
use std::fs::OpenOptions;
use std::io::Write;
use std::path::PathBuf;
use std::sync::{Mutex, OnceLock};

struct Run {
    base: PathBuf,
    id: String,
    dir: PathBuf,
}

static RUN: OnceLock<Option<Run>> = OnceLock::new();

/// Per-label append handles for the log tee. Opened on first emission,
/// kept open for the process lifetime, line-flushed so a crash can lose
/// at most the in-flight line.
static TEE_HANDLES: OnceLock<Mutex<HashMap<String, std::fs::File>>> = OnceLock::new();

fn get_run() -> Option<&'static Run> {
    RUN.get_or_init(init_run).as_ref()
}

fn init_run() -> Option<Run> {
    use std::env;

    // Inheritance path: a parent morloc process already published a run
    // dir. Trust it only if the parent PID matches our own ppid -- this
    // protects against stale shell-exported MORLOC_RUN_DIR values.
    let inherited = {
        let dir = env::var("MORLOC_RUN_DIR").ok();
        let ppid = env::var("MORLOC_RUN_PARENT_PID")
            .ok()
            .and_then(|s| s.parse::<i32>().ok());
        match (dir, ppid) {
            (Some(d), Some(p)) if p == unsafe { libc::getppid() } => Some(d),
            _ => None,
        }
    };

    let run = if let Some(d) = inherited {
        let p = PathBuf::from(&d);
        let id = p
            .file_name()
            .and_then(|n| n.to_str())
            .map(str::to_string)
            .unwrap_or_default();
        let base = p.parent().map(PathBuf::from).unwrap_or_default();
        Run { base, id, dir: p }
    } else {
        new_run()
    };

    // Republish env so OUR children -- pools, nested morloc programs --
    // see a parent-pid that matches their getppid and inherit cleanly.
    env::set_var("MORLOC_RUN_DIR", &run.dir);
    env::set_var("MORLOC_RUN_BASE", &run.base);
    env::set_var("MORLOC_RUN_PARENT_PID", std::process::id().to_string());

    Some(run)
}

fn new_run() -> Run {
    let base = std::env::var("MORLOC_RUN_BASE")
        .map(PathBuf::from)
        .unwrap_or_else(|_| default_base());
    let id = gen_id();
    let dir = base.join(&id);
    Run { base, id, dir }
}

fn default_base() -> PathBuf {
    if let Ok(x) = std::env::var("XDG_CACHE_HOME") {
        return PathBuf::from(x).join("morloc/runs");
    }
    if let Ok(home) = std::env::var("HOME") {
        return PathBuf::from(home).join(".cache/morloc/runs");
    }
    PathBuf::from("/tmp/morloc/runs")
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
/// non-empty) `$run_dir/$label`. Writes `start.json` on first creation
/// of the run dir. Returns the deepest created path on success, `None`
/// if anything failed (the caller treats this as "log to stderr only").
pub fn ensure_run_dir(label: Option<&str>) -> Option<PathBuf> {
    let run = get_run()?;
    if std::fs::create_dir_all(&run.dir).is_err() {
        return None;
    }
    write_start_json_once(run);
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

fn write_start_json_once(run: &Run) {
    let path = run.dir.join("start.json");
    if path.exists() {
        return;
    }
    let now = chrono::Utc::now().to_rfc3339();
    let argv: Vec<String> = std::env::args().collect();
    let argv_json = argv
        .iter()
        .map(|a| json_escape(a))
        .collect::<Vec<_>>()
        .join(",");
    let s = format!(
        "{{\n  \"run_id\": \"{}\",\n  \"started_at\": \"{}\",\n  \"pid\": {},\n  \"host\": {},\n  \"argv\": [{}]\n}}\n",
        run.id,
        now,
        std::process::id(),
        json_escape(&hostname()),
        argv_json,
    );
    let _ = std::fs::write(&path, s);
}

fn json_escape(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 0x20 => {
                out.push_str(&format!("\\u{:04x}", c as u32));
            }
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

fn hostname() -> String {
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
    let map = TEE_HANDLES.get_or_init(|| Mutex::new(HashMap::new()));
    let mut handles = match map.lock() {
        Ok(g) => g,
        Err(p) => p.into_inner(),
    };
    if !handles.contains_key(label) {
        match OpenOptions::new().append(true).create(true).open(&path) {
            Ok(f) => {
                handles.insert(label.to_string(), f);
            }
            Err(_) => return,
        }
    }
    if let Some(f) = handles.get_mut(label) {
        let _ = writeln!(f, "{}", plain_text);
        let _ = f.flush();
    }
}

/// Write `summary.json` with final disposition, if the run dir has
/// been materialized. No-op when the dir does not exist (the user's
/// program never triggered creation; nothing to summarize). Stage 4
/// will extend this with the per-label timing table.
pub fn write_summary_json(exit_code: i32) {
    let run = match get_run() {
        Some(r) => r,
        None => return,
    };
    if !run.dir.exists() {
        return;
    }
    let now = chrono::Utc::now().to_rfc3339();
    let s = format!(
        "{{\n  \"run_id\": \"{}\",\n  \"ended_at\": \"{}\",\n  \"exit_code\": {}\n}}\n",
        run.id, now, exit_code,
    );
    let path = run.dir.join("summary.json");
    let _ = std::fs::write(&path, s);
}

// ── C-ABI bridge for nexus and pool processes ─────────────────────────

/// Force the lazy run-id resolution + env publication. The nexus calls
/// this once at startup so children inherit a fully-published env. Pool
/// processes never call it directly; they hit `get_run` lazily via the
/// first log emission.
#[no_mangle]
pub extern "C" fn morloc_run_init() {
    let _ = get_run();
}

/// Write `summary.json` and flush all tee handles. Called from the
/// nexus's `clean_exit` after pools have been torn down so any
/// in-flight log lines they wrote also land in the per-label files.
#[no_mangle]
pub extern "C" fn morloc_run_finalize(exit_code: i32) {
    write_summary_json(exit_code);
    if let Some(m) = TEE_HANDLES.get() {
        if let Ok(mut g) = m.lock() {
            g.clear();
        }
    }
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
    fn json_escape_handles_quotes_and_backslashes() {
        assert_eq!(json_escape("ab\"c\\d"), "\"ab\\\"c\\\\d\"");
    }

    #[test]
    fn json_escape_handles_newline() {
        assert_eq!(json_escape("a\nb"), "\"a\\nb\"");
    }
}
