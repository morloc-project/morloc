//! Run-scope log template rendering.
//!
//! The Haskell compiler pre-renders compile-time placeholders ({module},
//! {version}, {morloc_version}, ANSI color codes) into the manifest's
//! `run_log` field. This module fills the remaining runtime placeholders
//! and emits the result through `morloc_run_emit_line`, sharing the
//! per-label emitter's color / tee / quiet path.
//!
//! Invariant: prologue precedes dispatch; epilogue precedes
//! [`crate::process::clean_exit`]'s call to `morloc_run_finalize`.

use crate::manifest::RunLog;
use std::ffi::CString;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Mutex, OnceLock};
use std::time::Instant;

mod ffi {
    use std::ffi::c_char;
    extern "C" {
        pub fn morloc_run_emit_line(text: *const c_char);
        pub fn morloc_run_record_command(name: *const c_char);
        pub fn morloc_run_record_error(msg: *const c_char);
        pub fn morloc_run_id(buf: *mut c_char, len: usize) -> usize;
        pub fn morloc_hostname(buf: *mut c_char, len: usize) -> usize;
    }
}

struct State {
    run_log: Option<RunLog>,
    command: Mutex<Option<String>>,
    error: Mutex<Option<String>>,
    started_at: String,
    started_instant: Instant,
    prologue_emitted: AtomicBool,
    epilogue_emitted: AtomicBool,
}

static STATE: OnceLock<State> = OnceLock::new();

fn now_rfc3339() -> String {
    chrono::Utc::now().to_rfc3339()
}

fn ffi_buf_to_string<F: FnOnce(*mut std::ffi::c_char, usize) -> usize>(fill: F) -> String {
    let mut buf = [0u8; 256];
    let n = fill(buf.as_mut_ptr() as *mut std::ffi::c_char, buf.len());
    String::from_utf8_lossy(&buf[..n]).into_owned()
}

/// Stash run-scope state for later prologue/epilogue rendering. The
/// `started_at` capture happens here, so call this as close to the
/// nexus entry point as practical.
pub fn install(run_log: Option<RunLog>) {
    let _ = STATE.get_or_init(|| State {
        run_log,
        command: Mutex::new(None),
        error: Mutex::new(None),
        started_at: now_rfc3339(),
        started_instant: Instant::now(),
        prologue_emitted: AtomicBool::new(false),
        epilogue_emitted: AtomicBool::new(false),
    });
}

/// Record the subcommand name picked by dispatch. Becomes `{name}` in
/// templates and `command` in `summary.json`.
pub fn record_command(name: &str) {
    if let Some(st) = STATE.get() {
        if let Ok(mut g) = st.command.lock() {
            *g = Some(name.to_string());
        }
    }
    if let Ok(c) = CString::new(name) {
        unsafe { ffi::morloc_run_record_command(c.as_ptr()) };
    }
}

/// Record an error message verbatim. Lands in `summary.json` and in
/// the failure epilogue's `{error}` placeholder.
pub fn record_error(msg: &str) {
    if let Some(st) = STATE.get() {
        if let Ok(mut g) = st.error.lock() {
            *g = Some(msg.to_string());
        }
    }
    if let Ok(c) = CString::new(msg) {
        unsafe { ffi::morloc_run_record_error(c.as_ptr()) };
    }
}

/// Record `msg`, print `Error: <msg>` to stderr, and `clean_exit(1)`.
/// Single chokepoint for "morloc-level error reaches the user" so we
/// never forget the recording step at a new error site.
pub fn die_with_error(msg: &str) -> ! {
    record_error(msg);
    eprintln!("Error: {}", msg);
    crate::process::clean_exit(1);
}

fn render(
    tmpl: &str,
    name: &str,
    started_at: &str,
    finished_at: &str,
    runtime_seconds: f64,
    exit_code: Option<i32>,
    error: &str,
) -> String {
    let mut s = tmpl.replace("{name}", name);
    s = s.replace("{run_id}", &ffi_buf_to_string(|b, n| unsafe { ffi::morloc_run_id(b, n) }));
    s = s.replace("{started_at}", started_at);
    s = s.replace("{finished_at}", finished_at);
    s = s.replace("{runtime}", &format!("{:.6}", runtime_seconds));
    s = s.replace("{pid}", &std::process::id().to_string());
    s = s.replace("{hostname}", &ffi_buf_to_string(|b, n| unsafe { ffi::morloc_hostname(b, n) }));
    s = s.replace(
        "{exit_code}",
        &exit_code.map(|n| n.to_string()).unwrap_or_default(),
    );
    s.replace("{error}", error)
}

fn emit(line: &str) {
    let c = match CString::new(line) {
        Ok(c) => c,
        Err(_) => return,
    };
    unsafe { ffi::morloc_run_emit_line(c.as_ptr()) };
}

fn command_or_empty(st: &State) -> String {
    st.command
        .lock()
        .ok()
        .and_then(|g| g.clone())
        .unwrap_or_default()
}

/// Pick a template, render it under `ctx`, and emit. Idempotent via
/// `emitted` — a second call with the same flag is a no-op. Returns
/// without emitting when the template slot is `None`.
fn emit_once<F>(emitted: &AtomicBool, tmpl: Option<&str>, render_with: F)
where
    F: FnOnce(&str) -> String,
{
    let tmpl = match tmpl {
        Some(t) => t,
        None => return,
    };
    if emitted.swap(true, Ordering::SeqCst) {
        return;
    }
    emit(&render_with(tmpl));
}

/// Render and emit the prologue. Idempotent.
pub fn emit_prologue() {
    let st = match STATE.get() {
        Some(s) => s,
        None => return,
    };
    let tmpl = st.run_log.as_ref().and_then(|rl| rl.prologue.as_deref());
    emit_once(&st.prologue_emitted, tmpl, |t| {
        render(
            t,
            &command_or_empty(st),
            &st.started_at,
            "",
            0.0,
            None,
            "",
        )
    });
}

/// Render and emit the appropriate epilogue branch. Idempotent. Called
/// from `clean_exit`; safe to call from multiple paths.
pub fn emit_epilogue(exit_code: i32) {
    let st = match STATE.get() {
        Some(s) => s,
        None => return,
    };
    let rl = match st.run_log.as_ref() {
        Some(r) => r,
        None => return,
    };
    let tmpl = if exit_code == 0 {
        rl.epilogue_ok.as_deref()
    } else {
        rl.epilogue_fail.as_deref()
    };
    let finished_at = now_rfc3339();
    let runtime_seconds = st.started_instant.elapsed().as_secs_f64();
    let error_str = st
        .error
        .lock()
        .ok()
        .and_then(|g| g.clone())
        .unwrap_or_default();
    emit_once(&st.epilogue_emitted, tmpl, |t| {
        render(
            t,
            &command_or_empty(st),
            &st.started_at,
            &finished_at,
            runtime_seconds,
            Some(exit_code),
            &error_str,
        )
    });
}
