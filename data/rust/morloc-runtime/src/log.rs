//! Log message emission for `log: true` labeled manifolds.
//!
//! Each language pool calls these two C-ABI entry points. The morloc
//! compiler pre-renders the static placeholders ({name}, {module}, ANSI
//! color codes, etc.); this module fills in {date}, {runtime}, {id},
//! strips CSI sequences when stderr is not a TTY or NO_COLOR is set,
//! and writes the result + newline to stderr atomically.
//!
//! When `MORLOC_RUN_DIR` is set, the plain-text (color-stripped) form
//! of every emitted line is also appended to `$MORLOC_RUN_DIR/$group/log`
//! via the runtime [`crate::run::tee_log_line`] helper. The tee is keyed
//! by the user-chosen label group (e.g. `"a"` from `a:map`), so per-
//! label tail/grep workflows survive across labels and processes.
//!
//! ```c
//! uint64_t morloc_log_next_id(void);
//! void morloc_log_emit(
//!     const char* tmpl,
//!     const char* group,
//!     double runtime_seconds,
//!     uint64_t call_id);
//! ```

use chrono::Utc;
use libc::c_char;
use std::ffi::CStr;
use std::io::{self, IsTerminal, Write};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::OnceLock;

static CALL_COUNTER: AtomicU64 = AtomicU64::new(0);

#[no_mangle]
pub extern "C" fn morloc_log_next_id() -> u64 {
    CALL_COUNTER.fetch_add(1, Ordering::Relaxed)
}

fn pool_pid() -> i32 {
    // Stable post-fork, so caching across emits is safe (no syscall per call).
    static CACHED: OnceLock<i32> = OnceLock::new();
    *CACHED.get_or_init(|| unsafe { libc::getpid() })
}

/// Color is suppressed when stderr is redirected (pipe/file) OR when
/// `NO_COLOR` is set (https://no-color.org/).
fn color_enabled() -> bool {
    static CACHED: OnceLock<bool> = OnceLock::new();
    *CACHED.get_or_init(|| {
        std::env::var_os("NO_COLOR").is_none() && io::stderr().is_terminal()
    })
}

/// Global kill-switch for all morloc-emitted log lines. When
/// `MORLOC_QUIET` is set to any non-empty value the per-label start /
/// done lines, the prologue, and the epilogue are all suppressed at
/// the source -- neither stderr nor the rundir tee receives them.
/// Cached on first read so the env-var lookup is a one-time cost.
fn quiet() -> bool {
    static CACHED: OnceLock<bool> = OnceLock::new();
    *CACHED.get_or_init(|| {
        std::env::var_os("MORLOC_QUIET")
            .map(|v| !v.is_empty())
            .unwrap_or(false)
    })
}

/// Remove all ANSI escape sequences (CSI color codes, OSC, etc.)
/// from a string. Delegates to [`anstream::adapter::strip_str`],
/// which uses an ECMA-48-conformant parser; the function exists
/// only as a named call-site for clarity at the use sites in
/// [`morloc_log_emit`] and [`morloc_run_emit_line`].
fn strip_csi(s: &str) -> String {
    anstream::adapter::strip_str(s).to_string()
}

/// Safety: `tmpl` must be a null-terminated UTF-8 byte sequence. A null
/// pointer is treated as a no-op. `group` may be NULL or empty -- if so,
/// the per-label tee is skipped; the stderr emission still happens.
#[no_mangle]
pub unsafe extern "C" fn morloc_log_emit(
    tmpl: *const c_char,
    group: *const c_char,
    runtime_seconds: f64,
    call_id: u64,
) {
    if tmpl.is_null() || quiet() {
        return;
    }
    let tmpl_str = match CStr::from_ptr(tmpl).to_str() {
        Ok(s) => s,
        Err(_) => return,
    };
    let group_str: Option<&str> = if group.is_null() {
        None
    } else {
        CStr::from_ptr(group).to_str().ok()
    };

    let date = Utc::now().format("%Y-%m-%dT%H:%M:%SZ").to_string();
    let id_str = format!("{}:{}", pool_pid(), call_id);
    let runtime_str = format!("{:.6}", runtime_seconds);

    let mut s = tmpl_str.replace("{date}", &date);
    s = s.replace("{runtime}", &runtime_str);
    s = s.replace("{id}", &id_str);

    // Always compute the plain form -- it's what the tee writes, and
    // it's what stderr writes when color is suppressed. strip_csi has a
    // fast path when the string has no ESC bytes, so this is free for
    // the common no-color template.
    let plain = strip_csi(&s);

    {
        let stderr = io::stderr();
        let mut handle = stderr.lock();
        let text: &str = if color_enabled() { &s } else { &plain };
        let _ = writeln!(handle, "{}", text);
    }

    if let Some(g) = group_str {
        crate::run::tee_log_line(g, &plain);
    }
}

/// Emit a fully-rendered run-scope line (prologue or epilogue) to
/// stderr and tee to `$MORLOC_RUN_DIR/log` when the rundir is active.
/// Distinct from [`morloc_log_emit`] because run-scope events are not
/// per-label, have no `{date}`/`{runtime}`/`{id}` placeholders to
/// fill, and write to the top-level log file rather than a label
/// subdir.
///
/// Suppressed entirely under `MORLOC_QUIET`. NO_COLOR / TTY rules
/// mirror the per-label emitter for visual consistency.
///
/// Safety: `text` must be a NUL-terminated UTF-8 string.
#[no_mangle]
pub unsafe extern "C" fn morloc_run_emit_line(text: *const c_char) {
    if text.is_null() || quiet() {
        return;
    }
    let s = match CStr::from_ptr(text).to_str() {
        Ok(s) => s,
        Err(_) => return,
    };
    let plain = strip_csi(s);
    {
        let stderr = io::stderr();
        let mut handle = stderr.lock();
        let out: &str = if color_enabled() { s } else { &plain };
        let _ = writeln!(handle, "{}", out);
    }
    crate::run::tee_run_log_line(&plain);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn strip_csi_removes_color_codes() {
        let s = "\x1b[31mred\x1b[0m plain";
        assert_eq!(strip_csi(s), "red plain");
    }

    #[test]
    fn strip_csi_leaves_unicode_intact() {
        // "caf" + U+00E9 (e-acute, UTF-8 0xC3 0xA9) inside CSI. The
        // byte-level CSI walker must skip ESC..letter but pass through
        // 0xC3 0xA9 unchanged.
        let mut bytes: Vec<u8> = Vec::new();
        bytes.extend_from_slice(b"\x1b[1mcaf");
        bytes.extend_from_slice(&[0xC3, 0xA9]);
        bytes.extend_from_slice(b"\x1b[0m");
        let s = std::str::from_utf8(&bytes).unwrap();
        let expected = String::from_utf8(vec![b'c', b'a', b'f', 0xC3, 0xA9]).unwrap();
        assert_eq!(strip_csi(s), expected);
    }

    #[test]
    fn strip_csi_fast_path() {
        // No ESC byte -> returns input copy unchanged.
        let s = "plain ASCII no ANSI";
        assert_eq!(strip_csi(s), s);
    }

    #[test]
    fn next_id_monotonic() {
        let a = morloc_log_next_id();
        let b = morloc_log_next_id();
        assert!(b > a);
    }
}
