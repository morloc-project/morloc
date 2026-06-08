//! Log message emission for `log: true` labeled manifolds.
//!
//! Each language pool calls these two C-ABI entry points. The morloc
//! compiler pre-renders the static placeholders ({name}, {module}, ANSI
//! color codes, etc.); this module fills in {date}, {runtime}, {id},
//! strips CSI sequences when stderr is not a TTY or NO_COLOR is set,
//! and writes the result + newline to stderr atomically.
//!
//! ```c
//! uint64_t morloc_log_next_id(void);
//! void morloc_log_emit(const char* tmpl, double runtime_seconds, uint64_t call_id);
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

/// Remove all CSI sequences (`ESC '[' ... [a-zA-Z]`). Bytes involved in
/// a CSI sequence are all ASCII; multi-byte UTF-8 continuation bytes
/// (`>= 0x80`) can't match the pattern, so the byte-level walk is
/// UTF-8 safe.
fn strip_csi(s: &str) -> String {
    // Fast path: most log lines contain no ANSI.
    if !s.as_bytes().contains(&0x1b) {
        return s.to_string();
    }
    let bytes = s.as_bytes();
    let mut out = Vec::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == 0x1b && i + 1 < bytes.len() && bytes[i + 1] == b'[' {
            let mut j = i + 2;
            while j < bytes.len() && !bytes[j].is_ascii_alphabetic() {
                j += 1;
            }
            if j < bytes.len() {
                j += 1;
            }
            i = j;
        } else {
            out.push(bytes[i]);
            i += 1;
        }
    }
    // SAFETY: stripped only ASCII byte runs from valid UTF-8 input.
    String::from_utf8(out).unwrap_or_default()
}

/// Safety: `tmpl` must be a null-terminated UTF-8 byte sequence. A null
/// pointer is treated as a no-op.
#[no_mangle]
pub unsafe extern "C" fn morloc_log_emit(
    tmpl: *const c_char,
    runtime_seconds: f64,
    call_id: u64,
) {
    if tmpl.is_null() {
        return;
    }
    let tmpl_str = match CStr::from_ptr(tmpl).to_str() {
        Ok(s) => s,
        Err(_) => return,
    };

    let date = Utc::now().format("%Y-%m-%dT%H:%M:%SZ").to_string();
    let id_str = format!("{}:{}", pool_pid(), call_id);
    let runtime_str = format!("{:.6}", runtime_seconds);

    let mut s = tmpl_str.replace("{date}", &date);
    s = s.replace("{runtime}", &runtime_str);
    s = s.replace("{id}", &id_str);

    if !color_enabled() {
        s = strip_csi(&s);
    }

    let stderr = io::stderr();
    let mut handle = stderr.lock();
    let _ = writeln!(handle, "{}", s);
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
        let s = "\x1b[1mLet's eat π\x1b[0m";
        assert_eq!(strip_csi(s), "Let's eat π");
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
