// FFI surface for runtime configuration. The compiler emits two
// per-program knobs into the manifest (`inline_size`, `no_shm`); the
// nexus reads them and forwards both to libmorloc via env vars (for
// child pool processes) and via these setters (for its own libmorloc
// instance). Pool processes pick up the env vars on the first call
// into the runtime, via `packet::ensure_config_loaded`.

use std::ffi::CStr;
use std::os::raw::c_char;
use std::sync::atomic::Ordering;

use crate::packet::{set_file_packet_tmpdir, INLINE_THRESHOLD, SHM_ENABLED};

/// Override the inline threshold (in bytes). Values < 0 are treated as
/// 0 (never inline). Idempotent.
#[no_mangle]
pub unsafe extern "C" fn morloc_set_inline_threshold(bytes: i64) {
    let v: usize = if bytes < 0 { 0 } else { bytes as usize };
    INLINE_THRESHOLD.store(v, Ordering::Relaxed);
}

/// Read the live inline threshold.
#[no_mangle]
pub unsafe extern "C" fn morloc_get_inline_threshold() -> u64 {
    crate::packet::inline_threshold() as u64
}

/// Enable or disable shared memory at runtime. When disabled, any data
/// over the inline threshold is written to a temp file and passed by
/// path (PACKET_SOURCE_FILE) instead of via SHM (PACKET_SOURCE_RPTR).
#[no_mangle]
pub unsafe extern "C" fn morloc_set_shm_enabled(enabled: bool) {
    SHM_ENABLED.store(enabled, Ordering::Relaxed);
}

/// Read the live SHM-enabled flag.
#[no_mangle]
pub unsafe extern "C" fn morloc_get_shm_enabled() -> bool {
    crate::packet::shm_enabled()
}

/// Override the directory where file-packet intermediates land. A
/// null pointer or an empty string clears the override (libmorloc
/// then falls back to `$TMPDIR` / `/tmp`). The directory is created
/// lazily by the file-packet writer if it does not exist.
#[no_mangle]
pub unsafe extern "C" fn morloc_set_tmpdir(path: *const c_char) {
    if path.is_null() {
        set_file_packet_tmpdir(None);
        return;
    }
    match CStr::from_ptr(path).to_str() {
        Ok(s) if s.is_empty() => set_file_packet_tmpdir(None),
        Ok(s)                 => set_file_packet_tmpdir(Some(s.to_string())),
        Err(_)                => set_file_packet_tmpdir(None),
    }
}

#[cfg(test)]
mod tests {
    //! The env-var `Once` initializer in `packet::ensure_config_loaded`
    //! fires at most once per process, so we exercise the explicit FFI
    //! setters here rather than the env-var path. Run serialized to keep
    //! the global atomics from cross-test interference (cargo test is
    //! multi-threaded by default).

    use super::*;
    use crate::packet::{
        inline_threshold, shm_enabled, MORLOC_INLINE_THRESHOLD, TEST_CONFIG_LOCK,
    };

    #[test]
    fn setter_roundtrip_threshold() {
        let _guard = TEST_CONFIG_LOCK.lock().unwrap();
        unsafe { morloc_set_inline_threshold(1024) };
        assert_eq!(inline_threshold(), 1024);
        unsafe { morloc_set_inline_threshold(0) };
        assert_eq!(inline_threshold(), 0);
        unsafe { morloc_set_inline_threshold(MORLOC_INLINE_THRESHOLD as i64) };
        assert_eq!(inline_threshold(), MORLOC_INLINE_THRESHOLD);
    }

    #[test]
    fn negative_clamps_to_zero() {
        let _guard = TEST_CONFIG_LOCK.lock().unwrap();
        unsafe { morloc_set_inline_threshold(-42) };
        assert_eq!(inline_threshold(), 0);
        unsafe { morloc_set_inline_threshold(MORLOC_INLINE_THRESHOLD as i64) };
    }

    #[test]
    fn shm_toggle() {
        let _guard = TEST_CONFIG_LOCK.lock().unwrap();
        unsafe { morloc_set_shm_enabled(false) };
        assert!(!shm_enabled());
        unsafe { morloc_set_shm_enabled(true) };
        assert!(shm_enabled());
    }

    #[test]
    fn tmpdir_roundtrip() {
        use crate::packet::file_packet_tmpdir;
        use std::ffi::CString;
        let _guard = TEST_CONFIG_LOCK.lock().unwrap();

        let c = CString::new("/tmp/morloc-tmpdir-test").unwrap();
        unsafe { morloc_set_tmpdir(c.as_ptr()) };
        assert_eq!(file_packet_tmpdir().as_deref(), Some("/tmp/morloc-tmpdir-test"));

        // empty string -> unset
        let empty = CString::new("").unwrap();
        unsafe { morloc_set_tmpdir(empty.as_ptr()) };
        assert!(file_packet_tmpdir().is_none());

        // null -> unset
        unsafe { morloc_set_tmpdir(std::ptr::null()) };
        assert!(file_packet_tmpdir().is_none());
    }
}
