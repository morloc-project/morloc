//! Re-export of the shared `MorlocError` type and FFI errmsg helpers.
//! Definitions live in `morloc-runtime-types::error` so the nexus and
//! libmorloc.so see the same canonical type without state duplication.

pub use morloc_runtime_types::error::*;

/// Run the body of a C ABI entry point with `errmsg` cleared, converting
/// a panic into an error return: `errmsg` receives the panic message and
/// `on_panic` is the value returned. A panic that reaches the `extern "C"` boundary aborts
/// the process, which skips every unwind-time release the arena relies
/// on, so no decoder is trusted not to panic on the bytes it is handed.
///
/// # Safety
/// `errmsg` must be a valid `char**` or null.
pub unsafe fn guarded<R>(
    errmsg: *mut *mut std::ffi::c_char,
    on_panic: R,
    body: impl FnOnce() -> R,
) -> R {
    clear_errmsg(errmsg);
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(body)) {
        Ok(r) => r,
        Err(payload) => {
            let msg = payload
                .downcast_ref::<&str>()
                .map(|s| s.to_string())
                .or_else(|| payload.downcast_ref::<String>().cloned())
                .unwrap_or_else(|| "unknown panic".into());
            set_errmsg(errmsg, &MorlocError::Other(format!("internal error: {}", msg)));
            on_panic
        }
    }
}
