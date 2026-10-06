//! Re-export of the shared `MorlocError` type and FFI errmsg helpers.
//! Definitions live in `morloc-runtime-types::error` so the nexus and
//! libmorloc.so see the same canonical type without state duplication.

pub use morloc_runtime_types::error::*;

/// Run a call into a third-party format library with `errmsg` cleared,
/// returning `on_panic` with the panic message in `errmsg` if the library
/// panics on the bytes it is handed (model/panic.md PANIC-2).
///
/// # Safety
/// `errmsg` must be a valid `char**` or null.
pub unsafe fn guarded<R>(
    errmsg: *mut *mut std::ffi::c_char,
    on_panic: R,
    body: impl FnOnce() -> R,
) -> R {
    clear_errmsg(errmsg);
    match morloc_runtime_types::panic::catch(body) {
        Ok(r) => r,
        Err(caught) => {
            set_errmsg(errmsg, &MorlocError::Other(format!("internal error: {}", caught.message)));
            on_panic
        }
    }
}
