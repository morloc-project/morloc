//! Re-export of the shared `MorlocError` type and FFI errmsg helpers.
//! Definitions live in `morloc-runtime-types::error` so the nexus and
//! libmorloc.so see the same canonical type without state duplication.

pub use morloc_runtime_types::error::*;

/// Run a third-party format library's call on bytes from outside the
/// program, turning a panic the library raises on malformed input into an
/// error (model/panic.md PANIC-2). Only the library call goes inside.
pub fn decode<T>(what: &str, call: impl FnOnce() -> T) -> Result<T, MorlocError> {
    morloc_runtime_types::panic::catch(call).map_err(|caught| MorlocError::Other(format!("{what}: {}", caught.message)))
}
