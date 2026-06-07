//! Re-export of the shared `MorlocError` type and FFI errmsg helpers.
//! Definitions live in `morloc-runtime-types::error` so the nexus and
//! libmorloc.so see the same canonical type without state duplication.

pub use morloc_runtime_types::error::*;
