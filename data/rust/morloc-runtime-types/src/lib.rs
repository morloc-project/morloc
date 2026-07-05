//! Stateless morloc runtime types and pure functions.
//!
//! Holds the surface of `morloc-runtime` that has no `static` or
//! `thread_local!` state: wire-format types, schema definitions, packet
//! headers, pure parsers, hash functions, and the C-ABI CSchema mirror.
//! Both `libmorloc.so` (cdylib) and `morloc-nexus` depend on this crate
//! as a normal rlib so they can share the canonical type definitions
//! without duplicating any per-process state.
//!
//! Anything that would touch a `static`, `thread_local!`, or
//! `Once`/`Mutex`/`AtomicXxx` singleton is intentionally NOT in this
//! crate -- it lives only in `morloc-runtime` so the nexus must reach
//! it through libmorloc.so's C ABI.

pub mod error;
pub mod hash;
pub mod shm_types;
pub mod schema;
pub mod cschema;
pub mod null_check;
pub mod packet;
pub mod pattern;
pub mod compression;
pub mod daemon_socket;
pub mod stream_handle;
pub mod stdio_proto;

pub use cschema::is_top_null;

/// FFI return codes for the print_voidstar / pretty_print_voidstar
/// C ABI: 0 ok, 1 error (see errmsg), 2 downstream pipe closed. Shared
/// so libmorloc.so and morloc-nexus can't drift.
pub const PRINT_RESULT_OK: i32 = 0;
pub const PRINT_RESULT_ERR: i32 = 1;
pub const PRINT_RESULT_PIPE_CLOSED: i32 = 2;
