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

/// Shared i32 C-ABI result code (0=OK, 1=ERR, 2=PIPE_CLOSED) for the stdio
/// intrinsics that report a broken pipe by return code with no errmsg:
/// the print path AND mlc_write / mlc_flush / mlc_close. Named neutrally
/// (not PRINT_*) since @write/@flush/@close do not print; the C++ pool
/// mirrors it as MLC_RESULT_PIPE_CLOSED.
pub const MLC_RESULT_PIPE_CLOSED: i32 = 2;

/// Back-compat alias for the print path.
pub const PRINT_RESULT_PIPE_CLOSED: i32 = MLC_RESULT_PIPE_CLOSED;

/// Sentinel returned by `normalize_data_packet_to_fd` (an i64 byte count,
/// negative on error) when the destination fd's downstream closed the pipe
/// (EPIPE). Distinct from the generic `-1` error so the nexus stdout
/// bridge can classify it as a broken pipe rather than a generic failure.
pub const PACKET_TO_FD_PIPE_CLOSED: i64 = -2;
