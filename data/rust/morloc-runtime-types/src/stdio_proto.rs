//! Wire constants for the pool <-> nexus stdio RPC.
//!
//! The pool client (`morloc_runtime::stream`) and the nexus server
//! (`morloc_nexus::stdio_server`) share this module so opcodes and
//! status bytes can't drift between the two ends of the socket.

/// `@next` on a stdio-bound IStream: request one sub-packet from stdin.
pub const OP_NEXT_STDIO:  u8 = 1;

/// `@write` / flush on a stdio-bound OStream: send one sub-packet to
/// stdout / stderr.
pub const OP_WRITE_STDIO: u8 = 2;

pub const STATUS_OK:  u8 = 0;
pub const STATUS_ERR: u8 = 1;
pub const STATUS_EOF: u8 = 2;

/// Stdio kind byte carried in the SHM registry slot and in RPC
/// dispatch. Immutable after `@open`.
pub const STDIO_KIND_STDIN:  u8 = 0;
pub const STDIO_KIND_STDOUT: u8 = 1;
pub const STDIO_KIND_STDERR: u8 = 2;
