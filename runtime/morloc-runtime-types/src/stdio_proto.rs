//! Wire constants for the pool <-> nexus stdio RPC.
//!
//! The pool client (`morloc_runtime::stream`) and the nexus server
//! (`morloc_nexus::stdio_server`) share this module so opcodes and
//! status bytes can't drift between the two ends of the socket.

/// `@next` on a stdio-bound IStream: request one sub-packet from stdin.
pub const OP_NEXT_STDIO:  u8 = 1;

/// `@spawn`: start a channel's producer and watch it from the nexus, the one
/// process that outlives every pool worker. Request after the opcode:
/// `[handle: i64][mid: u32][path_len: u32][path][nargs: u32]` then per
/// argument packet `[len: u64][bytes]`. Response: ok, or err with a message.
pub const OP_SPAWN: u8 = 4;

/// `@open` or `@append` of a file `OStream`: the nexus opens, locks and
/// starts writing the file, and publishes its slot. Request after the
/// opcode: `[mode: u8][pid: u32][start: u64][call_id: u64][path_len: u32]
/// [path][schema_len: u32][schema]`. Response: ok with the handle in the
/// relptr field, or err with a message.
pub const OP_OPEN_STREAM: u8 = 5;

/// A stdout or stderr `OStream` a pool has published: the nexus starts
/// writing it. Request after the opcode: `[handle: i64]`. Response: ok, or
/// err with a message.
pub const OP_ADOPT_STREAM: u8 = 6;

pub const OPEN_CREATE: u8 = 0;
pub const OPEN_APPEND: u8 = 1;

pub const STATUS_OK:  u8 = 0;
pub const STATUS_ERR: u8 = 1;
pub const STATUS_EOF: u8 = 2;

/// The downstream consumer closed the pipe (a stdout/stderr write hit
/// `EPIPE`). Distinct from `STATUS_ERR` so the pool surfaces it as an
/// `<IO>` condition (`MorlocError::PipeClosed`) that `@catch` must not
/// swallow, rather than a generic recoverable error.
pub const STATUS_PIPE_CLOSED: u8 = 3;

/// Stdio kind byte carried in the SHM registry slot and in RPC
/// dispatch. Immutable after `@open`.
pub const STDIO_KIND_STDIN:  u8 = 0;
pub const STDIO_KIND_STDOUT: u8 = 1;
pub const STDIO_KIND_STDERR: u8 = 2;
