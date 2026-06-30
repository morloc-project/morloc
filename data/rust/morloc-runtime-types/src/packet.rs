//! Stateless half of the packet protocol: header layout, constants,
//! pure parsers, and pure constructors.
//!
//! The runtime-tunable config statics (`INLINE_THRESHOLD`,
//! `SHM_ENABLED`, `TMPDIR`, `CONFIG_INIT`) and the SHM-touching
//! payload helpers (`get_data_value`, `read_voidstar_binary`) live in
//! `morloc-runtime::packet` so they are not duplicated across the
//! nexus and libmorloc.so processes.

use crate::error::MorlocError;
use crate::schema::Schema;
use crate::shm_types::RelPtr;

// ── Magic & version constants ──────────────────────────────────────────────

pub const PACKET_MAGIC: u32 = 0x0707_f86d;
pub const THIS_PLAIN: u16 = 0;
pub const THIS_VERSION: u16 = 0;
pub const DEFAULT_FLAVOR: u16 = 0;
pub const DEFAULT_MODE: u16 = 0;

// ── Command type discriminants ─────────────────────────────────────────────

pub const PACKET_TYPE_DATA: u8 = 0;
pub const PACKET_TYPE_CALL: u8 = 1;
pub const PACKET_TYPE_PING: u8 = 2;
pub const PACKET_TYPE_STREAM: u8 = 3;
pub const PACKET_TYPE_FOOTER: u8 = 4;

// Stream packet length-field sentinel. Streams are append-only and have
// no global content length; the header's `length` field is always set to
// `u64::MAX`. Future versions may interpret a non-sentinel value as a
// declared maximum stream length, but no current writer or reader uses
// that semantics.
pub const STREAM_LENGTH_SENTINEL: u64 = u64::MAX;

// ── Stream/file handle kinds ───────────────────────────────────────────────
//
// `mlc_open` takes a `kind` byte selecting which handle type to create.
// The registry stores the kind alongside each entry so subsequent
// operations (`mlc_close`, `mlc_eval_pattern`, etc.) can dispatch
// internally without the caller having to specify the kind.

pub const MLC_KIND_IFILE: u8 = 0;
pub const MLC_KIND_ISTREAM: u8 = 1;
pub const MLC_KIND_OSTREAM: u8 = 2;

pub const fn handle_kind_name(k: u8) -> &'static str {
    match k {
        MLC_KIND_IFILE => "IFile",
        MLC_KIND_ISTREAM => "IStream",
        MLC_KIND_OSTREAM => "OStream",
        _ => "unknown",
    }
}

// ── Data source ────────────────────────────────────────────────────────────

pub const PACKET_SOURCE_MESG: u8 = 0x00;
pub const PACKET_SOURCE_FILE: u8 = 0x01;
pub const PACKET_SOURCE_RPTR: u8 = 0x02;

// ── Data format ────────────────────────────────────────────────────────────

pub const PACKET_FORMAT_JSON: u8 = 0x00;
pub const PACKET_FORMAT_MSGPACK: u8 = 0x01;
pub const PACKET_FORMAT_TEXT: u8 = 0x02;
pub const PACKET_FORMAT_DATA: u8 = 0x03;
pub const PACKET_FORMAT_VOIDSTAR: u8 = 0x04;
pub const PACKET_FORMAT_ARROW: u8 = 0x05;

// ── Compression / encryption ───────────────────────────────────────────────

pub const PACKET_COMPRESSION_NONE: u8 = 0x00;
pub const PACKET_COMPRESSION_ZSTD: u8 = 0x01;
pub const PACKET_ENCRYPTION_NONE: u8 = 0x00;

// ── Status ─────────────────────────────────────────────────────────────────

pub const PACKET_STATUS_PASS: u8 = 0x00;
pub const PACKET_STATUS_FAIL: u8 = 0x01;

// ── Entrypoint ─────────────────────────────────────────────────────────────

pub const PACKET_ENTRYPOINT_LOCAL: u8 = 0x00;
pub const PACKET_ENTRYPOINT_REMOTE_SFS: u8 = 0x01;

// ── Lower-case display names for the packet-byte constants above. ──────────
//
// Single source of truth for the strings used by `morloc-nexus file`,
// FFI error messages, and any future tooling. Returns `"unknown"`
// for unrecognized bytes rather than panicking — these are derived
// from on-disk packets that may carry future / corrupt values.

pub const fn packet_type_name(t: u8) -> &'static str {
    match t {
        PACKET_TYPE_DATA => "data",
        PACKET_TYPE_CALL => "call",
        PACKET_TYPE_PING => "ping",
        PACKET_TYPE_STREAM => "stream",
        PACKET_TYPE_FOOTER => "footer",
        _ => "unknown",
    }
}

pub const fn packet_source_name(s: u8) -> &'static str {
    match s {
        PACKET_SOURCE_MESG => "mesg",
        PACKET_SOURCE_FILE => "file",
        PACKET_SOURCE_RPTR => "rptr",
        _ => "unknown",
    }
}

pub const fn packet_format_name(f: u8) -> &'static str {
    match f {
        PACKET_FORMAT_JSON => "json",
        PACKET_FORMAT_MSGPACK => "msgpack",
        PACKET_FORMAT_TEXT => "text",
        PACKET_FORMAT_DATA => "data",
        PACKET_FORMAT_VOIDSTAR => "voidstar",
        PACKET_FORMAT_ARROW => "arrow",
        _ => "unknown",
    }
}

pub const fn packet_compression_name(c: u8) -> &'static str {
    match c {
        PACKET_COMPRESSION_NONE => "none",
        PACKET_COMPRESSION_ZSTD => "zstd",
        _ => "unknown",
    }
}

pub const fn packet_entrypoint_name(e: u8) -> &'static str {
    match e {
        PACKET_ENTRYPOINT_LOCAL => "local",
        PACKET_ENTRYPOINT_REMOTE_SFS => "remote_sfs",
        _ => "unknown",
    }
}

// ── Inline-threshold default ───────────────────────────────────────────────

// Compile-time default for the inline threshold. The effective runtime
// value (an `AtomicUsize` initialised from this constant, env vars, and
// FFI setters) lives in `morloc-runtime::packet` because it is process
// global state. The constant itself is safe to share.
pub const MORLOC_INLINE_THRESHOLD: usize = 64 * 1024;

// ── Metadata ───────────────────────────────────────────────────────────────

// ── Stream EOF tail ────────────────────────────────────────────────────────
//
// Stream files (`PACKET_TYPE_STREAM`) optionally end with a
// `PACKET_TYPE_FOOTER` packet, followed by a fixed 8-byte tail at the
// end of the file. The tail lets a reader detect footer presence in a
// single `pread` of the last 8 bytes, without scanning the body.
//
// Tail layout (8 bytes total):
//   bytes 0..4: footer_length  (u32 LE) -- byte size of the footer packet
//                                          *not* including this tail
//   bytes 4..8: footer_magic   ([u8; 4]) -- PACKET_MAGIC written
//                                          with bytes reversed
//                                          (07 07 f8 6d), so it cannot
//                                          collide with a packet header
//                                          start (which is the same
//                                          bytes in LE: 6d f8 07 07).
//
// A reader that sees `STREAM_TAIL_MAGIC` at `file_size - 4` walks back
// `footer_length + 8` bytes to the footer packet's first byte. The
// reader then re-validates the candidate footer (`PACKET_MAGIC` + a
// `PACKET_TYPE_FOOTER` command-type byte) before trusting it -- the
// 4-byte tail magic plus a 32-byte well-formed footer header give
// sufficient defence against accidental data-look-alikes. Footer
// absence (writer crashed, stream in-progress) is the case where the
// tail magic does not match.

pub const STREAM_TAIL_SIZE: usize = 8;
/// PACKET_MAGIC (0x0707_f86d) with its bytes reversed.
/// PACKET_MAGIC in LE is `[0x6d, 0xf8, 0x07, 0x07]`; the reversed bytes
/// `[0x07, 0x07, 0xf8, 0x6d]` cannot start a packet header.
pub const STREAM_TAIL_MAGIC: [u8; 4] = [0x07, 0x07, 0xf8, 0x6d];

/// Encode the 8-byte tail for an EOF-finalised stream file.
pub fn encode_stream_tail(footer_length: u32) -> [u8; STREAM_TAIL_SIZE] {
    let mut out = [0u8; STREAM_TAIL_SIZE];
    out[..4].copy_from_slice(&footer_length.to_le_bytes());
    out[4..].copy_from_slice(&STREAM_TAIL_MAGIC);
    out
}

/// Decode the 8-byte tail. Returns `Some(footer_length)` if the magic
/// matches; `None` otherwise (no footer present).
pub fn decode_stream_tail(tail: &[u8; STREAM_TAIL_SIZE]) -> Option<u32> {
    if tail[4..] != STREAM_TAIL_MAGIC {
        return None;
    }
    Some(u32::from_le_bytes(tail[..4].try_into().unwrap()))
}

pub const METADATA_TYPE_SCHEMA_STRING: u8 = 0x01;
pub const METADATA_TYPE_XXHASH: u8 = 0x02;
/// Layer 3 vol_idx hint: the producer of an on-disk MESG+VOIDSTAR
/// packet has encoded a 15-bit volume index into the high bits of
/// every relptr in the payload. The reader that mmaps this file can
/// (a) register the payload at that slot if it's free in the reader's
/// VOLUMES table -- the relptrs decode correctly with no walk -- or
/// (b) register at a different slot and run `remap_volume_indices`
/// to XOR-rewrite the high bits. The 16-bit body is a little-endian
/// u16 carrying the producer's chosen index; the high bit is reserved.
pub const METADATA_TYPE_VOL_INDEX: u8 = 0x03;
/// Frame index for multi-frame compressed payloads. The entry body is
/// `[frame_count: u32 LE, [(uncompressed_size: u64 LE, compressed_size: u64 LE); frame_count]]`.
/// Every `PACKET_COMPRESSION_ZSTD` payload carries this index so
/// decoders can `shmalloc(total_uncompressed_size)` once, slice the
/// destination per frame, and dispatch frame decoding to a worker
/// pool. The frame index is mandatory on compressed packets under the
/// multi-frame format -- a compressed packet that arrives without an
/// index is rejected by the decoder.
///
/// The entry's metadata `size` field may exceed
/// `4 + 16 * frame_count`: streaming encoders reserve worst-case room
/// up front (from `ceil(estimated_payload_bytes / FRAME_CHUNK_SIZE)`)
/// and patch the actual data in after the payload streams out, so
/// trailing slots may be zero. Decoders use the `frame_count` field
/// inside the entry body, not the entry's metadata size, to know how
/// many frames to consume.
pub const METADATA_TYPE_FRAME_INDEX: u8 = 0x04;

// ── Stream-footer metadata block types ────────────────────────────────────
//
// These types appear in the body of a `PACKET_TYPE_FOOTER` packet that
// terminates a `PACKET_TYPE_STREAM` file.
//
// Layout commitments:
//   - `SUBPACKET_INDEX` (`0x05`) appears only in the *final* footer (the
//     one written by `@close` / implicit-close-on-exit). Body is
//     `u64 count` followed by `count * u64` byte-offsets (offset of each
//     sub-packet header within the file, relative to file start).
//   - `STREAM_DIAG` (`0x06`) is a fixed-size `StreamDiag` struct (see
//     below) carrying running counters + tail window. Appears in both
//     temp and final footers. The temp footer's whole purpose is this
//     block.
//   - `FOOTER_FINAL` (`0x07`) is presence-only (zero-length body). Set in
//     the final footer; absent in temp footers. A reader uses this to
//     distinguish "writer crashed mid-stream" (temp footer only) from
//     "writer closed cleanly" (final footer).
pub const METADATA_TYPE_SUBPACKET_INDEX: u8 = 0x05;
pub const METADATA_TYPE_STREAM_DIAG: u8 = 0x06;
pub const METADATA_TYPE_FOOTER_FINAL: u8 = 0x07;

pub const METADATA_HEADER_MAGIC: [u8; 3] = *b"mmh";

// ── Frame-index helpers ────────────────────────────────────────────────────

/// One entry in a multi-frame compressed packet's frame index.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FrameEntry {
    pub uncompressed_size: u64,
    pub compressed_size: u64,
}

/// Bytes per frame slot inside the frame-index entry payload
/// (`u64` uncompressed_size + `u64` compressed_size). Encoders use
/// this to size the worst-case reservation: an entry covering
/// `n` frames is `FRAME_INDEX_HEADER_BYTES + n * FRAME_ENTRY_BYTES`.
pub const FRAME_ENTRY_BYTES: usize = 16;

/// Bytes before the per-frame array in the entry payload (the
/// `frame_count: u32 LE` prefix).
pub const FRAME_INDEX_HEADER_BYTES: usize = 4;

/// Encode a frame-index entry payload (excluding the 8-byte
/// `mmh + type + size` metadata-entry header). The returned bytes are
/// what an encoder writes into the body of a
/// `METADATA_TYPE_FRAME_INDEX` entry.
pub fn encode_frame_index_entry(frames: &[FrameEntry]) -> Vec<u8> {
    let mut out = Vec::with_capacity(
        FRAME_INDEX_HEADER_BYTES + frames.len() * FRAME_ENTRY_BYTES,
    );
    out.extend_from_slice(&(frames.len() as u32).to_le_bytes());
    for f in frames {
        out.extend_from_slice(&f.uncompressed_size.to_le_bytes());
        out.extend_from_slice(&f.compressed_size.to_le_bytes());
    }
    out
}

/// Decode a frame-index entry payload (the body of a
/// `METADATA_TYPE_FRAME_INDEX` entry, not the 8-byte entry header).
/// Returns `Err` on truncation or a `frame_count` that exceeds the
/// available data. Trailing bytes after `frame_count * FRAME_ENTRY_BYTES`
/// are ignored (encoder may reserve more than needed; padding stays
/// zero).
pub fn decode_frame_index_entry(data: &[u8]) -> Result<Vec<FrameEntry>, MorlocError> {
    if data.len() < FRAME_INDEX_HEADER_BYTES {
        return Err(MorlocError::Packet(format!(
            "frame index entry truncated: {} bytes",
            data.len()
        )));
    }
    let frame_count =
        u32::from_le_bytes([data[0], data[1], data[2], data[3]]) as usize;
    let needed = FRAME_INDEX_HEADER_BYTES
        .checked_add(frame_count.checked_mul(FRAME_ENTRY_BYTES).ok_or_else(|| {
            MorlocError::Packet("frame_count overflow".into())
        })?)
        .ok_or_else(|| MorlocError::Packet("frame index size overflow".into()))?;
    if data.len() < needed {
        return Err(MorlocError::Packet(format!(
            "frame index claims {} frames but entry only has {} bytes",
            frame_count,
            data.len()
        )));
    }
    let mut frames = Vec::with_capacity(frame_count);
    let mut cur = FRAME_INDEX_HEADER_BYTES;
    for _ in 0..frame_count {
        let uncompressed_size = u64::from_le_bytes(
            data[cur..cur + 8].try_into().unwrap(),
        );
        let compressed_size = u64::from_le_bytes(
            data[cur + 8..cur + 16].try_into().unwrap(),
        );
        frames.push(FrameEntry { uncompressed_size, compressed_size });
        cur += FRAME_ENTRY_BYTES;
    }
    Ok(frames)
}

/// Read the multi-frame index from a packet's metadata block, if
/// present. Returns `Ok(None)` when the packet has no frame index
/// entry (the legal shape for uncompressed packets); returns
/// `Ok(Some(frames))` when the entry is present and parses cleanly.
pub fn read_frame_index_from_meta(
    packet: &[u8],
) -> Result<Option<Vec<FrameEntry>>, MorlocError> {
    for (kind, data) in iter_packet_metadata(packet)? {
        if kind == METADATA_TYPE_FRAME_INDEX {
            return decode_frame_index_entry(data).map(Some);
        }
    }
    Ok(None)
}

// ── Packed structs matching the C binary layout ────────────────────────────

/// 8-byte command union. We represent each variant as its own struct and
/// transmute at the boundary.
#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct CommandType {
    pub cmd_type: u8,
    pub padding: [u8; 7],
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct CommandCall {
    pub cmd_type: u8,
    pub entrypoint: u8,
    pub padding: [u8; 2],
    pub midx: u32,
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct CommandData {
    pub cmd_type: u8,
    pub source: u8,
    pub format: u8,
    pub compression: u8,
    pub encryption: u8,
    pub status: u8,
    pub padding: [u8; 2],
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct CommandPing {
    pub cmd_type: u8,
    pub padding: [u8; 7],
}

/// The 8-byte command field stored as raw bytes. Interpreted based on the
/// first byte (cmd_type discriminant).
#[derive(Clone, Copy)]
#[repr(C, packed)]
pub union PacketCommand {
    pub cmd_type: CommandType,
    pub call: CommandCall,
    pub data: CommandData,
    pub ping: CommandPing,
    pub raw: [u8; 8],
}

impl std::fmt::Debug for PacketCommand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let tag = unsafe { self.cmd_type.cmd_type };
        match tag {
            PACKET_TYPE_DATA => write!(f, "Command::Data({:?})", unsafe { self.data }),
            PACKET_TYPE_CALL => write!(f, "Command::Call({:?})", unsafe { self.call }),
            PACKET_TYPE_PING => write!(f, "Command::Ping"),
            PACKET_TYPE_STREAM => write!(f, "Command::Stream"),
            PACKET_TYPE_FOOTER => write!(f, "Command::Footer"),
            _ => write!(f, "Command::Unknown({tag})"),
        }
    }
}

/// 32-byte packet header. Must match morloc_packet_header_t exactly.
#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct PacketHeader {
    pub magic: u32,
    pub plain: u16,
    pub version: u16,
    pub flavor: u16,
    pub mode: u16,
    pub command: PacketCommand,
    pub offset: u32,
    pub length: u64,
}

const _: () = assert!(std::mem::size_of::<PacketHeader>() == 32);
const _: () = assert!(std::mem::size_of::<PacketCommand>() == 8);
const _: () = assert!(std::mem::size_of::<CommandCall>() == 8);
const _: () = assert!(std::mem::size_of::<CommandData>() == 8);

/// 8-byte metadata header.
#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct MetadataHeader {
    pub magic: [u8; 3],
    pub metadata_type: u8,
    pub size: u32,
}

const _: () = assert!(std::mem::size_of::<MetadataHeader>() == 8);

// ── Packet construction helpers ────────────────────────────────────────────

impl PacketHeader {
    fn new(command: PacketCommand, offset: u32, length: u64) -> Self {
        PacketHeader {
            magic: PACKET_MAGIC,
            plain: THIS_PLAIN,
            version: THIS_VERSION,
            flavor: DEFAULT_FLAVOR,
            mode: DEFAULT_MODE,
            command,
            offset,
            length,
        }
    }

    /// Create a ping packet (no payload).
    pub fn ping() -> Self {
        Self::new(
            PacketCommand {
                ping: CommandPing {
                    cmd_type: PACKET_TYPE_PING,
                    padding: [0; 7],
                },
            },
            0, // no metadata
            0, // no payload
        )
    }

    /// Create a local call packet header.
    pub fn local_call(midx: u32, payload_len: u64) -> Self {
        Self::new(
            PacketCommand {
                call: CommandCall {
                    cmd_type: PACKET_TYPE_CALL,
                    entrypoint: PACKET_ENTRYPOINT_LOCAL,
                    padding: [0; 2],
                    midx,
                },
            },
            0, // no metadata between header and arg packets
            payload_len,
        )
    }

    /// Create a remote call packet header.
    pub fn remote_call(midx: u32, payload_len: u64) -> Self {
        Self::new(
            PacketCommand {
                call: CommandCall {
                    cmd_type: PACKET_TYPE_CALL,
                    entrypoint: PACKET_ENTRYPOINT_REMOTE_SFS,
                    padding: [0; 2],
                    midx,
                },
            },
            0,
            payload_len,
        )
    }

    /// Create a data packet header for inline message data.
    pub fn data_mesg(format: u8, payload_len: u64) -> Self {
        Self::new(
            PacketCommand {
                data: CommandData {
                    cmd_type: PACKET_TYPE_DATA,
                    source: PACKET_SOURCE_MESG,
                    format,
                    compression: PACKET_COMPRESSION_NONE,
                    encryption: PACKET_ENCRYPTION_NONE,
                    status: PACKET_STATUS_PASS,
                    padding: [0; 2],
                },
            },
            0, // metadata size set separately when building full packet
            payload_len,
        )
    }

    /// Create a data packet header for relative pointer (shared memory).
    pub fn data_rptr(format: u8, payload_len: u64) -> Self {
        Self::new(
            PacketCommand {
                data: CommandData {
                    cmd_type: PACKET_TYPE_DATA,
                    source: PACKET_SOURCE_RPTR,
                    format,
                    compression: PACKET_COMPRESSION_NONE,
                    encryption: PACKET_ENCRYPTION_NONE,
                    status: PACKET_STATUS_PASS,
                    padding: [0; 2],
                },
            },
            0, // metadata size set separately when building full packet
            payload_len,
        )
    }

    /// Create a stream packet header. Length is fixed at
    /// `STREAM_LENGTH_SENTINEL` (`u64::MAX`); writers never touch the
    /// header after creation. Metadata blocks (e.g. the element schema)
    /// live between the header and the first sub-packet; the caller
    /// sets `offset` directly after construction to the metadata size.
    pub fn stream() -> Self {
        Self::new(
            PacketCommand {
                ping: CommandPing {
                    cmd_type: PACKET_TYPE_STREAM,
                    padding: [0; 7],
                },
            },
            0, // metadata size set separately when building the full header
            STREAM_LENGTH_SENTINEL,
        )
    }

    /// Create a footer packet header. `length` is the byte size of the
    /// footer's tagged-block payload (analogous to a data packet's
    /// payload). Metadata blocks before the payload are optional;
    /// callers may set `offset` after construction if needed.
    pub fn footer(payload_len: u64) -> Self {
        Self::new(
            PacketCommand {
                ping: CommandPing {
                    cmd_type: PACKET_TYPE_FOOTER,
                    padding: [0; 7],
                },
            },
            0,
            payload_len,
        )
    }

    /// Create a fail packet with an error message.
    pub fn fail(error_msg_len: u64) -> Self {
        Self::new(
            PacketCommand {
                data: CommandData {
                    cmd_type: PACKET_TYPE_DATA,
                    source: PACKET_SOURCE_MESG,
                    format: PACKET_FORMAT_TEXT,
                    compression: PACKET_COMPRESSION_NONE,
                    encryption: PACKET_ENCRYPTION_NONE,
                    status: PACKET_STATUS_FAIL,
                    padding: [0; 2],
                },
            },
            0,
            error_msg_len,
        )
    }

    /// Total packet size (header + payload).
    pub fn total_size(&self) -> u64 {
        self.offset as u64 + self.length
    }

    /// Check if this is a valid morloc packet.
    pub fn is_valid(&self) -> bool {
        self.magic == PACKET_MAGIC
    }

    /// Get the command type.
    pub fn command_type(&self) -> u8 {
        unsafe { self.command.cmd_type.cmd_type }
    }

    pub fn is_ping(&self) -> bool {
        self.command_type() == PACKET_TYPE_PING
    }

    pub fn is_call(&self) -> bool {
        self.command_type() == PACKET_TYPE_CALL
    }

    pub fn is_data(&self) -> bool {
        self.command_type() == PACKET_TYPE_DATA
    }

    pub fn is_stream(&self) -> bool {
        self.command_type() == PACKET_TYPE_STREAM
    }

    pub fn is_footer(&self) -> bool {
        self.command_type() == PACKET_TYPE_FOOTER
    }

    pub fn is_local_call(&self) -> bool {
        self.is_call() && unsafe { self.command.call.entrypoint } == PACKET_ENTRYPOINT_LOCAL
    }

    pub fn is_remote_call(&self) -> bool {
        self.is_call() && unsafe { self.command.call.entrypoint } == PACKET_ENTRYPOINT_REMOTE_SFS
    }

    pub fn is_fail(&self) -> bool {
        self.is_data() && unsafe { self.command.data.status } == PACKET_STATUS_FAIL
    }

    /// Serialize the header to bytes.
    pub fn to_bytes(&self) -> [u8; 32] {
        unsafe { std::mem::transmute(*self) }
    }

    /// Deserialize a header from bytes.
    pub fn from_bytes(bytes: &[u8; 32]) -> Result<Self, MorlocError> {
        let header: PacketHeader = unsafe { std::mem::transmute(*bytes) };
        if !header.is_valid() {
            let magic = { header.magic };
            return Err(MorlocError::Packet(format!(
                "invalid magic: 0x{magic:08x}"
            )));
        }
        Ok(header)
    }
}

// ── Full-packet validity check ──────────────────────────────────────────────

/// Confirm that a byte slice is a structurally complete morloc packet:
/// a valid 32-byte header, with body length matching exactly
/// `header.offset + header.length`. Intended for use on bytes that
/// just came from disk (or any other untrusted source) before any
/// downstream code dereferences offsets into the buffer. Inter-pool
/// UDS-stream paths do NOT need to call this -- they ingest bytes
/// produced by morloc itself one process away, so the cost is not
/// worth paying on the hot path.
///
/// Returns the parsed header on success; on any structural problem
/// returns a `MorlocError::Packet` with a single-sentence
/// machine-and-human-readable summary that callers can surface
/// directly via `set_errmsg`.
pub fn validate_packet(bytes: &[u8]) -> Result<PacketHeader, MorlocError> {
    if bytes.len() < 32 {
        return Err(MorlocError::Packet(format!(
            "packet truncated: got {} bytes (header alone is 32)",
            bytes.len()
        )));
    }
    let mut hdr_buf = [0u8; 32];
    hdr_buf.copy_from_slice(&bytes[..32]);
    let hdr = PacketHeader::from_bytes(&hdr_buf)?;
    let offset = hdr.offset as usize;
    let length = hdr.length as usize;
    let expected = 32usize.checked_add(offset)
        .and_then(|n| n.checked_add(length))
        .ok_or_else(|| MorlocError::Packet(format!(
            "packet header reports offset + length overflow (offset={offset} length={length})"
        )))?;
    if bytes.len() != expected {
        return Err(MorlocError::Packet(format!(
            "packet size mismatch: header says {} bytes (32 header + {} metadata + {} payload), got {}",
            expected, offset, length, bytes.len()
        )));
    }
    Ok(hdr)
}

// ── Full packet construction (header + metadata + payload) ─────────────────

/// Build a complete data packet with schema metadata and relptr payload.
pub fn make_standard_data_packet(relptr: RelPtr, schema: &Schema) -> Vec<u8> {
    let schema_str = crate::schema::schema_to_string(schema);
    let schema_bytes = schema_str.as_bytes();
    let schema_len = schema_bytes.len() + 1; // +1 for null terminator

    // Metadata: header (8 bytes) + schema string (null-terminated), padded to 32-byte boundary
    let meta_header_size = std::mem::size_of::<MetadataHeader>();
    let raw_meta_len = meta_header_size + schema_len;
    let padded_meta_len = ((raw_meta_len + 31) / 32) * 32;

    // Payload: relptr
    let payload_len = std::mem::size_of::<RelPtr>();

    let total = 32 + padded_meta_len + payload_len;
    let mut packet = vec![0u8; total];

    // Write header
    let header = PacketHeader::data_rptr(PACKET_FORMAT_VOIDSTAR, payload_len as u64);
    let mut hdr = header;
    // Override offset to include metadata
    unsafe {
        let hdr_ptr = &mut hdr as *mut PacketHeader as *mut u8;
        // Set offset field (at byte 20 in packed struct)
        let offset_ptr = hdr_ptr.add(20) as *mut u32;
        *offset_ptr = padded_meta_len as u32;
    }
    let hdr_bytes = hdr.to_bytes();
    packet[..32].copy_from_slice(&hdr_bytes);

    // Write metadata header
    let meta_start = 32;
    packet[meta_start] = b'm';
    packet[meta_start + 1] = b'm';
    packet[meta_start + 2] = b'h';
    packet[meta_start + 3] = METADATA_TYPE_SCHEMA_STRING;
    let meta_size_bytes = (schema_len as u32).to_le_bytes();
    packet[meta_start + 4..meta_start + 8].copy_from_slice(&meta_size_bytes);

    // Write schema string (null-terminated)
    let schema_data_start = meta_start + meta_header_size;
    packet[schema_data_start..schema_data_start + schema_bytes.len()].copy_from_slice(schema_bytes);
    // Null terminator already there from vec![0u8]

    // Write relptr payload
    let payload_start = 32 + padded_meta_len;
    let relptr_bytes = relptr.to_ne_bytes();
    packet[payload_start..payload_start + relptr_bytes.len()].copy_from_slice(&relptr_bytes);

    packet
}

/// Build an inline MESG+MSGPACK data packet with schema metadata.
pub fn make_mesg_data_packet(mpk_data: &[u8], schema: &Schema) -> Vec<u8> {
    let schema_str = crate::schema::schema_to_string(schema);
    let schema_bytes = schema_str.as_bytes();
    let schema_len = schema_bytes.len() + 1; // +1 for null terminator

    let meta_header_size = std::mem::size_of::<MetadataHeader>();
    let raw_meta_len = meta_header_size + schema_len;
    let padded_meta_len = ((raw_meta_len + 31) / 32) * 32;

    let total = 32 + padded_meta_len + mpk_data.len();
    let mut packet = vec![0u8; total];

    // Write header
    let mut header = PacketHeader::data_mesg(PACKET_FORMAT_MSGPACK, mpk_data.len() as u64);
    // Set offset to metadata size
    unsafe {
        let hdr_ptr = &mut header as *mut PacketHeader as *mut u8;
        let offset_ptr = hdr_ptr.add(20) as *mut u32;
        *offset_ptr = padded_meta_len as u32;
    }
    let hdr_bytes = header.to_bytes();
    packet[..32].copy_from_slice(&hdr_bytes);

    // Write metadata header
    let meta_start = 32;
    packet[meta_start] = b'm';
    packet[meta_start + 1] = b'm';
    packet[meta_start + 2] = b'h';
    packet[meta_start + 3] = METADATA_TYPE_SCHEMA_STRING;
    let meta_size_bytes = (schema_len as u32).to_le_bytes();
    packet[meta_start + 4..meta_start + 8].copy_from_slice(&meta_size_bytes);

    // Write schema string
    let schema_data_start = meta_start + meta_header_size;
    packet[schema_data_start..schema_data_start + schema_bytes.len()].copy_from_slice(schema_bytes);

    // Write msgpack payload
    let payload_start = 32 + padded_meta_len;
    packet[payload_start..payload_start + mpk_data.len()].copy_from_slice(mpk_data);

    packet
}

/// Build a call packet from argument data packets.
pub fn make_local_call_packet(midx: u32, arg_packets: &[Vec<u8>]) -> Vec<u8> {
    let data_length: usize = arg_packets.iter().map(|p| p.len()).sum();
    let total = 32 + data_length;
    let mut packet = vec![0u8; total];

    // Write call header
    let header = PacketHeader::local_call(midx, data_length as u64);
    let hdr_bytes = header.to_bytes();
    packet[..32].copy_from_slice(&hdr_bytes);

    // Concatenate argument packets
    let mut pos = 32;
    for arg in arg_packets {
        packet[pos..pos + arg.len()].copy_from_slice(arg);
        pos += arg.len();
    }

    packet
}

/// Build a stream-file prefix: the 32-byte STREAM header plus a
/// metadata block carrying the element schema.
///
/// The result is *not* a complete packet — it is the opening bytes of
/// a stream file. A writer appends one or more full DATA sub-packets
/// after this prefix, then optionally a FOOTER packet plus the
/// `STREAM_TAIL_SIZE`-byte EOF tail.
///
/// The STREAM header's `length` field is `STREAM_LENGTH_SENTINEL`
/// (writers never touch the header again), and `offset` is the byte
/// size of the metadata block.
pub fn make_stream_header_block(schema: &Schema) -> Vec<u8> {
    let schema_str = crate::schema::schema_to_string(schema);
    let schema_bytes = schema_str.as_bytes();
    let schema_len = schema_bytes.len() + 1; // null terminator

    let meta_header_size = std::mem::size_of::<MetadataHeader>();
    let raw_meta_len = meta_header_size + schema_len;
    let padded_meta_len = raw_meta_len.div_ceil(METADATA_BLOCK_ALIGNMENT)
        * METADATA_BLOCK_ALIGNMENT;

    let total = 32 + padded_meta_len;
    let mut out = vec![0u8; total];

    // Write header
    let mut hdr = PacketHeader::stream();
    hdr.offset = padded_meta_len as u32;
    let hdr_bytes = hdr.to_bytes();
    out[..32].copy_from_slice(&hdr_bytes);

    // Write the schema metadata entry
    let meta_start = 32;
    out[meta_start..meta_start + 3].copy_from_slice(&METADATA_HEADER_MAGIC);
    out[meta_start + 3] = METADATA_TYPE_SCHEMA_STRING;
    out[meta_start + 4..meta_start + 8]
        .copy_from_slice(&(schema_len as u32).to_le_bytes());
    let schema_data_start = meta_start + meta_header_size;
    out[schema_data_start..schema_data_start + schema_bytes.len()]
        .copy_from_slice(schema_bytes);
    // Null terminator already there from the zero-initialised vec.

    out
}

/// Build a FOOTER packet from a sequence of tagged metadata blocks.
/// The payload is exactly `metadata_entries_end(...)`-style entries,
/// not padded (the FOOTER's `length` field gives the exact byte
/// count). Passing an empty `blocks` slice yields a header-only footer,
/// useful as a clean-close marker even when no cached indices exist.
pub fn make_footer_packet(blocks: &[(u8, &[u8])]) -> Vec<u8> {
    let payload_len: usize = blocks
        .iter()
        .map(|(_, body)| std::mem::size_of::<MetadataHeader>() + body.len())
        .sum();

    let total = 32 + payload_len;
    let mut out = vec![0u8; total];

    // The footer's metadata blocks sit directly after the 32-byte header.
    // `iter_packet_metadata` keys off `header.offset` to find the metadata
    // range (see iter_packet_metadata: meta_end = 32 + offset), so the
    // offset field must be set to the blocks' total byte length even
    // though footer packets have no separate post-metadata payload.
    let mut hdr = PacketHeader::footer(payload_len as u64);
    hdr.offset = payload_len as u32;
    let hdr_bytes = hdr.to_bytes();
    out[..32].copy_from_slice(&hdr_bytes);

    let mut cur = 32;
    for &(kind, body) in blocks {
        out[cur..cur + 3].copy_from_slice(&METADATA_HEADER_MAGIC);
        out[cur + 3] = kind;
        out[cur + 4..cur + 8]
            .copy_from_slice(&(body.len() as u32).to_le_bytes());
        cur += 8;
        out[cur..cur + body.len()].copy_from_slice(body);
        cur += body.len();
    }

    out
}

// ── Stream diagnostic block (StreamDiag) ──────────────────────────────────
//
// Fixed-size diagnostic block carried inside every stream footer (both
// temp and final). The temp footer's whole purpose is this block; it
// rewritten on every `@write` flush, so its size MUST stay constant.
//
// Field order is chosen so each u64 is on a natural 8-byte boundary
// without padding. Total payload = 160 bytes
// (16 B header lane + 8 B reserved + 72 B totals/timing + 64 B tail).

/// Current StreamDiag struct version. Bump when adding new trailing
/// fields. Older tooling reads only what it understands; newer fields
/// are appended at the end of the struct (via `_reserved` budget).
pub const STREAM_DIAG_VERSION: u32 = 1;

/// Capacity of the StreamDiag tail-window array. The diag's `tail_len`
/// field indicates how many entries are populated; entries beyond
/// `tail_len` are uninitialised and MUST NOT be consulted.
pub const STREAM_DIAG_TAIL_MAX: usize = 8;

/// Wire layout of the diagnostic block carried inside `METADATA_TYPE_
/// STREAM_DIAG`. Packed `#[repr(C)]` so every host-language tool can
/// read this directly from the on-disk bytes without going through a
/// parser.
///
/// Byte-counter semantics (locked in):
///   - `bytes_uncompressed_total` is the sum of each sub-packet's
///     uncompressed PAYLOAD size (not headers / metadata / footer).
///   - `bytes_compressed_total` is the sum of each sub-packet's on-disk
///     PAYLOAD size (not headers / metadata / footer).
///   - The ratio `bytes_uncompressed_total / bytes_compressed_total` is
///     therefore a clean payload-only compression ratio.
#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct StreamDiag {
    // ── Header lane: four u32s, two natural u64-aligned slots (16 B) ──
    pub diag_version:                u32,
    pub writer_pid:                  u32,
    pub n_oversize_subpackets:       u32,
    pub tail_len:                    u32,

    // ── Reserved (8 B) for future-extensible fields ───────────────────
    pub _reserved:                   u64,

    // ── Identification + running totals (80 B) ────────────────────────
    pub writer_start_time:           u64, // unix-epoch microseconds at @open
    pub subpacket_count:             u64,
    pub element_count:               u64,
    pub bytes_uncompressed_total:    u64,
    pub bytes_compressed_total:      u64,
    pub largest_packet_uncompressed: u64,
    pub largest_packet_idx:          u64,
    pub first_flush_time:            u64,
    pub last_flush_time:             u64,

    // ── Tail window (STREAM_DIAG_TAIL_MAX * 8 = 64 B) ─────────────────
    // tail[0..tail_len] holds the byte offsets of the most-recent
    // tail_len sub-packets in chronological order (oldest at index 0).
    // Slots beyond tail_len are uninitialised; readers MUST only
    // consult tail[0..tail_len].
    pub tail: [u64; STREAM_DIAG_TAIL_MAX],
}

const _: () = assert!(std::mem::size_of::<StreamDiag>() == 160);

impl StreamDiag {
    /// Construct a zero-initialised StreamDiag with `diag_version` set.
    /// Other fields are left zero; the caller populates them as it
    /// accumulates sub-packet flushes.
    pub fn new() -> Self {
        StreamDiag {
            diag_version: STREAM_DIAG_VERSION,
            writer_pid: 0,
            n_oversize_subpackets: 0,
            tail_len: 0,
            _reserved: 0,
            writer_start_time: 0,
            subpacket_count: 0,
            element_count: 0,
            bytes_uncompressed_total: 0,
            bytes_compressed_total: 0,
            largest_packet_uncompressed: 0,
            largest_packet_idx: 0,
            first_flush_time: 0,
            last_flush_time: 0,
            tail: [0; STREAM_DIAG_TAIL_MAX],
        }
    }

    /// View the in-memory struct as its raw on-disk bytes. Safe because
    /// `StreamDiag` is `#[repr(C, packed)]` with no relptrs and no
    /// padding (asserted at compile time).
    pub fn as_bytes(&self) -> [u8; 160] {
        let mut out = [0u8; 160];
        unsafe {
            std::ptr::copy_nonoverlapping(
                self as *const StreamDiag as *const u8,
                out.as_mut_ptr(),
                160,
            );
        }
        out
    }

    /// Reconstitute a StreamDiag from its on-disk bytes. Returns Err if
    /// `data` is shorter than `size_of::<StreamDiag>()`. Extra trailing
    /// bytes are ignored (future tooling may extend the struct; older
    /// readers must continue to parse the prefix they understand).
    pub fn from_bytes(data: &[u8]) -> Result<Self, MorlocError> {
        if data.len() < 160 {
            return Err(MorlocError::Packet(format!(
                "StreamDiag truncated: {} bytes < 160", data.len()
            )));
        }
        let mut buf = [0u8; 160];
        buf.copy_from_slice(&data[..160]);
        let diag: StreamDiag = unsafe {
            std::ptr::read_unaligned(buf.as_ptr() as *const StreamDiag)
        };
        Ok(diag)
    }
}

impl Default for StreamDiag {
    fn default() -> Self {
        Self::new()
    }
}

/// Total bytes a temp footer occupies on disk: footer packet header
/// (32 B) + one metadata block header (8 B) + StreamDiag payload
/// (160 B) + EOF tail (8 B). This number is exposed so OStream writers
/// can pwrite a fixed-size buffer of this exact length on every flush.
pub const STREAM_TEMP_FOOTER_BYTES: usize = 32 + 8 + 160 + STREAM_TAIL_SIZE;

/// Encode the bytes of a `temp` footer (no SUBPACKET_INDEX, no
/// FOOTER_FINAL). The returned vector is exactly `STREAM_TEMP_FOOTER_
/// BYTES` long: footer packet + EOF tail. Callers pwrite this in one
/// shot at the end of the data region and the new EOF tail offset
/// becomes `data_end + STREAM_TEMP_FOOTER_BYTES`.
pub fn make_temp_footer_packet(diag: &StreamDiag) -> Vec<u8> {
    let diag_bytes = diag.as_bytes();
    let footer_packet = make_footer_packet(&[
        (METADATA_TYPE_STREAM_DIAG, &diag_bytes),
    ]);
    debug_assert_eq!(footer_packet.len(), 32 + 8 + 160);
    let mut out = Vec::with_capacity(STREAM_TEMP_FOOTER_BYTES);
    out.extend_from_slice(&footer_packet);
    let tail = encode_stream_tail(footer_packet.len() as u32);
    out.extend_from_slice(&tail);
    debug_assert_eq!(out.len(), STREAM_TEMP_FOOTER_BYTES);
    out
}

/// Encode the bytes of a `final` footer (with full SUBPACKET_INDEX,
/// the StreamDiag block, and the FOOTER_FINAL flag). The returned
/// vector is `footer_packet + 8B tail`; size depends on the index
/// length but is always written once, at `@close`.
pub fn make_final_footer_packet(
    diag: &StreamDiag,
    subpacket_index: &[u64],
) -> Vec<u8> {
    // Build the subpacket-index body: u64 count followed by u64 offsets.
    let mut idx_body = Vec::with_capacity(8 + subpacket_index.len() * 8);
    idx_body.extend_from_slice(&(subpacket_index.len() as u64).to_le_bytes());
    for off in subpacket_index {
        idx_body.extend_from_slice(&off.to_le_bytes());
    }

    let diag_bytes = diag.as_bytes();
    let footer_packet = make_footer_packet(&[
        (METADATA_TYPE_STREAM_DIAG, &diag_bytes),
        (METADATA_TYPE_SUBPACKET_INDEX, &idx_body),
        (METADATA_TYPE_FOOTER_FINAL, &[]),
    ]);
    let mut out = Vec::with_capacity(footer_packet.len() + STREAM_TAIL_SIZE);
    out.extend_from_slice(&footer_packet);
    let tail = encode_stream_tail(footer_packet.len() as u32);
    out.extend_from_slice(&tail);
    out
}

/// Parse the SUBPACKET_INDEX entry payload (without the 8-byte mmh-
/// type-size header). Body is `u64 count` followed by `count * u64`.
pub fn decode_subpacket_index(data: &[u8]) -> Result<Vec<u64>, MorlocError> {
    if data.len() < 8 {
        return Err(MorlocError::Packet(format!(
            "SUBPACKET_INDEX entry truncated: {} bytes", data.len()
        )));
    }
    let count = u64::from_le_bytes(data[..8].try_into().unwrap()) as usize;
    let needed = 8 + count * 8;
    if data.len() < needed {
        return Err(MorlocError::Packet(format!(
            "SUBPACKET_INDEX claims {} entries but payload has {} bytes",
            count, data.len()
        )));
    }
    let mut out = Vec::with_capacity(count);
    let mut cur = 8;
    for _ in 0..count {
        out.push(u64::from_le_bytes(data[cur..cur + 8].try_into().unwrap()));
        cur += 8;
    }
    Ok(out)
}

/// Build a fail packet with an error message string.
pub fn make_fail_packet_bytes(error_msg: &str) -> Vec<u8> {
    let msg_bytes = error_msg.as_bytes();
    let total = 32 + msg_bytes.len();
    let mut packet = vec![0u8; total];

    let header = PacketHeader::fail(msg_bytes.len() as u64);
    let hdr_bytes = header.to_bytes();
    packet[..32].copy_from_slice(&hdr_bytes);
    packet[32..].copy_from_slice(msg_bytes);

    packet
}

/// Extract the payload from a data packet (bytes after header + metadata offset).
pub fn get_data_payload(packet: &[u8]) -> Result<&[u8], MorlocError> {
    if packet.len() < 32 {
        return Err(MorlocError::Packet("packet too small".into()));
    }
    let header = PacketHeader::from_bytes(packet[..32].try_into().unwrap())?;
    let offset = { header.offset } as usize;
    let length = { header.length } as usize;
    let start = 32 + offset;
    let end = start + length;
    if end > packet.len() {
        return Err(MorlocError::Packet("payload extends past packet end".into()));
    }
    Ok(&packet[start..end])
}

/// Extract error message from a fail packet.
pub fn get_error_message(packet: &[u8]) -> Result<Option<String>, MorlocError> {
    if packet.len() < 32 {
        return Err(MorlocError::Packet("packet too small".into()));
    }
    let header = PacketHeader::from_bytes(packet[..32].try_into().unwrap())?;
    if !header.is_fail() {
        return Ok(None);
    }
    let payload = get_data_payload(packet)?;
    Ok(Some(String::from_utf8_lossy(payload).into_owned()))
}

/// Iterator over the entries of a metadata block. Each entry is
/// `(metadata_type, data)` — the type byte from the entry header and
/// a borrow of the entry's payload bytes. Iteration stops on the
/// first non-`mmh` magic or on size overflow past `meta`'s end.
///
/// The input is the metadata block ALONE (not a full packet); for a
/// full packet the caller slices `&packet[32..32 + header.offset]`.
pub struct MetadataIter<'a> {
    meta: &'a [u8],
    pos: usize,
}

impl<'a> Iterator for MetadataIter<'a> {
    type Item = (u8, &'a [u8]);

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos + 8 > self.meta.len() {
            return None;
        }
        if self.meta[self.pos..self.pos + 3] != METADATA_HEADER_MAGIC {
            return None;
        }
        let meta_type = self.meta[self.pos + 3];
        let meta_size = u32::from_le_bytes([
            self.meta[self.pos + 4],
            self.meta[self.pos + 5],
            self.meta[self.pos + 6],
            self.meta[self.pos + 7],
        ]) as usize;
        let data_start = self.pos + 8;
        let data_end = data_start.checked_add(meta_size)?;
        if data_end > self.meta.len() {
            return None;
        }
        self.pos = data_end;
        Some((meta_type, &self.meta[data_start..data_end]))
    }
}

/// Iterate the entries of a standalone metadata block (NOT a full
/// packet). See [`iter_packet_metadata`] for the full-packet form.
pub fn iter_metadata(meta: &[u8]) -> MetadataIter<'_> {
    MetadataIter { meta, pos: 0 }
}

/// Return the byte length of the valid entries at the start of `meta`,
/// i.e. the offset of the first byte that is no longer a `mmh`-magic
/// metadata entry header. Padding bytes after the last valid entry
/// (e.g. the zero-pad that `make_mesg_data_packet` inserts to round
/// the block up to 32 bytes) lie past the returned offset.
///
/// Used by encoders that need to splice an additional entry into an
/// existing metadata block: the new entry must sit immediately after
/// the last valid entry, since the metadata iterator stops at the
/// first non-`mmh` byte and would not otherwise see the splice.
pub fn metadata_entries_end(meta: &[u8]) -> usize {
    let mut cur = 0usize;
    while cur + 8 <= meta.len() {
        if meta[cur..cur + 3] != METADATA_HEADER_MAGIC {
            break;
        }
        let size = u32::from_le_bytes([
            meta[cur + 4],
            meta[cur + 5],
            meta[cur + 6],
            meta[cur + 7],
        ]) as usize;
        let next = match cur.checked_add(8).and_then(|v| v.checked_add(size)) {
            Some(n) => n,
            None => break,
        };
        if next > meta.len() {
            break;
        }
        cur = next;
    }
    cur
}

/// Width to which metadata blocks are padded so the payload starts on a
/// well-known alignment boundary. Matches `make_mesg_data_packet`'s
/// 32-byte rounding.
pub const METADATA_BLOCK_ALIGNMENT: usize = 32;

/// Build a new metadata block by appending a single entry `(kind, body)`
/// to the valid entries at the start of `existing`. Any trailing
/// padding in `existing` (zero bytes past `metadata_entries_end`) is
/// discarded; the result is padded back up to `METADATA_BLOCK_ALIGNMENT`
/// so the payload retains its alignment.
///
/// Encoders that need to insert a `METADATA_TYPE_FRAME_INDEX` entry into
/// the metadata block of an existing packet (`compress_packet`,
/// `normalize_data_packet_for_output`, etc.) use this helper instead of
/// hand-stitching the entry header + body + padding.
pub fn append_metadata_entry(existing: &[u8], kind: u8, body: &[u8]) -> Vec<u8> {
    let entries_end = metadata_entries_end(existing);
    let mut out = Vec::with_capacity(
        entries_end + 8 + body.len() + METADATA_BLOCK_ALIGNMENT,
    );
    out.extend_from_slice(&existing[..entries_end]);
    out.extend_from_slice(&METADATA_HEADER_MAGIC);
    out.push(kind);
    out.extend_from_slice(&(body.len() as u32).to_le_bytes());
    out.extend_from_slice(body);
    let padded = out.len().div_ceil(METADATA_BLOCK_ALIGNMENT) * METADATA_BLOCK_ALIGNMENT;
    out.resize(padded, 0);
    out
}

/// Build a new metadata block by concatenating the valid entries at the
/// start of `existing` with `prepended` entries that should appear
/// before them (e.g. a freshly-computed `METADATA_TYPE_VOL_INDEX` entry
/// the streaming encoder writes before the passed-through metadata).
/// The result is padded to `METADATA_BLOCK_ALIGNMENT`.
pub fn prepend_metadata_entries(prepended: &[u8], existing: &[u8]) -> Vec<u8> {
    let entries_end = metadata_entries_end(existing);
    let mut out =
        Vec::with_capacity(prepended.len() + entries_end + METADATA_BLOCK_ALIGNMENT);
    out.extend_from_slice(prepended);
    out.extend_from_slice(&existing[..entries_end]);
    let padded = out.len().div_ceil(METADATA_BLOCK_ALIGNMENT) * METADATA_BLOCK_ALIGNMENT;
    out.resize(padded, 0);
    out
}

/// Iterate the entries of a full packet's metadata section. Errors
/// on a too-small or invalid-header packet; returns an empty iterator
/// when the packet declares no metadata.
pub fn iter_packet_metadata(packet: &[u8]) -> Result<MetadataIter<'_>, MorlocError> {
    if packet.len() < 32 {
        return Err(MorlocError::Packet("packet too small".into()));
    }
    let header = PacketHeader::from_bytes(packet[..32].try_into().unwrap())?;
    let offset = { header.offset } as usize;
    let meta_end = 32usize
        .checked_add(offset)
        .filter(|&e| e <= packet.len())
        .ok_or_else(|| MorlocError::Packet("metadata extends past packet end".into()))?;
    Ok(iter_metadata(&packet[32..meta_end]))
}

/// Decode a null-terminated UTF-8 (lossy) string from the payload of
/// a `METADATA_TYPE_SCHEMA_STRING` entry. Re-used by every metadata
/// scanner that needs the schema text.
pub fn decode_schema_entry(data: &[u8]) -> String {
    let len = data.iter().position(|&b| b == 0).unwrap_or(data.len());
    String::from_utf8_lossy(&data[..len]).into_owned()
}

/// Read the Layer 3 vol_idx hint from packet metadata, if present.
pub fn read_vol_index_from_meta(packet: &[u8]) -> Result<Option<u16>, MorlocError> {
    for (kind, data) in iter_packet_metadata(packet)? {
        if kind == METADATA_TYPE_VOL_INDEX && data.len() >= 2 {
            return Ok(Some(u16::from_le_bytes([data[0], data[1]])));
        }
    }
    Ok(None)
}

/// Read the schema string from packet metadata section.
pub fn read_schema_from_meta(packet: &[u8]) -> Result<Option<String>, MorlocError> {
    for (kind, data) in iter_packet_metadata(packet)? {
        if kind == METADATA_TYPE_SCHEMA_STRING {
            return Ok(Some(decode_schema_entry(data)));
        }
    }
    Ok(None)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_header_size() {
        assert_eq!(std::mem::size_of::<PacketHeader>(), 32);
    }

    #[test]
    fn test_ping_roundtrip() {
        let ping = PacketHeader::ping();
        assert!(ping.is_valid());
        assert!(ping.is_ping());
        let bytes = ping.to_bytes();
        let recovered = PacketHeader::from_bytes(&bytes).unwrap();
        assert!(recovered.is_ping());
    }

    #[test]
    fn test_call_packet() {
        let call = PacketHeader::local_call(42, 128);
        assert!(call.is_call());
        assert!(call.is_local_call());
        assert!(!call.is_remote_call());
        let bytes = call.to_bytes();
        let recovered = PacketHeader::from_bytes(&bytes).unwrap();
        assert!(recovered.is_local_call());
        let midx = unsafe { recovered.command.call.midx };
        assert_eq!(midx, 42);
        let len = { recovered.length };
        assert_eq!(len, 128);
    }

    #[test]
    fn test_fail_packet() {
        let fail = PacketHeader::fail(100);
        assert!(fail.is_data());
        assert!(fail.is_fail());
    }

    #[test]
    fn test_data_mesg() {
        let data = PacketHeader::data_mesg(PACKET_FORMAT_MSGPACK, 256);
        assert!(data.is_data());
        assert!(!data.is_fail());
        let fmt = unsafe { data.command.data.format };
        assert_eq!(fmt, PACKET_FORMAT_MSGPACK);
        let len = { data.length };
        assert_eq!(len, 256);
    }

    #[test]
    fn frame_index_entry_roundtrip() {
        let frames = vec![
            FrameEntry { uncompressed_size: 16 << 20, compressed_size: 6 << 20 },
            FrameEntry { uncompressed_size: 16 << 20, compressed_size: 5_500_000 },
            FrameEntry { uncompressed_size: 4_532_768, compressed_size: 1_234_567 },
        ];
        let encoded = encode_frame_index_entry(&frames);
        assert_eq!(
            encoded.len(),
            FRAME_INDEX_HEADER_BYTES + frames.len() * FRAME_ENTRY_BYTES
        );
        let decoded = decode_frame_index_entry(&encoded).unwrap();
        assert_eq!(decoded, frames);
    }

    #[test]
    fn frame_index_entry_empty_roundtrip() {
        // Zero-frame index is legal (an encoder reserves space for the
        // worst case and may emit zero frames if the input was empty).
        let encoded = encode_frame_index_entry(&[]);
        assert_eq!(encoded.len(), FRAME_INDEX_HEADER_BYTES);
        let decoded = decode_frame_index_entry(&encoded).unwrap();
        assert!(decoded.is_empty());
    }

    #[test]
    fn frame_index_entry_tolerates_trailing_padding() {
        // Streaming encoders reserve max-N slots and may patch in
        // fewer than max-N actual frames. Trailing zero padding past
        // `frame_count * FRAME_ENTRY_BYTES` must be ignored by the
        // decoder, which keys off the frame_count field.
        let frames = vec![FrameEntry {
            uncompressed_size: 16 << 20,
            compressed_size: 5 << 20,
        }];
        let mut encoded = encode_frame_index_entry(&frames);
        encoded.extend_from_slice(&[0u8; FRAME_ENTRY_BYTES * 5]); // five empty slots
        let decoded = decode_frame_index_entry(&encoded).unwrap();
        assert_eq!(decoded, frames);
    }

    #[test]
    fn frame_index_entry_rejects_truncated_header() {
        let bad = [0u8; 3];
        assert!(decode_frame_index_entry(&bad).is_err());
    }

    #[test]
    fn frame_index_entry_rejects_count_past_data() {
        // frame_count = 4 but the entry only carries 1 slot.
        let mut bad = 4u32.to_le_bytes().to_vec();
        bad.extend_from_slice(&[0u8; FRAME_ENTRY_BYTES]);
        let err = decode_frame_index_entry(&bad).unwrap_err();
        assert!(matches!(err, MorlocError::Packet(_)));
    }

    #[test]
    fn read_frame_index_from_meta_returns_none_when_absent() {
        // Build a minimal MESG+VOIDSTAR packet with only the schema
        // metadata entry. `read_frame_index_from_meta` should report
        // no frame index without erroring.
        let payload = vec![0u8; 32];
        let schema_str = "u4"; // any short schema is fine
        let packet = make_mesg_data_packet(&payload, schema_str);
        let got = read_frame_index_from_meta(&packet).unwrap();
        assert!(got.is_none());
    }

    #[test]
    fn stream_header_roundtrip() {
        let stream = PacketHeader::stream();
        assert!(stream.is_valid());
        assert!(stream.is_stream());
        assert!(!stream.is_data());
        assert!(!stream.is_call());
        assert!(!stream.is_ping());
        assert!(!stream.is_footer());
        let len = { stream.length };
        assert_eq!(len, STREAM_LENGTH_SENTINEL);
        let bytes = stream.to_bytes();
        let recovered = PacketHeader::from_bytes(&bytes).unwrap();
        assert!(recovered.is_stream());
        let recovered_len = { recovered.length };
        assert_eq!(recovered_len, STREAM_LENGTH_SENTINEL);
    }

    #[test]
    fn footer_header_roundtrip() {
        let footer = PacketHeader::footer(1234);
        assert!(footer.is_valid());
        assert!(footer.is_footer());
        assert!(!footer.is_stream());
        assert!(!footer.is_data());
        let len = { footer.length };
        assert_eq!(len, 1234);
        let bytes = footer.to_bytes();
        let recovered = PacketHeader::from_bytes(&bytes).unwrap();
        assert!(recovered.is_footer());
        let recovered_len = { recovered.length };
        assert_eq!(recovered_len, 1234);
    }

    #[test]
    fn stream_tail_roundtrip() {
        for footer_len in [0u32, 1, 256, 1 << 20, u32::MAX] {
            let tail = encode_stream_tail(footer_len);
            assert_eq!(tail.len(), STREAM_TAIL_SIZE);
            assert_eq!(&tail[4..], &STREAM_TAIL_MAGIC);
            let decoded = decode_stream_tail(&tail).unwrap();
            assert_eq!(decoded, footer_len);
        }
    }

    #[test]
    fn stream_tail_magic_is_reversed_packet_magic() {
        // PACKET_MAGIC bytes in LE: [0x6d, 0xf8, 0x07, 0x07].
        // Tail magic must be those bytes reversed: [0x07, 0x07, 0xf8, 0x6d].
        // This guarantees the tail magic cannot start a packet header.
        let packet_magic_le = PACKET_MAGIC.to_le_bytes();
        let mut reversed = packet_magic_le;
        reversed.reverse();
        assert_eq!(STREAM_TAIL_MAGIC, reversed);
    }

    #[test]
    fn stream_tail_rejects_wrong_magic() {
        let mut tail = encode_stream_tail(42);
        tail[4] ^= 0xFF;
        assert!(decode_stream_tail(&tail).is_none());
    }

    #[test]
    fn stream_tail_rejects_zero_bytes() {
        let tail = [0u8; STREAM_TAIL_SIZE];
        assert!(decode_stream_tail(&tail).is_none());
    }

    #[test]
    fn packet_type_names_cover_new_types() {
        assert_eq!(packet_type_name(PACKET_TYPE_STREAM), "stream");
        assert_eq!(packet_type_name(PACKET_TYPE_FOOTER), "footer");
    }

    // ── End-to-end stream-file layout round-trips ──────────────────────

    /// Helper: build N "data" sub-packets carrying `i: u32` for i in 0..n.
    /// Uses the test-local `make_mesg_data_packet` (string schema).
    fn build_subpackets(schema_str: &str, n: u32) -> Vec<Vec<u8>> {
        (0..n)
            .map(|i| {
                let bytes = i.to_le_bytes();
                make_mesg_data_packet(&bytes, schema_str)
            })
            .collect()
    }

    /// Helper: build a complete stream file = header + sub-packets + footer + tail.
    fn build_stream_file(schema: &Schema, schema_str: &str, n: u32) -> Vec<u8> {
        let mut file = make_stream_header_block(schema);
        for sp in build_subpackets(schema_str, n) {
            file.extend_from_slice(&sp);
        }
        let footer = make_footer_packet(&[]);
        file.extend_from_slice(&footer);
        let tail = encode_stream_tail(footer.len() as u32);
        file.extend_from_slice(&tail);
        file
    }

    /// Walk forward through a stream file's body, returning the byte
    /// ranges of each contained sub-packet. Stops at EOF or at the
    /// first non-DATA packet (which is the footer, if present).
    fn enumerate_subpackets(file: &[u8]) -> Vec<(usize, usize)> {
        let header = PacketHeader::from_bytes(file[..32].try_into().unwrap()).unwrap();
        assert!(header.is_stream());
        let mut cur = 32 + header.offset as usize;
        let mut out = Vec::new();
        while cur + 32 <= file.len() {
            let sub_hdr = PacketHeader::from_bytes(
                file[cur..cur + 32].try_into().unwrap(),
            )
            .unwrap();
            if !sub_hdr.is_data() {
                break; // hit footer or unknown packet
            }
            let total = 32 + sub_hdr.offset as usize + sub_hdr.length as usize;
            out.push((cur, cur + total));
            cur += total;
        }
        out
    }

    #[test]
    fn stream_file_roundtrip_with_footer() {
        let schema = crate::schema::parse_schema("u4").unwrap();
        let file = build_stream_file(&schema, "u4", 5);

        // 1. EOF tail says footer is present.
        let tail_off = file.len() - STREAM_TAIL_SIZE;
        let tail: &[u8; STREAM_TAIL_SIZE] = file[tail_off..].try_into().unwrap();
        let footer_len = decode_stream_tail(tail).expect("footer should be present");

        // 2. Footer packet sits at `file_size - tail - footer_len`.
        let footer_off = tail_off - footer_len as usize;
        let footer_hdr = PacketHeader::from_bytes(
            file[footer_off..footer_off + 32].try_into().unwrap(),
        )
        .unwrap();
        assert!(footer_hdr.is_footer());

        // 3. Stream header is at file[0..]. Schema metadata recovers.
        let stream_hdr = PacketHeader::from_bytes(file[..32].try_into().unwrap()).unwrap();
        assert!(stream_hdr.is_stream());
        let stream_hdr_len = { stream_hdr.length };
        assert_eq!(stream_hdr_len, STREAM_LENGTH_SENTINEL);
        let recovered_schema = read_schema_from_meta(&file[..32 + stream_hdr.offset as usize])
            .unwrap()
            .unwrap();
        assert_eq!(recovered_schema, crate::schema::schema_to_string(&schema));

        // 4. Sub-packets enumerable; payloads contain 0..5 in u32 LE.
        let ranges = enumerate_subpackets(&file);
        assert_eq!(ranges.len(), 5);
        for (i, &(start, end)) in ranges.iter().enumerate() {
            let pkt = &file[start..end];
            let payload = get_data_payload(pkt).unwrap();
            assert_eq!(payload.len(), 4);
            let val = u32::from_le_bytes(payload.try_into().unwrap());
            assert_eq!(val as usize, i);
        }
    }

    #[test]
    fn stream_file_roundtrip_without_footer() {
        // Simulate a writer that crashed (or in-progress stream): all
        // sub-packets written, but no footer and no EOF tail.
        let schema = crate::schema::parse_schema("u4").unwrap();
        let mut file = make_stream_header_block(&schema);
        for sp in build_subpackets("u4", 3) {
            file.extend_from_slice(&sp);
        }

        // EOF tail check: last 16 bytes should NOT match the magic.
        let tail_off = file.len() - STREAM_TAIL_SIZE;
        let tail: &[u8; STREAM_TAIL_SIZE] = file[tail_off..].try_into().unwrap();
        assert!(decode_stream_tail(tail).is_none());

        // Forward-scan still recovers all 3 sub-packets.
        let ranges = enumerate_subpackets(&file);
        assert_eq!(ranges.len(), 3);
    }

    #[test]
    fn stream_concat_invariant() {
        // Two footer-less streams of matching schema: their bodies
        // (sub-packets) can be concatenated under one of the headers
        // into a single valid stream containing all sub-packets in
        // order.
        let schema = crate::schema::parse_schema("u4").unwrap();

        // Stream A: 3 sub-packets carrying 0, 1, 2.
        let mut a = make_stream_header_block(&schema);
        for sp in build_subpackets("u4", 3) {
            a.extend_from_slice(&sp);
        }

        // Stream B: 2 sub-packets, renumber to 10, 11.
        let mut b = make_stream_header_block(&schema);
        for v in &[10u32, 11] {
            let pkt = make_mesg_data_packet(&v.to_le_bytes(), "u4");
            b.extend_from_slice(&pkt);
        }

        // Concat: keep A's header+metadata, append B's body only.
        let a_header_len = 32 + PacketHeader::from_bytes(a[..32].try_into().unwrap())
            .unwrap()
            .offset as usize;
        let b_header_len = 32 + PacketHeader::from_bytes(b[..32].try_into().unwrap())
            .unwrap()
            .offset as usize;
        let mut merged = a.clone();
        merged.extend_from_slice(&b[b_header_len..]);

        // Sanity: the merged file's header is unchanged.
        assert_eq!(&merged[..a_header_len], &a[..a_header_len]);

        // Forward-scan recovers all 5 sub-packets in order.
        let ranges = enumerate_subpackets(&merged);
        assert_eq!(ranges.len(), 5);
        let expected = [0u32, 1, 2, 10, 11];
        for (i, &(start, end)) in ranges.iter().enumerate() {
            let payload = get_data_payload(&merged[start..end]).unwrap();
            let val = u32::from_le_bytes(payload.try_into().unwrap());
            assert_eq!(val, expected[i]);
        }
    }

    #[test]
    fn footer_packet_carries_tagged_blocks() {
        // The footer is meant to be an extensible tagged-block payload.
        // Verify we can encode N blocks and decode them via the existing
        // metadata iterator.
        let block_a: &[u8] = b"hello";
        let block_b: &[u8] = &[1, 2, 3, 4, 5, 6, 7, 8];
        let footer = make_footer_packet(&[
            (0x10, block_a),
            (0x11, block_b),
        ]);
        let hdr = PacketHeader::from_bytes(footer[..32].try_into().unwrap()).unwrap();
        assert!(hdr.is_footer());

        // Footer metadata blocks sit between offset 32 and 32+offset
        // -- the standard `iter_packet_metadata` layout. Footers have
        // no separate post-metadata payload, so `header.offset ==
        // header.length`. Both fields point at the same byte boundary.
        let off = { hdr.offset } as usize;
        let len = { hdr.length } as usize;
        assert_eq!(off, len);
        let payload = &footer[32..32 + off];

        let entries: Vec<(u8, &[u8])> = iter_metadata(payload).collect();
        assert_eq!(entries.len(), 2);
        assert_eq!(entries[0].0, 0x10);
        assert_eq!(entries[0].1, block_a);
        assert_eq!(entries[1].0, 0x11);
        assert_eq!(entries[1].1, block_b);
    }

    #[test]
    fn stream_type_distinguishable_from_data() {
        // A reader inspecting the first byte of the command field must
        // be able to tell a STREAM file apart from a DATA file without
        // any additional context. Verify the discriminants do not collide.
        let stream = PacketHeader::stream();
        let data = PacketHeader::data_mesg(PACKET_FORMAT_MSGPACK, 0);
        assert_ne!(stream.command_type(), data.command_type());
    }

    #[test]
    fn read_frame_index_from_meta_returns_entries_when_present() {
        // Construct a packet by hand with both a schema and a
        // frame-index metadata entry, then verify the reader returns
        // the frames in order.
        let schema_bytes = b"au1\0";
        let schema_entry_size = schema_bytes.len();
        let frames = vec![
            FrameEntry { uncompressed_size: 100, compressed_size: 40 },
            FrameEntry { uncompressed_size: 200, compressed_size: 80 },
        ];
        let index_entry = encode_frame_index_entry(&frames);
        let meta_bytes = {
            let mut m = Vec::new();
            // schema entry
            m.extend_from_slice(&METADATA_HEADER_MAGIC);
            m.push(METADATA_TYPE_SCHEMA_STRING);
            m.extend_from_slice(&(schema_entry_size as u32).to_le_bytes());
            m.extend_from_slice(schema_bytes);
            // frame index entry
            m.extend_from_slice(&METADATA_HEADER_MAGIC);
            m.push(METADATA_TYPE_FRAME_INDEX);
            m.extend_from_slice(&(index_entry.len() as u32).to_le_bytes());
            m.extend_from_slice(&index_entry);
            m
        };
        // Wrap in a packet header. We only need the offset field to
        // point past the metadata; iteration stops when meta is
        // exhausted.
        let mut hdr = PacketHeader::data_mesg(PACKET_FORMAT_VOIDSTAR, 0);
        hdr.offset = meta_bytes.len() as u32;
        let mut packet = hdr.to_bytes().to_vec();
        packet.extend_from_slice(&meta_bytes);
        let got = read_frame_index_from_meta(&packet).unwrap().unwrap();
        assert_eq!(got, frames);
    }

    /// Helper: build a minimal MESG+VOIDSTAR packet with a single
    /// schema-string metadata entry wrapping `payload`.
    fn make_mesg_data_packet(payload: &[u8], schema: &str) -> Vec<u8> {
        let mut schema_bytes = schema.as_bytes().to_vec();
        schema_bytes.push(0); // null terminator
        let mut meta = Vec::new();
        meta.extend_from_slice(&METADATA_HEADER_MAGIC);
        meta.push(METADATA_TYPE_SCHEMA_STRING);
        meta.extend_from_slice(&(schema_bytes.len() as u32).to_le_bytes());
        meta.extend_from_slice(&schema_bytes);
        let mut hdr = PacketHeader::data_mesg(PACKET_FORMAT_VOIDSTAR, payload.len() as u64);
        hdr.offset = meta.len() as u32;
        let mut packet = hdr.to_bytes().to_vec();
        packet.extend_from_slice(&meta);
        packet.extend_from_slice(payload);
        packet
    }

    #[test]
    fn stream_diag_size_is_160() {
        // Locks in the wire format: any future field addition must
        // bump STREAM_DIAG_VERSION and either fit in the _reserved
        // u64 or extend the struct deliberately.
        assert_eq!(std::mem::size_of::<StreamDiag>(), 160);
    }

    #[test]
    fn stream_diag_roundtrip() {
        let mut diag = StreamDiag::new();
        diag.writer_pid = 12345;
        diag.subpacket_count = 42;
        diag.element_count = 12_345_678;
        diag.bytes_uncompressed_total = 16 << 20;
        diag.bytes_compressed_total = 5 << 20;
        diag.largest_packet_uncompressed = 100 << 20;
        diag.largest_packet_idx = 7;
        diag.first_flush_time = 1_700_000_000_000_000;
        diag.last_flush_time = 1_700_000_001_000_000;
        diag.tail_len = 3;
        diag.tail[0] = 64;
        diag.tail[1] = 16 << 20;
        diag.tail[2] = 32 << 20;

        let bytes = diag.as_bytes();
        let recovered = StreamDiag::from_bytes(&bytes).unwrap();

        assert_eq!({ recovered.diag_version }, STREAM_DIAG_VERSION);
        assert_eq!({ recovered.writer_pid }, 12345);
        assert_eq!({ recovered.subpacket_count }, 42);
        assert_eq!({ recovered.element_count }, 12_345_678);
        assert_eq!({ recovered.bytes_uncompressed_total }, 16 << 20);
        assert_eq!({ recovered.bytes_compressed_total }, 5 << 20);
        assert_eq!({ recovered.largest_packet_uncompressed }, 100 << 20);
        assert_eq!({ recovered.largest_packet_idx }, 7);
        assert_eq!({ recovered.tail_len }, 3);
        assert_eq!({ recovered.tail[0] }, 64);
        assert_eq!({ recovered.tail[1] }, 16 << 20);
        assert_eq!({ recovered.tail[2] }, 32 << 20);
    }

    #[test]
    fn stream_diag_from_bytes_rejects_truncated() {
        let short = [0u8; 100];
        assert!(StreamDiag::from_bytes(&short).is_err());
    }

    #[test]
    fn stream_diag_from_bytes_tolerates_trailing_bytes() {
        // Forward-compat: a future writer extends the struct; older
        // readers should still parse the prefix they know.
        let diag = StreamDiag::new();
        let mut padded = diag.as_bytes().to_vec();
        padded.extend_from_slice(&[0xAB; 32]);
        let recovered = StreamDiag::from_bytes(&padded).unwrap();
        assert_eq!({ recovered.diag_version }, STREAM_DIAG_VERSION);
    }

    #[test]
    fn temp_footer_packet_has_fixed_size() {
        // The headline performance commitment for OStream: temp footer
        // rewrite is a fixed-size pwrite regardless of file size.
        let mut diag = StreamDiag::new();
        diag.writer_pid = 1;
        diag.subpacket_count = 1_000_000;
        let bytes = make_temp_footer_packet(&diag);
        assert_eq!(bytes.len(), STREAM_TEMP_FOOTER_BYTES);

        let mut bigger_diag = StreamDiag::new();
        bigger_diag.subpacket_count = u64::MAX;
        let bigger_bytes = make_temp_footer_packet(&bigger_diag);
        assert_eq!(bigger_bytes.len(), STREAM_TEMP_FOOTER_BYTES);
    }

    #[test]
    fn temp_footer_packet_is_parseable() {
        let mut diag = StreamDiag::new();
        diag.subpacket_count = 99;
        diag.tail_len = 1;
        diag.tail[0] = 256;
        let bytes = make_temp_footer_packet(&diag);

        // Parse the footer packet header (skip the 8-byte EOF tail).
        let footer_packet_len = bytes.len() - STREAM_TAIL_SIZE;
        let footer = &bytes[..footer_packet_len];
        let hdr = PacketHeader::from_bytes(footer[..32].try_into().unwrap()).unwrap();
        assert!(hdr.is_footer());

        // Iterate metadata. Should find STREAM_DIAG only (no
        // FOOTER_FINAL, no SUBPACKET_INDEX in temp footers).
        let meta = iter_packet_metadata(footer).unwrap();
        let mut saw_diag = false;
        let mut saw_final = false;
        let mut saw_index = false;
        for (kind, body) in meta {
            match kind {
                METADATA_TYPE_STREAM_DIAG => {
                    saw_diag = true;
                    let parsed = StreamDiag::from_bytes(body).unwrap();
                    assert_eq!({ parsed.subpacket_count }, 99);
                    assert_eq!({ parsed.tail_len }, 1);
                    assert_eq!({ parsed.tail[0] }, 256);
                }
                METADATA_TYPE_FOOTER_FINAL => saw_final = true,
                METADATA_TYPE_SUBPACKET_INDEX => saw_index = true,
                _ => {}
            }
        }
        assert!(saw_diag, "temp footer must carry StreamDiag");
        assert!(!saw_final, "temp footer must NOT carry FOOTER_FINAL");
        assert!(!saw_index, "temp footer must NOT carry SUBPACKET_INDEX");

        // EOF tail decodes to the footer-packet length.
        let tail: [u8; STREAM_TAIL_SIZE] =
            bytes[footer_packet_len..].try_into().unwrap();
        let recovered_len = decode_stream_tail(&tail).unwrap();
        assert_eq!(recovered_len as usize, footer_packet_len);
    }

    #[test]
    fn final_footer_packet_carries_all_three_blocks() {
        let diag = StreamDiag::new();
        let index: Vec<u64> = (0..5).map(|i| (i as u64) * (16 << 20)).collect();
        let bytes = make_final_footer_packet(&diag, &index);

        let footer_packet_len = bytes.len() - STREAM_TAIL_SIZE;
        let footer = &bytes[..footer_packet_len];
        let hdr = PacketHeader::from_bytes(footer[..32].try_into().unwrap()).unwrap();
        assert!(hdr.is_footer());

        let mut saw_diag = false;
        let mut saw_final = false;
        let mut recovered_index: Option<Vec<u64>> = None;
        for (kind, body) in iter_packet_metadata(footer).unwrap() {
            match kind {
                METADATA_TYPE_STREAM_DIAG => saw_diag = true,
                METADATA_TYPE_FOOTER_FINAL => {
                    saw_final = true;
                    assert_eq!(body.len(), 0);
                }
                METADATA_TYPE_SUBPACKET_INDEX => {
                    recovered_index = Some(decode_subpacket_index(body).unwrap());
                }
                _ => {}
            }
        }
        assert!(saw_diag);
        assert!(saw_final, "final footer must carry FOOTER_FINAL");
        assert_eq!(recovered_index.as_deref(), Some(index.as_slice()));
    }

    #[test]
    fn subpacket_index_decode_rejects_truncated() {
        // count claims 4 entries but only 2 offsets provided.
        let mut bad = 4u64.to_le_bytes().to_vec();
        bad.extend_from_slice(&0u64.to_le_bytes());
        bad.extend_from_slice(&1u64.to_le_bytes());
        assert!(decode_subpacket_index(&bad).is_err());
    }

    #[test]
    fn subpacket_index_empty_roundtrip() {
        // Zero sub-packets is legal (writer crashed before first flush);
        // decoder must handle the count=0 case.
        let body = 0u64.to_le_bytes();
        let recovered = decode_subpacket_index(&body).unwrap();
        assert!(recovered.is_empty());
    }
}
