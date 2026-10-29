//! Stateless half of the packet protocol: header layout, constants,
//! pure parsers, and pure constructors.

use crate::error::MorlocError;
use crate::schema::Schema;
use crate::shm_types::RelPtr;

// ── Magic & version constants ──────────────────────────────────────────────

pub const PACKET_MAGIC: u32 = 0x0707_f86d;
pub const THIS_PLAIN: u16 = 0;
pub const THIS_VERSION: u16 = 0;
pub const DEFAULT_FLAVOR: u16 = 0;
pub const DEFAULT_MODE: u16 = 0;

/// Mode bit on a request on a local connection: the caller can reuse the
/// connection for its next call. On the reply: the callee keeps it (NET-7).
/// The caller clears it from every reply it hands on.
pub const MODE_KEEP: u16 = 1;

// ── Command type discriminants ─────────────────────────────────────────────

pub const PACKET_TYPE_DATA: u8 = 0;
pub const PACKET_TYPE_CALL: u8 = 1;
pub const PACKET_TYPE_PING: u8 = 2;
pub const PACKET_TYPE_STREAM: u8 = 3;
pub const PACKET_TYPE_FOOTER: u8 = 4;

// The `length` of every stream header: a stream has no global content length.
pub const STREAM_LENGTH_SENTINEL: u64 = u64::MAX;

// ── Stream/file handle kinds ───────────────────────────────────────────────

pub const MLC_KIND_IFILE: u8 = 0;
pub const MLC_KIND_ISTREAM: u8 = 1;
pub const MLC_KIND_OSTREAM: u8 = 2;
/// A stream whose producer and readers run at once: batches pass through
/// shared memory, never a file.
pub const MLC_KIND_CHANNEL: u8 = 3;

pub const fn handle_kind_name(k: u8) -> &'static str {
    match k {
        MLC_KIND_IFILE => "IFile",
        MLC_KIND_ISTREAM => "IStream",
        MLC_KIND_OSTREAM => "OStream",
        MLC_KIND_CHANNEL => "channel",
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
/// A failure because the call's downstream reader closed its pipe.
pub const PACKET_STATUS_PIPE_CLOSED: u8 = 0x02;

// ── Entrypoint ─────────────────────────────────────────────────────────────

pub const PACKET_ENTRYPOINT_LOCAL: u8 = 0x00;
pub const PACKET_ENTRYPOINT_REMOTE_SFS: u8 = 0x01;

// ── Lower-case display names for the packet-byte constants above. ──────────

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

pub const MORLOC_INLINE_THRESHOLD: usize = 64 * 1024;

// ── Metadata ───────────────────────────────────────────────────────────────

// ── Stream EOF tail ────────────────────────────────────────────────────────
//
// A stream file may end with a FOOTER packet followed by an 8-byte tail:
//   bytes 0..4: footer_length (u32 LE), the footer packet's size, excluding the tail
//   bytes 4..8: STREAM_TAIL_MAGIC
// The footer packet starts `footer_length + 8` bytes before EOF.

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
/// On a MESG+VOIDSTAR packet: the 15-bit volume index the producer encoded
/// in the high bits of every relptr in the payload, as a little-endian u16.
pub const METADATA_TYPE_VOL_INDEX: u8 = 0x03;
/// Frame index of a compressed payload. The entry body is
/// `[frame_count: u32 LE, [(uncompressed_size: u64 LE, compressed_size: u64 LE); frame_count]]`.
/// Every `PACKET_COMPRESSION_ZSTD` payload must carry one.
///
/// The entry's metadata `size` may exceed `4 + 16 * frame_count`, since
/// encoders may reserve room up front; trailing slots may be zero.
/// Decoders read `frame_count`, not the entry size.
pub const METADATA_TYPE_FRAME_INDEX: u8 = 0x04;

// ── Stream-footer metadata block types ────────────────────────────────────
//
// Blocks in the body of a `PACKET_TYPE_FOOTER` packet:
//   - `SUBPACKET_INDEX` (`0x05`), final footer only: `u64 count` followed
//     by `count * (u64 offset, u64 elem_count)`, the offset of each
//     sub-packet header from file start and its element count.
//   - `STREAM_DIAG` (`0x06`), temp and final footers: a `StreamDiag`.
//   - `FOOTER_FINAL` (`0x07`), final footer only: empty body. Its absence
//     means the writer did not finish the stream.
//   - `FOOTER_STATUS` (`0x08`), final footer only: one byte, a
//     `FOOTER_STATUS_*` value.
pub const METADATA_TYPE_SUBPACKET_INDEX: u8 = 0x05;
pub const METADATA_TYPE_STREAM_DIAG: u8 = 0x06;
pub const METADATA_TYPE_FOOTER_FINAL: u8 = 0x07;
pub const METADATA_TYPE_FOOTER_STATUS: u8 = 0x08;

/// Final-footer status values (the body of `METADATA_TYPE_FOOTER_STATUS`).
pub const FOOTER_STATUS_CLOSED: u8 = 0;
pub const FOOTER_STATUS_PAUSED: u8 = 1;
pub const FOOTER_STATUS_FAILED: u8 = 2;
pub const FOOTER_STATUS_REPAIRED: u8 = 3;

pub const METADATA_HEADER_MAGIC: [u8; 3] = *b"mmh";

// ── Subpacket-index helpers ────────────────────────────────────────────────

/// One SUBPACKET_INDEX entry: the byte offset of a sub-packet's header in
/// the stream file, and that sub-packet's element count.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SubpacketEntry {
    pub offset: u64,
    pub elem_count: u64,
}

/// Bytes per entry inside the SUBPACKET_INDEX block payload
/// (`u64 offset` + `u64 elem_count`). Body size is
/// `SUBPACKET_INDEX_HEADER_BYTES + n * SUBPACKET_ENTRY_BYTES`.
pub const SUBPACKET_ENTRY_BYTES: usize = 16;

/// Bytes before the per-entry array in the SUBPACKET_INDEX block
/// payload (the `count: u64 LE` prefix).
pub const SUBPACKET_INDEX_HEADER_BYTES: usize = 8;

// ── Frame-index helpers ────────────────────────────────────────────────────

/// One entry in a multi-frame compressed packet's frame index.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FrameEntry {
    pub uncompressed_size: u64,
    pub compressed_size: u64,
}

/// The total uncompressed and compressed sizes of `frames`, or an error when
/// a corrupt index sums past the address space.
pub fn frame_totals(frames: &[FrameEntry]) -> Result<(usize, usize), MorlocError> {
    frames.iter().try_fold((0usize, 0usize), |(u, c), f| {
        match (
            u.checked_add(crate::width::usize_from_u64(f.uncompressed_size)),
            c.checked_add(crate::width::usize_from_u64(f.compressed_size)),
        ) {
            (Some(u), Some(c)) => Ok((u, c)),
            _ => Err(MorlocError::Packet("frame index sizes overflow".into())),
        }
    })
}

/// Bytes per frame slot in the frame-index entry payload
/// (`u64` uncompressed_size + `u64` compressed_size).
pub const FRAME_ENTRY_BYTES: usize = 16;

/// Bytes before the per-frame array in the entry payload (the
/// `frame_count: u32 LE` prefix).
pub const FRAME_INDEX_HEADER_BYTES: usize = 4;

/// Encode a frame-index entry body (without the 8-byte metadata-entry header).
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

/// Decode a frame-index entry body (without the 8-byte entry header).
/// Bytes past `frame_count` slots are ignored.
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

/// The frame index in a packet's metadata, or `None` when it has none.
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
    /// SHM-13: for a reference packet, the generation of the block it names
    /// (little endian); 0 means unchecked.
    pub generation: [u8; 2],
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

/// Byte offsets of a data command's fields within the 32-byte packet
/// header, derived from the structs. Code that reads or patches packet
/// bytes directly uses these.
pub const PKT_HEADER_SIZE: usize = 32;
pub const PKT_CMD_OFF: usize = std::mem::offset_of!(PacketHeader, command);
pub const PKT_CMD_TYPE_OFF: usize = PKT_CMD_OFF + std::mem::offset_of!(CommandData, cmd_type);
pub const PKT_SOURCE_OFF: usize = PKT_CMD_OFF + std::mem::offset_of!(CommandData, source);
pub const PKT_FORMAT_OFF: usize = PKT_CMD_OFF + std::mem::offset_of!(CommandData, format);
pub const PKT_COMPRESSION_OFF: usize = PKT_CMD_OFF + std::mem::offset_of!(CommandData, compression);
pub const PKT_ENCRYPTION_OFF: usize = PKT_CMD_OFF + std::mem::offset_of!(CommandData, encryption);
pub const PKT_OFFSET_OFF: usize = std::mem::offset_of!(PacketHeader, offset);
pub const PKT_MODE_OFF: usize = std::mem::offset_of!(PacketHeader, mode);
pub const PKT_GENERATION_OFF: usize = PKT_CMD_OFF + std::mem::offset_of!(CommandData, generation);
pub const PKT_LENGTH_OFF: usize = std::mem::offset_of!(PacketHeader, length);

// Pin the wire layout: the C header and every binding assume these.
const _: () = assert!(PKT_CMD_TYPE_OFF == 12);
const _: () = assert!(PKT_SOURCE_OFF == 13);
const _: () = assert!(PKT_FORMAT_OFF == 14);
const _: () = assert!(PKT_COMPRESSION_OFF == 15);
const _: () = assert!(PKT_ENCRYPTION_OFF == 16);
const _: () = assert!(PKT_OFFSET_OFF == 20);
const _: () = assert!(PKT_LENGTH_OFF == 24);

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
                    generation: [0; 2],
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
                    generation: [0; 2],
                },
            },
            0, // metadata size set separately when building full packet
            payload_len,
        )
    }

/// A stream header: `length` is `STREAM_LENGTH_SENTINEL`, and the caller
/// sets `offset` to the metadata size.
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

/// A footer header; `length` is the byte size of its tagged blocks.
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
                    generation: [0; 2],
                },
            },
            0,
            error_msg_len,
        )
    }

    pub fn total_size(&self) -> u64 {
        self.offset as u64 + self.length
    }

    pub fn is_valid(&self) -> bool {
        self.magic == PACKET_MAGIC
    }

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
        self.is_data() && matches!(unsafe { self.command.data.status }, PACKET_STATUS_FAIL | PACKET_STATUS_PIPE_CLOSED)
    }

    pub fn is_pipe_closed(&self) -> bool {
        self.is_data() && unsafe { self.command.data.status } == PACKET_STATUS_PIPE_CLOSED
    }

    pub fn to_bytes(&self) -> [u8; 32] {
        unsafe { std::mem::transmute(*self) }
    }

    pub fn from_bytes(bytes: &[u8; 32]) -> Result<Self, MorlocError> {
        let header: PacketHeader = unsafe { std::mem::transmute(*bytes) };
        if !header.is_valid() {
            let magic = { header.magic };
            return Err(MorlocError::Packet(format!(
                "invalid magic: 0x{magic:08x}\n{NOT_A_PACKET}"
            )));
        }
        Ok(header)
    }
}

/// The hint given with every error that finds bytes not shaped like a packet.
pub const NOT_A_PACKET: &str =
    "expected a morloc voidstar packet, but the data is in another format";

// ── Full-packet validity check ──────────────────────────────────────────────

/// Confirm that `bytes` is one complete packet: a valid header and exactly
/// `32 + offset + length` bytes. For bytes from an untrusted source, before
/// any offset into them is followed.
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

    let meta_header_size = std::mem::size_of::<MetadataHeader>();
    let raw_meta_len = meta_header_size + schema_len;
    let padded_meta_len = ((raw_meta_len + 31) / 32) * 32;

    let payload_len = std::mem::size_of::<RelPtr>();

    let total = 32 + padded_meta_len + payload_len;
    let mut packet = vec![0u8; total];

    let header = PacketHeader::data_rptr(PACKET_FORMAT_VOIDSTAR, payload_len as u64);
    let mut hdr = header;
    hdr.offset = padded_meta_len as u32;
    let hdr_bytes = hdr.to_bytes();
    packet[..32].copy_from_slice(&hdr_bytes);

    let meta_start = 32;
    packet[meta_start] = b'm';
    packet[meta_start + 1] = b'm';
    packet[meta_start + 2] = b'h';
    packet[meta_start + 3] = METADATA_TYPE_SCHEMA_STRING;
    let meta_size_bytes = (schema_len as u32).to_le_bytes();
    packet[meta_start + 4..meta_start + 8].copy_from_slice(&meta_size_bytes);

    let schema_data_start = meta_start + meta_header_size;
    packet[schema_data_start..schema_data_start + schema_bytes.len()].copy_from_slice(schema_bytes);

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

    let mut header = PacketHeader::data_mesg(PACKET_FORMAT_MSGPACK, mpk_data.len() as u64);
    header.offset = padded_meta_len as u32;
    let hdr_bytes = header.to_bytes();
    packet[..32].copy_from_slice(&hdr_bytes);

    let meta_start = 32;
    packet[meta_start] = b'm';
    packet[meta_start + 1] = b'm';
    packet[meta_start + 2] = b'h';
    packet[meta_start + 3] = METADATA_TYPE_SCHEMA_STRING;
    let meta_size_bytes = (schema_len as u32).to_le_bytes();
    packet[meta_start + 4..meta_start + 8].copy_from_slice(&meta_size_bytes);

    let schema_data_start = meta_start + meta_header_size;
    packet[schema_data_start..schema_data_start + schema_bytes.len()].copy_from_slice(schema_bytes);

    let payload_start = 32 + padded_meta_len;
    packet[payload_start..payload_start + mpk_data.len()].copy_from_slice(mpk_data);

    packet
}

/// Build a call packet from argument data packets.
pub fn make_local_call_packet(midx: u32, arg_packets: &[Vec<u8>]) -> Vec<u8> {
    let data_length: usize = arg_packets.iter().map(|p| p.len()).sum();
    let total = 32 + data_length;
    let mut packet = vec![0u8; total];

    let header = PacketHeader::local_call(midx, data_length as u64);
    let hdr_bytes = header.to_bytes();
    packet[..32].copy_from_slice(&hdr_bytes);

    let mut pos = 32;
    for arg in arg_packets {
        packet[pos..pos + arg.len()].copy_from_slice(arg);
        pos += arg.len();
    }

    packet
}

/// The opening bytes of a stream file: the STREAM header and a metadata
/// block carrying the list schema `[a]`. Sub-packets follow it, then
/// optionally a FOOTER packet and the EOF tail.
///
/// `schema` must be an Array schema.
pub fn make_stream_header_block(schema: &Schema) -> Vec<u8> {
    debug_assert!(
        schema.serial_type == crate::schema::SerialType::Array,
        "make_stream_header_block: non-Array schema {:?} (compiler bug)",
        schema.serial_type,
    );
    let schema_str = crate::schema::schema_to_string(schema);
    let schema_bytes = schema_str.as_bytes();
    let schema_len = schema_bytes.len() + 1; // null terminator

    let meta_header_size = std::mem::size_of::<MetadataHeader>();
    let raw_meta_len = meta_header_size + schema_len;
    let padded_meta_len = raw_meta_len.div_ceil(METADATA_BLOCK_ALIGNMENT)
        * METADATA_BLOCK_ALIGNMENT;

    let total = 32 + padded_meta_len;
    let mut out = vec![0u8; total];

    let mut hdr = PacketHeader::stream();
    hdr.offset = padded_meta_len as u32;
    let hdr_bytes = hdr.to_bytes();
    out[..32].copy_from_slice(&hdr_bytes);

    let meta_start = 32;
    out[meta_start..meta_start + 3].copy_from_slice(&METADATA_HEADER_MAGIC);
    out[meta_start + 3] = METADATA_TYPE_SCHEMA_STRING;
    out[meta_start + 4..meta_start + 8]
        .copy_from_slice(&(schema_len as u32).to_le_bytes());
    let schema_data_start = meta_start + meta_header_size;
    out[schema_data_start..schema_data_start + schema_bytes.len()]
        .copy_from_slice(schema_bytes);

    out
}

/// A FOOTER packet holding `blocks` as unpadded metadata entries.
pub fn make_footer_packet(blocks: &[(u8, &[u8])]) -> Vec<u8> {
    let payload_len: usize = blocks
        .iter()
        .map(|(_, body)| std::mem::size_of::<MetadataHeader>() + body.len())
        .sum();

    let total = 32 + payload_len;
    let mut out = vec![0u8; total];

// `iter_packet_metadata` reads entries from `32..32 + offset`, so `offset`
// covers the blocks even though they are also the payload.
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
// Carried in every stream footer, temp and final. Its size is fixed, so a
// temp footer is always `STREAM_TEMP_FOOTER_BYTES` long.

/// Current StreamDiag struct version; bump when adding fields.
pub const STREAM_DIAG_VERSION: u32 = 1;

/// Capacity of the StreamDiag tail window; only `tail[0..tail_len]` is meaningful.
pub const STREAM_DIAG_TAIL_MAX: usize = 8;

/// Wire layout of the `METADATA_TYPE_STREAM_DIAG` body.
#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct StreamDiag {
    pub diag_version:                u32,
    pub writer_pid:                  u32,
    pub n_oversize_subpackets:       u32,
    pub tail_len:                    u32,

    pub _reserved:                   u64,

    pub writer_start_time:           u64, // unix-epoch microseconds at @open
    pub subpacket_count:             u64,
    pub element_count:               u64,
    pub bytes_uncompressed_total:    u64,
    pub bytes_compressed_total:      u64,
    pub largest_packet_uncompressed: u64,
    pub largest_packet_idx:          u64,
    pub first_flush_time:            u64,
    pub last_flush_time:             u64,

    // Byte offsets of the most recent `tail_len` sub-packets, oldest first.
    pub tail: [u64; STREAM_DIAG_TAIL_MAX],
}

const _: () = assert!(std::mem::size_of::<StreamDiag>() == 160);

impl StreamDiag {
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

/// Parse the first 160 bytes of `data`; trailing bytes are ignored.
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

/// Aligned copy of a `StreamDiag`, with `tail` cut to `tail_len` entries.
#[derive(Debug, Clone)]
pub struct StreamDiagView {
    pub diag_version: u32,
    pub writer_pid: u32,
    pub n_oversize_subpackets: u32,
    pub writer_start_time: u64,
    pub subpacket_count: u64,
    pub element_count: u64,
    pub bytes_uncompressed_total: u64,
    pub bytes_compressed_total: u64,
    pub largest_packet_uncompressed: u64,
    pub largest_packet_idx: u64,
    pub first_flush_time: u64,
    pub last_flush_time: u64,
    pub tail: Vec<u64>,
}

impl StreamDiag {
    pub fn snapshot(&self) -> StreamDiagView {
        let tail_len = { self.tail_len } as usize;
        let capped = tail_len.min(STREAM_DIAG_TAIL_MAX);
        let tail_copy: [u64; STREAM_DIAG_TAIL_MAX] = { self.tail };
        StreamDiagView {
            diag_version: { self.diag_version },
            writer_pid: { self.writer_pid },
            n_oversize_subpackets: { self.n_oversize_subpackets },
            writer_start_time: { self.writer_start_time },
            subpacket_count: { self.subpacket_count },
            element_count: { self.element_count },
            bytes_uncompressed_total: { self.bytes_uncompressed_total },
            bytes_compressed_total: { self.bytes_compressed_total },
            largest_packet_uncompressed: { self.largest_packet_uncompressed },
            largest_packet_idx: { self.largest_packet_idx },
            first_flush_time: { self.first_flush_time },
            last_flush_time: { self.last_flush_time },
            tail: tail_copy[..capped].to_vec(),
        }
    }
}

/// Bytes of a temp footer on disk: footer packet header (32 B) + one
/// metadata block header (8 B) + StreamDiag (160 B) + EOF tail (8 B).
pub const STREAM_TEMP_FOOTER_BYTES: usize = 32 + 8 + 160 + STREAM_TAIL_SIZE;

/// A temp footer (STREAM_DIAG only) and its EOF tail, exactly
/// `STREAM_TEMP_FOOTER_BYTES` long.
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

/// A final footer (STREAM_DIAG, SUBPACKET_INDEX, FOOTER_FINAL and
/// FOOTER_STATUS) and its EOF tail.
pub fn make_final_footer_packet(
    diag: &StreamDiag,
    subpacket_entries: &[SubpacketEntry],
    status: u8,
) -> Vec<u8> {
    let mut idx_body = Vec::with_capacity(
        SUBPACKET_INDEX_HEADER_BYTES + subpacket_entries.len() * SUBPACKET_ENTRY_BYTES,
    );
    idx_body.extend_from_slice(&(subpacket_entries.len() as u64).to_le_bytes());
    for entry in subpacket_entries {
        idx_body.extend_from_slice(&entry.offset.to_le_bytes());
        idx_body.extend_from_slice(&entry.elem_count.to_le_bytes());
    }

    let diag_bytes = diag.as_bytes();
    let status_body = [status];
    let footer_packet = make_footer_packet(&[
        (METADATA_TYPE_STREAM_DIAG, &diag_bytes),
        (METADATA_TYPE_SUBPACKET_INDEX, &idx_body),
        (METADATA_TYPE_FOOTER_FINAL, &[]),
        (METADATA_TYPE_FOOTER_STATUS, &status_body),
    ]);
    let mut out = Vec::with_capacity(footer_packet.len() + STREAM_TAIL_SIZE);
    out.extend_from_slice(&footer_packet);
    let tail = encode_stream_tail(footer_packet.len() as u32);
    out.extend_from_slice(&tail);
    out
}

/// The status in a FOOTER_STATUS body; an empty body reads as `FOOTER_STATUS_CLOSED`.
pub fn decode_footer_status(data: &[u8]) -> u8 {
    if data.is_empty() {
        FOOTER_STATUS_CLOSED
    } else {
        data[0]
    }
}

/// Parse a SUBPACKET_INDEX body (without the 8-byte entry header).
pub fn decode_subpacket_index(data: &[u8]) -> Result<Vec<SubpacketEntry>, MorlocError> {
    if data.len() < SUBPACKET_INDEX_HEADER_BYTES {
        return Err(MorlocError::Packet(format!(
            "SUBPACKET_INDEX entry truncated: {} bytes", data.len()
        )));
    }
    let count = u64::from_le_bytes(data[..8].try_into().unwrap());
    let needed = usize::try_from(count)
        .ok()
        .and_then(|n| n.checked_mul(SUBPACKET_ENTRY_BYTES))
        .and_then(|n| n.checked_add(SUBPACKET_INDEX_HEADER_BYTES));
    if needed.map_or(true, |needed| data.len() < needed) {
        return Err(MorlocError::Packet(format!(
            "SUBPACKET_INDEX claims {} entries but payload has {} bytes",
            count, data.len()
        )));
    }
    let count = count as usize;
    let mut out = Vec::with_capacity(count);
    let mut cur = SUBPACKET_INDEX_HEADER_BYTES;
    for _ in 0..count {
        let offset = u64::from_le_bytes(data[cur..cur + 8].try_into().unwrap());
        let elem_count = u64::from_le_bytes(data[cur + 8..cur + 16].try_into().unwrap());
        out.push(SubpacketEntry { offset, elem_count });
        cur += SUBPACKET_ENTRY_BYTES;
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
    let start = 32 + { header.offset } as usize;
    let length = header.length;
    let end = usize::try_from(length).ok().and_then(|n| n.checked_add(start));
    match end {
        Some(end) if end <= packet.len() => Ok(&packet[start..end]),
        _ => Err(MorlocError::Packet("payload extends past packet end".into())),
    }
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

/// Iterator over the entries of a metadata block, yielding
/// `(metadata_type, data)`. It stops at the first non-`mmh` magic or at an
/// entry that runs past the block's end.
///
/// The input is the metadata block alone; for a full packet slice
/// `&packet[32..32 + header.offset]`.
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

/// The byte length of the valid entries at the start of `meta`; padding
/// after the last entry lies past it. A new entry must be spliced in here,
/// since iteration stops at the first non-`mmh` byte.
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

/// Width to which metadata blocks are padded, so the payload starts aligned.
pub const METADATA_BLOCK_ALIGNMENT: usize = 32;

/// A new metadata block: the valid entries at the start of `existing`,
/// then the entry `(kind, body)`, padded to `METADATA_BLOCK_ALIGNMENT`.
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

/// A new metadata block: `prepended`, then the valid entries at the start
/// of `existing`, padded to `METADATA_BLOCK_ALIGNMENT`.
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

/// Decode a null-terminated UTF-8 (lossy) `METADATA_TYPE_SCHEMA_STRING` body.
pub fn decode_schema_entry(data: &[u8]) -> String {
    let len = data.iter().position(|&b| b == 0).unwrap_or(data.len());
    String::from_utf8_lossy(&data[..len]).into_owned()
}

/// The `METADATA_TYPE_VOL_INDEX` hint in a packet's metadata, if present.
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

// ── Forward-scan recovery ─────────────────────────────────────────────────

/// What a forward scan over a stream file's sub-packets recovers.
#[derive(Debug, Clone, Default)]
pub struct ForwardScan {
    /// Byte offsets (from file start) of each intact sub-packet header.
    pub subpacket_offsets: Vec<u64>,
    /// Sum of the array-size prefixes of the uncompressed voidstar
    /// sub-packets.
    pub element_count: u64,
    /// Bytes from `body_start` through the last intact sub-packet.
    pub bytes_scanned: u64,
    /// True iff the scan stopped at a truncated sub-packet, which is not in
    /// `subpacket_offsets`.
    pub partial: bool,
    /// Sub-packets left out of `element_count`.
    pub compressed_uncounted: u64,
    /// True iff the scan stopped at its sub-packet cap.
    pub scan_capped: bool,
}

/// Walk the sub-packet headers from `body_start` until the first non-data
/// packet, a corrupt header, or EOF, with no cap.
pub fn forward_scan_subpackets(
    mmap: &[u8],
    body_start: u64,
) -> Result<ForwardScan, MorlocError> {
    forward_scan_subpackets_capped(mmap, body_start, u64::MAX)
}

/// `forward_scan_subpackets`, stopping after `cap` sub-packets with `scan_capped` set.
pub fn forward_scan_subpackets_capped(
    mmap: &[u8],
    body_start: u64,
    cap: u64,
) -> Result<ForwardScan, MorlocError> {
    let size = mmap.len() as u64;
    if body_start > size {
        return Err(MorlocError::Packet(format!(
            "forward_scan: body_start {} exceeds file size {}",
            body_start, size,
        )));
    }
    let mut out = ForwardScan::default();
    let mut cur = body_start;

    while cur + 32 <= size {
        if out.subpacket_offsets.len() as u64 >= cap {
            out.scan_capped = true;
            break;
        }
        let hdr_start = cur as usize;
        let hdr_bytes: &[u8; 32] = mmap[hdr_start..hdr_start + 32]
            .try_into()
            .expect("32-byte slice");
        let header = match PacketHeader::from_bytes(hdr_bytes) {
            Ok(h) => h,
            Err(_) => break, // corrupt header: stop cleanly
        };
        if header.is_footer() {
            break;
        }
        if !header.is_data() {
            break;
        }
        let offset = header.offset as u64;
        let length = header.length as u64;
        let total = 32u64
            .checked_add(offset)
            .and_then(|x| x.checked_add(length))
            .ok_or_else(|| MorlocError::Packet(
                "forward_scan: sub-packet total-size overflow".into(),
            ))?;
        if cur + total > size {
            out.partial = true;
            break;
        }

        // Only an uncompressed voidstar payload starts with the array-size prefix.
        let data = unsafe { header.command.data };
        let is_voidstar = data.format == PACKET_FORMAT_VOIDSTAR;
        let is_uncompressed = data.compression == PACKET_COMPRESSION_NONE;
        if is_voidstar && is_uncompressed {
            let payload_start = (cur + 32 + offset) as usize;
            if payload_start + 8 <= mmap.len() {
                let size_bytes: [u8; 8] = mmap
                    [payload_start..payload_start + 8]
                    .try_into()
                    .expect("8-byte size prefix");
                let elem_count = u64::from_le_bytes(size_bytes);
                out.element_count = out.element_count.saturating_add(elem_count);
            } else {
                out.partial = true;
                break;
            }
        } else {
            out.compressed_uncounted = out.compressed_uncounted.saturating_add(1);
        }

        out.subpacket_offsets.push(cur);
        cur += total;
    }
    out.bytes_scanned = cur.saturating_sub(body_start);
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_header_size() {
        assert_eq!(std::mem::size_of::<PacketHeader>(), 32);
    }

    #[test]
    fn test_bad_magic_names_expected_format() {
        let mut bytes = [0u8; 32];
        bytes[..4].copy_from_slice(b">sp|");
        let msg = PacketHeader::from_bytes(&bytes).unwrap_err().to_string();
        assert!(msg.contains("invalid magic: 0x7c70733e"), "{msg}");
        assert!(msg.contains("expected a morloc voidstar packet"), "{msg}");
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
        let encoded = encode_frame_index_entry(&[]);
        assert_eq!(encoded.len(), FRAME_INDEX_HEADER_BYTES);
        let decoded = decode_frame_index_entry(&encoded).unwrap();
        assert!(decoded.is_empty());
    }

    #[test]
    fn frame_index_entry_tolerates_trailing_padding() {
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

    /// The list-shaped value schema `[a]` a stream header must carry (see
    /// make_stream_header_block); tests pass the bare element `a`. The `a`
    /// prefix is the Array schema-string form.
    fn list_schema(elem: &Schema) -> Schema {
        crate::schema::parse_schema(&format!(
            "a{}", crate::schema::schema_to_string(elem)
        )).unwrap()
    }

    /// Helper: build a complete stream file = header + sub-packets + footer + tail.
    fn build_stream_file(schema: &Schema, schema_str: &str, n: u32) -> Vec<u8> {
        let mut file = make_stream_header_block(&list_schema(schema));
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

        let tail_off = file.len() - STREAM_TAIL_SIZE;
        let tail: &[u8; STREAM_TAIL_SIZE] = file[tail_off..].try_into().unwrap();
        let footer_len = decode_stream_tail(tail).expect("footer should be present");

        let footer_off = tail_off - footer_len as usize;
        let footer_hdr = PacketHeader::from_bytes(
            file[footer_off..footer_off + 32].try_into().unwrap(),
        )
        .unwrap();
        assert!(footer_hdr.is_footer());

        let stream_hdr = PacketHeader::from_bytes(file[..32].try_into().unwrap()).unwrap();
        assert!(stream_hdr.is_stream());
        let stream_hdr_len = { stream_hdr.length };
        assert_eq!(stream_hdr_len, STREAM_LENGTH_SENTINEL);
        let recovered_schema = read_schema_from_meta(&file[..32 + stream_hdr.offset as usize])
            .unwrap()
            .unwrap();
        // The header carries the list-shaped value schema `[u4]`, not the
        // bare element `u4`.
        assert_eq!(recovered_schema, crate::schema::schema_to_string(&list_schema(&schema)));

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
        // No footer and no EOF tail.
        let schema = crate::schema::parse_schema("u4").unwrap();
        let mut file = make_stream_header_block(&list_schema(&schema));
        for sp in build_subpackets("u4", 3) {
            file.extend_from_slice(&sp);
        }

        let tail_off = file.len() - STREAM_TAIL_SIZE;
        let tail: &[u8; STREAM_TAIL_SIZE] = file[tail_off..].try_into().unwrap();
        assert!(decode_stream_tail(tail).is_none());

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

        let mut a = make_stream_header_block(&list_schema(&schema));
        for sp in build_subpackets("u4", 3) {
            a.extend_from_slice(&sp);
        }

        let mut b = make_stream_header_block(&list_schema(&schema));
        for v in &[10u32, 11] {
            let pkt = make_mesg_data_packet(&v.to_le_bytes(), "u4");
            b.extend_from_slice(&pkt);
        }

        let a_header_len = 32 + PacketHeader::from_bytes(a[..32].try_into().unwrap())
            .unwrap()
            .offset as usize;
        let b_header_len = 32 + PacketHeader::from_bytes(b[..32].try_into().unwrap())
            .unwrap()
            .offset as usize;
        let mut merged = a.clone();
        merged.extend_from_slice(&b[b_header_len..]);

        assert_eq!(&merged[..a_header_len], &a[..a_header_len]);

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
        let block_a: &[u8] = b"hello";
        let block_b: &[u8] = &[1, 2, 3, 4, 5, 6, 7, 8];
        let footer = make_footer_packet(&[
            (0x10, block_a),
            (0x11, block_b),
        ]);
        let hdr = PacketHeader::from_bytes(footer[..32].try_into().unwrap()).unwrap();
        assert!(hdr.is_footer());

        // Footers have no post-metadata payload, so `offset == length`.
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
        let stream = PacketHeader::stream();
        let data = PacketHeader::data_mesg(PACKET_FORMAT_MSGPACK, 0);
        assert_ne!(stream.command_type(), data.command_type());
    }

    #[test]
    fn read_frame_index_from_meta_returns_entries_when_present() {
        let schema_bytes = b"au1\0";
        let schema_entry_size = schema_bytes.len();
        let frames = vec![
            FrameEntry { uncompressed_size: 100, compressed_size: 40 },
            FrameEntry { uncompressed_size: 200, compressed_size: 80 },
        ];
        let index_entry = encode_frame_index_entry(&frames);
        let meta_bytes = {
            let mut m = Vec::new();
            m.extend_from_slice(&METADATA_HEADER_MAGIC);
            m.push(METADATA_TYPE_SCHEMA_STRING);
            m.extend_from_slice(&(schema_entry_size as u32).to_le_bytes());
            m.extend_from_slice(schema_bytes);
            m.extend_from_slice(&METADATA_HEADER_MAGIC);
            m.push(METADATA_TYPE_FRAME_INDEX);
            m.extend_from_slice(&(index_entry.len() as u32).to_le_bytes());
            m.extend_from_slice(&index_entry);
            m
        };
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
        let diag = StreamDiag::new();
        let mut padded = diag.as_bytes().to_vec();
        padded.extend_from_slice(&[0xAB; 32]);
        let recovered = StreamDiag::from_bytes(&padded).unwrap();
        assert_eq!({ recovered.diag_version }, STREAM_DIAG_VERSION);
    }

    #[test]
    fn temp_footer_packet_has_fixed_size() {
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

        let footer_packet_len = bytes.len() - STREAM_TAIL_SIZE;
        let footer = &bytes[..footer_packet_len];
        let hdr = PacketHeader::from_bytes(footer[..32].try_into().unwrap()).unwrap();
        assert!(hdr.is_footer());

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

        let tail: [u8; STREAM_TAIL_SIZE] =
            bytes[footer_packet_len..].try_into().unwrap();
        let recovered_len = decode_stream_tail(&tail).unwrap();
        assert_eq!(recovered_len as usize, footer_packet_len);
    }

    #[test]
    fn final_footer_packet_carries_all_blocks_including_status() {
        let diag = StreamDiag::new();
        let entries: Vec<SubpacketEntry> = (0..5)
            .map(|i| SubpacketEntry {
                offset: (i as u64) * (16 << 20),
                elem_count: 1024 + (i as u64) * 7,
            })
            .collect();
        let bytes = make_final_footer_packet(&diag, &entries, FOOTER_STATUS_PAUSED);

        let footer_packet_len = bytes.len() - STREAM_TAIL_SIZE;
        let footer = &bytes[..footer_packet_len];
        let hdr = PacketHeader::from_bytes(footer[..32].try_into().unwrap()).unwrap();
        assert!(hdr.is_footer());

        let mut saw_diag = false;
        let mut saw_final = false;
        let mut recovered_entries: Option<Vec<SubpacketEntry>> = None;
        let mut recovered_status: Option<u8> = None;
        for (kind, body) in iter_packet_metadata(footer).unwrap() {
            match kind {
                METADATA_TYPE_STREAM_DIAG => saw_diag = true,
                METADATA_TYPE_FOOTER_FINAL => {
                    saw_final = true;
                    assert_eq!(body.len(), 0);
                }
                METADATA_TYPE_SUBPACKET_INDEX => {
                    recovered_entries = Some(decode_subpacket_index(body).unwrap());
                }
                METADATA_TYPE_FOOTER_STATUS => {
                    recovered_status = Some(decode_footer_status(body));
                }
                _ => {}
            }
        }
        assert!(saw_diag);
        assert!(saw_final, "final footer must carry FOOTER_FINAL");
        assert_eq!(recovered_entries.as_deref(), Some(entries.as_slice()));
        assert_eq!(recovered_status, Some(FOOTER_STATUS_PAUSED));
    }

    #[test]
    fn decode_footer_status_defaults_on_missing_body() {
        assert_eq!(decode_footer_status(&[]), FOOTER_STATUS_CLOSED);
        assert_eq!(decode_footer_status(&[FOOTER_STATUS_PAUSED]), FOOTER_STATUS_PAUSED);
        assert_eq!(decode_footer_status(&[FOOTER_STATUS_REPAIRED]), FOOTER_STATUS_REPAIRED);
    }

    #[test]
    fn subpacket_index_decode_rejects_truncated() {
        // count claims 4 entries (needs 8 + 4*16 = 72 bytes) but only
        // one pair (16 bytes) provided after the count header.
        let mut bad = 4u64.to_le_bytes().to_vec();
        bad.extend_from_slice(&0u64.to_le_bytes());
        bad.extend_from_slice(&1u64.to_le_bytes());
        assert!(decode_subpacket_index(&bad).is_err());
    }

    #[test]
    fn subpacket_index_empty_roundtrip() {
        let body = 0u64.to_le_bytes();
        let recovered = decode_subpacket_index(&body).unwrap();
        assert!(recovered.is_empty());
    }

    #[test]
    fn subpacket_index_pair_roundtrip() {
        let mut body = 3u64.to_le_bytes().to_vec();
        for &(off, n) in &[(0u64, 100u64), (1024, 250), (2048, 175)] {
            body.extend_from_slice(&off.to_le_bytes());
            body.extend_from_slice(&n.to_le_bytes());
        }
        let recovered = decode_subpacket_index(&body).unwrap();
        assert_eq!(recovered, vec![
            SubpacketEntry { offset: 0,    elem_count: 100 },
            SubpacketEntry { offset: 1024, elem_count: 250 },
            SubpacketEntry { offset: 2048, elem_count: 175 },
        ]);
    }

    /// A count whose entries would not fit the address space is refused,
    /// not wrapped into a small one.
    #[test]
    fn subpacket_index_decode_rejects_a_count_past_the_address_space() {
        for count in [u64::MAX, u64::MAX / 16 + 1, 1u64 << 60] {
            let mut body = count.to_le_bytes().to_vec();
            body.extend_from_slice(&[0u8; 32]);
            assert!(decode_subpacket_index(&body).is_err(), "{count}");
        }
    }

    #[test]
    fn a_payload_length_past_the_address_space_is_refused() {
        let mut header = PacketHeader::data_mesg(PACKET_FORMAT_VOIDSTAR, u64::MAX - 8);
        header.offset = 0;
        let mut packet = header.to_bytes().to_vec();
        packet.extend_from_slice(&[0u8; 16]);
        assert!(get_data_payload(&packet).is_err());
    }

    #[test]
    fn subpacket_index_decode_rejects_short_header() {
        let bad = [0u8; 4];
        assert!(decode_subpacket_index(&bad).is_err());
    }

    /// Build a synthetic uncompressed sub-packet header + payload with a
    /// given array-size prefix, for exercising forward_scan_subpackets.
    fn build_uncompressed_subpacket(elem_count: u64, payload_extra: usize) -> Vec<u8> {
        let payload_len = 8 + payload_extra;
        let mut hdr = PacketHeader::data_mesg(PACKET_FORMAT_VOIDSTAR, payload_len as u64);
        hdr.offset = 0; // no metadata between header and payload
        let mut out = Vec::with_capacity(32 + payload_len);
        out.extend_from_slice(&hdr.to_bytes());
        out.extend_from_slice(&elem_count.to_le_bytes());
        out.resize(32 + payload_len, 0);
        out
    }

    #[test]
    fn forward_scan_empty_body() {
        let mmap = vec![0u8; 32]; // any 32-byte prelude
        let scan = forward_scan_subpackets(&mmap, 32).unwrap();
        assert!(scan.subpacket_offsets.is_empty());
        assert_eq!(scan.element_count, 0);
        assert_eq!(scan.bytes_scanned, 0);
        assert!(!scan.partial);
        assert_eq!(scan.compressed_uncounted, 0);
    }

    #[test]
    fn forward_scan_two_uncompressed_subpackets() {
        let mut mmap = vec![0u8; 32];
        mmap.extend_from_slice(&build_uncompressed_subpacket(100, 0));
        let off1 = mmap.len() as u64;
        let expected_off0: u64 = 32;
        mmap.extend_from_slice(&build_uncompressed_subpacket(250, 0));
        let scan = forward_scan_subpackets(&mmap, 32).unwrap();
        assert_eq!(scan.subpacket_offsets, vec![expected_off0, off1]);
        assert_eq!(scan.element_count, 350);
        assert!(!scan.partial);
        assert_eq!(scan.compressed_uncounted, 0);
    }

    #[test]
    fn forward_scan_stops_at_footer() {
        let mut mmap = vec![0u8; 32];
        mmap.extend_from_slice(&build_uncompressed_subpacket(10, 0));
        mmap.extend_from_slice(&build_uncompressed_subpacket(20, 0));
        let footer_hdr = PacketHeader::footer(0);
        mmap.extend_from_slice(&footer_hdr.to_bytes());
        let scan = forward_scan_subpackets(&mmap, 32).unwrap();
        assert_eq!(scan.subpacket_offsets.len(), 2);
        assert_eq!(scan.element_count, 30);
        assert!(!scan.partial);
    }

    #[test]
    fn forward_scan_marks_partial_on_truncation() {
        let mut mmap = vec![0u8; 32];
        mmap.extend_from_slice(&build_uncompressed_subpacket(3, 0));
        mmap.extend_from_slice(&build_uncompressed_subpacket(4, 0));
        let mut trunc = PacketHeader::data_mesg(PACKET_FORMAT_VOIDSTAR, 4096);
        trunc.offset = 0;
        mmap.extend_from_slice(&trunc.to_bytes());
        let scan = forward_scan_subpackets(&mmap, 32).unwrap();
        assert_eq!(scan.subpacket_offsets.len(), 2);
        assert_eq!(scan.element_count, 7);
        assert!(scan.partial);
    }

    #[test]
    fn forward_scan_rejects_out_of_range_body_start() {
        let mmap = vec![0u8; 32];
        assert!(forward_scan_subpackets(&mmap, 64).is_err());
    }
}
