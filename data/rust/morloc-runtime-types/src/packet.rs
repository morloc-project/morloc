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
}
