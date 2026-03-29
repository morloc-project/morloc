use crate::error::MorlocError;

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
pub const PACKET_ENCRYPTION_NONE: u8 = 0x00;

// ── Status ─────────────────────────────────────────────────────────────────

pub const PACKET_STATUS_PASS: u8 = 0x00;
pub const PACKET_STATUS_FAIL: u8 = 0x01;

// ── Entrypoint ────────────────────���────────────────────────────────────────

pub const PACKET_ENTRYPOINT_LOCAL: u8 = 0x00;
pub const PACKET_ENTRYPOINT_REMOTE_SFS: u8 = 0x01;

// ── Inline threshold ─────────────────────────────────────��─────────────────

pub const MORLOC_INLINE_THRESHOLD: usize = 64 * 1024;

// ── Metadata ─────────��─────────────────────────────────────────────────────

pub const METADATA_TYPE_SCHEMA_STRING: u8 = 0x01;
pub const METADATA_TYPE_XXHASH: u8 = 0x02;
pub const METADATA_HEADER_MAGIC: [u8; 3] = *b"mmh";

// ── Packed structs matching the C binary layout ────���───────────────────────

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

// ── Full packet construction (header + metadata + payload) ─────────────────

/// Build a complete data packet with schema metadata and relptr payload.
pub fn make_standard_data_packet(relptr: crate::shm::RelPtr, schema: &crate::Schema) -> Vec<u8> {
    let schema_str = crate::schema::schema_to_string(schema);
    let schema_bytes = schema_str.as_bytes();
    let schema_len = schema_bytes.len() + 1; // +1 for null terminator

    // Metadata: header (8 bytes) + schema string (null-terminated), padded to 32-byte boundary
    let meta_header_size = std::mem::size_of::<MetadataHeader>();
    let raw_meta_len = meta_header_size + schema_len;
    let padded_meta_len = ((raw_meta_len + 31) / 32) * 32;

    // Payload: relptr
    let payload_len = std::mem::size_of::<crate::shm::RelPtr>();

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
pub fn make_mesg_data_packet(mpk_data: &[u8], schema: &crate::Schema) -> Vec<u8> {
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

/// Read the schema string from packet metadata section.
pub fn read_schema_from_meta(packet: &[u8]) -> Result<Option<String>, MorlocError> {
    if packet.len() < 32 {
        return Err(MorlocError::Packet("packet too small".into()));
    }
    let header = PacketHeader::from_bytes(packet[..32].try_into().unwrap())?;
    let offset = { header.offset } as usize;
    if offset == 0 {
        return Ok(None);
    }

    // Scan metadata headers
    let meta_start = 32;
    let meta_end = meta_start + offset;
    let mut pos = meta_start;
    while pos + 8 <= meta_end {
        if packet[pos] == b'm' && packet[pos + 1] == b'm' && packet[pos + 2] == b'h' {
            let meta_type = packet[pos + 3];
            let meta_size = u32::from_le_bytes([
                packet[pos + 4], packet[pos + 5], packet[pos + 6], packet[pos + 7],
            ]) as usize;
            if meta_type == METADATA_TYPE_SCHEMA_STRING {
                let str_start = pos + 8;
                let str_end = str_start + meta_size;
                if str_end <= meta_end {
                    let bytes = &packet[str_start..str_end];
                    // Find null terminator
                    let len = bytes.iter().position(|&b| b == 0).unwrap_or(bytes.len());
                    return Ok(Some(String::from_utf8_lossy(&bytes[..len]).into_owned()));
                }
            }
            pos += 8 + meta_size;
        } else {
            break;
        }
    }
    Ok(None)
}

/// Get the voidstar value from a data packet (resolves relptr to absptr).
pub fn get_data_value(
    packet: &[u8],
    schema: &crate::Schema,
) -> Result<crate::shm::AbsPtr, MorlocError> {
    let header = PacketHeader::from_bytes(packet[..32].try_into().unwrap())?;
    let source = unsafe { header.command.data.source };
    let format = unsafe { header.command.data.format };

    let payload = get_data_payload(packet)?;

    match source {
        PACKET_SOURCE_RPTR => {
            // Payload is a relptr
            if payload.len() < std::mem::size_of::<crate::shm::RelPtr>() {
                return Err(MorlocError::Packet("relptr payload too small".into()));
            }
            let relptr = crate::shm::RelPtr::from_ne_bytes(
                payload[..std::mem::size_of::<crate::shm::RelPtr>()].try_into().unwrap()
            );
            crate::shm::rel2abs(relptr)
        }
        PACKET_SOURCE_MESG => {
            match format {
                PACKET_FORMAT_MSGPACK => {
                    crate::mpack::unpack_with_schema(payload, schema)
                }
                PACKET_FORMAT_JSON => {
                    let json_str = std::str::from_utf8(payload)
                        .map_err(|e| MorlocError::Packet(format!("invalid UTF-8: {}", e)))?;
                    crate::json::read_json_with_schema(json_str, schema)
                }
                PACKET_FORMAT_VOIDSTAR => {
                    read_voidstar_binary(payload, schema)
                }
                _ => {
                    Err(MorlocError::Packet(format!(
                        "unsupported data format: {}", format
                    )))
                }
            }
        }
        _ => Err(MorlocError::Packet(format!("unsupported source: {}", source))),
    }
}

// ── Inline voidstar deserialization ─────────────────────────────────────────

/// Read a flat voidstar binary blob into shared memory, adjusting relptrs.
fn read_voidstar_binary(
    blob: &[u8],
    schema: &crate::Schema,
) -> Result<crate::shm::AbsPtr, MorlocError> {
    use crate::shm;

    let base = shm::shmalloc(blob.len())?;
    unsafe { std::ptr::copy_nonoverlapping(blob.as_ptr(), base, blob.len()) };

    let base_rel = shm::abs2rel(base)?;
    adjust_voidstar_relptrs(base, schema, base_rel)?;
    Ok(base)
}

/// Adjust relptrs in a voidstar blob that was copied into SHM.
/// The blob's internal relptrs are offsets from position 0 of the blob.
/// Adding `base_rel` converts them to valid SHM relptrs.
fn adjust_voidstar_relptrs(
    data: crate::shm::AbsPtr,
    schema: &crate::Schema,
    base_rel: crate::shm::RelPtr,
) -> Result<(), MorlocError> {
    use crate::schema::SerialType;
    use crate::shm::{self, Array, Tensor};

    unsafe {
        match schema.serial_type {
            SerialType::String | SerialType::Array => {
                let arr = &mut *(data as *mut Array);
                arr.data += base_rel;
                // Recurse into elements if variable-width (strings are always fixed-width bytes)
                if !schema.parameters.is_empty() && !schema.parameters[0].is_fixed_width() {
                    let arr_data = shm::rel2abs(arr.data)?;
                    let elem_width = schema.parameters[0].width;
                    for i in 0..arr.size {
                        let elem = arr_data.add(i * elem_width);
                        adjust_voidstar_relptrs(elem, &schema.parameters[0], base_rel)?;
                    }
                }
            }
            SerialType::Tuple | SerialType::Map => {
                for i in 0..schema.parameters.len() {
                    let child = data.add(schema.offsets[i]);
                    adjust_voidstar_relptrs(child, &schema.parameters[i], base_rel)?;
                }
            }
            SerialType::Optional => {
                let tag = *data;
                if tag != 0 {
                    let inner_offset = schema.offsets.first().copied().unwrap_or(
                        shm::align_up(1, schema.parameters[0].alignment().max(1)),
                    );
                    let child = data.add(inner_offset);
                    adjust_voidstar_relptrs(child, &schema.parameters[0], base_rel)?;
                }
            }
            SerialType::Tensor => {
                let tensor = &mut *(data as *mut Tensor);
                if tensor.total_elements > 0 {
                    tensor.shape += base_rel;
                    tensor.data += base_rel;
                }
            }
            _ => {} // Fixed-width primitives: no relptrs to adjust
        }
    }
    Ok(())
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
}
