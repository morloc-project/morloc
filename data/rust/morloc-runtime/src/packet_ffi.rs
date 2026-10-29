//! C ABI wrappers for packet functions.

use std::ffi::{c_char, c_void, CStr};
use std::ptr;

use crate::cschema::CSchema;
use crate::error::{clear_errmsg, set_errmsg, MorlocError};
use crate::packet::*;
use crate::shm::{self, AbsPtr, RelPtr};

// ── morloc_call_t ────────────────────────────────────────────────────────────

/// Matches C `morloc_call_t` layout.
#[repr(C)]
pub struct MorlocCall {
    pub midx: u32,
    pub args: *mut *mut u8,
    pub nargs: usize,
    pub owns_args: i32,
}

// ── Header reading ───────────────────────────────────────────────────────────

pub(crate) unsafe fn read_morloc_packet_header(
    msg: *const u8,
    errmsg: *mut *mut c_char,
) -> *mut PacketHeader {
    clear_errmsg(errmsg);
    if msg.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("Cannot make packet from NULL pointer".into()));
        return ptr::null_mut();
    }
    let header = &*(msg as *const PacketHeader);
    if !header.is_valid() {
        set_errmsg(errmsg, &MorlocError::Packet("Malformed morloc packet".into()));
        return ptr::null_mut();
    }
    let length = header.length;
    if length > (isize::MAX as u64) - 32 - u64::from(u32::MAX) {
        set_errmsg(errmsg, &MorlocError::Packet(format!(
            "a packet header claims a {length}-byte payload, more than any packet can hold"
        )));
        return ptr::null_mut();
    }
    msg as *mut PacketHeader
}

pub(crate) unsafe fn packet_is_ping(
    packet: *const u8,
    errmsg: *mut *mut c_char,
) -> bool {
    clear_errmsg(errmsg);
    let header = read_morloc_packet_header(packet, errmsg);
    if header.is_null() { return false; }
    (*header).is_ping()
}

pub(crate) unsafe fn packet_is_local_call(
    packet: *const u8,
    errmsg: *mut *mut c_char,
) -> bool {
    clear_errmsg(errmsg);
    let header = read_morloc_packet_header(packet, errmsg);
    if header.is_null() { return false; }
    (*header).is_local_call()
}

pub(crate) unsafe fn packet_is_remote_call(
    packet: *const u8,
    errmsg: *mut *mut c_char,
) -> bool {
    clear_errmsg(errmsg);
    let header = read_morloc_packet_header(packet, errmsg);
    if header.is_null() { return false; }
    (*header).is_remote_call()
}

// ── Packet size ──────────────────────────────────────────────────────────────

pub(crate) unsafe fn morloc_packet_size_from_header(
    header: *const PacketHeader,
) -> usize {
    if header.is_null() { return 0; }
    32 + (*header).offset as usize + (*header).length as usize
}

pub(crate) unsafe fn morloc_packet_size(
    packet: *const u8,
    errmsg: *mut *mut c_char,
) -> usize {
    clear_errmsg(errmsg);
    let header = read_morloc_packet_header(packet, errmsg);
    if header.is_null() { return 0; }
    morloc_packet_size_from_header(header)
}

// ── Duplicate ────────────────────────────────────────────────────────────────

/// Finish with a reply received over a socket: give back the shared-memory
/// reference its sender donated, if it names a block, and free the packet.
pub(crate) unsafe fn release_received_packet(packet: *mut u8) {
    if packet.is_null() {
        return;
    }
    if let Ok(Some(block)) = packet_rptr_block(packet) {
        let _ = shm::shfree(block);
    }
    libc::free(packet as *mut c_void);
}

/// A copy of `packet` a pool can return as its own result when the result
/// is one of its arguments, unchanged. A packet naming a shared-memory
/// block gets a reference of its own, and `block_out` receives the block
/// for the pool's tracker (null when the packet names none).
pub(crate) unsafe fn morloc_dup_packet(
    packet: *const u8,
    block_out: *mut *mut c_void,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);
    if !block_out.is_null() {
        *block_out = ptr::null_mut();
    }
    let header = read_morloc_packet_header(packet, errmsg);
    if header.is_null() {
        return ptr::null_mut();
    }
    let size = morloc_packet_size_from_header(header);
    let source = (*header).command.data.source;
    if source != PACKET_SOURCE_MESG && source != PACKET_SOURCE_FILE && source != PACKET_SOURCE_RPTR {
        set_errmsg(errmsg, &MorlocError::Packet(format!(
            "cannot return an argument packet of unknown source 0x{source:02x} unchanged"
        )));
        return ptr::null_mut();
    }
    let block = match packet_rptr_block(packet)
        .and_then(|b| b.map_or(Ok(ptr::null_mut()), |b| shm::shincref(b).map(|()| b)))
    {
        Ok(b) => b,
        Err(e) => {
            set_errmsg(errmsg, &e);
            return ptr::null_mut();
        }
    };
    let copy = libc::malloc(size) as *mut u8;
    if copy.is_null() {
        if !block.is_null() {
            let _ = shm::release(block);
        }
        set_errmsg(errmsg, &MorlocError::Other("morloc_dup_packet: out of memory".into()));
        return ptr::null_mut();
    }
    ptr::copy_nonoverlapping(packet, copy, size);
    if !block_out.is_null() {
        *block_out = block as *mut c_void;
    }
    copy
}

// ── Ping ─────────────────────────────────────────────────────────────────────

pub(crate) unsafe fn return_ping(
    packet: *const u8,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);
    if !packet_is_ping(packet, errmsg) {
        if (*errmsg).is_null() {
            set_errmsg(errmsg, &MorlocError::Packet("Not a ping packet".into()));
        }
        return ptr::null_mut();
    }
    let size = morloc_packet_size(packet, errmsg);
    if size == 0 { return ptr::null_mut(); }
    let buf = libc::calloc(size, 1) as *mut u8;
    if buf.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("Failed to allocate ping response".into()));
        return ptr::null_mut();
    }
    ptr::copy_nonoverlapping(packet, buf, size);
    // NET-7: the echo must not claim the callee keeps the connection; a
    // callee that keeps sets the bit on its reply itself.
    crate::ipc_ffi::take_keep_bit(buf);
    buf
}

// ── Data packet construction helpers ─────────────────────────────────────────

/// Build metadata section: metadata header + schema string, padded to 32-byte boundary.
/// Returns (metadata_buffer, padded_length). Returns (null, 0) if schema is null.
///
/// # Safety
/// `schema` must be null or a valid CSchema pointer.
unsafe fn build_schema_metadata(schema: *const CSchema) -> (*mut u8, usize) {
    if schema.is_null() {
        return (ptr::null_mut(), 0);
    }
    let rs = CSchema::to_rust(schema);
    let schema_str = crate::schema::schema_to_string(&rs);
    let schema_bytes = schema_str.as_bytes();
    let schema_len = schema_bytes.len() + 1; // +1 for null terminator
    let meta_header_size = 8; // sizeof(morloc_metadata_header_t)
    let raw_meta_len = meta_header_size + schema_len;
    let padded_meta_len = ((raw_meta_len + 31) / 32) * 32;

    let metadata = libc::calloc(padded_meta_len, 1) as *mut u8;
    if metadata.is_null() {
        return (ptr::null_mut(), 0);
    }

    *metadata = b'm';
    *metadata.add(1) = b'm';
    *metadata.add(2) = b'h';
    *metadata.add(3) = METADATA_TYPE_SCHEMA_STRING;
    *(metadata.add(4) as *mut u32) = schema_len as u32;

    ptr::copy_nonoverlapping(schema_bytes.as_ptr(), metadata.add(meta_header_size), schema_bytes.len());
    // Null terminator already zeroed by calloc

    (metadata, padded_meta_len)
}

/// Generic data packet builder.
///
/// # Safety
/// If non-null, `data` must point to `data_length` readable bytes.
/// If non-null, `metadata` must point to `metadata_length` readable bytes.
unsafe fn make_data_packet_raw(
    data: *const u8,
    data_length: usize,
    metadata: *const u8,
    metadata_length: usize,
    src: u8,
    fmt: u8,
    cmpr: u8,
    encr: u8,
    status: u8,
) -> *mut u8 {
    let total = 32 + metadata_length + data_length;
    let packet = libc::calloc(total, 1) as *mut u8;
    if packet.is_null() { return ptr::null_mut(); }

    // Build command
    let cmd = CommandData {
        cmd_type: PACKET_TYPE_DATA,
        source: src,
        format: fmt,
        compression: cmpr,
        encryption: encr,
        status,
        generation: [0; 2],
    };
    let header = PacketHeader {
        magic: PACKET_MAGIC,
        plain: THIS_PLAIN,
        version: THIS_VERSION,
        flavor: DEFAULT_FLAVOR,
        mode: DEFAULT_MODE,
        command: PacketCommand { data: cmd },
        offset: metadata_length as u32,
        length: data_length as u64,
    };
    let hdr_bytes = header.to_bytes();
    ptr::copy_nonoverlapping(hdr_bytes.as_ptr(), packet, 32);

    if !metadata.is_null() && metadata_length > 0 {
        ptr::copy_nonoverlapping(metadata, packet.add(32), metadata_length);
    }
    if !data.is_null() && data_length > 0 {
        ptr::copy_nonoverlapping(data, packet.add(32 + metadata_length), data_length);
    }

    packet
}

/// Generic data packet with schema metadata.
unsafe fn make_data_packet_with_schema(
    data: *const u8,
    data_length: usize,
    schema: *const CSchema,
    src: u8,
    fmt: u8,
    cmpr: u8,
    encr: u8,
    status: u8,
) -> *mut u8 {
    let (metadata, metadata_length) = build_schema_metadata(schema);
    let result = make_data_packet_raw(
        data, data_length, metadata, metadata_length, src, fmt, cmpr, encr, status,
    );
    if !metadata.is_null() {
        libc::free(metadata as *mut c_void);
    }
    result
}

// ── Standard data packet (RPTR + VOIDSTAR) ──────────────────────────────────

pub(crate) unsafe fn make_standard_data_packet(
    relptr: RelPtr,
    schema: *const CSchema,
) -> *mut u8 {
    let packet = make_data_packet_with_schema(
        ptr::null(),
        std::mem::size_of::<RelPtr>(),
        schema,
        PACKET_SOURCE_RPTR,
        PACKET_FORMAT_VOIDSTAR,
        PACKET_COMPRESSION_NONE,
        PACKET_ENCRYPTION_NONE,
        PACKET_STATUS_PASS,
    );
    if packet.is_null() { return ptr::null_mut(); }

    let header = &*(packet as *const PacketHeader);
    let payload_offset = 32 + header.offset as usize;
    *(packet.add(payload_offset) as *mut RelPtr) = relptr;
    stamp_generation(packet, relptr);

    packet
}

pub(crate) unsafe fn make_arrow_data_packet(
    relptr: RelPtr,
    schema: *const CSchema,
) -> *mut u8 {
    let packet = make_data_packet_with_schema(
        ptr::null(),
        std::mem::size_of::<RelPtr>(),
        schema,
        PACKET_SOURCE_RPTR,
        PACKET_FORMAT_ARROW,
        PACKET_COMPRESSION_NONE,
        PACKET_ENCRYPTION_NONE,
        PACKET_STATUS_PASS,
    );
    if packet.is_null() { return ptr::null_mut(); }

    let header = &*(packet as *const PacketHeader);
    let payload_offset = 32 + header.offset as usize;
    *(packet.add(payload_offset) as *mut RelPtr) = relptr;
    stamp_generation(packet, relptr);

    packet
}

// SHM-13: the packet names the allocation its block holds now.
unsafe fn stamp_generation(packet: *mut u8, relptr: RelPtr) {
    if let Some(g) = shm::rel2abs(relptr).ok().and_then(|abs| shm::block_generation(abs)) {
        ptr::copy_nonoverlapping(g.to_le_bytes().as_ptr(), packet.add(PKT_GENERATION_OFF), 2);
    }
}

// SHM-13: the block a reference packet names, provided it is still the
// allocation the packet was made for. Otherwise the packet is disarmed, so
// that no release through it reaches the block's new owner.
unsafe fn checked_rptr_block(packet: *const u8, payload: *const u8) -> Result<AbsPtr, MorlocError> {
    let relptr = *(payload as *const RelPtr);
    let abs = shm::rel2abs(relptr)?;
    let stamp = u16::from_le_bytes([*packet.add(PKT_GENERATION_OFF), *packet.add(PKT_GENERATION_OFF + 1)]);
    let found = shm::block_generation(abs);
    if stamp == 0 || found == Some(stamp) {
        return Ok(abs);
    }
    *(payload as *mut RelPtr) = shm::RELNULL;
    Err(MorlocError::Shm(format!(
        "a reference packet names a shared-memory block that was released and reused \
         (generation {stamp} expected, {found:?} found)"
    )))
}

// ── Msgpack packets ──────────────────────────────────────────────────────────

pub(crate) unsafe fn make_mpk_data_packet(
    mpk_filename: *const c_char,
    schema: *const CSchema,
) -> *mut u8 {
    if mpk_filename.is_null() { return ptr::null_mut(); }
    let filename = CStr::from_ptr(mpk_filename);
    let bytes = filename.to_bytes();
    make_data_packet_with_schema(
        bytes.as_ptr(),
        bytes.len(),
        schema,
        PACKET_SOURCE_FILE,
        PACKET_FORMAT_MSGPACK,
        PACKET_COMPRESSION_NONE,
        PACKET_ENCRYPTION_NONE,
        PACKET_STATUS_PASS,
    )
}

/// FILE-source indirection whose referenced file is itself a morloc
/// data packet (`PACKET_FORMAT_DATA`). The outer packet's `compression`
/// byte refers to the filename bytes (always NONE); the referenced
/// file carries its own header and compression byte.
pub(crate) unsafe fn make_data_indirection_packet(
    dat_filename: *const c_char,
    schema: *const CSchema,
) -> *mut u8 {
    if dat_filename.is_null() { return ptr::null_mut(); }
    let filename = CStr::from_ptr(dat_filename);
    let bytes = filename.to_bytes();
    make_data_packet_with_schema(
        bytes.as_ptr(),
        bytes.len(),
        schema,
        PACKET_SOURCE_FILE,
        PACKET_FORMAT_DATA,
        PACKET_COMPRESSION_NONE,
        PACKET_ENCRYPTION_NONE,
        PACKET_STATUS_PASS,
    )
}

/// A table argument that names its file; the receiving pool reads it.
pub(crate) unsafe fn make_table_file_packet(
    path: *const c_char,
    schema: *const CSchema,
) -> *mut u8 {
    if path.is_null() { return ptr::null_mut(); }
    let bytes = CStr::from_ptr(path).to_bytes();
    make_data_packet_with_schema(
        bytes.as_ptr(),
        bytes.len(),
        schema,
        PACKET_SOURCE_FILE,
        PACKET_FORMAT_ARROW,
        PACKET_COMPRESSION_NONE,
        PACKET_ENCRYPTION_NONE,
        PACKET_STATUS_PASS,
    )
}

pub(crate) unsafe fn make_data_packet_from_mpk(
    mpk: *const c_char,
    mpk_size: usize,
    schema: *const CSchema,
) -> *mut u8 {
    make_data_packet_with_schema(
        mpk as *const u8,
        mpk_size,
        schema,
        PACKET_SOURCE_MESG,
        PACKET_FORMAT_MSGPACK,
        PACKET_COMPRESSION_NONE,
        PACKET_ENCRYPTION_NONE,
        PACKET_STATUS_PASS,
    )
}

// ── Schema from metadata ─────────────────────────────────────────────────────

pub(crate) unsafe fn read_schema_from_packet_meta(
    packet: *const u8,
    errmsg: *mut *mut c_char,
) -> *mut c_char {
    clear_errmsg(errmsg);
    let header = read_morloc_packet_header(packet, errmsg);
    if header.is_null() { return ptr::null_mut(); }

    let offset = (*header).offset as usize;
    if offset < 8 { return ptr::null_mut(); } // no room for metadata header

    let meta_start = 32usize;
    let meta_end = meta_start + offset;
    let mut pos = meta_start;
    while pos + 8 <= meta_end {
        if *packet.add(pos) == b'm' && *packet.add(pos + 1) == b'm' && *packet.add(pos + 2) == b'h' {
            let meta_type = *packet.add(pos + 3);
            let meta_size = *(packet.add(pos + 4) as *const u32) as usize;
            if meta_type == METADATA_TYPE_SCHEMA_STRING {
                return packet.add(pos + 8) as *mut c_char;
            }
            pos += 8 + meta_size;
        } else {
            break;
        }
    }
    ptr::null_mut()
}

// ── Fail packet ──────────────────────────────────────────────────────────────

pub(crate) unsafe fn make_fail_packet(
    failure_message: *const c_char,
) -> *mut u8 {
    if failure_message.is_null() { return ptr::null_mut(); }
    make_status_packet(CStr::from_ptr(failure_message).to_bytes(), PACKET_STATUS_FAIL)
}

/// A fail packet saying the call ended because its downstream reader
/// closed the pipe; a caller re-raises it rather than handing it to `@try`.
pub(crate) unsafe fn make_pipe_closed_packet(
    failure_message: *const c_char,
) -> *mut u8 {
    if failure_message.is_null() { return ptr::null_mut(); }
    make_status_packet(
        CStr::from_ptr(failure_message).to_bytes(),
        morloc_runtime_types::packet::PACKET_STATUS_PIPE_CLOSED,
    )
}

/// A fail packet saying a pool the call depended on ended (LIFE-5).
pub(crate) fn make_peer_died_packet(failure_message: &str) -> *mut u8 {
    make_status_packet(failure_message.as_bytes(), morloc_runtime_types::packet::PACKET_STATUS_PEER_DIED)
}

/// A text packet carrying `msg` with failure `status`.
fn make_status_packet(msg: &[u8], status: u8) -> *mut u8 {
    unsafe {
        make_data_packet_raw(
            msg.as_ptr(),
            msg.len(),
            ptr::null(),
            0,
            PACKET_SOURCE_MESG,
            PACKET_FORMAT_TEXT,
            PACKET_COMPRESSION_NONE,
            PACKET_ENCRYPTION_NONE,
            status,
        )
    }
}

pub(crate) unsafe fn morloc_packet_is_pipe_closed(packet: *const u8) -> bool {
    if packet.is_null() { return false; }
    let mut err: *mut c_char = ptr::null_mut();
    let header = read_morloc_packet_header(packet, &mut err);
    if !err.is_null() { libc::free(err as *mut c_void); }
    !header.is_null() && (*header).is_pipe_closed()
}

// ── Error message extraction ─────────────────────────────────────────────────

pub(crate) unsafe fn get_morloc_data_packet_error_message(
    data: *const u8,
    errmsg: *mut *mut c_char,
) -> *mut c_char {
    clear_errmsg(errmsg);
    let header = read_morloc_packet_header(data, errmsg);
    if header.is_null() { return ptr::null_mut(); }

    if (*header).is_fail() {
        let payload_start = 32 + (*header).offset as usize;
        let payload_len = (*header).length as usize;
        let buf = libc::calloc(payload_len + 1, 1) as *mut c_char;
        if buf.is_null() {
            set_errmsg(errmsg, &MorlocError::Other("Failed to allocate error message".into()));
            return ptr::null_mut();
        }
        ptr::copy_nonoverlapping(data.add(payload_start), buf as *mut u8, payload_len);
        return buf;
    }

    ptr::null_mut()
}

// ── get_morloc_data_packet_value ─────────────────────────────────────────────

pub(crate) unsafe fn get_morloc_data_packet_value(
    data: *const u8,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);
    let header = read_morloc_packet_header(data, errmsg);
    if header.is_null() { return ptr::null_mut(); }

    if (*header).command_type() != PACKET_TYPE_DATA {
        set_errmsg(errmsg, &MorlocError::Packet("Expected a data packet".into()));
        return ptr::null_mut();
    }

    let packet_error = get_morloc_data_packet_error_message(data, errmsg);
    if !packet_error.is_null() {
        let err_str = CStr::from_ptr(packet_error).to_string_lossy().into_owned();
        libc::free(packet_error as *mut c_void);
        set_errmsg(errmsg, &MorlocError::Packet(format!("\n{}", err_str)));
        return ptr::null_mut();
    }
    clear_errmsg(errmsg);

    let compression = (*header).command.data.compression;
    if compression == morloc_runtime_types::packet::PACKET_COMPRESSION_ZSTD {
        let packet_size = morloc_packet_size_from_header(header);
        let packet_bytes = std::slice::from_raw_parts(data, packet_size);
        let decompressed = match morloc_runtime_types::compression::decompress_packet(packet_bytes) {
            Ok(v) => v,
            Err(e) => { set_errmsg(errmsg, &e); return ptr::null_mut(); }
        };
        return get_morloc_data_packet_value(decompressed.as_ptr(), schema, errmsg);
    }

    let rs = CSchema::to_rust(schema);
    let source = (*header).command.data.source;
    let format = (*header).command.data.format;
    let payload_start = 32 + (*header).offset as usize;
    let payload_len = (*header).length as usize;

    match source {
        PACKET_SOURCE_MESG => {
            if format == PACKET_FORMAT_MSGPACK {
                let payload = std::slice::from_raw_parts(data.add(payload_start), payload_len);
                match crate::mpack::unpack_with_schema(payload, &rs) {
                    Ok(abs) => abs,
                    Err(e) => { set_errmsg(errmsg, &e); ptr::null_mut() }
                }
            } else if format == PACKET_FORMAT_VOIDSTAR {
                let payload = std::slice::from_raw_parts(data.add(payload_start), payload_len);
                match crate::voidstar::read_binary(payload, &rs) {
                    Ok(abs) => abs,
                    Err(e) => { set_errmsg(errmsg, &e); ptr::null_mut() }
                }
            } else if format == PACKET_FORMAT_ARROW {
                // A table written out as a self-contained packet carries
                // Arrow IPC bytes.
                crate::arrow_ipc_reader::ipc_payload_to_block(
                    data.add(payload_start), payload_len, schema, errmsg,
                ) as *mut u8
            } else {
                set_errmsg(errmsg, &MorlocError::Packet(
                    format!("Invalid format from mesg: 0x{:02x}", format)
                ));
                ptr::null_mut()
            }
        }
        PACKET_SOURCE_FILE => {
            if payload_len > 4096 {
                set_errmsg(errmsg, &MorlocError::Packet(format!(
                    "a file packet's path is {payload_len} bytes; at most 4096 are read"
                )));
                return ptr::null_mut();
            }
            let filename_bytes = std::slice::from_raw_parts(data.add(payload_start), payload_len);
            let Ok(filename) = std::str::from_utf8(filename_bytes) else {
                set_errmsg(errmsg, &MorlocError::Packet("a file packet's path is not UTF-8".into()));
                return ptr::null_mut();
            };
            let filename = filename.trim_end_matches('\0');
            if format == PACKET_FORMAT_MSGPACK {
                match std::fs::read(filename) {
                    Ok(file_data) => {
                        match crate::mpack::unpack_with_schema(&file_data, &rs) {
                            Ok(abs) => abs,
                            Err(e) => { set_errmsg(errmsg, &e); ptr::null_mut() }
                        }
                    }
                    Err(e) => {
                        set_errmsg(errmsg, &MorlocError::Io(e));
                        ptr::null_mut()
                    }
                }
            } else if format == PACKET_FORMAT_ARROW {
                let path_cstr = match std::ffi::CString::new(filename) {
                    Ok(c) => c,
                    Err(_) => {
                        set_errmsg(errmsg, &MorlocError::Packet(
                            "FILE+ARROW payload has embedded NUL".into()
                        ));
                        return ptr::null_mut();
                    }
                };
                let rel = crate::arrow_ipc_reader::read_table_file_to_shm(
                    path_cstr.as_ptr(), schema, errmsg,
                );
                if rel == shm::RELNULL {
                    return ptr::null_mut();
                }
                match shm::rel2abs(rel) {
                    Ok(abs) => abs,
                    Err(e) => { set_errmsg(errmsg, &e); ptr::null_mut() }
                }
            } else if format == PACKET_FORMAT_DATA {
                // The file at `filename` is a complete morloc data packet.
                let filename_cstr = match std::ffi::CString::new(filename) {
                    Ok(c) => c,
                    Err(_) => {
                        set_errmsg(errmsg, &MorlocError::Packet(
                            "FILE+DATA payload has embedded NUL".into()
                        ));
                        return ptr::null_mut();
                    }
                };
                match crate::cli::load_packet_file(filename_cstr.as_ptr(), schema) {
                    Ok(p) => p as *mut u8,
                    Err(e) => { set_errmsg(errmsg, &e); ptr::null_mut() }
                }
            } else {
                set_errmsg(errmsg, &MorlocError::Packet(
                    format!("Invalid format from file: 0x{:02x}", format)
                ));
                ptr::null_mut()
            }
        }
        PACKET_SOURCE_RPTR => {
            if payload_len < 8 {
                set_errmsg(errmsg, &MorlocError::Packet(format!(
                    "a shared-memory reference packet carries {payload_len} payload bytes, not 8"
                )));
                ptr::null_mut()
            } else if format == PACKET_FORMAT_VOIDSTAR || format == PACKET_FORMAT_ARROW {
                match checked_rptr_block(data, data.add(payload_start)) {
                    Ok(abs) => abs,
                    Err(e) => { set_errmsg(errmsg, &e); ptr::null_mut() }
                }
            } else {
                set_errmsg(errmsg, &MorlocError::Packet(
                    format!("For RPTR source, expected voidstar or arrow format, found: 0x{:02x}", format)
                ));
                ptr::null_mut()
            }
        }
        _ => {
            set_errmsg(errmsg, &MorlocError::Packet("Invalid source".into()));
            ptr::null_mut()
        }
    }
}

// ── Call packet construction ─────────────────────────────────────────────────

/// Take a reference on the shared block a data packet names, held on
/// behalf of whoever receives the packet. A no-op for packets whose
/// payload travels inside them. Fails when the block can no longer be
/// referenced.
pub(crate) unsafe fn donate_packet_reference(
    packet: *const u8,
) -> Result<(), MorlocError> {
    match packet_rptr_block(packet)? {
        Some(abs) => {
            crate::shm::shincref(abs)?;
            crate::shm::hand_on_reference();
            Ok(())
        }
        None => Ok(()),
    }
}

/// Count a reply's donated reference as this process's own (SHM-8).
pub(crate) unsafe fn inherit_reply(packet: *const u8) {
    if let Ok(Some(_)) = packet_rptr_block(packet) {
        crate::shm::take_on_reference();
    }
}

pub(crate) unsafe fn morloc_inherit_reply(packet: *const u8) {
    inherit_reply(packet)
}

/// Give back a reference taken by `donate_packet_reference` when the packet
/// it was taken for never reached anyone.
pub(crate) unsafe fn revoke_packet_reference(packet: *const u8) {
    if let Ok(Some(abs)) = packet_rptr_block(packet) {
        crate::shm::take_on_reference();
        let _ = crate::shm::release(abs);
    }
}

/// The shared-memory block a data packet names by reference, or `None` for
/// a packet whose payload travels inside it.
pub(crate) unsafe fn packet_rptr_block(packet: *const u8) -> Result<Option<AbsPtr>, MorlocError> {
    if packet.is_null() {
        return Ok(None);
    }
    let header = packet as *const PacketHeader;
    if (*header).command_type() != PACKET_TYPE_DATA
        || (*header).command.data.source != PACKET_SOURCE_RPTR
        || ((*header).length as usize) < std::mem::size_of::<RelPtr>()
    {
        return Ok(None);
    }
    checked_rptr_block(packet, packet.add(32 + (*header).offset as usize)).map(Some)
}

unsafe fn make_call_packet_gen(
    midx: u32,
    entrypoint: u8,
    arg_packets: *const *const u8,
    nargs: usize,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);

    let mut data_length: usize = 0;
    for i in 0..nargs {
        let arg = read_morloc_packet_header(*arg_packets.add(i), errmsg);
        if arg.is_null() { return ptr::null_mut(); }
        data_length += morloc_packet_size_from_header(arg);
    }

    let total = 32 + data_length;
    let packet = libc::calloc(total, 1) as *mut u8;
    if packet.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("Failed to allocate call packet".into()));
        return ptr::null_mut();
    }

    let cmd = CommandCall {
        cmd_type: PACKET_TYPE_CALL,
        entrypoint,
        padding: [0; 2],
        midx,
    };
    let header = PacketHeader {
        magic: PACKET_MAGIC,
        plain: THIS_PLAIN,
        version: THIS_VERSION,
        flavor: DEFAULT_FLAVOR,
        mode: DEFAULT_MODE,
        command: PacketCommand { call: cmd },
        offset: 0,
        length: data_length as u64,
    };
    let hdr_bytes = header.to_bytes();
    ptr::copy_nonoverlapping(hdr_bytes.as_ptr(), packet, 32);

    let mut pos = 32;
    for i in 0..nargs {
        let arg = read_morloc_packet_header(*arg_packets.add(i), errmsg);
        if arg.is_null() {
            libc::free(packet as *mut c_void);
            return ptr::null_mut();
        }
        let arg_size = morloc_packet_size_from_header(arg);
        ptr::copy_nonoverlapping(*arg_packets.add(i), packet.add(pos), arg_size);
        pos += arg_size;
    }

    packet
}

pub(crate) unsafe fn make_morloc_local_call_packet(
    midx: u32,
    arg_packets: *const *const u8,
    nargs: usize,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    make_call_packet_gen(midx, PACKET_ENTRYPOINT_LOCAL, arg_packets, nargs, errmsg)
}

pub(crate) unsafe fn make_morloc_remote_call_packet(
    midx: u32,
    arg_packets: *const *const u8,
    nargs: usize,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    make_call_packet_gen(midx, PACKET_ENTRYPOINT_REMOTE_SFS, arg_packets, nargs, errmsg)
}

// ── Call packet reading ──────────────────────────────────────────────────────

pub(crate) unsafe fn read_morloc_call_packet(
    packet: *const u8,
    errmsg: *mut *mut c_char,
) -> *mut MorlocCall {
    clear_errmsg(errmsg);

    let call = libc::calloc(1, std::mem::size_of::<MorlocCall>()) as *mut MorlocCall;
    if call.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("calloc failed".into()));
        return ptr::null_mut();
    }

    let header = read_morloc_packet_header(packet, errmsg);
    if header.is_null() {
        libc::free(call as *mut c_void);
        return ptr::null_mut();
    }
    if (*header).command_type() != PACKET_TYPE_CALL {
        set_errmsg(errmsg, &MorlocError::Packet("Expected packet to be a call".into()));
        libc::free(call as *mut c_void);
        return ptr::null_mut();
    }

    (*call).midx = (*header).command.call.midx;
    (*call).nargs = 0;
    (*call).args = ptr::null_mut();
    (*call).owns_args = 0; // borrowing pointers into packet

    let start_pos = 32 + (*header).offset as usize;
    let end_pos = morloc_packet_size_from_header(header);

    let mut pos = start_pos;
    while pos < end_pos {
        let arg_size = if end_pos - pos < 32 {
            0
        } else {
            morloc_packet_size(packet.add(pos), errmsg)
        };
        if arg_size == 0 || arg_size > end_pos - pos {
            if !errmsg.is_null() && (*errmsg).is_null() {
                set_errmsg(errmsg, &MorlocError::Packet(format!(
                    "argument #{} of a call packet runs past the packet's end", (*call).nargs
                )));
            }
            free_morloc_call(call);
            return ptr::null_mut();
        }
        pos += arg_size;
        (*call).nargs += 1;
    }

    (*call).args = libc::calloc((*call).nargs, std::mem::size_of::<*mut u8>()) as *mut *mut u8;
    if (*call).args.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("calloc failed for args".into()));
        libc::free(call as *mut c_void);
        return ptr::null_mut();
    }

    pos = start_pos;
    for i in 0..(*call).nargs {
        let arg_header = read_morloc_packet_header(packet.add(pos), errmsg);
        if arg_header.is_null() {
            free_morloc_call(call);
            return ptr::null_mut();
        }
        if (*arg_header).command_type() != PACKET_TYPE_DATA {
            set_errmsg(errmsg, &MorlocError::Packet(
                format!("Argument #{} is not a DATA packet (type={})", i, (*arg_header).command_type())
            ));
            free_morloc_call(call);
            return ptr::null_mut();
        }
        *(*call).args.add(i) = packet.add(pos) as *mut u8;
        pos += morloc_packet_size_from_header(arg_header);
    }

    call
}

pub(crate) unsafe fn free_morloc_call(call: *mut MorlocCall) {
    if call.is_null() { return; }
    let c = &*call;
    if !c.args.is_null() {
        if c.owns_args != 0 {
            for i in 0..c.nargs {
                let arg = *c.args.add(i);
                if !arg.is_null() {
                    libc::free(arg as *mut c_void);
                }
            }
        }
        libc::free(c.args as *mut c_void);
    }
    libc::free(call as *mut c_void);
}

// ── flatten_voidstar_to_buffer ───────────────────────────────────────────────

pub(crate) unsafe fn flatten_voidstar_to_buffer(
    data: *const c_void,
    schema: *const CSchema,
    out_buf: *mut *mut u8,
    out_size: *mut usize,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    *out_buf = ptr::null_mut();
    *out_size = 0;

    let rs = CSchema::to_rust(schema);
    match crate::voidstar::flatten_to_buffer(data as AbsPtr, &rs) {
        Ok(buf) => {
            let len = buf.len();
            let c_buf = libc::malloc(len) as *mut u8;
            if c_buf.is_null() {
                set_errmsg(errmsg, &MorlocError::Other("malloc failed".into()));
                return 1;
            }
            ptr::copy_nonoverlapping(buf.as_ptr(), c_buf, len);
            *out_buf = c_buf;
            *out_size = len;
            0
        }
        Err(e) => {
            set_errmsg(errmsg, &e);
            1
        }
    }
}

// ── make_data_packet_auto ────────────────────────────────────────────────────

pub(crate) unsafe fn make_data_packet_auto(
    voidstar: *mut c_void,
    relptr: RelPtr,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    make_data_packet_with(voidstar, relptr, schema, false, errmsg)
}

/// [`make_data_packet_auto`]; when `whole`, a value sent by reference that
/// spans several blocks is first copied into one (SHM-15).
pub(crate) unsafe fn make_data_packet_with(
    voidstar: *mut c_void,
    relptr: RelPtr,
    schema: *const CSchema,
    whole: bool,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);
    let rs = CSchema::to_rust(schema);

    let threshold = crate::packet::inline_threshold();
    let t_sz = crate::log::trace_enabled().then(std::time::Instant::now);
    // Capped at threshold + 1: only whether the value fits inline matters.
    let flat_size = match crate::ffi::calc_voidstar_size_bounded(
        voidstar as *const u8,
        &rs,
        threshold.saturating_add(1),
    ) {
        Ok(s) => s,
        Err(e) => {
            set_errmsg(errmsg, &e);
            return ptr::null_mut();
        }
    };
    crate::morloc_trace!(
        "[make_data_packet_auto] calc_voidstar_size_bounded ({} KiB cap) = {} bytes in {:.2?}",
        threshold / 1024,
        flat_size,
        t_sz.map(|t| t.elapsed()).unwrap_or_default()
    );

    if flat_size <= threshold {
        return make_inline_data_packet(voidstar, schema, errmsg);
    }

    if crate::packet::shm_enabled() {
        if whole && !crate::voidstar::within_its_block(voidstar as crate::shm::AbsPtr, &rs) {
            return match crate::voidstar::deep_copy_to_block(voidstar as *const u8, &rs)
                .and_then(|copy| crate::shm::abs2rel(copy))
            {
                Ok(rel) => make_standard_data_packet(rel, schema),
                Err(e) => { set_errmsg(errmsg, &e); ptr::null_mut() }
            };
        }
        make_standard_data_packet(relptr, schema)
    } else {
        make_file_data_packet_voidstar(voidstar, schema, &rs, errmsg)
    }
}

/// A packet that carries the value's flat form inside it, whatever its
/// size. A table's flat form is its block.
pub(crate) unsafe fn make_inline_data_packet(
    voidstar: *mut c_void,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);
    let rs = CSchema::to_rust(schema);
    match crate::voidstar::flatten_to_buffer(voidstar as AbsPtr, &rs) {
        Ok(blob) => {
            let packet = make_data_packet_with_schema(
                blob.as_ptr(),
                blob.len(),
                schema,
                PACKET_SOURCE_MESG,
                PACKET_FORMAT_VOIDSTAR,
                PACKET_COMPRESSION_NONE,
                PACKET_ENCRYPTION_NONE,
                PACKET_STATUS_PASS,
            );
            if packet.is_null() {
                set_errmsg(errmsg, &MorlocError::Other("Failed to create inline data packet".into()));
            }
            packet
        }
        Err(e) => {
            set_errmsg(errmsg, &e);
            ptr::null_mut()
        }
    }
}

// ── normalize_data_packet_for_output ─────────────────────────────────────────
//
// Produce a self-contained packet for writing to disk: an RPTR or FILE
// payload is inlined (source MESG) and, when `compression_level > 0` and
// the packet is uncompressed, zstd-compressed. An already-ZSTD payload
// is not recompressed: its level is not recoverable from the frame
// header. FAIL and non-DATA packets pass through.
pub(crate) unsafe fn normalize_data_packet_for_output(
    packet: *const u8,
    packet_size: usize,
    compression_level: u8,
    out_buf: *mut *mut u8,
    out_size: *mut usize,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    *out_buf = ptr::null_mut();
    *out_size = 0;

    let header = read_morloc_packet_header(packet, errmsg);
    if header.is_null() {
        return 1;
    }

    if !(*header).is_data() || (*header).is_fail() {
        return alloc_out(packet, packet_size, out_buf, out_size, errmsg);
    }

    let source = (*header).command.data.source;
    let format = (*header).command.data.format;
    let compression = (*header).command.data.compression;
    let metadata_size = (*header).offset as usize;
    let payload_start = 32 + metadata_size;
    let payload_len = (*header).length as usize;

    if payload_start + payload_len > packet_size {
        set_errmsg(errmsg, &MorlocError::Packet(
            "packet payload extends past packet bounds".into()
        ));
        return 1;
    }

    let materialized: Option<Vec<u8>> = match source {
        PACKET_SOURCE_MESG => None,
        PACKET_SOURCE_RPTR => {
            match read_rptr_payload(packet, payload_start, format, errmsg) {
                Some(v) => Some(v),
                None => return 1,
            }
        }
        PACKET_SOURCE_FILE => {
            match read_file_payload(packet, payload_start, payload_len, format, errmsg) {
                Some(v) => Some(v),
                None => return 1,
            }
        }
        _ => {
            set_errmsg(errmsg, &MorlocError::Packet(format!(
                "unsupported packet source 0x{:02x}", source
            )));
            return 1;
        }
    };
    let raw_payload: &[u8] = match materialized.as_deref() {
        Some(slice) => slice,
        None => std::slice::from_raw_parts(packet.add(payload_start), payload_len),
    };

    let compressed: Option<(Vec<u8>, Vec<crate::packet::FrameEntry>)> =
        if compression_level > 0 && compression == PACKET_COMPRESSION_NONE {
            let lvl = match crate::compression::CompressionLevel::from_u8(compression_level) {
                Ok(l) => l,
                Err(e) => { set_errmsg(errmsg, &e); return 1; }
            };
            if lvl.is_none() {
                None
            } else {
                match crate::compression::compress_payload_zstd(raw_payload, lvl) {
                    Ok(v) => Some(v),
                    Err(e) => { set_errmsg(errmsg, &e); return 1; }
                }
            }
        } else {
            None
        };
    let new_metadata: Vec<u8> = match &compressed {
        Some((_, frames)) => {
            let body = crate::packet::encode_frame_index_entry(frames);
            let original = std::slice::from_raw_parts(packet.add(32), metadata_size);
            crate::packet::append_metadata_entry(
                original,
                crate::packet::METADATA_TYPE_FRAME_INDEX,
                &body,
            )
        }
        None => std::slice::from_raw_parts(packet.add(32), metadata_size).to_vec(),
    };
    let (final_payload, final_compression): (&[u8], u8) = match &compressed {
        Some((bytes, _)) => (bytes.as_slice(), PACKET_COMPRESSION_ZSTD),
        None => (raw_payload, compression),
    };

    let new_metadata_size = new_metadata.len();
    let total = 32 + new_metadata_size + final_payload.len();
    let mem = libc::malloc(total) as *mut u8;
    if mem.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("malloc failed".into()));
        return 1;
    }

    let mut new_header = *header;
    new_header.command.data.source = PACKET_SOURCE_MESG;
    new_header.command.data.compression = final_compression;
    new_header.offset = new_metadata_size as u32;
    let mut hdr_bytes = new_header.to_bytes();
    let new_len = final_payload.len() as u64;
    hdr_bytes[24..32].copy_from_slice(&new_len.to_le_bytes());

    ptr::copy_nonoverlapping(hdr_bytes.as_ptr(), mem, 32);
    if new_metadata_size > 0 {
        ptr::copy_nonoverlapping(
            new_metadata.as_ptr(),
            mem.add(32),
            new_metadata_size,
        );
    }
    if !final_payload.is_empty() {
        ptr::copy_nonoverlapping(
            final_payload.as_ptr(),
            mem.add(32 + new_metadata_size),
            final_payload.len(),
        );
    }

    *out_buf = mem;
    *out_size = total;
    0
}

unsafe fn read_file_payload(
    packet: *const u8,
    payload_start: usize,
    payload_len: usize,
    format: u8,
    errmsg: *mut *mut c_char,
) -> Option<Vec<u8>> {
    if format != PACKET_FORMAT_MSGPACK {
        set_errmsg(errmsg, &MorlocError::Packet(format!(
            "FILE source with unsupported format 0x{:02x}", format
        )));
        return None;
    }
    let filename_bytes = std::slice::from_raw_parts(
        packet.add(payload_start),
        payload_len.min(4096),
    );
    let filename = std::str::from_utf8(filename_bytes).unwrap_or("");
    let filename = filename.trim_end_matches('\0');
    match std::fs::read(filename) {
        Ok(data) => Some(data),
        Err(e) => {
            set_errmsg(errmsg, &MorlocError::Io(e));
            None
        }
    }
}

unsafe fn read_rptr_payload(
    packet: *const u8,
    payload_start: usize,
    format: u8,
    errmsg: *mut *mut c_char,
) -> Option<Vec<u8>> {
    let relptr = *(packet.add(payload_start) as *const RelPtr);
    let abs_ptr = match shm::rel2abs(relptr) {
        Ok(p) => p,
        Err(e) => { set_errmsg(errmsg, &e); return None; }
    };

    match format {
        PACKET_FORMAT_VOIDSTAR => {
            let schema_ptr = read_schema_from_packet_meta(packet, errmsg);
            if schema_ptr.is_null() {
                if !errmsg.is_null() && (*errmsg).is_null() {
                    set_errmsg(errmsg, &MorlocError::Packet(
                        "RPTR+VOIDSTAR packet missing schema metadata".into()
                    ));
                }
                return None;
            }
            let schema_str = match CStr::from_ptr(schema_ptr).to_str() {
                Ok(s) => s,
                Err(_) => {
                    set_errmsg(errmsg, &MorlocError::Packet(
                        "schema metadata is not valid UTF-8".into()
                    ));
                    return None;
                }
            };
            let rs = match crate::schema::parse_schema(schema_str) {
                Ok(s) => s,
                Err(e) => { set_errmsg(errmsg, &e); return None; }
            };
            match crate::voidstar::flatten_to_buffer(abs_ptr, &rs) {
                Ok(buf) => Some(buf),
                Err(e) => { set_errmsg(errmsg, &e); None }
            }
        }
        PACKET_FORMAT_ARROW => {
            let mut arrow_buf: *mut u8 = ptr::null_mut();
            let mut arrow_len: usize = 0;
            let rc = crate::arrow_ipc_reader::write_arrow_ipc_to_buffer(
                abs_ptr as *const _,
                &mut arrow_buf,
                &mut arrow_len,
                errmsg,
            );
            if rc != 0 || arrow_buf.is_null() {
                return None;
            }
            let v = std::slice::from_raw_parts(arrow_buf, arrow_len).to_vec();
            libc::free(arrow_buf as *mut c_void);
            Some(v)
        }
        _ => {
            set_errmsg(errmsg, &MorlocError::Packet(format!(
                "RPTR source with unsupported format 0x{:02x}", format
            )));
            None
        }
    }
}

// ── normalize_data_packet_to_fd ──────────────────────────────────────────────
//
// Writes the normalized packet to `fd`; returns total bytes written, or a
// negative value on error. A large payload named by reference (RPTR or
// FILE, but not an RPTR table) bound for a positionally writable fd is
// streamed.
pub(crate) unsafe fn normalize_data_packet_to_fd(
    packet: *const u8,
    packet_size: usize,
    compression_level: u8,
    fd: libc::c_int,
    errmsg: *mut *mut c_char,
) -> i64 {
    clear_errmsg(errmsg);

    let header = read_morloc_packet_header(packet, errmsg);
    if header.is_null() {
        return -1;
    }

    let is_data = (*header).is_data();
    let is_fail = (*header).is_fail();
    let source = if is_data { (*header).command.data.source } else { 0 };
    let format = if is_data { (*header).command.data.format } else { 0 };

    let stream_eligible = is_data
        && !is_fail
        && source != PACKET_SOURCE_MESG
        && !(source == PACKET_SOURCE_RPTR && format == PACKET_FORMAT_ARROW)
        && positionally_writable(fd);

    if stream_eligible {
        match estimate_payload_size(packet, source, format, errmsg) {
            Some(est) if est >= STREAMING_THRESHOLD => {
                return stream_packet_to_fd(
                    packet, packet_size, compression_level, fd, est, errmsg,
                );
            }
            Some(_) => {} // below threshold: fall through to buffered
            None => return -1, // estimation hit an error (already set)
        }
    }

    buffered_packet_to_fd(packet, packet_size, compression_level, fd, errmsg)
}

// Below 1 MiB the buffered alloc + memcpy beats the streaming setup cost.
const STREAMING_THRESHOLD: u64 = 1 << 20;

unsafe fn buffered_packet_to_fd(
    packet: *const u8,
    packet_size: usize,
    compression_level: u8,
    fd: libc::c_int,
    errmsg: *mut *mut c_char,
) -> i64 {
    let mut out_buf: *mut u8 = ptr::null_mut();
    let mut out_size: usize = 0;
    let rc = normalize_data_packet_for_output(
        packet, packet_size, compression_level,
        &mut out_buf, &mut out_size, errmsg,
    );
    if rc != 0 || out_buf.is_null() {
        return -1;
    }
    let n = write_all_fd(fd, out_buf, out_size, errmsg);
    libc::free(out_buf as *mut c_void);
    n
}

/// Write `bytes` to a new file `morloc-pkt-<pid>-<seq>.mpk` in `dir`,
/// created exclusively and never through a symlink, so a name another user
/// planted in a shared directory is skipped rather than written through.
fn create_packet_file(
    dir: &std::path::Path,
    pid: u32,
    mut next_seq: impl FnMut() -> u64,
    bytes: &[u8],
) -> std::io::Result<std::path::PathBuf> {
    use std::io::Write;
    use std::os::unix::fs::OpenOptionsExt;
    for _ in 0..64 {
        let path = dir.join(format!("morloc-pkt-{}-{}.mpk", pid, next_seq()));
        let opened = std::fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .mode(0o600)
            .custom_flags(libc::O_NOFOLLOW)
            .open(&path);
        match opened {
            Ok(mut f) => {
                if let Err(e) = f.write_all(bytes) {
                    let _ = std::fs::remove_file(&path);
                    return Err(e);
                }
                return Ok(path);
            }
            Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => continue,
            Err(e) => return Err(e),
        }
    }
    Err(std::io::Error::new(std::io::ErrorKind::AlreadyExists, "no free name for a file packet"))
}

// Returns bytes written, -1 with errmsg set, or PACKET_TO_FD_PIPE_CLOSED
// without errmsg on EPIPE.
pub(crate) unsafe fn write_all_fd(
    fd: libc::c_int,
    buf: *const u8,
    len: usize,
    errmsg: *mut *mut c_char,
) -> i64 {
    let mut written: usize = 0;
    while written < len {
        let n = libc::write(
            fd,
            buf.add(written) as *const c_void,
            len - written,
        );
        if n < 0 {
            let err = std::io::Error::last_os_error();
            if err.kind() == std::io::ErrorKind::Interrupted {
                continue;
            }
            if err.raw_os_error() == Some(libc::EPIPE) {
                return morloc_runtime_types::PACKET_TO_FD_PIPE_CLOSED;
            }
            set_errmsg(errmsg, &MorlocError::Io(err));
            return -1;
        }
        if n == 0 {
            set_errmsg(errmsg, &MorlocError::Other("write returned 0".into()));
            return -1;
        }
        written += n as usize;
    }
    written as i64
}

// Whether a write at a chosen offset lands there: true for a regular file
// not opened for appending. The streaming path patches the packet's length
// and frame index in place once the payload is written; pipes and sockets
// cannot seek, and an O_APPEND descriptor (a `>>` redirect) appends every
// pwrite regardless of its offset.
unsafe fn positionally_writable(fd: libc::c_int) -> bool {
    let mut st: libc::stat = std::mem::zeroed();
    if libc::fstat(fd, &mut st) != 0 || (st.st_mode & libc::S_IFMT) != libc::S_IFREG {
        return false;
    }
    let flags = libc::fcntl(fd, libc::F_GETFL);
    flags != -1 && flags & libc::O_APPEND == 0
}

// Estimated on-disk payload size, used to choose streaming and to size
// the frame-index reservation. None with errmsg set when the relptr does
// not resolve; other failures give Some(0), which selects buffering.
unsafe fn estimate_payload_size(
    packet: *const u8,
    source: u8,
    format: u8,
    errmsg: *mut *mut c_char,
) -> Option<u64> {
    let header = packet as *const PacketHeader;
    let metadata_size = (*header).offset as usize;
    let payload_start = 32 + metadata_size;
    let payload_len = (*header).length as usize;

    match source {
        PACKET_SOURCE_RPTR => {
            if format != PACKET_FORMAT_VOIDSTAR {
                return Some(0);
            }
            let relptr = *(packet.add(payload_start) as *const RelPtr);
            let abs_ptr = match shm::rel2abs(relptr) {
                Ok(p) => p,
                Err(e) => { set_errmsg(errmsg, &e); return None; }
            };
            let t_est = std::time::Instant::now();
            // The size of the block the value heads bounds its serialized size when
            // the value lies within that block.
            if let Some(sz) = unsafe { shm::shm_block_size(abs_ptr) } {
                crate::morloc_trace!(
                    "[estimate_payload_size] shm_block_size = {} MiB in {:.2?}",
                    sz / (1 << 20),
                    t_est.elapsed()
                );
                return Some(sz as u64);
            }
            let schema_ptr = read_schema_from_packet_meta(packet, errmsg);
            if schema_ptr.is_null() {
                return Some(0);
            }
            let schema_str = match CStr::from_ptr(schema_ptr).to_str() {
                Ok(s) => s,
                Err(_) => return Some(0),
            };
            let rs = match crate::schema::parse_schema(schema_str) {
                Ok(s) => s,
                Err(_) => return Some(0),
            };
            let r = match crate::ffi::calc_voidstar_size_inner(abs_ptr, &rs) {
                Ok(sz) => Some(sz as u64),
                Err(_) => Some(0),
            };
            crate::morloc_trace!(
                "[estimate_payload_size] calc_voidstar_size_inner = {} MiB in {:.2?}",
                r.unwrap_or(0) / (1 << 20),
                t_est.elapsed()
            );
            r
        }
        PACKET_SOURCE_FILE => {
            let filename_bytes = std::slice::from_raw_parts(
                packet.add(payload_start),
                payload_len.min(4096),
            );
            let filename = std::str::from_utf8(filename_bytes).unwrap_or("");
            let filename = filename.trim_end_matches('\0');
            match std::fs::metadata(filename) {
                Ok(md) => Some(md.len()),
                Err(_) => Some(0),
            }
        }
        _ => Some(payload_len as u64),
    }
}

// Streaming path: write header + metadata + payload directly to the fd.
// Returns total bytes written, or -1 with errmsg set.
unsafe fn stream_packet_to_fd(
    packet: *const u8,
    packet_size: usize,
    compression_level: u8,
    fd: libc::c_int,
    estimated_payload_bytes: u64,
    errmsg: *mut *mut c_char,
) -> i64 {
    let t_stream = std::time::Instant::now();
    crate::morloc_trace!(
        "[stream_packet_to_fd] entering: est_payload={} MiB, lvl={}",
        estimated_payload_bytes / (1 << 20),
        compression_level
    );
    let header_in = packet as *const PacketHeader;
    let metadata_size = (*header_in).offset as usize;
    let payload_start = 32 + metadata_size;
    let payload_len = (*header_in).length as usize;
    let source = (*header_in).command.data.source;
    let format = (*header_in).command.data.format;

    if payload_start + payload_len > packet_size {
        set_errmsg(errmsg, &MorlocError::Packet(
            "packet payload extends past packet bounds".into()
        ));
        return -1;
    }

    let lvl_opt = if compression_level > 0 {
        match crate::compression::CompressionLevel::from_u8(compression_level) {
            Ok(l) if !l.is_none() => Some(l),
            Ok(_) => None,
            Err(e) => { set_errmsg(errmsg, &e); return -1; }
        }
    } else {
        None
    };

    let start_pos = libc::lseek(fd, 0, libc::SEEK_CUR);
    if start_pos < 0 {
        set_errmsg(errmsg, &MorlocError::Io(std::io::Error::last_os_error()));
        return -1;
    }

    let mut new_header = *header_in;
    new_header.command.data.source = PACKET_SOURCE_MESG;
    new_header.command.data.compression = if lvl_opt.is_some() {
        PACKET_COMPRESSION_ZSTD
    } else {
        PACKET_COMPRESSION_NONE
    };

    // The payload's relptrs are written buffer-relative (volume 0), never
    // with the producer's volume index: a file whose offsets matched live
    // SHM relptrs would resolve silently if a reader forgot to rebase it,
    // where a buffer-relative offset fails to resolve.
    // When compressing, the frame-index entry is reserved at its worst-case
    // size for `estimated_payload_bytes` and patched in once the payload is
    // written. Decoders read the entry's `frame_count`, not its size field,
    // so trailing zero padding is fine.
    let original_meta = std::slice::from_raw_parts(packet.add(32), metadata_size);
    let frame_index_reserved_body: usize = if lvl_opt.is_some() {
        let max_frames =
            crate::compression::max_frames_for(estimated_payload_bytes as usize);
        crate::compression::frame_index_entry_max_bytes(max_frames)
    } else {
        0
    };
    let original = original_meta.to_vec();
    let new_meta: Vec<u8> = if frame_index_reserved_body > 0 {
        let zeros = vec![0u8; frame_index_reserved_body];
        crate::packet::append_metadata_entry(
            &original,
            METADATA_TYPE_FRAME_INDEX,
            &zeros,
        )
    } else {
        original
    };

    let extended_offset = new_meta.len();
    new_header.offset = extended_offset as u32;
    let mut hdr_bytes = new_header.to_bytes();
    hdr_bytes[24..32].copy_from_slice(&0u64.to_le_bytes());

    let n = write_all_fd(fd, hdr_bytes.as_ptr(), 32, errmsg);
    if n < 0 {
        return n;
    }
    let frame_index_body_start = if frame_index_reserved_body > 0 {
        let entry_total = 8 + frame_index_reserved_body;
        let entries_end_in_new = crate::packet::metadata_entries_end(&new_meta);
        let body_offset_in_meta = entries_end_in_new - entry_total + 8;
        Some(start_pos + 32 + body_offset_in_meta as i64)
    } else {
        None
    };
    if extended_offset > 0 {
        let n = write_all_fd(fd, new_meta.as_ptr(), extended_offset, errmsg);
        if n < 0 {
            return n;
        }
    }

    let payload_origin = start_pos + 32 + extended_offset as i64;
    let t_walk = std::time::Instant::now();
    let stream_result = match source {
        PACKET_SOURCE_RPTR => stream_rptr_payload(
            packet, payload_start, format, fd, lvl_opt, errmsg,
        ),
        PACKET_SOURCE_FILE => stream_file_payload(
            packet, payload_start, payload_len, format, fd, lvl_opt, errmsg,
        ),
        _ => {
            set_errmsg(errmsg, &MorlocError::Packet(format!(
                "stream_packet_to_fd called with unstreamable source 0x{:02x}", source
            )));
            return -1;
        }
    };
    crate::morloc_trace!(
        "[stream_packet_to_fd] walk+encode took {:.2?}",
        t_walk.elapsed()
    );
    let frame_index = match stream_result {
        Ok(frames) => frames,
        Err(()) => return -1,
    };

    let end_pos = libc::lseek(fd, 0, libc::SEEK_CUR);
    if end_pos < 0 {
        set_errmsg(errmsg, &MorlocError::Io(std::io::Error::last_os_error()));
        return -1;
    }
    let payload_size = (end_pos - payload_origin) as u64;
    let length_le = payload_size.to_le_bytes();
    let rc = libc::pwrite(
        fd,
        length_le.as_ptr() as *const c_void,
        8,
        start_pos + 24,
    );
    if rc != 8 {
        set_errmsg(errmsg, &MorlocError::Io(std::io::Error::last_os_error()));
        return -1;
    }

    if let Some(body_start) = frame_index_body_start {
        if frame_index.is_empty() {
            set_errmsg(errmsg, &MorlocError::Packet(
                "stream encoder returned empty frame index for compressed packet".into(),
            ));
            return -1;
        }
        let body = crate::packet::encode_frame_index_entry(&frame_index);
        if body.len() > frame_index_reserved_body {
            set_errmsg(errmsg, &MorlocError::Packet(format!(
                "frame index ({} bytes) exceeds reserved space ({} bytes); \
                 estimated payload was {} bytes",
                body.len(),
                frame_index_reserved_body,
                estimated_payload_bytes,
            )));
            return -1;
        }
        let rc = libc::pwrite(
            fd,
            body.as_ptr() as *const c_void,
            body.len(),
            body_start,
        );
        if rc as usize != body.len() {
            set_errmsg(errmsg, &MorlocError::Io(std::io::Error::last_os_error()));
            return -1;
        }
    }

    crate::morloc_trace!(
        "[stream_packet_to_fd] total: {:.2?} (compressed payload {} MiB, {} frames)",
        t_stream.elapsed(),
        payload_size / (1 << 20),
        frame_index.len()
    );
    (end_pos - start_pos) as i64
}

struct FdWriter(libc::c_int);

impl std::io::Write for FdWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        loop {
            let n = unsafe {
                libc::write(self.0, buf.as_ptr() as *const c_void, buf.len())
            };
            if n < 0 {
                let err = std::io::Error::last_os_error();
                if err.kind() == std::io::ErrorKind::Interrupted {
                    continue;
                }
                return Err(err);
            }
            return Ok(n as usize);
        }
    }
    fn flush(&mut self) -> std::io::Result<()> { Ok(()) }
}

// Returns the frame index; empty when uncompressed.
unsafe fn stream_rptr_payload(
    packet: *const u8,
    payload_start: usize,
    format: u8,
    fd: libc::c_int,
    lvl: Option<crate::compression::CompressionLevel>,
    errmsg: *mut *mut c_char,
) -> Result<Vec<crate::packet::FrameEntry>, ()> {
    if format != PACKET_FORMAT_VOIDSTAR {
        set_errmsg(errmsg, &MorlocError::Packet(format!(
            "stream_rptr_payload: expected VOIDSTAR, got 0x{:02x}", format
        )));
        return Err(());
    }
    let relptr = *(packet.add(payload_start) as *const RelPtr);
    let abs_ptr = match shm::rel2abs(relptr) {
        Ok(p) => p,
        Err(e) => { set_errmsg(errmsg, &e); return Err(()); }
    };
    let schema_ptr = read_schema_from_packet_meta(packet, errmsg);
    if schema_ptr.is_null() {
        if (*errmsg).is_null() {
            set_errmsg(errmsg, &MorlocError::Packet(
                "RPTR+VOIDSTAR packet missing schema metadata".into()
            ));
        }
        return Err(());
    }
    let schema_str = match CStr::from_ptr(schema_ptr).to_str() {
        Ok(s) => s,
        Err(_) => {
            set_errmsg(errmsg, &MorlocError::Packet(
                "schema metadata is not valid UTF-8".into()
            ));
            return Err(());
        }
    };
    let rs = match crate::schema::parse_schema(schema_str) {
        Ok(s) => s,
        Err(e) => { set_errmsg(errmsg, &e); return Err(()); }
    };

    let writer = FdWriter(fd);
    let t_total = std::time::Instant::now();
    let mut walker_elapsed: Option<std::time::Duration> = None;
    let walker_ref = &mut walker_elapsed;
    let result = crate::compression::stream_encode_with(
        writer,
        lvl,
        |sink| {
            let r = crate::voidstar::write_flat_to_writer_with_vol_idx(
                sink, abs_ptr, &rs, 0,
            ).map(|_| ());
            *walker_ref = Some(t_total.elapsed());
            r
        },
    );
    let total = t_total.elapsed();
    if let Some(walker) = walker_elapsed {
        crate::morloc_trace!(
            "[stream_rptr_payload] walker took {:.2?}, drain took {:.2?}",
            walker,
            total.saturating_sub(walker)
        );
    }
    match result {
        Ok((_writer, frames)) => Ok(frames),
        Err(e) => {
            set_errmsg(errmsg, &e);
            Err(())
        }
    }
}

// Returns the frame index; empty when uncompressed.
unsafe fn stream_file_payload(
    packet: *const u8,
    payload_start: usize,
    payload_len: usize,
    format: u8,
    fd: libc::c_int,
    lvl: Option<crate::compression::CompressionLevel>,
    errmsg: *mut *mut c_char,
) -> Result<Vec<crate::packet::FrameEntry>, ()> {
    if format != PACKET_FORMAT_MSGPACK {
        set_errmsg(errmsg, &MorlocError::Packet(format!(
            "stream_file_payload: expected MSGPACK, got 0x{:02x}", format
        )));
        return Err(());
    }
    let filename_bytes = std::slice::from_raw_parts(
        packet.add(payload_start),
        payload_len.min(4096),
    );
    let filename = std::str::from_utf8(filename_bytes).unwrap_or("");
    let filename = filename.trim_end_matches('\0');
    let mut src = match std::fs::File::open(filename) {
        Ok(f) => f,
        Err(e) => { set_errmsg(errmsg, &MorlocError::Io(e)); return Err(()); }
    };

    let writer = FdWriter(fd);
    match lvl {
        Some(lvl) => {
            match crate::compression::stream_encode_reader(writer, &mut src, lvl) {
                Ok((_writer, frames)) => Ok(frames),
                Err(e) => {
                    set_errmsg(errmsg, &e);
                    Err(())
                }
            }
        }
        None => {
            let mut writer = writer;
            match std::io::copy(&mut src, &mut writer).map_err(MorlocError::Io) {
                Ok(_) => Ok(Vec::new()),
                Err(e) => {
                    set_errmsg(errmsg, &e);
                    Err(())
                }
            }
        }
    }
}

unsafe fn alloc_out(
    src: *const u8,
    len: usize,
    out_buf: *mut *mut u8,
    out_size: *mut usize,
    errmsg: *mut *mut c_char,
) -> i32 {
    let mem = libc::malloc(len) as *mut u8;
    if mem.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("malloc failed".into()));
        return 1;
    }
    if len > 0 {
        ptr::copy_nonoverlapping(src, mem, len);
    }
    *out_buf = mem;
    *out_size = len;
    0
}

// ── make_file_data_packet_voidstar ───────────────────────────────────────────
//
// A FILE+MSGPACK packet for a value when shared memory is disabled: the
// value is written as msgpack to a new file the packet names.
unsafe fn make_file_data_packet_voidstar(
    voidstar: *mut c_void,
    schema: *const CSchema,
    rs: &crate::schema::Schema,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    use std::sync::atomic::{AtomicU64, Ordering};
    static SEQ: AtomicU64 = AtomicU64::new(0);

    let mpk = match crate::mpack::pack_with_schema(voidstar as AbsPtr, rs) {
        Ok(b) => b,
        Err(e) => { set_errmsg(errmsg, &e); return ptr::null_mut(); }
    };

    // Files in a user-supplied tmpdir are kept for inspection.
    let user_dir = crate::packet::file_packet_tmpdir();
    let persistent = user_dir.is_some();
    let dir: std::path::PathBuf = match user_dir {
        Some(d) => std::path::PathBuf::from(d),
        None    => std::env::temp_dir(),
    };
    if let Err(e) = std::fs::create_dir_all(&dir) {
        set_errmsg(errmsg, &MorlocError::Io(e));
        return ptr::null_mut();
    }
    let pid = std::process::id();
    let path = match create_packet_file(&dir, pid, || SEQ.fetch_add(1, Ordering::Relaxed), &mpk) {
        Ok(p) => p,
        Err(e) => {
            set_errmsg(errmsg, &MorlocError::Io(e));
            return ptr::null_mut();
        }
    };

    let path_str = match path.to_str() {
        Some(s) => s,
        None => {
            set_errmsg(errmsg, &MorlocError::Other("Non-UTF8 temp path for file packet".into()));
            let _ = std::fs::remove_file(&path);
            return ptr::null_mut();
        }
    };
    let path_cstr = match std::ffi::CString::new(path_str) {
        Ok(c) => c,
        Err(_) => {
            set_errmsg(errmsg, &MorlocError::Other("Embedded NUL in temp path for file packet".into()));
            let _ = std::fs::remove_file(&path);
            return ptr::null_mut();
        }
    };

    let packet = make_mpk_data_packet(path_cstr.as_ptr(), schema);
    if packet.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("Failed to create FILE data packet".into()));
        let _ = std::fs::remove_file(&path);
        return ptr::null_mut();
    }

    if !persistent {
        crate::eval_arena::record_file_if_active(path);
    }
    packet
}

// ── print_morloc_data_packet ─────────────────────────────────────────────────

pub(crate) unsafe fn print_morloc_data_packet(
    packet: *const u8,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    let header = read_morloc_packet_header(packet, errmsg);
    if header.is_null() { return 1; }

    if (*header).command_type() != PACKET_TYPE_DATA {
        set_errmsg(errmsg, &MorlocError::Packet("Expected a data packet".into()));
        return 1;
    }

    let packet_error = get_morloc_data_packet_error_message(packet, errmsg);
    if !packet_error.is_null() {
        let err_str = CStr::from_ptr(packet_error).to_string_lossy().into_owned();
        libc::free(packet_error as *mut c_void);
        set_errmsg(errmsg, &MorlocError::Packet(format!("\n{}", err_str)));
        return 1;
    }
    clear_errmsg(errmsg);

    let rs = CSchema::to_rust(schema);
    let source = (*header).command.data.source;
    let format = (*header).command.data.format;
    let packet_size = morloc_packet_size_from_header(header);

    match source {
        PACKET_SOURCE_MESG | PACKET_SOURCE_FILE => {
            let rc = print_binary(packet, packet_size, errmsg);
            if rc != 0 {
                return rc;
            }
        }
        PACKET_SOURCE_RPTR => {
            match format {
                PACKET_FORMAT_VOIDSTAR => {
                    let payload_start = 32 + (*header).offset as usize;
                    let relptr = std::ptr::read_unaligned(packet.add(payload_start) as *const RelPtr);
                    let voidstar_ptr = match shm::rel2abs(relptr) {
                        Ok(p) => p,
                        Err(e) => { set_errmsg(errmsg, &e); return 1; }
                    };

                    let flat_size = match crate::ffi::calc_voidstar_size_inner(voidstar_ptr, &rs) {
                        Ok(s) => s,
                        Err(e) => { set_errmsg(errmsg, &e); return 1; }
                    };

                    let mut new_header = *header;
                    new_header.command.data.source = PACKET_SOURCE_MESG;
                    new_header.command.data.format = PACKET_FORMAT_VOIDSTAR;
                    new_header.length = flat_size as u64;

                    let rc = print_binary(&new_header as *const PacketHeader as *const u8, 32, errmsg);
                    if rc != 0 {
                        return rc;
                    }

                    let offset = (*header).offset as usize;
                    if offset > 0 {
                        let rc = print_binary(packet.add(32), offset, errmsg);
                        if rc != 0 {
                            return rc;
                        }
                    }

                    match crate::voidstar::write_binary_to_fd(libc::STDOUT_FILENO, voidstar_ptr, &rs) {
                        Ok(_) => {}
                        Err(e) => {
                            set_errmsg(errmsg, &e);
                            return if matches!(e, MorlocError::PipeClosed) { crate::ipc_ffi::note_pipe_closed() } else { 1 };
                        }
                    }
                }
                _ => {
                    let rc = print_binary(packet, packet_size, errmsg);
                    if rc != 0 {
                        return rc;
                    }
                }
            }
        }
        _ => {
            set_errmsg(errmsg, &MorlocError::Packet("Invalid source".into()));
            return 1;
        }
    }

    0 // EXIT_PASS
}

/// Write binary data to stdout.
///
/// # Safety
/// `buf` must point to at least `count` readable bytes.
unsafe fn print_binary(
    buf: *const u8,
    count: usize,
    errmsg: *mut *mut c_char,
) -> i32 {
    crate::utility::print_binary(buf as *const c_char, count, errmsg)
}

// ── Tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod donation_tests {

    //! The reference a sender takes on behalf of a recipient must keep the
    //! block alive across the sender's own release.
    use super::*;

    #[test]
    fn a_donated_reference_survives_the_sender_release() {
        let _shm = crate::own_test_registry();

        let payload: &[u8] = b"the recipient must still see this";
        let abs = crate::shm::shmalloc(payload.len()).expect("allocate");
        unsafe {
            std::ptr::copy_nonoverlapping(payload.as_ptr(), abs as *mut u8, payload.len());
        }
        assert_eq!(unsafe { crate::shm::reference_count(abs) }, Some(1));

        let rel = crate::shm::abs2rel(abs).expect("relptr");
        let schema = crate::schema::Schema::primitive(crate::schema::SerialType::Uint8);
        let cs = CSchema::from_rust(&schema);
        let packet = unsafe { make_standard_data_packet(rel, cs) };
        assert!(!packet.is_null(), "expected an RPTR packet");

        // The sender takes the recipient's reference before the packet goes.
        unsafe { donate_packet_reference(packet) }.expect("donate");
        assert_eq!(unsafe { crate::shm::reference_count(abs) }, Some(2));

        // The sender now releases its own, as it does at its next dispatch.
        crate::shm::shfree(abs).expect("sender release");

        // The block must still be alive and unscrubbed for the recipient.
        assert_eq!(
            unsafe { crate::shm::reference_count(abs) }, Some(1),
            "the donated reference did not survive the sender's release",
        );
        let seen = unsafe { std::slice::from_raw_parts(abs as *const u8, payload.len()) };
        assert_eq!(seen, payload, "block was scrubbed while the recipient still held it");

        // The recipient releases exactly once, and the block is then free.
        crate::shm::shfree(abs).expect("recipient release");
        assert_eq!(unsafe { crate::shm::reference_count(abs) }, Some(0));

        unsafe { libc::free(packet as *mut libc::c_void) };
        unsafe { CSchema::free(cs) };
    }

    fn counted(tag: &str, want: i64) -> bool {
        let got = crate::shm::held_references();
        if got != want {
            eprintln!("{tag}: this process counts {got} references, expected {want}");
        }
        got == want
    }

    #[test]
    fn a_donated_reference_leaves_the_senders_count_and_joins_the_receivers() {
        let _shm = crate::own_test_registry();
        assert!(crate::fork_policy::exits_cleanly_in_a_forked_child(|| {
            let abs = crate::shm::shmalloc(64).expect("allocate");
            let rel = crate::shm::abs2rel(abs).expect("relptr");
            let schema = crate::schema::Schema::primitive(crate::schema::SerialType::Uint8);
            let cs = CSchema::from_rust(&schema);
            let packet = unsafe { make_standard_data_packet(rel, cs) };
            let mut ok = counted("allocated", 1);
            unsafe { donate_packet_reference(packet) }.expect("donate");
            ok &= counted("donated", 1);
            crate::shm::shfree(abs).expect("sender release");
            ok &= counted("sender released", 0);
            unsafe { inherit_reply(packet) };
            ok &= counted("received", 1);
            crate::shm::shfree(abs).expect("receiver release");
            ok &= counted("receiver released", 0);
            unsafe { libc::free(packet as *mut libc::c_void) };
            unsafe { CSchema::free(cs) };
            ok
        }));
    }

    #[test]
    fn a_revoked_donation_returns_to_the_senders_count() {
        let _shm = crate::own_test_registry();
        assert!(crate::fork_policy::exits_cleanly_in_a_forked_child(|| {
            let abs = crate::shm::shmalloc(64).expect("allocate");
            let rel = crate::shm::abs2rel(abs).expect("relptr");
            let schema = crate::schema::Schema::primitive(crate::schema::SerialType::Uint8);
            let cs = CSchema::from_rust(&schema);
            let packet = unsafe { make_standard_data_packet(rel, cs) };
            unsafe { donate_packet_reference(packet) }.expect("donate");
            unsafe { revoke_packet_reference(packet) };
            let mut ok = counted("revoked", 1);
            crate::shm::shfree(abs).expect("release");
            ok &= counted("released", 0);
            unsafe { libc::free(packet as *mut libc::c_void) };
            unsafe { CSchema::free(cs) };
            ok
        }));
    }

    #[test]
    fn a_failed_send_gives_the_reference_back() {
        let _shm = crate::own_test_registry();
        let abs = crate::shm::shmalloc(64).expect("allocate");
        let rel = crate::shm::abs2rel(abs).expect("relptr");
        let schema = crate::schema::Schema::primitive(crate::schema::SerialType::Uint8);
        let cs = CSchema::from_rust(&schema);
        let packet = unsafe { make_standard_data_packet(rel, cs) };

        unsafe { donate_packet_reference(packet) }.expect("donate");
        assert_eq!(unsafe { crate::shm::reference_count(abs) }, Some(2));
        unsafe { revoke_packet_reference(packet) };
        assert_eq!(
            unsafe { crate::shm::reference_count(abs) }, Some(1),
            "a packet that reached nobody left its reference behind",
        );

        crate::shm::shfree(abs).expect("release");
        unsafe { libc::free(packet as *mut libc::c_void) };
        unsafe { CSchema::free(cs) };
    }
}

#[cfg(test)]
mod auto_routing_tests {
    //! `make_data_packet_auto` inlines a value whose flat size is at most the
    //! inline threshold (source MESG) and sends a larger one by reference
    //! (source RPTR).
    use super::*;

    #[test]
    fn a_pipe_closed_packet_is_a_failure_that_says_so() {
        let msg = std::ffi::CString::new("closed").unwrap();
        let p = unsafe { make_pipe_closed_packet(msg.as_ptr()) };
        let f = unsafe { make_fail_packet(msg.as_ptr()) };
        let mut err: *mut c_char = ptr::null_mut();
        let text = unsafe { get_morloc_data_packet_error_message(p, &mut err) };
        assert!(!text.is_null());
        assert_eq!(unsafe { CStr::from_ptr(text) }.to_str().unwrap(), "closed");
        assert!(unsafe { morloc_packet_is_pipe_closed(p) });
        assert!(!unsafe { morloc_packet_is_pipe_closed(f) });
        unsafe {
            libc::free(text as *mut c_void);
            libc::free(p as *mut c_void);
            libc::free(f as *mut c_void);
        }
    }
    use crate::packet::{
        MORLOC_INLINE_THRESHOLD, PACKET_SOURCE_MESG, PACKET_SOURCE_RPTR,
        TEST_CONFIG_LOCK,
    };
    use crate::shm::{self, Array, RelPtr};

    use morloc_runtime_types::packet::PKT_SOURCE_OFF as SOURCE_OFFSET;

    #[must_use]
    fn ensure_shm() -> crate::ArenaShared {
        crate::init_test_shm()
    }

    /// An `Array<u8>` of `n` bytes in shared memory.
    unsafe fn build_byte_array_voidstar(n: usize) -> (AbsPtr, RelPtr) {
        let total = std::mem::size_of::<Array>() + n;
        let mut blob = vec![0u8; total];
        let arr = Array {
            size: n,
            data: std::mem::size_of::<Array>() as RelPtr,
        };
        std::ptr::copy_nonoverlapping(
            &arr as *const Array as *const u8,
            blob.as_mut_ptr(),
            std::mem::size_of::<Array>(),
        );
        for i in 0..n {
            blob[std::mem::size_of::<Array>() + i] = (i & 0xff) as u8;
        }
        let schema = byte_array_schema();
        let abs = crate::voidstar::read_binary(&blob, &schema).unwrap();
        let rel = shm::abs2rel(abs).unwrap();
        (abs, rel)
    }

    fn byte_array_schema() -> crate::schema::Schema {
        // Array of UInt8 (`au1` in the schema mini-language).
        crate::schema::parse_schema("au1").unwrap()
    }

    /// The `source` byte of the packet `make_data_packet_auto` builds for an
    /// `n`-byte array.
    fn auto_route_source_for(n: usize) -> u8 {
        let _guard = TEST_CONFIG_LOCK.lock().unwrap();
        let _shm = ensure_shm();
        let schema = byte_array_schema();
        let cs = crate::cschema::CSchema::from_rust(&schema);
        unsafe {
            let (abs, rel) = build_byte_array_voidstar(n);
            let mut errmsg: *mut c_char = ptr::null_mut();
            let packet = make_data_packet_auto(
                abs as *mut c_void,
                rel,
                cs as *const crate::cschema::CSchema,
                &mut errmsg,
            );
            assert!(
                !packet.is_null(),
                "make_data_packet_auto returned null for N={}",
                n
            );
            let source = *packet.add(SOURCE_OFFSET);
            assert_eq!(
                *packet.add(12),
                crate::packet::PACKET_TYPE_DATA,
                "expected DATA packet for N={}",
                n
            );
            libc::free(packet as *mut c_void);
            crate::cschema::CSchema::free(cs);
            let _ = shm::shfree(abs);
            source
        }
    }

    #[test]
    fn empty_payload_inlines() {
        assert_eq!(auto_route_source_for(0), PACKET_SOURCE_MESG);
    }

    #[test]
    fn one_byte_inlines() {
        assert_eq!(auto_route_source_for(1), PACKET_SOURCE_MESG);
    }

    #[test]
    fn small_payload_inlines() {
        assert_eq!(auto_route_source_for(1024), PACKET_SOURCE_MESG);
    }

    #[test]
    fn just_below_threshold_inlines() {
        assert_eq!(auto_route_source_for(65000), PACKET_SOURCE_MESG);
    }

    /// The flat size of an `n`-byte array minus `n`: the wrapper plus
    /// `array_data_alignment - 1` bytes of alignment slack.
    fn byte_array_flat_overhead() -> usize {
        let schema = byte_array_schema();
        let elem_align = schema.parameters[0].array_data_alignment();
        std::mem::size_of::<Array>() + elem_align.saturating_sub(1)
    }

    #[test]
    fn at_threshold_inlines() {
        // The contract is `flat_size <= threshold`: equality inlines.
        let n = MORLOC_INLINE_THRESHOLD - byte_array_flat_overhead();
        assert_eq!(auto_route_source_for(n), PACKET_SOURCE_MESG);
    }

    #[test]
    fn one_byte_over_threshold_uses_rptr() {
        let n = MORLOC_INLINE_THRESHOLD - byte_array_flat_overhead() + 1;
        assert_eq!(auto_route_source_for(n), PACKET_SOURCE_RPTR);
    }

    #[test]
    fn bounded_size_exits_early_above_bound() {
        let _guard = TEST_CONFIG_LOCK.lock().unwrap();
        let _shm = ensure_shm();
        let schema = byte_array_schema();
        let n = 128 * 1024;
        let (abs, _rel) = unsafe { build_byte_array_voidstar(n) };
        let bound = 1024usize;
        let sz = crate::ffi::calc_voidstar_size_bounded(abs as *const u8, &schema, bound)
            .unwrap();
        assert!(
            sz > bound,
            "bounded walk returned {} for {}-byte payload; expected > bound {}",
            sz, n, bound
        );
        let exact = crate::ffi::calc_voidstar_size_inner(abs as *const u8, &schema).unwrap();
        assert!(
            exact >= n,
            "exact size {} smaller than raw data length {}",
            exact, n
        );
        let _ = shm::shfree(abs);
    }

    #[test]
    fn bounded_size_matches_exact_when_bound_clears_payload() {
        let _guard = TEST_CONFIG_LOCK.lock().unwrap();
        let _shm = ensure_shm();
        let schema = byte_array_schema();
        let n = 256usize;
        let (abs, _rel) = unsafe { build_byte_array_voidstar(n) };
        let exact = crate::ffi::calc_voidstar_size_inner(abs as *const u8, &schema).unwrap();
        let bounded = crate::ffi::calc_voidstar_size_bounded(
            abs as *const u8, &schema, exact + 1024,
        ).unwrap();
        assert_eq!(exact, bounded);
        let _ = shm::shfree(abs);
    }

    #[test]
    fn well_over_threshold_uses_rptr() {
        assert_eq!(auto_route_source_for(128 * 1024), PACKET_SOURCE_RPTR);
    }
}

mod c_abi {
    use super::*;

    #[no_mangle]
    pub unsafe extern "C" fn packet_is_ping(packet: *const u8, errmsg: *mut *mut c_char) -> bool {
        super::packet_is_ping(packet, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn packet_is_local_call(packet: *const u8, errmsg: *mut *mut c_char) -> bool {
        super::packet_is_local_call(packet, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn packet_is_remote_call(packet: *const u8, errmsg: *mut *mut c_char) -> bool {
        super::packet_is_remote_call(packet, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn morloc_packet_size(packet: *const u8, errmsg: *mut *mut c_char) -> usize {
        super::morloc_packet_size(packet, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn morloc_dup_packet(packet: *const u8, block_out: *mut *mut c_void, errmsg: *mut *mut c_char) -> *mut u8 {
        super::morloc_dup_packet(packet, block_out, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn return_ping(packet: *const u8, errmsg: *mut *mut c_char) -> *mut u8 {
        super::return_ping(packet, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn make_standard_data_packet(relptr: RelPtr, schema: *const CSchema) -> *mut u8 {
        super::make_standard_data_packet(relptr, schema)
    }

    #[no_mangle]
    pub unsafe extern "C" fn make_arrow_data_packet(relptr: RelPtr, schema: *const CSchema) -> *mut u8 {
        super::make_arrow_data_packet(relptr, schema)
    }

    #[no_mangle]
    pub unsafe extern "C" fn make_table_file_packet(path: *const c_char, schema: *const CSchema) -> *mut u8 {
        super::make_table_file_packet(path, schema)
    }

    #[no_mangle]
    pub unsafe extern "C" fn make_data_packet_from_mpk(mpk: *const c_char, mpk_size: usize, schema: *const CSchema) -> *mut u8 {
        super::make_data_packet_from_mpk(mpk, mpk_size, schema)
    }

    #[no_mangle]
    pub unsafe extern "C" fn make_fail_packet(failure_message: *const c_char) -> *mut u8 {
        super::make_fail_packet(failure_message)
    }

    #[no_mangle]
    pub unsafe extern "C" fn make_pipe_closed_packet(failure_message: *const c_char) -> *mut u8 {
        super::make_pipe_closed_packet(failure_message)
    }

    #[no_mangle]
    pub unsafe extern "C" fn morloc_packet_is_pipe_closed(packet: *const u8) -> bool {
        super::morloc_packet_is_pipe_closed(packet)
    }

    #[no_mangle]
    pub unsafe extern "C" fn get_morloc_data_packet_error_message(data: *const u8, errmsg: *mut *mut c_char) -> *mut c_char {
        super::get_morloc_data_packet_error_message(data, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn get_morloc_data_packet_value(data: *const u8, schema: *const CSchema, errmsg: *mut *mut c_char) -> *mut u8 {
        super::get_morloc_data_packet_value(data, schema, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn morloc_inherit_reply(packet: *const u8) {
        super::morloc_inherit_reply(packet)
    }

    #[no_mangle]
    pub unsafe extern "C" fn make_morloc_local_call_packet(midx: u32, arg_packets: *const *const u8, nargs: usize, errmsg: *mut *mut c_char) -> *mut u8 {
        super::make_morloc_local_call_packet(midx, arg_packets, nargs, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn read_morloc_call_packet(packet: *const u8, errmsg: *mut *mut c_char) -> *mut MorlocCall {
        super::read_morloc_call_packet(packet, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn free_morloc_call(call: *mut MorlocCall) {
        super::free_morloc_call(call)
    }

    #[no_mangle]
    pub unsafe extern "C" fn make_data_packet_auto(voidstar: *mut c_void, relptr: RelPtr, schema: *const CSchema, errmsg: *mut *mut c_char) -> *mut u8 {
        super::make_data_packet_auto(voidstar, relptr, schema, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn make_inline_data_packet(voidstar: *mut c_void, schema: *const CSchema, errmsg: *mut *mut c_char) -> *mut u8 {
        super::make_inline_data_packet(voidstar, schema, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn normalize_data_packet_for_output(packet: *const u8, packet_size: usize, compression_level: u8, out_buf: *mut *mut u8, out_size: *mut usize, errmsg: *mut *mut c_char) -> i32 {
        super::normalize_data_packet_for_output(packet, packet_size, compression_level, out_buf, out_size, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn normalize_data_packet_to_fd(packet: *const u8, packet_size: usize, compression_level: u8, fd: libc::c_int, errmsg: *mut *mut c_char) -> i64 {
        super::normalize_data_packet_to_fd(packet, packet_size, compression_level, fd, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn print_morloc_data_packet(packet: *const u8, schema: *const CSchema, errmsg: *mut *mut c_char) -> i32 {
        super::print_morloc_data_packet(packet, schema, errmsg)
    }
}

#[cfg(test)]
mod packet_file_tests {
    #[test]
    fn a_file_packet_never_writes_through_a_planted_name() {
        let dir = std::env::temp_dir().join(format!("mlc-pkt-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        let victim = dir.join("victim");
        std::fs::write(&victim, b"untouched").unwrap();
        std::os::unix::fs::symlink(&victim, dir.join("morloc-pkt-7-0.mpk")).unwrap();
        let mut seq = 0;
        let path = super::create_packet_file(&dir, 7, || { seq += 1; seq - 1 }, b"packet").unwrap();
        assert_eq!(path, dir.join("morloc-pkt-7-1.mpk"));
        assert_eq!(std::fs::read(&victim).unwrap(), b"untouched");
        assert_eq!(std::fs::read(&path).unwrap(), b"packet");
        let _ = std::fs::remove_dir_all(&dir);
    }
}

#[cfg(test)]
mod generation_tests {
    use super::*;

    // SHM-13: a reference packet names the allocation it was made for; one
    // whose block was released and reused is refused, and disarmed so that
    // no later release through it touches the new owner's block.
    #[test]
    fn a_packet_naming_a_reused_block_is_refused_and_disarmed() {
        let _arena = crate::own_test_shm();
        let _fallback = crate::shm::tests::small_volume("gp");
        let schema = crate::schema::Schema::primitive(crate::schema::SerialType::Uint8);
        let cs = CSchema::from_rust(&schema);
        let abs = crate::shm::shmalloc(64).expect("allocate");
        let packet = unsafe { make_standard_data_packet(crate::shm::abs2rel(abs).unwrap(), cs) };
        crate::shm::shfree(abs).unwrap();
        let (again, held) = crate::shm::tests::reallocate_at(abs, 64);
        let mut err: *mut c_char = ptr::null_mut();
        let got = unsafe { get_morloc_data_packet_value(packet, cs, &mut err) };
        let disarmed = unsafe { packet_rptr_block(packet) }.ok().flatten().is_none();
        let count = unsafe { crate::shm::reference_count(again) };
        for p in held.into_iter().chain([again]) {
            crate::shm::shfree(p).unwrap();
        }
        crate::shm::shclose().unwrap();
        unsafe {
            if !err.is_null() {
                libc::free(err as *mut c_void);
            }
            libc::free(packet as *mut c_void);
            CSchema::free(cs);
        }
        assert!(got.is_null() && !err.is_null(), "a packet for a released block was read");
        assert!(disarmed, "the refused packet still names the block");
        assert_eq!(count, Some(1), "the new owner's count changed");
    }

    #[test]
    fn a_packet_for_a_live_block_is_read() {
        let _shm = crate::own_test_registry();
        let schema = crate::schema::Schema::primitive(crate::schema::SerialType::Uint8);
        let cs = CSchema::from_rust(&schema);
        let abs = crate::shm::shmalloc(64).expect("allocate");
        let packet = unsafe { make_standard_data_packet(crate::shm::abs2rel(abs).unwrap(), cs) };
        let mut err: *mut c_char = ptr::null_mut();
        let got = unsafe { get_morloc_data_packet_value(packet, cs, &mut err) };
        crate::shm::shfree(abs).unwrap();
        unsafe {
            libc::free(packet as *mut c_void);
            CSchema::free(cs);
        }
        assert!(err.is_null());
        assert_eq!(got, abs as *mut u8);
    }
}

#[cfg(test)]
mod call_packet_bounds_tests {
    use super::*;

    unsafe fn call_with_one_arg() -> Vec<u8> {
        let arg = make_fail_packet(c"x".as_ptr());
        let args = [arg as *const u8];
        let mut err: *mut c_char = ptr::null_mut();
        let call = make_morloc_local_call_packet(7, args.as_ptr(), 1, &mut err);
        assert!(err.is_null());
        let size = morloc_packet_size(call, &mut err);
        let bytes = std::slice::from_raw_parts(call, size).to_vec();
        libc::free(call as *mut c_void);
        libc::free(arg as *mut c_void);
        bytes
    }

    unsafe fn read_call(bytes: &[u8]) -> Result<usize, String> {
        let mut err: *mut c_char = ptr::null_mut();
        let call = read_morloc_call_packet(bytes.as_ptr(), &mut err);
        if call.is_null() {
            let msg = CStr::from_ptr(err).to_string_lossy().into_owned();
            libc::free(err as *mut c_void);
            return Err(msg);
        }
        let n = (*call).nargs;
        free_morloc_call(call);
        Ok(n)
    }

    #[test]
    fn a_well_formed_call_packet_reads_its_arguments() {
        unsafe { assert_eq!(read_call(&call_with_one_arg()), Ok(1)) };
    }

    /// An argument whose own header claims more bytes than the call packet
    /// holds is refused before anything past the packet is read.
    #[test]
    fn an_argument_past_the_call_packets_end_is_refused() {
        unsafe {
            let mut bytes = call_with_one_arg();
            let arg_start = 32 + u32::from_le_bytes(bytes[20..24].try_into().unwrap()) as usize;
            let arg_length_at = arg_start + 24;
            bytes[arg_length_at..arg_length_at + 8].copy_from_slice(&(1u64 << 20).to_le_bytes());
            let msg = read_call(&bytes).unwrap_err();
            assert!(msg.contains("runs past the packet's end"), "{msg}");
        }
    }
}
