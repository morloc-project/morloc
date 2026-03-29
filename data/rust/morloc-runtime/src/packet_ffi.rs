//! C ABI wrappers for packet functions.
//! Replaces packet.c with calls to Rust packet.rs + voidstar.rs.

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

#[no_mangle]
pub unsafe extern "C" fn read_morloc_packet_header(
    msg: *const u8,
    errmsg: *mut *mut c_char,
) -> *mut PacketHeader {
    clear_errmsg(errmsg);
    if msg.is_null() {
        set_errmsg(errmsg, &MorlocError::Packet("Cannot make packet from NULL pointer".into()));
        return ptr::null_mut();
    }
    // Validate magic
    let header = &*(msg as *const PacketHeader);
    if !header.is_valid() {
        set_errmsg(errmsg, &MorlocError::Packet("Malformed morloc packet".into()));
        return ptr::null_mut();
    }
    msg as *mut PacketHeader
}

#[no_mangle]
pub unsafe extern "C" fn packet_is_ping(
    packet: *const u8,
    errmsg: *mut *mut c_char,
) -> bool {
    clear_errmsg(errmsg);
    let header = read_morloc_packet_header(packet, errmsg);
    if header.is_null() { return false; }
    (*header).is_ping()
}

#[no_mangle]
pub unsafe extern "C" fn packet_is_local_call(
    packet: *const u8,
    errmsg: *mut *mut c_char,
) -> bool {
    clear_errmsg(errmsg);
    let header = read_morloc_packet_header(packet, errmsg);
    if header.is_null() { return false; }
    (*header).is_local_call()
}

#[no_mangle]
pub unsafe extern "C" fn packet_is_remote_call(
    packet: *const u8,
    errmsg: *mut *mut c_char,
) -> bool {
    clear_errmsg(errmsg);
    let header = read_morloc_packet_header(packet, errmsg);
    if header.is_null() { return false; }
    (*header).is_remote_call()
}

// ── Packet size ──────────────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn morloc_packet_size_from_header(
    header: *const PacketHeader,
) -> usize {
    if header.is_null() { return 0; }
    32 + (*header).offset as usize + (*header).length as usize
}

#[no_mangle]
pub unsafe extern "C" fn morloc_packet_size(
    packet: *const u8,
    errmsg: *mut *mut c_char,
) -> usize {
    clear_errmsg(errmsg);
    let header = read_morloc_packet_header(packet, errmsg);
    if header.is_null() { return 0; }
    morloc_packet_size_from_header(header)
}

// ── Ping ─────────────────────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn return_ping(
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
        set_errmsg(errmsg, &MorlocError::Packet("Failed to allocate ping response".into()));
        return ptr::null_mut();
    }
    ptr::copy_nonoverlapping(packet, buf, size);
    buf
}

#[no_mangle]
pub extern "C" fn make_ping_packet() -> *mut u8 {
    // SAFETY: calloc returns null or a valid pointer to 32 zeroed bytes.
    let buf = unsafe { libc::calloc(32, 1) as *mut u8 };
    if buf.is_null() { return ptr::null_mut(); }
    let header = PacketHeader::ping();
    let bytes = header.to_bytes();
    // SAFETY: buf points to 32 bytes; bytes is exactly 32 bytes from to_bytes().
    unsafe { ptr::copy_nonoverlapping(bytes.as_ptr(), buf, 32) };
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

    // Write metadata header
    *metadata = b'm';
    *metadata.add(1) = b'm';
    *metadata.add(2) = b'h';
    *metadata.add(3) = METADATA_TYPE_SCHEMA_STRING;
    *(metadata.add(4) as *mut u32) = schema_len as u32;

    // Write schema string
    ptr::copy_nonoverlapping(schema_bytes.as_ptr(), metadata.add(meta_header_size), schema_bytes.len());
    // Null terminator already zeroed by calloc

    (metadata, padded_meta_len)
}

/// Generic data packet builder matching C's make_morloc_data_packet.
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
        padding: [0; 2],
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

#[no_mangle]
pub unsafe extern "C" fn make_standard_data_packet(
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

    // Write the relptr into the payload area
    let header = &*(packet as *const PacketHeader);
    let payload_offset = 32 + header.offset as usize;
    *(packet.add(payload_offset) as *mut RelPtr) = relptr;

    packet
}

#[no_mangle]
pub unsafe extern "C" fn make_arrow_data_packet(
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

    packet
}

// ── Msgpack packets ──────────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn make_mpk_data_packet(
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

#[no_mangle]
pub unsafe extern "C" fn make_data_packet_from_mpk(
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

// ── get_data_packet_as_mpk ───────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn get_data_packet_as_mpk(
    packet: *const u8,
    schema: *const CSchema,
    mpk_out: *mut *mut c_char,
    mpk_size_out: *mut usize,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    *mpk_out = ptr::null_mut();
    *mpk_size_out = 0;

    let header = read_morloc_packet_header(packet, errmsg);
    if header.is_null() { return 0; }

    if (*header).command_type() != PACKET_TYPE_DATA {
        set_errmsg(errmsg, &MorlocError::Packet("Expected a data packet".into()));
        return 0;
    }

    // Check for error
    if (*header).is_fail() {
        let payload_start = 32 + (*header).offset as usize;
        let payload_len = (*header).length as usize;
        let msg = std::str::from_utf8_unchecked(
            std::slice::from_raw_parts(packet.add(payload_start), payload_len)
        );
        set_errmsg(errmsg, &MorlocError::Packet(format!("\n{}", msg)));
        return 0;
    }

    let rs = CSchema::to_rust(schema);
    let source = (*header).command.data.source;
    let format = (*header).command.data.format;
    let payload_start = 32 + (*header).offset as usize;
    let payload_len = (*header).length as usize;
    let payload = std::slice::from_raw_parts(packet.add(payload_start), payload_len);

    if source == PACKET_SOURCE_MESG && format == PACKET_FORMAT_MSGPACK {
        // Inline msgpack: copy directly
        let buf = libc::malloc(payload_len) as *mut c_char;
        if buf.is_null() {
            set_errmsg(errmsg, &MorlocError::Packet("malloc failed".into()));
            return 0;
        }
        ptr::copy_nonoverlapping(payload.as_ptr(), buf as *mut u8, payload_len);
        *mpk_out = buf;
        *mpk_size_out = payload_len;
    } else if source == PACKET_SOURCE_MESG && format == PACKET_FORMAT_VOIDSTAR {
        // Inline voidstar: load into SHM then convert to msgpack
        match crate::voidstar::read_binary(payload, &rs) {
            Ok(abs) => {
                match crate::mpack::pack_with_schema(abs, &rs) {
                    Ok(data) => {
                        let buf = libc::malloc(data.len()) as *mut u8;
                        if buf.is_null() {
                            set_errmsg(errmsg, &MorlocError::Packet("malloc failed".into()));
                            return 0;
                        }
                        ptr::copy_nonoverlapping(data.as_ptr(), buf, data.len());
                        *mpk_out = buf as *mut c_char;
                        *mpk_size_out = data.len();
                    }
                    Err(e) => { set_errmsg(errmsg, &e); return 0; }
                }
                // Free SHM
                let _ = crate::voidstar::free_by_schema(abs, &rs);
                let _ = shm::shfree(abs);
            }
            Err(e) => { set_errmsg(errmsg, &e); return 0; }
        }
    } else if source == PACKET_SOURCE_FILE && format == PACKET_FORMAT_MSGPACK {
        // File-based msgpack: read the file
        let filename_bytes = &payload[..payload_len.min(4096)];
        let filename = std::str::from_utf8(filename_bytes).unwrap_or("");
        let filename = filename.trim_end_matches('\0');
        match std::fs::read(filename) {
            Ok(data) => {
                let buf = libc::malloc(data.len()) as *mut u8;
                if buf.is_null() {
                    set_errmsg(errmsg, &MorlocError::Packet("malloc failed".into()));
                    return 0;
                }
                ptr::copy_nonoverlapping(data.as_ptr(), buf, data.len());
                *mpk_out = buf as *mut c_char;
                *mpk_size_out = data.len();
            }
            Err(e) => {
                set_errmsg(errmsg, &MorlocError::Io(e));
                return 0;
            }
        }
    } else if source == PACKET_SOURCE_RPTR && format == PACKET_FORMAT_VOIDSTAR {
        // Voidstar via relptr: convert to msgpack
        let relptr = *(payload.as_ptr() as *const RelPtr);
        match shm::rel2abs(relptr) {
            Ok(abs) => {
                match crate::mpack::pack_with_schema(abs, &rs) {
                    Ok(data) => {
                        let buf = libc::malloc(data.len()) as *mut u8;
                        if buf.is_null() {
                            set_errmsg(errmsg, &MorlocError::Packet("malloc failed".into()));
                            return 0;
                        }
                        ptr::copy_nonoverlapping(data.as_ptr(), buf, data.len());
                        *mpk_out = buf as *mut c_char;
                        *mpk_size_out = data.len();
                    }
                    Err(e) => { set_errmsg(errmsg, &e); return 0; }
                }
            }
            Err(e) => { set_errmsg(errmsg, &e); return 0; }
        }
    } else {
        set_errmsg(errmsg, &MorlocError::Packet(
            format!("Unsupported packet source/format: 0x{:02x}/0x{:02x}", source, format)
        ));
        return 0;
    }

    1 // true
}

// ── Schema from metadata ─────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn read_schema_from_packet_meta(
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
                // Return pointer into the packet buffer (matches C behavior)
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

#[no_mangle]
pub unsafe extern "C" fn make_fail_packet(
    failure_message: *const c_char,
) -> *mut u8 {
    if failure_message.is_null() { return ptr::null_mut(); }
    let msg = CStr::from_ptr(failure_message).to_bytes();
    make_data_packet_raw(
        msg.as_ptr(),
        msg.len(),
        ptr::null(),
        0,
        PACKET_SOURCE_MESG,
        PACKET_FORMAT_TEXT,
        PACKET_COMPRESSION_NONE,
        PACKET_ENCRYPTION_NONE,
        PACKET_STATUS_FAIL,
    )
}

// ── Error message extraction ─────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn get_morloc_data_packet_error_message(
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
            set_errmsg(errmsg, &MorlocError::Packet("Failed to allocate error message".into()));
            return ptr::null_mut();
        }
        ptr::copy_nonoverlapping(data.add(payload_start), buf as *mut u8, payload_len);
        return buf;
    }

    ptr::null_mut()
}

// ── get_morloc_data_packet_value ─────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn get_morloc_data_packet_value(
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

    // Check for error
    let packet_error = get_morloc_data_packet_error_message(data, errmsg);
    if !packet_error.is_null() {
        let err_str = CStr::from_ptr(packet_error).to_string_lossy().into_owned();
        libc::free(packet_error as *mut c_void);
        set_errmsg(errmsg, &MorlocError::Packet(format!("\n{}", err_str)));
        return ptr::null_mut();
    }
    // Clear any errmsg from get_morloc_data_packet_error_message
    clear_errmsg(errmsg);

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
            } else {
                set_errmsg(errmsg, &MorlocError::Packet(
                    format!("Invalid format from mesg: 0x{:02x}", format)
                ));
                ptr::null_mut()
            }
        }
        PACKET_SOURCE_FILE => {
            if format == PACKET_FORMAT_MSGPACK {
                let filename_bytes = std::slice::from_raw_parts(data.add(payload_start), payload_len.min(4096));
                let filename = std::str::from_utf8(filename_bytes).unwrap_or("");
                let filename = filename.trim_end_matches('\0');
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
            } else {
                set_errmsg(errmsg, &MorlocError::Packet(
                    format!("Invalid format from file: 0x{:02x}", format)
                ));
                ptr::null_mut()
            }
        }
        PACKET_SOURCE_RPTR => {
            if format == PACKET_FORMAT_VOIDSTAR || format == PACKET_FORMAT_ARROW {
                let relptr = *(data.add(payload_start) as *const RelPtr);
                match shm::rel2abs(relptr) {
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

unsafe fn make_call_packet_gen(
    midx: u32,
    entrypoint: u8,
    arg_packets: *const *const u8,
    nargs: usize,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);

    // Calculate total data length
    let mut data_length: usize = 0;
    for i in 0..nargs {
        let arg = read_morloc_packet_header(*arg_packets.add(i), errmsg);
        if arg.is_null() { return ptr::null_mut(); }
        data_length += morloc_packet_size_from_header(arg);
    }

    let total = 32 + data_length;
    let packet = libc::calloc(total, 1) as *mut u8;
    if packet.is_null() {
        set_errmsg(errmsg, &MorlocError::Packet("Failed to allocate call packet".into()));
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

#[no_mangle]
pub unsafe extern "C" fn make_morloc_local_call_packet(
    midx: u32,
    arg_packets: *const *const u8,
    nargs: usize,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    make_call_packet_gen(midx, PACKET_ENTRYPOINT_LOCAL, arg_packets, nargs, errmsg)
}

#[no_mangle]
pub unsafe extern "C" fn make_morloc_remote_call_packet(
    midx: u32,
    arg_packets: *const *const u8,
    nargs: usize,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    make_call_packet_gen(midx, PACKET_ENTRYPOINT_REMOTE_SFS, arg_packets, nargs, errmsg)
}

// ── Call packet reading ──────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn read_morloc_call_packet(
    packet: *const u8,
    errmsg: *mut *mut c_char,
) -> *mut MorlocCall {
    clear_errmsg(errmsg);

    let call = libc::calloc(1, std::mem::size_of::<MorlocCall>()) as *mut MorlocCall;
    if call.is_null() {
        set_errmsg(errmsg, &MorlocError::Packet("calloc failed".into()));
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
    let end_pos = start_pos + (*header).length as usize;

    // First pass: count args
    let mut pos = start_pos;
    while pos < end_pos {
        let arg_size = morloc_packet_size(packet.add(pos), errmsg);
        if arg_size == 0 {
            free_morloc_call(call);
            return ptr::null_mut();
        }
        pos += arg_size;
        (*call).nargs += 1;
    }

    // Allocate args array
    (*call).args = libc::calloc((*call).nargs, std::mem::size_of::<*mut u8>()) as *mut *mut u8;
    if (*call).args.is_null() {
        set_errmsg(errmsg, &MorlocError::Packet("calloc failed for args".into()));
        libc::free(call as *mut c_void);
        return ptr::null_mut();
    }

    // Second pass: fill pointers (borrowing into original packet)
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

#[no_mangle]
pub unsafe extern "C" fn free_morloc_call(call: *mut MorlocCall) {
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

// adjust_voidstar_relptrs: still provided by cli.c (will move to Rust when cli.c is ported)
// read_voidstar_binary: still provided by cli.c (will move to Rust when cli.c is ported)

// ── write_voidstar_binary (for intrinsics.c) ─────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn write_voidstar_binary(
    fd: i32,
    data: *const c_void,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> RelPtr {
    clear_errmsg(errmsg);
    let rs = CSchema::to_rust(schema);
    match crate::voidstar::write_binary_to_fd(fd, data as AbsPtr, &rs) {
        Ok(n) => n as RelPtr,
        Err(e) => {
            set_errmsg(errmsg, &e);
            -1isize as RelPtr
        }
    }
}

// ── flatten_voidstar_to_buffer ───────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn flatten_voidstar_to_buffer(
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
                set_errmsg(errmsg, &MorlocError::Packet("malloc failed".into()));
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

// read_voidstar_binary: still provided by cli.c (will move to Rust when cli.c is ported)

// ── make_data_packet_auto ────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn make_data_packet_auto(
    voidstar: *mut c_void,
    relptr: RelPtr,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);
    let rs = CSchema::to_rust(schema);

    let flat_size = match crate::ffi::calc_voidstar_size_inner(voidstar as *const u8, &rs) {
        Ok(s) => s,
        Err(e) => {
            set_errmsg(errmsg, &e);
            return ptr::null_mut();
        }
    };

    if flat_size <= MORLOC_INLINE_THRESHOLD {
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
                    set_errmsg(errmsg, &MorlocError::Packet("Failed to create inline data packet".into()));
                }
                return packet;
            }
            Err(e) => {
                set_errmsg(errmsg, &e);
                return ptr::null_mut();
            }
        }
    }

    make_standard_data_packet(relptr, schema)
}

// ── print_morloc_data_packet ─────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn print_morloc_data_packet(
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

    // Check for error
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
            // Print the raw packet bytes
            if print_binary(packet, packet_size, errmsg) != 0 {
                return 1;
            }
        }
        PACKET_SOURCE_RPTR => {
            match format {
                PACKET_FORMAT_VOIDSTAR => {
                    let payload_start = 32 + (*header).offset as usize;
                    let relptr = *(packet.add(payload_start) as *const RelPtr);
                    let voidstar_ptr = match shm::rel2abs(relptr) {
                        Ok(p) => p,
                        Err(e) => { set_errmsg(errmsg, &e); return 1; }
                    };

                    // Build modified header with flat size
                    let flat_size = match crate::ffi::calc_voidstar_size_inner(voidstar_ptr, &rs) {
                        Ok(s) => s,
                        Err(e) => { set_errmsg(errmsg, &e); return 1; }
                    };

                    let mut new_header = *header;
                    new_header.command.data.format = PACKET_FORMAT_VOIDSTAR;
                    // Safely set length (packed struct)
                    let new_hdr_ptr = &mut new_header as *mut PacketHeader as *mut u8;
                    *(new_hdr_ptr.add(24) as *mut u64) = flat_size as u64;

                    // Print header
                    if print_binary(&new_header as *const PacketHeader as *const u8, 32, errmsg) != 0 {
                        return 1;
                    }

                    // Print metadata
                    let offset = (*header).offset as usize;
                    if offset > 0 {
                        if print_binary(packet.add(32), offset, errmsg) != 0 {
                            return 1;
                        }
                    }

                    // Write flattened voidstar data to stdout
                    match crate::voidstar::write_binary_to_fd(libc::STDOUT_FILENO, voidstar_ptr, &rs) {
                        Ok(_) => {}
                        Err(e) => { set_errmsg(errmsg, &e); return 1; }
                    }
                }
                _ => {
                    // Other formats: print raw packet
                    if print_binary(packet, packet_size, errmsg) != 0 {
                        return 1;
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
    let mut written: usize = 0;
    while written < count {
        let n = libc::write(
            libc::STDOUT_FILENO,
            buf.add(written) as *const c_void,
            count - written,
        );
        if n < 0 {
            set_errmsg(errmsg, &MorlocError::Io(std::io::Error::last_os_error()));
            return 1;
        }
        written += n as usize;
    }
    0
}
