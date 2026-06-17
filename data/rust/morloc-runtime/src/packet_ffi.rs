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
                // Free SHM. shm::shfree zeros the whole block on final ref-drop;
                // since `abs` came from read_binary's single-block layout, one
                // shfree releases the wrapper and all packed sub-data.
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

    let threshold = crate::packet::inline_threshold();
    if flat_size <= threshold {
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

    if crate::packet::shm_enabled() {
        make_standard_data_packet(relptr, schema)
    } else {
        make_file_data_packet_voidstar(voidstar, schema, &rs, errmsg)
    }
}

// ── normalize_data_packet_for_output ─────────────────────────────────────────
//
// Produce a self-contained packet suitable for writing to disk. The
// pool may return a result packet whose source is RPTR (an 8-byte
// relptr into SHM) or FILE (a filename whose contents are msgpack);
// neither survives the lifetime of the producing process. This helper
// rewrites such packets so the payload is inline (source = MESG),
// then optionally applies zstd compression.
//
// Normalization rules (source x format x compression):
//
//   MESG + any format, compression = NONE
//      level > 0: compress_packet (sets compression = ZSTD)
//      level = 0: copy through unchanged
//
//   MESG + any format, compression = ZSTD
//      copy through unchanged (do not recompress)
//
//   RPTR + VOIDSTAR
//      dereference SHM, flatten voidstar to a flat buffer (the same
//      writer the SLURM serialization path uses), wrap as
//      MESG + VOIDSTAR, then compress if level > 0.
//
//   RPTR + ARROW
//      dereference SHM, serialize the Arrow SHM table to IPC bytes
//      (existing write_arrow_ipc_to_buffer), wrap as MESG + ARROW,
//      then compress if level > 0.
//
//   FILE + MSGPACK
//      read the file, wrap the msgpack bytes as MESG + MSGPACK, then
//      compress if level > 0. Inlining keeps the on-disk packet
//      meaningful after the producing process exits.
//
// FAIL packets and non-DATA packets (CALL/PING) pass through.
//
// The output buffer is libc::malloc'd; the caller must libc::free.
#[no_mangle]
pub unsafe extern "C" fn normalize_data_packet_for_output(
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

    let is_data = (*header).is_data();
    let is_fail = (*header).is_fail();

    // CALL/PING and FAIL packets ride out unchanged.
    if !is_data || is_fail {
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

    // MESG: either pass through or just compress the existing payload.
    if source == PACKET_SOURCE_MESG {
        if compression_level == 0 || compression != PACKET_COMPRESSION_NONE {
            return alloc_out(packet, packet_size, out_buf, out_size, errmsg);
        }
        return compress_inline(packet, packet_size, compression_level, out_buf, out_size, errmsg);
    }

    // RPTR or FILE: materialize the payload into inline bytes, then
    // build a fresh MESG packet, then (optionally) compress.
    let new_payload: Vec<u8> = match source {
        PACKET_SOURCE_RPTR => {
            match read_rptr_payload(packet, payload_start, format, errmsg) {
                Some(v) => v,
                None => return 1,
            }
        }
        PACKET_SOURCE_FILE => {
            if format != PACKET_FORMAT_MSGPACK {
                set_errmsg(errmsg, &MorlocError::Packet(format!(
                    "FILE source with unsupported format 0x{:02x}", format
                )));
                return 1;
            }
            let filename_bytes = std::slice::from_raw_parts(
                packet.add(payload_start),
                payload_len.min(4096),
            );
            let filename = std::str::from_utf8(filename_bytes).unwrap_or("");
            let filename = filename.trim_end_matches('\0');
            match std::fs::read(filename) {
                Ok(data) => data,
                Err(e) => {
                    set_errmsg(errmsg, &MorlocError::Io(e));
                    return 1;
                }
            }
        }
        _ => {
            set_errmsg(errmsg, &MorlocError::Packet(format!(
                "unsupported packet source 0x{:02x}", source
            )));
            return 1;
        }
    };

    // Build MESG packet: same metadata, new payload, source = MESG,
    // compression = NONE, format and schema unchanged.
    let mut new_header = *header;
    new_header.command.data.source = PACKET_SOURCE_MESG;
    new_header.command.data.compression = PACKET_COMPRESSION_NONE;
    let mut hdr_bytes = new_header.to_bytes();
    let new_len = new_payload.len() as u64;
    hdr_bytes[24..32].copy_from_slice(&new_len.to_le_bytes());

    let mut mesg_packet = Vec::with_capacity(32 + metadata_size + new_payload.len());
    mesg_packet.extend_from_slice(&hdr_bytes);
    if metadata_size > 0 {
        let meta = std::slice::from_raw_parts(packet.add(32), metadata_size);
        mesg_packet.extend_from_slice(meta);
    }
    mesg_packet.extend_from_slice(&new_payload);

    // Optional compression.
    let final_bytes = if compression_level > 0 {
        match crate::compression::CompressionLevel::from_u8(compression_level) {
            Ok(lvl) if !lvl.is_none() => {
                match crate::compression::compress_packet(&mesg_packet, lvl) {
                    Ok(crate::compression::CompressOutcome::Compressed(v)) => v,
                    Ok(crate::compression::CompressOutcome::NoOp) => mesg_packet,
                    Err(e) => { set_errmsg(errmsg, &e); return 1; }
                }
            }
            Ok(_) => mesg_packet,
            Err(e) => { set_errmsg(errmsg, &e); return 1; }
        }
    } else {
        mesg_packet
    };

    alloc_out(final_bytes.as_ptr(), final_bytes.len(), out_buf, out_size, errmsg)
}

// Materialize the SHM-backed payload of an RPTR packet into a flat
// byte buffer suitable for inlining.
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
                if (*errmsg).is_null() {
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

// Compress a MESG packet that is already inline and uncompressed. The
// caller has already verified source == MESG and compression == NONE.
unsafe fn compress_inline(
    packet: *const u8,
    packet_size: usize,
    level: u8,
    out_buf: *mut *mut u8,
    out_size: *mut usize,
    errmsg: *mut *mut c_char,
) -> i32 {
    let lvl = match crate::compression::CompressionLevel::from_u8(level) {
        Ok(l) => l,
        Err(e) => { set_errmsg(errmsg, &e); return 1; }
    };
    if lvl.is_none() {
        return alloc_out(packet, packet_size, out_buf, out_size, errmsg);
    }
    let slice = std::slice::from_raw_parts(packet, packet_size);
    match crate::compression::compress_packet(slice, lvl) {
        Ok(crate::compression::CompressOutcome::Compressed(v)) => {
            alloc_out(v.as_ptr(), v.len(), out_buf, out_size, errmsg)
        }
        Ok(crate::compression::CompressOutcome::NoOp) => {
            alloc_out(packet, packet_size, out_buf, out_size, errmsg)
        }
        Err(e) => { set_errmsg(errmsg, &e); 1 }
    }
}

// libc::malloc a copy of `src[..len]` and hand it back through the
// (out_buf, out_size) outparams. Used by every success path of
// normalize_data_packet_for_output so the C ABI is uniform.
unsafe fn alloc_out(
    src: *const u8,
    len: usize,
    out_buf: *mut *mut u8,
    out_size: *mut usize,
    errmsg: *mut *mut c_char,
) -> i32 {
    let mem = libc::malloc(len) as *mut u8;
    if mem.is_null() {
        set_errmsg(errmsg, &MorlocError::Packet("malloc failed".into()));
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
// FILE-routed data path for `morloc make --no-shm` (or when SHM is
// disabled at runtime). Serializes the voidstar with the same
// msgpack writer used for cross-language transport, drops the bytes
// into a unique temp file, and emits a FILE+MSGPACK packet pointing
// at that path. The file is registered with `eval_arena` so it is
// unlinked when the surrounding eval scope ends. The receiver reads
// the file in `get_data_packet_as_mpk` / `get_morloc_data_packet_value`
// (existing FILE+MSGPACK branch).
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

    // Persistence rule: a user-supplied `--tmpdir` (manifest /
    // env var / FFI setter) means "I picked this location to inspect
    // files, don't delete them out from under me". Default behavior
    // (no override) puts files in $TMPDIR and registers them with
    // eval_arena for cleanup at end-of-eval.
    let user_dir = crate::packet::file_packet_tmpdir();
    let persistent = user_dir.is_some();
    let dir: std::path::PathBuf = match user_dir {
        Some(d) => std::path::PathBuf::from(d),
        None    => std::env::temp_dir(),
    };
    // Create the directory if a user-specified path doesn't exist yet.
    // std::env::temp_dir() always exists, but a manifest-supplied
    // `--tmpdir` may not.
    if let Err(e) = std::fs::create_dir_all(&dir) {
        set_errmsg(errmsg, &MorlocError::Io(e));
        return ptr::null_mut();
    }
    let pid = std::process::id();
    let seq = SEQ.fetch_add(1, Ordering::Relaxed);
    let path = dir.join(format!("morloc-pkt-{}-{}.mpk", pid, seq));

    if let Err(e) = std::fs::write(&path, &mpk) {
        set_errmsg(errmsg, &MorlocError::Io(e));
        return ptr::null_mut();
    }

    let path_str = match path.to_str() {
        Some(s) => s,
        None => {
            set_errmsg(errmsg, &MorlocError::Packet("Non-UTF8 temp path for file packet".into()));
            let _ = std::fs::remove_file(&path);
            return ptr::null_mut();
        }
    };
    let path_cstr = match std::ffi::CString::new(path_str) {
        Ok(c) => c,
        Err(_) => {
            set_errmsg(errmsg, &MorlocError::Packet("Embedded NUL in temp path for file packet".into()));
            let _ = std::fs::remove_file(&path);
            return ptr::null_mut();
        }
    };

    let packet = make_mpk_data_packet(path_cstr.as_ptr(), schema);
    if packet.is_null() {
        set_errmsg(errmsg, &MorlocError::Packet("Failed to create FILE data packet".into()));
        let _ = std::fs::remove_file(&path);
        return ptr::null_mut();
    }

    // Only track for auto-cleanup when no user override is in effect.
    // A `--tmpdir` user is expecting to inspect these files; an
    // automatic unlink would defeat the purpose.
    if !persistent {
        crate::eval_arena::record_file_if_active(path);
    }
    packet
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

// ── Tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod auto_routing_tests {
    //! Verifies the inline-vs-RPTR routing decision in
    //! `make_data_packet_auto`. The packet header layout puts the
    //! `source` byte at offset 13 (4 magic + 2 plain + 2 version + 2
    //! flavor + 2 mode + 1 cmd_type = 13). PACKET_SOURCE_MESG = 0x00
    //! triggers the inline path; PACKET_SOURCE_RPTR = 0x02 ships the
    //! relptr. Tests synthesize an `Array<u8>` voidstar of a chosen
    //! flat size, run the routing function, and inspect the resulting
    //! packet's source byte.
    //!
    //! The flat size of an `Array<u8>` of N data bytes is `16 + N`
    //! (an `Array` wrapper is 16 bytes: usize size + isize relptr).
    //! `MORLOC_INLINE_THRESHOLD` is 65536 bytes. So:
    //!   - N <= 65520 → flat <= 65536 → MESG (inline)
    //!   - N >  65520 → flat >  65536 → RPTR
    //!
    //! Sizes far from the boundary are picked to avoid the test
    //! becoming alignment-sensitive; the boundary itself is checked
    //! once each side.
    use super::*;
    use crate::packet::{
        MORLOC_INLINE_THRESHOLD, PACKET_SOURCE_MESG, PACKET_SOURCE_RPTR,
        TEST_CONFIG_LOCK,
    };
    use crate::shm::{self, Array, RelPtr};

    /// Byte offset of the `source` field inside the 32-byte packet
    /// header (see `PacketHeader` in `packet.rs`).
    const SOURCE_OFFSET: usize = 13;

    fn ensure_shm() {
        crate::init_test_shm();
    }

    /// Build a flat blob laid out as `Array<u8>` with `n` data bytes,
    /// hand it to `voidstar::read_binary` to materialize into SHM, and
    /// return both the abs-ptr and the relptr ready to feed into
    /// `make_data_packet_auto`.
    unsafe fn build_byte_array_voidstar(n: usize) -> (AbsPtr, RelPtr) {
        let total = std::mem::size_of::<Array>() + n;
        let mut blob = vec![0u8; total];
        // Wrapper: size = n, data = relative offset 16 within the blob
        // (read_binary's adjust_relptrs adds the global base so it
        // resolves to the byte-array region after copy).
        let arr = Array {
            size: n,
            data: std::mem::size_of::<Array>() as RelPtr,
        };
        std::ptr::copy_nonoverlapping(
            &arr as *const Array as *const u8,
            blob.as_mut_ptr(),
            std::mem::size_of::<Array>(),
        );
        // Fill data with a recognizable pattern so any in-place
        // corruption later would be visible (the pattern itself is
        // not asserted on; tests only check the source byte).
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

    /// Run `make_data_packet_auto` on a synthetic byte-array voidstar
    /// of size `n` and return the `source` byte from the resulting
    /// packet header.
    ///
    /// Acquires [`TEST_CONFIG_LOCK`] for the duration of the call so
    /// the read of [`crate::packet::inline_threshold`] inside
    /// `make_data_packet_auto` can't race with a write from a
    /// concurrent `config_ffi::tests` test.
    fn auto_route_source_for(n: usize) -> u8 {
        let _guard = TEST_CONFIG_LOCK.lock().unwrap();
        ensure_shm();
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
            // Sanity: cmd_type at offset 12 should be DATA.
            assert_eq!(
                *packet.add(12),
                crate::packet::PACKET_TYPE_DATA,
                "expected DATA packet for N={}",
                n
            );
            libc::free(packet as *mut c_void);
            crate::cschema::CSchema::free(cs);
            // Best-effort SHM cleanup so the small test pool isn't
            // exhausted across many tests; arena isn't active here.
            let _ = shm::shfree(abs);
            source
        }
    }

    #[test]
    fn empty_payload_inlines() {
        // N=0: just the 16-byte wrapper, no data. Far below threshold.
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
        // 16 + 65000 = 65016 < 65536. Comfortable under the cutoff.
        assert_eq!(auto_route_source_for(65000), PACKET_SOURCE_MESG);
    }

    /// Per-element overhead the size calculator adds beyond the raw
    /// `sizeof::<Array>() + N` payload for the byte-array schema:
    /// the wrapper plus the `array_data_alignment - 1` bytes the
    /// allocator may need for SIMD/BLAS-aligned element storage
    /// (see [`crate::ffi::calc_voidstar_size_inner`]).
    fn byte_array_flat_overhead() -> usize {
        let schema = byte_array_schema();
        let elem_align = schema.parameters[0].array_data_alignment();
        std::mem::size_of::<Array>() + elem_align.saturating_sub(1)
    }

    #[test]
    fn at_threshold_inlines() {
        // The contract is `flat_size <= MORLOC_INLINE_THRESHOLD` so the
        // exactly-equal case must inline. flat_size for an N-byte
        // byte-array payload is `overhead + N`; pick N so the sum
        // equals the threshold exactly (this is also the largest
        // allowed inline payload).
        let n = MORLOC_INLINE_THRESHOLD - byte_array_flat_overhead();
        assert_eq!(auto_route_source_for(n), PACKET_SOURCE_MESG);
    }

    #[test]
    fn one_byte_over_threshold_uses_rptr() {
        // Same wrapper, one extra data byte: must flip to RPTR.
        let n = MORLOC_INLINE_THRESHOLD - byte_array_flat_overhead() + 1;
        assert_eq!(auto_route_source_for(n), PACKET_SOURCE_RPTR);
    }

    #[test]
    fn well_over_threshold_uses_rptr() {
        // ~128 KB. Comfortably in the RPTR regime.
        assert_eq!(auto_route_source_for(128 * 1024), PACKET_SOURCE_RPTR);
    }
}
