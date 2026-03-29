//! CLI argument handling and voidstar utility functions.
//! Replaces cli.c.

use std::ffi::{c_char, c_void, CStr};
use std::ptr;

use crate::cschema::CSchema;
use crate::error::{clear_errmsg, set_errmsg, MorlocError};
use crate::packet;
use crate::shm;

// ── argument_t lifecycle ───────────────────────────────────────────────────

// argument_t is defined in eval.h (C). We use it opaquely via libc pointers.
// The struct: { value: *mut c_char, fields: *mut *mut c_char, default_fields: *mut *mut c_char, size: usize }
#[repr(C)]
pub struct ArgumentT {
    pub value: *mut c_char,
    pub fields: *mut *mut c_char,
    pub default_fields: *mut *mut c_char,
    pub size: usize,
}

#[no_mangle]
pub unsafe extern "C" fn initialize_positional(value: *mut c_char) -> *mut ArgumentT {
    let arg = libc::calloc(1, std::mem::size_of::<ArgumentT>()) as *mut ArgumentT;
    if arg.is_null() {
        return ptr::null_mut();
    }
    (*arg).value = if value.is_null() {
        ptr::null_mut()
    } else {
        libc::strdup(value)
    };
    (*arg).size = 0;
    arg
}

#[no_mangle]
pub unsafe extern "C" fn initialize_unrolled(
    size: usize,
    default_value: *mut c_char,
    fields: *mut *mut c_char,
    default_fields: *mut *mut c_char,
) -> *mut ArgumentT {
    let arg = libc::calloc(1, std::mem::size_of::<ArgumentT>()) as *mut ArgumentT;
    if arg.is_null() {
        return ptr::null_mut();
    }
    (*arg).value = if default_value.is_null() {
        ptr::null_mut()
    } else {
        libc::strdup(default_value)
    };
    (*arg).size = size;

    (*arg).fields = libc::calloc(size, std::mem::size_of::<*mut c_char>()) as *mut *mut c_char;
    for i in 0..size {
        let f = *fields.add(i);
        if !f.is_null() {
            *(*arg).fields.add(i) = libc::strdup(f);
        }
    }

    (*arg).default_fields =
        libc::calloc(size, std::mem::size_of::<*mut c_char>()) as *mut *mut c_char;
    for i in 0..size {
        let d = *default_fields.add(i);
        if !d.is_null() {
            *(*arg).default_fields.add(i) = libc::strdup(d);
        }
    }

    arg
}

#[no_mangle]
pub unsafe extern "C" fn free_argument_t(arg: *mut ArgumentT) {
    if arg.is_null() {
        return;
    }
    if !(*arg).value.is_null() {
        libc::free((*arg).value as *mut c_void);
    }
    if !(*arg).fields.is_null() {
        for i in 0..(*arg).size {
            let f = *(*arg).fields.add(i);
            if !f.is_null() {
                libc::free(f as *mut c_void);
            }
        }
        libc::free((*arg).fields as *mut c_void);
    }
    if !(*arg).default_fields.is_null() {
        for i in 0..(*arg).size {
            let d = *(*arg).default_fields.add(i);
            if !d.is_null() {
                libc::free(d as *mut c_void);
            }
        }
        libc::free((*arg).default_fields as *mut c_void);
    }
    libc::free(arg as *mut c_void);
}

// ── shfree_by_schema ───────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn shfree_by_schema(
    ptr: *mut c_void,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> bool {
    clear_errmsg(errmsg);
    if ptr.is_null() || schema.is_null() {
        return true;
    }
    let rs = CSchema::to_rust(schema);
    match shfree_by_schema_inner(ptr as *mut u8, &rs) {
        Ok(_) => true,
        Err(e) => {
            set_errmsg(errmsg, &e);
            false
        }
    }
}

fn shfree_by_schema_inner(
    ptr: *mut u8,
    schema: &crate::schema::Schema,
) -> Result<(), MorlocError> {
    use crate::schema::SerialType;

    // SAFETY: ptr points to voidstar data in SHM with layout described by schema.
    // We recursively visit sub-structures and zero metadata before the parent shfree.
    unsafe {
        match schema.serial_type {
            SerialType::String | SerialType::Array => {
                let arr = &*(ptr as *const shm::Array);
                if arr.data > 0 {
                    if !schema.parameters.is_empty() && !schema.parameters[0].is_fixed_width() {
                        let arr_data = shm::rel2abs(arr.data)?;
                        let elem_width = schema.parameters[0].width;
                        for i in 0..arr.size {
                            shfree_by_schema_inner(
                                arr_data.add(i * elem_width),
                                &schema.parameters[0],
                            )?;
                        }
                    }
                }
            }
            SerialType::Tuple | SerialType::Map => {
                for i in 0..schema.parameters.len() {
                    let child = ptr.add(schema.offsets[i]);
                    shfree_by_schema_inner(child, &schema.parameters[i])?;
                }
            }
            SerialType::Tensor => {
                // shape and data are inline, freed by parent shfree
            }
            _ => {
                // fixed-size: no sub-data
            }
        }
        // Zero this node's metadata
        std::ptr::write_bytes(ptr, 0, schema.width);
    }
    Ok(())
}

// ── adjust_voidstar_relptrs ────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn adjust_voidstar_relptrs(
    data: *mut c_void,
    schema: *const CSchema,
    base_rel: shm::RelPtr,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    let rs = CSchema::to_rust(schema);
    match adjust_relptrs_inner(data as *mut u8, &rs, base_rel) {
        Ok(_) => 0,
        Err(e) => {
            set_errmsg(errmsg, &e);
            1
        }
    }
}

fn adjust_relptrs_inner(
    data: *mut u8,
    schema: &crate::schema::Schema,
    base_rel: shm::RelPtr,
) -> Result<(), MorlocError> {
    use crate::schema::SerialType;

    // SAFETY: data points to a voidstar blob in SHM. We adjust relptrs in-place;
    // all pointer arithmetic stays within the blob's bounds as defined by schema.
    unsafe {
        match schema.serial_type {
            SerialType::String | SerialType::Array => {
                let arr = &mut *(data as *mut shm::Array);
                arr.data += base_rel;
                if !schema.parameters.is_empty() && !schema.parameters[0].is_fixed_width() {
                    let arr_data = shm::rel2abs(arr.data)?;
                    let elem_width = schema.parameters[0].width;
                    for i in 0..arr.size {
                        adjust_relptrs_inner(
                            arr_data.add(i * elem_width),
                            &schema.parameters[0],
                            base_rel,
                        )?;
                    }
                }
            }
            SerialType::Tuple | SerialType::Map => {
                for i in 0..schema.parameters.len() {
                    adjust_relptrs_inner(
                        data.add(schema.offsets[i]),
                        &schema.parameters[i],
                        base_rel,
                    )?;
                }
            }
            SerialType::Optional => {
                let tag = *data;
                if tag != 0 && !schema.parameters.is_empty() {
                    let inner_offset = schema.offsets.first().copied().unwrap_or(
                        shm::align_up(1, schema.parameters[0].alignment().max(1)),
                    );
                    adjust_relptrs_inner(
                        data.add(inner_offset),
                        &schema.parameters[0],
                        base_rel,
                    )?;
                }
            }
            SerialType::Tensor => {
                let tensor = &mut *(data as *mut shm::Tensor);
                if tensor.total_elements > 0 {
                    tensor.shape += base_rel;
                    tensor.data += base_rel;
                }
            }
            _ => {}
        }
    }
    Ok(())
}

// ── read_voidstar_binary ───────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn read_voidstar_binary(
    blob: *const u8,
    blob_size: usize,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut c_void {
    clear_errmsg(errmsg);
    let rs = CSchema::to_rust(schema);

    let base = match shm::shmalloc(blob_size) {
        Ok(p) => p,
        Err(e) => {
            set_errmsg(errmsg, &e);
            return ptr::null_mut();
        }
    };
    std::ptr::copy_nonoverlapping(blob, base, blob_size);

    let base_rel = match shm::abs2rel(base) {
        Ok(r) => r,
        Err(e) => {
            let _ = shm::shfree(base);
            set_errmsg(errmsg, &e);
            return ptr::null_mut();
        }
    };

    if let Err(e) = adjust_relptrs_inner(base, &rs, base_rel) {
        let _ = shm::shfree(base);
        set_errmsg(errmsg, &e);
        return ptr::null_mut();
    }

    base as *mut c_void
}

// ── load_morloc_data_file ──────────────────────────────────────────────────
// This function is complex and calls many C functions (read_json_with_schema,
// unpack_with_schema). Keep delegating to C for now via extern declarations.

#[no_mangle]
pub unsafe extern "C" fn load_morloc_data_file(
    path: *const c_char,
    data: *mut u8,
    data_size: usize,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut c_void {
    clear_errmsg(errmsg);

    extern "C" {
        fn read_json_with_schema(
            dest: *mut u8, json: *mut c_char, schema: *const CSchema,
            errmsg: *mut *mut c_char,
        ) -> *mut u8;
        fn unpack_with_schema(
            mpk: *const c_char, mpk_size: usize, schema: *const CSchema,
            mlcptr: *mut *mut c_void, errmsg: *mut *mut c_char,
        ) -> i32;
    }

    if data_size == 0 {
        set_errmsg(errmsg, &MorlocError::Other("Cannot parse 0-length data".into()));
        return ptr::null_mut();
    }

    let path_str = CStr::from_ptr(path).to_string_lossy();
    let mut err: *mut c_char = ptr::null_mut();

    // 1. Extension-based dispatch
    if path_str.ends_with(".json") {
        let json_buf = libc::realloc(data as *mut c_void, data_size + 1) as *mut u8;
        if json_buf.is_null() {
            libc::free(data as *mut c_void);
            set_errmsg(errmsg, &MorlocError::Other("realloc failed".into()));
            return ptr::null_mut();
        }
        *json_buf.add(data_size) = 0;
        let result = read_json_with_schema(ptr::null_mut(), json_buf as *mut c_char, schema, &mut err);
        if !err.is_null() {
            libc::free(json_buf as *mut c_void);
            *errmsg = err;
            return ptr::null_mut();
        }
        libc::free(json_buf as *mut c_void);
        return result as *mut c_void;
    }

    if path_str.ends_with(".mpk") || path_str.ends_with(".msgpack") {
        let mut result: *mut c_void = ptr::null_mut();
        unpack_with_schema(data as *const c_char, data_size, schema, &mut result, &mut err);
        libc::free(data as *mut c_void);
        if !err.is_null() {
            *errmsg = err;
            return ptr::null_mut();
        }
        return result;
    }

    // 2. Check for morloc packet header
    if data_size >= 32 {
        let magic = *(data as *const u32);
        if magic == packet::PACKET_MAGIC {
            let header_bytes: &[u8; 32] = &*(data as *const [u8; 32]);
            if let Ok(header) = packet::PacketHeader::from_bytes(header_bytes) {
                if !header.is_data() {
                    libc::free(data as *mut c_void);
                    set_errmsg(errmsg, &MorlocError::Other(format!("Expected data packet in '{}'", path_str)));
                    return ptr::null_mut();
                }
                let offset = { header.offset } as usize;
                let length = { header.length } as usize;
                let payload = data.add(32 + offset);
                let format = { header.command.data.format };

                if format == packet::PACKET_FORMAT_VOIDSTAR {
                    let result = read_voidstar_binary(payload, length, schema, &mut err);
                    libc::free(data as *mut c_void);
                    if !err.is_null() { *errmsg = err; return ptr::null_mut(); }
                    return result;
                } else if format == packet::PACKET_FORMAT_MSGPACK {
                    let mut result: *mut c_void = ptr::null_mut();
                    unpack_with_schema(payload as *const c_char, length, schema, &mut result, &mut err);
                    libc::free(data as *mut c_void);
                    if !err.is_null() { *errmsg = err; return ptr::null_mut(); }
                    return result;
                } else {
                    libc::free(data as *mut c_void);
                    set_errmsg(errmsg, &MorlocError::Other(format!("Unsupported format 0x{:02x} in '{}'", format, path_str)));
                    return ptr::null_mut();
                }
            }
        }
    }

    // 3. Try JSON
    let first_byte = *data;
    let may_be_json = matches!(first_byte,
        b'\'' | b'"' | b'[' | b'{' | b't' | b'f' | b'n' |
        b'\t' | b'\n' | b'\r' | b' ' |
        b'0'..=b'9' | b'-'
    );

    if (data_size > 1 && may_be_json) || (data_size == 1 && first_byte >= b'0' && first_byte <= b'9') {
        let json_buf = libc::realloc(data as *mut c_void, data_size + 1) as *mut u8;
        if !json_buf.is_null() {
            *json_buf.add(data_size) = 0;
            let result = read_json_with_schema(ptr::null_mut(), json_buf as *mut c_char, schema, &mut err);
            if err.is_null() && !result.is_null() {
                libc::free(json_buf as *mut c_void);
                return result as *mut c_void;
            }
            if !err.is_null() { libc::free(err as *mut c_void); err = ptr::null_mut(); }
            // Fall through to try msgpack
            // Note: data pointer may have been invalidated by realloc
            // Use json_buf as the data pointer going forward
            let mut result: *mut c_void = ptr::null_mut();
            unpack_with_schema(json_buf as *const c_char, data_size, schema, &mut result, &mut err);
            libc::free(json_buf as *mut c_void);
            if !err.is_null() { *errmsg = err; return ptr::null_mut(); }
            return result;
        }
    }

    // 4. Try msgpack
    let mut result: *mut c_void = ptr::null_mut();
    unpack_with_schema(data as *const c_char, data_size, schema, &mut result, &mut err);
    libc::free(data as *mut c_void);
    if !err.is_null() { *errmsg = err; return ptr::null_mut(); }
    result
}

// ── upload_packet (static helper) ────────────────────────────────────────────

/// Copy a voidstar packet into SHM, adjusting relptrs.
///
/// # Safety
/// `dest` must point to schema.width writable bytes in SHM.
/// `data` must point to a valid voidstar blob within [data, data_end].
unsafe fn upload_packet(
    dest: *mut u8,
    data: *const u8,
    data_end: usize,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    let rs = CSchema::to_rust(schema);

    match upload_packet_inner(dest, data, data_end, schema, &rs) {
        Ok(_) => 0,
        Err(e) => {
            set_errmsg(errmsg, &e);
            1
        }
    }
}

unsafe fn upload_packet_inner(
    dest: *mut u8,
    data: *const u8,
    data_end: usize,
    schema: *const CSchema,
    rs: &crate::schema::Schema,
) -> Result<(), MorlocError> {
    use crate::schema::SerialType;

    match rs.serial_type {
        SerialType::String | SerialType::Array => {
            if (data as usize + rs.width - 1) <= data_end {
                return Err(MorlocError::Packet("Data is too small to store an array header".into()));
            }
            ptr::copy_nonoverlapping(data, dest, rs.width);
            let arr = &mut *(dest as *mut shm::Array);
            let arr_data_offset = arr.data as usize;
            let arr_data = data.add(arr_data_offset);
            let elem_width = rs.parameters[0].width;
            let arr_size = arr.size * elem_width;

            if (arr_data as usize + arr_size - 1) > data_end {
                return Err(MorlocError::Packet("Data is too small to contain array values".into()));
            }

            let data_ptr = shm::shmemcpy(arr_data, arr_size)?;

            if !rs.is_fixed_width() {
                let elem_schema = &rs.parameters[0];
                // Need the C schema for each element
                let elem_c_schema = (*schema).parameters;
                if !elem_c_schema.is_null() {
                    let elem_cs = *elem_c_schema;
                    for i in 0..arr.size {
                        upload_packet_inner(
                            data_ptr.add(i * elem_width),
                            arr_data.add(i * elem_width),
                            data_end,
                            elem_cs,
                            elem_schema,
                        )?;
                    }
                }
            }

            arr.data = shm::abs2rel(data_ptr)?;
        }
        SerialType::Tuple | SerialType::Map => {
            for i in 0..rs.parameters.len() {
                let elem_cs = if (*schema).parameters.is_null() {
                    return Err(MorlocError::Packet("NULL parameters in schema".into()));
                } else {
                    *(*schema).parameters.add(i)
                };
                upload_packet_inner(
                    dest.add(rs.offsets[i]),
                    data.add(rs.offsets[i]),
                    data_end,
                    elem_cs,
                    &rs.parameters[i],
                )?;
            }
        }
        _ => {
            if (data as usize + rs.width - 1) > data_end {
                return Err(MorlocError::Packet("Given data packet is too small".into()));
            }
            ptr::copy_nonoverlapping(data, dest, rs.width);
        }
    }
    Ok(())
}

// ── parse_cli_data_argument_singular ─────────────────────────────────────────

unsafe fn parse_cli_data_argument_singular(
    mut dest: *mut u8,
    arg: *mut c_char,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);

    extern "C" {
        fn read_json_with_schema(
            dest: *mut u8, json: *mut c_char, schema: *const CSchema,
            errmsg: *mut *mut c_char,
        ) -> *mut u8;
        fn file_exists(filename: *const c_char) -> bool;
        fn read_binary_fd(file: *mut libc::FILE, file_size: *mut usize, errmsg: *mut *mut c_char) -> *mut u8;
    }

    let rs = CSchema::to_rust(schema);
    let mut err: *mut c_char = ptr::null_mut();
    let mut fd: *mut libc::FILE = ptr::null_mut();

    // handle STDIN
    let stdin_path = b"/dev/stdin\0";
    let dash_path = b"-\0";
    if libc::strcmp(arg, stdin_path.as_ptr() as *const c_char) == 0
        || libc::strcmp(arg, dash_path.as_ptr() as *const c_char) == 0
    {
        fd = libc::fdopen(libc::STDIN_FILENO, b"rb\0".as_ptr() as *const c_char);
    } else if file_exists(arg) {
        fd = libc::fopen(arg, b"rb\0".as_ptr() as *const c_char);
        if fd.is_null() {
            set_errmsg(errmsg, &MorlocError::Other(
                format!("The argument '{}' is a filename, but it can't be read",
                    CStr::from_ptr(arg).to_string_lossy())
            ));
            return ptr::null_mut();
        }
    }

    if fd.is_null() {
        // Literal JSON data
        if dest.is_null() {
            match shm::shcalloc(1, rs.width) {
                Ok(p) => dest = p,
                Err(e) => { set_errmsg(errmsg, &e); return ptr::null_mut(); }
            }
        }
        dest = read_json_with_schema(dest, arg, schema, &mut err);
        if !err.is_null() {
            *errmsg = err;
            return ptr::null_mut();
        }
        return dest;
    }

    // File or stdin
    let mut data_size: usize = 0;
    let data = read_binary_fd(fd, &mut data_size, &mut err);
    // Don't close stdin
    if fd != libc::fdopen(libc::STDIN_FILENO, b"rb\0".as_ptr() as *const c_char) {
        libc::fclose(fd);
    }
    if !err.is_null() {
        if !data.is_null() { libc::free(data as *mut c_void); }
        *errmsg = err;
        return ptr::null_mut();
    }

    // Special case: RPTR packets
    if data_size >= 32 {
        let magic = *(data as *const u32);
        if magic == packet::PACKET_MAGIC {
            let header = &*(data as *const packet::PacketHeader);
            let source = header.command.data.source;
            let format = header.command.data.format;
            if source == packet::PACKET_SOURCE_RPTR && format == packet::PACKET_FORMAT_VOIDSTAR {
                if dest.is_null() {
                    match shm::shcalloc(1, rs.width) {
                        Ok(p) => dest = p,
                        Err(e) => {
                            libc::free(data as *mut c_void);
                            set_errmsg(errmsg, &e);
                            return ptr::null_mut();
                        }
                    }
                }
                let voidstar_ptr = data.add(32 + header.offset as usize);
                if upload_packet(dest, voidstar_ptr, voidstar_ptr as usize + data_size - 1, schema, &mut err) != 0 {
                    libc::free(data as *mut c_void);
                    *errmsg = err;
                    return ptr::null_mut();
                }
                libc::free(data as *mut c_void);
                return dest;
            }
        }
    }

    // All other formats: canonical file loader (takes ownership of data)
    dest = load_morloc_data_file(arg, data, data_size, schema, &mut err) as *mut u8;
    if !err.is_null() {
        *errmsg = err;
        return ptr::null_mut();
    }
    dest
}

// ── parse_cli_data_argument_unrolled ─────────────────────────────────────────

unsafe fn parse_cli_data_argument_unrolled(
    mut dest: *mut u8,
    default_value: *mut c_char,
    fields: *mut *mut c_char,
    default_fields: *mut *mut c_char,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);
    let rs = CSchema::to_rust(schema);
    let mut err: *mut c_char = ptr::null_mut();
    let mut using_record_default = false;

    if dest.is_null() {
        match shm::shcalloc(1, rs.width) {
            Ok(p) => dest = p,
            Err(e) => { set_errmsg(errmsg, &e); return ptr::null_mut(); }
        }
    }

    if !default_value.is_null() {
        dest = parse_cli_data_argument_singular(dest, default_value, schema, &mut err);
        if !err.is_null() { *errmsg = err; return ptr::null_mut(); }
        using_record_default = true;
    }

    use crate::schema::SerialType;
    match rs.serial_type {
        SerialType::Tuple | SerialType::Map => {
            for i in 0..rs.parameters.len() {
                let element_dest = dest.add(rs.offsets[i]);
                let field_val = *fields.add(i);
                let elem_cs = *(*schema).parameters.add(i);

                if !field_val.is_null() {
                    // Free memory from default for this field
                    shfree_by_schema(element_dest as *mut c_void, elem_cs, &mut err);
                    if !err.is_null() { libc::free(err as *mut c_void); err = ptr::null_mut(); }

                    let result = parse_cli_data_argument_singular(
                        element_dest, field_val, elem_cs, &mut err,
                    );
                    if !err.is_null() { *errmsg = err; return ptr::null_mut(); }
                    let _ = result; // result writes into element_dest
                } else if using_record_default {
                    continue;
                } else {
                    let default_field = *default_fields.add(i);
                    if !default_field.is_null() {
                        let result = parse_cli_data_argument_singular(
                            element_dest, default_field, elem_cs, &mut err,
                        );
                        if !err.is_null() { *errmsg = err; return ptr::null_mut(); }
                        let _ = result;
                    } else {
                        set_errmsg(errmsg, &MorlocError::Other(
                            format!("Field {} missing with no default or default record", i)
                        ));
                        return ptr::null_mut();
                    }
                }
            }
        }
        _ => {
            set_errmsg(errmsg, &MorlocError::Other("Only record and tuple types may be unrolled".into()));
            return ptr::null_mut();
        }
    }
    dest
}

// ── parse_cli_data_argument ──────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn parse_cli_data_argument(
    dest: *mut u8,
    arg: *const ArgumentT,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);
    let mut err: *mut c_char = ptr::null_mut();

    let result = if (*arg).fields.is_null() {
        parse_cli_data_argument_singular(dest, (*arg).value, schema, &mut err)
    } else {
        parse_cli_data_argument_unrolled(
            dest, (*arg).value, (*arg).fields, (*arg).default_fields, schema, &mut err,
        )
    };

    if !err.is_null() {
        *errmsg = err;
        return ptr::null_mut();
    }
    if result.is_null() {
        return ptr::null_mut();
    }

    let relptr = match shm::abs2rel(result) {
        Ok(r) => r,
        Err(e) => { set_errmsg(errmsg, &e); return ptr::null_mut(); }
    };

    // Call the Rust make_standard_data_packet FFI
    crate::packet_ffi::make_standard_data_packet(relptr, schema)
}

// ── make_call_packet_from_cli ────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn make_call_packet_from_cli(
    dest: *mut u8,
    mid: u32,
    args: *mut *mut ArgumentT,   // NULL-terminated
    arg_schema_strs: *mut *mut c_char, // NULL-terminated
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);
    let mut err: *mut c_char = ptr::null_mut();

    // Count and parse schemas
    let mut nschemas: usize = 0;
    while !(*arg_schema_strs.add(nschemas)).is_null() {
        nschemas += 1;
    }

    let mut schemas: Vec<*mut CSchema> = Vec::with_capacity(nschemas);
    for i in 0..nschemas {
        let schema = crate::ffi::parse_schema(*arg_schema_strs.add(i), &mut err);
        if !err.is_null() {
            for s in &schemas { CSchema::free(*s); }
            *errmsg = err;
            return ptr::null_mut();
        }
        schemas.push(schema);
    }

    // Count args
    let mut nargs: usize = 0;
    while !(*args.add(nargs)).is_null() {
        nargs += 1;
    }

    // Parse each argument into a data packet
    let mut packet_args: Vec<*const u8> = Vec::with_capacity(nargs);
    for i in 0..nargs {
        let packet = parse_cli_data_argument(dest, *args.add(i), schemas[i], &mut err);
        if !err.is_null() {
            for p in &packet_args { libc::free(*p as *mut c_void); }
            for s in &schemas { CSchema::free(*s); }
            *errmsg = err;
            return ptr::null_mut();
        }
        packet_args.push(packet as *const u8);
    }

    // Build call packet
    let call_packet = crate::packet_ffi::make_morloc_local_call_packet(
        mid, packet_args.as_ptr(), nargs, &mut err,
    );

    for p in &packet_args { libc::free(*p as *mut c_void); }
    for s in &schemas { CSchema::free(*s); }

    if !err.is_null() {
        *errmsg = err;
        return ptr::null_mut();
    }
    call_packet
}
