//! File-based packet caching with xxHash keys.
//! Replaces cache.c.

use std::ffi::{c_char, c_void, CStr, CString};
use std::ptr;

use crate::cschema::CSchema;
use crate::error::{clear_errmsg, set_errmsg, MorlocError};
use crate::hash;
use crate::shm;

// ── hash_voidstar ──────────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn hash_voidstar(
    data: *const c_void,
    schema: *const CSchema,
    seed: u64,
    errmsg: *mut *mut c_char,
) -> u64 {
    clear_errmsg(errmsg);
    let rs = CSchema::to_rust(schema);
    match hash_voidstar_inner(data as *const u8, &rs, seed) {
        Ok(h) => h,
        Err(e) => {
            set_errmsg(errmsg, &e);
            0
        }
    }
}

fn hash_voidstar_inner(
    data: *const u8,
    schema: &crate::schema::Schema,
    seed: u64,
) -> Result<u64, MorlocError> {
    use crate::schema::SerialType;

    // SAFETY: data points to voidstar data in SHM with layout described by schema.
    // All reads (Array headers, element data) are within schema-defined bounds.
    unsafe {
        match schema.serial_type {
            SerialType::String | SerialType::Array => {
                let arr = &*(data as *const shm::Array);
                let elem_width = if schema.parameters.is_empty() {
                    1 // string bytes
                } else {
                    schema.parameters[0].width
                };
                let elem_data = shm::rel2abs(arr.data)?;

                if schema.is_fixed_width() || schema.serial_type == SerialType::String {
                    let total = elem_width * arr.size;
                    let bytes = std::slice::from_raw_parts(elem_data, total);
                    Ok(hash::xxh64_with_seed(bytes, seed))
                } else {
                    let mut h = seed;
                    for i in 0..arr.size {
                        h = hash_voidstar_inner(
                            elem_data.add(i * elem_width),
                            &schema.parameters[0],
                            h,
                        )?;
                    }
                    Ok(h)
                }
            }
            SerialType::Tuple | SerialType::Map => {
                if schema.is_fixed_width() {
                    let bytes = std::slice::from_raw_parts(data, schema.width);
                    Ok(hash::xxh64_with_seed(bytes, seed))
                } else {
                    let mut h = seed;
                    for i in 0..schema.parameters.len() {
                        h = hash_voidstar_inner(
                            data.add(schema.offsets[i]),
                            &schema.parameters[i],
                            h,
                        )?;
                    }
                    Ok(h)
                }
            }
            _ => {
                let bytes = std::slice::from_raw_parts(data, schema.width);
                Ok(hash::xxh64_with_seed(bytes, seed))
            }
        }
    }
}

// ── hash_morloc_packet ─────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn hash_morloc_packet(
    packet: *const u8,
    schema: *const CSchema,
    seed: u64,
    hash_out: *mut u64,
    errmsg: *mut *mut c_char,
) -> bool {
    clear_errmsg(errmsg);
    *hash_out = 0;

    extern "C" {
        fn read_morloc_packet_header(
            msg: *const u8,
            errmsg: *mut *mut c_char,
        ) -> *const crate::packet::PacketHeader;
        fn morloc_packet_size(packet: *const u8, errmsg: *mut *mut c_char) -> usize;
        fn get_morloc_data_packet_value(
            data: *const u8,
            schema: *const CSchema,
            errmsg: *mut *mut c_char,
        ) -> *mut u8;
    }

    let mut err: *mut c_char = ptr::null_mut();
    let header = read_morloc_packet_header(packet, &mut err);
    if header.is_null() {
        if !err.is_null() {
            *errmsg = err;
        }
        return false;
    }

    let cmd_type = (*header).command_type();
    if cmd_type == crate::packet::PACKET_TYPE_CALL {
        let midx = { (*header).command.call.midx };
        *hash_out = crate::utility::mix(seed, midx as u64);
        let offset = { (*header).offset } as usize;
        let length = { (*header).length } as usize;
        let arg_data = packet.add(32 + offset);
        let mut arg_start = 0usize;
        while arg_start < length {
            let arg_size = morloc_packet_size(arg_data.add(arg_start), &mut err);
            if !err.is_null() {
                *errmsg = err;
                return false;
            }
            let arg_bytes = std::slice::from_raw_parts(arg_data.add(arg_start), arg_size);
            *hash_out = crate::utility::mix(*hash_out, hash::xxh64_with_seed(arg_bytes, *hash_out));
            arg_start += arg_size;
        }
    } else if cmd_type == crate::packet::PACKET_TYPE_DATA {
        let voidstar = get_morloc_data_packet_value(packet, schema, &mut err);
        if voidstar.is_null() {
            if !err.is_null() {
                *errmsg = err;
            }
            return false;
        }
        let rs = CSchema::to_rust(schema);
        match hash_voidstar_inner(voidstar, &rs, seed) {
            Ok(h) => *hash_out = h,
            Err(e) => {
                set_errmsg(errmsg, &e);
                return false;
            }
        }
    } else {
        set_errmsg(
            errmsg,
            &MorlocError::Other(format!("Cannot hash packet with command 0x{:02x}", cmd_type)),
        );
        return false;
    }

    true
}

// ── Cache filename generation ──────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn make_cache_filename_ext(
    key: u64,
    cache_path: *const c_char,
    ext: *const c_char,
    errmsg: *mut *mut c_char,
) -> *mut c_char {
    clear_errmsg(errmsg);
    let path = CStr::from_ptr(cache_path).to_string_lossy();
    let extension = CStr::from_ptr(ext).to_string_lossy();
    let filename = format!("{}/{:016x}{}", path, key, extension);
    match CString::new(filename) {
        Ok(cs) => cs.into_raw(),
        Err(_) => {
            set_errmsg(errmsg, &MorlocError::Other("CString conversion failed".into()));
            ptr::null_mut()
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn make_cache_filename(
    key: u64,
    cache_path: *const c_char,
    errmsg: *mut *mut c_char,
) -> *mut c_char {
    let ext = CString::new(".packet").unwrap();
    make_cache_filename_ext(key, cache_path, ext.as_ptr(), errmsg)
}

// ── Cache operations ───────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn put_cache_packet(
    voidstar: *const u8,
    schema: *const CSchema,
    key: u64,
    cache_path: *const c_char,
    errmsg: *mut *mut c_char,
) -> *mut c_char {
    clear_errmsg(errmsg);

    extern "C" {
        fn make_mpk_data_packet(filename: *const c_char, schema: *const CSchema) -> *mut u8;
        fn morloc_packet_size(packet: *const u8, errmsg: *mut *mut c_char) -> usize;
        fn pack_with_schema(
            mlc: *const c_void, schema: *const CSchema,
            mpk: *mut *mut c_char, mpk_size: *mut usize,
            errmsg: *mut *mut c_char,
        ) -> i32;
        fn write_atomic(
            filename: *const c_char, data: *const u8, size: usize,
            errmsg: *mut *mut c_char,
        ) -> i32;
    }

    let mut err: *mut c_char = ptr::null_mut();

    // Generate filenames
    let pkt_filename = make_cache_filename(key, cache_path, &mut err);
    if pkt_filename.is_null() {
        *errmsg = err;
        return ptr::null_mut();
    }

    let dat_ext = CString::new(".dat").unwrap();
    let dat_filename = make_cache_filename_ext(key, cache_path, dat_ext.as_ptr(), &mut err);
    if dat_filename.is_null() {
        libc::free(pkt_filename as *mut c_void);
        *errmsg = err;
        return ptr::null_mut();
    }

    // Create data packet pointing to the .dat file
    let data_packet = make_mpk_data_packet(dat_filename, schema);
    if data_packet.is_null() {
        libc::free(pkt_filename as *mut c_void);
        libc::free(dat_filename as *mut c_void);
        set_errmsg(errmsg, &MorlocError::Other("Failed to create data packet".into()));
        return ptr::null_mut();
    }

    let pkt_size = morloc_packet_size(data_packet, &mut err);

    // Pack voidstar to msgpack
    let mut mpk_data: *mut c_char = ptr::null_mut();
    let mut mpk_size: usize = 0;
    let rc = pack_with_schema(voidstar as *const c_void, schema, &mut mpk_data, &mut mpk_size, &mut err);
    if rc != 0 {
        libc::free(data_packet as *mut c_void);
        libc::free(pkt_filename as *mut c_void);
        libc::free(dat_filename as *mut c_void);
        *errmsg = err;
        return ptr::null_mut();
    }

    // Write packet file
    write_atomic(pkt_filename, data_packet, pkt_size, &mut err);
    libc::free(data_packet as *mut c_void);
    if !err.is_null() {
        libc::free(mpk_data as *mut c_void);
        libc::free(pkt_filename as *mut c_void);
        libc::free(dat_filename as *mut c_void);
        *errmsg = err;
        return ptr::null_mut();
    }

    // Write data file
    write_atomic(dat_filename, mpk_data as *const u8, mpk_size, &mut err);
    libc::free(mpk_data as *mut c_void);
    libc::free(dat_filename as *mut c_void);
    if !err.is_null() {
        libc::free(pkt_filename as *mut c_void);
        *errmsg = err;
        return ptr::null_mut();
    }

    // Return the packet filename
    let result = libc::strdup(pkt_filename);
    libc::free(pkt_filename as *mut c_void);
    result
}

#[no_mangle]
pub unsafe extern "C" fn get_cache_packet(
    key: u64,
    cache_path: *const c_char,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);
    let mut err: *mut c_char = ptr::null_mut();

    let filename = make_cache_filename(key, cache_path, &mut err);
    if filename.is_null() {
        *errmsg = err;
        return ptr::null_mut();
    }

    extern "C" {
        fn read_binary_file(
            filename: *const c_char, file_size: *mut usize,
            errmsg: *mut *mut c_char,
        ) -> *mut u8;
    }

    let mut file_size: usize = 0;
    let data = read_binary_file(filename, &mut file_size, &mut err);
    libc::free(filename as *mut c_void);
    if data.is_null() {
        *errmsg = err;
    }
    data
}

#[no_mangle]
pub unsafe extern "C" fn del_cache_packet(
    key: u64,
    cache_path: *const c_char,
    errmsg: *mut *mut c_char,
) -> bool {
    clear_errmsg(errmsg);
    let mut err: *mut c_char = ptr::null_mut();

    let filename = make_cache_filename(key, cache_path, &mut err);
    if filename.is_null() {
        *errmsg = err;
        return false;
    }

    let rc = libc::unlink(filename);
    if rc != 0 {
        set_errmsg(
            errmsg,
            &MorlocError::Other(format!(
                "Failed to delete cache file '{}'",
                CStr::from_ptr(filename).to_string_lossy()
            )),
        );
        libc::free(filename as *mut c_void);
        return false;
    }
    libc::free(filename as *mut c_void);
    true
}

#[no_mangle]
pub unsafe extern "C" fn check_cache_packet(
    key: u64,
    cache_path: *const c_char,
    errmsg: *mut *mut c_char,
) -> *mut c_char {
    clear_errmsg(errmsg);
    let mut err: *mut c_char = ptr::null_mut();

    let filename = make_cache_filename(key, cache_path, &mut err);
    if filename.is_null() {
        *errmsg = err;
        return ptr::null_mut();
    }

    let mut sb: libc::stat = std::mem::zeroed();
    if libc::stat(filename, &mut sb) == 0 {
        let result = libc::strdup(filename);
        libc::free(filename as *mut c_void);
        return result;
    }
    libc::free(filename as *mut c_void);
    ptr::null_mut() // Not an error — cache miss
}
