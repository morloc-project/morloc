//! Intrinsic functions for morloc: save/load/show/read/hash.
//! Replaces intrinsics.c. These are thin wrappers around serialization functions.

use std::ffi::{c_char, c_void, CStr, CString};
use std::ptr;

use crate::cschema::CSchema;
use crate::error::{clear_errmsg, set_errmsg, MorlocError};

// ── mlc_save: serialize to msgpack file ────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn mlc_save(
    data: *const c_void,
    schema: *const CSchema,
    path: *const c_char,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);

    extern "C" {
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
    let mut mpk: *mut c_char = ptr::null_mut();
    let mut mpk_size: usize = 0;

    let rc = pack_with_schema(data, schema, &mut mpk, &mut mpk_size, &mut err);
    if rc != 0 {
        *errmsg = err;
        return 1;
    }

    let wrc = write_atomic(path, mpk as *const u8, mpk_size, &mut err);
    libc::free(mpk as *mut c_void);
    if wrc != 0 {
        *errmsg = err;
        return 1;
    }
    0
}

// ── mlc_save_json: serialize to JSON file ──────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn mlc_save_json(
    data: *const c_void,
    schema: *const CSchema,
    path: *const c_char,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);

    extern "C" {
        fn voidstar_to_json_string(
            data: *const c_void, schema: *const CSchema,
            errmsg: *mut *mut c_char,
        ) -> *mut c_char;
        fn write_atomic(
            filename: *const c_char, data: *const u8, size: usize,
            errmsg: *mut *mut c_char,
        ) -> i32;
    }

    let mut err: *mut c_char = ptr::null_mut();
    let json = voidstar_to_json_string(data, schema, &mut err);
    if json.is_null() {
        *errmsg = err;
        return 1;
    }

    let json_len = libc::strlen(json);
    let wrc = write_atomic(path, json as *const u8, json_len, &mut err);
    libc::free(json as *mut c_void);
    if wrc != 0 {
        *errmsg = err;
        return 1;
    }
    0
}

// ── mlc_save_voidstar: serialize to binary voidstar file ───────────────────

#[no_mangle]
pub unsafe extern "C" fn mlc_save_voidstar(
    data: *const c_void,
    schema: *const CSchema,
    path: *const c_char,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);

    extern "C" {
        fn flatten_voidstar_to_buffer(
            data: *const c_void, schema: *const CSchema,
            out_buf: *mut *mut u8, out_size: *mut usize,
            errmsg: *mut *mut c_char,
        ) -> i32;
        fn write_binary_fd(
            fd: i32, buf: *const c_char, count: usize,
            errmsg: *mut *mut c_char,
        ) -> i32;
    }

    let mut err: *mut c_char = ptr::null_mut();

    // Get directory for temp file
    let path_str = CStr::from_ptr(path).to_string_lossy();
    let parent = std::path::Path::new(path_str.as_ref()).parent();
    let dir = match parent {
        Some(p) if !p.as_os_str().is_empty() => p.to_string_lossy().into_owned(),
        _ => ".".to_string(),
    };

    let tmp_template = format!("{}/morloc-tmp_XXXXXX\0", dir);
    let mut tmp_buf: Vec<u8> = tmp_template.into_bytes();
    let fd = libc::mkstemp(tmp_buf.as_mut_ptr() as *mut c_char);
    if fd < 0 {
        set_errmsg(errmsg, &MorlocError::Io(std::io::Error::last_os_error()));
        return 1;
    }

    // Write packet header placeholder
    let header_size = std::mem::size_of::<crate::packet::PacketHeader>();
    let zeros = vec![0u8; header_size];
    if write_binary_fd(fd, zeros.as_ptr() as *const c_char, header_size, &mut err) != 0 {
        libc::close(fd);
        libc::unlink(tmp_buf.as_ptr() as *const c_char);
        *errmsg = err;
        return 1;
    }

    // Flatten voidstar
    let mut blob: *mut u8 = ptr::null_mut();
    let mut blob_size: usize = 0;
    if flatten_voidstar_to_buffer(data, schema, &mut blob, &mut blob_size, &mut err) != 0 {
        libc::close(fd);
        libc::unlink(tmp_buf.as_ptr() as *const c_char);
        *errmsg = err;
        return 1;
    }

    // Write flattened data
    if write_binary_fd(fd, blob as *const c_char, blob_size, &mut err) != 0 {
        libc::free(blob as *mut c_void);
        libc::close(fd);
        libc::unlink(tmp_buf.as_ptr() as *const c_char);
        *errmsg = err;
        return 1;
    }
    libc::free(blob as *mut c_void);

    // Seek back and write real header
    libc::lseek(fd, 0, libc::SEEK_SET);
    let header = crate::packet::PacketHeader::data_mesg(
        crate::packet::PACKET_FORMAT_VOIDSTAR,
        blob_size as u64,
    );
    let hdr_bytes = header.to_bytes();
    write_binary_fd(fd, hdr_bytes.as_ptr() as *const c_char, hdr_bytes.len(), &mut err);

    libc::fsync(fd);
    libc::close(fd);

    // Atomic rename
    if libc::rename(tmp_buf.as_ptr() as *const c_char, path) != 0 {
        libc::unlink(tmp_buf.as_ptr() as *const c_char);
        set_errmsg(errmsg, &MorlocError::Io(std::io::Error::last_os_error()));
        return 1;
    }

    0
}

// ── mlc_load: load from file (auto-detect format) ─────────────────────────

#[no_mangle]
pub unsafe extern "C" fn mlc_load(
    path: *const c_char,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut c_void {
    clear_errmsg(errmsg);

    extern "C" {
        fn file_exists(filename: *const c_char) -> bool;
        fn read_binary_file(
            filename: *const c_char, file_size: *mut usize,
            errmsg: *mut *mut c_char,
        ) -> *mut u8;
        fn load_morloc_data_file(
            path: *const c_char, data: *mut u8, data_size: usize,
            schema: *const CSchema, errmsg: *mut *mut c_char,
        ) -> *mut c_void;
    }

    if !file_exists(path) {
        return ptr::null_mut();
    }

    let mut err: *mut c_char = ptr::null_mut();
    let mut file_size: usize = 0;
    let data = read_binary_file(path, &mut file_size, &mut err);
    if data.is_null() {
        *errmsg = err;
        return ptr::null_mut();
    }

    let result = load_morloc_data_file(path, data, file_size, schema, &mut err);
    if result.is_null() && !err.is_null() {
        *errmsg = err;
    }
    result
}

// ── mlc_hash: hash voidstar data ───────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn mlc_hash(
    data: *const c_void,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut c_char {
    clear_errmsg(errmsg);

    let mut err: *mut c_char = ptr::null_mut();
    let hash = crate::cache::hash_voidstar(data, schema, 0, &mut err);
    if !err.is_null() {
        *errmsg = err;
        return ptr::null_mut();
    }

    let hex = format!("{:016x}", hash);
    match CString::new(hex) {
        Ok(cs) => cs.into_raw(),
        Err(_) => {
            set_errmsg(errmsg, &MorlocError::Other("CString error".into()));
            ptr::null_mut()
        }
    }
}

// ── mlc_show: serialize to JSON string ─────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn mlc_show(
    data: *const c_void,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut c_char {
    clear_errmsg(errmsg);

    extern "C" {
        fn voidstar_to_json_string(
            data: *const c_void, schema: *const CSchema,
            errmsg: *mut *mut c_char,
        ) -> *mut c_char;
    }

    voidstar_to_json_string(data, schema, errmsg)
}

// ── mlc_read: deserialize from JSON string ─────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn mlc_read(
    json_str: *const c_char,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut c_void {
    clear_errmsg(errmsg);

    extern "C" {
        fn read_json_with_schema(
            dest: *mut u8, json: *mut c_char, schema: *const CSchema,
            errmsg: *mut *mut c_char,
        ) -> *mut u8;
    }

    let json_copy = libc::strdup(json_str);
    if json_copy.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("strdup failed".into()));
        return ptr::null_mut();
    }

    let mut err: *mut c_char = ptr::null_mut();
    let result = read_json_with_schema(ptr::null_mut(), json_copy, schema, &mut err);
    libc::free(json_copy as *mut c_void);
    if result.is_null() {
        if !err.is_null() {
            libc::free(err as *mut c_void);
        }
    }
    result as *mut c_void
}

// write_voidstar_binary is provided by packet.c (still C)
// It will be ported when packet.c is ported to Rust.

// Unused Rust implementation kept for future use
#[allow(dead_code)]
unsafe fn _write_voidstar_binary_rust(
    fd: i32,
    data: *const c_void,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> isize {
    clear_errmsg(errmsg);

    extern "C" {
        fn flatten_voidstar_to_buffer(
            data: *const c_void, schema: *const CSchema,
            out_buf: *mut *mut u8, out_size: *mut usize,
            errmsg: *mut *mut c_char,
        ) -> i32;
        fn write_binary_fd(
            fd: i32, buf: *const c_char, count: usize,
            errmsg: *mut *mut c_char,
        ) -> i32;
    }

    let mut err: *mut c_char = ptr::null_mut();
    let mut blob: *mut u8 = ptr::null_mut();
    let mut blob_size: usize = 0;

    if flatten_voidstar_to_buffer(data, schema, &mut blob, &mut blob_size, &mut err) != 0 {
        *errmsg = err;
        return -1;
    }

    if write_binary_fd(fd, blob as *const c_char, blob_size, &mut err) != 0 {
        libc::free(blob as *mut c_void);
        *errmsg = err;
        return -1;
    }

    libc::free(blob as *mut c_void);
    blob_size as isize
}
