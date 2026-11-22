//! C ABI wrappers for JSON functions.
//! Replaces json.c's core functions with calls to Rust json.rs.
//! Arrow output and json_buf API are also implemented here.

use std::ffi::{c_char, c_void, CStr, CString};
use std::ptr;

use crate::cschema::CSchema;
use crate::error::{clear_errmsg, set_errmsg, MorlocError};

// ── quoted ─────────────────────────────────────────────────────────────────

pub(crate) unsafe fn quoted(input: *const c_char) -> *mut c_char {
    if input.is_null() {
        return ptr::null_mut();
    }
    let s = CStr::from_ptr(input);
    let bytes = s.to_bytes();
    let len = bytes.len();
    // Simple wrapping: "input" (matching C behavior — no escaping)
    let buf = libc::calloc(len + 3, 1) as *mut c_char;
    if buf.is_null() {
        return ptr::null_mut();
    }
    *buf = b'"' as c_char;
    std::ptr::copy_nonoverlapping(bytes.as_ptr(), buf.add(1) as *mut u8, len);
    *buf.add(len + 1) = b'"' as c_char;
    buf
}

// ── read_json_with_schema ──────────────────────────────────────────────────

pub(crate) unsafe fn read_json_with_schema(
    dest: *mut u8,
    json_str: *mut c_char,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);
    if json_str.is_null() || schema.is_null() {
        set_errmsg(errmsg, &MorlocError::NullPointer);
        return ptr::null_mut();
    }

    let rs = crate::packet_ffi::rust_schema(schema);
    let json = match CStr::from_ptr(json_str).to_str() {
        Ok(s) => s,
        Err(e) => {
            let msg = format!("JSON text is not valid UTF-8 (byte {})", e.valid_up_to());
            set_errmsg(errmsg, &MorlocError::Serialization(msg));
            return ptr::null_mut();
        }
    };

    let dest_opt = if dest.is_null() { None } else { Some(dest) };
    match crate::json::read_json_with_schema_dest(dest_opt, &json, &rs) {
        Ok(ptr) => ptr,
        Err(e) => {
            set_errmsg(errmsg, &e);
            ptr::null_mut()
        }
    }
}

// ── voidstar_to_json_string ────────────────────────────────────────────────

pub(crate) unsafe fn voidstar_to_json_string(
    data: *const c_void,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut c_char {
    clear_errmsg(errmsg);
    let rs = crate::packet_ffi::rust_schema(schema);
    match crate::json::voidstar_to_json_string(data as *mut u8, &rs) {
        Ok(s) => {
            match CString::new(s) {
                Ok(cs) => crate::error::malloc_cstr(&cs),
                Err(_) => {
                    set_errmsg(errmsg, &MorlocError::Other("CString conversion failed".into()));
                    ptr::null_mut()
                }
            }
        }
        Err(e) => {
            set_errmsg(errmsg, &e);
            ptr::null_mut()
        }
    }
}

// ── voidstar_to_raw_bytes ──────────────────────────────────────────────────

/// Serialize a `-f raw` value (`Str` / `Vector U8` / list variants) to its raw
/// content bytes in a `libc::malloc`'d buffer (so the daemon can free it with
/// `libc::free` in `daemon_free_response`). `*out_len` receives the byte length.
/// Returns null with `errmsg` set on error. Used for a media-typed daemon HTTP
/// response body.
pub(crate) unsafe fn voidstar_to_raw_bytes(
    data: *const c_void,
    schema: *const CSchema,
    out_len: *mut usize,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);
    if !out_len.is_null() {
        *out_len = 0;
    }
    let rs = crate::packet_ffi::rust_schema(schema);
    match crate::json::voidstar_raw_to_bytes(data as *mut u8, &rs) {
        Ok(bytes) => {
            let len = bytes.len();
            let buf = libc::malloc(len.max(1)) as *mut u8;
            if buf.is_null() {
                set_errmsg(errmsg, &MorlocError::Other("malloc failed for raw bytes".into()));
                return ptr::null_mut();
            }
            if len > 0 {
                ptr::copy_nonoverlapping(bytes.as_ptr(), buf, len);
            }
            if !out_len.is_null() {
                *out_len = len;
            }
            buf
        }
        Err(e) => {
            set_errmsg(errmsg, &e);
            ptr::null_mut()
        }
    }
}

// ── print_voidstar ─────────────────────────────────────────────────────────

use morloc_runtime_types::{PRINT_RESULT_OK, PRINT_RESULT_ERR, PRINT_RESULT_PIPE_CLOSED};

pub(crate) unsafe fn print_voidstar(
    data: *const c_void,
    schema: *const CSchema,
    keep_null: bool,
    errmsg: *mut *mut c_char,
) -> i32 {
    print_dispatch(data, schema, keep_null, errmsg, crate::json::print_voidstar)
}

pub(crate) unsafe fn pretty_print_voidstar(
    data: *const c_void,
    schema: *const CSchema,
    keep_null: bool,
    errmsg: *mut *mut c_char,
) -> i32 {
    print_dispatch(data, schema, keep_null, errmsg, crate::json::pretty_print_voidstar)
}

/// Emit `data` as JSON-lines (one element per line for list schemas,
/// one line total for scalars). Streams element-by-element -- peak
/// memory is one element's JSON body, not the whole list.
pub(crate) unsafe fn print_voidstar_jsonl(
    data: *const c_void,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    let rs = crate::packet_ffi::rust_schema(schema);
    match crate::json::print_voidstar_jsonl(data as *mut u8, &rs) {
        Ok(()) => PRINT_RESULT_OK,
        Err(MorlocError::PipeClosed) => PRINT_RESULT_PIPE_CLOSED,
        Err(e) => { set_errmsg(errmsg, &e); PRINT_RESULT_ERR }
    }
}

use morloc_runtime_types::{RAW_FAILED, RAW_NOT_HANDLED, RAW_PIPE_CLOSED, RAW_WRITTEN};

/// Write the bodies of a `[Str]`, `[[U8]]` or `[U8]` stream sub-packet to
/// `fd` from where they lie in `bytes`, with no copy into shared memory.
/// Returns `RAW_NOT_HANDLED` for any other sub-packet (compressed, another
/// format or schema), which the caller then writes the general way.
#[cfg(test)]
pub(crate) unsafe fn write_raw_subpacket(bytes: &[u8], fd: i32) -> Result<i32, MorlocError> {
    use morloc_runtime_types::packet as pk;
    if bytes.len() < 32 {
        return Ok(RAW_NOT_HANDLED);
    }
    let header = pk::PacketHeader::from_bytes(bytes[..32].try_into().unwrap())?;
    let meta_end = 32 + header.offset as usize;
    let Some(end) = meta_end.checked_add(header.length as usize).filter(|&e| e <= bytes.len()) else {
        return Ok(RAW_NOT_HANDLED);
    };
    write_raw_parts(&bytes[..meta_end], &bytes[meta_end..end], fd)
}

/// `write_raw_subpacket` of a sub-packet held in two parts: `head`, its
/// header and metadata, and `payload`, the region the header's length
/// describes, whole or less its alignment padding. The bodies are written
/// as they lie: their producers checked them (FMT-10), and only a debug
/// build checks them again (DIM-3).
pub(crate) unsafe fn write_raw_parts(head: &[u8], payload: &[u8], fd: i32) -> Result<i32, MorlocError> {
    use crate::json::RawShape;
    use crate::voidstar::{Local, Space};
    use morloc_runtime_types::packet as pk;
    use morloc_runtime_types::shm_types::Array;
    if head.len() < 32 {
        return Ok(RAW_NOT_HANDLED);
    }
    let header = pk::PacketHeader::from_bytes(head[..32].try_into().unwrap())?;
    if !header.is_data() {
        return Ok(RAW_NOT_HANDLED);
    }
    let data = header.command.data;
    if data.format != pk::PACKET_FORMAT_VOIDSTAR || data.compression != pk::PACKET_COMPRESSION_NONE {
        return Ok(RAW_NOT_HANDLED);
    }
    let meta_end = 32 + header.offset as usize;
    let length = header.length as usize;
    if head.len() != meta_end || payload.len() > length || length - payload.len() >= 8 {
        return Ok(RAW_NOT_HANDLED);
    }
    let Some(schema) = pk::read_schema_from_meta(head)? else {
        return Ok(RAW_NOT_HANDLED);
    };
    let schema = crate::schema::parse_schema(&schema)?;
    let per_element = match crate::json::raw_shape(&schema) {
        Some(RawShape::Bodies) => true,
        Some(RawShape::Bytes) => false,
        Some(RawShape::Body) | None => return Ok(RAW_NOT_HANDLED),
    };
    if payload.len() < std::mem::size_of::<Array>() {
        return Err(MorlocError::Packet("a stdout sub-packet is shorter than its root".into()));
    }
    let space = Local::new(payload.as_ptr(), payload.len());
    let root: Array = std::ptr::read_unaligned(payload.as_ptr() as *const Array);
    let text = schema.parameters[0].serial_type == crate::schema::SerialType::String;
    let mut bodies: Vec<(*const u8, usize)> = Vec::new();
    let mut push = |p: *const u8, n: usize| match bodies.last_mut() {
        Some((q, m)) if (*q).wrapping_add(*m) == p => *m += n,
        _ => bodies.push((p, n)),
    };
    if root.size > 0 {
        if per_element {
            let w = std::mem::size_of::<Array>();
            let elems = space.resolve(root.data, root.size.checked_mul(w).ok_or_else(|| MorlocError::Packet("a stdout sub-packet is too large".into()))?)? as *const u8;
            for i in 0..root.size {
                let a: Array = std::ptr::read_unaligned(elems.add(i * w) as *const Array);
                if a.size > 0 {
                    let p = space.resolve(a.data, a.size)? as *const u8;
                    if text {
                        crate::voidstar::check_program_text(p, a.size)?;
                    }
                    push(p, a.size);
                }
            }
        } else {
            push(space.resolve(root.data, root.size)? as *const u8, root.size);
        }
    }
    writev_all(fd, &bodies)
}

/// Write every body in order, resuming after a short write.
unsafe fn writev_all(fd: i32, bodies: &[(*const u8, usize)]) -> Result<i32, MorlocError> {
    writev_all_with(fd, bodies, |iov| libc::writev(fd, iov.as_ptr(), iov.len() as i32))
}

/// `writev_all`, through `write`, which returns as `writev` does.
unsafe fn writev_all_with(
    fd: i32,
    bodies: &[(*const u8, usize)],
    mut write: impl FnMut(&[libc::iovec]) -> isize,
) -> Result<i32, MorlocError> {
    const MAX_IOV: usize = 1024;
    const MAX_BYTES: usize = 1 << 30;
    let mut i = 0;
    let mut done_in_first = 0usize;
    let mut iov: Vec<libc::iovec> = Vec::with_capacity(MAX_IOV.min(bodies.len()));
    loop {
        while i < bodies.len() && bodies[i].1 == done_in_first {
            i += 1;
            done_in_first = 0;
        }
        if i == bodies.len() {
            break;
        }
        iov.clear();
        let mut total = 0usize;
        for (k, &(p, len)) in bodies[i..].iter().take(MAX_IOV).enumerate() {
            let skip = if k == 0 { done_in_first } else { 0 };
            let take = (len - skip).min(MAX_BYTES - total);
            iov.push(libc::iovec { iov_base: p.add(skip) as *mut libc::c_void, iov_len: take });
            total += take;
            if total == MAX_BYTES {
                break;
            }
        }
        let r = write(&iov);
        if r < 0 {
            let e = std::io::Error::last_os_error();
            match e.raw_os_error() {
                Some(libc::EINTR) => continue,
                Some(libc::EPIPE) => return Ok(RAW_PIPE_CLOSED),
                Some(libc::EBADF) if fd == libc::STDOUT_FILENO => return Ok(RAW_WRITTEN),
                _ => return Err(MorlocError::Io(e)),
            }
        }
        if r == 0 {
            return Err(MorlocError::Io(std::io::Error::new(std::io::ErrorKind::WriteZero, "stdout accepted no bytes")));
        }
        let mut left = r as usize;
        while left > 0 {
            let rest = bodies[i].1 - done_in_first;
            if left >= rest {
                left -= rest;
                i += 1;
                done_in_first = 0;
            } else {
                done_in_first += left;
                left = 0;
            }
        }
    }
    Ok(RAW_WRITTEN)
}

/// Emit a `Str`/`[Str]` voidstar as verbatim bytes (the `-f raw` format).
pub(crate) unsafe fn print_voidstar_raw(
    data: *const c_void,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    let rs = crate::packet_ffi::rust_schema(schema);
    match crate::json::print_voidstar_raw(data as *mut u8, &rs) {
        Ok(()) => PRINT_RESULT_OK,
        Err(MorlocError::PipeClosed) => PRINT_RESULT_PIPE_CLOSED,
        Err(e) => { set_errmsg(errmsg, &e); PRINT_RESULT_ERR }
    }
}

unsafe fn print_dispatch(
    data: *const c_void,
    schema: *const CSchema,
    keep_null: bool,
    errmsg: *mut *mut c_char,
    write: unsafe fn(*mut u8, &crate::schema::Schema, bool) -> Result<(), MorlocError>,
) -> i32 {
    clear_errmsg(errmsg);
    let rs = crate::packet_ffi::rust_schema(schema);
    // SAFETY: the C caller passes a live value of `schema`.
    match unsafe { write(data as *mut u8, &rs, keep_null) } {
        Ok(()) => PRINT_RESULT_OK,
        Err(MorlocError::PipeClosed) => PRINT_RESULT_PIPE_CLOSED,
        Err(e) => { set_errmsg(errmsg, &e); PRINT_RESULT_ERR }
    }
}

// ── json_buf API (used by daemon.c for discovery JSON) ─────────────────────

/// Dynamic JSON string builder.
pub struct JsonBuf {
    buf: String,
    needs_comma: Vec<bool>,
}

pub(crate) fn json_buf_new() -> *mut JsonBuf {
    Box::into_raw(Box::new(JsonBuf {
        buf: String::with_capacity(256),
        needs_comma: Vec::new(),
    }))
}

pub(crate) unsafe fn json_buf_free(jb: *mut JsonBuf) {
    if !jb.is_null() {
        let _ = Box::from_raw(jb);
    }
}

pub(crate) unsafe fn json_buf_finish(jb: *mut JsonBuf) -> *mut c_char {
    if jb.is_null() {
        return ptr::null_mut();
    }
    let jb = Box::from_raw(jb);
    match CString::new(jb.buf) {
        Ok(cs) => crate::error::malloc_cstr(&cs),
        Err(_) => ptr::null_mut(),
    }
}

unsafe fn jb_maybe_comma(jb: &mut JsonBuf) {
    if let Some(needs) = jb.needs_comma.last_mut() {
        if *needs {
            jb.buf.push(',');
        }
        *needs = true;
    }
}

pub(crate) unsafe fn json_write_obj_start(jb: *mut JsonBuf) {
    if jb.is_null() { return; }
    let jb = &mut *jb;
    jb_maybe_comma(jb);
    jb.buf.push('{');
    jb.needs_comma.push(false);
}

pub(crate) unsafe fn json_write_obj_end(jb: *mut JsonBuf) {
    if jb.is_null() { return; }
    let jb = &mut *jb;
    jb.needs_comma.pop();
    jb.buf.push('}');
}

pub(crate) unsafe fn json_write_arr_start(jb: *mut JsonBuf) {
    if jb.is_null() { return; }
    let jb = &mut *jb;
    jb_maybe_comma(jb);
    jb.buf.push('[');
    jb.needs_comma.push(false);
}

pub(crate) unsafe fn json_write_arr_end(jb: *mut JsonBuf) {
    if jb.is_null() { return; }
    let jb = &mut *jb;
    jb.needs_comma.pop();
    jb.buf.push(']');
}

pub(crate) unsafe fn json_write_key(jb: *mut JsonBuf, key: *const c_char) {
    if jb.is_null() || key.is_null() { return; }
    let jb = &mut *jb;
    jb_maybe_comma(jb);
    let s = CStr::from_ptr(key).to_string_lossy();
    jb.buf.push('"');
    jb.buf.push_str(&s);
    jb.buf.push_str("\":");
    // Don't set needs_comma — the value will follow immediately
    if let Some(needs) = jb.needs_comma.last_mut() {
        *needs = false;
    }
}

pub(crate) unsafe fn json_write_string(jb: *mut JsonBuf, val: *const c_char) {
    if jb.is_null() { return; }
    let jb = &mut *jb;
    jb_maybe_comma(jb);
    if val.is_null() {
        jb.buf.push_str("null");
    } else {
        let s = CStr::from_ptr(val).to_string_lossy();
        // JSON-escape the string
        jb.buf.push('"');
        for ch in s.chars() {
            match ch {
                '"' => jb.buf.push_str("\\\""),
                '\\' => jb.buf.push_str("\\\\"),
                '\n' => jb.buf.push_str("\\n"),
                '\r' => jb.buf.push_str("\\r"),
                '\t' => jb.buf.push_str("\\t"),
                c if c < '\x20' => {
                    jb.buf.push_str(&format!("\\u{:04x}", c as u32));
                }
                c => jb.buf.push(c),
            }
        }
        jb.buf.push('"');
    }
}

pub(crate) unsafe fn json_write_int(jb: *mut JsonBuf, val: i64) {
    if jb.is_null() { return; }
    let jb = &mut *jb;
    jb_maybe_comma(jb);
    jb.buf.push_str(&val.to_string());
}

pub(crate) unsafe fn json_write_uint(jb: *mut JsonBuf, val: u64) {
    if jb.is_null() { return; }
    let jb = &mut *jb;
    jb_maybe_comma(jb);
    jb.buf.push_str(&val.to_string());
}

pub(crate) unsafe fn json_write_bool(jb: *mut JsonBuf, val: bool) {
    if jb.is_null() { return; }
    let jb = &mut *jb;
    jb_maybe_comma(jb);
    jb.buf.push_str(if val { "true" } else { "false" });
}

pub(crate) unsafe fn json_write_null(jb: *mut JsonBuf) {
    if jb.is_null() { return; }
    let jb = &mut *jb;
    jb_maybe_comma(jb);
    jb.buf.push_str("null");
}

pub(crate) unsafe fn json_write_raw(jb: *mut JsonBuf, raw: *const c_char) {
    if jb.is_null() || raw.is_null() { return; }
    let jb = &mut *jb;
    jb_maybe_comma(jb);
    let s = CStr::from_ptr(raw).to_string_lossy();
    jb.buf.push_str(&s);
}

mod c_abi {
    use super::*;

    #[no_mangle]
    pub unsafe extern "C" fn quoted(input: *const c_char) -> *mut c_char {
        super::quoted(input)
    }

    #[no_mangle]
    pub unsafe extern "C" fn read_json_with_schema(dest: *mut u8, json_str: *mut c_char, schema: *const CSchema, errmsg: *mut *mut c_char) -> *mut u8 {
        super::read_json_with_schema(dest, json_str, schema, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn voidstar_to_json_string(data: *const c_void, schema: *const CSchema, errmsg: *mut *mut c_char) -> *mut c_char {
        super::voidstar_to_json_string(data, schema, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn voidstar_to_raw_bytes(data: *const c_void, schema: *const CSchema, out_len: *mut usize, errmsg: *mut *mut c_char) -> *mut u8 {
        super::voidstar_to_raw_bytes(data, schema, out_len, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn print_voidstar(data: *const c_void, schema: *const CSchema, keep_null: bool, errmsg: *mut *mut c_char) -> i32 {
        super::print_voidstar(data, schema, keep_null, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn pretty_print_voidstar(data: *const c_void, schema: *const CSchema, keep_null: bool, errmsg: *mut *mut c_char) -> i32 {
        super::pretty_print_voidstar(data, schema, keep_null, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn print_voidstar_jsonl(data: *const c_void, schema: *const CSchema, errmsg: *mut *mut c_char) -> i32 {
        super::print_voidstar_jsonl(data, schema, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn print_voidstar_raw(data: *const c_void, schema: *const CSchema, errmsg: *mut *mut c_char) -> i32 {
        super::print_voidstar_raw(data, schema, errmsg)
    }

    /// `write_raw_parts` over the C ABI: one of the `RAW_*` codes, with a
    /// message in `*errmsg` for `RAW_FAILED`. The caller vouches that the
    /// frame is one this run's program built (FMT-10): its text is not
    /// scanned outside a debug build.
    #[no_mangle]
    pub unsafe extern "C" fn mlc_write_raw_subpacket(
        head: *const u8, head_len: u64, payload: *const u8, payload_len: u64, fd: i32, errmsg: *mut *mut c_char,
    ) -> i32 {
        clear_errmsg(errmsg);
        if head.is_null() || (payload.is_null() && payload_len > 0) {
            return super::RAW_NOT_HANDLED;
        }
        let payload = if payload_len == 0 { &[][..] } else { std::slice::from_raw_parts(payload, payload_len as usize) };
        match super::write_raw_parts(std::slice::from_raw_parts(head, head_len as usize), payload, fd) {
            Ok(code) => code,
            Err(e) => { set_errmsg(errmsg, &e); super::RAW_FAILED }
        }
    }

    #[no_mangle]
    pub extern "C" fn json_buf_new() -> *mut JsonBuf {
        super::json_buf_new()
    }

    #[no_mangle]
    pub unsafe extern "C" fn json_buf_free(jb: *mut JsonBuf) {
        super::json_buf_free(jb)
    }

    #[no_mangle]
    pub unsafe extern "C" fn json_buf_finish(jb: *mut JsonBuf) -> *mut c_char {
        super::json_buf_finish(jb)
    }

    #[no_mangle]
    pub unsafe extern "C" fn json_write_obj_start(jb: *mut JsonBuf) {
        super::json_write_obj_start(jb)
    }

    #[no_mangle]
    pub unsafe extern "C" fn json_write_obj_end(jb: *mut JsonBuf) {
        super::json_write_obj_end(jb)
    }

    #[no_mangle]
    pub unsafe extern "C" fn json_write_arr_start(jb: *mut JsonBuf) {
        super::json_write_arr_start(jb)
    }

    #[no_mangle]
    pub unsafe extern "C" fn json_write_arr_end(jb: *mut JsonBuf) {
        super::json_write_arr_end(jb)
    }

    #[no_mangle]
    pub unsafe extern "C" fn json_write_key(jb: *mut JsonBuf, key: *const c_char) {
        super::json_write_key(jb, key)
    }

    #[no_mangle]
    pub unsafe extern "C" fn json_write_string(jb: *mut JsonBuf, val: *const c_char) {
        super::json_write_string(jb, val)
    }

    #[no_mangle]
    pub unsafe extern "C" fn json_write_int(jb: *mut JsonBuf, val: i64) {
        super::json_write_int(jb, val)
    }

    #[no_mangle]
    pub unsafe extern "C" fn json_write_uint(jb: *mut JsonBuf, val: u64) {
        super::json_write_uint(jb, val)
    }

    #[no_mangle]
    pub unsafe extern "C" fn json_write_bool(jb: *mut JsonBuf, val: bool) {
        super::json_write_bool(jb, val)
    }

    #[no_mangle]
    pub unsafe extern "C" fn json_write_null(jb: *mut JsonBuf) {
        super::json_write_null(jb)
    }

    #[no_mangle]
    pub unsafe extern "C" fn json_write_raw(jb: *mut JsonBuf, raw: *const c_char) {
        super::json_write_raw(jb, raw)
    }
}

#[cfg(test)]
mod raw_write_tests {
    use morloc_runtime_types::packet as pk;

    fn subpacket(schema: &str, payload: &[u8], compression: u8) -> Vec<u8> {
        let meta_len = (8 + schema.len() + 1 + 31) / 32 * 32;
        let mut hdr = pk::PacketHeader::data_mesg(pk::PACKET_FORMAT_VOIDSTAR, payload.len() as u64);
        hdr.offset = meta_len as u32;
        hdr.command.data.compression = compression;
        let mut out = hdr.to_bytes().to_vec();
        let mut meta = vec![0u8; meta_len];
        meta[..3].copy_from_slice(b"mmh");
        meta[3] = pk::METADATA_TYPE_SCHEMA_STRING;
        meta[4..8].copy_from_slice(&((schema.len() + 1) as u32).to_le_bytes());
        meta[8..8 + schema.len()].copy_from_slice(schema.as_bytes());
        out.extend_from_slice(&meta);
        out.extend_from_slice(payload);
        out
    }

    fn array(size: usize, data: usize) -> [u8; 16] {
        let mut a = [0u8; 16];
        a[..8].copy_from_slice(&size.to_ne_bytes());
        a[8..].copy_from_slice(&(data as i64).to_ne_bytes());
        a
    }

    fn list_payload(bodies: &[&[u8]]) -> Vec<u8> {
        let mut p = array(bodies.len(), 16).to_vec();
        let mut at = 16 + 16 * bodies.len();
        for b in bodies {
            p.extend_from_slice(&array(b.len(), at));
            at += b.len();
        }
        for b in bodies {
            p.extend_from_slice(b);
        }
        p
    }

    fn written(bytes: &[u8]) -> (i32, Vec<u8>) {
        let mut fds = [0i32; 2];
        assert_eq!(unsafe { libc::pipe(fds.as_mut_ptr()) }, 0);
        let code = unsafe { super::write_raw_subpacket(bytes, fds[1]) }.unwrap();
        unsafe { libc::close(fds[1]) };
        let mut got = Vec::new();
        let mut buf = [0u8; 4096];
        loop {
            let n = unsafe { libc::read(fds[0], buf.as_mut_ptr() as *mut libc::c_void, buf.len()) };
            if n <= 0 {
                break;
            }
            got.extend_from_slice(&buf[..n as usize]);
        }
        unsafe { libc::close(fds[0]) };
        (code, got)
    }

    #[test]
    fn text_and_byte_shapes_are_written_and_every_other_shape_is_declined() {
        let bodies: &[&[u8]] = &[b"one\n", b"", b"two\n"];
        let list = list_payload(bodies);
        for schema in ["as", "aau1"] {
            assert_eq!(written(&subpacket(schema, &list, 0)), (super::RAW_WRITTEN, b"one\ntwo\n".to_vec()), "{schema}");
        }
        let mut flat = array(5, 16).to_vec();
        flat.extend_from_slice(b"bytes");
        assert_eq!(written(&subpacket("au1", &flat, 0)), (super::RAW_WRITTEN, b"bytes".to_vec()));
        assert_eq!(written(&subpacket("as", &array(0, 16), 0)), (super::RAW_WRITTEN, Vec::new()));
        for schema in ["aai8", "aas", "ai8", "s", "a?s"] {
            assert_eq!(written(&subpacket(schema, &list, 0)), (super::RAW_NOT_HANDLED, Vec::new()), "{schema}");
        }
        assert_eq!(written(&subpacket("as", &list, pk::PACKET_COMPRESSION_ZSTD)), (super::RAW_NOT_HANDLED, Vec::new()));
    }

    fn written_parts(head: &[u8], payload: &[u8]) -> (i32, Vec<u8>) {
        let mut fds = [0i32; 2];
        assert_eq!(unsafe { libc::pipe(fds.as_mut_ptr()) }, 0);
        let code = unsafe { super::write_raw_parts(head, payload, fds[1]) }.unwrap();
        unsafe { libc::close(fds[1]) };
        let mut got = Vec::new();
        let mut buf = [0u8; 4096];
        loop {
            let n = unsafe { libc::read(fds[0], buf.as_mut_ptr() as *mut libc::c_void, buf.len()) };
            if n <= 0 {
                break;
            }
            got.extend_from_slice(&buf[..n as usize]);
        }
        unsafe { libc::close(fds[0]) };
        (code, got)
    }

    #[test]
    fn a_sub_packet_in_head_and_payload_parts_writes_what_the_joined_one_does() {
        let bodies: &[&[u8]] = &[b"alpha\n", b"", b"be", b"ta\n"];
        let list = list_payload(bodies);
        for (schema, payload) in [("as", list.clone()), ("aau1", list.clone()), ("au1", {
            let mut flat = array(5, 16).to_vec();
            flat.extend_from_slice(b"bytes");
            flat
        })] {
            let pad = payload.len().next_multiple_of(8) - payload.len();
            let mut padded = payload.clone();
            padded.resize(payload.len() + pad, 0);
            let joined = subpacket(schema, &padded, 0);
            let head_len = joined.len() - padded.len();
            let whole = written(&joined);
            assert_eq!(whole.0, super::RAW_WRITTEN, "{schema}");
            assert_eq!(written_parts(&joined[..head_len], &payload), whole, "{schema}: payload without its padding");
            assert_eq!(written_parts(&joined[..head_len], &padded), whole, "{schema}: payload with its padding");
        }
        let joined = subpacket("as", &list, 0);
        let head_len = joined.len() - list.len();
        assert_eq!(written_parts(&joined[..head_len - 1], &joined[head_len - 1..]).0, super::RAW_NOT_HANDLED);
        assert_eq!(written_parts(&joined[..head_len], &list[..list.len() - 8]).0, super::RAW_NOT_HANDLED);
        assert_eq!(written_parts(&joined[..head_len], &joined[head_len - 8..]).0, super::RAW_NOT_HANDLED);
    }

    #[test]
    fn empty_bodies_anywhere_are_written_as_nothing() {
        let mut fds = [0i32; 2];
        assert_eq!(unsafe { libc::pipe(fds.as_mut_ptr()) }, 0);
        let (a, b) = (b"ab".to_vec(), b"cd".to_vec());
        let bodies = [(a.as_ptr(), 0), (a.as_ptr(), 2), (b.as_ptr(), 0), (b.as_ptr(), 2), (b.as_ptr(), 0)];
        assert_eq!(unsafe { super::writev_all(fds[1], &bodies[..1]) }.unwrap(), super::RAW_WRITTEN);
        assert_eq!(unsafe { super::writev_all(fds[1], &bodies) }.unwrap(), super::RAW_WRITTEN);
        unsafe { libc::close(fds[1]) };
        let mut buf = [0u8; 16];
        let n = unsafe { libc::read(fds[0], buf.as_mut_ptr() as *mut libc::c_void, buf.len()) };
        unsafe { libc::close(fds[0]) };
        assert_eq!(&buf[..n as usize], b"abcd");
    }

    #[test]
    fn a_body_outside_the_payload_is_refused() {
        let mut list = list_payload(&[b"abc"]);
        list[16..24].copy_from_slice(&999usize.to_ne_bytes());
        assert!(unsafe { super::write_raw_subpacket(&subpacket("as", &list, 0), -1) }.is_err());
    }

    #[test]
    fn short_writes_resume_inside_a_body() {
        let bodies_owned: Vec<Vec<u8>> = (0..2100u32)
            .map(|i| format!("line {i}\n").into_bytes())
            .chain([Vec::new(), vec![b'x'; 5000], b"end".to_vec()])
            .collect();
        let bodies: Vec<(*const u8, usize)> = bodies_owned.iter().map(|b| (b.as_ptr(), b.len())).collect();
        for cap in [1usize, 3, 7, 1000, 4096] {
            let mut got = Vec::new();
            let code = unsafe {
                super::writev_all_with(-1, &bodies, |iov| {
                    let mut left = cap;
                    for v in iov {
                        let n = v.iov_len.min(left);
                        got.extend_from_slice(std::slice::from_raw_parts(v.iov_base as *const u8, n));
                        left -= n;
                        if left == 0 {
                            break;
                        }
                    }
                    (cap - left) as isize
                })
            }
            .unwrap();
            assert_eq!(code, super::RAW_WRITTEN);
            assert!(got == bodies_owned.concat(), "cap {cap}: the bytes written differ from the bodies");
        }
    }

    const CHILD_ENV: &str = "MORLOC_RAW_WRITE_TEST_CHILD";

    fn status_of_child(name: &str, env: &[(&str, &str)]) -> i32 {
        let mut cmd = std::process::Command::new(std::env::current_exe().unwrap());
        cmd.args([&format!("json_ffi::raw_write_tests::{name}"), "--exact", "--ignored", "--test-threads=1"])
            .env(CHILD_ENV, "1")
            .env_remove("MORLOC_SKIP_UTF8_CHECK")
            .env_remove("MORLOC_PROGRAM_SKIPS_UTF8_CHECK")
            .env_remove(crate::origin::CODE_CHECKS_ENV)
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null());
        for (k, v) in env {
            cmd.env(k, v);
        }
        cmd.status().unwrap().code().unwrap_or(-1)
    }

    fn not_text() -> Vec<u8> {
        list_payload(&[b"ok\n", b"a\xffb\n", b"\xe2\x82"])
    }

    // FMT-10
    #[test]
    fn raw_output_writes_a_str_that_is_not_text_byte_for_byte() {
        let packet = subpacket("as", &not_text(), 0);
        assert_eq!(written(&packet), (super::RAW_WRITTEN, b"ok\na\xffb\n\xe2\x82".to_vec()));
    }

    // FMT-10, DIM-3
    #[test]
    fn a_debug_build_checks_the_text_of_its_own_raw_output_unless_the_check_is_off() {
        assert_eq!(status_of_child("child_writes_a_str_that_is_not_text", &[(crate::origin::CODE_CHECKS_ENV, "1")]), 0);
        assert_eq!(
            status_of_child(
                "child_writes_a_str_that_is_not_text",
                &[(crate::origin::CODE_CHECKS_ENV, "1"), ("MORLOC_SKIP_UTF8_CHECK", "1")]
            ),
            0
        );
    }

    #[test]
    #[ignore]
    fn child_writes_a_str_that_is_not_text() {
        if std::env::var_os(CHILD_ENV).is_none() {
            return;
        }
        let packet = subpacket("as", &not_text(), 0);
        let ok = if morloc_runtime_types::utf8::skip_utf8_check() {
            written(&packet) == (super::RAW_WRITTEN, b"ok\na\xffb\n\xe2\x82".to_vec())
        } else {
            let e = unsafe { super::write_raw_subpacket(&packet, -1) }.unwrap_err().to_string();
            e.contains("not valid UTF-8 (byte 1 of 4)") && !e.contains("outside")
        };
        unsafe { libc::_exit(if ok { 0 } else { 1 }) };
    }

    // FMT-10, DIM-3
    #[test]
    fn a_stdout_sub_packet_is_materialized_as_the_programs_own_value() {
        let _g = crate::init_test_shm();
        let elem = crate::schema::parse_schema("s").unwrap();
        let list = crate::schema::parse_schema("as").unwrap();
        let packet = subpacket("as", &not_text(), 0);
        let outside = crate::stream::shared_materialize_subpacket_from_bytes(&packet, &elem, crate::origin::Origin::Outside)
            .expect_err("text read from outside is checked")
            .to_string();
        assert!(outside.contains("read from outside"), "{outside}");
        let own = crate::stream::shared_materialize_subpacket_from_bytes(&packet, &elem, crate::origin::Origin::Program).unwrap();
        let json = crate::json::voidstar_to_json_string(own, &list).expect_err("JSON cannot carry a Str that is not text").to_string();
        crate::shm::shfree(own).unwrap();
        assert!(json.contains("not valid UTF-8 (byte 1 of 4)") && !json.contains("outside"), "{json}");
    }

    // FMT-10, DIM-3
    #[test]
    fn a_debug_build_checks_the_text_of_its_own_stdout_unless_the_check_is_off() {
        assert_eq!(status_of_child("child_materializes_a_str_that_is_not_text", &[(crate::origin::CODE_CHECKS_ENV, "1")]), 0);
        assert_eq!(
            status_of_child(
                "child_materializes_a_str_that_is_not_text",
                &[(crate::origin::CODE_CHECKS_ENV, "1"), ("MORLOC_SKIP_UTF8_CHECK", "1")]
            ),
            0
        );
    }

    #[test]
    #[ignore]
    fn child_materializes_a_str_that_is_not_text() {
        if std::env::var_os(CHILD_ENV).is_none() {
            return;
        }
        let _g = crate::init_test_shm();
        let elem = crate::schema::parse_schema("s").unwrap();
        let packet = subpacket("as", &not_text(), 0);
        let got = crate::stream::shared_materialize_subpacket_from_bytes(&packet, &elem, crate::origin::Origin::Program);
        let ok = match got {
            Ok(p) => {
                let _ = crate::shm::shfree(p);
                morloc_runtime_types::utf8::skip_utf8_check()
            }
            Err(e) => {
                let e = e.to_string();
                !morloc_runtime_types::utf8::skip_utf8_check() && e.contains("not valid UTF-8 (byte 1 of 4)") && !e.contains("outside")
            }
        };
        unsafe { libc::_exit(if ok { 0 } else { 1 }) };
    }

    #[test]
    fn bodies_written_through_a_pipe_arrive_whole_and_in_order() {
        let mut fds = [0i32; 2];
        assert_eq!(unsafe { libc::pipe(fds.as_mut_ptr()) }, 0);
        let (r, w) = (fds[0], fds[1]);
        let bodies_owned: Vec<Vec<u8>> = (0..3000u32)
            .map(|i| format!("line {i}\n").into_bytes())
            .chain(std::iter::once(vec![b'x'; 1 << 20]))
            .collect();
        let want: Vec<u8> = bodies_owned.concat();
        let reader = std::thread::spawn(move || {
            let mut got = Vec::new();
            let mut buf = [0u8; 777];
            loop {
                let n = unsafe { libc::read(r, buf.as_mut_ptr() as *mut libc::c_void, buf.len()) };
                if n <= 0 {
                    break;
                }
                got.extend_from_slice(&buf[..n as usize]);
            }
            unsafe { libc::close(r) };
            got
        });
        let bodies: Vec<(*const u8, usize)> = bodies_owned.iter().map(|b| (b.as_ptr(), b.len())).collect();
        let code = unsafe { super::writev_all(w, &bodies) }.unwrap();
        unsafe { libc::close(w) };
        assert_eq!(code, super::RAW_WRITTEN);
        assert!(reader.join().unwrap() == want, "the bytes read differ from the bodies written");
    }
}
