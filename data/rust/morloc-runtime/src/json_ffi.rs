//! C ABI wrappers for JSON functions.
//! Replaces json.c's core functions with calls to Rust json.rs.
//! Arrow output and json_buf API are also implemented here.

use std::ffi::{c_char, c_void, CStr, CString};
use std::ptr;

use crate::cschema::CSchema;
use crate::error::{clear_errmsg, set_errmsg, MorlocError};

// ── quoted ─────────────────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn quoted(input: *const c_char) -> *mut c_char {
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

#[no_mangle]
pub unsafe extern "C" fn read_json_with_schema(
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

    let rs = CSchema::to_rust(schema);
    let json = CStr::from_ptr(json_str).to_string_lossy();

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

#[no_mangle]
pub unsafe extern "C" fn voidstar_to_json_string(
    data: *const c_void,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut c_char {
    clear_errmsg(errmsg);
    let rs = CSchema::to_rust(schema);
    match crate::json::voidstar_to_json_string(data as *mut u8, &rs) {
        Ok(s) => {
            match CString::new(s) {
                Ok(cs) => cs.into_raw(),
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
#[no_mangle]
pub unsafe extern "C" fn voidstar_to_raw_bytes(
    data: *const c_void,
    schema: *const CSchema,
    out_len: *mut usize,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);
    if !out_len.is_null() {
        *out_len = 0;
    }
    let rs = CSchema::to_rust(schema);
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

#[no_mangle]
pub unsafe extern "C" fn print_voidstar(
    data: *const c_void,
    schema: *const CSchema,
    keep_null: bool,
    errmsg: *mut *mut c_char,
) -> i32 {
    print_dispatch(data, schema, keep_null, errmsg, crate::json::print_voidstar)
}

#[no_mangle]
pub unsafe extern "C" fn pretty_print_voidstar(
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
#[no_mangle]
pub unsafe extern "C" fn print_voidstar_jsonl(
    data: *const c_void,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    let rs = CSchema::to_rust(schema);
    match crate::json::print_voidstar_jsonl(data as *mut u8, &rs) {
        Ok(()) => PRINT_RESULT_OK,
        Err(MorlocError::PipeClosed) => PRINT_RESULT_PIPE_CLOSED,
        Err(e) => { set_errmsg(errmsg, &e); PRINT_RESULT_ERR }
    }
}

/// Emit a `Str`/`[Str]` voidstar as verbatim bytes (the `-f raw` format).
#[no_mangle]
pub unsafe extern "C" fn print_voidstar_raw(
    data: *const c_void,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    let rs = CSchema::to_rust(schema);
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
    write: fn(*mut u8, &crate::schema::Schema, bool) -> Result<(), MorlocError>,
) -> i32 {
    clear_errmsg(errmsg);
    let rs = CSchema::to_rust(schema);
    match write(data as *mut u8, &rs, keep_null) {
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

#[no_mangle]
pub extern "C" fn json_buf_new() -> *mut JsonBuf {
    Box::into_raw(Box::new(JsonBuf {
        buf: String::with_capacity(256),
        needs_comma: Vec::new(),
    }))
}

#[no_mangle]
pub unsafe extern "C" fn json_buf_free(jb: *mut JsonBuf) {
    if !jb.is_null() {
        let _ = Box::from_raw(jb);
    }
}

#[no_mangle]
pub unsafe extern "C" fn json_buf_finish(jb: *mut JsonBuf) -> *mut c_char {
    if jb.is_null() {
        return ptr::null_mut();
    }
    let jb = Box::from_raw(jb);
    match CString::new(jb.buf) {
        Ok(cs) => cs.into_raw(),
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

#[no_mangle]
pub unsafe extern "C" fn json_write_obj_start(jb: *mut JsonBuf) {
    if jb.is_null() { return; }
    let jb = &mut *jb;
    jb_maybe_comma(jb);
    jb.buf.push('{');
    jb.needs_comma.push(false);
}

#[no_mangle]
pub unsafe extern "C" fn json_write_obj_end(jb: *mut JsonBuf) {
    if jb.is_null() { return; }
    let jb = &mut *jb;
    jb.needs_comma.pop();
    jb.buf.push('}');
}

#[no_mangle]
pub unsafe extern "C" fn json_write_arr_start(jb: *mut JsonBuf) {
    if jb.is_null() { return; }
    let jb = &mut *jb;
    jb_maybe_comma(jb);
    jb.buf.push('[');
    jb.needs_comma.push(false);
}

#[no_mangle]
pub unsafe extern "C" fn json_write_arr_end(jb: *mut JsonBuf) {
    if jb.is_null() { return; }
    let jb = &mut *jb;
    jb.needs_comma.pop();
    jb.buf.push(']');
}

#[no_mangle]
pub unsafe extern "C" fn json_write_key(jb: *mut JsonBuf, key: *const c_char) {
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

#[no_mangle]
pub unsafe extern "C" fn json_write_string(jb: *mut JsonBuf, val: *const c_char) {
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

#[no_mangle]
pub unsafe extern "C" fn json_write_int(jb: *mut JsonBuf, val: i64) {
    if jb.is_null() { return; }
    let jb = &mut *jb;
    jb_maybe_comma(jb);
    jb.buf.push_str(&val.to_string());
}

#[no_mangle]
pub unsafe extern "C" fn json_write_uint(jb: *mut JsonBuf, val: u64) {
    if jb.is_null() { return; }
    let jb = &mut *jb;
    jb_maybe_comma(jb);
    jb.buf.push_str(&val.to_string());
}

#[no_mangle]
pub unsafe extern "C" fn json_write_bool(jb: *mut JsonBuf, val: bool) {
    if jb.is_null() { return; }
    let jb = &mut *jb;
    jb_maybe_comma(jb);
    jb.buf.push_str(if val { "true" } else { "false" });
}

#[no_mangle]
pub unsafe extern "C" fn json_write_null(jb: *mut JsonBuf) {
    if jb.is_null() { return; }
    let jb = &mut *jb;
    jb_maybe_comma(jb);
    jb.buf.push_str("null");
}

#[no_mangle]
pub unsafe extern "C" fn json_write_raw(jb: *mut JsonBuf, raw: *const c_char) {
    if jb.is_null() || raw.is_null() { return; }
    let jb = &mut *jb;
    jb_maybe_comma(jb);
    let s = CStr::from_ptr(raw).to_string_lossy();
    jb.buf.push_str(&s);
}

