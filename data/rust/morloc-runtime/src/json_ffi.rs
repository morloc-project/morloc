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

// ── print_voidstar ─────────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn print_voidstar(
    data: *const c_void,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> bool {
    clear_errmsg(errmsg);
    let rs = CSchema::to_rust(schema);
    match crate::json::print_voidstar(data as *mut u8, &rs) {
        Ok(_) => true,
        Err(e) => {
            set_errmsg(errmsg, &e);
            false
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn pretty_print_voidstar(
    data: *const c_void,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> bool {
    clear_errmsg(errmsg);
    let rs = CSchema::to_rust(schema);
    match crate::json::pretty_print_voidstar(data as *mut u8, &rs) {
        Ok(_) => true,
        Err(e) => {
            set_errmsg(errmsg, &e);
            false
        }
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

// ── Arrow JSON output ──────────────────────────────────────────────────────
// Arrow output is complex and depends on the Arrow C Data Interface.
// These are implemented in C (arrow_json.c) and linked via the hybrid build.
// The functions below are stubs that will be overridden by the C implementations
// when we create a separate arrow_json.c file.
// For now, remove the Rust stubs and let C json.c's implementations be used
// from a separate compilation unit.

// print_arrow_as_json and print_arrow_as_table are provided by the C
// arrow_json code (extracted from json.c, kept in build as arrow_json.c)

#[allow(dead_code)]
unsafe fn _print_arrow_as_json_stub(
    data: *const c_void,
    errmsg: *mut *mut c_char,
) -> bool {
    clear_errmsg(errmsg);

    // Use the arrow.c implementation which handles the Arrow C Data Interface
    extern "C" {
        fn arrow_column_desc(
            header: *const c_void,
            col_idx: usize,
        ) -> *const c_void;
        fn arrow_column_data(
            header: *const c_void,
            col_idx: usize,
        ) -> *const c_void;
        fn arrow_column_name(
            header: *const c_void,
            col_idx: usize,
        ) -> *const c_char;
    }

    // Read arrow_shm_header fields
    // arrow_shm_header_t: { magic: u32, n_columns: u32, n_rows: u64, ... }
    let header = data as *const u8;
    let n_columns = *(header.add(4) as *const u32) as usize;
    let n_rows = *(header.add(8) as *const u64) as usize;

    // Build JSON array of objects
    print!("[");
    for row in 0..n_rows {
        if row > 0 { print!(","); }
        print!("{{");
        for col in 0..n_columns {
            if col > 0 { print!(","); }
            let name = arrow_column_name(data, col);
            let name_str = if name.is_null() { "?" } else { CStr::from_ptr(name).to_str().unwrap_or("?") };
            print!("\"{}\":", name_str);

            let desc = arrow_column_desc(data, col);
            if desc.is_null() {
                print!("null");
                continue;
            }
            // desc is arrow_column_desc_t: { type: u8, length: u32, null_count: u32, name_offset, data_offset }
            let col_type = *(desc as *const u8);
            let col_data = arrow_column_data(data, col);

            // Print value based on type
            // Types: 0=nil, 1=bool, 2=i8, ..., 11=f64, 13=string
            match col_type {
                4 => { // i32
                    let vals = col_data as *const i32;
                    print!("{}", *vals.add(row));
                }
                5 => { // i64
                    let vals = col_data as *const i64;
                    print!("{}", *vals.add(row));
                }
                11 => { // f64
                    let vals = col_data as *const f64;
                    let mut cbuf = [0u8; 64];
                    let fmt = b"%.15g\0";
                    let n = libc::snprintf(cbuf.as_mut_ptr() as *mut c_char, 64, fmt.as_ptr() as *const c_char, *vals.add(row));
                    if n > 0 {
                        let s = std::str::from_utf8(&cbuf[..n as usize]).unwrap_or("0");
                        print!("{}", s);
                    }
                }
                13 => { // string
                    // Arrow strings: offsets array + data buffer
                    // For simplicity, use arrow_column_data which gives the data pointer
                    // This is a simplified implementation — full Arrow string handling
                    // requires offset arrays
                    print!("\"<string>\"");
                }
                _ => {
                    print!("null");
                }
            }
        }
        print!("}}");
    }
    println!("]");

    true
}

#[allow(dead_code)]
unsafe fn _print_arrow_as_table_stub(
    _data: *const c_void,
    errmsg: *mut *mut c_char,
) -> bool {
    clear_errmsg(errmsg);
    // Stub — Arrow table output is rarely used
    // The full implementation would print TSV-formatted columns
    set_errmsg(errmsg, &MorlocError::Other("Arrow table output not yet implemented in Rust".into()));
    false
}
