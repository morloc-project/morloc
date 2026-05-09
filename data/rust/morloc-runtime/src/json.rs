//! JSON <-> Voidstar conversion.
//!
//! # Safety model
//!
//! All SHM pointer operations are encapsulated in `ShmWriter` (write) and
//! `ShmReader` (read). Each has a single `unsafe fn new()` constructor;
//! all subsequent reads/writes through the handle are safe methods.
//! The only remaining `unsafe` blocks are `libc::snprintf` for float
//! formatting and constructing readers/writers at known-valid offsets.

use crate::error::MorlocError;
use crate::schema::{Schema, SerialType};
use crate::shm::{self, AbsPtr, Array, RELNULL};
use indexmap::IndexMap;
use serde_json::value::RawValue;
use std::collections::HashMap;
use std::str::FromStr;

// ── Safe SHM abstractions ────────────────────────────────────────────────────

/// Write handle to a region of shared memory.
///
/// # Safety invariant
/// `ptr` was obtained from `shmalloc` and points to at least `len` writable bytes.
struct ShmWriter {
    ptr: *mut u8,
    #[cfg(debug_assertions)]
    len: usize,
}

impl ShmWriter {
    /// # Safety
    /// `ptr` must point to `len` bytes of valid, writable SHM.
    unsafe fn new(ptr: *mut u8, len: usize) -> Self {
        let _ = len;
        ShmWriter { ptr, #[cfg(debug_assertions)] len }
    }

    fn as_ptr(&self) -> *mut u8 { self.ptr }

    fn write_bytes(&self, offset: usize, src: &[u8]) {
        debug_assert!(offset + src.len() <= { #[cfg(debug_assertions)] { self.len } #[cfg(not(debug_assertions))] { usize::MAX } });
        unsafe { std::ptr::copy_nonoverlapping(src.as_ptr(), self.ptr.add(offset), src.len()); }
    }

    fn zero(&self, offset: usize, count: usize) {
        unsafe { std::ptr::write_bytes(self.ptr.add(offset), 0, count); }
    }

    fn write_val<T: Copy>(&self, offset: usize, val: T) {
        unsafe { (self.ptr.add(offset) as *mut T).write_unaligned(val); }
    }

    fn write_array_header(&self, offset: usize, size: usize, data_rel: shm::RelPtr) {
        let arr = Array { size, data: data_rel };
        unsafe {
            std::ptr::copy_nonoverlapping(
                &arr as *const Array as *const u8, self.ptr.add(offset),
                std::mem::size_of::<Array>(),
            );
        }
    }

    fn sub(&self, offset: usize, sub_len: usize) -> ShmWriter {
        unsafe { ShmWriter::new(self.ptr.add(offset), sub_len) }
    }
}

/// Read handle to SHM data.
///
/// # Safety invariant
/// `ptr` was obtained from `rel2abs`/`shmalloc` and points to valid readable SHM.
struct ShmReader { ptr: *const u8 }

impl ShmReader {
    /// # Safety
    /// `ptr` must point to valid, readable shared memory.
    unsafe fn new(ptr: *const u8) -> Self { ShmReader { ptr } }

    fn read_val<T: Copy>(&self, offset: usize) -> T {
        unsafe { (self.ptr.add(offset) as *const T).read_unaligned() }
    }
    fn read_u8(&self, offset: usize) -> u8 { self.read_val(offset) }
    fn read_array(&self, offset: usize) -> Array { unsafe { *(self.ptr.add(offset) as *const Array) } }

    fn read_str(&self, offset: usize, len: usize) -> &str {
        unsafe {
            std::str::from_utf8(std::slice::from_raw_parts(self.ptr.add(offset), len)).unwrap_or("")
        }
    }

    fn at(&self, offset: usize) -> ShmReader {
        unsafe { ShmReader::new(self.ptr.add(offset)) }
    }
}

// ── JSON -> Voidstar ───────────────────────────────────────────────────────

pub fn read_json_with_schema(json_str: &str, schema: &Schema) -> Result<AbsPtr, MorlocError> {
    read_json_with_schema_dest(None, json_str, schema)
}

pub fn read_json_with_schema_dest(
    dest: Option<AbsPtr>, json_str: &str, schema: &Schema,
) -> Result<AbsPtr, MorlocError> {
    // Parse to RawValue (preserves the raw text of every leaf). Avoids
    // serde_json's f64 fallback for numbers that exceed i64/u64, which
    // silently corrupts BigInt (`Int`) values.
    let rv: Box<RawValue> = serde_json::from_str(json_str)
        .map_err(|e| MorlocError::Serialization(format!("JSON parse error: {}", e)))?;
    json_to_voidstar(&rv, schema, dest)
}

fn alloc(dest: Option<AbsPtr>, size: usize) -> Result<ShmWriter, MorlocError> {
    let ptr = match dest { Some(p) => p, None => shm::shmalloc(size)? };
    // SAFETY: ptr from shmalloc or caller-provided valid SHM of sufficient size
    Ok(unsafe { ShmWriter::new(ptr, size) })
}

fn json_to_voidstar(
    rv: &RawValue, schema: &Schema, dest: Option<AbsPtr>,
) -> Result<AbsPtr, MorlocError> {
    let text = rv.get();
    match schema.serial_type {
        SerialType::Nil => {
            if !is_null(text) { return Err(err(&format!("expected null, got {}", truncate_for_msg(text)))); }
            let w = alloc(dest, 1)?; w.write_val::<u8>(0, 0); Ok(w.as_ptr())
        }
        SerialType::Bool => {
            let t = text.trim();
            let b = match t {
                "true" => 1u8, "false" => 0u8,
                _ => return Err(err(&format!("expected bool, got {}", truncate_for_msg(t)))),
            };
            let w = alloc(dest, 1)?; w.write_val::<u8>(0, b); Ok(w.as_ptr())
        }
        SerialType::Sint8  => { let w = alloc(dest, 1)?; w.write_val::<i8>(0,  parse_sint(text, i8::MIN  as i64, i8::MAX  as i64, "Int8")?  as i8);  Ok(w.as_ptr()) }
        SerialType::Sint16 => { let w = alloc(dest, 2)?; w.write_val::<i16>(0, parse_sint(text, i16::MIN as i64, i16::MAX as i64, "Int16")? as i16); Ok(w.as_ptr()) }
        SerialType::Sint32 => { let w = alloc(dest, 4)?; w.write_val::<i32>(0, parse_sint(text, i32::MIN as i64, i32::MAX as i64, "Int32")? as i32); Ok(w.as_ptr()) }
        SerialType::Sint64 => { let w = alloc(dest, 8)?; w.write_val::<i64>(0, parse_sint(text, i64::MIN,        i64::MAX,        "Int64")?);        Ok(w.as_ptr()) }
        SerialType::Uint8  => { let w = alloc(dest, 1)?; w.write_val::<u8>(0,  parse_uint(text, u8::MAX  as u64, "UInt8")?  as u8);  Ok(w.as_ptr()) }
        SerialType::Uint16 => { let w = alloc(dest, 2)?; w.write_val::<u16>(0, parse_uint(text, u16::MAX as u64, "UInt16")? as u16); Ok(w.as_ptr()) }
        SerialType::Uint32 => { let w = alloc(dest, 4)?; w.write_val::<u32>(0, parse_uint(text, u32::MAX as u64, "UInt32")? as u32); Ok(w.as_ptr()) }
        SerialType::Uint64 => { let w = alloc(dest, 8)?; w.write_val::<u64>(0, parse_uint(text, u64::MAX,        "UInt64")?);        Ok(w.as_ptr()) }
        SerialType::Float32 => { let w = alloc(dest, 4)?; w.write_val::<f32>(0, parse_float(text, "Float32")? as f32); Ok(w.as_ptr()) }
        SerialType::Float64 => { let w = alloc(dest, 8)?; w.write_val::<f64>(0, parse_float(text, "Float64")?);        Ok(w.as_ptr()) }

        SerialType::Int => {
            // Variable-width integer (BigInt): hand the raw digit string
            // straight to decimal_to_limbs. RawValue preserves the input
            // text exactly, so values beyond i64/u64 round-trip without
            // f64 corruption.
            let digits = extract_bigint_digits(text)?;
            let limbs = crate::eval_ffi::decimal_to_limbs(digits)?;
            let nlimbs = limbs.len();
            // Inline layout: [size:i64, value_or_relptr:i64] = 16 bytes
            let w = alloc(dest, 16)?;
            if nlimbs <= 1 {
                w.write_val::<i64>(0, nlimbs as i64);
                w.write_val::<i64>(8, if nlimbs == 1 { limbs[0] as i64 } else { 0 });
            } else {
                let limb_bytes = nlimbs * 8;
                let abs = shm::shmemcpy(limbs.as_ptr() as *const u8, limb_bytes)?;
                w.write_val::<usize>(0, nlimbs);
                w.write_val::<shm::RelPtr>(8, shm::abs2rel(abs)?);
            }
            Ok(w.as_ptr())
        }

        SerialType::String => {
            // serde_json::from_str::<String> decodes JSON escapes (\n, \uXXXX, etc.)
            let s: String = serde_json::from_str(text)
                .map_err(|e| err(&format!("expected string: {}", e)))?;
            let bytes = s.as_bytes();
            let hdr = std::mem::size_of::<Array>();

            let (w, data_rel) = if dest.is_some() {
                let w = alloc(dest, hdr)?;
                let data_rel = if bytes.is_empty() { RELNULL } else {
                    shm::abs2rel(shm::shmemcpy(bytes.as_ptr(), bytes.len())?)?
                };
                (w, data_rel)
            } else {
                let w = alloc(None, hdr + bytes.len())?;
                w.write_bytes(hdr, bytes);
                // SAFETY: data is hdr bytes into the same shmalloc block
                let data_rel = shm::abs2rel(unsafe { w.as_ptr().add(hdr) })?;
                (w, data_rel)
            };
            w.write_array_header(0, bytes.len(), data_rel);
            Ok(w.as_ptr())
        }

        SerialType::Array => {
            let children = parse_array_children(text)?;
            let es = schema.parameters.first().ok_or_else(|| err("array has no element type"))?;
            let n = children.len();
            // Validate array length against schema constraint (offsets[0], 0 = unconstrained)
            let expected = schema.offsets.first().copied().unwrap_or(0);
            if expected > 0 && n != expected {
                return Err(MorlocError::Other(format!(
                    "Array length mismatch: expected {}, got {}", expected, n
                )));
            }
            let ew = es.width;
            let hdr = std::mem::size_of::<Array>();

            let (hw, data_ptr) = if dest.is_some() {
                let hw = alloc(dest, hdr)?;
                let dp = if n > 0 { shm::shmalloc(n * ew)? } else { std::ptr::null_mut() };
                (hw, dp)
            } else {
                let w = alloc(None, hdr + n * ew)?;
                // SAFETY: data is hdr bytes into the same shmalloc block
                let dp = unsafe { w.as_ptr().add(hdr) };
                (w, dp)
            };
            let data_rel = if data_ptr.is_null() { RELNULL } else { shm::abs2rel(data_ptr)? };

            for (i, child) in children.iter().enumerate() {
                // SAFETY: data_ptr + i * ew is within the data allocation
                let ep = unsafe { data_ptr.add(i * ew) };
                json_to_voidstar(child, es, Some(ep))?;
            }
            hw.write_array_header(0, n, data_rel);
            Ok(hw.as_ptr())
        }

        SerialType::Tuple => {
            let children = parse_array_children(text)?;
            if children.len() != schema.parameters.len() {
                return Err(err(&format!("expected {} fields, got {}", schema.parameters.len(), children.len())));
            }
            let w = alloc(dest, schema.width)?;
            w.zero(0, schema.width);
            for (i, (child, fs)) in children.iter().zip(schema.parameters.iter()).enumerate() {
                let sub = w.sub(schema.offsets[i], fs.width);
                json_to_voidstar(child, fs, Some(sub.as_ptr()))?;
            }
            Ok(w.as_ptr())
        }

        SerialType::Map => {
            // Preserve the existing tolerance: a Map can be encoded either as
            // a JSON object {"k": v, ...} or as a positional JSON array
            // [v0, v1, ...]. The trimmed first byte tells us which.
            let t = text.trim_start();
            let w = alloc(dest, schema.width)?;
            w.zero(0, schema.width);
            if t.starts_with('{') {
                let obj = parse_object_children(text)?;
                for (i, (key, fs)) in schema.keys.iter().zip(schema.parameters.iter()).enumerate() {
                    let sub = w.sub(schema.offsets[i], fs.width);
                    match obj.get(key) {
                        Some(child) => { json_to_voidstar(child, fs, Some(sub.as_ptr()))?; }
                        // Missing key: synthesize a 'null' RawValue so the
                        // child handler decides whether null is acceptable
                        // (e.g. allowed for Optional, error for non-optional).
                        // Matches the old extract_fields behavior that
                        // substituted Value::Null for missing keys.
                        None => {
                            let null_rv: Box<RawValue> = serde_json::from_str("null")
                                .expect("'null' is always valid JSON");
                            json_to_voidstar(&null_rv, fs, Some(sub.as_ptr()))?;
                        }
                    }
                }
            } else {
                let children = parse_array_children(text)?;
                if children.len() != schema.parameters.len() {
                    return Err(err(&format!("expected {} fields, got {}", schema.parameters.len(), children.len())));
                }
                for (i, (child, fs)) in children.iter().zip(schema.parameters.iter()).enumerate() {
                    let sub = w.sub(schema.offsets[i], fs.width);
                    json_to_voidstar(child, fs, Some(sub.as_ptr()))?;
                }
            }
            Ok(w.as_ptr())
        }

        SerialType::Optional => {
            let inner = schema.parameters.first().ok_or_else(|| err("optional has no inner type"))?;
            let off = shm::align_up(1, inner.alignment().max(1));
            let total = off + inner.width;
            let w = alloc(dest, total)?;
            if is_null(text) {
                w.zero(0, total);
            } else {
                w.write_val::<u8>(0, 1);
                json_to_voidstar(rv, inner, Some(w.sub(off, inner.width).as_ptr()))?;
            }
            Ok(w.as_ptr())
        }
        SerialType::Table => {
            // JSON-to-Table is conceptually meaningful (read records from
            // a JSON array and build an Arrow buffer) but is not the
            // standard JSON-load path; CSV/JSON-to-Arrow lives in the
            // dedicated arrow_ipc_reader module. Hitting this branch
            // means a Table value was routed through the generic JSON
            // loader, which has no Arrow construction logic. Surface a
            // clear error rather than silently corrupt.
            Err(err("Cannot load a Table from generic JSON; use the Arrow CSV/JSON reader path"))
        }
    }
}

fn parse_array_children(text: &str) -> Result<Vec<Box<RawValue>>, MorlocError> {
    serde_json::from_str(text).map_err(|e| MorlocError::Serialization(format!(
        "expected JSON array: {} (in {})", e, truncate_for_msg(text)
    )))
}

fn parse_object_children(text: &str) -> Result<HashMap<String, Box<RawValue>>, MorlocError> {
    serde_json::from_str(text).map_err(|e| MorlocError::Serialization(format!(
        "expected JSON object: {} (in {})", e, truncate_for_msg(text)
    )))
}

// ── Voidstar -> JSON ───────────────────────────────────────────────────────

pub fn voidstar_to_json_string(ptr: AbsPtr, schema: &Schema) -> Result<String, MorlocError> {
    let mut buf = String::new();
    // SAFETY: ptr from shmalloc/rel2abs — valid SHM
    let r = unsafe { ShmReader::new(ptr) };
    to_json(&r, schema, &mut buf)?;
    Ok(buf)
}

pub fn print_voidstar(ptr: AbsPtr, schema: &Schema) -> Result<(), MorlocError> {
    println!("{}", voidstar_to_json_string(ptr, schema)?);
    Ok(())
}

pub fn pretty_print_voidstar(ptr: AbsPtr, schema: &Schema) -> Result<(), MorlocError> {
    let json = voidstar_to_json_string(ptr, schema)?;
    let rv: Box<RawValue> = serde_json::from_str(&json).map_err(|e| err(&e.to_string()))?;
    let t = rv.get().trim_start();
    // Top-level scalars: render without surrounding quotes (for strings) and
    // without re-parsing numbers (preserving BigInt precision).
    if let Some(first) = t.chars().next() {
        match first {
            '"' => {
                // JSON string - unescape and print without quotes.
                let s: String = serde_json::from_str(rv.get())
                    .map_err(|e| err(&format!("string decode: {}", e)))?;
                println!("{}", s);
                return Ok(());
            }
            '{' | '[' => {
                let mut buf = String::new();
                pretty_indent(&rv, 0, &mut buf);
                println!("{}", buf);
                return Ok(());
            }
            _ => {
                // Number, bool, or null - emit raw text. Numbers keep full
                // precision because we never round-trip through Value::Number.
                println!("{}", rv.get().trim());
                return Ok(());
            }
        }
    }
    // Empty input shouldn't occur (to_json always emits something), but be safe.
    println!("{}", rv.get());
    Ok(())
}

/// Recursively pretty-print a RawValue tree with two-space indentation.
/// Scalars (numbers, strings, bools, null) are emitted verbatim from the
/// raw text; only containers (arrays/objects) introduce whitespace.
fn pretty_indent(rv: &RawValue, depth: usize, buf: &mut String) {
    let text = rv.get().trim();
    let first = text.chars().next();
    match first {
        Some('[') => {
            // Try to parse as array of children. Fall back to raw text on parse error.
            match serde_json::from_str::<Vec<Box<RawValue>>>(text) {
                Ok(children) if !children.is_empty() => {
                    buf.push('[');
                    for (i, child) in children.iter().enumerate() {
                        if i > 0 { buf.push(','); }
                        buf.push('\n');
                        push_indent(buf, depth + 1);
                        pretty_indent(child, depth + 1, buf);
                    }
                    buf.push('\n');
                    push_indent(buf, depth);
                    buf.push(']');
                }
                _ => buf.push_str(text), // empty array, or parse error: emit verbatim
            }
        }
        Some('{') => {
            // IndexMap preserves source key order so the pretty-printed
            // object matches the JSON that to_json emitted (which is
            // schema-ordered). serde_json::Map is locked to Value, so we
            // can't use it for Box<RawValue>.
            match serde_json::from_str::<IndexMap<String, Box<RawValue>>>(text) {
                Ok(map) if !map.is_empty() => {
                    buf.push('{');
                    let n = map.len();
                    for (i, (key, child)) in map.iter().enumerate() {
                        buf.push('\n');
                        push_indent(buf, depth + 1);
                        // Re-escape the key via serde_json::to_string (returns "key" with surrounding quotes).
                        match serde_json::to_string(key) {
                            Ok(quoted) => buf.push_str(&quoted),
                            Err(_) => { buf.push('"'); buf.push_str(key); buf.push('"'); }
                        }
                        buf.push_str(": ");
                        pretty_indent(child, depth + 1, buf);
                        if i + 1 < n { buf.push(','); }
                    }
                    buf.push('\n');
                    push_indent(buf, depth);
                    buf.push('}');
                }
                _ => buf.push_str(text),
            }
        }
        // Scalars: emit raw text. Numbers (including BigInts) survive intact.
        _ => buf.push_str(text),
    }
}

fn push_indent(buf: &mut String, depth: usize) {
    for _ in 0..depth { buf.push_str("  "); }
}

fn to_json(r: &ShmReader, schema: &Schema, buf: &mut String) -> Result<(), MorlocError> {
    match schema.serial_type {
        SerialType::Nil    => buf.push_str("null"),
        SerialType::Bool   => buf.push_str(if r.read_u8(0) != 0 { "true" } else { "false" }),
        SerialType::Sint8  => buf.push_str(&(r.read_val::<i8>(0)).to_string()),
        SerialType::Sint16 => buf.push_str(&(r.read_val::<i16>(0)).to_string()),
        SerialType::Sint32 => buf.push_str(&(r.read_val::<i32>(0)).to_string()),
        SerialType::Sint64 => buf.push_str(&(r.read_val::<i64>(0)).to_string()),
        SerialType::Uint8  => buf.push_str(&r.read_u8(0).to_string()),
        SerialType::Uint16 => buf.push_str(&(r.read_val::<u16>(0)).to_string()),
        SerialType::Uint32 => buf.push_str(&(r.read_val::<u32>(0)).to_string()),
        SerialType::Uint64 => buf.push_str(&(r.read_val::<u64>(0)).to_string()),
        SerialType::Float32 => write_float(buf, r.read_val::<f32>(0) as f64, b"%.7g\0"),
        SerialType::Float64 => write_float(buf, r.read_val::<f64>(0), b"%.15g\0"),

        SerialType::Int => {
            // Inline layout: [size:i64, value_or_relptr:i64]
            let size = r.read_val::<usize>(0);
            if size == 0 {
                buf.push('0');
            } else if size == 1 {
                // Inline: second field is the value directly
                let val = r.read_val::<i64>(8);
                buf.push_str(&val.to_string());
            } else {
                // Overflow: second field is relptr to limb array
                let relptr = r.read_val::<shm::RelPtr>(8);
                let data_ptr = shm::rel2abs(relptr)?;
                let limbs: Vec<u64> = (0..size)
                    .map(|i| unsafe { *((data_ptr as *const u64).add(i)) })
                    .collect();
                buf.push_str(&crate::eval_ffi::limbs_to_decimal(&limbs));
            }
        }

        SerialType::String => {
            let arr = r.read_array(0);
            if arr.size == 0 || arr.data == RELNULL {
                buf.push_str("\"\"");
            } else {
                // SAFETY: arr.data resolved to valid SHM string bytes
                let dr = unsafe { ShmReader::new(shm::rel2abs(arr.data)?) };
                json_escape(dr.read_str(0, arr.size), buf);
            }
        }
        SerialType::Array => {
            let arr = r.read_array(0);
            let es = &schema.parameters[0];
            buf.push('[');
            if arr.size > 0 && arr.data != RELNULL {
                let data = shm::rel2abs(arr.data)?;
                for i in 0..arr.size {
                    if i > 0 { buf.push(','); }
                    // SAFETY: data + i * es.width within array data block
                    let er = unsafe { ShmReader::new(data.add(i * es.width)) };
                    to_json(&er, es, buf)?;
                }
            }
            buf.push(']');
        }
        SerialType::Tuple => {
            buf.push('[');
            for (i, fs) in schema.parameters.iter().enumerate() {
                if i > 0 { buf.push(','); }
                to_json(&r.at(schema.offsets[i]), fs, buf)?;
            }
            buf.push(']');
        }
        SerialType::Map => {
            buf.push('{');
            for (i, fs) in schema.parameters.iter().enumerate() {
                if i > 0 { buf.push(','); }
                if i < schema.keys.len() { buf.push('"'); buf.push_str(&schema.keys[i]); buf.push_str("\":"); }
                to_json(&r.at(schema.offsets[i]), fs, buf)?;
            }
            buf.push('}');
        }
        SerialType::Optional => {
            if r.read_u8(0) == 0 {
                buf.push_str("null");
            } else {
                let inner = &schema.parameters[0];
                to_json(&r.at(shm::align_up(1, inner.alignment().max(1))), inner, buf)?;
            }
        }
        SerialType::Table => {
            // Reverse of the json_to_voidstar Table branch: rendering a
            // Table to JSON requires walking the Arrow buffer's records,
            // which lives in a dedicated path (arrow-to-json), not the
            // generic JSON serialiser. Surface a clear error rather than
            // emit malformed JSON.
            return Err(err("Cannot render a Table to generic JSON; use the Arrow-to-JSON path"));
        }
    }
    Ok(())
}

// ── Helpers ────────────────────────────────────────────────────────────────

fn json_escape(s: &str, buf: &mut String) {
    buf.push('"');
    for ch in s.chars() {
        match ch {
            '"' => buf.push_str("\\\""), '\\' => buf.push_str("\\\\"), '/' => buf.push_str("\\/"),
            '\x08' => buf.push_str("\\b"), '\x0c' => buf.push_str("\\f"),
            '\n' => buf.push_str("\\n"), '\r' => buf.push_str("\\r"), '\t' => buf.push_str("\\t"),
            c if c < '\x20' => buf.push_str(&format!("\\u{:04x}", c as u32)),
            c => buf.push(c),
        }
    }
    buf.push('"');
}

fn err(msg: &str) -> MorlocError { MorlocError::Serialization(msg.into()) }

fn is_null(text: &str) -> bool { text.trim() == "null" }

fn truncate_for_msg(s: &str) -> String {
    let t = s.trim();
    if t.len() <= 80 { return t.to_string(); }
    let mut end = 77;
    while end > 0 && !t.is_char_boundary(end) { end -= 1; }
    format!("{}...", &t[..end])
}

/// Strip optional surrounding double-quotes (JSON string form) and reject
/// any non-integer sigils. Returns the digit body for `decimal_to_limbs`.
fn extract_bigint_digits(text: &str) -> Result<&str, MorlocError> {
    let t = text.trim();
    let body = if t.len() >= 2 && t.starts_with('"') && t.ends_with('"') {
        &t[1..t.len() - 1]
    } else {
        t
    };
    let body = body.trim();
    if body.bytes().any(|b| b == b'.' || b == b'e' || b == b'E') {
        return Err(MorlocError::Serialization(format!(
            "expected integer for Int, got {}", truncate_for_msg(t)
        )));
    }
    Ok(body)
}

/// Parse a fixed-width signed integer leaf. Operates directly on the raw
/// JSON text (preserves precision for diagnostics), rejects float syntax,
/// and reports out-of-range values with the original magnitude verbatim.
fn parse_sint(text: &str, lo: i64, hi: i64, name: &str) -> Result<i64, MorlocError> {
    let t = text.trim();
    if t.bytes().any(|b| b == b'.' || b == b'e' || b == b'E') {
        return Err(MorlocError::Serialization(format!(
            "expected integer for {}, got {}", name, truncate_for_msg(t)
        )));
    }
    match i64::from_str(t) {
        Ok(v) if v >= lo && v <= hi => Ok(v),
        Ok(v) => Err(MorlocError::Serialization(format!(
            "value {} out of range for {} (range {} to {})", v, name, lo, hi
        ))),
        Err(_) => {
            // i64::from_str failed: the value is either malformed or exceeds
            // i64. If the body is a valid decimal-digit run (with optional
            // leading '-'), it must be out of range; otherwise it's invalid.
            let body = t.strip_prefix('-').unwrap_or(t);
            if !body.is_empty() && body.bytes().all(|b| b.is_ascii_digit()) {
                Err(MorlocError::Serialization(format!(
                    "value {} out of range for {} (range {} to {})", t, name, lo, hi
                )))
            } else {
                Err(MorlocError::Serialization(format!(
                    "invalid integer for {}: {}", name, truncate_for_msg(t)
                )))
            }
        }
    }
}

fn parse_uint(text: &str, hi: u64, name: &str) -> Result<u64, MorlocError> {
    let t = text.trim();
    if t.bytes().any(|b| b == b'.' || b == b'e' || b == b'E') {
        return Err(MorlocError::Serialization(format!(
            "expected unsigned integer for {}, got {}", name, truncate_for_msg(t)
        )));
    }
    if let Some(rest) = t.strip_prefix('-') {
        // Negative is necessarily out of range for unsigned; report so.
        if !rest.is_empty() && rest.bytes().all(|b| b.is_ascii_digit()) {
            return Err(MorlocError::Serialization(format!(
                "value {} out of range for {} (range 0 to {})", t, name, hi
            )));
        }
        return Err(MorlocError::Serialization(format!(
            "invalid unsigned integer for {}: {}", name, truncate_for_msg(t)
        )));
    }
    match u64::from_str(t) {
        Ok(v) if v <= hi => Ok(v),
        Ok(v) => Err(MorlocError::Serialization(format!(
            "value {} out of range for {} (range 0 to {})", v, name, hi
        ))),
        Err(_) => {
            if !t.is_empty() && t.bytes().all(|b| b.is_ascii_digit()) {
                Err(MorlocError::Serialization(format!(
                    "value {} out of range for {} (range 0 to {})", t, name, hi
                )))
            } else {
                Err(MorlocError::Serialization(format!(
                    "invalid unsigned integer for {}: {}", name, truncate_for_msg(t)
                )))
            }
        }
    }
}

// Non-finite IEEE-754 values cannot appear as numeric literals in RFC 8259
// JSON. We follow the spec's recommended workaround: represent them as
// strings. The emit form is compact lowercase ("nan", "inf", "-inf"); the
// decoder accepts case-insensitive variants of every common spelling
// (nan/inf/infinity, with optional sign), as well as bareword `null` for
// best-effort recovery of payloads written by older morloc runtimes that
// emitted `null` for non-finites.
fn parse_float(text: &str, name: &str) -> Result<f64, MorlocError> {
    let t = text.trim();
    // Strip surrounding double-quotes if the value arrived as a JSON string
    // (the encoder emits "nan"/"inf"/"-inf"). RawValue text for a JSON
    // string includes the quotes verbatim.
    let unquoted = if t.len() >= 2 && t.starts_with('"') && t.ends_with('"') {
        &t[1..t.len() - 1]
    } else {
        t
    };
    if let Some(v) = parse_nonfinite(unquoted) {
        return Ok(v);
    }
    f64::from_str(unquoted).map_err(|_| MorlocError::Serialization(format!(
        "expected {}, got {}", name, truncate_for_msg(t)
    )))
}

// Recognise non-finite literal forms case-insensitively. Returns None if the
// input is not a recognised non-finite spelling. Accepts: nan, +nan, -nan,
// inf, +inf, -inf, infinity, +infinity, -infinity, and bareword null
// (legacy recovery -> NaN).
fn parse_nonfinite(s: &str) -> Option<f64> {
    let lower = s.to_ascii_lowercase();
    match lower.as_str() {
        "nan" | "+nan" | "-nan" => Some(f64::NAN),
        "inf" | "+inf" | "infinity" | "+infinity" => Some(f64::INFINITY),
        "-inf" | "-infinity" => Some(f64::NEG_INFINITY),
        "null" => Some(f64::NAN),
        _ => None,
    }
}

fn write_float(buf: &mut String, f: f64, fmt: &[u8]) {
    if f.is_nan() {
        buf.push_str("\"nan\"");
        return;
    }
    if f.is_infinite() {
        buf.push_str(if f > 0.0 { "\"inf\"" } else { "\"-inf\"" });
        return;
    }
    let mut cbuf = [0u8; 64];
    // SAFETY: snprintf writes to stack-local buffer with explicit size limit
    let n = unsafe { libc::snprintf(cbuf.as_mut_ptr() as *mut libc::c_char, cbuf.len(), fmt.as_ptr() as *const libc::c_char, f) };
    if n > 0 && (n as usize) < cbuf.len() {
        buf.push_str(std::str::from_utf8(&cbuf[..n as usize]).unwrap_or("0"));
    } else {
        buf.push_str("0");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::schema::parse_schema;
    fn setup() { crate::init_test_shm(); }

    #[test] fn test_int()     { setup(); let s = parse_schema("i4").unwrap(); let p = read_json_with_schema("42", &s).unwrap(); assert_eq!(voidstar_to_json_string(p, &s).unwrap(), "42"); }
    #[test] fn test_string()  { setup(); let s = parse_schema("s").unwrap(); let p = read_json_with_schema("\"hello\"", &s).unwrap(); assert_eq!(voidstar_to_json_string(p, &s).unwrap(), "\"hello\""); }
    #[test] fn test_bool()    { setup(); let s = parse_schema("b").unwrap(); let p = read_json_with_schema("true", &s).unwrap(); assert_eq!(voidstar_to_json_string(p, &s).unwrap(), "true"); }
    #[test] fn test_array()   { setup(); let s = parse_schema("ai4").unwrap(); let p = read_json_with_schema("[1,2,3]", &s).unwrap(); assert_eq!(voidstar_to_json_string(p, &s).unwrap(), "[1,2,3]"); }
    #[test] fn test_opt_some(){ setup(); let s = parse_schema("?i4").unwrap(); let p = read_json_with_schema("5", &s).unwrap(); assert_eq!(voidstar_to_json_string(p, &s).unwrap(), "5"); }
    #[test] fn test_opt_null(){ setup(); let s = parse_schema("?i4").unwrap(); let p = read_json_with_schema("null", &s).unwrap(); assert_eq!(voidstar_to_json_string(p, &s).unwrap(), "null"); }
}
