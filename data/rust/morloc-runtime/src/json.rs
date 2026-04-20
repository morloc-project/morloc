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
    let value: serde_json::Value = serde_json::from_str(json_str)
        .map_err(|e| MorlocError::Serialization(format!("JSON parse error: {}", e)))?;
    json_to_voidstar(&value, schema, dest)
}

fn alloc(dest: Option<AbsPtr>, size: usize) -> Result<ShmWriter, MorlocError> {
    let ptr = match dest { Some(p) => p, None => shm::shmalloc(size)? };
    // SAFETY: ptr from shmalloc or caller-provided valid SHM of sufficient size
    Ok(unsafe { ShmWriter::new(ptr, size) })
}

fn json_to_voidstar(
    value: &serde_json::Value, schema: &Schema, dest: Option<AbsPtr>,
) -> Result<AbsPtr, MorlocError> {
    match schema.serial_type {
        SerialType::Nil => { let w = alloc(dest, 1)?; w.write_val::<u8>(0, 0); Ok(w.as_ptr()) }
        SerialType::Bool => {
            let b = value.as_bool().ok_or_else(|| err("expected bool"))?;
            let w = alloc(dest, 1)?; w.write_val::<u8>(0, b as u8); Ok(w.as_ptr())
        }
        SerialType::Sint8  => { let w = alloc(dest, 1)?; w.write_val::<i8>(0,  as_i64(value)? as i8);  Ok(w.as_ptr()) }
        SerialType::Sint16 => { let w = alloc(dest, 2)?; w.write_val::<i16>(0, as_i64(value)? as i16); Ok(w.as_ptr()) }
        SerialType::Sint32 => { let w = alloc(dest, 4)?; w.write_val::<i32>(0, as_i64(value)? as i32); Ok(w.as_ptr()) }
        SerialType::Sint64 => { let w = alloc(dest, 8)?; w.write_val::<i64>(0, as_i64(value)?);        Ok(w.as_ptr()) }
        SerialType::Uint8  => { let w = alloc(dest, 1)?; w.write_val::<u8>(0,  as_u64(value)? as u8);  Ok(w.as_ptr()) }
        SerialType::Uint16 => { let w = alloc(dest, 2)?; w.write_val::<u16>(0, as_u64(value)? as u16); Ok(w.as_ptr()) }
        SerialType::Uint32 => { let w = alloc(dest, 4)?; w.write_val::<u32>(0, as_u64(value)? as u32); Ok(w.as_ptr()) }
        SerialType::Uint64 => { let w = alloc(dest, 8)?; w.write_val::<u64>(0, as_u64(value)?);        Ok(w.as_ptr()) }
        SerialType::Float32 => { let w = alloc(dest, 4)?; w.write_val::<f32>(0, as_f64(value)? as f32); Ok(w.as_ptr()) }
        SerialType::Float64 => { let w = alloc(dest, 8)?; w.write_val::<f64>(0, as_f64(value)?);        Ok(w.as_ptr()) }

        SerialType::String => {
            let s = value.as_str().ok_or_else(|| err("expected string"))?;
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
            let arr_val = value.as_array().ok_or_else(|| err("expected array"))?;
            let es = schema.parameters.first().ok_or_else(|| err("array has no element type"))?;
            let n = arr_val.len();
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

            for (i, elem) in arr_val.iter().enumerate() {
                // SAFETY: data_ptr + i * ew is within the data allocation
                let ep = unsafe { data_ptr.add(i * ew) };
                json_to_voidstar(elem, es, Some(ep))?;
            }
            hw.write_array_header(0, n, data_rel);
            Ok(hw.as_ptr())
        }

        SerialType::Tuple | SerialType::Map => {
            let fields = extract_fields(value, schema)?;
            if fields.len() != schema.parameters.len() {
                return Err(err(&format!("expected {} fields, got {}", schema.parameters.len(), fields.len())));
            }
            let w = alloc(dest, schema.width)?;
            w.zero(0, schema.width);
            for (i, (fv, fs)) in fields.iter().zip(schema.parameters.iter()).enumerate() {
                let sub = w.sub(schema.offsets[i], fs.width);
                json_to_voidstar(fv, fs, Some(sub.as_ptr()))?;
            }
            Ok(w.as_ptr())
        }

        SerialType::Optional => {
            let inner = schema.parameters.first().ok_or_else(|| err("optional has no inner type"))?;
            let off = shm::align_up(1, inner.alignment().max(1));
            let total = off + inner.width;
            let w = alloc(dest, total)?;
            if value.is_null() {
                w.zero(0, total);
            } else {
                w.write_val::<u8>(0, 1);
                json_to_voidstar(value, inner, Some(w.sub(off, inner.width).as_ptr()))?;
            }
            Ok(w.as_ptr())
        }

        SerialType::Tensor => Err(err("Tensor JSON parsing not yet implemented")),
    }
}

fn extract_fields(value: &serde_json::Value, schema: &Schema) -> Result<Vec<serde_json::Value>, MorlocError> {
    if schema.serial_type == SerialType::Map && value.is_object() {
        let obj = value.as_object().unwrap();
        Ok(schema.keys.iter().map(|k| obj.get(k).cloned().unwrap_or(serde_json::Value::Null)).collect())
    } else {
        value.as_array().ok_or_else(|| err("expected array for tuple/map")).cloned()
    }
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
    let v: serde_json::Value = serde_json::from_str(&json).map_err(|e| err(&e.to_string()))?;
    match &v {
        // Print strings as raw text (unescaped, no quotes)
        serde_json::Value::String(s) => println!("{}", s),
        // Print numbers and bools as plain values
        serde_json::Value::Number(n) => println!("{}", n),
        serde_json::Value::Bool(b) => println!("{}", b),
        serde_json::Value::Null => println!("null"),
        // Print arrays and objects as indented JSON
        _ => println!("{}", serde_json::to_string_pretty(&v).map_err(|e| err(&e.to_string()))?),
    }
    Ok(())
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
        SerialType::Tensor => {
            // SAFETY: reading Tensor struct from SHM
            let tensor = unsafe { &*(r.ptr as *const shm::Tensor) };
            if tensor.total_elements == 0 {
                buf.push_str("[]");
            } else {
                let ndim = schema.offsets.first().copied().unwrap_or(1);
                let sp = shm::rel2abs(tensor.shape)?;
                // SAFETY: sp points to ndim i64 values in SHM
                let shape: Vec<usize> = (0..ndim).map(|i| unsafe { *((sp as *const i64).add(i)) } as usize).collect();
                let dp = shm::rel2abs(tensor.data)?;
                let es = &schema.parameters[0];
                tensor_to_json(buf, dp, &shape, tensor.total_elements, es)?;
            }
        }
    }
    Ok(())
}

fn tensor_to_json(
    buf: &mut String, data: *const u8, shape: &[usize], stride: usize, es: &Schema,
) -> Result<(), MorlocError> {
    buf.push('[');
    if shape.len() == 1 {
        for i in 0..shape[0] {
            if i > 0 { buf.push(','); }
            // SAFETY: data + i * es.width within tensor data
            let r = unsafe { ShmReader::new(data.add(i * es.width)) };
            to_json(&r, es, buf)?;
        }
    } else {
        let inner = stride / shape[0];
        for i in 0..shape[0] {
            if i > 0 { buf.push(','); }
            tensor_to_json(buf, data.wrapping_add(i * inner * es.width), &shape[1..], inner, es)?;
        }
    }
    buf.push(']');
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
fn as_i64(v: &serde_json::Value) -> Result<i64, MorlocError> { v.as_i64().ok_or_else(|| err("expected integer")) }
fn as_u64(v: &serde_json::Value) -> Result<u64, MorlocError> { v.as_u64().ok_or_else(|| err("expected unsigned integer")) }
fn as_f64(v: &serde_json::Value) -> Result<f64, MorlocError> { v.as_f64().ok_or_else(|| err("expected number")) }

fn write_float(buf: &mut String, f: f64, fmt: &[u8]) {
    if f.is_nan() || f.is_infinite() { buf.push_str("null"); return; }
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
