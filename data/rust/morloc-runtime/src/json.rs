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
use crate::recur::{self, RecurEnv};
use crate::schema::{Schema, SerialType};
use crate::shm::{self, AbsPtr, Array, RelPtr, RELNULL};
use serde_json::value::RawValue;
use std::collections::HashMap;
use std::io::{self, Write};
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

    fn as_ptr(&self) -> *const u8 { self.ptr }
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
    let mut env: RecurEnv = Vec::new();
    json_to_voidstar(&rv, schema, dest, &mut env)
}

/// Partial-record loader for the CLI dispatcher.
///
/// Given a JSON record source (object or array form), produce a vector
/// aligned to `schema.parameters` where each slot is either a freshly
/// allocated SHM voidstar holding that field's value, or `None` if the
/// source omitted the field. The schema must be a `Map`.
///
/// Object form may be partial: any key in the source must match a
/// field; missing keys yield `None`. Unknown keys raise an error so
/// typos surface clearly.
///
/// Array form must be complete: positional encoding has no notion of
/// "missing" and a shorter array is ambiguous between "drop the last
/// field" and "drop a middle one". Length mismatch is an error.
///
/// Callers own the returned `AbsPtr`s and must arrange for them to be
/// either consumed into a larger record or released. The CLI
/// dispatcher (`parse_cli_data_argument_unrolled`) handles both.
pub fn load_record_fields_from_json(
    json_str: &str,
    schema: &Schema,
) -> Result<Vec<Option<AbsPtr>>, MorlocError> {
    if !matches!(schema.serial_type, SerialType::Map) {
        return Err(err("load_record_fields_from_json requires a record (Map) schema"));
    }
    let rv: Box<RawValue> = serde_json::from_str(json_str)
        .map_err(|e| MorlocError::Serialization(format!("JSON parse error: {}", e)))?;
    let text = rv.get();
    let t = text.trim_start();
    let n = schema.parameters.len();
    let mut out: Vec<Option<AbsPtr>> = Vec::with_capacity(n);

    if t.starts_with('{') {
        let obj = parse_object_children(text)?;
        // Detect unknown keys: cheaper as a set lookup than nested loops.
        let known: std::collections::HashSet<&str> =
            schema.keys.iter().map(|s| s.as_str()).collect();
        for k in obj.keys() {
            if !known.contains(k.as_str()) {
                return Err(err(&format!("unknown field '{}' in record bundle", k)));
            }
        }
        let mut env: RecurEnv = Vec::new();
        for (key, fs) in schema.keys.iter().zip(schema.parameters.iter()) {
            match obj.get(key) {
                Some(child) => {
                    let abs = shm::shmalloc(fs.width)?;
                    // SAFETY: abs is freshly allocated with fs.width bytes.
                    unsafe { std::ptr::write_bytes(abs, 0, fs.width) };
                    json_to_voidstar(child, fs, Some(abs), &mut env)?;
                    out.push(Some(abs));
                }
                None => out.push(None),
            }
        }
    } else if t.starts_with('[') {
        let children = parse_array_children(text)?;
        if children.len() != n {
            return Err(err(&format!(
                "record array form must have exactly {} fields (one per schema field, in declaration order), got {}",
                n,
                children.len()
            )));
        }
        let mut env: RecurEnv = Vec::new();
        for (child, fs) in children.iter().zip(schema.parameters.iter()) {
            let abs = shm::shmalloc(fs.width)?;
            // SAFETY: abs is freshly allocated with fs.width bytes.
            unsafe { std::ptr::write_bytes(abs, 0, fs.width) };
            json_to_voidstar(child, fs, Some(abs), &mut env)?;
            out.push(Some(abs));
        }
    } else {
        return Err(err(&format!(
            "record source must be a JSON object {{...}} or array [...], got {}",
            truncate_for_msg(t)
        )));
    }
    Ok(out)
}

fn alloc(dest: Option<AbsPtr>, size: usize) -> Result<ShmWriter, MorlocError> {
    let ptr = match dest { Some(p) => p, None => shm::shmalloc(size)? };
    // SAFETY: ptr from shmalloc or caller-provided valid SHM of sufficient size
    Ok(unsafe { ShmWriter::new(ptr, size) })
}

fn json_to_voidstar(
    rv: &RawValue, schema: &Schema, dest: Option<AbsPtr>, env: &mut RecurEnv,
) -> Result<AbsPtr, MorlocError> {
    recur::with_scope(env, schema, |env| json_to_voidstar_inner(rv, schema, dest, env))
}

fn json_to_voidstar_inner(
    rv: &RawValue, schema: &Schema, dest: Option<AbsPtr>, env: &mut RecurEnv,
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

        SerialType::IFile | SerialType::OStream | SerialType::IStream => {
            // JSON value is the file path. Lay it down as a tagged
            // stream-handle field (TAG_PATH) so the codec primitives can
            // pick it up uniformly.
            use morloc_runtime_types::stream_handle as sh;
            let s: String = serde_json::from_str(text)
                .map_err(|e| err(&format!("expected string: {}", e)))?;
            let bytes = s.as_bytes();

            let (w, payload) = if dest.is_some() {
                let w = alloc(dest, sh::STREAM_HANDLE_FIELD_SIZE)?;
                let payload = if bytes.is_empty() {
                    RELNULL as u64
                } else {
                    let block = shm::shmalloc(sh::path_suballoc_size(bytes.len()))?;
                    unsafe { sh::write_path_suballoc(block, bytes); }
                    shm::abs2rel(block)? as u64
                };
                (w, payload)
            } else {
                let suballoc = sh::path_suballoc_size(bytes.len());
                let w = alloc(None, sh::STREAM_HANDLE_FIELD_SIZE + suballoc)?;
                let payload = if bytes.is_empty() {
                    RELNULL as u64
                } else {
                    let body_ptr = unsafe {
                        w.as_ptr().add(sh::STREAM_HANDLE_FIELD_SIZE)
                    };
                    unsafe { sh::write_path_suballoc(body_ptr, bytes); }
                    shm::abs2rel(body_ptr)? as u64
                };
                (w, payload)
            };
            unsafe { sh::write_field(w.as_ptr(), sh::TAG_PATH, payload); }
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
                json_to_voidstar(child, es, Some(ep), env)?;
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
                json_to_voidstar(child, fs, Some(sub.as_ptr()), env)?;
            }
            Ok(w.as_ptr())
        }

        SerialType::Map => {
            // Records on the JSON wire accept two shapes:
            //
            //  - Object form `{"key": val, ...}`. The canonical
            //    user-facing shape. Every required field must be
            //    present; missing keys for a non-Optional field
            //    raise an error here. The partial-load entry point
            //    [`load_record_fields_from_json`] is the path for
            //    callers (the unrolled CLI dispatcher) that want to
            //    accept partial bundles and fill missing fields
            //    from elsewhere.
            //
            //  - Array form `[v0, v1, ...]`. Schema-agnostic;
            //    preserves the property that any tuple-shaped wire
            //    payload becomes a record when given the matching
            //    schema. Length must equal `schema.parameters.len()`
            //    exactly -- shorter or longer is an error because
            //    positional encoding has no notion of "missing key".
            let t = text.trim_start();
            let w = alloc(dest, schema.width)?;
            w.zero(0, schema.width);
            if t.starts_with('{') {
                let obj = parse_object_children(text)?;
                for (i, (key, fs)) in schema.keys.iter().zip(schema.parameters.iter()).enumerate() {
                    let sub = w.sub(schema.offsets[i], fs.width);
                    match obj.get(key) {
                        Some(child) => { json_to_voidstar(child, fs, Some(sub.as_ptr()), env)?; }
                        None => {
                            return Err(err(&format!(
                                "missing required field '{}' in record",
                                key
                            )));
                        }
                    }
                }
            } else if t.starts_with('[') {
                let children = parse_array_children(text)?;
                if children.len() != schema.parameters.len() {
                    return Err(err(&format!(
                        "record array form must have exactly {} fields (one per schema field, in declaration order), got {}",
                        schema.parameters.len(),
                        children.len()
                    )));
                }
                for (i, (child, fs)) in children.iter().zip(schema.parameters.iter()).enumerate() {
                    let sub = w.sub(schema.offsets[i], fs.width);
                    json_to_voidstar(child, fs, Some(sub.as_ptr()), env)?;
                }
            } else {
                return Err(err(&format!(
                    "record source must be a JSON object {{...}} or array [...], got {}",
                    truncate_for_msg(t)
                )));
            }
            Ok(w.as_ptr())
        }

        SerialType::Optional => {
            // The Optional slot is a single relptr. Absent → write RELNULL.
            // Present → allocate the inner T separately and store its
            // relptr in the slot. Same pattern as Array (which has long
            // stored its data buffer behind a relptr).
            let inner = schema.parameters.first().ok_or_else(|| err("optional has no inner type"))?;
            let slot_size = std::mem::size_of::<RelPtr>();
            let w = alloc(dest, slot_size)?;
            if is_null(text) {
                w.write_val::<RelPtr>(0, RELNULL);
            } else {
                let inner_abs = shm::shmalloc(inner.width)?;
                // SAFETY: inner_abs is freshly allocated with inner.width bytes.
                unsafe { std::ptr::write_bytes(inner_abs, 0, inner.width) };
                json_to_voidstar(rv, inner, Some(inner_abs), env)?;
                w.write_val::<RelPtr>(0, shm::abs2rel(inner_abs)?);
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
        SerialType::Recur => {
            // Back-reference: resolve to the schema declared at an
            // ancestor &Name binding site and continue the load under
            // that schema with the same JSON value. Mirrors mpack's
            // Recur arm; identical safety story (see recur.rs note).
            let name = schema.name.as_deref().unwrap_or("");
            let target_ptr = recur::lookup(env, name)?;
            // SAFETY: see the to_json Recur arm above.
            let target = unsafe { &*target_ptr };
            json_to_voidstar(rv, target, dest, env)
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

/// 64 KiB matches the default Linux pipe buffer; sized so writes fill a
/// downstream `read` in one syscall.
const BUFWRITER_CAPACITY: usize = 64 * 1024;

/// `None` = flat, `Some(depth)` = pretty at that indent depth. Threaded
/// through the walker so containers know whether to emit newlines +
/// indentation; scalars ignore it.
type Pretty = Option<usize>;

/// Streaming JSON writer. Emits directly into `w` -- peak memory is the
/// caller's buffer (typically 64 KiB), not the size of the output.
pub fn write_json(ptr: AbsPtr, schema: &Schema, w: &mut dyn Write) -> Result<(), MorlocError> {
    let r = unsafe { ShmReader::new(ptr) };
    let mut env: RecurEnv = Vec::new();
    to_json(&r, schema, w, &mut env, None)
}

/// Buffered variant for callers that need an owned String (daemon
/// discovery JSON, cli logging, intrinsics, tests). The walker only ever
/// emits valid UTF-8, so from_utf8 is defensive.
pub fn voidstar_to_json_string(ptr: AbsPtr, schema: &Schema) -> Result<String, MorlocError> {
    let mut buf: Vec<u8> = Vec::new();
    write_json(ptr, schema, &mut buf)?;
    String::from_utf8(buf).map_err(|e| err(&format!("json utf8: {}", e)))
}

/// True when the top-level wire value is "null-ish": either Unit (Nil) or
/// an Optional whose relptr is RELNULL. Nested null inside a container
/// is not detected -- that would lose structural information.
pub fn is_top_null(ptr: AbsPtr, schema: &Schema) -> bool {
    match schema.serial_type {
        SerialType::Nil => true,
        SerialType::Optional => {
            let r = unsafe { ShmReader::new(ptr) };
            r.read_val::<RelPtr>(0) == RELNULL
        }
        _ => false,
    }
}

pub fn print_voidstar(ptr: AbsPtr, schema: &Schema, keep_null: bool) -> Result<(), MorlocError> {
    write_to_stdout(ptr, schema, keep_null, None)
}

pub fn pretty_print_voidstar(ptr: AbsPtr, schema: &Schema, keep_null: bool) -> Result<(), MorlocError> {
    // Top-level String renders as the unescaped body (terminal convenience
    // for `--print`). Other single-scalar returns fall through to the
    // streaming walker.
    if let SerialType::String = schema.serial_type {
        if !keep_null && is_top_null(ptr, schema) { return Ok(()); }
        let mut w = io::BufWriter::with_capacity(BUFWRITER_CAPACITY, io::stdout().lock());
        let r = unsafe { ShmReader::new(ptr) };
        let arr = r.read_array(0);
        if arr.size > 0 && arr.data != RELNULL {
            let dr = unsafe { ShmReader::new(shm::rel2abs(arr.data)?) };
            map_io(w.write_all(dr.read_str(0, arr.size).as_bytes()))?;
        }
        map_io(w.write_all(b"\n"))?;
        return map_io(w.flush());
    }
    write_to_stdout(ptr, schema, keep_null, Some(0))
}

fn write_to_stdout(ptr: AbsPtr, schema: &Schema, keep_null: bool, pretty: Pretty)
    -> Result<(), MorlocError>
{
    if !keep_null && is_top_null(ptr, schema) { return Ok(()); }
    let mut w = io::BufWriter::with_capacity(BUFWRITER_CAPACITY, io::stdout().lock());
    let r = unsafe { ShmReader::new(ptr) };
    let mut env: RecurEnv = Vec::new();
    to_json(&r, schema, &mut w, &mut env, pretty)?;
    map_io(w.write_all(b"\n"))?;
    map_io(w.flush())
}

/// Distinguishes "downstream closed the pipe" (fast-exit with 141 at
/// the FFI boundary) from a genuine serialization error. Other io kinds
/// are downgraded to Serialization since the walker has no way to
/// communicate transient io problems.
#[inline]
fn map_io<T>(r: io::Result<T>) -> Result<T, MorlocError> {
    match r {
        Ok(v) => Ok(v),
        Err(e) if e.kind() == io::ErrorKind::BrokenPipe => Err(MorlocError::PipeClosed),
        Err(e) => Err(MorlocError::Serialization(e.to_string())),
    }
}

fn to_json(
    r: &ShmReader,
    schema: &Schema,
    w: &mut dyn Write,
    env: &mut RecurEnv,
    pretty: Pretty,
) -> Result<(), MorlocError> {
    recur::with_scope(env, schema, |env| to_json_inner(r, schema, w, env, pretty))
}

fn to_json_inner(
    r: &ShmReader,
    schema: &Schema,
    w: &mut dyn Write,
    env: &mut RecurEnv,
    pretty: Pretty,
) -> Result<(), MorlocError> {
    match schema.serial_type {
        SerialType::Nil    => map_io(w.write_all(b"null"))?,
        SerialType::Bool   => map_io(w.write_all(
            if r.read_u8(0) != 0 { b"true" } else { b"false" }
        ))?,
        SerialType::Sint8  => map_io(write!(w, "{}", r.read_val::<i8>(0)))?,
        SerialType::Sint16 => map_io(write!(w, "{}", r.read_val::<i16>(0)))?,
        SerialType::Sint32 => map_io(write!(w, "{}", r.read_val::<i32>(0)))?,
        SerialType::Sint64 => map_io(write!(w, "{}", r.read_val::<i64>(0)))?,
        SerialType::Uint8  => map_io(write!(w, "{}", r.read_u8(0)))?,
        SerialType::Uint16 => map_io(write!(w, "{}", r.read_val::<u16>(0)))?,
        SerialType::Uint32 => map_io(write!(w, "{}", r.read_val::<u32>(0)))?,
        SerialType::Uint64 => map_io(write!(w, "{}", r.read_val::<u64>(0)))?,
        SerialType::Float32 => write_float(w, r.read_val::<f32>(0) as f64, b"%.7g\0")?,
        SerialType::Float64 => write_float(w, r.read_val::<f64>(0), b"%.15g\0")?,

        SerialType::Int => {
            // Inline layout: [size:i64, value_or_relptr:i64]
            let size = r.read_val::<usize>(0);
            if size == 0 {
                map_io(w.write_all(b"0"))?;
            } else if size == 1 {
                map_io(write!(w, "{}", r.read_val::<i64>(8)))?;
            } else {
                // Overflow: relptr to limb array. Per-value bounded alloc
                // (~19 chars per limb), not O(output).
                let relptr = r.read_val::<shm::RelPtr>(8);
                let data_ptr = shm::rel2abs(relptr)?;
                let limbs: Vec<u64> = (0..size)
                    .map(|i| unsafe { *((data_ptr as *const u64).add(i)) })
                    .collect();
                let s = crate::eval_ffi::limbs_to_decimal(&limbs);
                map_io(w.write_all(s.as_bytes()))?;
            }
        }

        SerialType::String => {
            let arr = r.read_array(0);
            if arr.size == 0 || arr.data == RELNULL {
                map_io(w.write_all(b"\"\""))?;
            } else {
                let dr = unsafe { ShmReader::new(shm::rel2abs(arr.data)?) };
                json_escape(dr.read_str(0, arr.size), w)?;
            }
        }
        SerialType::IFile | SerialType::OStream | SerialType::IStream => {
            // TAG_PATH renders the file path straight into JSON; TAG_HANDLE
            // looks up the path via the local SHM registry so the on-disk
            // JSON always carries a path string.
            use morloc_runtime_types::stream_handle as sh;
            let field_ptr = r.as_ptr();
            let tag = unsafe { sh::read_tag(field_ptr) };
            let payload = unsafe { sh::read_payload(field_ptr) };
            if tag == sh::TAG_PATH {
                if payload == RELNULL as u64 {
                    map_io(w.write_all(b"\"\""))?;
                } else {
                    let suballoc = shm::rel2abs(payload as shm::RelPtr)?;
                    let path_len = unsafe { sh::read_path_size(suballoc) } as usize;
                    if path_len == 0 {
                        map_io(w.write_all(b"\"\""))?;
                    } else {
                        let bytes = unsafe {
                            std::slice::from_raw_parts(suballoc.add(8), path_len)
                        };
                        let s = std::str::from_utf8(bytes).map_err(|_| {
                            MorlocError::Serialization(
                                "json stream-handle: path is not valid UTF-8".into(),
                            )
                        })?;
                        json_escape(s, w)?;
                    }
                }
            } else if tag == sh::TAG_HANDLE {
                let path = crate::stream::handle_path(payload as i64)?;
                json_escape(&path, w)?;
            } else {
                return Err(MorlocError::Serialization(format!(
                    "json stream-handle: unsupported tag {}", tag,
                )));
            }
        }
        SerialType::Array => {
            let arr = r.read_array(0);
            let es = &schema.parameters[0];
            let empty = arr.size == 0 || arr.data == RELNULL;
            write_container(w, b'[', b']', pretty, empty, |w, child_pretty| {
                let data = shm::rel2abs(arr.data)?;
                for i in 0..arr.size {
                    write_sep(w, i, pretty)?;
                    let er = unsafe { ShmReader::new(data.add(i * es.width)) };
                    to_json(&er, es, w, env, child_pretty)?;
                }
                Ok(())
            })?;
        }
        SerialType::Tuple => {
            let n = schema.parameters.len();
            write_container(w, b'[', b']', pretty, n == 0, |w, child_pretty| {
                for (i, fs) in schema.parameters.iter().enumerate() {
                    write_sep(w, i, pretty)?;
                    to_json(&r.at(schema.offsets[i]), fs, w, env, child_pretty)?;
                }
                Ok(())
            })?;
        }
        SerialType::Map => {
            let n = schema.parameters.len();
            write_container(w, b'{', b'}', pretty, n == 0, |w, child_pretty| {
                for (i, fs) in schema.parameters.iter().enumerate() {
                    write_sep(w, i, pretty)?;
                    if i < schema.keys.len() {
                        // Keys are morloc identifiers -- ASCII, no escaping needed.
                        map_io(w.write_all(b"\""))?;
                        map_io(w.write_all(schema.keys[i].as_bytes()))?;
                        map_io(w.write_all(if pretty.is_some() { b"\": " } else { b"\":" }))?;
                    }
                    to_json(&r.at(schema.offsets[i]), fs, w, env, child_pretty)?;
                }
                Ok(())
            })?;
        }
        SerialType::Optional => {
            let relptr: RelPtr = r.read_val(0);
            if relptr == RELNULL {
                map_io(w.write_all(b"null"))?;
            } else {
                let inner = &schema.parameters[0];
                let inner_reader = unsafe { ShmReader::new(shm::rel2abs(relptr)?) };
                to_json(&inner_reader, inner, w, env, pretty)?;
            }
        }
        SerialType::Table => {
            return Err(err("Cannot render a Table to generic JSON; use the Arrow-to-JSON path"));
        }
        SerialType::Recur => {
            // Back-reference: resolve via env stack to the ancestor &Name
            // binding, then continue with the same reader (the wire layout
            // at this position IS the resolved schema's layout). See
            // recur.rs for lifetime rationale.
            let name = schema.name.as_deref().unwrap_or("");
            let target_ptr = recur::lookup(env, name)?;
            let target = unsafe { &*target_ptr };
            to_json(r, target, w, env, pretty)?;
        }
    }
    Ok(())
}

/// Emit the container's open/close bytes + newlines/indent as required.
/// `body` writes the interior elements (children already know their
/// depth via `child_pretty`).
fn write_container<F>(
    w: &mut dyn Write,
    open: u8,
    close: u8,
    pretty: Pretty,
    empty: bool,
    body: F,
) -> Result<(), MorlocError>
where
    F: FnOnce(&mut dyn Write, Pretty) -> Result<(), MorlocError>,
{
    if empty {
        return map_io(w.write_all(&[open, close]));
    }
    match pretty {
        None => {
            map_io(w.write_all(&[open]))?;
            body(w, None)?;
            map_io(w.write_all(&[close]))
        }
        Some(depth) => {
            map_io(w.write_all(&[open, b'\n']))?;
            body(w, Some(depth + 1))?;
            map_io(w.write_all(b"\n"))?;
            write_indent(w, depth)?;
            map_io(w.write_all(&[close]))
        }
    }
}

/// Between-element separator. Flat: `,`. Pretty: `,\n` after every element
/// except the last, plus `<indent>` before every element.
fn write_sep(w: &mut dyn Write, i: usize, pretty: Pretty) -> Result<(), MorlocError> {
    match pretty {
        None => if i > 0 { map_io(w.write_all(b","))?; },
        Some(depth) => {
            if i > 0 { map_io(w.write_all(b",\n"))?; }
            write_indent(w, depth + 1)?;
        }
    }
    Ok(())
}

fn write_indent(w: &mut dyn Write, depth: usize) -> Result<(), MorlocError> {
    // Preallocated slice so a deep indent is one write, not `depth` writes.
    const SPACES: &[u8; 128] = &[b' '; 128];
    let mut remaining = depth * 2;
    while remaining > 0 {
        let n = remaining.min(SPACES.len());
        map_io(w.write_all(&SPACES[..n]))?;
        remaining -= n;
    }
    Ok(())
}

// ── Helpers ────────────────────────────────────────────────────────────────

fn json_escape(s: &str, w: &mut dyn Write) -> Result<(), MorlocError> {
    // Batch consecutive "safe" characters into a single write to keep the
    // per-scalar overhead close to the C printf version. Only escape the
    // JSON-reserved / control-range codepoints.
    map_io(w.write_all(b"\""))?;
    let bytes = s.as_bytes();
    let mut safe_start = 0usize;
    let mut i = 0usize;
    while i < bytes.len() {
        let b = bytes[i];
        let escape: Option<&[u8]> = match b {
            b'"'  => Some(b"\\\""),
            b'\\' => Some(b"\\\\"),
            b'/'  => Some(b"\\/"),
            0x08  => Some(b"\\b"),
            0x0c  => Some(b"\\f"),
            b'\n' => Some(b"\\n"),
            b'\r' => Some(b"\\r"),
            b'\t' => Some(b"\\t"),
            c if c < 0x20 => None, // handled below with formatted \u
            _ => { i += 1; continue; }
        };
        if i > safe_start {
            map_io(w.write_all(&bytes[safe_start..i]))?;
        }
        match escape {
            Some(seq) => { map_io(w.write_all(seq))?; }
            None => { map_io(write!(w, "\\u{:04x}", b as u32))?; }
        }
        i += 1;
        safe_start = i;
    }
    if safe_start < bytes.len() {
        map_io(w.write_all(&bytes[safe_start..]))?;
    }
    map_io(w.write_all(b"\""))
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

// Recognise the canonical non-finite wire tokens. Case-sensitive,
// matching the writer (which emits exactly `"inf"` / `"-inf"` /
// `"nan"`). Any other spelling is rejected so the caller surfaces a
// clear parse error.
fn parse_nonfinite(s: &str) -> Option<f64> {
    match s {
        "nan" => Some(f64::NAN),
        "inf" => Some(f64::INFINITY),
        "-inf" => Some(f64::NEG_INFINITY),
        _ => None,
    }
}

fn write_float(w: &mut dyn Write, f: f64, fmt: &[u8]) -> Result<(), MorlocError> {
    if f.is_nan() {
        return map_io(w.write_all(b"\"nan\""));
    }
    if f.is_infinite() {
        return map_io(w.write_all(if f > 0.0 { b"\"inf\"" } else { b"\"-inf\"" }));
    }
    let mut cbuf = [0u8; 64];
    // SAFETY: snprintf writes to stack-local buffer with explicit size limit
    let n = unsafe { libc::snprintf(cbuf.as_mut_ptr() as *mut libc::c_char, cbuf.len(), fmt.as_ptr() as *const libc::c_char, f) };
    if n > 0 && (n as usize) < cbuf.len() {
        // snprintf produces ASCII digits/sign/exponent -- no UTF-8 check needed.
        map_io(w.write_all(&cbuf[..n as usize]))
    } else {
        map_io(w.write_all(b"0"))
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

    // Record `{m :: i4, n :: i4}` -- schema encoding `m21mi41ni4`.
    fn rec2_schema() -> Schema { parse_schema("m21mi41ni4").unwrap() }

    #[test]
    fn test_record_missing_field_errors() {
        setup();
        let s = rec2_schema();
        let r = read_json_with_schema("{\"m\":1}", &s);
        let msg = r.err().expect("missing field must error").to_string();
        assert!(msg.contains("missing required field 'n'"), "got: {}", msg);
    }

    #[test]
    fn test_record_array_wrong_length_errors() {
        setup();
        let s = rec2_schema();
        let r = read_json_with_schema("[1]", &s);
        let msg = r.err().expect("short array must error").to_string();
        assert!(msg.contains("exactly 2 fields"), "got: {}", msg);
    }

    #[test]
    fn test_partial_load_full_object() {
        setup();
        let s = rec2_schema();
        let v = load_record_fields_from_json("{\"m\":7,\"n\":11}", &s).unwrap();
        assert_eq!(v.len(), 2);
        assert!(v[0].is_some());
        assert!(v[1].is_some());
    }

    #[test]
    fn test_partial_load_partial_object() {
        setup();
        let s = rec2_schema();
        let v = load_record_fields_from_json("{\"m\":7}", &s).unwrap();
        assert_eq!(v.len(), 2);
        assert!(v[0].is_some(), "present field should be Some");
        assert!(v[1].is_none(), "absent field should be None");
    }

    #[test]
    fn test_partial_load_unknown_field_errors() {
        setup();
        let s = rec2_schema();
        let r = load_record_fields_from_json("{\"m\":7,\"oops\":1}", &s);
        let msg = r.err().expect("unknown key must error").to_string();
        assert!(msg.contains("unknown field 'oops'"), "got: {}", msg);
    }

    #[test]
    fn test_partial_load_array_must_be_complete() {
        setup();
        let s = rec2_schema();
        let r = load_record_fields_from_json("[1]", &s);
        let msg = r.err().expect("short array must error").to_string();
        assert!(msg.contains("exactly 2 fields"), "got: {}", msg);

        let v = load_record_fields_from_json("[1,2]", &s).unwrap();
        assert!(v.iter().all(|x| x.is_some()));
    }

    #[test]
    fn test_partial_load_non_map_schema_errors() {
        setup();
        let s = parse_schema("i4").unwrap();
        let r = load_record_fields_from_json("42", &s);
        assert!(r.is_err());
    }

    // A Write impl that returns BrokenPipe after `n` successful bytes.
    // Used to verify the walker propagates MorlocError::PipeClosed
    // rather than a generic Io error, and that no output is buffered
    // beyond the point of failure.
    struct PipeAfter {
        remaining: usize,
        written: Vec<u8>,
    }
    impl std::io::Write for PipeAfter {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            if self.remaining == 0 {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::BrokenPipe,
                    "test: pipe closed",
                ));
            }
            let n = buf.len().min(self.remaining);
            self.written.extend_from_slice(&buf[..n]);
            self.remaining -= n;
            if self.remaining == 0 && n < buf.len() {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::BrokenPipe,
                    "test: pipe closed mid-write",
                ));
            }
            Ok(n)
        }
        fn flush(&mut self) -> std::io::Result<()> { Ok(()) }
    }

    #[test]
    fn test_write_json_streams_without_buffering() {
        setup();
        // A 100-element array: write_json should feed the sink one chunk
        // at a time. We just care that the produced bytes match the
        // buffered voidstar_to_json_string form -- if it does, the
        // streaming walker is emitting the same JSON as the batched one.
        let s = parse_schema("ai4").unwrap();
        let raw = "[".to_string() + &(0..100).map(|i| i.to_string()).collect::<Vec<_>>().join(",") + "]";
        let p = read_json_with_schema(&raw, &s).unwrap();
        let expected = voidstar_to_json_string(p, &s).unwrap();

        let mut sink: Vec<u8> = Vec::new();
        write_json(p, &s, &mut sink).unwrap();
        assert_eq!(String::from_utf8(sink).unwrap(), expected);
    }

    #[test]
    fn test_write_json_maps_broken_pipe_to_pipe_closed() {
        setup();
        // Fail on the very first write to prove the walker doesn't
        // silently swallow the error.
        let s = parse_schema("ai4").unwrap();
        let p = read_json_with_schema("[1,2,3,4,5]", &s).unwrap();
        let mut sink = PipeAfter { remaining: 0, written: Vec::new() };
        let r = write_json(p, &s, &mut sink);
        assert!(
            matches!(r, Err(MorlocError::PipeClosed)),
            "expected PipeClosed, got {:?}", r,
        );
    }

    #[test]
    fn test_write_json_pipe_closed_partway() {
        setup();
        // Accept a few bytes, then fail. The walker must still surface
        // PipeClosed (not Io / Serialization) -- upstream policy relies
        // on this distinction to pick exit code 141 vs 1.
        let s = parse_schema("ai4").unwrap();
        let p = read_json_with_schema("[100,200,300,400,500,600]", &s).unwrap();
        let mut sink = PipeAfter { remaining: 4, written: Vec::new() };
        let r = write_json(p, &s, &mut sink);
        assert!(
            matches!(r, Err(MorlocError::PipeClosed)),
            "expected PipeClosed after partial write, got {:?}", r,
        );
        // And we did emit at least the bytes the sink accepted before
        // failing -- the walker isn't buffering the whole JSON in memory
        // before writing the first byte.
        assert!(!sink.written.is_empty(), "walker held back all output before first failure");
    }
}
