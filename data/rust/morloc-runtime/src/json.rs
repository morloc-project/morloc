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
use crate::shm::{self, AbsPtr, Array, RelPtr, RELNULL};
use crate::walk::{self, Frame, Stack, Visit, Walker};
use serde_json::value::RawValue;
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

    /// Raw bytes with no UTF-8 validation -- for `Vector U8` / `[U8]` bodies,
    /// which may be arbitrary binary (a `render` handler's raw output).
    fn read_bytes(&self, offset: usize, len: usize) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.ptr.add(offset), len) }
    }



    fn as_ptr(&self) -> *const u8 { self.ptr }
}

// ── JSON -> Voidstar ───────────────────────────────────────────────────────

/// The constructors a bare (unquoted) token could name: every constructor
/// of an enum, the argument-free constructors of a variant (the others are
/// shapes, not words), looking through an optional. `None` for a type a
/// bare word can never name.
pub fn bare_ctor_names(schema: &Schema) -> Option<Vec<&str>> {
    match schema.serial_type {
        SerialType::Enum => Some(schema.keys.iter().map(|k| k.as_str()).collect()),
        SerialType::Variant => Some(
            schema
                .keys
                .iter()
                .zip(schema.parameters.iter())
                .filter(|(_, arm)| arm.size == 0)
                .map(|(k, _)| k.as_str())
                .collect(),
        ),
        SerialType::Optional => schema.parameters.first().and_then(bare_ctor_names),
        _ => None,
    }
}

/// The constructor a bare token names, matched without regard to case.
///
/// A bare token is what a person types; the constructor's spelling is the
/// author's convention, and the typist should not have to reproduce it. A
/// quoted string is machine text and never reaches here, so JSON stays
/// case-strict. Two constructors differing only in case are rejected at
/// declaration, so a fold either finds one name or none.
pub fn match_ctor_name<'a>(names: &[&'a str], token: &str) -> Option<&'a str> {
    let want = token.to_lowercase();
    names.iter().find(|k| k.to_lowercase() == want).copied()
}

/// True when a bare token names a constructor of this schema's type, which
/// is what lets the token bypass the source classifier: a bare word that
/// is a constructor is the value, not a file that failed to exist.
pub fn is_bare_ctor_token(schema: &Schema, token: &str) -> bool {
    bare_ctor_names(schema)
        .map(|names| match_ctor_name(&names, token.trim()).is_some())
        .unwrap_or(false)
}

pub fn read_json_with_schema(json_str: &str, schema: &Schema) -> Result<AbsPtr, MorlocError> {
    read_json_with_schema_dest(None, json_str, schema)
}

pub fn read_json_with_schema_dest(
    dest: Option<AbsPtr>, json_str: &str, schema: &Schema,
) -> Result<AbsPtr, MorlocError> {
    // Parse to RawValue (preserves the raw text of every leaf). Avoids
    // serde_json's f64 fallback for numbers that exceed i64/u64, which
    // silently corrupts BigInt (`Int`) values.
    let rv: Box<RawValue> = match serde_json::from_str(json_str) {
        Ok(v) => v,
        Err(e) => {
            // A bare constructor name is the natural way to spell an enum on
            // the command line (`cmd G`), but it is not valid JSON on its
            // own. Quoting it on the retry is unambiguous BECAUSE the first
            // attempt already succeeded for every JSON literal: `null`,
            // numbers and quoted strings all parse, so only a bare word
            // reaches here. That is what keeps `?DNA` able to say `null`
            // for absent while still accepting `G` for present.
            let token = json_str.trim();
            let word_shaped = !token.is_empty()
                && token.chars().all(|c| c.is_alphanumeric() || c == '_');
            if let Some(names) = bare_ctor_names(schema).filter(|_| word_shaped) {
                let name = match_ctor_name(&names, token).ok_or_else(|| {
                    MorlocError::Serialization(format!(
                        "'{}' is not a constructor of this type; expected one of {}",
                        token,
                        names.join(", ")
                    ))
                })?;
                let requoted = serde_json::to_string(name)
                    .map_err(|_| MorlocError::Serialization("JSON parse error".into()))?;
                serde_json::from_str(&requoted).map_err(|e2| {
                    MorlocError::Serialization(format!("JSON parse error: {}", e2))
                })?
            } else {
                return Err(MorlocError::Serialization(format!("JSON parse error: {}", e)));
            }
        }
    };
    let res = crate::recur::Resolver::new(schema);
    load_value(&res, rv.get(), schema, dest)
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
    let n = schema.parameters.len();
    let mut out: Vec<Option<AbsPtr>> = vec![None; n];
    // Each field is walked on its own under the record's tree, so a field
    // of a recursive record still refers back to it.
    let res = crate::recur::Resolver::new(schema);
    let counts = count_containers(text.as_bytes());
    let mut lx = Lexer::new(text.as_bytes(), counts);
    let load_field = |lx: &mut Lexer, i: usize| -> Result<AbsPtr, MorlocError> {
        let fs = &schema.parameters[i];
        let abs = shm::shmalloc(fs.width)?;
        // SAFETY: abs is freshly allocated with fs.width bytes.
        unsafe { std::ptr::write_bytes(abs, 0, fs.width) };
        let mut w = LoadWalk { res: &res, lx, seen: Vec::new(), free: Vec::new() };
        let mut st = Stack::new();
        st.enter(fs, abs, 0);
        walk::run(&mut w, &mut st)?;
        Ok(abs)
    };
    match lx.peek() {
        Some(b'{') => {
            lx.pos += 1;
            lx.next_container += 1;
            let mut first = true;
            loop {
                lx.skip_ws();
                if lx.peek() == Some(b'}') {
                    lx.pos += 1;
                    break;
                }
                if !first {
                    lx.expect(b',')?;
                }
                first = false;
                let key = lx.key()?;
                let i = schema
                    .keys
                    .iter()
                    .position(|k| *k == key)
                    .ok_or_else(|| err(&format!("unknown field '{}' in record bundle", key)))?;
                if out[i].is_some() {
                    return Err(err(&format!("field '{}' given twice in record bundle", key)));
                }
                out[i] = Some(load_field(&mut lx, i)?);
            }
        }
        Some(b'[') => {
            lx.pos += 1;
            let given = lx.counts[lx.next_container];
            lx.next_container += 1;
            if given != n {
                return Err(err(&format!(
                    "record array form must have exactly {} fields (one per schema field, in declaration order), got {}",
                    n, given
                )));
            }
            for i in 0..n {
                if i > 0 {
                    lx.expect(b',')?;
                }
                out[i] = Some(load_field(&mut lx, i)?);
            }
            lx.expect(b']')?;
        }
        _ => {
            return Err(err(&format!(
                "record source must be a JSON object {{...}} or array [...], got {}",
                truncate_for_msg(text.trim_start())
            )));
        }
    }
    Ok(out)
}

fn alloc(dest: Option<AbsPtr>, size: usize) -> Result<ShmWriter, MorlocError> {
    let ptr = match dest { Some(p) => p, None => shm::shmalloc(size)? };
    // SAFETY: ptr from shmalloc or caller-provided valid SHM of sufficient size
    Ok(unsafe { ShmWriter::new(ptr, size) })
}

/// Load `text`, a JSON value already checked to be well formed, under
/// `schema` (a node of the tree `res` indexes), into `dest` or a fresh
/// block.
fn load_value(
    res: &crate::recur::Resolver<'_>,
    text: &str,
    schema: &Schema,
    dest: Option<AbsPtr>,
) -> Result<AbsPtr, MorlocError> {
    let counts = count_containers(text.as_bytes());
    let mut lx = Lexer::new(text.as_bytes(), counts);
    let schema = res.resolve(schema)?;
    // A root leaf, and a root string or array, get one block holding the
    // slot and its data together; every nested value writes into a slot
    // its parent laid out.
    let root = match (schema.serial_type, dest) {
        (SerialType::Array, None) => {
            lx.skip_ws();
            if lx.peek() != Some(b'[') {
                return Err(err(&format!("expected JSON array, got {}", truncate_for_msg(text))));
            }
            let n = lx.counts[lx.next_container];
            let es = schema.parameters.first().ok_or_else(|| err("array has no element type"))?;
            let hdr = std::mem::size_of::<Array>();
            let w = alloc(None, hdr + n * es.width)?;
            w.zero(0, hdr + n * es.width);
            w.as_ptr()
        }
        (SerialType::Tuple | SerialType::Map | SerialType::Optional | SerialType::Variant, _) => {
            let w = alloc(dest, schema.width)?;
            w.zero(0, schema.width);
            w.as_ptr()
        }
        (SerialType::Array, Some(d)) => d,
        _ => {
            // A leaf takes the whole text.
            let span = lx.value_span()?;
            return write_leaf(span, schema, dest);
        }
    };
    let mut w = LoadWalk { res, lx: &mut lx, seen: Vec::new(), free: Vec::new() };
    let mut st = Stack::new();
    let single_block = dest.is_none() && schema.serial_type == SerialType::Array;
    st.enter(schema, root, if single_block { 1 } else { 0 });
    walk::run(&mut w, &mut st)?;
    Ok(root)
}

/// The element or member count of every `[` and `{` in `text`, in text
/// order, so a container can be sized before its children are read.
fn count_containers(text: &[u8]) -> Vec<usize> {
    let mut counts: Vec<usize> = Vec::new();
    // Index into `counts` of each open container, innermost last, with
    // whether it has seen a value yet.
    let mut open: Vec<(usize, bool)> = Vec::new();
    let mut i = 0;
    while i < text.len() {
        match text[i] {
            b'"' => {
                if let Some(top) = open.last_mut() {
                    top.1 = true;
                }
                i += 1;
                while i < text.len() && text[i] != b'"' {
                    if text[i] == b'\\' {
                        i += 1;
                    }
                    i += 1;
                }
            }
            b'[' | b'{' => {
                if let Some(top) = open.last_mut() {
                    top.1 = true;
                }
                open.push((counts.len(), false));
                counts.push(0);
            }
            b']' | b'}' => {
                if let Some((idx, seen)) = open.pop() {
                    if seen {
                        counts[idx] += 1;
                    }
                }
            }
            b',' => {
                if let Some((idx, _)) = open.last() {
                    counts[*idx] += 1;
                }
            }
            b' ' | b'\t' | b'\n' | b'\r' => {}
            _ => {
                if let Some(top) = open.last_mut() {
                    top.1 = true;
                }
            }
        }
        i += 1;
    }
    counts
}

/// A cursor over well-formed JSON text. Only token boundaries are found
/// here; leaves are decoded by the same parsers as before from the token's
/// text.
struct Lexer<'t> {
    s: &'t [u8],
    pos: usize,
    counts: Vec<usize>,
    /// The next entry of `counts`: containers are entered in text order.
    next_container: usize,
}

impl<'t> Lexer<'t> {
    fn new(s: &'t [u8], counts: Vec<usize>) -> Lexer<'t> {
        Lexer { s, pos: 0, counts, next_container: 0 }
    }

    fn skip_ws(&mut self) {
        while self.pos < self.s.len() && matches!(self.s[self.pos], b' ' | b'\t' | b'\n' | b'\r') {
            self.pos += 1;
        }
    }

    fn peek(&mut self) -> Option<u8> {
        self.skip_ws();
        self.s.get(self.pos).copied()
    }

    fn expect(&mut self, c: u8) -> Result<(), MorlocError> {
        if self.peek() == Some(c) {
            self.pos += 1;
            Ok(())
        } else {
            Err(err(&format!("expected '{}' at byte {} of the JSON value", c as char, self.pos)))
        }
    }

    /// The text of the next value: a string up to its closing quote, a
    /// number or literal up to its delimiter, or a whole container.
    fn value_span(&mut self) -> Result<&'t str, MorlocError> {
        let start = self.pos_after_ws();
        let end = self.skip_value()?;
        Ok(std::str::from_utf8(&self.s[start..end]).expect("JSON text is UTF-8"))
    }

    fn pos_after_ws(&mut self) -> usize {
        self.skip_ws();
        self.pos
    }

    /// Advance past the next value, returning where it ended. Containers
    /// passed over are accounted for in `next_container`.
    fn skip_value(&mut self) -> Result<usize, MorlocError> {
        self.skip_ws();
        let s = self.s;
        let mut i = self.pos;
        match s.get(i) {
            Some(b'"') => {
                i += 1;
                while i < s.len() && s[i] != b'"' {
                    if s[i] == b'\\' {
                        i += 1;
                    }
                    i += 1;
                }
                i += 1;
            }
            Some(b'[') | Some(b'{') => {
                let mut depth = 0usize;
                while i < s.len() {
                    match s[i] {
                        b'"' => {
                            i += 1;
                            while i < s.len() && s[i] != b'"' {
                                if s[i] == b'\\' {
                                    i += 1;
                                }
                                i += 1;
                            }
                        }
                        b'[' | b'{' => {
                            depth += 1;
                            self.next_container += 1;
                        }
                        b']' | b'}' => {
                            depth -= 1;
                            if depth == 0 {
                                i += 1;
                                break;
                            }
                        }
                        _ => {}
                    }
                    i += 1;
                }
            }
            Some(_) => {
                while i < s.len() && !matches!(s[i], b',' | b']' | b'}' | b' ' | b'\t' | b'\n' | b'\r') {
                    i += 1;
                }
            }
            None => return Err(err("unexpected end of JSON value")),
        }
        self.pos = i;
        Ok(i)
    }

    /// An object member's key, decoded, with its colon consumed.
    fn key(&mut self) -> Result<String, MorlocError> {
        if self.peek() != Some(b'"') {
            return Err(err(&format!("expected an object key at byte {} of the JSON value", self.pos)));
        }
        let span = self.value_span()?;
        let key: String = serde_json::from_str(span).map_err(|e| err(&format!("bad object key: {e}")))?;
        self.expect(b':')?;
        Ok(key)
    }

    /// Enter the container at the cursor, returning its element count.
    fn open(&mut self, c: u8) -> Result<usize, MorlocError> {
        self.expect(c)?;
        let n = self.counts[self.next_container];
        self.next_container += 1;
        Ok(n)
    }
}

/// Decode a leaf from its token text into `dest` (or a fresh block).
fn write_leaf(text: &str, schema: &Schema, dest: Option<AbsPtr>) -> Result<AbsPtr, MorlocError> {
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
        SerialType::Sint8  => { let w = alloc(dest, 1)?; w.write_val::<i8>(0,  parse_sint(text, i8::MIN  as i64, i8::MAX  as i64, "I8")?  as i8);  Ok(w.as_ptr()) }
        SerialType::Sint16 => { let w = alloc(dest, 2)?; w.write_val::<i16>(0, parse_sint(text, i16::MIN as i64, i16::MAX as i64, "I16")? as i16); Ok(w.as_ptr()) }
        SerialType::Sint32 => { let w = alloc(dest, 4)?; w.write_val::<i32>(0, parse_sint(text, i32::MIN as i64, i32::MAX as i64, "I32")? as i32); Ok(w.as_ptr()) }
        SerialType::Sint64 => { let w = alloc(dest, 8)?; w.write_val::<i64>(0, parse_sint(text, i64::MIN,        i64::MAX,        "I64")?);        Ok(w.as_ptr()) }
        SerialType::Uint8  => { let w = alloc(dest, 1)?; w.write_val::<u8>(0,  parse_uint(text, u8::MAX  as u64, "U8")?  as u8);  Ok(w.as_ptr()) }
        // JSON is the human- and LLM-facing format, so an enum reads and
        // writes as its constructor NAME. Only a declared name is accepted,
        // and a rejection names the whole legal set -- which is possible
        // precisely because the schema carries the constructor list.
        SerialType::Enum => {
            let t = text.trim();
            let name = t.strip_prefix('"').and_then(|x| x.strip_suffix('"')).ok_or_else(|| {
                err(&format!(
                    "expected one of {}, got {}",
                    schema.keys.join(", "),
                    truncate_for_msg(t)
                ))
            })?;
            let tag = schema.keys.iter().position(|k| k == name).ok_or_else(|| {
                err(&format!(
                    "'{}' is not a constructor of this type; expected one of {}",
                    name,
                    schema.keys.join(", ")
                ))
            })?;
            let w = alloc(dest, 1)?;
            w.write_val::<u8>(0, tag as u8);
            Ok(w.as_ptr())
        }
        SerialType::Uint16 => { let w = alloc(dest, 2)?; w.write_val::<u16>(0, parse_uint(text, u16::MAX as u64, "U16")? as u16); Ok(w.as_ptr()) }
        SerialType::Uint32 => { let w = alloc(dest, 4)?; w.write_val::<u32>(0, parse_uint(text, u32::MAX as u64, "U32")? as u32); Ok(w.as_ptr()) }
        SerialType::Uint64 => { let w = alloc(dest, 8)?; w.write_val::<u64>(0, parse_uint(text, u64::MAX,        "U64")?);        Ok(w.as_ptr()) }
        SerialType::Float32 => { let w = alloc(dest, 4)?; w.write_val::<f32>(0, parse_float(text, "F32")? as f32); Ok(w.as_ptr()) }
        SerialType::Float64 => { let w = alloc(dest, 8)?; w.write_val::<f64>(0, parse_float(text, "F64")?);        Ok(w.as_ptr()) }

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

        other => Err(err(&format!("{other:?} is not a leaf type"))),
    }
}

/// The JSON loader. A frame's `data` is the slot the node is written into;
/// `x` is 1 for a root array laid out as one block with its data, and a
/// record's seen-set id or array-form marker once the record is open.
/// Object members are read in the order the text gives them, and a
/// deferred child consumes its own text before the parent resumes, so the
/// cursor position is all the continuation needs.
struct LoadWalk<'a, 'r, 't> {
    res: &'a crate::recur::Resolver<'r>,
    lx: &'a mut Lexer<'t>,
    /// The fields each open object-form record has seen, one bit per
    /// field; a frame's `x` names its set. Sets are reused once a record
    /// closes.
    seen: Vec<Vec<u64>>,
    free: Vec<usize>,
}

impl<'a, 'r, 't> LoadWalk<'a, 'r, 't> {
    /// A cleared set for a record of `n` fields.
    fn seen_set(&mut self, n: usize) -> usize {
        let words = (n + 63) / 64;
        match self.free.pop() {
            Some(id) => {
                self.seen[id].clear();
                self.seen[id].resize(words, 0);
                id
            }
            None => {
                self.seen.push(vec![0u64; words]);
                self.seen.len() - 1
            }
        }
    }

    /// Read the next value into `slot` under `s`: a leaf in place, a
    /// container in place when nothing below it can recurse, else on the
    /// stack beneath the parent's continuation.
    fn child(
        &mut self,
        st: &mut Stack<u64>,
        f: &Frame<u64>,
        idx: usize,
        s: &'r Schema,
        slot: AbsPtr,
    ) -> Result<Visit, MorlocError> {
        if self.res.flat(s) {
            self.step(st, Frame::new(s, slot, 0))?;
            Ok(Visit::Done)
        } else {
            walk::defer(self, st, f, idx, s, slot, 0);
            Ok(Visit::Deferred)
        }
    }

    /// Where an array's data region lives: right after the header for a
    /// root array laid out as one block, else in the block the header
    /// points at (allocated on the first visit).
    fn array_data(&mut self, f: &Frame<u64>, n: usize, ew: usize) -> Result<*mut u8, MorlocError> {
        let hdr = std::mem::size_of::<Array>();
        let hw = unsafe { ShmWriter::new(f.data as *mut u8, hdr) };
        if f.idx == 0 {
            let dp = if f.x == 1 {
                unsafe { (f.data as *mut u8).add(hdr) }
            } else if n > 0 {
                let dp = shm::shmalloc(n * ew)?;
                // SAFETY: freshly allocated with n * ew bytes.
                unsafe { std::ptr::write_bytes(dp, 0, n * ew) };
                dp
            } else {
                std::ptr::null_mut()
            };
            let data_rel = if dp.is_null() { RELNULL } else { shm::abs2rel(dp)? };
            hw.write_array_header(0, n, data_rel);
            return Ok(dp);
        }
        let arr = unsafe { *(f.data as *const Array) };
        Ok(shm::rel2abs(arr.data)?)
    }
}

impl<'a, 'r, 't> Walker<u64> for LoadWalk<'a, 'r, 't> {
    fn step(&mut self, st: &mut Stack<u64>, f: Frame<u64>) -> Result<(), MorlocError> {
        // SAFETY: frames hold nodes of the tree the resolver indexes, and
        // `data` is a slot of the node's width laid out by the parent.
        let schema: &'r Schema = self.res.resolve(unsafe { &*f.schema })?;
        let slot = f.data as *mut u8;
        match schema.serial_type {
            SerialType::Array => {
                let es = schema.parameters.first().ok_or_else(|| err("array has no element type"))?;
                let ew = es.width;
                let n = if f.idx == 0 {
                    if self.lx.peek() != Some(b'[') {
                        let span = self.lx.value_span()?;
                        return Err(err(&format!("expected JSON array, got {}", truncate_for_msg(span))));
                    }
                    let n = self.lx.open(b'[')?;
                    // Validate array length against the schema constraint
                    // (offsets[0]; 0 = unconstrained).
                    let expected = schema.offsets.first().copied().unwrap_or(0);
                    if expected > 0 && n != expected {
                        return Err(MorlocError::Other(format!(
                            "Array length mismatch: expected {}, got {}", expected, n
                        )));
                    }
                    n
                } else {
                    unsafe { (*(f.data as *const Array)).size }
                };
                let data = self.array_data(&f, n, ew)?;
                let flat_elem = self.res.flat(es);
                for i in f.idx..n {
                    if i > 0 {
                        self.lx.expect(b',')?;
                    }
                    // SAFETY: data + i * ew is within the data allocation.
                    let ep = unsafe { data.add(i * ew) };
                    if flat_elem {
                        self.step(st, Frame::new(es, ep, 0))?;
                    } else if self.child(st, &f, i, es, ep)? == Visit::Deferred {
                        return Ok(());
                    }
                }
                self.lx.expect(b']')
            }
            SerialType::Tuple => {
                let n = schema.parameters.len();
                if f.idx == 0 {
                    if self.lx.peek() != Some(b'[') {
                        let span = self.lx.value_span()?;
                        return Err(err(&format!("expected JSON array, got {}", truncate_for_msg(span))));
                    }
                    let given = self.lx.open(b'[')?;
                    if given != n {
                        return Err(err(&format!("expected {} fields, got {}", n, given)));
                    }
                }
                for i in f.idx..n {
                    if i > 0 {
                        self.lx.expect(b',')?;
                    }
                    let sub = unsafe { slot.add(schema.offsets[i]) };
                    if self.child(st, &f, i, &schema.parameters[i], sub)? == Visit::Deferred {
                        return Ok(());
                    }
                }
                self.lx.expect(b']')
            }
            SerialType::Map => {
                // Records on the JSON wire accept two shapes:
                //
                //  - Object form `{"key": val, ...}`, in any member order.
                //    Every field must be present; a missing one raises an
                //    error here. The partial-load entry point
                //    `load_record_fields_from_json` is the path for callers
                //    (the unrolled CLI dispatcher) that accept partial bundles
                //    and fill missing fields from elsewhere. A key the record
                //    does not have is skipped; a key given twice is an error.
                //
                //  - Array form `[v0, v1, ...]`, positional. Length must equal
                //    the field count exactly -- positional encoding has no
                //    notion of a missing key.
                //
                // The frame's `x` names the record's seen set in object form
                // and is `u64::MAX` in array form, where `idx` is the next
                // field; in object form `idx` counts members read.
                let n = schema.parameters.len();
                let mut x = f.x;
                if f.idx == 0 && x != u64::MAX {
                    match self.lx.peek() {
                        Some(b'{') => {
                            self.lx.open(b'{')?;
                            x = self.seen_set(n) as u64;
                        }
                        Some(b'[') => {
                            let given = self.lx.open(b'[')?;
                            if given != n {
                                return Err(err(&format!(
                                    "record array form must have exactly {} fields (one per schema field, in declaration order), got {}",
                                    n, given
                                )));
                            }
                            x = u64::MAX;
                        }
                        _ => {
                            let span = self.lx.value_span()?;
                            return Err(err(&format!(
                                "record source must be a JSON object {{...}} or array [...], got {}",
                                truncate_for_msg(span)
                            )));
                        }
                    }
                }
                let mut g = f;
                g.x = x;
                if x == u64::MAX {
                    for i in f.idx..n {
                        if i > 0 {
                            self.lx.expect(b',')?;
                        }
                        let sub = unsafe { slot.add(schema.offsets[i]) };
                        if self.child(st, &g, i, &schema.parameters[i], sub)? == Visit::Deferred {
                            return Ok(());
                        }
                    }
                    return self.lx.expect(b']');
                }
                let id = x as usize;
                let mut members = f.idx;
                loop {
                    if self.lx.peek() == Some(b'}') {
                        self.lx.pos += 1;
                        break;
                    }
                    if members > 0 {
                        self.lx.expect(b',')?;
                    }
                    let key = self.lx.key()?;
                    members += 1;
                    match schema.keys.iter().position(|k| *k == key) {
                        None => {
                            self.lx.skip_value()?;
                        }
                        Some(i) => {
                            let (word, bit) = (i / 64, 1u64 << (i % 64));
                            if self.seen[id][word] & bit != 0 {
                                return Err(err(&format!("field '{}' given twice in record", key)));
                            }
                            self.seen[id][word] |= bit;
                            let sub = unsafe { slot.add(schema.offsets[i]) };
                            let fs = &schema.parameters[i];
                            if self.res.flat(fs) {
                                self.step(st, Frame::new(fs, sub, 0))?;
                            } else {
                                walk::defer(self, st, &g, members - 1, fs, sub, 0);
                                return Ok(());
                            }
                        }
                    }
                }
                for (i, key) in schema.keys.iter().enumerate() {
                    if self.seen[id][i / 64] & (1u64 << (i % 64)) == 0 {
                        return Err(err(&format!("missing required field '{}' in record", key)));
                    }
                }
                self.free.push(id);
                Ok(())
            }
            SerialType::Optional => {
                if f.idx > 0 {
                    return Ok(());
                }
                // The slot is a single relptr. Absent: RELNULL. Present: the
                // inner T in its own block, pointed at from the slot.
                let inner = schema.parameters.first().ok_or_else(|| err("optional has no inner type"))?;
                let w = unsafe { ShmWriter::new(slot, std::mem::size_of::<RelPtr>()) };
                if self.lx.peek() == Some(b'n') {
                    let span = self.lx.value_span()?;
                    if !is_null(span) {
                        return Err(err(&format!("expected a value or null, got {}", truncate_for_msg(span))));
                    }
                    w.write_val::<RelPtr>(0, RELNULL);
                    return Ok(());
                }
                let inner_abs = shm::shmalloc(inner.width)?;
                // SAFETY: inner_abs is freshly allocated with inner.width bytes.
                unsafe { std::ptr::write_bytes(inner_abs, 0, inner.width) };
                w.write_val::<RelPtr>(0, shm::abs2rel(inner_abs)?);
                self.child(st, &f, 0, inner, inner_abs)?;
                Ok(())
            }
            // A variant is externally tagged: `{"Circle": [1.0]}` for an arm
            // with fields, and the bare name `"Dot"` for one without. The
            // single-key-object form is unambiguous and is what serde and
            // most hand-written encoders already produce.
            SerialType::Variant => {
                if f.idx > 0 {
                    // The payload has been read; the object closes here.
                    if self.lx.peek() == Some(b',') {
                        return Err(err("a variant object must have exactly one key, the constructor name"));
                    }
                    return self.lx.expect(b'}');
                }
                let w = unsafe { ShmWriter::new(slot, 16) };
                let (name, has_payload) = match self.lx.peek() {
                    Some(b'"') => {
                        let span = self.lx.value_span()?;
                        (span[1..span.len() - 1].to_string(), false)
                    }
                    Some(b'{') => {
                        let members = self.lx.open(b'{')?;
                        if members != 1 {
                            return Err(err("a variant object must have exactly one key, the constructor name"));
                        }
                        (self.lx.key()?, true)
                    }
                    _ => {
                        let span = self.lx.value_span()?;
                        return Err(err(&format!(
                            "expected a tagged variant object, got {}",
                            truncate_for_msg(span)
                        )));
                    }
                };
                let tag = schema.keys.iter().position(|k| *k == name).ok_or_else(|| {
                    err(&format!(
                        "'{}' is not a constructor of this type; expected one of {}",
                        name,
                        schema.keys.join(", ")
                    ))
                })?;
                let arm = &schema.parameters[tag];
                w.write_val::<u8>(0, tag as u8);
                // The seven bytes between the tag and the payload pointer
                // are written explicitly so a variant's bytes are fully
                // determined by its value, rather than by whatever the
                // allocator last left there.
                for i in 1..8 {
                    w.write_val::<u8>(i, 0);
                }
                if arm.size == 0 {
                    w.write_val::<RelPtr>(8, RELNULL);
                    if has_payload {
                        // A payload given to a constructor without fields is
                        // ignored, as it always was.
                        self.lx.skip_value()?;
                        return self.lx.expect(b'}');
                    }
                    return Ok(());
                }
                if !has_payload {
                    return Err(err(&format!(
                        "constructor '{name}' takes {} fields but was given none",
                        arm.size
                    )));
                }
                let payload = shm::shmalloc(arm.width)?;
                // SAFETY: freshly allocated with arm.width bytes.
                unsafe { std::ptr::write_bytes(payload, 0, arm.width) };
                w.write_val::<RelPtr>(8, shm::abs2rel(payload)?);
                if self.child(st, &f, 0, arm, payload)? == Visit::Done {
                    let mut g = f;
                    g.idx = 1;
                    return self.step(st, g);
                }
                Ok(())
            }
            SerialType::Table => {
                // JSON-to-Table is conceptually meaningful (read records from
                // a JSON array and build an Arrow buffer) but is not the
                // standard JSON-load path; CSV/JSON-to-Arrow lives in the
                // dedicated arrow_ipc_reader module.
                Err(err("Cannot load a Table from generic JSON; use the Arrow CSV/JSON reader path"))
            }
            SerialType::Recur => unreachable!("a back-reference resolves before it is stepped"),
            _ => {
                let span = self.lx.value_span()?;
                write_leaf(span, schema, Some(slot))?;
                Ok(())
            }
        }
    }
}

// ── Voidstar -> JSON ───────────────────────────────────────────────────────

/// 64 KiB matches the default Linux pipe buffer; sized so writes fill a
/// downstream `read` in one syscall.
const BUFWRITER_CAPACITY: usize = 64 * 1024;

/// `None` = flat, `Some(depth)` = pretty at that indent depth. Threaded
/// through the walker so containers know whether to emit newlines +
/// indentation; scalars ignore it.
type Pretty = Option<usize>;

/// The value as indented JSON, as `pretty_print_voidstar` prints it.
#[cfg(test)]
pub(crate) fn pretty_json_string(ptr: AbsPtr, schema: &Schema) -> Result<String, MorlocError> {
    let mut buf: Vec<u8> = Vec::new();
    to_json(ptr, schema, &mut buf, Some(0))?;
    Ok(String::from_utf8(buf).expect("JSON output is UTF-8"))
}

/// Streaming JSON writer. Emits directly into `w` -- peak memory is the
/// caller's buffer (typically 64 KiB), not the size of the output.
pub fn write_json(ptr: AbsPtr, schema: &Schema, w: &mut dyn Write) -> Result<(), MorlocError> {
    to_json(ptr, schema, w, None)
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

/// JSON-lines: one element per line. Streams element-by-element so
/// peak memory is one element's JSON body, not the whole list.
///
/// * `Array<T>` schema: iterate elements and emit each as JSON + `\n`.
/// * Non-list schemas: emit the whole value on one line (equivalent
///   to `-f json` with a trailing newline). `-f jsonl` on a scalar
///   still parses as valid JSON-lines (one line, one value).
pub fn print_voidstar_jsonl(ptr: AbsPtr, schema: &Schema) -> Result<(), MorlocError> {
    let mut w = io::BufWriter::with_capacity(BUFWRITER_CAPACITY, io::stdout().lock());
    match schema.serial_type {
        SerialType::Array => {
            let r = unsafe { ShmReader::new(ptr) };
            let arr = r.read_array(0);
            if arr.size == 0 || arr.data == RELNULL {
                return map_io(w.flush());
            }
            let es = &schema.parameters[0];
            let data = shm::rel2abs(arr.data)?;
            // Each element is walked under the whole schema, so a
            // back-reference inside an element still resolves.
            let res = crate::recur::Resolver::new(schema);
            for i in 0..arr.size {
                to_json_under(&res, unsafe { data.add(i * es.width) }, es, &mut w, None)?;
                map_io(w.write_all(b"\n"))?;
            }
            map_io(w.flush())
        }
        _ => {
            to_json(ptr, schema, &mut w, None)?;
            map_io(w.write_all(b"\n"))?;
            map_io(w.flush())
        }
    }
}

/// Write the raw byte body of a `String` voidstar (an `Array<u1>`) with no
/// quoting or escaping.
fn write_str_body<W: io::Write>(r: &ShmReader, w: &mut W) -> Result<(), MorlocError> {
    let arr = r.read_array(0);
    if arr.size > 0 && arr.data != RELNULL {
        let dr = unsafe { ShmReader::new(shm::rel2abs(arr.data)?) };
        map_io(w.write_all(dr.read_str(0, arr.size).as_bytes()))?;
    }
    Ok(())
}

/// Write the raw bytes of a `Vector U8` / `[U8]` voidstar (an `Array<u8>`),
/// with no interpretation -- the bytes are emitted exactly, so a `render`
/// handler can produce arbitrary binary output.
fn write_u8_array_body<W: io::Write>(r: &ShmReader, w: &mut W) -> Result<(), MorlocError> {
    let arr = r.read_array(0);
    if arr.size > 0 && arr.data != RELNULL {
        let dr = unsafe { ShmReader::new(shm::rel2abs(arr.data)?) };
        map_io(w.write_all(dr.read_bytes(0, arr.size)))?;
    }
    Ok(())
}

/// Write a `-f raw` value's content bytes to `w`. Supported shapes:
///   * `Str`         -- the body, unquoted (whole-list textual render)
///   * `[Str]`       -- concatenated bodies (streaming textual render)
///   * `Vector U8`   -- the raw bytes (whole-list binary render)
///   * `[Vector U8]` -- concatenated byte blocks (streaming binary render)
/// Any other shape is a clear error. Does NOT flush -- the caller flushes
/// (stdout) or owns the buffer (`Vec`).
fn write_voidstar_raw<W: io::Write>(
    w: &mut W,
    ptr: AbsPtr,
    schema: &Schema,
) -> Result<(), MorlocError> {
    match schema.serial_type {
        SerialType::String => {
            let r = unsafe { ShmReader::new(ptr) };
            write_str_body(&r, w)
        }
        SerialType::Array => {
            let es = schema.parameters.first().ok_or_else(|| {
                err("-f raw: array output has no element schema")
            })?;
            let r = unsafe { ShmReader::new(ptr) };
            let arr = r.read_array(0);
            if arr.size == 0 || arr.data == RELNULL {
                return Ok(());
            }
            // `Vector U8` / `[U8]`: the array data IS the byte body.
            if es.serial_type == SerialType::Uint8 {
                return write_u8_array_body(&r, w);
            }
            // `[Str]` / `[Vector U8]`: emit each element's body in turn.
            let elem_is_u8_vec = es.serial_type == SerialType::Array
                && es.parameters.first().map_or(false, |i| i.serial_type == SerialType::Uint8);
            if es.serial_type != SerialType::String && !elem_is_u8_vec {
                return Err(err(
                    "-f raw requires Str, [Str], Vector U8, or [Vector U8] output \
                     (a `render` handler's bytes)",
                ));
            }
            let data = shm::rel2abs(arr.data)?;
            for i in 0..arr.size {
                let er = unsafe { ShmReader::new(data.add(i * es.width)) };
                if elem_is_u8_vec {
                    write_u8_array_body(&er, w)?;
                } else {
                    write_str_body(&er, w)?;
                }
            }
            Ok(())
        }
        _ => Err(err(
            "-f raw requires Str, [Str], Vector U8, or [Vector U8] output \
             (a `render` handler's bytes)",
        )),
    }
}

/// Verbatim `-f raw` output to stdout -- the output path for `render` terminal
/// handlers (which produce the final bytes).
pub fn print_voidstar_raw(ptr: AbsPtr, schema: &Schema) -> Result<(), MorlocError> {
    let mut w = io::BufWriter::with_capacity(BUFWRITER_CAPACITY, io::stdout().lock());
    write_voidstar_raw(&mut w, ptr, schema)?;
    map_io(w.flush())
}

/// The same `-f raw` content bytes as an owned buffer -- for a media-typed
/// daemon HTTP response body (`Content-Type: <@mime>`).
pub fn voidstar_raw_to_bytes(ptr: AbsPtr, schema: &Schema) -> Result<Vec<u8>, MorlocError> {
    let mut buf: Vec<u8> = Vec::new();
    write_voidstar_raw(&mut buf, ptr, schema)?;
    Ok(buf)
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
    to_json(ptr, schema, &mut w, pretty)?;
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

/// Write the value at `ptr` as JSON.
fn to_json(ptr: AbsPtr, schema: &Schema, w: &mut dyn Write, pretty: Pretty) -> Result<(), MorlocError> {
    let res = crate::recur::Resolver::new(schema);
    to_json_under(&res, ptr, schema, w, pretty)
}

/// Write the value at `ptr`, whose schema is a node of the tree `res`
/// indexes (the root or any node beneath it).
fn to_json_under<'r>(
    res: &crate::recur::Resolver<'r>,
    ptr: AbsPtr,
    schema: &'r Schema,
    w: &mut dyn Write,
    pretty: Pretty,
) -> Result<(), MorlocError> {
    let mut jw = JsonWalk { res, w };
    let mut st = Stack::new();
    st.enter(schema, ptr, pretty);
    walk::run(&mut jw, &mut st)
}

/// The JSON writer. A frame's `x` is the node's pretty-print depth
/// (`None` when printing flat). A container writes its opening bracket on
/// its first visit and its closing bracket as its post action, so a
/// deferred child's whole subtree lands between them.
struct JsonWalk<'a, 'r> {
    res: &'a crate::recur::Resolver<'r>,
    w: &'a mut dyn Write,
}

impl<'a, 'r> JsonWalk<'a, 'r> {
    fn child(
        &mut self,
        st: &mut Stack<Pretty>,
        f: &Frame<Pretty>,
        idx: usize,
        s: &'r Schema,
        data: *const u8,
        pretty: Pretty,
    ) -> Result<Visit, MorlocError> {
        if self.res.flat(s) {
            self.step(st, Frame::new(s, data, pretty))?;
            Ok(Visit::Done)
        } else {
            walk::defer(self, st, f, idx, s, data, pretty);
            Ok(Visit::Deferred)
        }
    }

    /// Open a container, or write it whole when it is empty. Returns
    /// whether there is a body to write.
    fn open(&mut self, open: u8, close: u8, pretty: Pretty, empty: bool) -> Result<bool, MorlocError> {
        if empty {
            map_io(self.w.write_all(&[open, close]))?;
            return Ok(false);
        }
        match pretty {
            None => map_io(self.w.write_all(&[open]))?,
            Some(_) => map_io(self.w.write_all(&[open, b'\n']))?,
        }
        Ok(true)
    }

    fn close(&mut self, close: u8, pretty: Pretty) -> Result<(), MorlocError> {
        if let Some(depth) = pretty {
            map_io(self.w.write_all(b"\n"))?;
            write_indent(self.w, depth)?;
        }
        map_io(self.w.write_all(&[close]))
    }
}

/// The depth children of a container at `pretty` print at.
fn child_pretty(pretty: Pretty) -> Pretty {
    pretty.map(|d| d + 1)
}

impl<'a, 'r> Walker<Pretty> for JsonWalk<'a, 'r> {
    fn has_finish(&self, f: &Frame<Pretty>) -> bool {
        // SAFETY: as in `step`.
        let s = match self.res.resolve(unsafe { &*f.schema }) {
            Ok(s) => s,
            Err(_) => return false,
        };
        let r = unsafe { ShmReader::new(f.data as *mut u8) };
        match s.serial_type {
            SerialType::Array => {
                let arr = r.read_array(0);
                !(arr.size == 0 || arr.data == RELNULL)
            }
            SerialType::Tuple | SerialType::Map => !s.parameters.is_empty(),
            SerialType::Variant => {
                let tag = r.read_u8(0) as usize;
                s.parameters.get(tag).map_or(false, |arm| arm.size != 0)
            }
            _ => false,
        }
    }

    fn finish(&mut self, _st: &mut Stack<Pretty>, f: Frame<Pretty>) -> Result<(), MorlocError> {
        let s = self.res.resolve(unsafe { &*f.schema })?;
        match s.serial_type {
            SerialType::Array | SerialType::Tuple => self.close(b']', f.x),
            SerialType::Map => self.close(b'}', f.x),
            SerialType::Variant => map_io(self.w.write_all(b"}")),
            _ => Ok(()),
        }
    }

    fn step(&mut self, st: &mut Stack<Pretty>, f: Frame<Pretty>) -> Result<(), MorlocError> {
        // SAFETY: frames hold nodes of the tree the resolver indexes, and
        // `data` points at a value laid out as that schema describes.
        let schema: &'r Schema = self.res.resolve(unsafe { &*f.schema })?;
        let r = unsafe { ShmReader::new(f.data as *mut u8) };
        let pretty = f.x;
        let w = &mut *self.w;
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
            SerialType::Variant => {
                if f.idx > 0 {
                    return Ok(());
                }
                let tag = r.read_u8(0) as usize;
                let name = schema.keys.get(tag).ok_or_else(|| {
                    MorlocError::Serialization(format!(
                        "variant tag {} is out of range; the type has {} arms",
                        tag, schema.size
                    ))
                })?;
                let arm = &schema.parameters[tag];
                if arm.size == 0 {
                    map_io(write!(w, "\"{}\"", name))?
                } else {
                    let payload = r.read_val::<RelPtr>(8);
                    map_io(write!(w, "{{\"{}\":", name))?;
                    if payload == RELNULL {
                        map_io(w.write_all(b"null"))?;
                    } else {
                        let inner = shm::rel2abs(payload)?;
                        if self.child(st, &f, 0, arm, inner, pretty)? == Visit::Deferred {
                            return Ok(());
                        }
                    }
                    return walk::end_step(self, st, &f);
                }
            }
            SerialType::Enum   => {
                let tag = r.read_u8(0) as usize;
                // A tag with no constructor means the value and the schema
                // disagree. Say so rather than inventing a name.
                let name = schema.keys.get(tag).ok_or_else(|| {
                    MorlocError::Serialization(format!(
                        "enum tag {} is out of range; the type has {} constructors ({})",
                        tag,
                        schema.size,
                        schema.keys.join(", ")
                    ))
                })?;
                map_io(write!(w, "\"{}\"", name))?
            }
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
                if f.idx == 0 {
                    let empty = arr.size == 0 || arr.data == RELNULL;
                    if !self.open(b'[', b']', pretty, empty)? {
                        return Ok(());
                    }
                }
                let data = shm::rel2abs(arr.data)?;
                let cp = child_pretty(pretty);
                let flat_elem = self.res.flat(es);
                for i in f.idx..arr.size {
                    write_sep(self.w, i, pretty)?;
                    let p = unsafe { data.add(i * es.width) };
                    if flat_elem {
                        self.step(st, Frame::new(es, p, cp))?;
                    } else if self.child(st, &f, i, es, p, cp)? == Visit::Deferred {
                        return Ok(());
                    }
                }
                return walk::end_step(self, st, &f);
            }
            SerialType::Tuple => {
                let n = schema.parameters.len();
                if f.idx == 0 && !self.open(b'[', b']', pretty, n == 0)? {
                    return Ok(());
                }
                let cp = child_pretty(pretty);
                for (i, fs) in schema.parameters.iter().enumerate().skip(f.idx) {
                    write_sep(self.w, i, pretty)?;
                    let p = unsafe { f.data.add(schema.offsets[i]) };
                    if self.child(st, &f, i, fs, p, cp)? == Visit::Deferred {
                        return Ok(());
                    }
                }
                return walk::end_step(self, st, &f);
            }
            SerialType::Map => {
                let n = schema.parameters.len();
                if f.idx == 0 && !self.open(b'{', b'}', pretty, n == 0)? {
                    return Ok(());
                }
                let cp = child_pretty(pretty);
                for (i, fs) in schema.parameters.iter().enumerate().skip(f.idx) {
                    write_sep(self.w, i, pretty)?;
                    if i < schema.keys.len() {
                        // Keys are morloc identifiers -- ASCII, no escaping needed.
                        map_io(self.w.write_all(b"\""))?;
                        map_io(self.w.write_all(schema.keys[i].as_bytes()))?;
                        map_io(self.w.write_all(if pretty.is_some() { b"\": " } else { b"\":" }))?;
                    }
                    let p = unsafe { f.data.add(schema.offsets[i]) };
                    if self.child(st, &f, i, fs, p, cp)? == Visit::Deferred {
                        return Ok(());
                    }
                }
                return walk::end_step(self, st, &f);
            }
            SerialType::Optional => {
                if f.idx > 0 {
                    return Ok(());
                }
                let relptr: RelPtr = r.read_val(0);
                if relptr == RELNULL {
                    map_io(w.write_all(b"null"))?;
                } else {
                    let inner = &schema.parameters[0];
                    let p = shm::rel2abs(relptr)?;
                    self.child(st, &f, 0, inner, p, pretty)?;
                }
            }
            SerialType::Table => {
                return Err(err("Cannot render a Table to generic JSON; use the Arrow-to-JSON path"));
            }
            SerialType::Recur => unreachable!("a back-reference resolves before it is stepped"),
        }
        Ok(())
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
    #[must_use]
    fn setup() -> std::sync::RwLockReadGuard<'static, ()> { crate::init_test_shm() }

    #[test] fn test_int()     { let _shm = setup(); let s = parse_schema("i4").unwrap(); let p = read_json_with_schema("42", &s).unwrap(); assert_eq!(voidstar_to_json_string(p, &s).unwrap(), "42"); }
    #[test] fn test_string()  { let _shm = setup(); let s = parse_schema("s").unwrap(); let p = read_json_with_schema("\"hello\"", &s).unwrap(); assert_eq!(voidstar_to_json_string(p, &s).unwrap(), "\"hello\""); }
    #[test] fn test_bool()    { let _shm = setup(); let s = parse_schema("b").unwrap(); let p = read_json_with_schema("true", &s).unwrap(); assert_eq!(voidstar_to_json_string(p, &s).unwrap(), "true"); }
    #[test] fn test_array()   { let _shm = setup(); let s = parse_schema("ai4").unwrap(); let p = read_json_with_schema("[1,2,3]", &s).unwrap(); assert_eq!(voidstar_to_json_string(p, &s).unwrap(), "[1,2,3]"); }
    #[test] fn test_opt_some(){ let _shm = setup(); let s = parse_schema("?i4").unwrap(); let p = read_json_with_schema("5", &s).unwrap(); assert_eq!(voidstar_to_json_string(p, &s).unwrap(), "5"); }
    #[test] fn test_opt_null(){ let _shm = setup(); let s = parse_schema("?i4").unwrap(); let p = read_json_with_schema("null", &s).unwrap(); assert_eq!(voidstar_to_json_string(p, &s).unwrap(), "null"); }

    // Record `{m :: i4, n :: i4}` -- schema encoding `m21mi41ni4`.
    fn rec2_schema() -> Schema { parse_schema("m21mi41ni4").unwrap() }

    /// A small deterministic generator of JSON values for a schema.
    struct Gen(u64);
    impl Gen {
        fn next(&mut self) -> u64 {
            self.0 ^= self.0 << 13;
            self.0 ^= self.0 >> 7;
            self.0 ^= self.0 << 17;
            self.0
        }
        fn below(&mut self, n: u64) -> u64 {
            self.next() % n
        }
        fn value(&mut self, s: &Schema, root: &Schema, depth: usize) -> String {
            match s.serial_type {
                SerialType::Nil => "null".into(),
                SerialType::Bool => if self.below(2) == 0 { "true" } else { "false" }.into(),
                SerialType::Sint8 | SerialType::Sint16 | SerialType::Sint32 | SerialType::Sint64 =>
                    format!("{}", self.below(200) as i64 - 100),
                SerialType::Uint8 | SerialType::Uint16 | SerialType::Uint32 | SerialType::Uint64 =>
                    format!("{}", self.below(200)),
                SerialType::Float32 | SerialType::Float64 => format!("{}.5", self.below(50) as i64 - 25),
                SerialType::Int => match self.below(3) {
                    0 => format!("{}", self.below(1000) as i64 - 500),
                    1 => "123456789012345678901234567890".into(),
                    _ => "-98765432109876543210".into(),
                },
                SerialType::String => {
                    let words = ["", "a", "hello world", r"tab\tnew\nline", r#"quote\"q"#, r"\u00e9t\u00e9", "[{,}]"];
                    format!("\"{}\"", words[self.below(words.len() as u64) as usize])
                }
                SerialType::Enum => format!("\"{}\"", s.keys[self.below(s.keys.len() as u64) as usize]),
                SerialType::Array => {
                    let n = if depth > 6 { 0 } else { self.below(4) };
                    let items: Vec<String> = (0..n).map(|_| self.value(&s.parameters[0], root, depth + 1)).collect();
                    format!("[{}]", items.join(", "))
                }
                SerialType::Tuple => {
                    let items: Vec<String> = s.parameters.iter().map(|p| self.value(p, root, depth + 1)).collect();
                    format!("[{}]", items.join(","))
                }
                SerialType::Map => {
                    if self.below(4) == 0 {
                        let items: Vec<String> = s.parameters.iter().map(|p| self.value(p, root, depth + 1)).collect();
                        format!("[{}]", items.join(","))
                    } else {
                        // Members in a shuffled order.
                        let mut idx: Vec<usize> = (0..s.parameters.len()).collect();
                        for i in (1..idx.len()).rev() {
                            let j = self.below(i as u64 + 1) as usize;
                            idx.swap(i, j);
                        }
                        let items: Vec<String> = idx
                            .iter()
                            .map(|&i| format!("\"{}\" : {}", s.keys[i], self.value(&s.parameters[i], root, depth + 1)))
                            .collect();
                        format!("{{ {} }}", items.join(" , "))
                    }
                }
                SerialType::Optional => {
                    if depth > 6 || self.below(3) == 0 {
                        "null".into()
                    } else {
                        self.value(&s.parameters[0], root, depth + 1)
                    }
                }
                SerialType::Variant => {
                    let mut arms: Vec<usize> = if depth > 6 {
                        (0..s.keys.len()).filter(|&i| s.parameters[i].size == 0).collect()
                    } else {
                        (0..s.keys.len()).collect()
                    };
                    // A type whose every arm carries fields ends through
                    // the type it holds.
                    if arms.is_empty() {
                        arms = (0..s.keys.len()).collect();
                    }
                    let i = arms[self.below(arms.len() as u64) as usize];
                    if s.parameters[i].size == 0 {
                        format!("\"{}\"", s.keys[i])
                    } else {
                        format!("{{\"{}\":{}}}", s.keys[i], self.value(&s.parameters[i], root, depth + 1))
                    }
                }
                SerialType::Recur => {
                    let target = crate::recur::Resolver::new(root);
                    let t = target.resolve(s).unwrap();
                    // The declaration is a node of the root tree; walk it.
                    self.value(t, root, depth + 1)
                }
                _ => "null".into(),
            }
        }
    }

    /// Whatever the loader accepts, the writer prints back as the same
    /// JSON document, for random values over every schema shape.
    #[test]
    fn test_random_values_round_trip() {
        let _shm = setup();
        let schemas = [
            "i4", "s", "as", "t3si4s", "aai4", "t2?i4s", "m22idj4tagsas", "v23Nil04Cons2i4s", "e21A1B",
            "&2LLm24headi84tail?^2LL", "&4Treev24Leaf04Node3i8^4Tree^4Tree",
            "&1Av23Nil05ACons2i8&1Bv15BCons2i8^1A", "&4Rosem21vi84kidsa^4Rose", "a&2LLm24headi84tail?^2LL",
            "m31af81b?s1cat2i4?i4", "?v23Nil04Cons2i4s",
        ];
        let mut g = Gen(0x9e3779b97f4a7c15);
        for schema_str in schemas {
            let schema = parse_schema(schema_str).unwrap();
            for _ in 0..40 {
                let text = g.value(&schema, &schema, 0);
                let ptr = read_json_with_schema(&text, &schema)
                    .unwrap_or_else(|e| panic!("{schema_str}: {text}: {e}"));
                let out = voidstar_to_json_string(ptr, &schema).unwrap();
                let a: serde_json::Value = serde_json::from_str(&text).unwrap();
                let b: serde_json::Value = serde_json::from_str(&out).unwrap();
                let res = crate::recur::Resolver::new(&schema);
                assert_eq!(normalize(&res, &schema, a), b, "{schema_str}: {text} -> {out}");
            }
        }
    }

    /// The document as the writer would have spelt it: array-form records
    /// become objects and back-references take their declaration's shape.
    fn normalize(res: &crate::recur::Resolver<'_>, s: &Schema, v: serde_json::Value) -> serde_json::Value {
        use serde_json::Value;
        let s = res.resolve(s).unwrap();
        match (s.serial_type, v) {
            (SerialType::Map, Value::Array(items)) => Value::Object(
                s.keys.iter().cloned().zip(items.into_iter().zip(s.parameters.iter()).map(|(x, p)| normalize(res, p, x))).collect(),
            ),
            (SerialType::Map, Value::Object(m)) => Value::Object(
                m.into_iter()
                    .map(|(k, x)| {
                        let i = s.keys.iter().position(|kk| *kk == k).unwrap();
                        (k, normalize(res, &s.parameters[i], x))
                    })
                    .collect(),
            ),
            (SerialType::Tuple, Value::Array(items)) => Value::Array(
                items.into_iter().zip(s.parameters.iter()).map(|(x, p)| normalize(res, p, x)).collect(),
            ),
            (SerialType::Array, Value::Array(items)) => Value::Array(
                items.into_iter().map(|x| normalize(res, &s.parameters[0], x)).collect(),
            ),
            (SerialType::Optional, Value::Null) => Value::Null,
            (SerialType::Optional, x) => normalize(res, &s.parameters[0], x),
            (SerialType::Variant, Value::Object(m)) => Value::Object(
                m.into_iter()
                    .map(|(k, x)| {
                        let i = s.keys.iter().position(|kk| *kk == k).unwrap();
                        (k, normalize(res, &s.parameters[i], x))
                    })
                    .collect(),
            ),
            (_, x) => x,
        }
    }

    #[test]
    fn test_record_missing_field_errors() {
        let _shm = setup();
        let s = rec2_schema();
        let r = read_json_with_schema("{\"m\":1}", &s);
        let msg = r.err().expect("missing field must error").to_string();
        assert!(msg.contains("missing required field 'n'"), "got: {}", msg);
    }

    #[test]
    fn test_record_array_wrong_length_errors() {
        let _shm = setup();
        let s = rec2_schema();
        let r = read_json_with_schema("[1]", &s);
        let msg = r.err().expect("short array must error").to_string();
        assert!(msg.contains("exactly 2 fields"), "got: {}", msg);
    }

    #[test]
    fn test_partial_load_full_object() {
        let _shm = setup();
        let s = rec2_schema();
        let v = load_record_fields_from_json("{\"m\":7,\"n\":11}", &s).unwrap();
        assert_eq!(v.len(), 2);
        assert!(v[0].is_some());
        assert!(v[1].is_some());
    }

    #[test]
    fn test_partial_load_partial_object() {
        let _shm = setup();
        let s = rec2_schema();
        let v = load_record_fields_from_json("{\"m\":7}", &s).unwrap();
        assert_eq!(v.len(), 2);
        assert!(v[0].is_some(), "present field should be Some");
        assert!(v[1].is_none(), "absent field should be None");
    }

    #[test]
    fn test_partial_load_unknown_field_errors() {
        let _shm = setup();
        let s = rec2_schema();
        let r = load_record_fields_from_json("{\"m\":7,\"oops\":1}", &s);
        let msg = r.err().expect("unknown key must error").to_string();
        assert!(msg.contains("unknown field 'oops'"), "got: {}", msg);
    }

    #[test]
    fn test_partial_load_array_must_be_complete() {
        let _shm = setup();
        let s = rec2_schema();
        let r = load_record_fields_from_json("[1]", &s);
        let msg = r.err().expect("short array must error").to_string();
        assert!(msg.contains("exactly 2 fields"), "got: {}", msg);

        let v = load_record_fields_from_json("[1,2]", &s).unwrap();
        assert!(v.iter().all(|x| x.is_some()));
    }

    #[test]
    fn test_partial_load_non_map_schema_errors() {
        let _shm = setup();
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
        let _shm = setup();
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
        let _shm = setup();
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
        let _shm = setup();
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
