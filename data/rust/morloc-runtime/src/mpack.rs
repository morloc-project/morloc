//! MessagePack <-> Voidstar conversion.
//!
//! Replaces serialize.c + mpack.c. Uses the `rmp` crate for MessagePack I/O.
//! The voidstar binary format is morloc-specific (Array/Tensor structs with relptrs).

use crate::error::MorlocError;
use crate::recur::{self, RecurEnv};
use crate::schema::{Schema, SerialType};
use crate::shm::{self, AbsPtr, Array, RELNULL};

// ── Voidstar -> MessagePack ────────────────────────────────────────────────

/// Serialize voidstar data to MessagePack bytes.
pub fn pack_with_schema(ptr: AbsPtr, schema: &Schema) -> Result<Vec<u8>, MorlocError> {
    let mut buf = Vec::with_capacity(256);
    let mut env: RecurEnv = Vec::new();
    pack_data(ptr, schema, &mut buf, &mut env)?;
    Ok(buf)
}

fn pack_data(
    ptr: AbsPtr,
    schema: &Schema,
    buf: &mut Vec<u8>,
    env: &mut RecurEnv,
) -> Result<(), MorlocError> {
    recur::with_scope(env, schema, |env| pack_data_inner(ptr, schema, buf, env))
}

fn pack_data_inner(
    ptr: AbsPtr,
    schema: &Schema,
    buf: &mut Vec<u8>,
    env: &mut RecurEnv,
) -> Result<(), MorlocError> {
    // SAFETY: ptr points to voidstar data in SHM with layout described by schema.
    // All reads are within bounds defined by schema.width, Array headers, etc.
    unsafe {
        match schema.serial_type {
            SerialType::Nil => {
                rmp::encode::write_nil(buf)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack nil: {}", e)))?;
            }
            SerialType::Bool => {
                let v = *ptr != 0;
                rmp::encode::write_bool(buf, v)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack bool: {}", e)))?;
            }
            SerialType::Uint8 => {
                rmp::encode::write_uint(buf, *ptr as u64)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack uint: {}", e)))?;
            }
            SerialType::Uint16 => {
                rmp::encode::write_uint(buf, *(ptr as *const u16) as u64)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack uint: {}", e)))?;
            }
            SerialType::Uint32 => {
                rmp::encode::write_uint(buf, *(ptr as *const u32) as u64)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack uint: {}", e)))?;
            }
            SerialType::Uint64 => {
                rmp::encode::write_uint(buf, *(ptr as *const u64))
                    .map_err(|e| MorlocError::Serialization(format!("msgpack uint: {}", e)))?;
            }
            SerialType::Sint8 => {
                rmp::encode::write_sint(buf, *(ptr as *const i8) as i64)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack sint: {}", e)))?;
            }
            SerialType::Sint16 => {
                rmp::encode::write_sint(buf, *(ptr as *const i16) as i64)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack sint: {}", e)))?;
            }
            SerialType::Sint32 => {
                rmp::encode::write_sint(buf, *(ptr as *const i32) as i64)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack sint: {}", e)))?;
            }
            SerialType::Sint64 => {
                rmp::encode::write_sint(buf, *(ptr as *const i64))
                    .map_err(|e| MorlocError::Serialization(format!("msgpack sint: {}", e)))?;
            }
            SerialType::Float32 => {
                let f = *(ptr as *const f32) as f64;
                rmp::encode::write_f64(buf, f)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack float: {}", e)))?;
            }
            SerialType::Float64 => {
                let f = *(ptr as *const f64);
                rmp::encode::write_f64(buf, f)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack float: {}", e)))?;
            }
            SerialType::Int => {
                // Inline BigInt: [size, value_or_relptr]
                let size = *(ptr as *const usize);
                if size <= 1 {
                    let val = *(ptr.add(8) as *const i64);
                    rmp::encode::write_sint(buf, if size == 0 { 0 } else { val })
                        .map_err(|e| MorlocError::Serialization(format!("msgpack bigint: {}", e)))?;
                } else {
                    let relptr = *(ptr.add(std::mem::size_of::<usize>()) as *const shm::RelPtr);
                    let data = shm::rel2abs(relptr)?;
                    let bytes = std::slice::from_raw_parts(data, size * 8);
                    rmp::encode::write_bin_len(buf, bytes.len() as u32)
                        .map_err(|e| MorlocError::Serialization(format!("msgpack bigint: {}", e)))?;
                    buf.extend_from_slice(bytes);
                }
            }
            SerialType::String => {
                let arr = &*(ptr as *const Array);
                let data = shm::rel2abs(arr.data)?;
                let bytes = std::slice::from_raw_parts(data, arr.size);
                rmp::encode::write_str_len(buf, arr.size as u32)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack str: {}", e)))?;
                buf.extend_from_slice(bytes);
            }
            SerialType::IFile | SerialType::OStream | SerialType::IStream => {
                // Persistence to msgpack uses path form regardless of the
                // in-memory tag. TAG_HANDLE values look up the path via
                // the local SHM registry so a file on disk always carries
                // a path the next reader can `mlc_open` against.
                use morloc_runtime_types::stream_handle as sh;
                let field = ptr as *const u8;
                let tag = sh::read_tag(field);
                let payload = sh::read_payload(field);
                let path: String = if tag == sh::TAG_PATH {
                    if payload == sh::RELNULL_PAYLOAD {
                        String::new()
                    } else {
                        let suballoc = shm::rel2abs(payload as shm::RelPtr)?;
                        let path_len = sh::read_path_size(suballoc) as usize;
                        let bytes = std::slice::from_raw_parts(suballoc.add(8), path_len);
                        std::str::from_utf8(bytes)
                            .map_err(|_| MorlocError::Serialization(
                                "msgpack stream-handle: path is not valid UTF-8".into(),
                            ))?
                            .to_string()
                    }
                } else if tag == sh::TAG_HANDLE {
                    crate::stream::handle_path(payload as i64)?
                } else {
                    return Err(MorlocError::Serialization(format!(
                        "msgpack stream-handle: unsupported tag {}", tag,
                    )));
                };
                let bytes = path.as_bytes();
                rmp::encode::write_str_len(buf, bytes.len() as u32)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack str: {}", e)))?;
                buf.extend_from_slice(bytes);
            }
            SerialType::Array => {
                let arr = &*(ptr as *const Array);
                let elem_schema = &schema.parameters[0];
                let elem_width = elem_schema.width;

                rmp::encode::write_array_len(buf, arr.size as u32)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack array: {}", e)))?;

                if arr.size > 0 && arr.data != RELNULL {
                    let data = shm::rel2abs(arr.data)?;
                    for i in 0..arr.size {
                        let elem_ptr = data.add(i * elem_width);
                        pack_data(elem_ptr, elem_schema, buf, env)?;
                    }
                }
            }
            SerialType::Tuple | SerialType::Map => {
                rmp::encode::write_array_len(buf, schema.parameters.len() as u32)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack tuple: {}", e)))?;

                for (i, field_schema) in schema.parameters.iter().enumerate() {
                    let field_ptr = ptr.add(schema.offsets[i]);
                    pack_data(field_ptr, field_schema, buf, env)?;
                }
            }
            SerialType::Optional => {
                // The Optional slot is a single relptr: RELNULL for absent,
                // otherwise the relptr to T's body elsewhere in the buffer.
                let relptr = *(ptr as *const shm::RelPtr);
                if relptr == shm::RELNULL {
                    rmp::encode::write_nil(buf)
                        .map_err(|e| MorlocError::Serialization(format!("msgpack nil: {}", e)))?;
                } else {
                    let inner_schema = &schema.parameters[0];
                    let inner_ptr = shm::rel2abs(relptr)?;
                    pack_data(inner_ptr, inner_schema, buf, env)?;
                }
            }
            SerialType::Table => {
                // Tables travel as Arrow IPC blobs through SHM, never as
                // msgpack. Hitting this case means a Table value got
                // routed to the msgpack path; the caller should have
                // dispatched via the Arrow C Data Interface instead.
                return Err(MorlocError::Serialization(
                    "Cannot msgpack-encode a Table; Tables use the Arrow IPC SHM wire path".into(),
                ));
            }
            SerialType::Recur => {
                // Resolve the back-reference to the declared named
                // schema on the env stack and pack the data as if it
                // were that schema. The data has finite depth in
                // SHM (relptrs into smaller subtrees), so recursion
                // terminates at the natural base cases (empty arrays
                // or null optionals).
                let name = schema.name.as_deref().unwrap_or("");
                let target_ptr = recur::lookup(env, name)?;
                // SAFETY: target_ptr came from the env stack which holds
                // pointers derived from live `Schema` references in the
                // outer call tree; the borrow is still valid here.
                let target = &*target_ptr;
                pack_data(ptr, target, buf, env)?;
            }
        }
    }
    Ok(())
}

// ── MessagePack -> Voidstar ────────────────────────────────────────────────

/// Deserialize MessagePack bytes into voidstar format in SHM.
pub fn unpack_with_schema(
    data: &[u8],
    schema: &Schema,
) -> Result<AbsPtr, MorlocError> {
    // Two-pass: first calculate size, then deserialize
    let size = calc_unpack_size(data, schema)?;
    let base = shm::shmalloc(size)?;
    // SAFETY: base is freshly allocated with `size` bytes.
    unsafe { std::ptr::write_bytes(base, 0, size) };

    // SAFETY: cursor starts at base + schema.width, within the allocated region.
    let mut cursor = unsafe { base.add(schema.width) };
    let mut reader = &data[..];
    let mut env: RecurEnv = Vec::new();
    unpack_obj(base, schema, &mut cursor, &mut reader, &mut env)?;
    Ok(base)
}

fn unpack_obj(
    ptr: AbsPtr,
    schema: &Schema,
    cursor: &mut AbsPtr,
    reader: &mut &[u8],
    env: &mut RecurEnv,
) -> Result<(), MorlocError> {
    recur::with_scope(env, schema, |env| unpack_obj_inner(ptr, schema, cursor, reader, env))
}

fn unpack_obj_inner(
    ptr: AbsPtr,
    schema: &Schema,
    cursor: &mut AbsPtr,
    reader: &mut &[u8],
    env: &mut RecurEnv,
) -> Result<(), MorlocError> {
    use rmp::decode;

    // SAFETY: ptr and cursor point into a single contiguous SHM allocation
    // sized by calc_unpack_size. Each write respects schema.width bounds.
    unsafe {
        match schema.serial_type {
            SerialType::Nil => {
                decode::read_nil(reader)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack nil: {}", e)))?;
                *ptr = 0;
            }
            SerialType::Bool => {
                let v = decode::read_bool(reader)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack bool: {}", e)))?;
                *ptr = v as u8;
            }
            SerialType::Uint8 | SerialType::Uint16 | SerialType::Uint32 | SerialType::Uint64
            | SerialType::Sint8 | SerialType::Sint16 | SerialType::Sint32 | SerialType::Sint64 => {
                unpack_int(ptr, schema.serial_type, reader)?;
            }
            SerialType::Float32 => {
                let f = read_float(reader)?;
                *(ptr as *mut f32) = f as f32;
            }
            SerialType::Float64 => {
                let f = read_float(reader)?;
                *(ptr as *mut f64) = f;
            }
            SerialType::Int => {
                // Inline BigInt: [size, value_or_relptr]
                let fields = ptr as *mut i64;
                let saved = *reader;
                if let Ok(len) = rmp::decode::read_bin_len(reader) {
                    // Overflow: multi-limb
                    let len = len as usize;
                    let nlimbs = len / 8;
                    *fields = nlimbs as i64;
                    *(fields.add(1)) = shm::abs2rel(*cursor)? as i64;
                    if nlimbs > 0 && reader.len() >= len {
                        std::ptr::copy_nonoverlapping(reader.as_ptr(), *cursor, len);
                        *reader = &(*reader)[len..];
                    }
                    *cursor = cursor.add(nlimbs * 8);
                } else {
                    // Inline: single integer value
                    *reader = saved;
                    let val: i64 = rmp::decode::read_int(reader)
                        .map_err(|e| MorlocError::Serialization(format!("msgpack bigint: {}", e)))?;
                    *fields = 1;
                    *(fields.add(1)) = val;
                }
            }
            SerialType::String => {
                let len = decode::read_str_len(reader)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack str len: {}", e)))?
                    as usize;
                let arr = &mut *(ptr as *mut Array);
                arr.size = len;
                arr.data = shm::abs2rel(*cursor)?;

                // Read string bytes directly
                if len > 0 {
                    if reader.len() < len {
                        return Err(MorlocError::Serialization("msgpack str truncated".into()));
                    }
                    std::ptr::copy_nonoverlapping(reader.as_ptr(), *cursor, len);
                    *reader = &reader[len..];
                }
                *cursor = cursor.add(len);
            }
            SerialType::IFile | SerialType::OStream | SerialType::IStream => {
                // msgpack carries the path as a string; we lay it down in
                // path form (TAG_PATH + `{size, bytes}` suballoc). The
                // receiver's language bridge will see the schema's F/O/I
                // code and re-open via `mlc_open` on first use.
                use morloc_runtime_types::stream_handle as sh;
                let len = decode::read_str_len(reader)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack str len: {}", e)))?
                    as usize;
                let field = ptr as *mut u8;
                if len == 0 {
                    sh::write_field(field, sh::TAG_PATH, sh::RELNULL_PAYLOAD);
                } else {
                    if reader.len() < len {
                        return Err(MorlocError::Serialization("msgpack str truncated".into()));
                    }
                    let rel = shm::abs2rel(*cursor)?;
                    let bytes = std::slice::from_raw_parts(reader.as_ptr(), len);
                    sh::write_path_suballoc(*cursor, bytes);
                    sh::write_field(field, sh::TAG_PATH, rel as u64);
                    *reader = &reader[len..];
                    *cursor = cursor.add(sh::path_suballoc_size(len));
                }
            }
            SerialType::Array => {
                let n = decode::read_array_len(reader)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack array len: {}", e)))?
                    as usize;
                let elem_schema = &schema.parameters[0];
                let elem_width = elem_schema.width;

                let arr = &mut *(ptr as *mut Array);
                arr.size = n;

                // Align cursor for element data
                // (bumps to 64 for primitive numerics for SIMD/BLAS)
                let align = elem_schema.array_data_alignment();
                let aligned = shm::align_up(*cursor as usize, align);
                *cursor = aligned as AbsPtr;

                arr.data = shm::abs2rel(*cursor)?;
                let data_start = *cursor;
                *cursor = cursor.add(n * elem_width);

                for i in 0..n {
                    let elem_ptr = data_start.add(i * elem_width);
                    unpack_obj(elem_ptr, elem_schema, cursor, reader, env)?;
                }
            }
            SerialType::Tuple | SerialType::Map => {
                let _n = decode::read_array_len(reader)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack tuple len: {}", e)))?;
                for (i, field_schema) in schema.parameters.iter().enumerate() {
                    let field_ptr = ptr.add(schema.offsets[i]);
                    unpack_obj(field_ptr, field_schema, cursor, reader, env)?;
                }
            }
            SerialType::Optional => {
                let inner_schema = &schema.parameters[0];
                let relptr_slot = ptr as *mut shm::RelPtr;

                // Peek at the next byte to detect nil
                if !reader.is_empty() && reader[0] == 0xc0 {
                    // Absent: write RELNULL, consume nil byte
                    decode::read_nil(reader)
                        .map_err(|e| MorlocError::Serialization(format!("msgpack nil: {}", e)))?;
                    *relptr_slot = shm::RELNULL;
                } else {
                    // Present: allocate inner T at the cursor, write its
                    // relptr into the Optional slot, then unpack T into
                    // the cursor region. The cursor advances past T's
                    // header width here; T's own walker advances it
                    // further for any sub-data it has.
                    let inner_align = inner_schema.alignment().max(1);
                    let aligned = shm::align_up(*cursor as usize, inner_align);
                    *cursor = aligned as AbsPtr;
                    *relptr_slot = shm::abs2rel(*cursor)?;
                    let inner_ptr = *cursor;
                    *cursor = cursor.add(inner_schema.width);
                    unpack_obj(inner_ptr, inner_schema, cursor, reader, env)?;
                }
            }
            SerialType::Table => {
                // See pack_data: Tables are not msgpack-serialised. Hitting
                // this case is a routing bug in the caller.
                return Err(MorlocError::Serialization(
                    "Cannot msgpack-decode a Table; Tables use the Arrow IPC SHM wire path".into(),
                ));
            }
            SerialType::Recur => {
                // Mirror of pack_data: resolve the back-reference and
                // unpack as if the data carried the named schema.
                let name = schema.name.as_deref().unwrap_or("");
                let target_ptr = recur::lookup(env, name)?;
                // SAFETY: see pack_data's Recur arm for the lifetime
                // argument; the env-stored pointer is still live here.
                let target = &*target_ptr;
                unpack_obj(ptr, target, cursor, reader, env)?;
            }
        }
    }
    Ok(())
}

fn unpack_int(ptr: AbsPtr, st: SerialType, reader: &mut &[u8]) -> Result<(), MorlocError> {
    // Use rmp's generic read_int which handles all integer markers
    let val: i64 = rmp::decode::read_int(reader)
        .map_err(|e| MorlocError::Serialization(format!("msgpack int: {}", e)))?;

    // Range-check before narrowing. The msgpack stream may carry a wider
    // integer than the receiving schema slot (e.g. a producer that wrote
    // an i64-tagged value that fits in the wire width but not the target
    // type). Silent truncation here would defeat the json.rs check.
    let check_s = |lo: i64, hi: i64, name: &str| -> Result<i64, MorlocError> {
        if val < lo || val > hi {
            Err(MorlocError::Serialization(format!(
                "value {} out of range for {} (range {} to {})", val, name, lo, hi
            )))
        } else { Ok(val) }
    };
    let check_u = |hi: u64, name: &str| -> Result<u64, MorlocError> {
        if val < 0 || (val as u64) > hi {
            Err(MorlocError::Serialization(format!(
                "value {} out of range for {} (range 0 to {})", val, name, hi
            )))
        } else { Ok(val as u64) }
    };

    // Compute the narrowed values (with checks) outside the unsafe block.
    let i8v = if matches!(st, SerialType::Sint8)  { check_s(i8::MIN  as i64, i8::MAX  as i64, "Int8" )? as i8  } else { 0 };
    let i16v = if matches!(st, SerialType::Sint16) { check_s(i16::MIN as i64, i16::MAX as i64, "Int16")? as i16 } else { 0 };
    let i32v = if matches!(st, SerialType::Sint32) { check_s(i32::MIN as i64, i32::MAX as i64, "Int32")? as i32 } else { 0 };
    let u8v  = if matches!(st, SerialType::Uint8)  { check_u(u8::MAX  as u64, "UInt8" )? as u8  } else { 0 };
    let u16v = if matches!(st, SerialType::Uint16) { check_u(u16::MAX as u64, "UInt16")? as u16 } else { 0 };
    let u32v = if matches!(st, SerialType::Uint32) { check_u(u32::MAX as u64, "UInt32")? as u32 } else { 0 };
    let u64v: u64 = if matches!(st, SerialType::Uint64) {
        if val < 0 {
            return Err(MorlocError::Serialization(format!(
                "value {} out of range for UInt64 (range 0 to {})", val, u64::MAX
            )));
        }
        val as u64
    } else { 0 };

    // SAFETY: ptr points to schema.width bytes in SHM; each cast writes exactly that width.
    unsafe {
        match st {
            SerialType::Sint8  => *(ptr as *mut i8)  = i8v,
            SerialType::Sint16 => *(ptr as *mut i16) = i16v,
            SerialType::Sint32 => *(ptr as *mut i32) = i32v,
            SerialType::Sint64 => *(ptr as *mut i64) = val,
            SerialType::Uint8  => *ptr               = u8v,
            SerialType::Uint16 => *(ptr as *mut u16) = u16v,
            SerialType::Uint32 => *(ptr as *mut u32) = u32v,
            SerialType::Uint64 => *(ptr as *mut u64) = u64v,
            _ => {}
        }
    }
    Ok(())
}

fn read_float(reader: &mut &[u8]) -> Result<f64, MorlocError> {
    let marker = rmp::decode::read_marker(reader)
        .map_err(|_| MorlocError::Serialization("msgpack float: unexpected EOF".into()))?;
    match marker {
        rmp::Marker::F32 => {
            let bits = read_be_u32(reader)?;
            Ok(f32::from_bits(bits) as f64)
        }
        rmp::Marker::F64 => {
            let bits = read_be_u64(reader)?;
            Ok(f64::from_bits(bits))
        }
        // Integer markers can appear for integer-valued floats - read the data manually
        rmp::Marker::FixPos(v) => Ok(v as f64),
        rmp::Marker::FixNeg(v) => Ok(v as f64),
        _ => {
            // For other integer encodings, read bytes manually
            let n = match marker {
                rmp::Marker::U8 => { read_byte(reader)? as f64 }
                rmp::Marker::U16 => { read_be_u16(reader)? as f64 }
                rmp::Marker::U32 => { read_be_u32(reader)? as f64 }
                rmp::Marker::U64 => { read_be_u64(reader)? as f64 }
                rmp::Marker::I8 => { read_byte(reader)? as i8 as f64 }
                rmp::Marker::I16 => { read_be_u16(reader)? as i16 as f64 }
                rmp::Marker::I32 => { read_be_u32(reader)? as i32 as f64 }
                rmp::Marker::I64 => { read_be_u64(reader)? as i64 as f64 }
                _ => {
                    return Err(MorlocError::Serialization(format!(
                        "unexpected msgpack marker {:?} for float", marker
                    )));
                }
            };
            Ok(n)
        }
    }
}

// Also fix read_f32/read_f64 - rmp's functions include the marker, but we already consumed it
// So we need to read the raw data bytes directly.

fn read_byte(reader: &mut &[u8]) -> Result<u8, MorlocError> {
    if reader.is_empty() { return Err(MorlocError::Serialization("unexpected EOF".into())); }
    let v = reader[0];
    *reader = &reader[1..];
    Ok(v)
}

fn read_be_u16(reader: &mut &[u8]) -> Result<u16, MorlocError> {
    if reader.len() < 2 { return Err(MorlocError::Serialization("unexpected EOF".into())); }
    let v = u16::from_be_bytes([reader[0], reader[1]]);
    *reader = &reader[2..];
    Ok(v)
}

fn read_be_u32(reader: &mut &[u8]) -> Result<u32, MorlocError> {
    if reader.len() < 4 { return Err(MorlocError::Serialization("unexpected EOF".into())); }
    let v = u32::from_be_bytes([reader[0], reader[1], reader[2], reader[3]]);
    *reader = &reader[4..];
    Ok(v)
}

fn read_be_u64(reader: &mut &[u8]) -> Result<u64, MorlocError> {
    if reader.len() < 8 { return Err(MorlocError::Serialization("unexpected EOF".into())); }
    let v = u64::from_be_bytes([reader[0], reader[1], reader[2], reader[3], reader[4], reader[5], reader[6], reader[7]]);
    *reader = &reader[8..];
    Ok(v)
}

// ── Size calculation for unpack ────────────────────────────────────────────

fn calc_unpack_size(data: &[u8], schema: &Schema) -> Result<usize, MorlocError> {
    let mut reader = data;
    let mut env: RecurEnv = Vec::new();
    calc_size_r(schema, &mut reader, &mut env)
}

fn calc_size_r(
    schema: &Schema,
    reader: &mut &[u8],
    env: &mut RecurEnv,
) -> Result<usize, MorlocError> {
    recur::with_scope(env, schema, |env| calc_size_r_inner(schema, reader, env))
}

fn calc_size_r_inner(
    schema: &Schema,
    reader: &mut &[u8],
    env: &mut RecurEnv,
) -> Result<usize, MorlocError> {
    match schema.serial_type {
        SerialType::Nil => {
            rmp::decode::read_nil(reader).ok();
            Ok(1)
        }
        SerialType::Bool => {
            rmp::decode::read_bool(reader).ok();
            Ok(1)
        }
        SerialType::Sint8 | SerialType::Uint8 => { skip_int(reader)?; Ok(1) }
        SerialType::Sint16 | SerialType::Uint16 => { skip_int(reader)?; Ok(2) }
        SerialType::Sint32 | SerialType::Uint32 | SerialType::Float32 => { skip_int(reader)?; Ok(4) }
        SerialType::Sint64 | SerialType::Uint64 | SerialType::Float64 => { skip_int(reader)?; Ok(8) }
        SerialType::Int => {
            // Inline BigInt: 16 bytes for common case, more for overflow
            let saved = *reader;
            if let Ok(len) = rmp::decode::read_bin_len(reader) {
                let len = len as usize;
                if reader.len() >= len { *reader = &reader[len..]; }
                // Overflow: 16-byte header + alignment + limb data
                Ok(16 + std::mem::align_of::<u64>() - 1 + len)
            } else {
                *reader = saved;
                skip_int(reader)?;
                Ok(16) // Inline: just the [size, value] pair
            }
        }
        SerialType::String => {
            let len = rmp::decode::read_str_len(reader)
                .map_err(|e| MorlocError::Serialization(format!("size calc str: {}", e)))?
                as usize;
            if reader.len() >= len { *reader = &reader[len..]; }
            Ok(std::mem::size_of::<Array>() + len)
        }
        SerialType::IFile | SerialType::OStream | SerialType::IStream => {
            // Tagged stream-handle field: 16-byte inline + path suballoc
            // (`8 + path_len`). msgpack carries the path as a string.
            use morloc_runtime_types::stream_handle as sh;
            let len = rmp::decode::read_str_len(reader)
                .map_err(|e| MorlocError::Serialization(format!("size calc str: {}", e)))?
                as usize;
            if reader.len() >= len { *reader = &reader[len..]; }
            let suballoc = if len == 0 { 0 } else { sh::path_suballoc_size(len) };
            Ok(sh::STREAM_HANDLE_FIELD_SIZE + suballoc)
        }
        SerialType::Array => {
            let n = rmp::decode::read_array_len(reader)
                .map_err(|e| MorlocError::Serialization(format!("size calc array: {}", e)))?
                as usize;
            let elem_schema = &schema.parameters[0];
            let mut total = std::mem::size_of::<Array>();
            // Alignment padding (bumps to 64 for primitive numerics for SIMD/BLAS)
            total = shm::align_up(total, elem_schema.array_data_alignment());
            for _ in 0..n {
                total += calc_size_r(elem_schema, reader, env)?;
            }
            Ok(total)
        }
        SerialType::Tuple | SerialType::Map => {
            let _n = rmp::decode::read_array_len(reader).ok();
            let mut total = schema.width;
            for field_schema in &schema.parameters {
                if !field_schema.is_fixed_width() {
                    total += calc_size_r(field_schema, reader, env)?;
                } else {
                    calc_size_r(field_schema, reader, env)?;
                }
            }
            Ok(total)
        }
        SerialType::Optional => {
            // The Optional slot is `schema.width` (= sizeof(RelPtr)) bytes.
            // When present, the inner T's data lives at the cursor and
            // needs `inner_align - 1` worst-case padding + sizeof(inner)
            // + whatever the inner's own variable extras contribute.
            let inner_schema = &schema.parameters[0];
            if !reader.is_empty() && reader[0] == 0xc0 {
                rmp::decode::read_nil(reader).ok();
                Ok(schema.width)
            } else {
                let inner_size = calc_size_r(inner_schema, reader, env)?;
                let align = inner_schema.alignment().max(1);
                Ok(schema.width + (align - 1) + inner_size)
            }
        }
        SerialType::Table => {
            // Tables are not msgpack-sized; the calc-size helper is part
            // of the msgpack decode pipeline and would only be invoked on
            // a Table by mistake. Return a serialisation error rather
            // than an arbitrary number.
            Err(MorlocError::Serialization(
                "Cannot compute msgpack size for a Table; Tables use the Arrow IPC SHM wire path".into(),
            ))
        }
        SerialType::Recur => {
            // Resolve to the named schema and account for its width
            // (the data lives behind a relptr at this slot, so we add
            // only schema.width plus whatever the body adds via reads).
            let name = schema.name.as_deref().unwrap_or("");
            let target_ptr = recur::lookup(env, name)?;
            // SAFETY: env pointer is valid for the duration of the walk.
            let target = unsafe { &*target_ptr };
            calc_size_r(target, reader, env)
        }
    }
}

fn skip_int(reader: &mut &[u8]) -> Result<(), MorlocError> {
    let marker = rmp::decode::read_marker(reader)
        .map_err(|_| MorlocError::Serialization("skip int: unexpected EOF".into()))?;
    let skip = match marker {
        rmp::Marker::FixPos(_) | rmp::Marker::FixNeg(_) => 0,
        rmp::Marker::U8 | rmp::Marker::I8 => 1,
        rmp::Marker::U16 | rmp::Marker::I16 => 2,
        rmp::Marker::U32 | rmp::Marker::I32 | rmp::Marker::F32 => 4,
        rmp::Marker::U64 | rmp::Marker::I64 | rmp::Marker::F64 => 8,
        _ => 0,
    };
    if reader.len() >= skip {
        *reader = &reader[skip..];
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::schema::parse_schema;
    use crate::json;

    fn setup_shm() {
        crate::init_test_shm();
    }

    #[test]
    fn test_roundtrip_int_via_msgpack() {
        setup_shm();
        let schema = parse_schema("i4").unwrap();
        // JSON -> voidstar -> msgpack -> voidstar -> JSON
        let ptr1 = json::read_json_with_schema("42", &schema).unwrap();
        let mpk = pack_with_schema(ptr1, &schema).unwrap();
        let ptr2 = unpack_with_schema(&mpk, &schema).unwrap();
        let json_out = json::voidstar_to_json_string(ptr2, &schema).unwrap();
        assert_eq!(json_out, "42");
    }

    #[test]
    fn test_roundtrip_string_via_msgpack() {
        setup_shm();
        let schema = parse_schema("s").unwrap();
        let ptr1 = json::read_json_with_schema("\"hello world\"", &schema).unwrap();
        let mpk = pack_with_schema(ptr1, &schema).unwrap();
        let ptr2 = unpack_with_schema(&mpk, &schema).unwrap();
        let json_out = json::voidstar_to_json_string(ptr2, &schema).unwrap();
        assert_eq!(json_out, "\"hello world\"");
    }

    #[test]
    fn test_roundtrip_array_via_msgpack() {
        setup_shm();
        let schema = parse_schema("ai4").unwrap();
        let ptr1 = json::read_json_with_schema("[10,20,30]", &schema).unwrap();
        let mpk = pack_with_schema(ptr1, &schema).unwrap();
        let ptr2 = unpack_with_schema(&mpk, &schema).unwrap();
        let json_out = json::voidstar_to_json_string(ptr2, &schema).unwrap();
        assert_eq!(json_out, "[10,20,30]");
    }

    #[test]
    fn test_roundtrip_bool_via_msgpack() {
        setup_shm();
        let schema = parse_schema("b").unwrap();
        let ptr1 = json::read_json_with_schema("true", &schema).unwrap();
        let mpk = pack_with_schema(ptr1, &schema).unwrap();
        let ptr2 = unpack_with_schema(&mpk, &schema).unwrap();
        let json_out = json::voidstar_to_json_string(ptr2, &schema).unwrap();
        assert_eq!(json_out, "true");
    }

    #[test]
    fn test_roundtrip_optional_null_via_msgpack() {
        setup_shm();
        let schema = parse_schema("?i4").unwrap();
        let ptr1 = json::read_json_with_schema("null", &schema).unwrap();
        let mpk = pack_with_schema(ptr1, &schema).unwrap();
        let ptr2 = unpack_with_schema(&mpk, &schema).unwrap();
        let json_out = json::voidstar_to_json_string(ptr2, &schema).unwrap();
        assert_eq!(json_out, "null");
    }

    #[test]
    fn test_pack_only_string() {
        setup_shm();
        let schema = parse_schema("s").unwrap();
        let ptr1 = json::read_json_with_schema("\"hi\"", &schema).unwrap();
        let mpk = pack_with_schema(ptr1, &schema).unwrap();
        assert!(!mpk.is_empty());
        assert_eq!(mpk.len(), 3);
    }

    #[test]
    fn test_unpack_only_int() {
        setup_shm();
        let schema = parse_schema("i4").unwrap();
        // msgpack for 42 = [42] (fixint)
        let mpk = vec![42u8];
        let ptr = unpack_with_schema(&mpk, &schema).unwrap();
        let json_out = json::voidstar_to_json_string(ptr, &schema).unwrap();
        assert_eq!(json_out, "42");
    }

    #[test]
    fn test_unpack_only_string() {
        setup_shm();
        let schema = parse_schema("s").unwrap();
        // msgpack for "hi" = [0xa2, 0x68, 0x69]
        let mpk = vec![0xa2, 0x68, 0x69];
        let size = calc_unpack_size(&mpk, &schema).unwrap();
        eprintln!("unpack size for string: {} (Array={}, total={})", size, std::mem::size_of::<shm::Array>(), size);
        let ptr = unpack_with_schema(&mpk, &schema).unwrap();
        let json_out = json::voidstar_to_json_string(ptr, &schema).unwrap();
        assert_eq!(json_out, "\"hi\"");
    }
}
