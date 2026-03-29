//! MessagePack <-> Voidstar conversion.
//!
//! Replaces serialize.c + mpack.c. Uses the `rmp` crate for MessagePack I/O.
//! The voidstar binary format is morloc-specific (Array/Tensor structs with relptrs).

use crate::error::MorlocError;
use crate::schema::{Schema, SerialType};
use crate::shm::{self, AbsPtr, Array, RELNULL};

// ── Voidstar -> MessagePack ────────────────────────────────────────────────

/// Serialize voidstar data to MessagePack bytes.
pub fn pack_with_schema(ptr: AbsPtr, schema: &Schema) -> Result<Vec<u8>, MorlocError> {
    let mut buf = Vec::with_capacity(256);
    pack_data(ptr, schema, &mut buf)?;
    Ok(buf)
}

fn pack_data(ptr: AbsPtr, schema: &Schema, buf: &mut Vec<u8>) -> Result<(), MorlocError> {
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
            SerialType::String => {
                let arr = &*(ptr as *const Array);
                let data = shm::rel2abs(arr.data)?;
                let bytes = std::slice::from_raw_parts(data, arr.size);
                rmp::encode::write_str_len(buf, arr.size as u32)
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
                        pack_data(elem_ptr, elem_schema, buf)?;
                    }
                }
            }
            SerialType::Tuple | SerialType::Map => {
                rmp::encode::write_array_len(buf, schema.parameters.len() as u32)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack tuple: {}", e)))?;

                for (i, field_schema) in schema.parameters.iter().enumerate() {
                    let field_ptr = ptr.add(schema.offsets[i]);
                    pack_data(field_ptr, field_schema, buf)?;
                }
            }
            SerialType::Optional => {
                let tag = *ptr;
                if tag == 0 {
                    rmp::encode::write_nil(buf)
                        .map_err(|e| MorlocError::Serialization(format!("msgpack nil: {}", e)))?;
                } else {
                    let inner_schema = &schema.parameters[0];
                    let inner_offset = schema.offsets.first().copied()
                        .unwrap_or_else(|| shm::align_up(1, inner_schema.alignment().max(1)));
                    let inner_ptr = ptr.add(inner_offset);
                    pack_data(inner_ptr, inner_schema, buf)?;
                }
            }
            SerialType::Tensor => {
                return Err(MorlocError::Serialization(
                    "MessagePack serialization of tensors not yet supported".into(),
                ));
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
    unpack_obj(base, schema, &mut cursor, &mut reader)?;
    Ok(base)
}

fn unpack_obj(
    ptr: AbsPtr,
    schema: &Schema,
    cursor: &mut AbsPtr,
    reader: &mut &[u8],
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
            SerialType::Array => {
                let n = decode::read_array_len(reader)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack array len: {}", e)))?
                    as usize;
                let elem_schema = &schema.parameters[0];
                let elem_width = elem_schema.width;

                let arr = &mut *(ptr as *mut Array);
                arr.size = n;

                // Align cursor for element data
                let align = elem_schema.alignment();
                let aligned = shm::align_up(*cursor as usize, align);
                *cursor = aligned as AbsPtr;

                arr.data = shm::abs2rel(*cursor)?;
                let data_start = *cursor;
                *cursor = cursor.add(n * elem_width);

                for i in 0..n {
                    let elem_ptr = data_start.add(i * elem_width);
                    unpack_obj(elem_ptr, elem_schema, cursor, reader)?;
                }
            }
            SerialType::Tuple | SerialType::Map => {
                let _n = decode::read_array_len(reader)
                    .map_err(|e| MorlocError::Serialization(format!("msgpack tuple len: {}", e)))?;
                for (i, field_schema) in schema.parameters.iter().enumerate() {
                    let field_ptr = ptr.add(schema.offsets[i]);
                    unpack_obj(field_ptr, field_schema, cursor, reader)?;
                }
            }
            SerialType::Optional => {
                let inner_schema = &schema.parameters[0];
                let inner_offset = schema.offsets.first().copied()
                    .unwrap_or_else(|| shm::align_up(1, inner_schema.alignment().max(1)));

                // Peek at the next byte to detect nil
                if !reader.is_empty() && reader[0] == 0xc0 {
                    // Null: consume nil byte, set tag = 0
                    decode::read_nil(reader)
                        .map_err(|e| MorlocError::Serialization(format!("msgpack nil: {}", e)))?;
                    *ptr = 0;
                } else {
                    // Present: set tag = 1, parse inner
                    *ptr = 1;
                    let inner_ptr = ptr.add(inner_offset);
                    unpack_obj(inner_ptr, inner_schema, cursor, reader)?;
                }
            }
            SerialType::Tensor => {
                return Err(MorlocError::Serialization(
                    "MessagePack tensor deserialization not yet supported".into(),
                ));
            }
        }
    }
    Ok(())
}

fn unpack_int(ptr: AbsPtr, st: SerialType, reader: &mut &[u8]) -> Result<(), MorlocError> {
    // Use rmp's generic read_int which handles all integer markers
    let val: i64 = rmp::decode::read_int(reader)
        .map_err(|e| MorlocError::Serialization(format!("msgpack int: {}", e)))?;

    // SAFETY: ptr points to schema.width bytes in SHM; each cast writes exactly that width.
    unsafe {
        match st {
            SerialType::Sint8 => *(ptr as *mut i8) = val as i8,
            SerialType::Sint16 => *(ptr as *mut i16) = val as i16,
            SerialType::Sint32 => *(ptr as *mut i32) = val as i32,
            SerialType::Sint64 => *(ptr as *mut i64) = val,
            SerialType::Uint8 => *ptr = val as u8,
            SerialType::Uint16 => *(ptr as *mut u16) = val as u16,
            SerialType::Uint32 => *(ptr as *mut u32) = val as u32,
            SerialType::Uint64 => *(ptr as *mut u64) = val as u64,
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
    calc_size_r(schema, &mut reader)
}

fn calc_size_r(schema: &Schema, reader: &mut &[u8]) -> Result<usize, MorlocError> {
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
        SerialType::String => {
            let len = rmp::decode::read_str_len(reader)
                .map_err(|e| MorlocError::Serialization(format!("size calc str: {}", e)))?
                as usize;
            if reader.len() >= len { *reader = &reader[len..]; }
            Ok(std::mem::size_of::<Array>() + len)
        }
        SerialType::Array => {
            let n = rmp::decode::read_array_len(reader)
                .map_err(|e| MorlocError::Serialization(format!("size calc array: {}", e)))?
                as usize;
            let elem_schema = &schema.parameters[0];
            let mut total = std::mem::size_of::<Array>();
            // Alignment padding
            total = shm::align_up(total, elem_schema.alignment());
            for _ in 0..n {
                total += calc_size_r(elem_schema, reader)?;
            }
            Ok(total)
        }
        SerialType::Tuple | SerialType::Map => {
            let _n = rmp::decode::read_array_len(reader).ok();
            let mut total = schema.width;
            for field_schema in &schema.parameters {
                if !field_schema.is_fixed_width() {
                    total += calc_size_r(field_schema, reader)?;
                } else {
                    calc_size_r(field_schema, reader)?;
                }
            }
            Ok(total)
        }
        SerialType::Optional => {
            let inner_schema = &schema.parameters[0];
            if !reader.is_empty() && reader[0] == 0xc0 {
                rmp::decode::read_nil(reader).ok();
                Ok(schema.width.max(1 + inner_schema.width))
            } else {
                let inner_size = calc_size_r(inner_schema, reader)?;
                let align = inner_schema.alignment().max(1);
                let offset = shm::align_up(1, align);
                Ok(offset + inner_size)
            }
        }
        SerialType::Tensor => Ok(0),
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
