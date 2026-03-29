//! Shared voidstar operations: relptr adjustment, binary serialization,
//! schema-aware free, and flatten-to-buffer.
//!
//! These functions operate on the morloc voidstar binary format in SHM.
//! They are used by packet.rs, cli.rs, and json.rs.

use crate::error::MorlocError;
use crate::schema::{Schema, SerialType};
use crate::shm::{self, AbsPtr, Array, RelPtr, Tensor};

// ── adjust_voidstar_relptrs ────────────────────────────────────────────────

/// Adjust all relptrs in a voidstar blob by adding base_rel.
/// Used after copying a flattened blob into SHM.
pub fn adjust_relptrs(
    data: AbsPtr,
    schema: &Schema,
    base_rel: RelPtr,
) -> Result<(), MorlocError> {
    // SAFETY: data points to a voidstar blob in SHM. We adjust relptrs in-place;
    // all pointer arithmetic stays within the blob's bounds as defined by schema.
    unsafe {
        match schema.serial_type {
            SerialType::String | SerialType::Array => {
                let arr = &mut *(data as *mut Array);
                arr.data += base_rel;
                if !schema.parameters.is_empty() && !schema.parameters[0].is_fixed_width() {
                    let arr_data = shm::rel2abs(arr.data)?;
                    let w = schema.parameters[0].width;
                    for i in 0..arr.size {
                        adjust_relptrs(arr_data.add(i * w), &schema.parameters[0], base_rel)?;
                    }
                }
            }
            SerialType::Tuple | SerialType::Map => {
                for i in 0..schema.parameters.len() {
                    adjust_relptrs(data.add(schema.offsets[i]), &schema.parameters[i], base_rel)?;
                }
            }
            SerialType::Optional => {
                if *data != 0 && !schema.parameters.is_empty() {
                    let off = schema.offsets.first().copied()
                        .unwrap_or_else(|| shm::align_up(1, schema.parameters[0].alignment().max(1)));
                    adjust_relptrs(data.add(off), &schema.parameters[0], base_rel)?;
                }
            }
            SerialType::Tensor => {
                let t = &mut *(data as *mut Tensor);
                if t.total_elements > 0 {
                    t.shape += base_rel;
                    t.data += base_rel;
                }
            }
            _ => {}
        }
    }
    Ok(())
}

// ── read_voidstar_binary ───────────────────────────────────────────────────

/// Read a flat voidstar binary blob into SHM, adjusting relptrs.
pub fn read_binary(blob: &[u8], schema: &Schema) -> Result<AbsPtr, MorlocError> {
    let base = shm::shmalloc(blob.len())?;
    // SAFETY: base is freshly allocated with blob.len() bytes.
    unsafe { std::ptr::copy_nonoverlapping(blob.as_ptr(), base, blob.len()) };
    let base_rel = shm::abs2rel(base)?;
    adjust_relptrs(base, schema, base_rel)?;
    Ok(base)
}

// ── shfree_by_schema ───────────────────────────────────────────────────────

/// Zero metadata for nested structures so the parent block can be cleanly freed.
/// Does NOT call shfree on sub-pointers (they're cursor-packed in the same block).
pub fn free_by_schema(ptr: AbsPtr, schema: &Schema) -> Result<(), MorlocError> {
    // SAFETY: ptr points to voidstar data in SHM with layout described by schema.
    // We zero metadata at schema.width offsets within the structure.
    unsafe {
        match schema.serial_type {
            SerialType::String | SerialType::Array => {
                let arr = &*(ptr as *const Array);
                if arr.data > 0 && !schema.parameters.is_empty() && !schema.parameters[0].is_fixed_width() {
                    let arr_data = shm::rel2abs(arr.data)?;
                    let w = schema.parameters[0].width;
                    for i in 0..arr.size {
                        free_by_schema(arr_data.add(i * w), &schema.parameters[0])?;
                    }
                }
            }
            SerialType::Tuple | SerialType::Map => {
                for i in 0..schema.parameters.len() {
                    free_by_schema(ptr.add(schema.offsets[i]), &schema.parameters[i])?;
                }
            }
            SerialType::Tensor => {} // inline, freed by parent
            _ => {}
        }
        std::ptr::write_bytes(ptr, 0, schema.width);
    }
    Ok(())
}

// ── flatten_voidstar_to_buffer ─────────────────────────────────────────────

/// Flatten a voidstar structure in SHM into a self-contained byte buffer.
/// Relptrs in the output are offsets from position 0 of the buffer.
pub fn flatten_to_buffer(data: AbsPtr, schema: &Schema) -> Result<Vec<u8>, MorlocError> {
    let total = crate::ffi::calc_voidstar_size_inner(data, schema)?;
    let mut buf = vec![0u8; total];

    // SAFETY: data points to at least schema.width bytes in SHM; buf has total >= schema.width bytes.
    unsafe { std::ptr::copy_nonoverlapping(data, buf.as_mut_ptr(), schema.width) };

    // Phase 2: fix up relptrs and copy variable-length data
    let mut cursor = schema.width;
    flatten_fixup(&mut buf, 0, data, schema, &mut cursor)?;

    Ok(buf)
}

fn flatten_fixup(
    buf: &mut [u8],
    buf_offset: usize,
    data: AbsPtr,
    schema: &Schema,
    cursor: &mut usize,
) -> Result<(), MorlocError> {
    // SAFETY: buf is sized by calc_voidstar_size_inner to hold the entire flattened structure.
    // data points to corresponding SHM data. cursor tracks write position within buf.
    unsafe {
        match schema.serial_type {
            SerialType::String | SerialType::Array => {
                let orig_arr = &*(data as *const Array);
                let buf_arr = &mut *(buf.as_mut_ptr().add(buf_offset) as *mut Array);
                if orig_arr.size == 0 {
                    buf_arr.data = 0;
                    return Ok(());
                }
                let orig_data = shm::rel2abs(orig_arr.data)?;
                let elem_schema = &schema.parameters[0];
                let align = elem_schema.alignment();
                *cursor = shm::align_up(*cursor, align);
                buf_arr.data = *cursor as RelPtr;
                let elem_w = elem_schema.width;
                let total_bytes = elem_w * orig_arr.size;
                buf[*cursor..*cursor + total_bytes].copy_from_slice(
                    std::slice::from_raw_parts(orig_data, total_bytes)
                );
                let elem_start = *cursor;
                *cursor += total_bytes;
                if !elem_schema.is_fixed_width() {
                    for i in 0..orig_arr.size {
                        flatten_fixup(
                            buf, elem_start + i * elem_w,
                            orig_data.add(i * elem_w), elem_schema, cursor,
                        )?;
                    }
                }
            }
            SerialType::Tuple | SerialType::Map => {
                for i in 0..schema.parameters.len() {
                    flatten_fixup(
                        buf, buf_offset + schema.offsets[i],
                        data.add(schema.offsets[i]), &schema.parameters[i], cursor,
                    )?;
                }
            }
            SerialType::Optional => {
                let tag = *buf.as_ptr().add(buf_offset);
                if tag != 0 && !schema.parameters.is_empty() {
                    let off = schema.offsets.first().copied()
                        .unwrap_or_else(|| shm::align_up(1, schema.parameters[0].alignment().max(1)));
                    flatten_fixup(
                        buf, buf_offset + off,
                        data.add(off), &schema.parameters[0], cursor,
                    )?;
                }
            }
            SerialType::Tensor => {
                let orig = &*(data as *const Tensor);
                let buf_t = &mut *(buf.as_mut_ptr().add(buf_offset) as *mut Tensor);
                if orig.total_elements == 0 {
                    buf_t.shape = 0;
                    buf_t.data = 0;
                    return Ok(());
                }
                let ndim = schema.offsets.first().copied().unwrap_or(0);
                let orig_shape = shm::rel2abs(orig.shape)?;
                *cursor = shm::align_up(*cursor, std::mem::align_of::<i64>());
                buf_t.shape = *cursor as RelPtr;
                let shape_bytes = ndim * std::mem::size_of::<i64>();
                buf[*cursor..*cursor + shape_bytes].copy_from_slice(
                    std::slice::from_raw_parts(orig_shape, shape_bytes)
                );
                *cursor += shape_bytes;

                let orig_data = shm::rel2abs(orig.data)?;
                let elem_w = schema.parameters[0].width;
                let elem_align = schema.parameters[0].alignment();
                *cursor = shm::align_up(*cursor, elem_align);
                buf_t.data = *cursor as RelPtr;
                let data_bytes = orig.total_elements * elem_w;
                buf[*cursor..*cursor + data_bytes].copy_from_slice(
                    std::slice::from_raw_parts(orig_data, data_bytes)
                );
                *cursor += data_bytes;
            }
            _ => {} // primitives already copied by parent
        }
    }
    Ok(())
}

// ── write_voidstar_binary (to fd) ──────────────────────────────────────────

/// Flatten voidstar and write to a file descriptor. Returns bytes written.
pub fn write_binary_to_fd(fd: i32, data: AbsPtr, schema: &Schema) -> Result<usize, MorlocError> {
    let buf = flatten_to_buffer(data, schema)?;
    // SAFETY: buf is a valid byte slice; fd is a valid file descriptor from the caller.
    let written = unsafe {
        libc::write(fd, buf.as_ptr() as *const std::ffi::c_void, buf.len())
    };
    if written < 0 {
        return Err(MorlocError::Io(std::io::Error::last_os_error()));
    }
    Ok(written as usize)
}
