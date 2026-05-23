//! Shared voidstar operations: relptr adjustment, binary serialization,
//! schema-aware free, and flatten-to-buffer.
//!
//! These functions operate on the morloc voidstar binary format in SHM.
//! They are used by packet.rs, cli.rs, and json.rs.

use crate::error::MorlocError;
use crate::recur::{self, RecurEnv};
use crate::schema::{Schema, SerialType};
use crate::shm::{self, AbsPtr, Array, RelPtr};

// ── adjust_voidstar_relptrs ────────────────────────────────────────────────

/// Adjust all relptrs in a voidstar blob by adding base_rel.
///
/// Used after copying a flattened blob into SHM: the blob's internal
/// relptrs are offsets from position 0 of the blob; adding base_rel
/// converts them to valid SHM-global relptrs. Walks every relptr-bearing
/// node (Array.data, BigInt overflow limbs, Optional slot, all parents)
/// and rebases.
///
/// Recursive records (MORLOC_RECUR back-references) require an env
/// stack so each `^<name>` resolves to the matching `&<name>` ancestor.
/// The `with_env` helper threads that state through; the public entry
/// here just initializes an empty stack.
pub fn adjust_relptrs(
    data: AbsPtr,
    schema: &Schema,
    base_rel: RelPtr,
) -> Result<(), MorlocError> {
    let mut env: RecurEnv = Vec::new();
    adjust_relptrs_with_env(data, schema, base_rel, &mut env)
}

fn adjust_relptrs_with_env(
    data: AbsPtr,
    schema: &Schema,
    base_rel: RelPtr,
    env: &mut RecurEnv,
) -> Result<(), MorlocError> {
    recur::with_scope(env, schema, |env| adjust_relptrs_inner(data, schema, base_rel, env))
}

fn adjust_relptrs_inner(
    data: AbsPtr,
    schema: &Schema,
    base_rel: RelPtr,
    env: &mut RecurEnv,
) -> Result<(), MorlocError> {
    // SAFETY: data points to a voidstar blob in SHM. We adjust relptrs in-place;
    // all pointer arithmetic stays within the blob's bounds as defined by schema.
    unsafe {
        match schema.serial_type {
            SerialType::Int => {
                // Inline BigInt: [size, value_or_relptr]. Only adjust when size > 1.
                let size = *(data as *const usize);
                if size > 1 {
                    let relptr = &mut *(data.add(std::mem::size_of::<usize>()) as *mut RelPtr);
                    *relptr += base_rel;
                }
                Ok(())
            }
            SerialType::String | SerialType::Array => {
                let arr = &mut *(data as *mut Array);
                arr.data += base_rel;
                if !schema.parameters.is_empty() && !schema.parameters[0].is_fixed_width() {
                    let arr_data = shm::rel2abs(arr.data)?;
                    let elem_schema = &schema.parameters[0];
                    let w = elem_schema.width;
                    for i in 0..arr.size {
                        adjust_relptrs_with_env(arr_data.add(i * w), elem_schema, base_rel, env)?;
                    }
                }
                Ok(())
            }
            SerialType::Tuple | SerialType::Map => {
                for i in 0..schema.parameters.len() {
                    adjust_relptrs_with_env(
                        data.add(schema.offsets[i]),
                        &schema.parameters[i],
                        base_rel,
                        env,
                    )?;
                }
                Ok(())
            }
            SerialType::Optional => {
                // The Optional slot is a relptr: RELNULL = absent, else
                // a buffer-relative offset that becomes SHM-relative
                // after adding base_rel. Then resolve and recurse into
                // the inner T to rebase any of its sub-relptrs too.
                let relptr_slot = &mut *(data as *mut RelPtr);
                if *relptr_slot != shm::RELNULL && !schema.parameters.is_empty() {
                    *relptr_slot += base_rel;
                    let inner_abs = shm::rel2abs(*relptr_slot)?;
                    adjust_relptrs_with_env(inner_abs, &schema.parameters[0], base_rel, env)?;
                }
                Ok(())
            }
            SerialType::Recur => {
                // Resolve to the named declaration on the env stack and
                // adjust as if the data carried that schema. Without this
                // branch, inner relptrs of recursive records (the
                // children Array.data of nested Trees, the Optional
                // relptrs of LL-style chains) are left unadjusted when
                // voidstar gets copied between regions.
                let name = schema.name.as_deref().unwrap_or("");
                let target_ptr = recur::lookup(env, name)?;
                let target = &*target_ptr;
                adjust_relptrs_with_env(data, target, base_rel, env)
            }
            _ => Ok(()),
        }
    }
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

// ── shfree_inplace ─────────────────────────────────────────────────────────

/// Release every separately-allocated SHM sub-block reachable from a voidstar
/// wrapper at `ptr`, without freeing the wrapper itself.
///
/// Use this when `ptr` points to a slot inside a larger allocation (e.g. a
/// record/tuple field, an array element of compound type) and you want to
/// release the slot's resources before overwriting the slot in place. The
/// caller is responsible for writing fresh contents into `ptr` afterwards or
/// for ensuring the parent allocation is itself freed.
///
/// Assumes multi-block layout: each Array/String payload, each BigInt limb
/// buffer, etc. is its own SHM block (the format produced by the eval
/// pipeline and by `read_json_with_schema`). Do not call on single-block
/// payloads from `read_voidstar_binary` / `unpack_with_schema` -- their
/// sub-data is cursor-packed inside the parent block, not at separately
/// freeable block heads.
///
/// Counterpart of [`deep_copy`]: deep_copy allocates the same set of
/// sub-blocks that shfree_inplace releases.
pub unsafe fn shfree_inplace(ptr: AbsPtr, schema: &Schema) -> Result<(), MorlocError> {
    match schema.serial_type {
        SerialType::String => {
            let arr = &*(ptr as *const Array);
            if arr.size > 0 && arr.data >= 0 {
                let data = shm::rel2abs(arr.data)?;
                shm::shfree(data)?;
            }
        }
        SerialType::Array => {
            let arr = &*(ptr as *const Array);
            if arr.size > 0 && arr.data >= 0 && !schema.parameters.is_empty() {
                let elem_schema = &schema.parameters[0];
                let data = shm::rel2abs(arr.data)?;
                if !elem_schema.is_fixed_width() {
                    let w = elem_schema.width;
                    for i in 0..arr.size {
                        shfree_inplace(data.add(i * w), elem_schema)?;
                    }
                }
                shm::shfree(data)?;
            }
        }
        SerialType::Tuple | SerialType::Map => {
            for i in 0..schema.parameters.len() {
                shfree_inplace(ptr.add(schema.offsets[i]), &schema.parameters[i])?;
            }
        }
        SerialType::Optional => {
            // The Optional slot is a relptr to the inner T. Free what the
            // pointer references, then null it so the slot is in a
            // well-defined "absent" state.
            let relptr_slot = &mut *(ptr as *mut shm::RelPtr);
            if *relptr_slot != shm::RELNULL && !schema.parameters.is_empty() {
                let inner_abs = shm::rel2abs(*relptr_slot)?;
                shfree_inplace(inner_abs, &schema.parameters[0])?;
                *relptr_slot = shm::RELNULL;
            }
        }
        SerialType::Int => {
            // Inline BigInt: [size, value_or_relptr]. Limb buffer only allocated
            // when size > 1.
            let size = *(ptr as *const usize);
            if size > 1 {
                let off = std::mem::size_of::<usize>();
                let relptr = *(ptr.add(off) as *const shm::RelPtr);
                if relptr >= 0 {
                    let limbs = shm::rel2abs(relptr)?;
                    shm::shfree(limbs)?;
                }
            }
        }
        _ => {
            // Fixed-size primitives (Bool/Sint*/Uint*/Float*/Nil/Table): no
            // sub-blocks to release.
        }
    }
    Ok(())
}

// ── deep_copy ──────────────────────────────────────────────────────────────

/// Deep-copy a voidstar tree into multi-block layout: every Array/String
/// payload and BigInt limb buffer is allocated in its own SHM block, so the
/// resulting tree can be released by separately shfree-ing each sub-block.
///
/// The source layout is irrelevant -- this works for both single-block
/// payloads (e.g. mlc_load output, CLI-arg voidstars) and pre-existing
/// multi-block trees. After the copy the source can be released as a single
/// shfree on its top-level block (single-block source) or via per-block
/// cleanup (multi-block source).
///
/// `dst` must point to `schema.width` writable bytes (typically a slot
/// inside an already-allocated parent block). Sub-block allocations are
/// charged to the SHM allocator and the resulting relptrs are written into
/// `dst`.
pub unsafe fn deep_copy(
    src: *const u8,
    dst: *mut u8,
    schema: &Schema,
) -> Result<(), MorlocError> {
    match schema.serial_type {
        SerialType::String => {
            let src_arr = &*(src as *const Array);
            let dst_arr = &mut *(dst as *mut Array);
            dst_arr.size = src_arr.size;
            if src_arr.size > 0 && src_arr.data >= 0 {
                let src_data = shm::rel2abs(src_arr.data)?;
                let new_data = shm::shmemcpy(src_data, src_arr.size)?;
                dst_arr.data = shm::abs2rel(new_data)?;
            } else {
                dst_arr.data = shm::RELNULL;
            }
        }
        SerialType::Array => {
            let src_arr = &*(src as *const Array);
            let dst_arr = &mut *(dst as *mut Array);
            dst_arr.size = src_arr.size;
            if src_arr.size > 0 && src_arr.data >= 0 && !schema.parameters.is_empty() {
                let elem_schema = &schema.parameters[0];
                let elem_width = elem_schema.width;
                let src_data = shm::rel2abs(src_arr.data)?;
                let new_data = shm::shcalloc(src_arr.size, elem_width)?;
                if elem_schema.is_fixed_width() {
                    std::ptr::copy_nonoverlapping(src_data, new_data, src_arr.size * elem_width);
                } else {
                    for i in 0..src_arr.size {
                        deep_copy(
                            src_data.add(i * elem_width),
                            new_data.add(i * elem_width),
                            elem_schema,
                        )?;
                    }
                }
                dst_arr.data = shm::abs2rel(new_data)?;
            } else {
                dst_arr.data = shm::RELNULL;
            }
        }
        SerialType::Tuple | SerialType::Map => {
            for i in 0..schema.parameters.len() {
                let off = schema.offsets[i];
                deep_copy(src.add(off), dst.add(off), &schema.parameters[i])?;
            }
        }
        SerialType::Optional => {
            // The Optional slot is a relptr. Absent → just copy RELNULL.
            // Present → allocate a fresh inner T in SHM, deep-copy into
            // it, and write the new relptr into the destination slot.
            let src_relptr = *(src as *const RelPtr);
            let dst_relptr_slot = dst as *mut RelPtr;
            if src_relptr == shm::RELNULL || schema.parameters.is_empty() {
                *dst_relptr_slot = shm::RELNULL;
            } else {
                let inner_schema = &schema.parameters[0];
                let src_inner = shm::rel2abs(src_relptr)?;
                let dst_inner = shm::shmalloc(inner_schema.width)?;
                std::ptr::write_bytes(dst_inner, 0, inner_schema.width);
                deep_copy(src_inner, dst_inner, inner_schema)?;
                *dst_relptr_slot = shm::abs2rel(dst_inner)?;
            }
        }
        SerialType::Int => {
            // Inline BigInt: [size, value_or_relptr]
            let size = *(src as *const usize);
            *(dst as *mut usize) = size;
            let off = std::mem::size_of::<usize>();
            if size > 1 {
                let src_relptr = *(src.add(off) as *const RelPtr);
                if src_relptr >= 0 {
                    let src_limbs = shm::rel2abs(src_relptr)?;
                    let new_limbs = shm::shmemcpy(src_limbs, size * std::mem::size_of::<u64>())?;
                    *(dst.add(off) as *mut RelPtr) = shm::abs2rel(new_limbs)?;
                } else {
                    *(dst.add(off) as *mut RelPtr) = shm::RELNULL;
                }
            } else {
                *(dst.add(off) as *mut i64) = *(src.add(off) as *const i64);
            }
        }
        SerialType::Table => {
            // Arrow IPC tables are a single relptr to an out-of-line Arrow
            // buffer. Properly deep-copying requires reproducing the buffer,
            // which is not implemented yet.
            return Err(MorlocError::Other(
                "voidstar::deep_copy: Table type not yet supported".into(),
            ));
        }
        _ => {
            // Fixed-size primitives (Bool/Sint*/Uint*/Float*/Nil): bit-copy
            std::ptr::copy_nonoverlapping(src, dst, schema.width);
        }
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
    let mut env: RecurEnv = Vec::new();
    flatten_fixup(&mut buf, 0, data, schema, &mut cursor, &mut env)?;

    Ok(buf)
}

fn flatten_fixup(
    buf: &mut [u8],
    buf_offset: usize,
    data: AbsPtr,
    schema: &Schema,
    cursor: &mut usize,
    env: &mut RecurEnv,
) -> Result<(), MorlocError> {
    recur::with_scope(env, schema, |env| {
        flatten_fixup_inner(buf, buf_offset, data, schema, cursor, env)
    })
}

fn flatten_fixup_inner(
    buf: &mut [u8],
    buf_offset: usize,
    data: AbsPtr,
    schema: &Schema,
    cursor: &mut usize,
    env: &mut RecurEnv,
) -> Result<(), MorlocError> {
    // SAFETY: buf is sized by calc_voidstar_size_inner to hold the entire flattened structure.
    // data points to corresponding SHM data. cursor tracks write position within buf.
    unsafe {
        match schema.serial_type {
            SerialType::Int => {
                // Inline BigInt: [size, value_or_relptr]
                // size ≤ 1: value is inline, nothing to fixup (already copied by parent)
                // size > 1: second field is relptr to limb data, need to copy limbs
                let size = *(data as *const usize);
                if size > 1 {
                    let relptr = *(data.add(std::mem::size_of::<usize>()) as *const RelPtr);
                    let orig_data = shm::rel2abs(relptr)?;
                    let align = std::mem::align_of::<u64>();
                    *cursor = shm::align_up(*cursor, align);
                    // Write new relptr into buffer
                    let buf_relptr = &mut *(buf.as_mut_ptr().add(buf_offset + std::mem::size_of::<usize>()) as *mut RelPtr);
                    *buf_relptr = *cursor as RelPtr;
                    let total_bytes = size * std::mem::size_of::<u64>();
                    buf[*cursor..*cursor + total_bytes].copy_from_slice(
                        std::slice::from_raw_parts(orig_data, total_bytes)
                    );
                    *cursor += total_bytes;
                }
                // size ≤ 1: inline value, no fixup needed
            }
            SerialType::String | SerialType::Array => {
                let orig_arr = &*(data as *const Array);
                let buf_arr = &mut *(buf.as_mut_ptr().add(buf_offset) as *mut Array);
                if orig_arr.size == 0 {
                    buf_arr.data = 0;
                    return Ok(());
                }
                let orig_data = shm::rel2abs(orig_arr.data)?;
                let elem_schema = &schema.parameters[0];
                // String stays at natural element alignment (1 byte for chars);
                // Array bumps to 64 for primitive numeric elements (SIMD/BLAS).
                let align = if schema.serial_type == SerialType::String {
                    elem_schema.alignment()
                } else {
                    elem_schema.array_data_alignment()
                };
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
                            orig_data.add(i * elem_w), elem_schema, cursor, env,
                        )?;
                    }
                }
            }
            SerialType::Tuple | SerialType::Map => {
                for i in 0..schema.parameters.len() {
                    flatten_fixup(
                        buf, buf_offset + schema.offsets[i],
                        data.add(schema.offsets[i]), &schema.parameters[i], cursor, env,
                    )?;
                }
            }
            SerialType::Optional => {
                // The Optional slot is a relptr. RELNULL = absent (already
                // copied into buf as RELNULL). Otherwise follow the relptr
                // in the source SHM to the inner T, copy T's body into buf
                // at the cursor, rewrite the buf-side relptr to be
                // buffer-relative, and recurse to copy any further
                // variable-length data behind T.
                let orig_relptr = *(data as *const RelPtr);
                let buf_relptr = &mut *(buf.as_mut_ptr().add(buf_offset) as *mut RelPtr);
                if orig_relptr == shm::RELNULL {
                    *buf_relptr = shm::RELNULL;
                } else {
                    let inner_schema = &schema.parameters[0];
                    let inner_align = inner_schema.alignment().max(1);
                    *cursor = shm::align_up(*cursor, inner_align);
                    let inner_buf_offset = *cursor;
                    *buf_relptr = inner_buf_offset as RelPtr;
                    let orig_inner = shm::rel2abs(orig_relptr)?;
                    let inner_width = inner_schema.width;
                    buf[inner_buf_offset..inner_buf_offset + inner_width]
                        .copy_from_slice(std::slice::from_raw_parts(orig_inner, inner_width));
                    *cursor += inner_width;
                    flatten_fixup(
                        buf, inner_buf_offset, orig_inner, inner_schema, cursor, env,
                    )?;
                }
            }
            SerialType::Recur => {
                // Resolve and recurse as if the data carried the named
                // target schema. Without this, the variable-length
                // sub-data (children Array.data, optional inner relptrs)
                // would not be copied into the flat buffer, and the
                // resulting inline packet would still reference the
                // original SHM region.
                let name = schema.name.as_deref().unwrap_or("");
                let target_ptr = recur::lookup(env, name)?;
                let target = &*target_ptr;
                flatten_fixup_inner(buf, buf_offset, data, target, cursor, env)?;
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
