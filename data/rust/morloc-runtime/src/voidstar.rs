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

// ── remap_volume_indices ───────────────────────────────────────────────────
//
// Not currently used by any live load path: the production loaders
// (`try_load_voidstar_packet_via_mmap`, `read_voidstar_binary_with_hint`,
// `voidstar::read_binary_with_hint`) all collapse the vol_idx XOR and
// the offset addition into a single ADD walk via `adjust_relptrs`
// with `delta = abs2rel(dest) - encode_relptr(hint, 0)`. This pure-XOR
// variant is exercised by the Layer-3 round-trip unit tests and kept
// in tree as a primitive for any future flow that needs to relocate
// already-loaded voidstar data between volumes without touching offsets.

/// Layer 3 slow path: rewrite the `vol_idx` bits in every relptr of a
/// voidstar blob via XOR with `xor_mask`. Used when a producer encoded
/// relptrs with one vol_idx but the consumer needs a different one
/// (slot collision in the reader's VOLUMES table).
///
/// `xor_mask` should already be shifted into position; callers usually
/// compute it as `((producer_vol_idx ^ consumer_vol_idx) as u64) << 48`.
///
/// When `xor_mask` is 0 this returns immediately without walking --
/// the fast path of Layer 3 (no collision, no rewrite needed) is
/// O(1).
pub fn remap_volume_indices(
    data: AbsPtr,
    schema: &Schema,
    xor_mask: u64,
) -> Result<(), MorlocError> {
    if xor_mask == 0 {
        return Ok(());
    }
    let mut env: RecurEnv = Vec::new();
    remap_volume_indices_with_env(data, schema, xor_mask, &mut env)
}

fn remap_volume_indices_with_env(
    data: AbsPtr,
    schema: &Schema,
    xor_mask: u64,
    env: &mut RecurEnv,
) -> Result<(), MorlocError> {
    recur::with_scope(env, schema, |env| {
        remap_volume_indices_inner(data, schema, xor_mask, env)
    })
}

fn remap_volume_indices_inner(
    data: AbsPtr,
    schema: &Schema,
    xor_mask: u64,
    env: &mut RecurEnv,
) -> Result<(), MorlocError> {
    // SAFETY: data points to a voidstar blob whose layout is described
    // by schema. All pointer arithmetic stays inside the blob.
    unsafe {
        match schema.serial_type {
            SerialType::Int => {
                let size = *(data as *const usize);
                if size > 1 {
                    let p = data.add(std::mem::size_of::<usize>()) as *mut u64;
                    *p ^= xor_mask;
                }
                Ok(())
            }
            SerialType::String | SerialType::Array => {
                let arr = &mut *(data as *mut Array);
                if arr.size == 0 {
                    // Zero-sized arrays carry data = 0 (no vol_mask).
                    return Ok(());
                }
                let saved_arr_data = arr.data;
                let arr_p = &mut arr.data as *mut RelPtr as *mut u64;
                *arr_p ^= xor_mask;
                if !schema.parameters.is_empty() && !schema.parameters[0].is_fixed_width() {
                    let arr_data = shm::rel2abs(arr.data)?;
                    let _ = saved_arr_data;
                    let elem_schema = &schema.parameters[0];
                    let w = elem_schema.width;
                    for i in 0..arr.size {
                        remap_volume_indices_with_env(
                            arr_data.add(i * w), elem_schema, xor_mask, env,
                        )?;
                    }
                }
                Ok(())
            }
            SerialType::Tuple | SerialType::Map => {
                for i in 0..schema.parameters.len() {
                    remap_volume_indices_with_env(
                        data.add(schema.offsets[i]),
                        &schema.parameters[i],
                        xor_mask,
                        env,
                    )?;
                }
                Ok(())
            }
            SerialType::Optional => {
                let relptr_slot = data as *mut RelPtr;
                let v = *relptr_slot;
                if v != shm::RELNULL && !schema.parameters.is_empty() {
                    *(relptr_slot as *mut u64) ^= xor_mask;
                    let inner_abs = shm::rel2abs(*relptr_slot)?;
                    remap_volume_indices_with_env(
                        inner_abs, &schema.parameters[0], xor_mask, env,
                    )?;
                }
                Ok(())
            }
            SerialType::Recur => {
                let name = schema.name.as_deref().unwrap_or("");
                let target_ptr = recur::lookup(env, name)?;
                let target = &*target_ptr;
                remap_volume_indices_with_env(data, target, xor_mask, env)
            }
            _ => Ok(()),
        }
    }
}

// ── read_voidstar_binary ───────────────────────────────────────────────────

/// Read a flat voidstar binary blob into SHM, adjusting relptrs.
/// Assumes the producer wrote buffer-relative (vol_idx=0) relptrs.
/// For Layer-3 emitter output (vol_idx baked into relptr high bits)
/// use `read_binary_with_hint`.
pub fn read_binary(blob: &[u8], schema: &Schema) -> Result<AbsPtr, MorlocError> {
    read_binary_with_hint(blob, schema, 0)
}

/// Read a flat voidstar binary blob into SHM, accounting for a
/// Layer-3 `vol_idx_hint`. `hint = 0` is equivalent to `read_binary`.
pub fn read_binary_with_hint(
    blob: &[u8],
    schema: &Schema,
    vol_idx_hint: u16,
) -> Result<AbsPtr, MorlocError> {
    let base = shm::shmalloc(blob.len())?;
    // SAFETY: base is freshly allocated with blob.len() bytes.
    unsafe { std::ptr::copy_nonoverlapping(blob.as_ptr(), base, blob.len()) };
    let base_rel = shm::abs2rel(base)?;
    let producer_base = shm::encode_relptr(vol_idx_hint as usize, 0);
    let delta = (base_rel as i64).wrapping_sub(producer_base as i64) as shm::RelPtr;
    adjust_relptrs(base, schema, delta)?;
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

// ── write_flat_to_writer (forward-only flatten) ───────────────────────────
//
// Forward-only counterpart to `flatten_to_buffer`. Emits the same byte
// sequence in strict cursor order so the output can be a `zstd::Encoder`
// or any other `Write` sink that can't be back-patched.
pub fn write_flat_to_writer<W: std::io::Write + ?Sized>(
    writer: &mut W,
    data: AbsPtr,
    schema: &Schema,
) -> Result<usize, MorlocError> {
    write_flat_to_writer_with_vol_idx(writer, data, schema, 0)
}

/// Like `write_flat_to_writer` but bakes a 15-bit `vol_idx` into the
/// high bits of every emitted relptr. Layer 3 on-disk output uses this
/// so a Layer-2 reader that mmaps the file and registers it at the same
/// slot can skip the rebase walk entirely. `vol_idx = 0` is identical
/// to the unparameterized `write_flat_to_writer`.
pub fn write_flat_to_writer_with_vol_idx<W: std::io::Write + ?Sized>(
    writer: &mut W,
    data: AbsPtr,
    schema: &Schema,
    vol_idx: u16,
) -> Result<usize, MorlocError> {
    let vol_mask = (vol_idx as u64) << 48;
    let mut state = EmitState { writer, cursor: 0, vol_mask };
    let mut env: RecurEnv = Vec::new();
    let tail_start = schema.width;
    recur::with_scope(&mut env, schema, |env| -> Result<(), MorlocError> {
        emit_slot_inner(&mut state, data, schema, tail_start, env)?;
        emit_tail_inner(&mut state, data, schema, env)?;
        Ok(())
    })?;
    Ok(state.cursor)
}

struct EmitState<'a, W: std::io::Write + ?Sized> {
    writer: &'a mut W,
    cursor: usize,
    /// `(vol_idx as u64) << 48`. ORed into every relptr the writer
    /// emits. Zero for unparameterized output (current default).
    vol_mask: u64,
}

impl<W: std::io::Write + ?Sized> EmitState<'_, W> {
    fn write_bytes(&mut self, bytes: &[u8]) -> Result<(), MorlocError> {
        self.writer.write_all(bytes).map_err(MorlocError::Io)?;
        self.cursor += bytes.len();
        Ok(())
    }

    fn pad_to(&mut self, target: usize) -> Result<(), MorlocError> {
        const PAD: [u8; 64] = [0u8; 64];
        while self.cursor < target {
            let n = (target - self.cursor).min(PAD.len());
            self.writer.write_all(&PAD[..n]).map_err(MorlocError::Io)?;
            self.cursor += n;
        }
        Ok(())
    }
}

fn emit_slot<W: std::io::Write + ?Sized>(
    e: &mut EmitState<'_, W>,
    data: AbsPtr,
    schema: &Schema,
    tail_start: usize,
    env: &mut RecurEnv,
) -> Result<(), MorlocError> {
    recur::with_scope(env, schema, |env| emit_slot_inner(e, data, schema, tail_start, env))
}

fn emit_slot_inner<W: std::io::Write + ?Sized>(
    e: &mut EmitState<'_, W>,
    data: AbsPtr,
    schema: &Schema,
    tail_start: usize,
    env: &mut RecurEnv,
) -> Result<(), MorlocError> {
    // Materialize the whole slot into a stack buffer (heap-fallback for
    // very wide structs), then push it to the writer in one call. Without
    // this, every relptr field inside a struct produces its own tiny
    // Write::write_all dispatch -- which adds up to hundreds of millions
    // of calls for arrays of variable-bearing tuples.
    // Covers every primitive (<= 16) and every realistic tuple/map width
    // in the stdlib. Wider structs fall back to a heap Vec.
    const STACK_SLOT_MAX: usize = 256;
    let width = schema.width;
    let vol_mask = e.vol_mask;
    if width <= STACK_SLOT_MAX {
        let mut buf = [0u8; STACK_SLOT_MAX];
        materialize_slot(&mut buf[..width], data, schema, tail_start, vol_mask, env)?;
        e.write_bytes(&buf[..width])?;
    } else {
        let mut buf = vec![0u8; width];
        materialize_slot(&mut buf, data, schema, tail_start, vol_mask, env)?;
        e.write_bytes(&buf)?;
    }
    Ok(())
}

// Fill `dest` (exactly `schema.width` bytes) with the slot's flattened
// bytes. Variable-bearing relptr fields are computed from `tail_start`
// rather than copied from the source. Primitive fields and structural
// scaffolding are bulk-copied from `data` first, then relptrs are
// overwritten in place. The caller has already pushed `schema`'s
// `with_scope` if applicable. `vol_mask` is ORed into every emitted
// relptr to bake a vol_idx hint into the on-disk form (Layer 3); 0
// means "leave high bits clear" (unparameterized).
fn materialize_slot(
    dest: &mut [u8],
    data: AbsPtr,
    schema: &Schema,
    tail_start: usize,
    vol_mask: u64,
    env: &mut RecurEnv,
) -> Result<(), MorlocError> {
    debug_assert_eq!(dest.len(), schema.width);
    // SAFETY: dest has schema.width bytes; data points to schema.width
    // valid bytes in SHM.
    unsafe {
        std::ptr::copy_nonoverlapping(data, dest.as_mut_ptr(), schema.width);
    }
    patch_slot_inner(dest, data, schema, tail_start, vol_mask, env)
}

fn patch_slot(
    dest: &mut [u8],
    data: AbsPtr,
    schema: &Schema,
    tail_start: usize,
    vol_mask: u64,
    env: &mut RecurEnv,
) -> Result<(), MorlocError> {
    recur::with_scope(env, schema, |env| {
        patch_slot_inner(dest, data, schema, tail_start, vol_mask, env)
    })
}

fn patch_slot_inner(
    dest: &mut [u8],
    data: AbsPtr,
    schema: &Schema,
    tail_start: usize,
    vol_mask: u64,
    env: &mut RecurEnv,
) -> Result<(), MorlocError> {
    unsafe {
        match schema.serial_type {
            SerialType::Int => {
                let size = *(data as *const usize);
                if size > 1 {
                    let limb_pos = shm::align_up(tail_start, std::mem::align_of::<u64>());
                    let baked = (limb_pos as u64) | vol_mask;
                    dest[std::mem::size_of::<usize>()..std::mem::size_of::<usize>() + 8]
                        .copy_from_slice(&baked.to_le_bytes());
                }
            }
            SerialType::String | SerialType::Array => {
                let arr = &*(data as *const Array);
                let new_relptr: u64 = if arr.size == 0 {
                    // Empty arrays use 0 as a sentinel for "no data"
                    // (size==0 short-circuits the rel2abs read). Don't
                    // OR vol_mask so readers can distinguish.
                    0
                } else {
                    let elem_schema = &schema.parameters[0];
                    let align = if schema.serial_type == SerialType::String {
                        elem_schema.alignment()
                    } else {
                        elem_schema.array_data_alignment()
                    };
                    shm::align_up(tail_start, align) as u64 | vol_mask
                };
                dest[8..16].copy_from_slice(&new_relptr.to_le_bytes());
            }
            SerialType::Tuple | SerialType::Map => {
                let mut field_tail_start = tail_start;
                for i in 0..schema.parameters.len() {
                    let field_offset = schema.offsets[i];
                    let field_schema = &schema.parameters[i];
                    let field_data = data.add(field_offset);
                    let field_dest = &mut dest
                        [field_offset..field_offset + field_schema.width];
                    patch_slot(field_dest, field_data, field_schema, field_tail_start, vol_mask, env)?;
                    field_tail_start =
                        tail_end_pos(field_data, field_schema, field_tail_start, env)?;
                }
            }
            SerialType::Optional => {
                let relptr_in = *(data as *const RelPtr);
                let new_value: i64 = if relptr_in == shm::RELNULL {
                    shm::RELNULL as i64
                } else {
                    let inner_schema = &schema.parameters[0];
                    let inner_align = inner_schema.alignment().max(1);
                    (shm::align_up(tail_start, inner_align) as u64 | vol_mask) as i64
                };
                dest[0..8].copy_from_slice(&new_value.to_le_bytes());
            }
            SerialType::Recur => {
                let name = schema.name.as_deref().unwrap_or("");
                let target_ptr = recur::lookup(env, name)?;
                let target = &*target_ptr;
                patch_slot_inner(dest, data, target, tail_start, vol_mask, env)?;
            }
            _ => {}
        }
    }
    Ok(())
}

fn emit_tail<W: std::io::Write + ?Sized>(
    e: &mut EmitState<'_, W>,
    data: AbsPtr,
    schema: &Schema,
    env: &mut RecurEnv,
) -> Result<(), MorlocError> {
    recur::with_scope(env, schema, |env| emit_tail_inner(e, data, schema, env))
}

fn emit_tail_inner<W: std::io::Write + ?Sized>(
    e: &mut EmitState<'_, W>,
    data: AbsPtr,
    schema: &Schema,
    env: &mut RecurEnv,
) -> Result<(), MorlocError> {
    unsafe {
        match schema.serial_type {
            SerialType::Int => {
                let size = *(data as *const usize);
                if size > 1 {
                    let limb_pos = shm::align_up(e.cursor, std::mem::align_of::<u64>());
                    e.pad_to(limb_pos)?;
                    let limb_relptr =
                        *(data.add(std::mem::size_of::<usize>()) as *const RelPtr);
                    let limb_data = shm::rel2abs(limb_relptr)?;
                    let total_bytes = size * std::mem::size_of::<u64>();
                    e.write_bytes(std::slice::from_raw_parts(limb_data, total_bytes))?;
                }
            }
            SerialType::String | SerialType::Array => {
                let arr = &*(data as *const Array);
                if arr.size > 0 {
                    let elem_schema = &schema.parameters[0];
                    let align = if schema.serial_type == SerialType::String {
                        elem_schema.alignment()
                    } else {
                        elem_schema.array_data_alignment()
                    };
                    let elem_pos = shm::align_up(e.cursor, align);
                    e.pad_to(elem_pos)?;
                    let elem_data = shm::rel2abs(arr.data)?;
                    let elem_w = elem_schema.width;

                    if elem_schema.is_fixed_width() {
                        e.write_bytes(std::slice::from_raw_parts(
                            elem_data,
                            arr.size * elem_w,
                        ))?;
                    } else {
                        // TODO: for very large arrays of variable-width elements
                        // this allocates 8 * arr.size bytes. At 20 M elements
                        // that's 160 MB. A two-pass-without-Vec variant would
                        // walk the source twice instead but keep peak memory
                        // bounded by SHM + encoder buffer.
                        let elements_end = elem_pos + arr.size * elem_w;
                        let mut tail_positions: Vec<usize> = Vec::with_capacity(arr.size);
                        let mut cur = elements_end;
                        for i in 0..arr.size {
                            tail_positions.push(cur);
                            cur = tail_end_pos(
                                elem_data.add(i * elem_w),
                                elem_schema,
                                cur,
                                env,
                            )?;
                        }
                        for i in 0..arr.size {
                            let slot_pos = elem_pos + i * elem_w;
                            e.pad_to(slot_pos)?;
                            emit_slot(
                                e,
                                elem_data.add(i * elem_w),
                                elem_schema,
                                tail_positions[i],
                                env,
                            )?;
                        }
                        for i in 0..arr.size {
                            e.pad_to(tail_positions[i])?;
                            emit_tail(
                                e,
                                elem_data.add(i * elem_w),
                                elem_schema,
                                env,
                            )?;
                        }
                    }
                }
            }
            SerialType::Tuple | SerialType::Map => {
                for i in 0..schema.parameters.len() {
                    emit_tail(
                        e,
                        data.add(schema.offsets[i]),
                        &schema.parameters[i],
                        env,
                    )?;
                }
            }
            SerialType::Optional => {
                let relptr_in = *(data as *const RelPtr);
                if relptr_in != shm::RELNULL {
                    let inner_schema = &schema.parameters[0];
                    let inner_align = inner_schema.alignment().max(1);
                    let inner_pos = shm::align_up(e.cursor, inner_align);
                    e.pad_to(inner_pos)?;
                    let inner_data = shm::rel2abs(relptr_in)?;
                    let after_inner_slot = inner_pos + inner_schema.width;
                    emit_slot(e, inner_data, inner_schema, after_inner_slot, env)?;
                    emit_tail(e, inner_data, inner_schema, env)?;
                }
            }
            SerialType::Recur => {
                let name = schema.name.as_deref().unwrap_or("");
                let target_ptr = recur::lookup(env, name)?;
                let target = &*target_ptr;
                emit_tail_inner(e, data, target, env)?;
            }
            _ => {}
        }
    }
    Ok(())
}

// Pure walk: returns the cursor position AFTER emitting the tail
// of `data`/`schema` starting at cursor `start`. Used by `emit_slot`
// to fill in relptr values for variable-bearing fields before the
// tail bytes have been written.
//
// This is essentially `calc_voidstar_size_inner` but tracks actual
// cursor positions (which depend on the real starting offset for
// alignment) instead of returning conservative sizes.
fn tail_end_pos(
    data: AbsPtr,
    schema: &Schema,
    start: usize,
    env: &mut RecurEnv,
) -> Result<usize, MorlocError> {
    recur::with_scope(env, schema, |env| tail_end_pos_inner(data, schema, start, env))
}

fn tail_end_pos_inner(
    data: AbsPtr,
    schema: &Schema,
    start: usize,
    env: &mut RecurEnv,
) -> Result<usize, MorlocError> {
    unsafe {
        match schema.serial_type {
            SerialType::Int => {
                let size = *(data as *const usize);
                if size <= 1 {
                    Ok(start)
                } else {
                    let aligned = shm::align_up(start, std::mem::align_of::<u64>());
                    Ok(aligned + size * std::mem::size_of::<u64>())
                }
            }
            SerialType::String => {
                let arr = &*(data as *const Array);
                if arr.size == 0 {
                    Ok(start)
                } else {
                    let elem_schema = &schema.parameters[0];
                    let elem_pos = shm::align_up(start, elem_schema.alignment());
                    Ok(elem_pos + arr.size * elem_schema.width)
                }
            }
            SerialType::Array => {
                let arr = &*(data as *const Array);
                if arr.size == 0 {
                    return Ok(start);
                }
                let elem_schema = &schema.parameters[0];
                let elem_pos = shm::align_up(start, elem_schema.array_data_alignment());
                let elem_w = elem_schema.width;
                let elements_end = elem_pos + arr.size * elem_w;
                if elem_schema.is_fixed_width() {
                    Ok(elements_end)
                } else {
                    let elem_data = shm::rel2abs(arr.data)?;
                    let mut cur = elements_end;
                    for i in 0..arr.size {
                        cur = tail_end_pos(
                            elem_data.add(i * elem_w),
                            elem_schema,
                            cur,
                            env,
                        )?;
                    }
                    Ok(cur)
                }
            }
            SerialType::Tuple | SerialType::Map => {
                let mut cur = start;
                for i in 0..schema.parameters.len() {
                    cur = tail_end_pos(
                        data.add(schema.offsets[i]),
                        &schema.parameters[i],
                        cur,
                        env,
                    )?;
                }
                Ok(cur)
            }
            SerialType::Optional => {
                let relptr_in = *(data as *const RelPtr);
                if relptr_in == shm::RELNULL {
                    Ok(start)
                } else {
                    let inner_schema = &schema.parameters[0];
                    let inner_align = inner_schema.alignment().max(1);
                    let inner_pos = shm::align_up(start, inner_align);
                    let inner_data = shm::rel2abs(relptr_in)?;
                    let after_inner_slot = inner_pos + inner_schema.width;
                    tail_end_pos(inner_data, inner_schema, after_inner_slot, env)
                }
            }
            SerialType::Recur => {
                let name = schema.name.as_deref().unwrap_or("");
                let target_ptr = recur::lookup(env, name)?;
                let target = &*target_ptr;
                tail_end_pos_inner(data, target, start, env)
            }
            _ => Ok(start),
        }
    }
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

// ── Tests: write_flat_to_writer byte-equivalence with flatten_to_buffer ───

#[cfg(test)]
mod flat_writer_tests {
    use super::*;
    use crate::schema::parse_schema;
    use crate::json::read_json_with_schema;

    fn setup() { crate::init_test_shm(); }

    // Build a voidstar from JSON, then verify both flatteners produce
    // equivalent output:
    //   * `flatten_to_buffer` pre-allocates a buffer sized by the
    //     worst-case `calc_voidstar_size_inner` (which pessimizes
    //     alignment), so its tail may contain stale zero bytes that
    //     no relptr addresses.
    //   * `write_flat_to_writer` emits the exact number of bytes the
    //     algorithm actually visits.
    // The new emitter's bytes must be a byte-identical prefix of the
    // old emitter's, and any old trailing bytes must be zero. Both
    // representations round-trip identically through `read_binary`.
    fn assert_byte_equal(json: &str, schema_str: &str) -> Vec<u8> {
        let schema = parse_schema(schema_str).unwrap();
        let abs = read_json_with_schema(json, &schema).unwrap();

        let buf_old = flatten_to_buffer(abs, &schema).unwrap();
        let mut buf_new: Vec<u8> = Vec::new();
        let n = write_flat_to_writer(&mut buf_new, abs, &schema).unwrap();

        assert_eq!(
            buf_new.len(), n,
            "schema={schema_str} json={json}: returned size ({n}) != bytes written ({})",
            buf_new.len()
        );
        assert!(
            buf_new.len() <= buf_old.len(),
            "schema={schema_str} json={json}: new ({}) is larger than old ({})",
            buf_new.len(), buf_old.len()
        );
        if buf_old[..buf_new.len()] != buf_new[..] {
            let diff = buf_old.iter().zip(buf_new.iter()).position(|(a, b)| a != b);
            panic!(
                "schema={schema_str} json={json}: prefix differs at byte {:?}\n  old[..{}]\n  new[..{}]",
                diff, buf_new.len(), buf_new.len(),
            );
        }
        for (i, &b) in buf_old[buf_new.len()..].iter().enumerate() {
            assert_eq!(
                b, 0,
                "schema={schema_str} json={json}: old trailing byte at offset {} is non-zero (0x{:02x})",
                buf_new.len() + i, b
            );
        }
        buf_new
    }

    #[test]
    fn primitives() {
        setup();
        assert_byte_equal("42", "i4");
        assert_byte_equal("-1", "i8");
        assert_byte_equal("3.14", "f8");
        assert_byte_equal("0.5", "f4");
        assert_byte_equal("true", "b");
        assert_byte_equal("false", "b");
    }

    #[test]
    fn strings() {
        setup();
        assert_byte_equal("\"\"", "s");
        assert_byte_equal("\"hello\"", "s");
        assert_byte_equal("\"a slightly longer string here\"", "s");
    }

    #[test]
    fn array_of_primitive_fixed() {
        setup();
        // Empty array
        assert_byte_equal("[]", "ai4");
        // Small array
        assert_byte_equal("[1,2,3]", "ai4");
        // Larger array: forces SIMD-aligned (64) data region
        assert_byte_equal("[1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]", "af8");
        // Bytes (u8 = 1-byte alignment, no padding)
        assert_byte_equal("[1, 2, 3, 4, 5]", "au1");
    }

    #[test]
    fn array_of_string() {
        setup();
        assert_byte_equal("[\"a\",\"bb\",\"ccc\"]", "as");
        assert_byte_equal("[]", "as");
        assert_byte_equal("[\"\"]", "as");
        // Many strings of varying length: stresses per-element tail accounting
        assert_byte_equal(
            "[\"hello\",\"world\",\"\",\"morloc\",\"x\"]",
            "as",
        );
    }

    #[test]
    fn nested_array() {
        setup();
        assert_byte_equal("[[1,2,3],[4,5],[]]", "aai4");
        assert_byte_equal("[[\"a\",\"b\"],[\"cd\"],[]]", "aas");
    }

    #[test]
    fn tuple_fixed_width() {
        setup();
        assert_byte_equal("[1, 2.5]", "t2i4f8");
        assert_byte_equal("[1, 2, 3]", "t3i4i4i4");
    }

    #[test]
    fn tuple_with_variable_fields() {
        setup();
        assert_byte_equal("[1, \"hello\"]", "t2i4s");
        assert_byte_equal("[\"a\", \"bb\", \"ccc\"]", "t3sss");
        assert_byte_equal("[\"a\", 42, \"bb\"]", "t3si4s");
        // Tuple of arrays: each field has its own variable region
        assert_byte_equal("[[1,2,3], [4,5,6]]", "t2ai4ai4");
        assert_byte_equal("[[1.0,2.0], [\"a\",\"b\"]]", "t2af8as");
    }

    #[test]
    fn array_of_tuple_of_strings() {
        setup();
        // Array of small fixed-arity string tuples.
        assert_byte_equal(
            "[[\"a\",\"b\",\"c\"], [\"dd\",\"ee\",\"ff\"], [\"\",\"\",\"\"]]",
            "at3sss",
        );
    }

    #[test]
    fn optional_some_and_none() {
        setup();
        assert_byte_equal("42", "?i4");
        assert_byte_equal("null", "?i4");
        assert_byte_equal("\"hello\"", "?s");
        assert_byte_equal("null", "?s");
        // Optional of array
        assert_byte_equal("[1,2,3]", "?ai4");
        assert_byte_equal("null", "?ai4");
    }

    #[test]
    fn tuple_with_optional_field() {
        setup();
        assert_byte_equal("[42, \"hi\"]", "t2?i4s");
        assert_byte_equal("[null, \"hi\"]", "t2?i4s");
        assert_byte_equal("[42, null]", "t2?i4?s");
    }

    #[test]
    fn empty_collections() {
        setup();
        assert_byte_equal("[]", "as");
        assert_byte_equal("[]", "aas");
        assert_byte_equal("[[],[],[]]", "aas");
    }

    // Round-trip through read_binary: the bytes emitted by
    // write_flat_to_writer (tighter than flatten_to_buffer's
    // worst-case-padded output) must still be a valid input for
    // the consumer side -- since the consumer just memcpy's into
    // SHM and walks relptrs, the shorter buffer is structurally
    // equivalent.
    fn assert_roundtrip(json: &str, schema_str: &str) {
        use crate::json::voidstar_to_json_string;
        let schema = parse_schema(schema_str).unwrap();
        let original_abs = read_json_with_schema(json, &schema).unwrap();
        let original_json = voidstar_to_json_string(original_abs, &schema).unwrap();

        let mut buf: Vec<u8> = Vec::new();
        write_flat_to_writer(&mut buf, original_abs, &schema).unwrap();

        let recovered_abs = read_binary(&buf, &schema).unwrap();
        let recovered_json = voidstar_to_json_string(recovered_abs, &schema).unwrap();

        assert_eq!(
            original_json, recovered_json,
            "roundtrip mismatch for schema={schema_str} json={json}"
        );
    }

    #[test]
    fn roundtrip_through_read_binary() {
        setup();
        assert_roundtrip("42", "i4");
        assert_roundtrip("3.14", "f8");
        assert_roundtrip("\"hello\"", "s");
        assert_roundtrip("[1,2,3]", "ai4");
        assert_roundtrip("[1.0, 2.0, 3.0]", "af8");
        assert_roundtrip("[\"a\",\"bb\",\"ccc\"]", "as");
        assert_roundtrip("[[1,2,3],[4,5],[]]", "aai4");
        assert_roundtrip("[1, \"hello\"]", "t2i4s");
        assert_roundtrip("[[1,2,3],[4,5,6]]", "t2ai4ai4");
        assert_roundtrip(
            "[[\"a\",\"b\",\"c\"], [\"dd\",\"ee\",\"ff\"]]",
            "at3sss",
        );
        assert_roundtrip("42", "?i4");
        assert_roundtrip("null", "?i4");
        assert_roundtrip("[42, \"hi\"]", "t2?i4s");
        assert_roundtrip("[null, \"hi\"]", "t2?i4s");
    }

    // `remap_volume_indices` recurses via `rel2abs` between XOR
    // operations, which means the walked relptrs must resolve to
    // mapped memory at every intermediate step. A "build-here, XOR
    // away, XOR back" round-trip therefore cannot work without
    // somewhere to point the corrupted intermediate values -- exactly
    // the gap `register_external_volume` used to fill. The function
    // stays in tree for future flows that need it (e.g. genuine
    // volume defragmentation), but it can only be tested at the
    // structural-walk level here.

    #[test]
    fn remap_volume_indices_zero_mask_is_noop() {
        // The fast-path short-circuit: an empty xor_mask returns
        // immediately without walking. Compose two writes (producer and
        // consumer both 0) so the bytes are identical before and after.
        setup();
        let schema = parse_schema("at3sss").unwrap();
        let original = read_json_with_schema(
            "[[\"a\",\"b\",\"c\"], [\"dd\",\"ee\",\"ff\"]]", &schema,
        ).unwrap();
        let mut buf: Vec<u8> = Vec::new();
        write_flat_to_writer_with_vol_idx(&mut buf, original, &schema, 7).unwrap();

        let dest = shm::shmalloc(buf.len()).unwrap();
        unsafe { std::ptr::copy_nonoverlapping(buf.as_ptr(), dest, buf.len()) };
        let before: Vec<u8> = unsafe {
            std::slice::from_raw_parts(dest, buf.len()).to_vec()
        };
        // Mask of zero must not modify a single byte.
        remap_volume_indices(dest, &schema, 0).unwrap();
        let after: Vec<u8> = unsafe {
            std::slice::from_raw_parts(dest, buf.len()).to_vec()
        };
        assert_eq!(before, after, "xor_mask=0 must be byte-level no-op");
        shm::shfree(dest).unwrap();
    }

    /// Bug-1 regression: a buffer produced by the Layer-3 emitter
    /// (`write_flat_to_writer_with_vol_idx` with hint > 0) MUST round-trip
    /// cleanly through `read_binary_with_hint`. Before the fix, the
    /// legacy reader called `adjust_relptrs(base, schema, abs2rel(base))`
    /// without subtracting `encode_relptr(hint, 0)`, so the producer's
    /// `hint << 48` bits added to the consumer's slot bits and the
    /// resulting vol_idx was nonsense. The JSON read-back would either
    /// segfault or silently produce garbage.
    fn assert_hint_aware_read_binary(json: &str, schema_str: &str, hint: u16) {
        use crate::json::voidstar_to_json_string;
        let schema = parse_schema(schema_str).unwrap();
        let original_abs = read_json_with_schema(json, &schema).unwrap();
        let original_json = voidstar_to_json_string(original_abs, &schema).unwrap();

        let mut buf: Vec<u8> = Vec::new();
        write_flat_to_writer_with_vol_idx(&mut buf, original_abs, &schema, hint).unwrap();

        let recovered_abs = read_binary_with_hint(&buf, &schema, hint).unwrap();
        let recovered_json = voidstar_to_json_string(recovered_abs, &schema).unwrap();
        assert_eq!(
            original_json, recovered_json,
            "read_binary_with_hint round-trip mismatch \
             for schema={schema_str} json={json} hint={hint}"
        );
    }

    #[test]
    fn read_binary_with_hint_handles_layer3_emitter() {
        setup();
        for &hint in &[0u16, 1, 17, 4242, 32767] {
            assert_hint_aware_read_binary("\"hello\"", "s", hint);
            assert_hint_aware_read_binary("[1,2,3]", "ai4", hint);
            assert_hint_aware_read_binary("[\"a\",\"bb\",\"ccc\"]", "as", hint);
            assert_hint_aware_read_binary("[1, \"hello\"]", "t2i4s", hint);
            assert_hint_aware_read_binary("[null, \"hi\"]", "t2?i4s", hint);
            assert_hint_aware_read_binary("[[1,2,3],[4,5],[]]", "aai4", hint);
        }
    }

    /// Tier-A: the read_binary path with hint=0 must reproduce the
    /// pre-Layer-3 behavior exactly. Catches regression in the legacy
    /// reader's wrapper.
    #[test]
    fn read_binary_no_hint_is_unchanged() {
        setup();
        let schema = parse_schema("at3sss").unwrap();
        let original = read_json_with_schema(
            "[[\"a\",\"b\",\"c\"], [\"dd\",\"ee\",\"ff\"]]", &schema,
        ).unwrap();
        let mut buf: Vec<u8> = Vec::new();
        write_flat_to_writer(&mut buf, original, &schema).unwrap();
        // Both APIs must give the same answer when hint is 0.
        let a = read_binary(&buf, &schema).unwrap();
        let b = read_binary_with_hint(&buf, &schema, 0).unwrap();
        use crate::json::voidstar_to_json_string;
        assert_eq!(
            voidstar_to_json_string(a, &schema).unwrap(),
            voidstar_to_json_string(b, &schema).unwrap(),
        );
    }
}
