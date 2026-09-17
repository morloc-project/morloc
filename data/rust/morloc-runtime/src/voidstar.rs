//! Shared voidstar operations: relptr adjustment, binary serialization,
//! schema-aware free, and flatten-to-buffer.
//!
//! These functions operate on the morloc voidstar binary format in SHM.
//! They are used by packet.rs, cli.rs, and json.rs.

use crate::error::MorlocError;
use crate::recur::Resolver;
use crate::schema::{Schema, SerialType};
use crate::shm::{self, AbsPtr, Array, RelPtr};
use crate::walk::{self, Frame, Stack, Visit, Walker};

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
/// Byte offset of a Variant's payload pointer within its 16-byte slot.
/// The tag occupies byte 0; bytes 1..8 are padding kept for a future
/// tag-version discriminator, mirroring the stream-handle union.
const VARIANT_PAYLOAD_OFFSET: usize = 8;

/// The schema of the arm a variant value currently holds.
///
/// Unlike Optional, whose pointee always has the single child schema, a
/// variant's payload type is chosen by the tag. Every walk over a variant
/// therefore has to read the value and not just the schema -- which is also
/// where a tag from a mismatched producer is caught.
fn variant_arm_schema(tag: u8, schema: &Schema) -> Result<&Schema, MorlocError> {
    schema.parameters.get(tag as usize).ok_or_else(|| {
        MorlocError::Serialization(format!(
            "variant tag {} is out of range; the type has {} arms",
            tag, schema.size
        ))
    })
}

pub fn adjust_relptrs(
    data: AbsPtr,
    schema: &Schema,
    base_rel: RelPtr,
) -> Result<(), MorlocError> {
    let mut w = RebaseWalk { res: Resolver::new(schema), mode: Rebase::Shm(base_rel) };
    let mut st = Stack::new();
    st.enter(schema, data, ());
    walk::run(&mut w, &mut st)
}

// ── shift_buffer_relptrs ───────────────────────────────────────────────────

/// Add `delta` to every relptr slot inside a self-contained voidstar
/// buffer, without dereferencing through `rel2abs`. The buffer's
/// relptrs are pure offsets from `buf_base` (i.e. `vol_idx = 0` in the
/// encoded form); an intermediate `rel2abs` on them would land in the
/// primary SHM volume rather than in the buffer, so any descent must
/// use buffer-local pointer arithmetic (`buf_base + offset`).
///
/// Use this instead of `adjust_relptrs` when the target of the shift
/// is not a fresh SHM allocation but an in-place move within a Vec /
/// per-slot write buffer -- e.g. the write-buffer compaction step and
/// the per-element blob relocation inside `append_one_element`.
pub unsafe fn shift_buffer_relptrs(
    buf_base: *mut u8,
    field_offset: usize,
    schema: &Schema,
    delta: isize,
) -> Result<(), MorlocError> {
    let mut w = RebaseWalk { res: Resolver::new(schema), mode: Rebase::Buffer { buf_base, delta } };
    let mut st = Stack::new();
    st.enter(schema, buf_base.add(field_offset), ());
    walk::run(&mut w, &mut st)
}

/// How a rebasing walk reaches the block a relptr names once it has been
/// rebased: through the SHM volume table, or by offset from a buffer.
enum Rebase {
    Shm(RelPtr),
    Buffer { buf_base: *mut u8, delta: isize },
}

/// Adds a constant to every relptr under a value, in place, descending
/// through each rebased pointer so the blocks it names are rebased too.
struct RebaseWalk<'r> {
    res: Resolver<'r>,
    mode: Rebase,
}

impl<'r> RebaseWalk<'r> {
    #[inline]
    fn shift(&self) -> RelPtr {
        match self.mode {
            Rebase::Shm(b) => b,
            Rebase::Buffer { delta, .. } => delta,
        }
    }

    /// The block a rebased relptr names.
    #[inline]
    fn target(&self, rel: RelPtr) -> Result<*mut u8, MorlocError> {
        match self.mode {
            Rebase::Shm(_) => shm::rel2abs(rel),
            // SAFETY: a buffer-local relptr is an offset into the buffer.
            Rebase::Buffer { buf_base, .. } => Ok(unsafe { buf_base.add(rel as usize) }),
        }
    }

    fn child(
        &mut self,
        st: &mut Stack<()>,
        f: &Frame<()>,
        idx: usize,
        s: &'r Schema,
        data: *const u8,
    ) -> Result<Visit, MorlocError> {
        if self.res.flat(s) {
            self.step(st, Frame::new(s, data, ()))?;
            Ok(Visit::Done)
        } else {
            walk::defer(self, st, f, idx, s, data, ());
            Ok(Visit::Deferred)
        }
    }
}

impl<'r> Walker<()> for RebaseWalk<'r> {
    fn step(&mut self, st: &mut Stack<()>, f: Frame<()>) -> Result<(), MorlocError> {
        // SAFETY: frames hold nodes of the tree the resolver was built from,
        // and `data` points at a value laid out as that schema describes;
        // every write stays inside the node's own slot.
        let s: &'r Schema = self.res.resolve(unsafe { &*f.schema })?;
        let data = f.data as *mut u8;
        let shift = self.shift();
        unsafe {
            match s.serial_type {
                SerialType::Int => {
                    // Inline BigInt: [size, value_or_relptr]. Only a limb
                    // pointer needs rebasing; the limbs themselves hold none.
                    let size = *(data as *const usize);
                    if size > 1 {
                        let relptr = &mut *(data.add(std::mem::size_of::<usize>()) as *mut RelPtr);
                        *relptr += shift;
                    }
                }
                SerialType::String | SerialType::Array => {
                    let arr = &mut *(data as *mut Array);
                    if f.idx == 0 {
                        // A buffer-local walk leaves an empty or null data
                        // pointer alone; the SHM walk rebases it regardless.
                        if matches!(self.mode, Rebase::Buffer { .. }) && (arr.size == 0 || arr.data <= 0) {
                            return Ok(());
                        }
                        arr.data += shift;
                    }
                    // An empty array points nowhere (its rebased sentinel is
                    // never read); a flat element region holds no pointers.
                    if arr.size == 0 || s.parameters.is_empty() || s.parameters[0].is_fixed_width() {
                        return Ok(());
                    }
                    let elem_schema = &s.parameters[0];
                    let w = elem_schema.width;
                    let elems = self.target(arr.data)?;
                    let flat_elem = self.res.flat(elem_schema);
                    for i in f.idx..arr.size {
                        let p = elems.add(i * w);
                        if flat_elem {
                            self.step(st, Frame::new(elem_schema, p, ()))?;
                        } else if self.child(st, &f, i, elem_schema, p)? == Visit::Deferred {
                            return Ok(());
                        }
                    }
                }
                SerialType::IFile | SerialType::OStream | SerialType::IStream => {
                    // Stream-handle tagged field. Only TAG_PATH payloads carry
                    // a relptr that needs rebasing; TAG_HANDLE's payload is an
                    // intra-nexus slot id with no offset semantics. RELNULL
                    // payloads (empty path) stay unchanged.
                    use morloc_runtime_types::stream_handle as sh;
                    if sh::read_tag(data) == sh::TAG_PATH {
                        let payload = sh::read_payload(data);
                        if payload != sh::RELNULL_PAYLOAD {
                            sh::write_field(data, sh::TAG_PATH, payload.wrapping_add(shift as u64));
                        }
                    }
                }
                SerialType::Tuple | SerialType::Map => {
                    for i in f.idx..s.parameters.len() {
                        let p = data.add(s.offsets[i]);
                        if self.child(st, &f, i, &s.parameters[i], p)? == Visit::Deferred {
                            return Ok(());
                        }
                    }
                }
                SerialType::Variant => {
                    // Tag byte, then a relptr to the arm's payload tuple.
                    // Rebase the pointer, then descend so the payload's own
                    // pointers are rebased too.
                    if f.idx > 0 {
                        return Ok(());
                    }
                    let arm = variant_arm_schema(*data, s)?;
                    let relptr_slot = &mut *(data.add(VARIANT_PAYLOAD_OFFSET) as *mut RelPtr);
                    if *relptr_slot != shm::RELNULL {
                        *relptr_slot += shift;
                        let inner = self.target(*relptr_slot)?;
                        self.child(st, &f, 0, arm, inner)?;
                    }
                }
                SerialType::Optional => {
                    // The Optional slot is a relptr: RELNULL = absent, else a
                    // pointer that is rebased and followed so the inner T's
                    // own pointers are rebased too.
                    if f.idx > 0 || s.parameters.is_empty() {
                        return Ok(());
                    }
                    let relptr_slot = &mut *(data as *mut RelPtr);
                    if *relptr_slot != shm::RELNULL {
                        *relptr_slot += shift;
                        let inner = self.target(*relptr_slot)?;
                        self.child(st, &f, 0, &s.parameters[0], inner)?;
                    }
                }
                _ => {} // primitives have no relptrs
            }
        }
        Ok(())
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
    // Default source resolver: standard SHM rel2abs. Callers reading
    // from file-backed regions instead use `deep_copy_with` with a
    // custom resolver that adds an offset to the file's payload base.
    deep_copy_with(src, dst, schema, &|p| shm::rel2abs(p))
}

/// Same as `deep_copy` but parameterised by the source-side relptr
/// resolver. The destination side always uses the SHM allocator and
/// `shm::rel2abs` for the destination's own sub-block relptrs; only
/// the source relptr -> AbsPtr resolution is customisable.
///
/// `resolve` is called whenever the walker needs to follow a relptr
/// embedded in the *source* data: Array.data, Optional inners, BigInt
/// limbs, etc. The returned `AbsPtr` must be a readable byte pointer
/// to the resolved data; the walker reads `size_bytes` from it where
/// `size_bytes` is determined by the schema.
///
/// For mmap'd file-backed source, the resolver decodes the file-
/// relative offset and returns `payload_base + offset` (with bounds
/// checking against the payload region). For SHM-resident source,
/// the resolver is `shm::rel2abs`.
pub unsafe fn deep_copy_with<R>(
    src: *const u8,
    dst: *mut u8,
    schema: &Schema,
    resolve: &R,
) -> Result<(), MorlocError>
where
    R: Fn(RelPtr) -> Result<crate::shm::AbsPtr, MorlocError>,
{
    let mut w = CopyWalk { res: Resolver::new(schema), resolve };
    let mut st = Stack::new();
    st.enter(schema, src, dst);
    walk::run(&mut w, &mut st)
}

/// Copies a value's blocks into fresh SHM allocations, one per block, so
/// the copy can be released block by block. A frame's `data` is the
/// source slot and its `x` the destination slot; a block for a pointer
/// field is allocated and pointed at before the walk descends into it.
struct CopyWalk<'r, 'f, R> {
    res: Resolver<'r>,
    resolve: &'f R,
}

impl<'r, 'f, R> CopyWalk<'r, 'f, R>
where
    R: Fn(RelPtr) -> Result<crate::shm::AbsPtr, MorlocError>,
{
    fn child(
        &mut self,
        st: &mut Stack<*mut u8>,
        f: &Frame<*mut u8>,
        idx: usize,
        s: &'r Schema,
        src: *const u8,
        dst: *mut u8,
    ) -> Result<Visit, MorlocError> {
        if self.res.flat(s) {
            self.step(st, Frame::new(s, src, dst))?;
            Ok(Visit::Done)
        } else {
            walk::defer(self, st, f, idx, s, src, dst);
            Ok(Visit::Deferred)
        }
    }
}

impl<'r, 'f, R> Walker<*mut u8> for CopyWalk<'r, 'f, R>
where
    R: Fn(RelPtr) -> Result<crate::shm::AbsPtr, MorlocError>,
{
    fn step(&mut self, st: &mut Stack<*mut u8>, f: Frame<*mut u8>) -> Result<(), MorlocError> {
        // SAFETY: frames hold nodes of the tree the resolver was built from;
        // `data` points at a source value laid out as that schema describes
        // and `x` at a destination slot of the same width.
        let schema: &'r Schema = self.res.resolve(unsafe { &*f.schema })?;
        let src = f.data;
        let dst = f.x;
        let resolve = self.resolve;
        unsafe {
            match schema.serial_type {
                SerialType::String => {
                    let src_arr = &*(src as *const Array);
                    let dst_arr = &mut *(dst as *mut Array);
                    dst_arr.size = src_arr.size;
                    if src_arr.size > 0 && src_arr.data >= 0 {
                        let src_data = resolve(src_arr.data)?;
                        let new_data = shm::shmemcpy(src_data, src_arr.size)?;
                        dst_arr.data = shm::abs2rel(new_data)?;
                    } else {
                        dst_arr.data = shm::RELNULL;
                    }
                }
                SerialType::IFile | SerialType::OStream | SerialType::IStream => {
                    // Stream-handle tagged field. TAG_PATH copies the suballoc
                    // (`{size: u64, bytes...}`); TAG_HANDLE has no suballoc.
                    use morloc_runtime_types::stream_handle as sh;
                    let tag = sh::read_tag(src);
                    let src_payload = sh::read_payload(src);
                    match tag {
                        t if t == sh::TAG_PATH => {
                            if src_payload == sh::RELNULL_PAYLOAD {
                                sh::write_field(dst, sh::TAG_PATH, sh::RELNULL_PAYLOAD);
                            } else {
                                let src_suballoc = resolve(src_payload as RelPtr)?;
                                let path_len = sh::read_path_size(src_suballoc) as usize;
                                let total = sh::path_suballoc_size(path_len);
                                let new_suballoc = shm::shmemcpy(src_suballoc, total)?;
                                sh::write_field(dst, sh::TAG_PATH, shm::abs2rel(new_suballoc)? as u64);
                            }
                        }
                        t if t == sh::TAG_HANDLE => {
                            sh::write_field(dst, sh::TAG_HANDLE, src_payload);
                        }
                        t => {
                            return Err(MorlocError::Other(format!(
                                "deep_copy: unsupported stream-handle tag {}", t,
                            )));
                        }
                    }
                }
                SerialType::Array => {
                    let src_arr = &*(src as *const Array);
                    let dst_arr = &mut *(dst as *mut Array);
                    if f.idx == 0 {
                        dst_arr.size = src_arr.size;
                        if !(src_arr.size > 0 && src_arr.data >= 0 && !schema.parameters.is_empty()) {
                            dst_arr.data = shm::RELNULL;
                            return Ok(());
                        }
                        let elem_schema = &schema.parameters[0];
                        let elem_width = elem_schema.width;
                        let src_data = resolve(src_arr.data)?;
                        let new_data = shm::shcalloc(src_arr.size, elem_width)?;
                        dst_arr.data = shm::abs2rel(new_data)?;
                        if elem_schema.is_fixed_width() {
                            std::ptr::copy_nonoverlapping(src_data, new_data, src_arr.size * elem_width);
                            return Ok(());
                        }
                    }
                    let elem_schema = &schema.parameters[0];
                    if elem_schema.is_fixed_width() {
                        return Ok(());
                    }
                    let elem_width = elem_schema.width;
                    let src_data = resolve(src_arr.data)?;
                    let new_data = shm::rel2abs(dst_arr.data)?;
                    let flat_elem = self.res.flat(elem_schema);
                    for i in f.idx..src_arr.size {
                        let (sp, dp) = (src_data.add(i * elem_width), new_data.add(i * elem_width));
                        if flat_elem {
                            self.step(st, Frame::new(elem_schema, sp, dp))?;
                        } else if self.child(st, &f, i, elem_schema, sp, dp)? == Visit::Deferred {
                            return Ok(());
                        }
                    }
                }
                SerialType::Tuple | SerialType::Map => {
                    for i in f.idx..schema.parameters.len() {
                        let off = schema.offsets[i];
                        if self.child(st, &f, i, &schema.parameters[i], src.add(off), dst.add(off))? == Visit::Deferred {
                            return Ok(());
                        }
                    }
                }
                SerialType::Variant => {
                    if f.idx > 0 {
                        return Ok(());
                    }
                    // Carry the tag across, then deep-copy the payload the
                    // tag selects into a fresh allocation. The bytes between
                    // tag and payload pointer are determined, as at every
                    // other site that writes a variant slot.
                    let tag = *src;
                    *dst = tag;
                    std::ptr::write_bytes(dst.add(1), 0, VARIANT_PAYLOAD_OFFSET - 1);
                    let arm = variant_arm_schema(tag, schema)?;
                    let src_relptr = *(src.add(VARIANT_PAYLOAD_OFFSET) as *const RelPtr);
                    let dst_relptr_slot = dst.add(VARIANT_PAYLOAD_OFFSET) as *mut RelPtr;
                    if src_relptr == shm::RELNULL {
                        *dst_relptr_slot = shm::RELNULL;
                    } else {
                        let src_inner = resolve(src_relptr)?;
                        let dst_inner = shm::shmalloc(arm.width)?;
                        std::ptr::write_bytes(dst_inner, 0, arm.width);
                        *dst_relptr_slot = shm::abs2rel(dst_inner)?;
                        self.child(st, &f, 0, arm, src_inner, dst_inner)?;
                    }
                }
                SerialType::Optional => {
                    if f.idx > 0 {
                        return Ok(());
                    }
                    // Absent: RELNULL. Present: a fresh inner T in SHM,
                    // pointed at from the destination slot, then copied.
                    let src_relptr = *(src as *const RelPtr);
                    let dst_relptr_slot = dst as *mut RelPtr;
                    if src_relptr == shm::RELNULL || schema.parameters.is_empty() {
                        *dst_relptr_slot = shm::RELNULL;
                    } else {
                        let inner_schema = &schema.parameters[0];
                        let src_inner = resolve(src_relptr)?;
                        let dst_inner = shm::shmalloc(inner_schema.width)?;
                        std::ptr::write_bytes(dst_inner, 0, inner_schema.width);
                        *dst_relptr_slot = shm::abs2rel(dst_inner)?;
                        self.child(st, &f, 0, inner_schema, src_inner, dst_inner)?;
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
                            let src_limbs = resolve(src_relptr)?;
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
                    // A table's value is a whole block rather than a slot of
                    // `width` bytes, so it has no place inside another value.
                    return Err(MorlocError::Other(
                        "voidstar::deep_copy: a table cannot be copied into a slot".into(),
                    ));
                }
                _ => {
                    // Fixed-size primitives (Bool/Sint*/Uint*/Float*/Nil): bit-copy
                    std::ptr::copy_nonoverlapping(src, dst, schema.width);
                }
            }
        }
        Ok(())
    }
}

// ── flatten_voidstar_to_buffer ─────────────────────────────────────────────

/// Flatten a voidstar structure in SHM into a self-contained byte buffer.
/// Relptrs in the output are offsets from position 0 of the buffer.
pub fn flatten_to_buffer(data: AbsPtr, schema: &Schema) -> Result<Vec<u8>, MorlocError> {
    let mut buf = Vec::new();
    flatten_into(&mut buf, data, schema)?;
    Ok(buf)
}

/// Like [`flatten_to_buffer`] but writes into a caller-provided buffer,
/// reusing its allocation across calls so hot loops that flatten many values
/// (e.g. the per-element `@write` append) pay no heap allocation per value.
/// `buf` is reset to a zero-filled `total` bytes (the trailing padding relies
/// on the zeros), discarding any prior contents.
pub fn flatten_into(
    buf: &mut Vec<u8>,
    data: AbsPtr,
    schema: &Schema,
) -> Result<(), MorlocError> {
    let total = crate::ffi::calc_voidstar_size_inner(data, schema)?;
    buf.clear();
    buf.resize(total, 0);

    if schema.serial_type == SerialType::Table {
        // A table's flat form is its block, already self-contained: every
        // offset inside it is relative to the block start.
        // SAFETY: `total` is the block's checked size.
        unsafe { std::ptr::copy_nonoverlapping(data, buf.as_mut_ptr(), total) };
        return Ok(());
    }

    // SAFETY: data points to at least schema.width bytes in SHM; buf has total >= schema.width bytes.
    unsafe { std::ptr::copy_nonoverlapping(data, buf.as_mut_ptr(), schema.width) };

    // Phase 2: fix up relptrs and copy variable-length data
    let mut w = FlattenWalk { res: Resolver::new(schema), buf: buf.as_mut_ptr(), len: buf.len(), cursor: schema.width };
    let mut st = Stack::new();
    st.enter(schema, data, 0);
    walk::run(&mut w, &mut st)?;

    Ok(())
}

/// Copies a value's variable-length blocks into a flat buffer behind the
/// slots the parent already copied, rewriting each pointer to the
/// buffer-relative offset of the block. A frame's `x` is the offset of the
/// node's slot in the buffer.
struct FlattenWalk<'r> {
    res: Resolver<'r>,
    buf: *mut u8,
    len: usize,
    cursor: usize,
}

impl<'r> FlattenWalk<'r> {
    /// The buffer region `[at, at + n)`, checked against the buffer's size,
    /// which the size walk chose to hold the whole value.
    fn region(&mut self, at: usize, n: usize) -> Result<&mut [u8], MorlocError> {
        if at.checked_add(n).map_or(true, |end| end > self.len) {
            return Err(MorlocError::Other("flatten: value larger than its computed size".into()));
        }
        // SAFETY: the range is inside the buffer.
        Ok(unsafe { std::slice::from_raw_parts_mut(self.buf.add(at), n) })
    }

    fn child(
        &mut self,
        st: &mut Stack<usize>,
        f: &Frame<usize>,
        idx: usize,
        s: &'r Schema,
        data: *const u8,
        at: usize,
    ) -> Result<Visit, MorlocError> {
        if self.res.flat(s) {
            self.step(st, Frame::new(s, data, at))?;
            Ok(Visit::Done)
        } else {
            walk::defer(self, st, f, idx, s, data, at);
            Ok(Visit::Deferred)
        }
    }
}

impl<'r> Walker<usize> for FlattenWalk<'r> {
    fn step(&mut self, st: &mut Stack<usize>, f: Frame<usize>) -> Result<(), MorlocError> {
        // SAFETY: frames hold nodes of the tree the resolver was built from;
        // `data` points at the SHM value the schema describes, and every
        // buffer write goes through `region`.
        let s: &'r Schema = self.res.resolve(unsafe { &*f.schema })?;
        let data = f.data;
        let at = f.x;
        unsafe {
            match s.serial_type {
                SerialType::Int => {
                    // Inline BigInt: [size, value_or_relptr]. An inline
                    // value was copied by the parent; limbs are copied here
                    // and the pointer rewritten.
                    let size = *(data as *const usize);
                    if size > 1 {
                        let relptr = *(data.add(std::mem::size_of::<usize>()) as *const RelPtr);
                        let limbs = shm::rel2abs(relptr)?;
                        self.cursor = shm::align_up(self.cursor, std::mem::align_of::<u64>());
                        let total = size * std::mem::size_of::<u64>();
                        let here = self.cursor;
                        self.region(here, total)?.copy_from_slice(std::slice::from_raw_parts(limbs, total));
                        self.region(at + std::mem::size_of::<usize>(), std::mem::size_of::<RelPtr>())?
                            .copy_from_slice(&(here as RelPtr).to_ne_bytes());
                        self.cursor += total;
                    }
                }
                SerialType::String | SerialType::Array => {
                    let orig = &*(data as *const Array);
                    let elem_schema = &s.parameters[0];
                    let elem_w = elem_schema.width;
                    if f.idx == 0 {
                        if orig.size == 0 {
                            std::ptr::write_unaligned(
                                self.region(at, std::mem::size_of::<Array>())?.as_mut_ptr() as *mut Array,
                                Array { size: 0, data: 0 },
                            );
                            return Ok(());
                        }
                        let src = shm::rel2abs(orig.data)?;
                        // A string stays at natural element alignment (1 byte
                        // for chars); an array bumps to 64 for primitive
                        // numeric elements (SIMD/BLAS).
                        let align = if matches!(s.serial_type, SerialType::String) {
                            elem_schema.alignment()
                        } else {
                            elem_schema.array_data_alignment()
                        };
                        self.cursor = shm::align_up(self.cursor, align);
                        // Checked: a corrupt or hostile size must not wrap the
                        // product into a short copy.
                        let total = elem_w
                            .checked_mul(orig.size)
                            .ok_or_else(|| MorlocError::Other("flatten: array data size overflow".into()))?;
                        let here = self.cursor;
                        self.region(here, total)?.copy_from_slice(std::slice::from_raw_parts(src, total));
                        std::ptr::write_unaligned(
                            self.region(at, std::mem::size_of::<Array>())?.as_mut_ptr() as *mut Array,
                            Array { size: orig.size, data: here as RelPtr },
                        );
                        self.cursor += total;
                    }
                    if elem_schema.is_fixed_width() {
                        return Ok(());
                    }
                    // The element region's offset was written into the
                    // buffer-side header above.
                    let hdr = &*(self.buf.add(at) as *const Array);
                    let elem_start = hdr.data as usize;
                    let src = shm::rel2abs(orig.data)?;
                    let flat_elem = self.res.flat(elem_schema);
                    for i in f.idx..orig.size {
                        let p = src.add(i * elem_w);
                        let slot = elem_start + i * elem_w;
                        if flat_elem {
                            self.step(st, Frame::new(elem_schema, p, slot))?;
                        } else if self.child(st, &f, i, elem_schema, p, slot)? == Visit::Deferred {
                            return Ok(());
                        }
                    }
                }
                SerialType::IFile | SerialType::OStream | SerialType::IStream => {
                    // Tagged stream-handle field: TAG_PATH copies the
                    // `{size, bytes}` suballoc into the buffer; TAG_HANDLE
                    // has no suballoc.
                    use morloc_runtime_types::stream_handle as sh;
                    let tag = sh::read_tag(data);
                    let src_payload = sh::read_payload(data);
                    let dst_field = self.region(at, sh::STREAM_HANDLE_FIELD_SIZE)?.as_mut_ptr();
                    match tag {
                        t if t == sh::TAG_PATH => {
                            if src_payload == sh::RELNULL_PAYLOAD {
                                sh::write_field(dst_field, sh::TAG_PATH, sh::RELNULL_PAYLOAD);
                            } else {
                                let src_suballoc = shm::rel2abs(src_payload as RelPtr)?;
                                let path_len = sh::read_path_size(src_suballoc) as usize;
                                let total = sh::path_suballoc_size(path_len);
                                self.cursor = shm::align_up(self.cursor, 8);
                                let here = self.cursor;
                                self.region(here, total)?
                                    .copy_from_slice(std::slice::from_raw_parts(src_suballoc, total));
                                self.cursor += total;
                                sh::write_field(dst_field, sh::TAG_PATH, here as u64);
                            }
                        }
                        t if t == sh::TAG_HANDLE => {
                            sh::write_field(dst_field, sh::TAG_HANDLE, src_payload);
                        }
                        t => {
                            return Err(MorlocError::Other(format!(
                                "flatten_fixup: unsupported stream-handle tag {}", t,
                            )));
                        }
                    }
                }
                SerialType::Tuple | SerialType::Map => {
                    for i in f.idx..s.parameters.len() {
                        let off = s.offsets[i];
                        if self.child(st, &f, i, &s.parameters[i], data.add(off), at + off)? == Visit::Deferred {
                            return Ok(());
                        }
                    }
                }
                SerialType::Variant => {
                    // The tag is already in the buffer (the parent copied the
                    // slot). Follow the payload pointer, copy the arm's body
                    // in at the cursor, and point the buffer-side slot at it.
                    if f.idx > 0 {
                        return Ok(());
                    }
                    let arm = variant_arm_schema(*data, s)?;
                    let orig_relptr = *(data.add(VARIANT_PAYLOAD_OFFSET) as *const RelPtr);
                    let slot = at + VARIANT_PAYLOAD_OFFSET;
                    if orig_relptr == shm::RELNULL {
                        self.region(slot, std::mem::size_of::<RelPtr>())?.copy_from_slice(&shm::RELNULL.to_ne_bytes());
                    } else {
                        self.cursor = shm::align_up(self.cursor, arm.alignment().max(1));
                        let here = self.cursor;
                        let inner = shm::rel2abs(orig_relptr)?;
                        self.region(here, arm.width)?.copy_from_slice(std::slice::from_raw_parts(inner, arm.width));
                        self.region(slot, std::mem::size_of::<RelPtr>())?.copy_from_slice(&(here as RelPtr).to_ne_bytes());
                        self.cursor += arm.width;
                        self.child(st, &f, 0, arm, inner, here)?;
                    }
                }
                SerialType::Optional => {
                    // The Optional slot is a relptr. RELNULL = absent;
                    // otherwise copy the inner T's body in at the cursor,
                    // point the buffer-side slot at it, and descend for any
                    // further blocks behind T.
                    if f.idx > 0 {
                        return Ok(());
                    }
                    let orig_relptr = *(data as *const RelPtr);
                    if orig_relptr == shm::RELNULL {
                        self.region(at, std::mem::size_of::<RelPtr>())?.copy_from_slice(&shm::RELNULL.to_ne_bytes());
                    } else {
                        let inner_schema = &s.parameters[0];
                        self.cursor = shm::align_up(self.cursor, inner_schema.alignment().max(1));
                        let here = self.cursor;
                        let inner = shm::rel2abs(orig_relptr)?;
                        self.region(here, inner_schema.width)?
                            .copy_from_slice(std::slice::from_raw_parts(inner, inner_schema.width));
                        self.region(at, std::mem::size_of::<RelPtr>())?.copy_from_slice(&(here as RelPtr).to_ne_bytes());
                        self.cursor += inner_schema.width;
                        self.child(st, &f, 0, inner_schema, inner, here)?;
                    }
                }
                _ => {} // primitives already copied by parent
            }
        }
        Ok(())
    }
}

// ── write_flat_to_writer (forward-only flatten) ───────────────────────────
//
// Forward-only counterpart to `flatten_to_buffer`. Emits the same byte
// sequence in strict cursor order so the output can be a `zstd::Encoder`
// or any other `Write` sink that can't be back-patched.
//
// A slot's pointer fields name positions further along the stream, so the
// value is walked twice: a measuring pass computes every pointer field's
// position into a table, in the order slots are written, and the emitting
// pass writes the slots with those values and the tails behind them. Both
// passes are one visit per node; the table costs eight bytes per pointer
// field of the value, which is at most half of what materialising the whole
// flat buffer would.
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
    let res = Resolver::new(schema);
    let mut fe = FlatEmit {
        res: &res,
        vol_mask: (vol_idx as u64) << 48,
        cursor: 0,
        table: Vec::new(),
        next: 0,
        out: None,
        scratch: Vec::new(),
        pcount: std::collections::HashMap::new(),
    };
    fe.run(data, schema)?;
    let total = fe.cursor;
    fe.cursor = 0;
    fe.next = 0;
    let mut sink = |bytes: &[u8]| writer.write_all(bytes);
    fe.out = Some(&mut sink);
    fe.run(data, schema)?;
    debug_assert_eq!(fe.cursor, total);
    debug_assert_eq!(fe.next, fe.table.len());
    Ok(total)
}

/// The two-pass flat emitter. Frames carry the value's node and the index
/// of the node's first pointer-field entry in `table`; an array frame
/// carries the index of its element region's first entry once the region
/// has been written.
struct FlatEmit<'a, 'r> {
    res: &'a Resolver<'r>,
    /// `(vol_idx as u64) << 48`, ORed into every emitted relptr.
    vol_mask: u64,
    cursor: usize,
    /// The encoded value of every pointer field, in the order slots are
    /// written. Filled by the measuring pass, read by the emitting pass.
    table: Vec<u64>,
    /// The emitting pass's cursor into `table`: the next entry to reserve.
    next: usize,
    /// The sink; `None` during the measuring pass.
    out: Option<&'a mut dyn FnMut(&[u8]) -> std::io::Result<()>>,
    /// A slot being materialised.
    scratch: Vec<u8>,
    /// Pointer-field counts per schema node.
    pcount: std::collections::HashMap<*const Schema, usize>,
}

impl<'a, 'r> FlatEmit<'a, 'r> {
    fn run(&mut self, data: AbsPtr, schema: &'r Schema) -> Result<(), MorlocError> {
        let n = self.pointer_fields(schema)?;
        let base = self.reserve(n);
        self.slot(schema, data, base)?;
        let mut st = Stack::new();
        st.enter(schema, data, base);
        walk::run(self, &mut st)
    }

    fn measuring(&self) -> bool {
        self.out.is_none()
    }

    /// Claim `n` table entries, returning the index of the first. The
    /// measuring pass grows the table; the emitting pass advances through
    /// it, and the two agree because they walk the same value.
    fn reserve(&mut self, n: usize) -> usize {
        if self.measuring() {
            let base = self.table.len();
            self.table.resize(base + n, 0);
            base
        } else {
            let base = self.next;
            self.next += n;
            base
        }
    }

    fn set(&mut self, at: usize, value: u64) {
        if self.measuring() {
            self.table[at] = value;
        }
    }

    fn pad_to(&mut self, target: usize) -> Result<(), MorlocError> {
        if let Some(w) = self.out.as_mut() {
            const PAD: [u8; 64] = [0u8; 64];
            let mut pos = self.cursor;
            while pos < target {
                let n = (target - pos).min(PAD.len());
                w(&PAD[..n]).map_err(MorlocError::Io)?;
                pos += n;
            }
        }
        self.cursor = self.cursor.max(target);
        Ok(())
    }

    fn write_bytes(&mut self, bytes: &[u8]) -> Result<(), MorlocError> {
        if let Some(w) = self.out.as_mut() {
            w(bytes).map_err(MorlocError::Io)?;
        }
        self.cursor += bytes.len();
        Ok(())
    }

    /// Record `n` bytes without a source to copy from (the measuring pass
    /// counts a region it will only materialise when emitting).
    fn advance(&mut self, n: usize) {
        self.cursor += n;
    }

    /// The number of pointer fields in a node's inline layout: one for each
    /// string, array, big integer, stream handle, optional or variant slot,
    /// reached through tuple and record fields.
    fn pointer_fields(&mut self, s: &'r Schema) -> Result<usize, MorlocError> {
        let s = self.res.resolve(s)?;
        let key = s as *const Schema;
        if let Some(&n) = self.pcount.get(&key) {
            return Ok(n);
        }
        let n = match s.serial_type {
            SerialType::Int
            | SerialType::String
            | SerialType::Array
            | SerialType::IFile
            | SerialType::OStream
            | SerialType::IStream
            | SerialType::Optional
            | SerialType::Variant => 1,
            SerialType::Tuple | SerialType::Map => {
                let mut total = 0;
                for p in &s.parameters {
                    total += self.pointer_fields(p)?;
                }
                total
            }
            _ => 0,
        };
        self.pcount.insert(key, n);
        Ok(n)
    }

    /// Write the slot of `s` at the cursor: its bytes as they lie in memory
    /// with every pointer field replaced by its table value. Costs nothing
    /// but the cursor when measuring.
    fn slot(&mut self, s: &'r Schema, data: AbsPtr, base: usize) -> Result<(), MorlocError> {
        let width = self.res.resolve(s)?.width;
        if self.measuring() {
            self.advance(width);
            return Ok(());
        }
        let mut buf = std::mem::take(&mut self.scratch);
        buf.clear();
        // SAFETY: `data` points at `width` bytes laid out as `s`.
        buf.extend_from_slice(unsafe { std::slice::from_raw_parts(data, width) });
        let mut j = base;
        self.patch(s, data, &mut buf, 0, &mut j)?;
        let r = self.write_bytes(&buf);
        self.scratch = buf;
        r
    }

    /// Overwrite the pointer fields of the node at `off` within `buf`, in
    /// inline order, with the table entries from `j` on.
    fn patch(&mut self, s: &'r Schema, data: AbsPtr, buf: &mut [u8], off: usize, j: &mut usize) -> Result<(), MorlocError> {
        let s = self.res.resolve(s)?;
        unsafe {
            match s.serial_type {
                SerialType::Int => {
                    let size = *(data as *const usize);
                    if size > 1 {
                        buf[off + 8..off + 16].copy_from_slice(&self.table[*j].to_le_bytes());
                    }
                    *j += 1;
                }
                SerialType::String | SerialType::Array => {
                    buf[off + 8..off + 16].copy_from_slice(&self.table[*j].to_le_bytes());
                    *j += 1;
                }
                SerialType::IFile | SerialType::OStream | SerialType::IStream => {
                    // The tag travels in the slot; bytes 1..8 stay zero.
                    use morloc_runtime_types::stream_handle as sh;
                    buf[off] = sh::read_tag(data);
                    for b in &mut buf[off + 1..off + 8] {
                        *b = 0;
                    }
                    buf[off + 8..off + 16].copy_from_slice(&self.table[*j].to_le_bytes());
                    *j += 1;
                }
                SerialType::Optional => {
                    buf[off..off + 8].copy_from_slice(&self.table[*j].to_le_bytes());
                    *j += 1;
                }
                SerialType::Variant => {
                    buf[off] = *data;
                    for b in &mut buf[off + 1..off + VARIANT_PAYLOAD_OFFSET] {
                        *b = 0;
                    }
                    buf[off + VARIANT_PAYLOAD_OFFSET..off + VARIANT_PAYLOAD_OFFSET + 8]
                        .copy_from_slice(&self.table[*j].to_le_bytes());
                    *j += 1;
                }
                SerialType::Tuple | SerialType::Map => {
                    for i in 0..s.parameters.len() {
                        let fo = s.offsets[i];
                        self.patch(&s.parameters[i], data.add(fo), buf, off + fo, j)?;
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }

    /// The table index of field `i` of tuple `s`, whose own entries start
    /// at `base`.
    fn field_base(&mut self, s: &'r Schema, base: usize, i: usize) -> Result<usize, MorlocError> {
        let mut b = base;
        for p in &s.parameters[..i] {
            b += self.pointer_fields(p)?;
        }
        Ok(b)
    }

    fn child(
        &mut self,
        st: &mut Stack<usize>,
        f: &Frame<usize>,
        idx: usize,
        s: &'r Schema,
        data: AbsPtr,
        base: usize,
    ) -> Result<Visit, MorlocError> {
        if self.res.flat(s) {
            self.step(st, Frame::new(s, data, base))?;
            Ok(Visit::Done)
        } else {
            walk::defer(self, st, f, idx, s, data, base);
            Ok(Visit::Deferred)
        }
    }
}

impl<'a, 'r> Walker<usize> for FlatEmit<'a, 'r> {
    /// Emit the tail of the node: the blocks its pointer fields name, each
    /// followed by its own tail, in inline order.
    fn step(&mut self, st: &mut Stack<usize>, f: Frame<usize>) -> Result<(), MorlocError> {
        // SAFETY: frames hold nodes of the tree the resolver indexes, and
        // `data` points at a value laid out as that schema describes.
        let s: &'r Schema = self.res.resolve(unsafe { &*f.schema })?;
        let data = f.data;
        let base = f.x;
        unsafe {
            match s.serial_type {
                SerialType::Int => {
                    let size = *(data as *const usize);
                    if size > 1 {
                        let pos = shm::align_up(self.cursor, std::mem::align_of::<u64>());
                        self.set(base, pos as u64 | self.vol_mask);
                        self.pad_to(pos)?;
                        let limbs = shm::rel2abs(*(data.add(std::mem::size_of::<usize>()) as *const RelPtr))?;
                        let total = size * std::mem::size_of::<u64>();
                        self.write_bytes(std::slice::from_raw_parts(limbs, total))?;
                    }
                }
                SerialType::String | SerialType::Array => {
                    let arr = &*(data as *const Array);
                    let elem_schema = &s.parameters[0];
                    let elem_w = elem_schema.width;
                    let mut ebase = f.x;
                    if f.idx == 0 {
                        if arr.size == 0 {
                            // Empty arrays use 0 as a sentinel for "no data"
                            // (size==0 short-circuits the rel2abs read), with
                            // no vol_mask so readers can distinguish.
                            self.set(base, 0);
                            return Ok(());
                        }
                        let align = if matches!(s.serial_type, SerialType::String) {
                            elem_schema.alignment()
                        } else {
                            elem_schema.array_data_alignment()
                        };
                        let pos = shm::align_up(self.cursor, align);
                        self.set(base, pos as u64 | self.vol_mask);
                        self.pad_to(pos)?;
                        let elems = shm::rel2abs(arr.data)?;
                        if elem_schema.is_fixed_width() {
                            self.write_bytes(std::slice::from_raw_parts(elems, arr.size * elem_w))?;
                            return Ok(());
                        }
                        // The element region: every element's slot, with
                        // its pointer fields' entries claimed in order.
                        let per = self.pointer_fields(elem_schema)?;
                        ebase = self.reserve(arr.size * per);
                        for i in 0..arr.size {
                            self.slot(elem_schema, elems.add(i * elem_w), ebase + i * per)?;
                        }
                    }
                    if elem_schema.is_fixed_width() {
                        return Ok(());
                    }
                    let per = self.pointer_fields(elem_schema)?;
                    let elems = shm::rel2abs(arr.data)?;
                    let flat_elem = self.res.flat(elem_schema);
                    let mut g = f;
                    g.x = ebase;
                    for i in f.idx..arr.size {
                        let p = elems.add(i * elem_w);
                        if flat_elem {
                            self.step(st, Frame::new(elem_schema, p, ebase + i * per))?;
                        } else if self.child(st, &g, i, elem_schema, p, ebase + i * per)? == Visit::Deferred {
                            return Ok(());
                        }
                    }
                }
                SerialType::IFile | SerialType::OStream | SerialType::IStream => {
                    // TAG_PATH's suballoc lands in the tail; TAG_HANDLE's
                    // slot id rides through in the slot unchanged.
                    use morloc_runtime_types::stream_handle as sh;
                    let tag = sh::read_tag(data);
                    let payload = sh::read_payload(data);
                    if tag != sh::TAG_PATH {
                        self.set(base, payload);
                    } else if payload == sh::RELNULL_PAYLOAD {
                        self.set(base, sh::RELNULL_PAYLOAD);
                    } else {
                        let pos = shm::align_up(self.cursor, 8);
                        self.set(base, pos as u64 | self.vol_mask);
                        self.pad_to(pos)?;
                        let suballoc = shm::rel2abs(payload as RelPtr)?;
                        let total = sh::path_suballoc_size(sh::read_path_size(suballoc) as usize);
                        self.write_bytes(std::slice::from_raw_parts(suballoc, total))?;
                    }
                }
                SerialType::Tuple | SerialType::Map => {
                    for i in f.idx..s.parameters.len() {
                        let fb = self.field_base(s, base, i)?;
                        if self.child(st, &f, i, &s.parameters[i], data.add(s.offsets[i]) as *mut u8, fb)? == Visit::Deferred {
                            return Ok(());
                        }
                    }
                }
                SerialType::Variant => {
                    if f.idx > 0 {
                        return Ok(());
                    }
                    let arm = variant_arm_schema(*data, s)?;
                    let relptr = *(data.add(VARIANT_PAYLOAD_OFFSET) as *const RelPtr);
                    if relptr == shm::RELNULL {
                        self.set(base, shm::RELNULL as u64);
                    } else {
                        let pos = shm::align_up(self.cursor, arm.alignment().max(1));
                        self.set(base, pos as u64 | self.vol_mask);
                        self.pad_to(pos)?;
                        let inner = shm::rel2abs(relptr)?;
                        let n = self.pointer_fields(arm)?;
                        let ibase = self.reserve(n);
                        self.slot(arm, inner, ibase)?;
                        self.child(st, &f, 0, arm, inner, ibase)?;
                    }
                }
                SerialType::Optional => {
                    if f.idx > 0 {
                        return Ok(());
                    }
                    let relptr = *(data as *const RelPtr);
                    if relptr == shm::RELNULL {
                        self.set(base, shm::RELNULL as u64);
                    } else {
                        let inner_schema = &s.parameters[0];
                        let pos = shm::align_up(self.cursor, inner_schema.alignment().max(1));
                        self.set(base, pos as u64 | self.vol_mask);
                        self.pad_to(pos)?;
                        let inner = shm::rel2abs(relptr)?;
                        let n = self.pointer_fields(inner_schema)?;
                        let ibase = self.reserve(n);
                        self.slot(inner_schema, inner, ibase)?;
                        self.child(st, &f, 0, inner_schema, inner, ibase)?;
                    }
                }
                _ => {}
            }
        }
        Ok(())
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

    #[must_use]
    fn setup() -> std::sync::RwLockReadGuard<'static, ()> { crate::init_test_shm() }

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
        let _shm = setup();
        assert_byte_equal("42", "i4");
        assert_byte_equal("-1", "i8");
        assert_byte_equal("3.14", "f8");
        assert_byte_equal("0.5", "f4");
        assert_byte_equal("true", "b");
        assert_byte_equal("false", "b");
    }

    #[test]
    fn strings() {
        let _shm = setup();
        assert_byte_equal("\"\"", "s");
        assert_byte_equal("\"hello\"", "s");
        assert_byte_equal("\"a slightly longer string here\"", "s");
    }

    #[test]
    fn array_of_primitive_fixed() {
        let _shm = setup();
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
        let _shm = setup();
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
        let _shm = setup();
        assert_byte_equal("[[1,2,3],[4,5],[]]", "aai4");
        assert_byte_equal("[[\"a\",\"b\"],[\"cd\"],[]]", "aas");
    }

    #[test]
    fn tuple_fixed_width() {
        let _shm = setup();
        assert_byte_equal("[1, 2.5]", "t2i4f8");
        assert_byte_equal("[1, 2, 3]", "t3i4i4i4");
    }

    #[test]
    fn tuple_with_variable_fields() {
        let _shm = setup();
        assert_byte_equal("[1, \"hello\"]", "t2i4s");
        assert_byte_equal("[\"a\", \"bb\", \"ccc\"]", "t3sss");
        assert_byte_equal("[\"a\", 42, \"bb\"]", "t3si4s");
        // Tuple of arrays: each field has its own variable region
        assert_byte_equal("[[1,2,3], [4,5,6]]", "t2ai4ai4");
        assert_byte_equal("[[1.0,2.0], [\"a\",\"b\"]]", "t2af8as");
    }

    #[test]
    fn array_of_tuple_of_strings() {
        let _shm = setup();
        // Array of small fixed-arity string tuples.
        assert_byte_equal(
            "[[\"a\",\"b\",\"c\"], [\"dd\",\"ee\",\"ff\"], [\"\",\"\",\"\"]]",
            "at3sss",
        );
    }

    #[test]
    fn optional_some_and_none() {
        let _shm = setup();
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
        let _shm = setup();
        assert_byte_equal("[42, \"hi\"]", "t2?i4s");
        assert_byte_equal("[null, \"hi\"]", "t2?i4s");
        assert_byte_equal("[42, null]", "t2?i4?s");
    }

    #[test]
    fn empty_collections() {
        let _shm = setup();
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
        let _shm = setup();
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
        let _shm = setup();
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
        let _shm = setup();
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
