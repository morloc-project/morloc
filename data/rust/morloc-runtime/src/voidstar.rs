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
use morloc_runtime_types::width;

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

/// Where a value's relative pointers point. `resolve` yields the address of
/// `extent` readable bytes at `rel`, or fails; it never yields an address
/// whose region leaves the space, so a walker that asks for every region it
/// reads cannot read outside the value's storage.
pub trait Space {
    fn resolve(&self, rel: RelPtr, extent: usize) -> Result<AbsPtr, MorlocError>;
}

/// Shared memory, resolved through the volume table.
pub struct Arena;

impl Space for Arena {
    #[inline]
    fn resolve(&self, rel: RelPtr, extent: usize) -> Result<AbsPtr, MorlocError> {
        shm::rel2abs_extent(rel, extent)
    }
}

/// One contiguous payload holding offsets from its own start: a flattened
/// buffer or a stream sub-packet mapped from a file. Its bytes come from
/// outside, so every region is checked against its length.
pub struct Local {
    base: *const u8,
    len: usize,
}

impl Local {
    /// # Safety
    ///
    /// `base..base + len` must be readable for as long as the space is used.
    pub unsafe fn new(base: *const u8, len: usize) -> Self {
        Local { base, len }
    }
}

impl Space for Local {
    #[inline]
    fn resolve(&self, rel: RelPtr, extent: usize) -> Result<AbsPtr, MorlocError> {
        if shm::relptr_is_sentinel(rel) || rel < 0 {
            return Err(payload_region_error(rel, extent, self.len));
        }
        let off = shm::relptr_offset(rel);
        match off.checked_add(extent) {
            Some(end) if end <= self.len => {
                // SAFETY: [off, end) lies inside the payload `new` vouched for.
                Ok(unsafe { self.base.add(off) } as AbsPtr)
            }
            _ => Err(payload_region_error(rel, extent, self.len)),
        }
    }
}

/// Why `extent` bytes at `rel` are not inside a `len`-byte payload.
pub fn payload_region_error(rel: RelPtr, extent: usize, len: usize) -> MorlocError {
    if shm::relptr_is_sentinel(rel) || rel < 0 {
        MorlocError::Other(format!("relptr {} in a local payload is a sentinel (corrupt payload?)", rel))
    } else {
        MorlocError::Other(format!(
            "a {}-byte region at offset {} runs past the {}-byte payload",
            extent,
            shm::relptr_offset(rel),
            len
        ))
    }
}

/// The C `morloc_space_t`: shared memory when `base` is null, else an inline
/// payload of `len` bytes at `base`.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct MorlocSpace {
    pub base: *const u8,
    pub len: usize,
}

impl MorlocSpace {
    pub const SHM: MorlocSpace = MorlocSpace { base: std::ptr::null(), len: 0 };
}

impl Space for MorlocSpace {
    #[inline]
    fn resolve(&self, rel: RelPtr, extent: usize) -> Result<AbsPtr, MorlocError> {
        if self.base.is_null() {
            Arena.resolve(rel, extent)
        } else {
            // SAFETY: whoever built the space vouches that `base..base + len`
            // is readable while it is used.
            unsafe { Local::new(self.base, self.len) }.resolve(rel, extent)
        }
    }
}

/// The path block a `TAG_PATH` stream-handle payload points at -- its 8-byte
/// length, then that many bytes -- with both regions checked against
/// `space`, or `None` for the empty-path payload. The path's bytes are the
/// block from offset 8.
///
/// # Safety
///
/// The space must stay mapped for the returned lifetime.
pub unsafe fn path_suballoc<'a, S: Space>(space: &S, payload: u64) -> Result<Option<&'a [u8]>, MorlocError> {
    use morloc_runtime_types::stream_handle as sh;
    if payload == sh::RELNULL_PAYLOAD {
        return Ok(None);
    }
    let rel = sh::payload_relptr(payload);
    let len_at = space.resolve(rel, 8)?;
    let total = sh::path_suballoc_size(width::usize_from_u64(sh::read_path_size(len_at)));
    let block = space.resolve(rel, total)?;
    Ok(Some(std::slice::from_raw_parts(block, total)))
}

/// `n` elements of `width` bytes, or an error when a size read from a value
/// overflows the address space.
pub(crate) fn region_len(n: usize, width: usize) -> Result<usize, MorlocError> {
    n.checked_mul(width).ok_or_else(|| {
        MorlocError::Other(format!("{} elements of {} bytes overflow the address space", n, width))
    })
}

/// The relptr range `[lo, hi)` of one block. A block lies inside one
/// volume, so its relptrs are contiguous, and bounds are checked on relptrs
/// directly without resolving each one to an address. Computed once per
/// block, not per value.
#[derive(Clone, Copy)]
pub struct RelWindow {
    lo: RelPtr,
    hi: RelPtr,
}

impl RelWindow {
    pub fn of_block(block: *const u8, len: usize) -> Result<RelWindow, MorlocError> {
        window_of(shm::abs2rel(block as AbsPtr)?, len)
    }

    /// The `len` bytes starting `offset` bytes into this window. Derived
    /// arithmetically, so an empty region at the block's end needs no
    /// address that lies past it.
    pub fn narrow(self, offset: usize, len: usize) -> Result<RelWindow, MorlocError> {
        let lo = isize::try_from(offset).ok().and_then(|o| self.lo.checked_add(o));
        match lo {
            Some(lo) => {
                let w = window_of(lo, len)?;
                if w.hi <= self.hi { Ok(w) } else {
                    Err(MorlocError::Shm("narrowed rebase window leaves its block".into()))
                }
            }
            None => Err(MorlocError::Shm("narrowed rebase window overflows".into())),
        }
    }
}

/// Rebase the relptrs of a value that lies wholly inside one block: every
/// rebased pointer, together with the bytes it addresses, must land inside
/// `window`, or the walk fails. A value copied into a fresh block can only
/// be valid if it is self-contained, so a pointer that leaves the block
/// means the copy or the rebase was wrong.
///
/// # Safety
///
/// `data` must point to a live value of `schema` whose relptrs are resolvable.
pub unsafe fn adjust_relptrs_within(
    data: AbsPtr,
    schema: &Schema,
    base_rel: RelPtr,
    window: RelWindow,
) -> Result<(), MorlocError> {
    // SAFETY: forwarded from this function's own contract.
    unsafe { adjust_records_within(data, 1, schema, base_rel, window) }
}

/// `adjust_relptrs_within` over `n` consecutive records of `schema`,
/// sharing one resolver and one stack across them.
///
/// # Safety
///
/// `first` must point to `n` consecutive records of `schema`, and every relptr they hold must be resolvable.
pub unsafe fn adjust_records_within(
    first: AbsPtr,
    n: usize,
    schema: &Schema,
    base_rel: RelPtr,
    window: RelWindow,
) -> Result<(), MorlocError> {
    let res = Resolver::new(schema);
    let mut w = RebaseWalk {
        res: &res,
        mode: Rebase::Shm(base_rel),
        window: Some(window),
    };
    let mut st = Stack::new();
    for k in 0..n {
        // SAFETY: the caller's records are `n` consecutive slots of
        // `schema.width` bytes.
        st.enter(schema, unsafe { first.add(k * schema.width) }, ());
        walk::run(&mut w, &mut st)?;
    }
    Ok(())
}

fn window_of(lo: RelPtr, len: usize) -> Result<RelWindow, MorlocError> {
    let hi = isize::try_from(len).ok().and_then(|n| lo.checked_add(n)).ok_or_else(|| {
        MorlocError::Shm(format!("rebase window at relptr {lo:#x} of {len} bytes overflows"))
    })?;
    Ok(RelWindow { lo, hi })
}

// ── shift_buffer_relptrs ───────────────────────────────────────────────────

/// Add `delta` to every relptr slot inside a self-contained voidstar
/// buffer, without dereferencing through `rel2abs`. The buffer's
/// relptrs are pure offsets from `buf_base` (i.e. `vol_idx = 0` in the
/// encoded form); an intermediate `rel2abs` on them would land in the
/// primary SHM volume rather than in the buffer, so any descent must
/// use buffer-local pointer arithmetic (`buf_base + offset`).
///
/// Use this instead of `adjust_relptrs_within` when the target of the shift
/// is not a fresh SHM allocation but an in-place move within a Vec /
/// per-slot write buffer -- e.g. the write-buffer compaction step and
/// the per-element blob relocation inside `append_one_element`.
///
/// Every shifted pointer must address bytes inside `[buf_base, buf_base +
/// buf_len)`.
pub unsafe fn shift_buffer_relptrs(
    buf_base: *mut u8,
    buf_len: usize,
    field_offset: usize,
    schema: &Schema,
    delta: isize,
) -> Result<(), MorlocError> {
    shift_buffer_relptrs_with(buf_base, buf_len, field_offset, schema, delta, &Resolver::new(schema))
}

/// [`shift_buffer_relptrs`] with a resolver of `schema` the caller built,
/// for a caller shifting many values of one schema.
///
/// # Safety
///
/// As [`shift_buffer_relptrs`].
pub unsafe fn shift_buffer_relptrs_with(
    buf_base: *mut u8,
    buf_len: usize,
    field_offset: usize,
    schema: &Schema,
    delta: isize,
    res: &Resolver<'_>,
) -> Result<(), MorlocError> {
    let window = Some(window_of(0, buf_len)?);
    let mut w = RebaseWalk { res, mode: Rebase::Buffer { buf_base, delta }, window };
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
struct RebaseWalk<'a, 'r> {
    res: &'a Resolver<'r>,
    mode: Rebase,
    /// When set, the relptr range every rebased pointer and the bytes it
    /// addresses must fall inside.
    window: Option<RelWindow>,
}

impl<'a, 'r> RebaseWalk<'a, 'r> {
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

    /// Reject a rebased relptr that, with the `extent` bytes it addresses,
    /// leaves the window.
    #[inline]
    fn check(&self, rel: RelPtr, extent: Option<usize>) -> Result<(), MorlocError> {
        let Some(RelWindow { lo, hi }) = self.window else { return Ok(()) };
        let end = extent.and_then(|e| isize::try_from(e).ok()).and_then(|e| rel.checked_add(e));
        match end {
            Some(end) if rel >= lo && end <= hi => Ok(()),
            _ => Err(MorlocError::Shm(format!(
                "rebased relptr {rel:#x} (+{extent:?} bytes) leaves its block [{lo:#x}, {hi:#x})"
            ))),
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

impl<'a, 'r> Walker<()> for RebaseWalk<'a, 'r> {
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
                        self.check(*relptr, size.checked_mul(std::mem::size_of::<u64>()))?;
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
                        // An empty array's pointer is never followed, but a
                        // rebased one could name an address before the
                        // block, even outside its volume; inside a window it
                        // is pinned to the block's start instead.
                        if let (0, Some(RelWindow { lo, .. })) = (arr.size, self.window) {
                            arr.data = lo;
                            return Ok(());
                        }
                        arr.data += shift;
                        if arr.size > 0 {
                            let w = s.parameters.first().map_or(1, |e| e.width);
                            self.check(arr.data, arr.size.checked_mul(w))?;
                        }
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
                            let moved = payload.wrapping_add(width::i64_from_isize(shift).cast_unsigned());
                            sh::write_field(data, sh::TAG_PATH, moved);
                            if self.window.is_some() {
                                // A path is an 8-byte length, then its bytes.
                                let rel = sh::payload_relptr(moved);
                                self.check(rel, Some(8))?;
                                let len = usize::try_from(*(self.target(rel)? as *const u64)).ok();
                                self.check(rel, len.and_then(|n| n.checked_add(8)))?;
                            }
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
                        self.check(*relptr_slot, Some(arm.width))?;
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
                        self.check(*relptr_slot, Some(s.parameters[0].width))?;
                        let inner = self.target(*relptr_slot)?;
                        self.child(st, &f, 0, &s.parameters[0], inner)?;
                    }
                }
                SerialType::Nil | SerialType::Bool | SerialType::Sint8 | SerialType::Sint16 | SerialType::Sint32 | SerialType::Sint64 | SerialType::Uint8 | SerialType::Uint16 | SerialType::Uint32 | SerialType::Uint64 | SerialType::Float32 | SerialType::Float64 | SerialType::Table | SerialType::Recur | SerialType::Enum => {} // primitives have no relptrs
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
    let landing = Landing::new(blob.len())?;
    // SAFETY: the landing's block is fresh and blob.len() bytes long.
    unsafe { std::ptr::copy_nonoverlapping(blob.as_ptr(), landing.as_mut_ptr(), blob.len()) };
    landing.relocate(schema, vol_idx_hint)
}

/// Bytes brought into a fresh SHM block from outside -- a file, a buffer, a
/// decompressor. Until relocated, the relptrs among them are offsets in the
/// producer's frame and must not be followed, so the only way to a usable
/// value is `relocate`, which rebases every pointer and checks it against
/// the block. A landing dropped unrelocated frees its block.
pub struct Landing {
    block: AbsPtr,
    len: usize,
}

impl Landing {
    pub fn new(len: usize) -> Result<Self, MorlocError> {
        Ok(Landing { block: shm::shmalloc(len)?, len })
    }

    /// Where to write the landed bytes: `len` writable bytes.
    pub fn as_mut_ptr(&self) -> *mut u8 {
        self.block
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Relocate the landed value of `schema` from the producer's frame: its
    /// relptrs are offsets from the value's start, carrying `vol_idx_hint` in
    /// their volume bits (0 for plain buffer-relative offsets, the only frame
    /// written now). Every rebased pointer and the bytes it addresses must lie
    /// inside the block.
    pub fn relocate(self, schema: &Schema, vol_idx_hint: u16) -> Result<AbsPtr, MorlocError> {
        // The root slot is read before any pointer is: a value shorter than
        // it would be read past the block.
        if self.len < schema.width {
            return Err(MorlocError::Serialization(format!(
                "a voidstar value of {} bytes is shorter than its {}-byte root", self.len, schema.width
            )));
        }
        let block_rel = shm::abs2rel(self.block)?;
        let producer_base = shm::encode_relptr(vol_idx_hint as usize, 0);
        let delta = (block_rel as i64).wrapping_sub(producer_base as i64) as RelPtr;
        let window = RelWindow::of_block(self.block, self.len)?;
        // SAFETY: the block holds `len` landed bytes, at least one root
        // wide; every pointer the rebase follows is checked against it.
        unsafe { adjust_relptrs_within(self.block, schema, delta, window) }?;
        let block = self.block;
        std::mem::forget(self);
        Ok(block)
    }
}

/// Decompress the zstd `frames` of `compressed` into one fresh SHM block
/// and relocate the value of `schema` there: the frames decode in parallel
/// straight into the block, with no intermediate buffer.
pub fn land_compressed_frames(
    frames: &[morloc_runtime_types::packet::FrameEntry],
    compressed: &[u8],
    schema: &Schema,
    vol_idx_hint: u16,
) -> Result<AbsPtr, MorlocError> {
    let (uncompressed, compressed_total) = morloc_runtime_types::packet::frame_totals(frames)?;
    if compressed_total != compressed.len() {
        return Err(MorlocError::Packet(format!(
            "frame index sums to {} compressed bytes but header.length = {}",
            compressed_total,
            compressed.len()
        )));
    }
    let landing = Landing::new(uncompressed)?;
    // SAFETY: the landing's block is fresh and `uncompressed` bytes long.
    let dest = unsafe { std::slice::from_raw_parts_mut(landing.as_mut_ptr(), uncompressed) };
    crate::compression::parallel_decompress_frames(frames, compressed, dest)?;
    landing.relocate(schema, vol_idx_hint)
}

impl Drop for Landing {
    fn drop(&mut self) {
        let _ = shm::shfree(self.block);
    }
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
/// Where a deep copy puts the blocks it needs for a value's
/// variable-length parts.
///
/// The default takes one SHM block per part, which is what the name
/// "deep copy" has always meant here. A copy that must hand its result to
/// a pool cannot do that: a pool releases a value with one `shfree` of the
/// root, so every block below the root would be lost. Such a caller sizes
/// the value first and passes [`Bump`], which cuts the parts out of one
/// block it already owns.
pub enum CopyAlloc<'a> {
    /// A fresh SHM block per part.
    Blocks,
    /// A fresh SHM block per part, each recorded so the caller can give
    /// them all back. Used to build a value whose size is not known until
    /// it exists, before [`consolidate`] copies it into one block.
    Recording(&'a mut Vec<crate::shm::AbsPtr>),
    /// Successive slices of a caller-owned region.
    Bump(&'a mut Bump),
}

/// A cursor over a region the caller has already allocated and sized.
pub struct Bump {
    next: *mut u8,
    end: *mut u8,
}

impl Bump {
    /// # Safety
    /// `start` must own `len` writable bytes for as long as this is used.
    pub unsafe fn new(start: *mut u8, len: usize) -> Self {
        Bump { next: start, end: start.add(len) }
    }

    fn take(&mut self, len: usize) -> Result<*mut u8, MorlocError> {
        // Every part is at least pointer-aligned where it needs to be,
        // because the sizes handed here are whole multiples of the widths
        // the schema describes; rounding up keeps that true across parts.
        let len = (len + 7) & !7;
        if (self.end as usize) - (self.next as usize) < len {
            return Err(MorlocError::Shm(
                "deep copy ran past the region it was sized for".into(),
            ));
        }
        let p = self.next;
        // SAFETY: the bound above proves `len` bytes remain.
        self.next = unsafe { p.add(len) };
        Ok(p)
    }
}

impl CopyAlloc<'_> {
    /// A zeroed region of `len` bytes.
    fn zeroed(&mut self, len: usize) -> Result<crate::shm::AbsPtr, MorlocError> {
        match self {
            CopyAlloc::Blocks => shm::shmalloc(len).map(|p| {
                // SAFETY: p owns len bytes.
                unsafe { std::ptr::write_bytes(p, 0, len) };
                p
            }),
            CopyAlloc::Recording(seen) => shm::shmalloc(len).map(|p| {
                seen.push(p);
                // SAFETY: p owns len bytes.
                unsafe { std::ptr::write_bytes(p, 0, len) };
                p
            }),
            CopyAlloc::Bump(b) => b.take(len).map(|p| {
                // SAFETY: take() proved len bytes remain.
                unsafe { std::ptr::write_bytes(p, 0, len) };
                p
            }),
        }
    }

    /// A region of `len` bytes holding a copy of `src`.
    fn copy_of(
        &mut self,
        src: *const u8,
        len: usize,
    ) -> Result<crate::shm::AbsPtr, MorlocError> {
        match self {
            // SAFETY (both arms): `src` is the region of `len` bytes the copy
            // walk resolved; its resolver is what bounds it.
            CopyAlloc::Blocks => unsafe { shm::shmemcpy(src, len) },
            CopyAlloc::Recording(seen) => unsafe { shm::shmemcpy(src, len) }.inspect(|&p| seen.push(p)),
            CopyAlloc::Bump(b) => {
                let p = b.take(len)?;
                // SAFETY: take() proved len bytes remain; src owns len bytes.
                unsafe { std::ptr::copy_nonoverlapping(src, p, len) };
                Ok(p)
            }
        }
    }
}

pub unsafe fn deep_copy(
    src: *const u8,
    dst: *mut u8,
    schema: &Schema,
) -> Result<(), MorlocError> {
    // A source in SHM; a file-backed source uses `deep_copy_with` and a
    // `Local` space.
    deep_copy_with(src, dst, schema, &Arena)
}

/// Deep-copy `src` into `dst`, cutting every variable-length part out of
/// `bump` rather than taking a block for each. The caller must have sized
/// `bump` with [`crate::ffi::calc_voidstar_size_inner`]; running short is an
/// error, not a silent overrun.
///
/// # Safety
/// As [`deep_copy`], plus `bump` must own its region for the call.
pub unsafe fn deep_copy_into(
    src: *const u8,
    dst: *mut u8,
    schema: &Schema,
    bump: &mut Bump,
) -> Result<(), MorlocError> {
    deep_copy_alloc(src, dst, schema, &Arena, CopyAlloc::Bump(bump))
}

/// Same as `deep_copy` but with the source in `space`. The destination is
/// always SHM; the source's relptrs are resolved through `space`, which
/// bounds every region the copy reads.
pub unsafe fn deep_copy_with<S: Space>(
    src: *const u8,
    dst: *mut u8,
    schema: &Schema,
    space: &S,
) -> Result<(), MorlocError> {
    deep_copy_alloc(src, dst, schema, space, CopyAlloc::Blocks)
}

/// Copy a value built as a graph of blocks into ONE block, and give the
/// graph back.
///
/// This is for a builder that cannot know its result's size until it has
/// parsed or walked its input: it builds with
/// [`CopyAlloc::Recording`] (or records its own allocations), then hands
/// the root and that list here. The value that comes back is a single
/// block, which is what a pool can release.
///
/// `root_block` is freed too; the returned pointer replaces it.
///
/// # Safety
/// `root` must point at a value laid out as `schema` describes, `parts`
/// must list every block below it and nothing else, and nothing may still
/// be pointing into any of them.
pub unsafe fn consolidate(
    root: crate::shm::AbsPtr,
    schema: &Schema,
    parts: &[crate::shm::AbsPtr],
) -> Result<crate::shm::AbsPtr, MorlocError> {
    // The flat size packs each part against the last; the cursor below
    // rounds each one up to eight bytes so a part that must be aligned is,
    // which can cost up to seven bytes per part. Budget that rather than
    // discover it as a short region.
    let total = crate::ffi::calc_voidstar_size_inner(root, schema)?
        + 8 * (parts.len() + 1);
    let dest = shm::shmalloc(total)?;
    std::ptr::write_bytes(dest, 0, total);
    let mut bump = Bump::new(dest.add(schema.width), total - schema.width);
    let r = deep_copy_alloc(root, dest, schema, &Arena, CopyAlloc::Bump(&mut bump));
    if let Err(e) = r {
        let _ = shm::shfree(dest);
        return Err(e);
    }
    for &p in parts {
        let _ = shm::shfree(p);
    }
    let _ = shm::shfree(root);
    Ok(dest)
}

/// Copy the value at `src` into one self-contained SHM block.
///
/// The single block is what a pool can release: it frees a value with one
/// `shfree` of the root, so a value spread over several blocks would lose
/// everything below it.
///
/// Sizes the region first and then copies once. The size walk is structural
/// -- a flat array costs one multiply rather than a visit per element -- and
/// it reports the bound on the part count that the bump's per-part rounding
/// has to be budgeted against. [`consolidate`] is the route for a builder
/// that cannot know its result's size until it exists; a caller copying a
/// value that already exists can know it, and pays one pass instead of two.
///
/// # Safety
/// `src` must point at a value laid out as `schema` describes.
pub unsafe fn deep_copy_to_block(
    src: *const u8,
    schema: &Schema,
) -> Result<crate::shm::AbsPtr, MorlocError> {
    let (payload, parts) = crate::ffi::calc_voidstar_layout(src, schema)?;
    // Each part is rounded up to eight bytes as it is cut from the bump, so
    // the region carries up to seven bytes of padding per part, plus the
    // root's own rounding.
    let total = payload
        .checked_add(8usize.saturating_mul(parts.saturating_add(1)))
        .ok_or_else(|| MorlocError::Shm("value too large to copy into one block".into()))?;
    let dest = shm::shmalloc(total)?;
    std::ptr::write_bytes(dest, 0, total);
    let mut bump = Bump::new(dest.add(schema.width), total - schema.width);
    if let Err(e) = deep_copy_into(src, dest, schema, &mut bump) {
        let _ = shm::shfree(dest);
        return Err(e);
    }
    Ok(dest)
}

/// [`deep_copy_with`] with the part allocator chosen by the caller.
///
/// # Safety
/// As [`deep_copy_with`].
pub unsafe fn deep_copy_alloc<S: Space>(
    src: *const u8,
    dst: *mut u8,
    schema: &Schema,
    space: &S,
    alloc: CopyAlloc<'_>,
) -> Result<(), MorlocError> {
    let mut w = CopyWalk { res: Resolver::new(schema), space, alloc };
    let mut st = Stack::new();
    st.enter(schema, src, dst);
    walk::run(&mut w, &mut st)
}

/// Copies a value's blocks into fresh SHM allocations, one per block, so
/// the copy can be released block by block. A frame's `data` is the
/// source slot and its `x` the destination slot; a block for a pointer
/// field is allocated and pointed at before the walk descends into it.
struct CopyWalk<'r, 'f, S> {
    res: Resolver<'r>,
    space: &'f S,
    alloc: CopyAlloc<'f>,
}

impl<'r, 'f, S: Space> CopyWalk<'r, 'f, S> {
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

impl<'r, 'f, S: Space> Walker<*mut u8> for CopyWalk<'r, 'f, S> {
    fn step(&mut self, st: &mut Stack<*mut u8>, f: Frame<*mut u8>) -> Result<(), MorlocError> {
        // SAFETY: frames hold nodes of the tree the resolver was built from;
        // `data` points at a source value laid out as that schema describes
        // and `x` at a destination slot of the same width.
        let schema: &'r Schema = self.res.resolve(unsafe { &*f.schema })?;
        let src = f.data;
        let dst = f.x;
        let space = self.space;
        unsafe {
            match schema.serial_type {
                SerialType::String => {
                    let src_arr = &*(src as *const Array);
                    let dst_arr = &mut *(dst as *mut Array);
                    dst_arr.size = src_arr.size;
                    if src_arr.size > 0 && src_arr.data >= 0 {
                        let src_data = space.resolve(src_arr.data, src_arr.size)?;
                        let new_data = self.alloc.copy_of(src_data, src_arr.size)?;
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
                            if let Some(block) = path_suballoc(space, src_payload)? {
                                let new_suballoc = self.alloc.copy_of(block.as_ptr(), block.len())?;
                                sh::write_field(dst, sh::TAG_PATH, sh::path_payload(shm::abs2rel(new_suballoc)?));
                            } else {
                                sh::write_field(dst, sh::TAG_PATH, sh::RELNULL_PAYLOAD);
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
                        let bytes = region_len(src_arr.size, elem_width)?;
                        let src_data = space.resolve(src_arr.data, bytes)?;
                        let new_data = self.alloc.zeroed(bytes)?;
                        dst_arr.data = shm::abs2rel(new_data)?;
                        if elem_schema.is_fixed_width() {
                            std::ptr::copy_nonoverlapping(src_data, new_data, bytes);
                            return Ok(());
                        }
                    }
                    let elem_schema = &schema.parameters[0];
                    if elem_schema.is_fixed_width() {
                        return Ok(());
                    }
                    let elem_width = elem_schema.width;
                    let src_data = space.resolve(src_arr.data, region_len(src_arr.size, elem_width)?)?;
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
                        let src_inner = space.resolve(src_relptr, arm.width)?;
                        let dst_inner = self.alloc.zeroed(arm.width)?;
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
                        let src_inner = space.resolve(src_relptr, inner_schema.width)?;
                        let dst_inner = self.alloc.zeroed(inner_schema.width)?;
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
                            let limb_bytes = region_len(size, std::mem::size_of::<u64>())?;
                            let src_limbs = space.resolve(src_relptr, limb_bytes)?;
                            let new_limbs = self.alloc.copy_of(src_limbs, limb_bytes)?;
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
                SerialType::Nil | SerialType::Bool | SerialType::Sint8 | SerialType::Sint16 | SerialType::Sint32 | SerialType::Sint64 | SerialType::Uint8 | SerialType::Uint16 | SerialType::Uint32 | SerialType::Uint64 | SerialType::Float32 | SerialType::Float64 | SerialType::Recur | SerialType::Enum => {
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
    flatten_into_mode(buf, data, schema, false, &Resolver::new(schema))
}

/// [`flatten_into`] for a value leaving the local registry: each
/// handle-form stream field is written in path form, its path laid out in
/// place among the value's other variable bytes, as a path-form field's
/// already is. A value flattened this way is self-contained: every byte
/// it points at follows its own slot in depth-first order.
pub fn flatten_into_portable(
    buf: &mut Vec<u8>,
    data: AbsPtr,
    schema: &Schema,
) -> Result<(), MorlocError> {
    flatten_into_mode(buf, data, schema, true, &Resolver::new(schema))
}

/// [`flatten_into_portable`] with a resolver of `schema` the caller built,
/// for a caller flattening many values of one schema.
pub fn flatten_into_portable_with(
    buf: &mut Vec<u8>,
    data: AbsPtr,
    schema: &Schema,
    res: &Resolver<'_>,
) -> Result<(), MorlocError> {
    flatten_into_mode(buf, data, schema, true, res)
}

fn flatten_into_mode(
    buf: &mut Vec<u8>,
    data: AbsPtr,
    schema: &Schema,
    portable: bool,
    res: &Resolver<'_>,
) -> Result<(), MorlocError> {
    let total = if portable {
        crate::ffi::calc_voidstar_size_portable_with(data, schema, res)?
    } else {
        crate::ffi::calc_voidstar_size_inner(data, schema)?
    };
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
    let mut w = FlattenWalk {
        res,
        buf: buf.as_mut_ptr(),
        len: buf.len(),
        cursor: schema.width,
        portable,
    };
    let mut st = Stack::new();
    st.enter(schema, data, 0);
    walk::run(&mut w, &mut st)?;

    Ok(())
}

/// Copies a value's variable-length blocks into a flat buffer behind the
/// slots the parent already copied, rewriting each pointer to the
/// buffer-relative offset of the block. A frame's `x` is the offset of the
/// node's slot in the buffer.
struct FlattenWalk<'a, 'r> {
    res: &'a Resolver<'r>,
    buf: *mut u8,
    len: usize,
    cursor: usize,
    /// Write handle-form stream fields as their paths.
    portable: bool,
}

impl<'a, 'r> FlattenWalk<'a, 'r> {
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

impl<'a, 'r> Walker<usize> for FlattenWalk<'a, 'r> {
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
                            if let Some(block) = path_suballoc(&Arena, src_payload)? {
                                self.cursor = shm::align_up(self.cursor, 8);
                                let here = self.cursor;
                                self.region(here, block.len())?.copy_from_slice(block);
                                self.cursor += block.len();
                                sh::write_field(dst_field, sh::TAG_PATH, width::u64_from_usize(here));
                            } else {
                                sh::write_field(dst_field, sh::TAG_PATH, sh::RELNULL_PAYLOAD);
                            }
                        }
                        t if t == sh::TAG_HANDLE && self.portable => {
                            let path = crate::handle_scan::portable_path(sh::payload_handle(src_payload))?;
                            if path.is_empty() {
                                sh::write_field(dst_field, sh::TAG_PATH, sh::RELNULL_PAYLOAD);
                            } else {
                                let total = sh::path_suballoc_size(path.len());
                                self.cursor = shm::align_up(self.cursor, 8);
                                let here = self.cursor;
                                sh::write_path_suballoc(self.region(here, total)?.as_mut_ptr(), path.as_bytes());
                                self.cursor += total;
                                sh::write_field(
                                    self.region(at, sh::STREAM_HANDLE_FIELD_SIZE)?.as_mut_ptr(),
                                    sh::TAG_PATH,
                                    here as u64,
                                );
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
                SerialType::Nil | SerialType::Bool | SerialType::Sint8 | SerialType::Sint16 | SerialType::Sint32 | SerialType::Sint64 | SerialType::Uint8 | SerialType::Uint16 | SerialType::Uint32 | SerialType::Uint64 | SerialType::Float32 | SerialType::Float64 | SerialType::Table | SerialType::Recur | SerialType::Enum => {} // primitives already copied by parent
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
            SerialType::Nil | SerialType::Bool | SerialType::Sint8 | SerialType::Sint16 | SerialType::Sint32 | SerialType::Sint64 | SerialType::Uint8 | SerialType::Uint16 | SerialType::Uint32 | SerialType::Uint64 | SerialType::Float32 | SerialType::Float64 | SerialType::Table | SerialType::Recur | SerialType::Enum => 0,
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
                SerialType::Nil | SerialType::Bool | SerialType::Sint8 | SerialType::Sint16 | SerialType::Sint32 | SerialType::Sint64 | SerialType::Uint8 | SerialType::Uint16 | SerialType::Uint32 | SerialType::Uint64 | SerialType::Float32 | SerialType::Float64 | SerialType::Table | SerialType::Recur | SerialType::Enum => {}
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
                        let block = path_suballoc(&Arena, payload)?.unwrap_or(&[]);
                        self.write_bytes(block)?;
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
                        self.set(base, width::i64_from_isize(shm::RELNULL).cast_unsigned());
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
                        self.set(base, width::i64_from_isize(shm::RELNULL).cast_unsigned());
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
                SerialType::Nil | SerialType::Bool | SerialType::Sint8 | SerialType::Sint16 | SerialType::Sint32 | SerialType::Sint64 | SerialType::Uint8 | SerialType::Uint16 | SerialType::Uint32 | SerialType::Uint64 | SerialType::Float32 | SerialType::Float64 | SerialType::Table | SerialType::Recur | SerialType::Enum => {}
            }
        }
        Ok(())
    }
}

// ── write_voidstar_binary (to fd) ──────────────────────────────────────────

/// Flatten voidstar and write to a file descriptor. Returns bytes written.
pub fn write_binary_to_fd(fd: i32, data: AbsPtr, schema: &Schema) -> Result<usize, MorlocError> {
    let buf = flatten_to_buffer(data, schema)?;
    crate::utility::write_all_to_fd(fd, &buf)?;
    Ok(buf.len())
}

// ── Tests: write_flat_to_writer byte-equivalence with flatten_to_buffer ───

#[cfg(test)]
mod flat_writer_tests {
    use super::*;
    use crate::schema::parse_schema;
    use crate::json::read_json_with_schema;

    #[must_use]
    fn setup() -> crate::ArenaShared { crate::init_test_shm() }

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

#[cfg(test)]
mod one_block_copy_tests {
    use super::*;
    use crate::json::{read_json_with_schema, voidstar_to_json_string};
    use crate::schema::parse_schema;

    /// Every value shape the deep copy can allocate for, so that the size
    /// walk's bound on the part count is exercised against each of the
    /// copier's allocation sites rather than only the string one.
    const SHAPES: &[(&str, &str)] = &[
        ("s", "\"plain\""),
        ("s", "\"\""),
        ("as", "[\"a\",\"bb\",\"ccc\"]"),
        ("as", "[]"),
        ("ai4", "[1,2,3,4,5]"),
        ("ai4", "[]"),
        // Array of variable-width elements: a part per element, plus one
        // for the element region itself.
        ("aas", "[[\"a\"],[\"bb\",\"ccc\"],[]]"),
        ("at2si4", "[[\"a\",1],[\"bb\",2],[\"ccc\",3]]"),
        ("t3sss", "[\"a\",\"bb\",\"ccc\"]"),
        ("m21ai41cs", "{\"a\":7,\"c\":\"seven\"}"),
        ("am21ai41cs", "[{\"a\":1,\"c\":\"x\"},{\"a\":2,\"c\":\"yy\"}]"),
        // Optional: the inner value is its own sub-allocation.
        ("?i4", "null"),
        ("?i4", "5"),
        ("a?s", "[\"a\",null,\"ccc\"]"),
        // Arbitrary-precision Int: small enough to inline, and large
        // enough to take a limb allocation.
        ("j", "3"),
        ("j", "123456789012345678901234567890123456789012345678901234567890"),
        ("aj", "[1,123456789012345678901234567890123456789012345678901234567890]"),
    ];

    /// A copy into one block must reproduce the value exactly, whatever its
    /// shape. The size walk bounds the parts the bump will cut, so a shape
    /// whose bound came out short would fail here with "deep copy ran past
    /// the region it was sized for" rather than silently truncating.
    #[test]
    fn a_one_block_copy_reproduces_every_shape() {
        let _shm = crate::own_test_registry();
        for (schema_str, json) in SHAPES {
            let schema = parse_schema(schema_str).unwrap();
            let src = read_json_with_schema(json, &schema).unwrap();
            let before = voidstar_to_json_string(src, &schema).unwrap();

            let copy = unsafe { deep_copy_to_block(src, &schema) }
                .unwrap_or_else(|e| panic!("{} {}: {:?}", schema_str, json, e));
            let after = voidstar_to_json_string(copy, &schema).unwrap();
            assert_eq!(before, after, "shape {} {}", schema_str, json);

            // One block: the pool releases a value with a single shfree, so
            // everything below the root has to live inside it.
            shm::shfree(copy).unwrap();
            shm::shfree(src).unwrap();
        }
    }

    /// The copy owns nothing outside the block it returns, so releasing
    /// that block releases all of it.
    #[test]
    fn a_one_block_copy_leaves_no_blocks_behind() {
        let _shm = crate::own_test_registry();
        let mut hist = [0usize; 40];
        let cycle = || {
            for (schema_str, json) in SHAPES {
                let schema = parse_schema(schema_str).unwrap();
                let src = read_json_with_schema(json, &schema).unwrap();
                let copy = unsafe { deep_copy_to_block(src, &schema) }.unwrap();
                shm::shfree(copy).unwrap();
                shm::shfree(src).unwrap();
            }
        };
        cycle();
        let before = shm::live_block_stats(&mut hist).0;
        for _ in 0..4 {
            cycle();
        }
        assert_eq!(shm::live_block_stats(&mut hist).0, before);
    }
}



#[cfg(test)]
mod bounded_rebase_tests {
    use super::*;
    use morloc_runtime_types::schema::parse_schema;

    /// A 64-byte block holding one 16-byte slot at offset 0 whose pointer
    /// field (at `ptr_at`) holds the block-relative offset `rel`.
    fn block_with(ptr_at: usize, rel: i64, size: usize) -> AbsPtr {
        let block = shm::shcalloc(1, 64).unwrap();
        unsafe {
            *(block as *mut usize) = size;
            *(block.add(ptr_at) as *mut i64) = rel;
        }
        block
    }

    fn rebase(block: AbsPtr, schema: &str) -> Result<(), MorlocError> {
        let schema = parse_schema(schema).unwrap();
        let base = shm::abs2rel(block).unwrap();
        unsafe { adjust_relptrs_within(block, &schema, base, RelWindow::of_block(block, 64)?) }
    }

    #[test]
    fn string_inside_block_is_rebased() {
        let _g = crate::init_test_shm();
        let block = block_with(8, 16, 5);
        rebase(block, "s").unwrap();
        let arr = unsafe { &*(block as *const Array) };
        assert_eq!(shm::rel2abs(arr.data).unwrap(), unsafe { block.add(16) });
        shm::shfree(block).unwrap();
    }

    #[test]
    fn string_leaving_block_is_rejected() {
        let _g = crate::init_test_shm();
        let block = block_with(8, 1000, 5);
        assert!(rebase(block, "s").is_err());
        shm::shfree(block).unwrap();
    }

    #[test]
    fn string_running_past_block_end_is_rejected() {
        let _g = crate::init_test_shm();
        // Starts inside the block, but 60 bytes from offset 16 overrun it.
        let block = block_with(8, 16, 60);
        assert!(rebase(block, "s").is_err());
        shm::shfree(block).unwrap();
    }

    #[test]
    fn variant_payload_leaving_block_is_rejected() {
        let _g = crate::init_test_shm();
        // Tag 0 (arm with one f8 field), payload pointer far outside.
        let block = block_with(VARIANT_PAYLOAD_OFFSET, 4096, 0);
        assert!(rebase(block, "v11A1f8").is_err());
        shm::shfree(block).unwrap();
    }
}

// ── pointer_span ───────────────────────────────────────────────────────────

/// The address range `[lo, hi)` covering every byte addressed by a live
/// pointer reachable from `n` consecutive records of `schema` at `first`,
/// or `None` when no record holds a live pointer. `space` resolves the
/// relptrs found in the records, in whatever space the records live (a
/// mapped file, an SHM block).
///
/// A flat array contributes its whole data range without its elements
/// being visited, so the cost is one step per pointer slot, not per byte.
///
/// Every addressed range must lie inside `bound`, the `[lo, hi)` addresses
/// of the memory the records belong to; a pointer or extent reaching past
/// it is an error, checked before anything it addresses is read.
///
/// # Safety
/// `first` must address `n` records laid out as `schema` describes, inside
/// `bound`, which must be readable.
pub unsafe fn pointer_span<S: Space>(
    first: *const u8,
    n: usize,
    schema: &Schema,
    space: &S,
    bound: (usize, usize),
) -> Result<Option<(usize, usize)>, MorlocError> {
    let mut w = SpanWalk { res: Resolver::new(schema), space, bound, span: None };
    let mut st = Stack::new();
    for k in 0..n {
        st.enter(schema, first.add(k * schema.width), ());
        walk::run(&mut w, &mut st)?;
    }
    Ok(w.span)
}

/// True when `kind` occurs anywhere in `schema`.
pub fn schema_holds(schema: &Schema, kind: SerialType) -> bool {
    schema.serial_type == kind || schema.parameters.iter().any(|p| schema_holds(p, kind))
}

struct SpanWalk<'r, S> {
    res: Resolver<'r>,
    space: &'r S,
    bound: (usize, usize),
    span: Option<(usize, usize)>,
}

impl<'r, S: Space> SpanWalk<'r, S> {
    /// Resolve `rel` and widen the span by the `extent` bytes it addresses.
    fn cover(&mut self, rel: RelPtr, extent: Option<usize>) -> Result<*const u8, MorlocError> {
        let extent = extent.ok_or_else(|| {
            MorlocError::Serialization(format!("pointer extent at relptr {rel} overflows"))
        })?;
        let at = self.space.resolve(rel, extent)?;
        let lo = at as usize;
        let hi = lo.checked_add(extent).ok_or_else(|| {
            MorlocError::Serialization(format!("pointer extent at {lo:#x} overflows"))
        })?;
        if lo < self.bound.0 || hi > self.bound.1 {
            return Err(MorlocError::Serialization(format!(
                "pointer to [{lo:#x}, {hi:#x}) leaves its sub-packet [{:#x}, {:#x})",
                self.bound.0, self.bound.1
            )));
        }
        self.span = Some(match self.span {
            Some((a, b)) => (a.min(lo), b.max(hi)),
            None => (lo, hi),
        });
        Ok(at)
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

impl<'r, S: Space> Walker<()> for SpanWalk<'r, S> {
    fn step(&mut self, st: &mut Stack<()>, f: Frame<()>) -> Result<(), MorlocError> {
        // SAFETY: frames hold nodes of the tree the resolver was built from,
        // and `data` points at a value laid out as that schema describes.
        let s: &'r Schema = self.res.resolve(unsafe { &*f.schema })?;
        let data = f.data;
        unsafe {
            match s.serial_type {
                SerialType::Nil
                | SerialType::Bool
                | SerialType::Sint8
                | SerialType::Sint16
                | SerialType::Sint32
                | SerialType::Sint64
                | SerialType::Uint8
                | SerialType::Uint16
                | SerialType::Uint32
                | SerialType::Uint64
                | SerialType::Float32
                | SerialType::Float64
                | SerialType::Enum => {}
                SerialType::Int => {
                    // Inline BigInt: [size, value_or_relptr]; more than one
                    // limb lives behind the pointer.
                    let size = *(data as *const usize);
                    if size > 1 {
                        let rel = *(data.add(std::mem::size_of::<usize>()) as *const RelPtr);
                        self.cover(rel, size.checked_mul(std::mem::size_of::<u64>()))?;
                    }
                }
                SerialType::String | SerialType::Array => {
                    let arr = &*(data as *const Array);
                    if arr.size == 0 {
                        return Ok(());
                    }
                    let elem = s.parameters.first();
                    let w = elem.map_or(1, |e| e.width);
                    let elems = if f.idx == 0 {
                        self.cover(arr.data, arr.size.checked_mul(w))?
                    } else {
                        self.space.resolve(arr.data, region_len(arr.size, w)?)? as *const u8
                    };
                    let Some(elem) = elem else { return Ok(()) };
                    if elem.is_fixed_width() {
                        return Ok(());
                    }
                    for i in f.idx..arr.size {
                        if self.child(st, &f, i, elem, elems.add(i * w))? == Visit::Deferred {
                            return Ok(());
                        }
                    }
                }
                SerialType::IFile | SerialType::OStream | SerialType::IStream => {
                    // Only a path-form handle points anywhere: at an 8-byte
                    // length followed by the path's bytes.
                    use morloc_runtime_types::stream_handle as sh;
                    if sh::read_tag(data) == sh::TAG_PATH {
                        let payload = sh::read_payload(data);
                        if payload != sh::RELNULL_PAYLOAD {
                            let at = self.cover(sh::payload_relptr(payload), Some(8))?;
                            let len = usize::try_from(*(at as *const u64)).ok();
                            self.cover(sh::payload_relptr(payload), len.and_then(|n| n.checked_add(8)))?;
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
                    if f.idx > 0 {
                        return Ok(());
                    }
                    let arm = variant_arm_schema(*data, s)?;
                    let rel = *(data.add(VARIANT_PAYLOAD_OFFSET) as *const RelPtr);
                    if rel != shm::RELNULL {
                        let inner = self.cover(rel, Some(arm.width))?;
                        self.child(st, &f, 0, arm, inner)?;
                    }
                }
                SerialType::Optional => {
                    if f.idx > 0 || s.parameters.is_empty() {
                        return Ok(());
                    }
                    let rel = *(data as *const RelPtr);
                    if rel != shm::RELNULL {
                        let inner = self.cover(rel, Some(s.parameters[0].width))?;
                        self.child(st, &f, 0, &s.parameters[0], inner)?;
                    }
                }
                SerialType::Table => {
                    return Err(MorlocError::Serialization(
                        "pointer_span: an Arrow table has no voidstar pointer layout".into(),
                    ));
                }
                SerialType::Recur => {
                    return Err(MorlocError::Schema(
                        "pointer_span: unresolved back-reference".into(),
                    ));
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod flatten_size_tests {
    use super::*;
    use crate::json::read_json_with_schema;
    use morloc_runtime_types::schema::parse_schema;

    /// A path-form stream handle after a field that leaves the cursor
    /// unaligned: flatten pads the path to 8 bytes, so the size it was
    /// given must have budgeted that padding.
    #[test]
    fn handle_path_after_odd_string_flattens() {
        let _g = crate::init_test_shm();
        for s in ["x", "abc", "abcdefg", "abcdefgh"] {
            let schema = parse_schema("t2sF").unwrap();
            let v = read_json_with_schema(&format!(r#"["{s}", "/tmp/some/path.stream"]"#), &schema).unwrap();
            let flat = flatten_to_buffer(v, &schema)
                .unwrap_or_else(|e| panic!("string {s:?}: {e}"));
            let back = read_binary(&flat, &schema).unwrap();
            assert_eq!(
                crate::json::voidstar_to_json_string(back, &schema).unwrap(),
                crate::json::voidstar_to_json_string(v, &schema).unwrap(),
            );
        }
    }
}
