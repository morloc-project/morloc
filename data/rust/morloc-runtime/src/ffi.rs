//! C ABI wrappers for libmorloc.so
//!
//! These `extern "C"` functions match the signatures in morloc.h.
//! Internally they call the Rust implementations and convert between
//! Rust Result<T,E> and the C ERRMSG convention (char** last arg).

use std::ffi::{c_char, c_void, CStr, CString};
use std::ptr;

use crate::error::{clear_errmsg, set_errmsg, MorlocError};
use crate::schema::{self};
use crate::shm::{self, AbsPtr, RelPtr, ShmHeader};
pub use crate::cschema::CSchema;

/// The ABI/wire-format version compiled into this libmorloc.so (see morloc.h's
/// `MORLOC_ABI_VERSION`). Provisioning compares it fail-closed against the
/// compiler's expected version so a mismatched prebuilt binary is refused.
pub(crate) fn morloc_abi_version() -> u32 {
    morloc_runtime_types::MORLOC_ABI_VERSION
}

/// LIFE-6: the build id of this libmorloc.
pub(crate) fn morloc_build_id() -> u64 {
    morloc_runtime_types::BUILD_ID
}

/// LIFE-6: end this process with EXIT_BUILD_SKEW when the nexus that started
/// it, or the installed binding that calls this (`binding`, 0 for code
/// compiled into the program), belongs to another build.
pub(crate) fn morloc_require_build(binding: u64) {
    let nexus = std::env::var(morloc_runtime_types::BUILD_ID_ENV).ok();
    let binding = (binding != 0).then_some(binding);
    if let Some(why) = morloc_runtime_types::build_skew(morloc_build_id(), binding, nexus.as_deref()) {
        let line = format!("Error: {why}\n");
        unsafe {
            libc::write(2, line.as_ptr() as *const c_void, line.len());
            libc::_exit(morloc_runtime_types::EXIT_BUILD_SKEW);
        }
    }
}

// ── Macro for ERRMSG-pattern FFI wrappers ──────────────────────────────────

/// Wrap a Rust Result-returning expression into the C ERRMSG convention.
/// On success: clears errmsg, returns the value.
/// On error: sets errmsg, returns $fail.
macro_rules! ffi_try {
    ($errmsg:expr, $fail:expr, $body:expr) => {{
        unsafe { clear_errmsg($errmsg) };
        match $body {
            Ok(val) => val,
            Err(e) => {
                unsafe { set_errmsg($errmsg, &e) };
                $fail
            }
        }
    }};
}

// CSchema type and conversions are in cschema.rs (always compiled).

// ── SHM functions ──────────────────────────────────────────────────────────

pub(crate) unsafe fn shinit(
    shm_basename: *const c_char,
    shm_size: usize,
    errmsg: *mut *mut c_char,
) -> *mut ShmHeader {
    let basename = CStr::from_ptr(shm_basename).to_string_lossy();
    ffi_try!(errmsg, ptr::null_mut(), shm::shinit(&basename, shm_size))
}

pub(crate) unsafe fn shclose(errmsg: *mut *mut c_char) -> bool {
    ffi_try!(errmsg, false, shm::shclose().map(|_| true))
}

// DAEMON-5
pub(crate) unsafe fn morloc_shretire(errmsg: *mut *mut c_char) -> bool {
    ffi_try!(errmsg, false, shm::shretire().map(|_| true))
}

/// Initialise the shared stream registry for the current nexus
/// invocation. Allocates (or attaches to) the registry's SHM volume
/// and publishes the magic gate. Safe to call multiple times --
/// subsequent calls observe the cached state and return immediately.
///
/// Pool processes that attach to an already-initialised session
/// can also call this; the bootstrap CAS handles concurrent callers
/// safely.
///
/// Returns the slot count in effect, or `usize::MAX` on error (with
/// `errmsg` populated).
pub(crate) unsafe fn stream_registry_init(
    errmsg: *mut *mut c_char,
) -> usize {
    ffi_try!(errmsg, usize::MAX, crate::stream::registry_init())
}

pub(crate) unsafe fn shm_set_fallback_dir(dir: *const c_char) {
    if !dir.is_null() {
        let d = CStr::from_ptr(dir).to_string_lossy();
        shm::shm_set_fallback_dir(&d);
    }
}

pub(crate) unsafe fn shmalloc(size: usize, errmsg: *mut *mut c_char) -> *mut c_void {
    ffi_try!(errmsg, ptr::null_mut(), shm::shmalloc(size).map(|p| p as *mut c_void))
}

/// Report blocks currently held across every mapped volume. Writes the
/// block count and byte total through the out pointers, and prints a
/// size-class breakdown to stderr.
pub(crate) unsafe fn mlc_shm_live_report(
    out_blocks: *mut usize,
    out_bytes: *mut usize,
) {
    let mut hist = [0usize; 40];
    let (blocks, bytes) = shm::live_block_stats(&mut hist);
    if !out_blocks.is_null() {
        *out_blocks = blocks;
    }
    if !out_bytes.is_null() {
        *out_bytes = bytes;
    }
    eprintln!("shm live: {} blocks, {} bytes", blocks, bytes);
    for (i, n) in hist.iter().enumerate() {
        if *n > 0 {
            eprintln!("  size < 2^{:<2} : {} blocks", i, n);
        }
    }
}

pub(crate) unsafe fn shfree(ptr: *mut c_void, errmsg: *mut *mut c_char) -> bool {
    ffi_try!(errmsg, false, shm::shfree(ptr as AbsPtr).map(|_| true))
}

pub(crate) unsafe fn shincref(ptr: *mut c_void, errmsg: *mut *mut c_char) -> bool {
    ffi_try!(errmsg, false, shm::shincref(ptr as AbsPtr).map(|_| true))
}

pub(crate) fn morloc_fork_generation() -> u64 {
    crate::fork_policy::generation()
}

pub(crate) unsafe fn total_shm_size() -> usize {
    shm::total_shm_size()
}

pub(crate) unsafe fn rel2abs(ptr: RelPtr, errmsg: *mut *mut c_char) -> *mut c_void {
    ffi_try!(errmsg, ptr::null_mut(), shm::rel2abs(ptr).map(|p| p as *mut c_void))
}

pub(crate) unsafe fn rel2abs_extent(ptr: RelPtr, extent: usize, errmsg: *mut *mut c_char) -> *mut c_void {
    ffi_try!(errmsg, ptr::null_mut(), shm::rel2abs_extent(ptr, extent).map(|p| p as *mut c_void))
}

// PANIC-14: a pool's defect exit asks this before it ends with 70.
pub(crate) unsafe fn morloc_exit_if_shared_memory_lost(msg: *const c_char) {
    if !msg.is_null() {
        shm::exit_if_shared_memory_lost(&CStr::from_ptr(msg).to_string_lossy());
    }
}

/// The error C's inline payload resolve reports. Always returns null.
pub(crate) unsafe fn morloc_payload_region_error(
    relptr: RelPtr,
    extent: usize,
    len: usize,
    errmsg: *mut *mut c_char,
) -> *mut c_void {
    set_errmsg(errmsg, &crate::voidstar::payload_region_error(relptr, extent, len));
    ptr::null_mut()
}

pub(crate) unsafe fn abs2rel(ptr: *mut c_void, errmsg: *mut *mut c_char) -> RelPtr {
    ffi_try!(errmsg, shm::RELNULL, shm::abs2rel(ptr as AbsPtr))
}

// ── Schema functions ───────────────────────────────────────────────────────

pub(crate) unsafe fn parse_schema(
    schema_str: *const c_char,
    errmsg: *mut *mut c_char,
) -> *mut CSchema {
    clear_errmsg(errmsg);
    if schema_str.is_null() {
        set_errmsg(errmsg, &MorlocError::NullPointer);
        return ptr::null_mut();
    }
    match c_schema(&CStr::from_ptr(schema_str).to_string_lossy()) {
        Ok(schema) => schema,
        Err(e) => {
            set_errmsg(errmsg, &e);
            ptr::null_mut()
        }
    }
}

/// A C schema for `text`, owned by the caller.
pub(crate) fn c_schema(text: &str) -> Result<*mut CSchema, MorlocError> {
    Ok(CSchema::from_rust(&schema::parse_schema(text)?))
}

pub(crate) unsafe fn schema_to_string(schema: *const CSchema) -> *mut c_char {
    if schema.is_null() {
        return ptr::null_mut();
    }
    match CString::new(CSchema::converted(schema).text()) {
        Ok(cs) => crate::error::malloc_cstr(&cs),
        Err(_) => ptr::null_mut(),
    }
}

pub(crate) unsafe fn free_schema(schema: *mut CSchema) {
    CSchema::free(schema);
}

/// Narrow `v` to the nearest `f32` at `out`, for the pool marshallers: 0 on
/// success, -1 when a finite `v` lies beyond the `f32` range.
pub(crate) unsafe fn morloc_f32_from_f64(v: f64, out: *mut f32) -> i32 {
    match morloc_runtime_types::width::f32_nearest(v) {
        Ok(f) => {
            *out = f;
            0
        }
        Err(_) => -1,
    }
}

// Hash: morloc_xxh64 is provided by utility.c (via xxhash.h inline)

// ── Serialization ──────────────────────────────────────────────────────────

pub(crate) unsafe fn pack_with_schema(
    mlc: *const c_void,
    schema: *const CSchema,
    mpkptr: *mut *mut c_char,
    mpk_size: *mut usize,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    *mpkptr = ptr::null_mut();
    *mpk_size = 0;

    let rs = crate::packet_ffi::rust_schema(schema);
    match crate::mpack::pack_with_schema(mlc as AbsPtr, &rs) {
        Ok(data) => {
            *mpk_size = data.len();
            let buf = libc::malloc(data.len()) as *mut u8;
            if buf.is_null() {
                set_errmsg(errmsg, &MorlocError::Shm("malloc failed".into()));
                return 1;
            }
            std::ptr::copy_nonoverlapping(data.as_ptr(), buf, data.len());
            *mpkptr = buf as *mut c_char;
            0
        }
        Err(e) => {
            set_errmsg(errmsg, &e);
            1
        }
    }
}

pub(crate) unsafe fn pack(
    mlc: *const c_void,
    schema_str: *const c_char,
    mpkptr: *mut *mut c_char,
    mpk_size: *mut usize,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    let s = CStr::from_ptr(schema_str).to_string_lossy();
    let schema = match schema::parse_schema(&s) {
        Ok(s) => s,
        Err(e) => {
            set_errmsg(errmsg, &e);
            return 1;
        }
    };
    let cs = CSchema::from_rust(&schema);
    let result = pack_with_schema(mlc, cs, mpkptr, mpk_size, errmsg);
    free_schema(cs);
    result
}

pub(crate) unsafe fn unpack_with_schema(
    mpk: *const c_char,
    mpk_size: usize,
    schema: *const CSchema,
    mlcptr: *mut *mut c_void,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    *mlcptr = ptr::null_mut();

    let data = std::slice::from_raw_parts(mpk as *const u8, mpk_size);
    let rs = crate::packet_ffi::rust_schema(schema);
    match crate::mpack::unpack_with_schema(data, &rs) {
        Ok(ptr) => {
            *mlcptr = ptr as *mut c_void;
            0
        }
        Err(e) => {
            set_errmsg(errmsg, &e);
            1
        }
    }
}

// quoted, print_voidstar, pretty_print_voidstar, read_json_with_schema
// are provided by json.c

// ── Schema utility functions needed by C code ──────────────────────────────

/// The size of the value at `data`, and an upper bound on the number of
/// sub-allocations a deep copy of it takes.
///
/// For a caller that copies into ONE block with a [`crate::voidstar::Bump`]:
/// the bump rounds every part up to eight bytes, so the region has to carry
/// up to seven bytes of padding per part, and the part count is not known
/// before the value is walked. This walks it once and reports both, which is
/// what lets such a caller size exactly and then copy once rather than
/// building the value in per-part blocks and consolidating them.
pub fn calc_voidstar_layout(
    data: *const u8,
    schema: &crate::schema::Schema,
) -> Result<(usize, usize), MorlocError> {
    let res = crate::recur::Resolver::new(schema);
    let mut w = SizeWalk {
        res: &res,
        total: 0,
        bound: usize::MAX,
        steps: 0,
        portable: false,
    };
    let mut st = crate::walk::Stack::new();
    st.enter(schema, data, false);
    crate::walk::run(&mut w, &mut st)?;
    Ok((w.total, w.steps))
}

/// The size of the value's portable flat form, in which each handle-form
/// stream field is written as its path; see
/// [`crate::voidstar::flatten_into_portable`].
pub fn calc_voidstar_size_portable(
    data: *const u8,
    schema: &crate::schema::Schema,
) -> Result<usize, MorlocError> {
    calc_voidstar_size_portable_with(data, schema, &crate::recur::Resolver::new(schema))
}

/// [`calc_voidstar_size_portable`] with a resolver of `schema` the caller
/// built, for a caller sizing many values of one schema.
pub fn calc_voidstar_size_portable_with(
    data: *const u8,
    schema: &crate::schema::Schema,
    res: &crate::recur::Resolver<'_>,
) -> Result<usize, MorlocError> {
    let mut w = SizeWalk {
        res,
        total: 0,
        bound: usize::MAX,
        steps: 0,
        portable: true,
    };
    let mut st = crate::walk::Stack::new();
    st.enter(schema, data, false);
    crate::walk::run(&mut w, &mut st)?;
    Ok(w.total)
}

pub fn calc_voidstar_size_inner(
    data: *const u8,
    schema: &crate::schema::Schema,
) -> Result<usize, MorlocError> {
    calc_voidstar_size_bounded(data, schema, usize::MAX)
}

/// Compute the serialized size of the voidstar at `data` under `schema`,
/// short-circuiting as soon as the accumulated total exceeds
/// `upper_bound`. Pass `usize::MAX` (or call [`calc_voidstar_size_inner`])
/// for the exact size; pass a real bound when the caller only needs
/// "size > bound?" -- e.g. the inline-vs-RPTR or streaming-threshold
/// routing decisions. Bounded mode visits O(bound) bytes of structure
/// instead of O(payload), turning multi-GiB voidstars from ~17s walks
/// into microseconds at the threshold.
///
/// On early-exit the return is the partial running total at the point
/// the bound was exceeded, which is guaranteed `> upper_bound` and so
/// preserves the `size > threshold` invariant the caller is testing.
pub fn calc_voidstar_size_bounded(
    data: *const u8,
    schema: &crate::schema::Schema,
    upper_bound: usize,
) -> Result<usize, MorlocError> {
    let res = crate::recur::Resolver::new(schema);
    let mut w = SizeWalk {
        res: &res,
        total: 0,
        bound: upper_bound,
        steps: 0,
        portable: false,
    };
    let mut st = crate::walk::Stack::new();
    st.enter(schema, data, false);
    crate::walk::run(&mut w, &mut st)?;
    Ok(w.total)
}

use crate::walk::Walker as _;

/// The size walk. A frame's `x` says whether the parent already counted
/// this node's slot (a tuple or record counts its whole fixed layout up
/// front), in which case only the bytes beyond the slot are added.
struct SizeWalk<'a, 'r> {
    res: &'a crate::recur::Resolver<'r>,
    total: usize,
    bound: usize,
    /// Nodes stepped. An upper bound on the sub-allocations a deep copy of
    /// this value takes: every allocation site in `deep_copy_alloc` sits in
    /// a node's own arm and at most one of them fires per step. A caller
    /// laying the value out in one block needs that bound to budget the
    /// per-part alignment padding; see [`calc_voidstar_layout`].
    steps: usize,
    /// Count each handle-form stream field as the path it will be written
    /// as, for the portable flat form.
    portable: bool,
}

impl<'a, 'r> SizeWalk<'a, 'r> {
    #[inline]
    fn add(&mut self, n: usize) {
        self.total = self.total.saturating_add(n);
    }

    /// Visit a child: in place when nothing below it can recurse, else on
    /// the stack beneath the parent's continuation.
    fn child(
        &mut self,
        st: &mut crate::walk::Stack<bool>,
        f: &crate::walk::Frame<bool>,
        idx: usize,
        s: &'r crate::schema::Schema,
        data: *const u8,
        slot_counted: bool,
    ) -> Result<crate::walk::Visit, MorlocError> {
        if self.res.flat(s) {
            self.step(st, crate::walk::Frame::new(s, data, slot_counted))?;
            Ok(crate::walk::Visit::Done)
        } else {
            crate::walk::defer(self, st, f, idx, s, data, slot_counted);
            Ok(crate::walk::Visit::Deferred)
        }
    }
}

impl<'a, 'r> crate::walk::Walker<bool> for SizeWalk<'a, 'r> {
    fn step(&mut self, st: &mut crate::walk::Stack<bool>, f: crate::walk::Frame<bool>) -> Result<(), MorlocError> {
        use crate::schema::SerialType;
        use crate::shm::{self, Array};
        use crate::walk::Visit;

        // In bounded mode the walk ends as soon as the running total is
        // past the bound; the total stays above it, which is all a caller
        // testing `size > bound` needs.
        if self.total > self.bound {
            st.clear();
            return Ok(());
        }
        self.steps = self.steps.saturating_add(1);
        // SAFETY: frames hold nodes of the tree the resolver was built
        // from, which outlives the walk; `data` points at a value laid out
        // as that schema describes.
        let s: &'r crate::schema::Schema = self.res.resolve(unsafe { &*f.schema })?;
        let data = f.data;
        let slot = if f.x { s.width } else { 0 };
        unsafe {
            match s.serial_type {
                SerialType::Int => {
                    // Inline BigInt: 16 bytes for size <= 1, extra limbs for overflow
                    let size = *(data as *const usize);
                    let own = if size <= 1 {
                        16 // inline: [size, value]
                    } else {
                        16 + std::mem::align_of::<u64>().saturating_sub(1)
                            + size * std::mem::size_of::<u64>()
                    };
                    self.add(own - slot);
                }
                SerialType::String => {
                    let arr = &*(data as *const Array);
                    self.add(std::mem::size_of::<Array>() + arr.size - slot);
                }
                SerialType::IFile | SerialType::OStream | SerialType::IStream => {
                    // Tagged stream-handle field: 16-byte inline + path
                    // suballoc (`8 + path_len`) for TAG_PATH, plus the
                    // worst-case padding the flatten inserts to 8-align it;
                    // no suballoc for TAG_HANDLE.
                    use morloc_runtime_types::stream_handle as sh;
                    let field = data as *const u8;
                    let mut own = sh::STREAM_HANDLE_FIELD_SIZE;
                    if self.portable && sh::read_tag(field) == sh::TAG_HANDLE {
                        let path = crate::handle_scan::portable_path(sh::payload_handle(sh::read_payload(field)))?;
                        if !path.is_empty() {
                            own += sh::path_suballoc_size(path.len())
                                + std::mem::align_of::<u64>() - 1;
                        }
                    } else if sh::read_tag(field) == sh::TAG_PATH {
                        if let Some(block) = crate::voidstar::path_suballoc(&crate::voidstar::Arena, sh::read_payload(field))? {
                            own += block.len() + std::mem::align_of::<u64>() - 1;
                        }
                    }
                    self.add(own - slot);
                }
                SerialType::Array => {
                    let arr = &*(data as *const Array);
                    let elem_schema = &s.parameters[0];
                    let elem_width = elem_schema.width;
                    if f.idx == 0 {
                        let mut own = std::mem::size_of::<Array>();
                        if arr.size == 0 {
                            self.add(own - slot);
                            return Ok(());
                        }
                        // bumps to 64 for primitive numerics for SIMD/BLAS
                        own += elem_schema.array_data_alignment().saturating_sub(1);
                        // A flat data region (fixed-width element) sizes to a
                        // single multiply; a variable element must be walked.
                        // See Schema::array_data_is_flat -- do NOT test
                        // schema.is_fixed_width() on the Array node, which
                        // silently forces the per-element path.
                        //
                        // Saturating: a corrupt or hostile `arr.size` must not
                        // wrap the product to a small value and misroute a huge
                        // array as inline; saturating to usize::MAX keeps it
                        // "oversized".
                        if s.array_data_is_flat() {
                            own = own.saturating_add(elem_width.saturating_mul(arr.size));
                            self.add(own - slot);
                            return Ok(());
                        }
                        self.add(own - slot);
                    }
                    if s.array_data_is_flat() {
                        return Ok(());
                    }
                    let elem_data = shm::rel2abs(arr.data)?;
                    let flat_elem = self.res.flat(elem_schema);
                    for i in f.idx..arr.size {
                        if self.total > self.bound {
                            st.clear();
                            return Ok(());
                        }
                        let p = elem_data.add(i * elem_width);
                        if flat_elem {
                            self.step(st, crate::walk::Frame::new(elem_schema, p, false))?;
                        } else if self.child(st, &f, i, elem_schema, p, false)? == Visit::Deferred {
                            return Ok(());
                        }
                    }
                }
                SerialType::Variant => {
                    // A resumed visit of a one-child node has nothing left.
                    if f.idx > 0 {
                        return Ok(());
                    }
                    // A variant slot is a tag plus a relptr to the arm's
                    // fields, so its flattened size is the slot, worst-case
                    // padding before the payload, and the payload's own total.
                    // A nullary arm has no payload and needs only the slot.
                    let tag = *data;
                    let arm = s.parameters.get(tag as usize).ok_or_else(|| {
                        MorlocError::Serialization(format!(
                            "variant tag {} is out of range; the type has {} arms",
                            tag, s.size
                        ))
                    })?;
                    let relptr = *(data.add(8) as *const shm::RelPtr);
                    if relptr == shm::RELNULL {
                        self.add(s.width - slot);
                    } else {
                        let payload = shm::rel2abs(relptr)?;
                        let align = arm.alignment().max(1);
                        self.add(s.width.saturating_add(align - 1) - slot);
                        self.child(st, &f, 0, arm, payload, false)?;
                    }
                }
                SerialType::Optional => {
                    if f.idx > 0 {
                        return Ok(());
                    }
                    // The slot is a single relptr. RELNULL: no payload;
                    // otherwise the slot, worst-case alignment padding before
                    // the inner T (the flatten / packer aligns the cursor
                    // before writing T at it), and T's full size.
                    let relptr = *(data as *const shm::RelPtr);
                    if relptr == shm::RELNULL {
                        self.add(s.width - slot);
                    } else {
                        let inner_data = shm::rel2abs(relptr)?;
                        let inner = &s.parameters[0];
                        let inner_align = inner.alignment().max(1);
                        self.add(s.width.saturating_add(inner_align - 1) - slot);
                        self.child(st, &f, 0, inner, inner_data, false)?;
                    }
                }
                SerialType::Tuple | SerialType::Map => {
                    if f.idx == 0 {
                        self.add(s.width - slot);
                    }
                    if s.is_fixed_width() {
                        return Ok(());
                    }
                    // Every field's slot is inside `s.width`; a fixed-width
                    // field adds nothing beyond it.
                    for i in f.idx..s.parameters.len() {
                        if self.total > self.bound {
                            st.clear();
                            return Ok(());
                        }
                        let field = &s.parameters[i];
                        if field.is_fixed_width() {
                            continue;
                        }
                        if self.child(st, &f, i, field, data.add(s.offsets[i]), true)? == Visit::Deferred {
                            return Ok(());
                        }
                    }
                }
                SerialType::Table => {
                    // A table's flat form is its block, header included.
                    let own = crate::arrow_shm::block_size(data as *const crate::arrow_shm::ArrowShmHeader)?;
                    self.add(own - slot);
                }
                SerialType::Nil | SerialType::Bool | SerialType::Sint8 | SerialType::Sint16 | SerialType::Sint32 | SerialType::Sint64 | SerialType::Uint8 | SerialType::Uint16 | SerialType::Uint32 | SerialType::Uint64 | SerialType::Float32 | SerialType::Float64 | SerialType::Recur | SerialType::Enum => self.add(s.width - slot),
            }
        }
        Ok(())
    }
}

mod c_abi {
    use super::*;

    #[no_mangle]
    pub extern "C" fn morloc_abi_version() -> u32 {
        super::morloc_abi_version()
    }

    #[no_mangle]
    pub extern "C" fn morloc_build_id() -> u64 {
        super::morloc_build_id()
    }

    #[no_mangle]
    pub extern "C" fn morloc_require_build(binding: u64) {
        super::morloc_require_build(binding)
    }

    #[no_mangle]
    pub unsafe extern "C" fn shinit(shm_basename: *const c_char, shm_size: usize, errmsg: *mut *mut c_char) -> *mut ShmHeader {
        super::shinit(shm_basename, shm_size, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn shclose(errmsg: *mut *mut c_char) -> bool {
        super::shclose(errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn morloc_shretire(errmsg: *mut *mut c_char) -> bool {
        super::morloc_shretire(errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn stream_registry_init(errmsg: *mut *mut c_char) -> usize {
        super::stream_registry_init(errmsg)
    }

    /// SLOT-33: `level` 255 means the `@write` level. 0 or 1 with `errmsg` set.
    #[no_mangle]
    pub unsafe extern "C" fn mlc_set_stdout_level(level: u8, errmsg: *mut *mut c_char) -> i32 {
        crate::error::clear_errmsg(errmsg);
        match crate::stream::set_stdout_level((level != 255).then_some(level)) {
            Ok(()) => 0,
            Err(e) => {
                crate::error::set_errmsg(errmsg, &e);
                1
            }
        }
    }

    #[no_mangle]
    pub unsafe extern "C" fn shm_set_fallback_dir(dir: *const c_char) {
        super::shm_set_fallback_dir(dir)
    }

    #[no_mangle]
    pub unsafe extern "C" fn shmalloc(size: usize, errmsg: *mut *mut c_char) -> *mut c_void {
        super::shmalloc(size, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn mlc_shm_live_report(out_blocks: *mut usize, out_bytes: *mut usize) {
        super::mlc_shm_live_report(out_blocks, out_bytes)
    }

    #[no_mangle]
    pub unsafe extern "C" fn shfree(ptr: *mut c_void, errmsg: *mut *mut c_char) -> bool {
        super::shfree(ptr, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn shincref(ptr: *mut c_void, errmsg: *mut *mut c_char) -> bool {
        super::shincref(ptr, errmsg)
    }

    #[no_mangle]
    pub extern "C" fn morloc_fork_generation() -> u64 {
        super::morloc_fork_generation()
    }

    #[no_mangle]
    pub unsafe extern "C" fn total_shm_size() -> usize {
        super::total_shm_size()
    }

    #[no_mangle]
    pub unsafe extern "C" fn morloc_exit_if_shared_memory_lost(msg: *const c_char) {
        super::morloc_exit_if_shared_memory_lost(msg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn rel2abs(ptr: RelPtr, errmsg: *mut *mut c_char) -> *mut c_void {
        super::rel2abs(ptr, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn rel2abs_extent(ptr: RelPtr, extent: usize, errmsg: *mut *mut c_char) -> *mut c_void {
        super::rel2abs_extent(ptr, extent, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn morloc_payload_region_error(relptr: RelPtr, extent: usize, len: usize, errmsg: *mut *mut c_char) -> *mut c_void {
        super::morloc_payload_region_error(relptr, extent, len, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn abs2rel(ptr: *mut c_void, errmsg: *mut *mut c_char) -> RelPtr {
        super::abs2rel(ptr, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn parse_schema(schema_str: *const c_char, errmsg: *mut *mut c_char) -> *mut CSchema {
        super::parse_schema(schema_str, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn schema_to_string(schema: *const CSchema) -> *mut c_char {
        super::schema_to_string(schema)
    }

    #[no_mangle]
    pub unsafe extern "C" fn free_schema(schema: *mut CSchema) {
        super::free_schema(schema)
    }

    #[no_mangle]
    pub unsafe extern "C" fn morloc_f32_from_f64(v: f64, out: *mut f32) -> i32 {
        super::morloc_f32_from_f64(v, out)
    }

    #[no_mangle]
    pub unsafe extern "C" fn pack_with_schema(mlc: *const c_void, schema: *const CSchema, mpkptr: *mut *mut c_char, mpk_size: *mut usize, errmsg: *mut *mut c_char) -> i32 {
        super::pack_with_schema(mlc, schema, mpkptr, mpk_size, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn pack(mlc: *const c_void, schema_str: *const c_char, mpkptr: *mut *mut c_char, mpk_size: *mut usize, errmsg: *mut *mut c_char) -> i32 {
        super::pack(mlc, schema_str, mpkptr, mpk_size, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn unpack_with_schema(mpk: *const c_char, mpk_size: usize, schema: *const CSchema, mlcptr: *mut *mut c_void, errmsg: *mut *mut c_char) -> i32 {
        super::unpack_with_schema(mpk, mpk_size, schema, mlcptr, errmsg)
    }

}

#[cfg(test)]
mod build_id_tests {
    const PROBE: &str = "MORLOC_TEST_BUILD_PROBE";

    fn probe(how: &str, nexus: Option<String>) -> std::process::Output {
        let mut cmd = std::process::Command::new(std::env::current_exe().unwrap());
        cmd.args(["--exact", "ffi::build_id_tests::a_pool_refuses_another_build", "--nocapture", "--test-threads=1"])
            .env(PROBE, how)
            .env_remove(morloc_runtime_types::BUILD_ID_ENV);
        if let Some(n) = nexus {
            cmd.env(morloc_runtime_types::BUILD_ID_ENV, n);
        }
        cmd.output().unwrap()
    }

    #[test]
    fn a_pool_refuses_another_build() {
        if let Ok(how) = std::env::var(PROBE) {
            let id = super::morloc_build_id();
            super::morloc_require_build(match how.as_str() {
                "binding" => id ^ 1,
                "compiled" => 0,
                _ => id,
            });
            std::process::exit(0);
        }
        let id = super::morloc_build_id();
        assert_eq!(id, morloc_runtime_types::BUILD_ID);
        for ok in [probe("same", Some(format!("{id:016x}"))), probe("same", None), probe("compiled", Some(format!("{id:016x}")))] {
            assert_eq!(ok.status.code(), Some(0), "{ok:?}");
        }
        let other = format!("{:016x}", id ^ 1);
        for out in [probe("binding", None), probe("same", Some(other.clone())), probe("compiled", Some(other))] {
            assert_eq!(out.status.code(), Some(morloc_runtime_types::EXIT_BUILD_SKEW), "{out:?}");
            let err = String::from_utf8_lossy(&out.stderr);
            assert_eq!(err.lines().filter(|l| l.starts_with("Error: ")).count(), 1, "{err}");
        }
    }

    #[test]
    fn every_binding_checks_the_build_it_was_made_with() {
        let repo = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../..");
        let read = |p: &str| std::fs::read_to_string(repo.join(p)).unwrap();
        for (path, call) in [
            ("data/lang/cpp/cppmorloc.cpp", "morloc_require_build(0)"),
            ("data/lang/py/pymorloc.c", "morloc_require_build(MORLOC_BINDING_BUILD_ID)"),
            ("data/lang/r/rmorloc.c", "morloc_require_build(MORLOC_BINDING_BUILD_ID)"),
            ("data/lang/rust/pool.rs", "::rustmorloc::require_build();"),
        ] {
            assert!(read(path).contains(call), "LIFE-6: {path} must call {call}");
        }
        assert!(!read("data/lang/cpp/init.sh").contains("BUILD_ID"), "LIFE-6: a compiled-in binding records no build id");
        for path in ["data/lang/py/init.sh", "data/lang/r/init.sh"] {
            let text = read(path);
            assert!(
                text.contains("--build-id") && text.contains("-DMORLOC_BINDING_BUILD_ID=0x${BUILD_ID}ULL"),
                "LIFE-6: {path} must build its binding with the installed build id"
            );
        }
    }
}
