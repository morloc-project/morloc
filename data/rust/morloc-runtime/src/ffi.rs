//! C ABI wrappers for libmorloc.so
//!
//! These `extern "C"` functions match the signatures in morloc.h.
//! Internally they call the Rust implementations and convert between
//! Rust Result<T,E> and the C ERRMSG convention (char** last arg).

use std::ffi::{c_char, c_void, CStr, CString};
use std::ptr;

use crate::error::{clear_errmsg, set_errmsg, MorlocError};
use crate::schema::{self};
use crate::shm::{self, AbsPtr, BlockHeader, RelPtr, ShmHeader, VolPtr};
pub use crate::cschema::CSchema;

/// The ABI/wire-format version compiled into this libmorloc.so (see morloc.h's
/// `MORLOC_ABI_VERSION`). Provisioning compares it fail-closed against the
/// compiler's expected version so a mismatched prebuilt binary is refused.
#[no_mangle]
pub extern "C" fn morloc_abi_version() -> u32 {
    morloc_runtime_types::MORLOC_ABI_VERSION
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

#[no_mangle]
pub unsafe extern "C" fn shinit(
    shm_basename: *const c_char,
    volume_index: usize,
    shm_size: usize,
    errmsg: *mut *mut c_char,
) -> *mut ShmHeader {
    let basename = CStr::from_ptr(shm_basename).to_string_lossy();
    ffi_try!(errmsg, ptr::null_mut(), shm::shinit(&basename, volume_index, shm_size))
}

#[no_mangle]
pub unsafe extern "C" fn shopen(
    volume_index: usize,
    errmsg: *mut *mut c_char,
) -> *mut ShmHeader {
    ffi_try!(
        errmsg,
        ptr::null_mut(),
        shm::shopen(volume_index).and_then(|opt| opt.ok_or(MorlocError::Shm("volume not found".into())))
    )
}

#[no_mangle]
pub unsafe extern "C" fn shclose(errmsg: *mut *mut c_char) -> bool {
    ffi_try!(errmsg, false, shm::shclose().map(|_| true))
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
#[no_mangle]
pub unsafe extern "C" fn stream_registry_init(
    errmsg: *mut *mut c_char,
) -> usize {
    ffi_try!(errmsg, usize::MAX, crate::stream::registry_init())
}

/// Enqueue a PID-sweep request to the dedicated sweeper thread.
/// Used by the nexus when it detects a pool has died (clean exit or
/// crash) so any slots that pool left open in the shared registry
/// are released without waiting for the per-call sweep (which only
/// fires when a dispatch ends normally).
///
/// `pid` and `start_time` together identify a unique process across
/// PID reuse. The sweeper walks the registry and discards slots whose
/// `(opener_pid, opener_pid_start_time)` matches both. Pass
/// `start_time = 0` to skip the start-time check (accepts the rare
/// PID-reuse false positive).
///
/// Non-blocking; the actual sweep runs on the sweeper thread.
#[no_mangle]
pub unsafe extern "C" fn stream_sweep_pid(
    pid: u32,
    start_time: u64,
) {
    crate::stream::sweeper_enqueue_pid(pid, start_time);
}

/// Read this process's start time from `/proc/PID/stat` (field 22,
/// clock ticks since boot). Used by the nexus to capture the
/// start_time of each spawned pool, which it then pairs with the
/// PID when enqueueing a PID sweep. Returns 0 on read failure (the
/// sweep will then accept a PID-only match).
#[no_mangle]
pub unsafe extern "C" fn stream_pid_start_time(
    pid: u32,
) -> u64 {
    crate::stream::read_pid_start_time_for(pid)
}

#[no_mangle]
pub unsafe extern "C" fn shm_set_fallback_dir(dir: *const c_char) {
    if !dir.is_null() {
        let d = CStr::from_ptr(dir).to_string_lossy();
        shm::shm_set_fallback_dir(&d);
    }
}

#[no_mangle]
pub unsafe extern "C" fn shmalloc(size: usize, errmsg: *mut *mut c_char) -> *mut c_void {
    ffi_try!(errmsg, ptr::null_mut(), shm::shmalloc(size).map(|p| p as *mut c_void))
}

#[no_mangle]
pub unsafe extern "C" fn shmemcpy(
    src: *mut c_void,
    size: usize,
    errmsg: *mut *mut c_char,
) -> *mut c_void {
    ffi_try!(
        errmsg,
        ptr::null_mut(),
        shm::shmemcpy(src as *const u8, size).map(|p| p as *mut c_void)
    )
}

#[no_mangle]
pub unsafe extern "C" fn shcalloc(
    nmemb: usize,
    size: usize,
    errmsg: *mut *mut c_char,
) -> *mut c_void {
    ffi_try!(errmsg, ptr::null_mut(), shm::shcalloc(nmemb, size).map(|p| p as *mut c_void))
}

/// Report blocks currently held across every mapped volume. Writes the
/// block count and byte total through the out pointers, and prints a
/// size-class breakdown to stderr.
#[no_mangle]
pub unsafe extern "C" fn mlc_shm_live_report(
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

#[no_mangle]
pub unsafe extern "C" fn shfree(ptr: *mut c_void, errmsg: *mut *mut c_char) -> bool {
    ffi_try!(errmsg, false, shm::shfree(ptr as AbsPtr).map(|_| true))
}

#[no_mangle]
pub unsafe extern "C" fn shincref(ptr: *mut c_void, errmsg: *mut *mut c_char) -> bool {
    ffi_try!(errmsg, false, shm::shincref(ptr as AbsPtr).map(|_| true))
}

#[no_mangle]
pub unsafe extern "C" fn total_shm_size() -> usize {
    shm::total_shm_size()
}

#[no_mangle]
pub unsafe extern "C" fn rel2abs(ptr: RelPtr, errmsg: *mut *mut c_char) -> *mut c_void {
    ffi_try!(errmsg, ptr::null_mut(), shm::rel2abs(ptr).map(|p| p as *mut c_void))
}

#[no_mangle]
pub unsafe extern "C" fn abs2rel(ptr: *mut c_void, errmsg: *mut *mut c_char) -> RelPtr {
    ffi_try!(errmsg, shm::RELNULL, shm::abs2rel(ptr as AbsPtr))
}

#[no_mangle]
pub unsafe extern "C" fn abs2shm(ptr: *mut c_void, errmsg: *mut *mut c_char) -> *mut ShmHeader {
    ffi_try!(errmsg, ptr::null_mut(), shm::abs2shm(ptr as AbsPtr))
}

#[no_mangle]
pub unsafe extern "C" fn abs2blk(ptr: *mut c_void, errmsg: *mut *mut c_char) -> *mut BlockHeader {
    clear_errmsg(errmsg);
    if ptr.is_null() {
        set_errmsg(errmsg, &MorlocError::NullPointer);
        return ptr::null_mut();
    }
    let blk = (ptr as *mut u8).sub(std::mem::size_of::<BlockHeader>()) as *mut BlockHeader;
    if (*blk).magic != shm::BLK_MAGIC {
        set_errmsg(errmsg, &MorlocError::Shm("Bad block magic".into()));
        return ptr::null_mut();
    }
    blk
}

#[no_mangle]
pub unsafe extern "C" fn vol2rel(ptr: VolPtr, shm_ptr: *const ShmHeader) -> RelPtr {
    shm::vol2rel(ptr, &*shm_ptr)
}

#[no_mangle]
pub unsafe extern "C" fn vol2abs(ptr: VolPtr, shm_ptr: *const ShmHeader) -> *mut c_void {
    shm::vol2abs(ptr, shm_ptr) as *mut c_void
}

// ── Schema functions ───────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn parse_schema(
    schema_str: *const c_char,
    errmsg: *mut *mut c_char,
) -> *mut CSchema {
    clear_errmsg(errmsg);
    if schema_str.is_null() {
        set_errmsg(errmsg, &MorlocError::NullPointer);
        return ptr::null_mut();
    }
    let s = CStr::from_ptr(schema_str).to_string_lossy();
    match schema::parse_schema(&s) {
        Ok(schema) => CSchema::from_rust(&schema),
        Err(e) => {
            set_errmsg(errmsg, &e);
            ptr::null_mut()
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn schema_to_string(schema: *const CSchema) -> *mut c_char {
    if schema.is_null() {
        return ptr::null_mut();
    }
    let rs = CSchema::to_rust(schema);
    let s = schema::schema_to_string(&rs);
    match CString::new(s) {
        Ok(cs) => cs.into_raw(),
        Err(_) => ptr::null_mut(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn free_schema(schema: *mut CSchema) {
    CSchema::free(schema);
}

#[no_mangle]
pub unsafe extern "C" fn schema_is_fixed_width(schema: *const CSchema) -> bool {
    if schema.is_null() {
        return true;
    }
    let rs = CSchema::to_rust(schema);
    rs.is_fixed_width()
}

#[no_mangle]
pub unsafe extern "C" fn schema_alignment(schema: *const CSchema) -> usize {
    if schema.is_null() {
        return 1;
    }
    let rs = CSchema::to_rust(schema);
    rs.alignment()
}

// Hash: morloc_xxh64 is provided by utility.c (via xxhash.h inline)

// ── Serialization ──────────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn pack_with_schema(
    mlc: *const c_void,
    schema: *const CSchema,
    mpkptr: *mut *mut c_char,
    mpk_size: *mut usize,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    *mpkptr = ptr::null_mut();
    *mpk_size = 0;

    let rs = CSchema::to_rust(schema);
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

#[no_mangle]
pub unsafe extern "C" fn pack(
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

#[no_mangle]
pub unsafe extern "C" fn unpack_with_schema(
    mpk: *const c_char,
    mpk_size: usize,
    schema: *const CSchema,
    mlcptr: *mut *mut c_void,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    *mlcptr = ptr::null_mut();

    let data = std::slice::from_raw_parts(mpk as *const u8, mpk_size);
    let rs = CSchema::to_rust(schema);
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

#[no_mangle]
pub unsafe extern "C" fn calculate_voidstar_size(
    data: *const c_void,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> usize {
    clear_errmsg(errmsg);
    if data.is_null() || schema.is_null() {
        return 0;
    }
    let rs = CSchema::to_rust(schema);
    match calc_voidstar_size_inner(data as *const u8, &rs) {
        Ok(size) => size,
        Err(e) => {
            set_errmsg(errmsg, &e);
            0
        }
    }
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
    let mut w = SizeWalk { res: crate::recur::Resolver::new(schema), total: 0, bound: upper_bound };
    let mut st = crate::walk::Stack::new();
    st.enter(schema, data, false);
    crate::walk::run(&mut w, &mut st)?;
    Ok(w.total)
}

use crate::walk::Walker as _;

/// The size walk. A frame's `x` says whether the parent already counted
/// this node's slot (a tuple or record counts its whole fixed layout up
/// front), in which case only the bytes beyond the slot are added.
struct SizeWalk<'r> {
    res: crate::recur::Resolver<'r>,
    total: usize,
    bound: usize,
}

impl<'r> SizeWalk<'r> {
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

impl<'r> crate::walk::Walker<bool> for SizeWalk<'r> {
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
                    // suballoc (`8 + path_len`) for TAG_PATH; no suballoc for
                    // TAG_HANDLE.
                    use morloc_runtime_types::stream_handle as sh;
                    let field = data as *const u8;
                    let mut own = sh::STREAM_HANDLE_FIELD_SIZE;
                    if sh::read_tag(field) == sh::TAG_PATH {
                        let payload = sh::read_payload(field);
                        if payload != shm::RELNULL as u64 {
                            let suballoc = shm::rel2abs(payload as shm::RelPtr)?;
                            let path_len = sh::read_path_size(suballoc) as usize;
                            own += sh::path_suballoc_size(path_len);
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
                _ => self.add(s.width - slot),
            }
        }
        Ok(())
    }
}

#[no_mangle]
pub unsafe extern "C" fn get_ptr(
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut c_void {
    clear_errmsg(errmsg);
    if schema.is_null() {
        return ptr::null_mut();
    }
    let rs = CSchema::to_rust(schema);
    ffi_try!(errmsg, ptr::null_mut(), shm::shmalloc(rs.width).map(|p| p as *mut c_void))
}
