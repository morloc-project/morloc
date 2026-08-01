//! rustmorloc: direct voidstar <-> native-Rust marshaller for the morloc Rust
//! pool member. This is the Rust analog of `cppmorloc.hpp`: a generic
//! `ToVoidstar`/`FromVoidstar` pair whose per-record impls are emitted by the
//! translator (`Members/Rust.hs`), while the substrate impls (scalars, String,
//! Vec, tuples, Option, Box) live here.
//!
//! The wire form is byte-identical to what cppmorloc/pymorloc/rmorloc produce:
//! a fixed-width inline region (`schema.width`) followed by a cursor-advanced
//! variable region, with `Array{size, data:RelPtr}` slots for String/Array and
//! a single relptr slot for Optional. Schema parsing, relptr math, and the
//! `Array` layout are reused from `morloc-runtime-types`; SHM allocation,
//! relptr resolution, and packet framing go through the `libmorloc.so` C ABI.
//!
//! Invariants (mirrored from the C++ member; see the plan I1..I8):
//!  * I1  Every packet returned to the pool host is C-allocated via a C-ABI
//!        `make_*` call (the host frees it with `libc::free`). The
//!        `morloc-runtime-types::packet` Vec builders are never used for a
//!        returned value.
//!  * I3  SHM allocated for a result outlives the socket send; freeing is
//!        deferred to the next dispatch via `dispatch_flush`. A per-alloc
//!        `ShmGuard` reclaims a half-built block if serialization panics.
//!  * I4  The recur env is thread-local (THREAD concurrency runs manifolds in
//!        one address space).
//!  * I5  `Str` is UTF-8 text by contract; invalid UTF-8 is a catchable throw.
//!  * I6  Multi-limb `Int` is rejected on consume (i64 cap).
//!  * I8  All scalar pokes through the byte cursor use unaligned access.

use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::ffi::{c_char, c_void, CString};
use morloc_runtime_types::cschema::CSchema;
use morloc_runtime_types::packet::{
    PACKET_FORMAT_VOIDSTAR as PKT_FORMAT_VOIDSTAR,
    PACKET_SOURCE_MESG as PKT_SOURCE_MESG,
    PACKET_SOURCE_RPTR as PKT_SOURCE_RPTR,
};
use morloc_runtime_types::shm_types::{align_up, encode_relptr, relptr_offset, Array, RelPtr, RELNULL};

// Re-export the schema surface a generated pool needs (also brings Schema/
// SerialType into scope here), so pool.rs has exactly one direct rlib
// dependency: rustmorloc, pinned by path in the bare-rustc build.
// morloc_runtime_types then resolves as rustmorloc's transitive dep by exact
// metadata hash, sidestepping crate-name ambiguity across rust-deps.
pub use morloc_runtime_types::schema::{parse_schema, Schema, SerialType};


// ---------------------------------------------------------------------------
// C ABI (resolved at final link of the pool binary against libmorloc.so).
// NOT linked into this rlib; an rlib may carry undefined references.
// ---------------------------------------------------------------------------
extern "C" {
    fn shmalloc(size: usize, errmsg: *mut *mut c_char) -> *mut c_void;
    fn shfree(ptr: *mut c_void, errmsg: *mut *mut c_char) -> bool;
    fn shincref(ptr: *mut c_void, errmsg: *mut *mut c_char) -> bool;
    fn abs2rel(ptr: *mut c_void, errmsg: *mut *mut c_char) -> isize;
    fn rel2abs(ptr: isize, errmsg: *mut *mut c_char) -> *mut c_void;
    fn make_data_packet_auto(voidstar: *mut c_void, relptr: isize,
                             schema: *const CSchema, errmsg: *mut *mut c_char) -> *mut u8;
    fn get_morloc_data_packet_value(data: *const u8, schema: *const CSchema,
                                    errmsg: *mut *mut c_char) -> *mut u8;
    fn make_fail_packet(msg: *const c_char) -> *mut u8;
    // Inline-threshold control: force a captured closure value to serialize
    // SELF-CONTAINED (embedded, not a SHM relptr) so its packet survives the
    // producing manifold's SHM reclamation (see `reify_capture`).
    fn morloc_set_inline_threshold(bytes: i64);
    fn morloc_get_inline_threshold() -> u64;
    // Cross-pool foreign call primitives (see `foreign_call`).
    fn make_morloc_local_call_packet(midx: u32, arg_packets: *const *const u8,
                                     nargs: usize, errmsg: *mut *mut c_char) -> *mut u8;
    fn send_and_receive_over_socket(socket_path: *const c_char, packet: *const u8,
                                    errmsg: *mut *mut c_char) -> *mut u8;
    fn get_morloc_data_packet_error_message(data: *const u8, errmsg: *mut *mut c_char) -> *mut c_char;
    fn pool_mark_busy();
    fn pool_mark_idle();
    // @show / @read : voidstar <-> JSON text.
    fn mlc_show(data: *const c_void, schema: *const CSchema, errmsg: *mut *mut c_char) -> *mut c_char;
    fn mlc_read(json_str: *const c_char, schema: *const CSchema, errmsg: *mut *mut c_char) -> *mut c_void;
    // File / stream / IO intrinsics. Signatures mirror the authoritative
    // #[no_mangle] defs in morloc-runtime::intrinsics; every heavy operation
    // (SHM, file handles, buffers, the shared slot registry, tmpfile, stdio
    // RPC to the nexus) lives behind these symbols in libmorloc, so each Rust
    // shim below is a thin wrapper, mirroring the C++ pool's `_mlc_*` helpers.
    fn mlc_hash(data: *const c_void, schema: *const CSchema, errmsg: *mut *mut c_char) -> *mut c_char;
    fn mlc_save(data: *const c_void, schema: *const CSchema, level: u8, path: *const c_char, errmsg: *mut *mut c_char) -> i32;
    fn mlc_save_json(data: *const c_void, schema: *const CSchema, level: u8, path: *const c_char, errmsg: *mut *mut c_char) -> i32;
    fn mlc_save_voidstar(data: *const c_void, schema: *const CSchema, level: u8, path: *const c_char, errmsg: *mut *mut c_char) -> i32;
    fn mlc_load(path: *const c_char, schema: *const CSchema, errmsg: *mut *mut c_char) -> *mut c_void;
    fn mlc_open(path: *const c_char, kind: u8, errmsg: *mut *mut c_char) -> i64;
    fn mlc_close(handle: i64, errmsg: *mut *mut c_char) -> i32;
    fn mlc_unlink_tmp(path: *const c_char, errmsg: *mut *mut c_char) -> i32;
    fn mlc_fschema(path: *const c_char, errmsg: *mut *mut c_char) -> *mut c_char;
    fn mlc_ifile_length(handle: i64, errmsg: *mut *mut c_char) -> i64;
    fn mlc_next(handle: i64, errmsg: *mut *mut c_char) -> *mut c_void;
    fn mlc_stream_layout(handle: i64, errmsg: *mut *mut c_char) -> *mut c_void;
    fn mlc_stream(ifile_handle: i64, errmsg: *mut *mut c_char) -> i64;
    fn mlc_ifile_walk(handle: i64, path: *const c_char, args_ptr: *const IFileWalkArg, n_args: u64, errmsg: *mut *mut c_char) -> *mut c_void;
    fn mlc_write(level: u8, handle: i64, payload_voidstar: *const c_void, errmsg: *mut *mut c_char) -> i32;
    fn mlc_append(schema_str: *const c_char, path: *const c_char, errmsg: *mut *mut c_char) -> i64;
    fn mlc_concat(paths: *const *const c_char, n_paths: usize, dest: *const c_char, errmsg: *mut *mut c_char) -> i32;
    fn mlc_flush(handle: i64, errmsg: *mut *mut c_char) -> i32;
    fn mlc_tell(errmsg: *mut *mut c_char) -> u64;
    fn mlc_tmpfile(errmsg: *mut *mut c_char) -> *mut c_char;
    fn mlc_open_ostream(schema_str: *const c_char, path: *const c_char, errmsg: *mut *mut c_char) -> i64;
    fn mlc_open_istream(schema_str: *const c_char, path: *const c_char, errmsg: *mut *mut c_char) -> i64;
    fn mlc_open_stdin(schema_str: *const c_char, errmsg: *mut *mut c_char) -> i64;
    fn mlc_open_stdout(schema_str: *const c_char, errmsg: *mut *mut c_char) -> i64;
    fn mlc_open_stderr(schema_str: *const c_char, errmsg: *mut *mut c_char) -> i64;
    // Canonicalises a schema to the string the runtime keys/compares streams
    // by (used by the open_*/append family). libc-malloc'd result.
    fn schema_to_string(schema: *const CSchema) -> *mut c_char;
    // Cross-pool stream-handle wire codec. A handle (IFile/IStream/OStream) is a
    // bare u64 slot id in-pool, but crosses a boundary as a 16-byte tagged field
    // (TAG_HANDLE inline, or TAG_PATH + a path suballoc) so the receiving pool
    // can re-resolve the slot. These marshal that field; `cursor` advances past
    // any path suballoc, `base_ptr` resolves a relptr (null for in-SHM reads).
    fn mlc_write_handle_voidstar(handle: i64, dest: *mut c_void, cursor: *mut *mut c_void, errmsg: *mut *mut c_char) -> i32;
    fn mlc_read_handle_voidstar(field: *const c_void, base_ptr: *const c_void, kind: u8, errmsg: *mut *mut c_char) -> i64;
}

/// C-ABI layout of one `@ifile_walk` runtime bracket argument (16 bytes).
/// Mirrors `morloc-runtime::intrinsics::IFileWalkArg`; rustmorloc cannot import
/// that type (it depends on `morloc-runtime-types`, not libmorloc), so the
/// layout is redeclared here and must stay in lockstep.
#[repr(C)]
struct IFileWalkArg {
    has: u8,
    _pad: [u8; 7],
    value: i64,
}

// The C-ABI functions take a `const Schema*` (C struct). We bridge our Rust
// `Schema` to it via `morloc_runtime_types::cschema::CSchema`. Treated opaquely
// at the extern boundary; the real layout lives in the rlib.
// The C-ABI packet functions take an immutable, read-only CSchema tree. The
// schema is fixed for the process lifetime (parsed once into the pool's schema
// table), so its CSchema conversion is cached per schema -- keyed by the stable
// &'static Schema address -- instead of being rebuilt and freed on every
// dispatch. Built once per thread per schema and intentionally never freed
// (bounded: one entry per distinct schema). This removes a full CSchema-tree
// allocate + free from the per-manifold-call hot path (put_value always; the
// get_value SHM path).
thread_local! {
    static CSCHEMA_CACHE: RefCell<HashMap<usize, *mut CSchema>> = RefCell::new(HashMap::new());
}

#[inline]
fn cschema_of(schema: &Schema) -> *mut CSchema {
    let key = schema as *const Schema as usize;
    CSCHEMA_CACHE.with(|c| {
        *c.borrow_mut()
            .entry(key)
            .or_insert_with(|| CSchema::from_rust(schema))
    })
}

// Packet header byte offsets (wire format is locked; see
// morloc-runtime-types::packet PacketHeader, `#[repr(C, packed)]`, 32 bytes).
// The source/format tag *values* are imported from morloc-runtime-types::packet
// (single source of truth for the wire format). These byte *offsets* are not
// exported as named constants there, so they stay local.
const PKT_HEADER_SIZE: usize = 32;
const PKT_SOURCE_OFF: usize = 12; // command.data.source
const PKT_FORMAT_OFF: usize = 13; // command.data.format
const PKT_OFFSET_OFF: usize = 20; // u32 metadata-block length
const PKT_LENGTH_OFF: usize = 24; // u64 data-block length

// ---------------------------------------------------------------------------
// Error carrier for @throw (I2 typed panic payload). The pool host's dispatch
// wrapper (generated) downcasts this to a fail packet; any other panic payload
// is a genuine bug and aborts.
// ---------------------------------------------------------------------------
pub struct MorlocThrow(pub String);

/// Raise a catchable morloc error from generated code (`@throw`).
pub fn morloc_throw(msg: impl Into<String>) -> ! {
    std::panic::panic_any(MorlocThrow(msg.into()));
}

/// `@catch fallible fallback`: run `fallible`; on a catchable morloc throw,
/// discard its partial manifold trace and run `fallback`. A non-throw panic (a
/// genuine bug) propagates unchanged (mirrors the C++ MorlocException vs
/// internal-abort split). Both arguments arrive pre-thunked (do-block closures).
pub fn mlc_catch<T, F, G>(fallible: F, fallback: G) -> T
where
    F: FnOnce() -> T,
    G: FnOnce() -> T,
{
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(fallible)) {
        Ok(v) => v,
        Err(payload) => {
            if payload.downcast_ref::<MorlocThrow>().is_some() {
                // The caught throw's partial trace must not leak into a later
                // error's traceback.
                TRACEBACK.with(|t| t.borrow_mut().clear());
                fallback()
            } else {
                std::panic::resume_unwind(payload);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// relptr resolution. Writes compute Array.data / optional slots via `to_rel`;
// reads resolve via `resolve`. A thread-local TEST_BASE lets #[test]s run the
// identical walk against a local buffer (buffer-relative relptrs) with no SHM.
// ---------------------------------------------------------------------------
thread_local! {
    static TEST_BASE: Cell<Option<usize>> = const { Cell::new(None) };
}

#[inline]
unsafe fn to_rel(ptr: *mut u8) -> RelPtr {
    if let Some(base) = TEST_BASE.with(|b| b.get()) {
        encode_relptr(0, (ptr as usize) - base)
    } else {
        let mut err: *mut c_char = std::ptr::null_mut();
        abs2rel(ptr as *mut c_void, &mut err)
    }
}

/// Resolve a relptr to an absolute pointer. `base` non-null => offset
/// arithmetic (inline MESG packet / test buffer); null => SHM volume table.
#[inline]
unsafe fn resolve(rel: RelPtr, base: *const u8) -> *const u8 {
    if !base.is_null() {
        base.add(relptr_offset(rel))
    } else {
        let mut err: *mut c_char = std::ptr::null_mut();
        rel2abs(rel, &mut err) as *const u8
    }
}

// ---------------------------------------------------------------------------
// Recur env (I4): thread-local stack of named-schema declarations. A back-ref
// (SerialType::Recur) resolves to the matching declaration higher on the walk.
// Entries hold a raw pointer into the caller's parsed schema tree, which
// outlives the walk (RAII scope pops before the tree is dropped).
// ---------------------------------------------------------------------------
thread_local! {
    static RECUR_ENV: Cell<Vec<(String, *const Schema)>> = const { Cell::new(Vec::new()) };
}

pub struct RecurScope {
    pushed: bool,
}
impl RecurScope {
    /// Push `schema`'s named declaration (if any) onto the recur env for the
    /// lifetime of the returned guard. Generated record impls call this at the
    /// top of their walk so a nested back-ref resolves at any depth (this is
    /// stricter than the C++ member, whose write path only pushes at the top).
    pub fn enter(schema: &Schema) -> RecurScope {
        let pushed = if schema.serial_type != SerialType::Recur {
            if let Some(name) = &schema.name {
                RECUR_ENV.with(|e| {
                    let mut v = e.take();
                    v.push((name.clone(), schema as *const Schema));
                    e.set(v);
                });
                true
            } else {
                false
            }
        } else {
            false
        };
        RecurScope { pushed }
    }
}
impl Drop for RecurScope {
    fn drop(&mut self) {
        if self.pushed {
            RECUR_ENV.with(|e| {
                let mut v = e.take();
                v.pop();
                e.set(v);
            });
        }
    }
}

/// Normalize a possibly-Recur schema to its named declaration. Non-Recur
/// schemas pass through unchanged (so callers can use it unconditionally at
/// the top of a walk). Panics (compiler-bug abort) on an unresolvable back-ref.
pub fn resolve_recur(schema: &Schema) -> &Schema {
    if schema.serial_type != SerialType::Recur {
        return schema;
    }
    let name = schema.name.as_deref().unwrap_or("");
    let found = RECUR_ENV.with(|e| {
        let v = e.take();
        let ptr = v.iter().rev().find(|(n, _)| n == name).map(|(_, p)| *p);
        e.set(v);
        ptr
    });
    match found {
        // SAFETY: the pointer references a declaration node in the same parsed
        // schema tree the caller borrows for the whole walk (I4).
        Some(p) => unsafe { &*p },
        None => panic!("MORLOC_INTERNAL_ABORT: Recur back-reference to undeclared schema '{name}'"),
    }
}

// ---------------------------------------------------------------------------
// SHM lifetime (I3): a deferred-free tracker flushed at dispatch entry, plus a
// per-alloc RAII guard that reclaims a half-built block on panic.
// ---------------------------------------------------------------------------
thread_local! {
    static SHM_TRACKER: Cell<Vec<*mut c_void>> = const { Cell::new(Vec::new()) };
}

fn track(ptr: *mut c_void) {
    SHM_TRACKER.with(|t| {
        let mut v = t.take();
        v.push(ptr);
        t.set(v);
    });
}

/// Free all deferred SHM blocks from the previous dispatch. Generated
/// `local_dispatch`/`remote_dispatch` call this at entry (cpp: pool.cpp:979).
pub fn dispatch_flush() {
    SHM_TRACKER.with(|t| {
        let v = t.take();
        for ptr in &v {
            let mut err: *mut c_char = std::ptr::null_mut();
            unsafe {
                shfree(*ptr, &mut err);
                discard_err(err);
            }
        }
        t.set(Vec::new());
    });
    // Reset any stale traceback frames from a prior dispatch (defensive; the
    // guard normally drains them when it forms the fail packet).
    TRACEBACK.with(|t| t.borrow_mut().clear());
}

// ---------------------------------------------------------------------------
// Error traceback. Each manifold holds a `FrameGuard` bound to its precomputed
// frame line (`\n  at <name> [rust] (mid=N, file:line:col)`). On a panic
// unwind the guard appends the line to a thread-local buffer -- innermost
// manifold first, matching the C++ member's catch-append-rethrow chain (which
// accumulates the same lines onto the exception message). `dispatch_guard`
// drains the buffer onto the throw message when it forms the fail packet, so
// the message + full manifold trace crosses the pool boundary as one string.
// ---------------------------------------------------------------------------
thread_local! {
    static TRACEBACK: RefCell<String> = RefCell::new(String::new());
}

/// RAII manifold-frame marker. A no-op on normal return and on the happy path;
/// on a panic unwind it records its frame line to the thread-local traceback.
pub struct FrameGuard {
    frame: &'static str,
}

impl FrameGuard {
    #[inline]
    pub fn new(frame: &'static str) -> FrameGuard {
        FrameGuard { frame }
    }
}

impl Drop for FrameGuard {
    #[inline]
    fn drop(&mut self) {
        if std::thread::panicking() {
            TRACEBACK.with(|t| t.borrow_mut().push_str(self.frame));
        }
    }
}

struct ShmGuard(Option<*mut c_void>);
impl ShmGuard {
    fn new(ptr: *mut c_void) -> ShmGuard { ShmGuard(Some(ptr)) }
    /// Hand ownership off (to the tracker); Drop no longer frees.
    fn commit(mut self) { self.0 = None; }
}
impl Drop for ShmGuard {
    fn drop(&mut self) {
        if let Some(ptr) = self.0 {
            let mut err: *mut c_char = std::ptr::null_mut();
            unsafe {
                shfree(ptr, &mut err);
                discard_err(err);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// The marshalling traits. Per-record impls are emitted by the translator.
// `shm_size` returns the FULL size (inline width + variable region, incl.
// worst-case alignment padding). `write` fills the inline slot at `dest` and
// appends variable data at `*cursor`, advancing it. `read` reconstructs a
// native value; `base` is the inline-packet/test buffer base or null for SHM.
// ---------------------------------------------------------------------------
pub trait ToVoidstar {
    fn shm_size(&self, schema: &Schema) -> usize;
    /// # Safety
    /// `dest` must point at a `schema.width`-byte inline slot and `*cursor`
    /// into a buffer with at least `self.shm_size(schema)` bytes remaining.
    unsafe fn write(&self, dest: *mut u8, cursor: &mut *mut u8, schema: &Schema);
}
pub trait FromVoidstar: Sized {
    /// # Safety
    /// `data` must point at a valid `schema`-shaped inline slot; `base` is the
    /// relptr resolution base (see `resolve`).
    unsafe fn read(schema: &Schema, data: *const u8, base: *const u8) -> Self;
}

// ---- scalars --------------------------------------------------------------
// Map a stream-handle SerialType to the `MLC_KIND_*` open-kind byte the reader
// reopens with (0/1/2). Distinct from the schema enum value (21/22/23).
#[inline]
fn handle_kind(t: SerialType) -> u8 {
    match t {
        SerialType::IFile => 0,
        SerialType::IStream => 1,
        SerialType::OStream => 2,
        _ => 0,
    }
}

// A C-ABI handle-codec error message, or `fallback` when none was set.
#[inline]
unsafe fn handle_err(err: *mut c_char, fallback: &str) -> String {
    if err.is_null() { fallback.to_string() } else { cstr_take(err) }
}

macro_rules! int_impl {
    ($t:ty) => {
        impl ToVoidstar for $t {
            #[inline]
            fn shm_size(&self, schema: &Schema) -> usize { schema.width }
            #[inline]
            unsafe fn write(&self, dest: *mut u8, cursor: &mut *mut u8, schema: &Schema) {
                match schema.serial_type {
                    // A stream handle (IFile/IStream/OStream) is a u64 slot id
                    // that crosses a boundary as a 16-byte tagged field, not a
                    // bare int. The forwarder emits TAG_HANDLE (inline slot id);
                    // `cursor` would advance past a path suballoc under TAG_PATH.
                    SerialType::IFile | SerialType::OStream | SerialType::IStream => {
                        let mut err: *mut c_char = std::ptr::null_mut();
                        let rc = mlc_write_handle_voidstar(
                            *self as i64,
                            dest as *mut c_void,
                            cursor as *mut *mut u8 as *mut *mut c_void,
                            &mut err,
                        );
                        if rc != 0 {
                            morloc_throw(handle_err(err, "mlc_write_handle_voidstar failed"));
                        }
                    }
                    // Inline BigInt [size=1, value] (16 bytes); v1 never emits
                    // multi-limb (I6 handles consume).
                    SerialType::Int => {
                        core::ptr::write_unaligned(dest as *mut i64, 1);
                        core::ptr::write_unaligned((dest as *mut i64).add(1), *self as i64);
                    }
                    _ => core::ptr::write_unaligned(dest as *mut $t, *self), // I8 unaligned
                }
            }
        }
        impl FromVoidstar for $t {
            #[inline]
            unsafe fn read(schema: &Schema, data: *const u8, base: *const u8) -> Self {
                match schema.serial_type {
                    // Read the 16-byte tagged field (payload at offset 8) and
                    // re-resolve it to a local handle -- NOT the plain-int path,
                    // which would read the tag byte at offset 0.
                    SerialType::IFile | SerialType::OStream | SerialType::IStream => {
                        let mut err: *mut c_char = std::ptr::null_mut();
                        let handle = mlc_read_handle_voidstar(
                            data as *const c_void,
                            base as *const c_void,
                            handle_kind(schema.serial_type),
                            &mut err,
                        );
                        if !err.is_null() || handle < 0 {
                            morloc_throw(handle_err(err, "mlc_read_handle_voidstar failed"));
                        }
                        handle as $t
                    }
                    _ => read_int(schema, data) as $t,
                }
            }
        }
    };
}
int_impl!(i8); int_impl!(i16); int_impl!(i32); int_impl!(i64);
int_impl!(u8); int_impl!(u16); int_impl!(u32); int_impl!(u64);

// Read an integer at schema width, widening to i128 (the common carrier);
// rejects multi-limb Int (I6). Mirrors cppmorloc.hpp:1128-1168.
#[inline]
unsafe fn read_int(schema: &Schema, data: *const u8) -> i128 {
    match schema.serial_type {
        SerialType::Sint8 => core::ptr::read_unaligned(data as *const i8) as i128,
        SerialType::Sint16 => core::ptr::read_unaligned(data as *const i16) as i128,
        SerialType::Sint32 => core::ptr::read_unaligned(data as *const i32) as i128,
        SerialType::Sint64 => core::ptr::read_unaligned(data as *const i64) as i128,
        SerialType::Uint8 => core::ptr::read_unaligned(data as *const u8) as i128,
        SerialType::Uint16 => core::ptr::read_unaligned(data as *const u16) as i128,
        SerialType::Uint32 => core::ptr::read_unaligned(data as *const u32) as i128,
        SerialType::Uint64 => core::ptr::read_unaligned(data as *const u64) as i128,
        SerialType::Bool => (core::ptr::read_unaligned(data) == 1) as i128,
        SerialType::Int => {
            let size = core::ptr::read_unaligned(data as *const i64);
            if size > 1 {
                morloc_throw(format!("Integer overflow: {size}-limb integer does not fit in a fixed-width type"));
            }
            if size == 0 { 0 } else { core::ptr::read_unaligned((data as *const i64).add(1)) as i128 }
        }
        _ => core::ptr::read_unaligned(data as *const i64) as i128,
    }
}

macro_rules! float_impl {
    ($t:ty) => {
        impl ToVoidstar for $t {
            #[inline]
            fn shm_size(&self, schema: &Schema) -> usize { schema.width }
            #[inline]
            unsafe fn write(&self, dest: *mut u8, _cursor: &mut *mut u8, _schema: &Schema) {
                core::ptr::write_unaligned(dest as *mut $t, *self); // I8
            }
        }
        impl FromVoidstar for $t {
            #[inline]
            unsafe fn read(schema: &Schema, data: *const u8, _base: *const u8) -> Self {
                match schema.serial_type {
                    SerialType::Float32 => core::ptr::read_unaligned(data as *const f32) as $t,
                    SerialType::Float64 => core::ptr::read_unaligned(data as *const f64) as $t,
                    _ => core::ptr::read_unaligned(data as *const $t),
                }
            }
        }
    };
}
float_impl!(f32); float_impl!(f64);

impl ToVoidstar for bool {
    #[inline]
    fn shm_size(&self, schema: &Schema) -> usize { schema.width }
    #[inline]
    unsafe fn write(&self, dest: *mut u8, _cursor: &mut *mut u8, _schema: &Schema) {
        core::ptr::write_unaligned(dest, if *self { 1u8 } else { 0u8 });
    }
}
impl FromVoidstar for bool {
    #[inline]
    unsafe fn read(_schema: &Schema, data: *const u8, _base: *const u8) -> Self {
        core::ptr::read_unaligned(data) == 1
    }
}

// ---- Unit () (NIL, the value of `<E> ()` effect results) -------------------
// NIL occupies a 1-byte inline slot (schema.width) with no variable region;
// nothing meaningful is written or read.
impl ToVoidstar for () {
    #[inline]
    fn shm_size(&self, _schema: &Schema) -> usize { 0 }
    #[inline]
    unsafe fn write(&self, _dest: *mut u8, _cursor: &mut *mut u8, _schema: &Schema) {}
}
impl FromVoidstar for () {
    #[inline]
    unsafe fn read(_schema: &Schema, _data: *const u8, _base: *const u8) -> Self {}
}

// ---- String (Str, I5: UTF-8 text by contract) -----------------------------
impl ToVoidstar for String {
    fn shm_size(&self, schema: &Schema) -> usize { schema.width + self.len() }
    unsafe fn write(&self, dest: *mut u8, cursor: &mut *mut u8, _schema: &Schema) {
        // String bytes are placed UNALIGNED (unlike Vec, which aligns) --
        // mirrors cppmorloc bytes_to_voidstar.
        let n = self.len();
        let data_rel = if n == 0 {
            RELNULL
        } else {
            let r = to_rel(*cursor);
            core::ptr::copy_nonoverlapping(self.as_ptr(), *cursor, n);
            *cursor = (*cursor).add(n);
            r
        };
        core::ptr::write_unaligned(dest as *mut Array, Array { size: n, data: data_rel });
    }
}
impl FromVoidstar for String {
    unsafe fn read(_schema: &Schema, data: *const u8, base: *const u8) -> Self {
        let a = core::ptr::read_unaligned(data as *const Array);
        if a.size == 0 {
            return String::new();
        }
        let p = resolve(a.data, base);
        let bytes = core::slice::from_raw_parts(p, a.size).to_vec();
        match String::from_utf8(bytes) {
            Ok(s) => s,
            Err(_) => morloc_throw("Str field is not valid UTF-8"), // I5
        }
    }
}

// ---- Vec (Array) ----------------------------------------------------------
impl<T: ToVoidstar> ToVoidstar for Vec<T> {
    fn shm_size(&self, schema: &Schema) -> usize {
        let elem = resolve_recur(&schema.parameters[0]);
        // width slot + worst-case cursor alignment padding + element data
        let mut total = schema.width + (elem.array_data_alignment() - 1);
        if elem.is_primitive_numeric() {
            total += self.len() * elem.width;
        } else {
            for x in self {
                total += x.shm_size(elem);
            }
        }
        total
    }
    unsafe fn write(&self, dest: *mut u8, cursor: &mut *mut u8, schema: &Schema) {
        let n = self.len();
        if n == 0 {
            core::ptr::write_unaligned(dest as *mut Array, Array { size: 0, data: RELNULL });
            return;
        }
        let elem = resolve_recur(&schema.parameters[0]);
        // align cursor for element data (bumps to 64 for primitive numerics)
        *cursor = align_up(*cursor as usize, elem.array_data_alignment()) as *mut u8;
        let data_rel = to_rel(*cursor);
        let start = *cursor;
        let width = elem.width;
        *cursor = start.add(n * width);
        for (i, x) in self.iter().enumerate() {
            x.write(start.add(i * width), cursor, elem);
        }
        core::ptr::write_unaligned(dest as *mut Array, Array { size: n, data: data_rel });
    }
}
impl<T: FromVoidstar> FromVoidstar for Vec<T> {
    unsafe fn read(schema: &Schema, data: *const u8, base: *const u8) -> Self {
        let a = core::ptr::read_unaligned(data as *const Array);
        if a.size == 0 {
            return Vec::new();
        }
        let elem = resolve_recur(&schema.parameters[0]);
        let start = resolve(a.data, base);
        let width = elem.width;
        let mut out = Vec::with_capacity(a.size);
        for i in 0..a.size {
            out.push(<T as FromVoidstar>::read(elem, start.add(i * width), base));
        }
        out
    }
}

// ---- Option (?T) ----------------------------------------------------------
impl<T: ToVoidstar> ToVoidstar for Option<T> {
    fn shm_size(&self, schema: &Schema) -> usize {
        match self {
            None => schema.width,
            Some(v) => {
                let inner = resolve_recur(&schema.parameters[0]);
                schema.width + (inner.alignment().max(1) - 1) + v.shm_size(inner)
            }
        }
    }
    unsafe fn write(&self, dest: *mut u8, cursor: &mut *mut u8, schema: &Schema) {
        match self {
            None => core::ptr::write_unaligned(dest as *mut RelPtr, RELNULL),
            Some(v) => {
                let inner = resolve_recur(&schema.parameters[0]);
                let align = inner.alignment().max(1);
                *cursor = align_up(*cursor as usize, align) as *mut u8;
                let slot = *cursor;
                let data_rel = to_rel(slot);
                *cursor = slot.add(inner.width);
                v.write(slot, cursor, inner);
                core::ptr::write_unaligned(dest as *mut RelPtr, data_rel);
            }
        }
    }
}
impl<T: FromVoidstar> FromVoidstar for Option<T> {
    unsafe fn read(schema: &Schema, data: *const u8, base: *const u8) -> Self {
        let rel = core::ptr::read_unaligned(data as *const RelPtr);
        if rel == RELNULL {
            return None;
        }
        let inner = resolve_recur(&schema.parameters[0]);
        let p = resolve(rel, base);
        Some(<T as FromVoidstar>::read(inner, p, base))
    }
}

// ---- Box (cycle-break indirection, I7) ------------------------------------
impl<T: ToVoidstar> ToVoidstar for Box<T> {
    fn shm_size(&self, schema: &Schema) -> usize { (**self).shm_size(schema) }
    unsafe fn write(&self, dest: *mut u8, cursor: &mut *mut u8, schema: &Schema) {
        (**self).write(dest, cursor, schema)
    }
}
impl<T: FromVoidstar> FromVoidstar for Box<T> {
    unsafe fn read(schema: &Schema, data: *const u8, base: *const u8) -> Self {
        Box::new(<T as FromVoidstar>::read(schema, data, base))
    }
}

// ---- tuples ---------------------------------------------------------------
macro_rules! tuple_impl {
    ($($T:ident $idx:tt),+) => {
        impl<$($T: ToVoidstar),+> ToVoidstar for ($($T,)+) {
            fn shm_size(&self, schema: &Schema) -> usize {
                let mut total = schema.width;
                $(
                    let fs = resolve_recur(&schema.parameters[$idx]);
                    let e = self.$idx.shm_size(fs);
                    if e > fs.width { total += e - fs.width; }
                )+
                total
            }
            unsafe fn write(&self, dest: *mut u8, cursor: &mut *mut u8, schema: &Schema) {
                $(
                    let fs = resolve_recur(&schema.parameters[$idx]);
                    self.$idx.write(dest.add(schema.offsets[$idx]), cursor, fs);
                )+
            }
        }
        impl<$($T: FromVoidstar),+> FromVoidstar for ($($T,)+) {
            unsafe fn read(schema: &Schema, data: *const u8, base: *const u8) -> Self {
                ($(
                    <$T as FromVoidstar>::read(
                        resolve_recur(&schema.parameters[$idx]),
                        data.add(schema.offsets[$idx]), base),
                )+)
            }
        }
    };
}
tuple_impl!(A 0, B 1);
tuple_impl!(A 0, B 1, C 2);
tuple_impl!(A 0, B 1, C 2, D 3);
tuple_impl!(A 0, B 1, C 2, D 3, E 4);
tuple_impl!(A 0, B 1, C 2, D 3, E 4, F 5);
tuple_impl!(A 0, B 1, C 2, D 3, E 4, F 5, G 6);
tuple_impl!(A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7);

// ---------------------------------------------------------------------------
// Function values (defunctionalization). A morloc function stored in data (a
// record/list field) or crossing a pool boundary is represented as a fat trait
// object `Rc<dyn MorlocFnN<..>>`: callable via `callN`, and reifiable to its
// origin via `reifyN`. A plain native closure is a THIN function value -- the
// blanket impl makes any `Fn(&A..)->R` a `MorlocFnN`, so `callN` monomorphizes
// (zero-cost, inlined) while a boxed one dispatches through the vtable. A
// closure that can CROSS is built as a `FatClosure` carrying its
// `(sockid, mid, captured)` origin, so `reifyN` returns `Some`; a thin one
// returns `None`. Function ARGUMENTS to higher-order functions stay monomorphic
// `impl Fn` (never boxed), so a local map/fold pays no dispatch cost.

/// The reified origin of a crossing closure: (home socket name, home manifold
/// id, serialized captured environment).
pub type ClosureOrigin = (String, i64, Vec<Vec<u8>>);

/// A crossing closure: a native closure plus the origin needed to reify it.
pub struct FatClosure<F> {
    pub f: F,
    pub origin: ClosureOrigin,
}

macro_rules! morloc_fn {
    ($trait:ident, $call:ident, $reify:ident, $( ($A:ident, $a:ident) ),+ ) => {
        pub trait $trait<$($A,)+ R> {
            fn $call(&self, $($a: &$A,)+) -> R;
            fn $reify(&self) -> Option<ClosureOrigin>;
        }
        // A native closure: callable, no recoverable origin.
        impl<$($A,)+ R, F: Fn($(&$A,)+) -> R> $trait<$($A,)+ R> for F {
            #[inline]
            fn $call(&self, $($a: &$A,)+) -> R { self($($a,)+) }
            fn $reify(&self) -> Option<ClosureOrigin> { None }
        }
        // A crossing closure: reifies to its stored origin.
        impl<$($A,)+ R, F: Fn($(&$A,)+) -> R> $trait<$($A,)+ R> for FatClosure<F> {
            #[inline]
            fn $call(&self, $($a: &$A,)+) -> R { (self.f)($($a,)+) }
            fn $reify(&self) -> Option<ClosureOrigin> { Some(self.origin.clone()) }
        }
        // A boxed function value (a record field, or a reflected proxy) is itself
        // a function value, so `&impl MorlocFnN` accepts it as well as a thin
        // closure. Delegates through the pointer to the object's own impl.
        impl<$($A,)+ R, T: $trait<$($A,)+ R> + ?Sized> $trait<$($A,)+ R> for std::rc::Rc<T> {
            #[inline]
            fn $call(&self, $($a: &$A,)+) -> R { (**self).$call($($a,)+) }
            fn $reify(&self) -> Option<ClosureOrigin> { (**self).$reify() }
        }
    };
}

morloc_fn!(MorlocFn1, call1, reify1, (A1, a1));
morloc_fn!(MorlocFn2, call2, reify2, (A1, a1), (A2, a2));
morloc_fn!(MorlocFn3, call3, reify3, (A1, a1), (A2, a2), (A3, a3));
morloc_fn!(MorlocFn4, call4, reify4, (A1, a1), (A2, a2), (A3, a3), (A4, a4));
morloc_fn!(MorlocFn5, call5, reify5, (A1, a1), (A2, a2), (A3, a3), (A4, a4), (A5, a5));
morloc_fn!(MorlocFn6, call6, reify6, (A1, a1), (A2, a2), (A3, a3), (A4, a4), (A5, a5), (A6, a6));
morloc_fn!(MorlocFn7, call7, reify7, (A1, a1), (A2, a2), (A3, a3), (A4, a4), (A5, a5), (A6, a6), (A7, a7));
morloc_fn!(MorlocFn8, call8, reify8, (A1, a1), (A2, a2), (A3, a3), (A4, a4), (A5, a5), (A6, a6), (A7, a7), (A8, a8));

// ---------------------------------------------------------------------------
// Packet bridge (production). `put_value` serializes a native value into a
// C-allocated data packet (I1); `get_value` reconstructs a native value from
// an argument packet. Both bracket the walk in a recur scope.
// ---------------------------------------------------------------------------

/// # Safety
/// `schema` must describe `value`'s wire type.
pub unsafe fn put_value<T: ToVoidstar>(value: &T, schema: &Schema) -> *mut u8 {
    let _recur = RecurScope::enter(schema);
    let total = value.shm_size(schema).max(1);
    let mut err: *mut c_char = std::ptr::null_mut();
    let root = shmalloc(total, &mut err) as *mut u8;
    if root.is_null() {
        return fail_packet_from_c(err, "shmalloc failed in put_value");
    }
    let guard = ShmGuard::new(root as *mut c_void);
    let mut cursor = root.add(schema.width);
    value.write(root, &mut cursor, schema); // panic -> guard shfree
    let relptr = abs2rel(root as *mut c_void, &mut err);
    let cs = cschema_of(schema);
    let packet = make_data_packet_auto(root as *mut c_void, relptr, cs, &mut err);
    if packet.is_null() {
        return fail_packet_from_c(err, "make_data_packet_auto failed"); // guard shfree
    }
    // Defer the root's free to the next dispatch (I3). Safe for both RPTR
    // packets (data still referenced) and inline packets (data already copied
    // into the C-allocated packet; freeing later is harmless).
    guard.commit();
    track(root as *mut c_void);
    packet
}

/// # Safety
/// `packet` must be a valid data packet whose schema matches `schema`.
pub unsafe fn get_value<T: FromVoidstar>(packet: *const u8, schema: &Schema) -> T {
    let _recur = RecurScope::enter(schema);
    let source = *packet.add(PKT_SOURCE_OFF);
    let format = *packet.add(PKT_FORMAT_OFF);

    if source == PKT_SOURCE_MESG && format == PKT_FORMAT_VOIDSTAR {
        // Inline: voidstar lives in the packet buffer; relptrs are buffer-relative.
        let meta = core::ptr::read_unaligned(packet.add(PKT_OFFSET_OFF) as *const u32) as usize;
        let payload = packet.add(PKT_HEADER_SIZE + meta);
        return <T as FromVoidstar>::read(schema, payload, payload);
    }

    // SHM path (RPTR, or MESG+MSGPACK): resolve via the C ABI, base = null.
    let cs = cschema_of(schema);
    let mut err: *mut c_char = std::ptr::null_mut();
    let voidstar = get_morloc_data_packet_value(packet, cs, &mut err);
    if !err.is_null() {
        let msg = cstr_take(err);
        morloc_throw(msg);
    }
    if source == PKT_SOURCE_RPTR {
        shincref(voidstar as *mut c_void, &mut err);
        if !err.is_null() {
            libc::free(err as *mut c_void);
            err = std::ptr::null_mut();
        }
        let _ = err;
        track(voidstar as *mut c_void);
    }
    <T as FromVoidstar>::read(schema, voidstar, std::ptr::null())
}

// Restore the inline threshold on drop (including a panic unwind out of
// `put_value`), mirroring the C++ `_mlc_reify_capture` save/restore.
struct ThresholdGuard(u64);
impl Drop for ThresholdGuard {
    fn drop(&mut self) {
        unsafe { morloc_set_inline_threshold(self.0 as i64) }
    }
}

/// Serialize a captured value into a SELF-CONTAINED wire packet: the inline
/// threshold is forced so the packet embeds its voidstar rather than a SHM
/// relptr that would dangle once the producing manifold's SHM is reclaimed
/// (a crossing closure is applied back later, from another pool). Returns the
/// owned packet bytes. Port of the C++ pool's `_mlc_reify_capture`.
///
/// # Safety
/// `schema` must describe `value`'s wire type.
pub unsafe fn reify_capture<T: ToVoidstar>(value: &T, schema: &Schema) -> Vec<u8> {
    let saved = morloc_get_inline_threshold();
    let _guard = ThresholdGuard(saved);
    morloc_set_inline_threshold(i64::MAX);
    let packet = put_value(value, schema);
    let offset = core::ptr::read_unaligned(packet.add(PKT_OFFSET_OFF) as *const u32) as usize;
    let length = core::ptr::read_unaligned(packet.add(PKT_LENGTH_OFF) as *const u64) as usize;
    let n = PKT_HEADER_SIZE + offset + length;
    let bytes = std::slice::from_raw_parts(packet as *const u8, n).to_vec();
    libc::free(packet as *mut c_void);
    bytes
}

// ---------------------------------------------------------------------------
// Cross-pool foreign call. Port of the C++ pool's `foreign_call_v`: build a
// local-call packet, round-trip it over the peer pool's socket, surface a
// fail-packet result as a catchable throw (I2), and incref/track a returned
// RPTR result so the peer pool's next dispatch flush cannot reclaim data this
// pool still references (I3).
// ---------------------------------------------------------------------------

// The pool tmpdir (argv[2]); socket filenames resolve against it. Set once at
// startup by the generated pool's main() before any dispatch.
static TMPDIR: std::sync::OnceLock<CString> = std::sync::OnceLock::new();

/// Record the pool tmpdir (argv[2]) for foreign-call socket resolution.
pub fn set_tmpdir(dir: &str) {
    let _ = TMPDIR.set(CString::new(dir).unwrap_or_default());
}

/// # Safety
/// `args` must be valid argument packets for manifold `mid` on the peer pool
/// served at `socket_filename` (relative to the pool tmpdir).
pub unsafe fn foreign_call(socket_filename: &str, mid: u32, args: &[*const u8]) -> *mut u8 {
    let tmpdir = TMPDIR
        .get()
        .map(|s| s.to_string_lossy().into_owned())
        .unwrap_or_default();
    let socket_path = match CString::new(format!("{}/{}", tmpdir, socket_filename)) {
        Ok(s) => s,
        Err(_) => morloc_throw("foreign_call: socket path contains an interior NUL"),
    };

    let mut err: *mut c_char = std::ptr::null_mut();
    let packet = make_morloc_local_call_packet(mid, args.as_ptr(), args.len(), &mut err);
    if !err.is_null() {
        morloc_throw(cstr_take(err));
    }

    pool_mark_busy();
    let result = send_and_receive_over_socket(socket_path.as_ptr(), packet, &mut err);
    pool_mark_idle();
    libc::free(packet as *mut c_void);
    if !err.is_null() {
        morloc_throw(cstr_take(err));
    }

    // A fail-packet result is a peer-side error -> catchable throw.
    let mut fail_err: *mut c_char = std::ptr::null_mut();
    let fail_msg = get_morloc_data_packet_error_message(result, &mut fail_err);
    discard_err(fail_err);
    if !fail_msg.is_null() {
        let msg = cstr_take(fail_msg);
        libc::free(result as *mut c_void);
        morloc_throw(msg);
    }

    // Incref + track a returned SHM-backed (RPTR) result.
    if *result.add(PKT_SOURCE_OFF) == PKT_SOURCE_RPTR {
        let meta = core::ptr::read_unaligned(result.add(PKT_OFFSET_OFF) as *const u32) as usize;
        let rel = core::ptr::read_unaligned(result.add(PKT_HEADER_SIZE + meta) as *const RelPtr);
        let mut rerr: *mut c_char = std::ptr::null_mut();
        let voidstar = rel2abs(rel, &mut rerr);
        discard_err(rerr);
        if !voidstar.is_null() {
            let mut ierr: *mut c_char = std::ptr::null_mut();
            shincref(voidstar, &mut ierr);
            discard_err(ierr);
            track(voidstar);
        }
    }

    result
}

// ---- @show / @read : value <-> JSON text via the C ABI --------------------

// Build `value` into a freshly SHM-allocated voidstar, hand it to `f`, then
// free the voidstar. Mirrors cppmorloc's `to_voidstar(...)` + `shfree` pattern.
unsafe fn with_voidstar<T: ToVoidstar, R>(
    value: &T,
    schema: &Schema,
    f: impl FnOnce(*mut c_void, *const CSchema, &mut *mut c_char) -> R,
) -> R {
    let _recur = RecurScope::enter(schema);
    let total = value.shm_size(schema).max(1);
    let mut err: *mut c_char = std::ptr::null_mut();
    let root = shmalloc(total, &mut err) as *mut u8;
    if root.is_null() {
        morloc_throw(cstr_take(err));
    }
    let guard = ShmGuard::new(root as *mut c_void);
    let mut cursor = root.add(schema.width);
    value.write(root, &mut cursor, schema); // panic -> guard shfree
    let r = f(root as *mut c_void, cschema_of(schema), &mut err);
    drop(guard); // the C call has consumed the voidstar; free it now
    if !err.is_null() {
        morloc_throw(cstr_take(err));
    }
    r
}

/// @show: serialize a value to its JSON text representation.
pub unsafe fn show<T: ToVoidstar>(value: &T, schema: &Schema) -> String {
    let json = with_voidstar(value, schema, |vs, cs, err| mlc_show(vs, cs, err));
    let s = std::ffi::CStr::from_ptr(json).to_string_lossy().into_owned();
    libc::free(json as *mut c_void);
    s
}

/// @read: parse JSON text into a typed value; a parse failure is a catchable
/// morloc error (so `@catch` can recover it).
pub unsafe fn read<T: FromVoidstar>(s: &str, schema: &Schema) -> T {
    let _recur = RecurScope::enter(schema);
    let json = match CString::new(s) {
        Ok(c) => c,
        Err(_) => morloc_throw("@read: input contains an interior NUL byte"),
    };
    let mut err: *mut c_char = std::ptr::null_mut();
    let voidstar = mlc_read(json.as_ptr(), cschema_of(schema), &mut err);
    if !err.is_null() {
        morloc_throw(format!("@read: {}", cstr_take(err)));
    }
    if voidstar.is_null() {
        morloc_throw(format!("@read: could not parse \"{}\"", s));
    }
    let result = <T as FromVoidstar>::read(schema, voidstar as *const u8, std::ptr::null());
    let mut e2: *mut c_char = std::ptr::null_mut();
    shfree(voidstar, &mut e2);
    discard_err(e2);
    result
}

// ---- File / stream / IO intrinsics ----------------------------------------
//
// Thin bridges over the libmorloc `mlc_*` C ABI, mirroring the C++ pool's
// `_mlc_*` helpers (data/lang/cpp/pool.cpp). The runtime owns all IO state; a
// shim only marshals values and propagates errors. Value-in ops stage the
// value through `with_voidstar`; value-out ops reconstruct a runtime-returned
// SHM voidstar via `FromVoidstar` then `shfree` it (the `@load`/`@read`
// shape). A handle is an opaque `u64` slot id (the ABI uses `i64`); the error
// string is always checked BEFORE the i64->u64 narrowing so a -1 error
// sentinel never becomes a bogus handle.

/// Convert a borrowed string arg to a CString, raising a catchable morloc
/// error on an interior NUL byte (matches `@read`).
unsafe fn cstr_arg(s: &str, what: &str) -> CString {
    match CString::new(s) {
        Ok(c) => c,
        Err(_) => morloc_throw(format!("{}: string contains an interior NUL byte", what)),
    }
}

/// Propagate a C-ABI error string as a catchable morloc throw (the Rust
/// analogue of the C++ `PROPAGATE_ERROR` macro). No-op when `err` is null.
#[inline]
unsafe fn check_err(err: *mut c_char) {
    if !err.is_null() {
        morloc_throw(cstr_take(err));
    }
}

/// Propagate a C-ABI error, then narrow an `i64` handle to the `u64` slot id
/// used in generated pools. The error is checked FIRST so a -1 sentinel is
/// never cast to `u64::MAX`.
#[inline]
unsafe fn handle_or_throw(handle: i64, err: *mut c_char, what: &str) -> u64 {
    check_err(err);
    if handle < 0 {
        morloc_throw(format!("{}: runtime returned an invalid handle", what));
    }
    handle as u64
}

/// Reconstruct a value from a runtime-returned SHM voidstar (the `@load`
/// shape): propagate any error, reject null, read via `FromVoidstar`, then
/// `shfree` the SHM block.
unsafe fn read_voidstar<T: FromVoidstar>(
    voidstar: *mut c_void,
    err: *mut c_char,
    schema: &Schema,
    what: &str,
) -> T {
    if !err.is_null() {
        morloc_throw(format!("{}: {}", what, cstr_take(err)));
    }
    if voidstar.is_null() {
        morloc_throw(format!("{}: runtime returned a null value", what));
    }
    let _recur = RecurScope::enter(schema);
    let result = <T as FromVoidstar>::read(schema, voidstar as *const u8, std::ptr::null());
    let mut e2: *mut c_char = std::ptr::null_mut();
    shfree(voidstar, &mut e2);
    discard_err(e2);
    result
}

/// Canonicalise `schema` to the string the runtime keys and compares streams
/// by (so it is byte-identical to what other pools write), hand the borrowed C
/// string to `f`, then free it. Mirrors the C++ helpers' `schema_to_string(..)
/// .. free(s)` and, like them, forwards the libc-owned pointer directly rather
/// than copying it into a Rust-owned buffer.
unsafe fn with_schema_str<R>(schema: &Schema, f: impl FnOnce(*const c_char) -> R) -> R {
    let s = schema_to_string(cschema_of(schema));
    if s.is_null() {
        morloc_throw("morloc IO: schema_to_string returned null");
    }
    let r = f(s);
    libc::free(s as *mut c_void);
    r
}

/// @hash: content hash of a value as a hex string.
pub unsafe fn hash<T: ToVoidstar>(value: &T, schema: &Schema) -> String {
    let h = with_voidstar(value, schema, |vs, cs, err| mlc_hash(vs, cs, err));
    if h.is_null() {
        morloc_throw("@hash: runtime returned null");
    }
    cstr_take(h)
}

/// @save: write a value to disk in the voidstar packet format. `level` is the
/// compression level (the runtime narrows it to a byte). Returns unit.
pub unsafe fn save<T: ToVoidstar>(value: &T, schema: &Schema, level: i64, path: &str) {
    let path_c = cstr_arg(path, "@save");
    let rc = with_voidstar(value, schema, |vs, cs, err| {
        mlc_save(vs, cs, level as u8, path_c.as_ptr(), err)
    });
    if rc != 0 {
        morloc_throw("@save: runtime write failed");
    }
}

/// @savej: write a value to disk as JSON.
pub unsafe fn save_json<T: ToVoidstar>(value: &T, schema: &Schema, level: i64, path: &str) {
    let path_c = cstr_arg(path, "@savej");
    let rc = with_voidstar(value, schema, |vs, cs, err| {
        mlc_save_json(vs, cs, level as u8, path_c.as_ptr(), err)
    });
    if rc != 0 {
        morloc_throw("@savej: runtime write failed");
    }
}

/// @savem: write a value to disk as a raw voidstar block.
pub unsafe fn save_voidstar<T: ToVoidstar>(value: &T, schema: &Schema, level: i64, path: &str) {
    let path_c = cstr_arg(path, "@savem");
    let rc = with_voidstar(value, schema, |vs, cs, err| {
        mlc_save_voidstar(vs, cs, level as u8, path_c.as_ptr(), err)
    });
    if rc != 0 {
        morloc_throw("@savem: runtime write failed");
    }
}

/// @load: read a saved packet from disk into a typed value.
pub unsafe fn load<T: FromVoidstar>(schema: &Schema, path: &str) -> T {
    let path_c = cstr_arg(path, "@load");
    let mut err: *mut c_char = std::ptr::null_mut();
    let voidstar = mlc_load(path_c.as_ptr(), cschema_of(schema), &mut err);
    read_voidstar(voidstar, err, schema, "@load")
}

/// @open (IFile): open a file as a handle of the given kind byte.
pub unsafe fn open(path: &str, kind: u8) -> u64 {
    let path_c = cstr_arg(path, "@open");
    let mut err: *mut c_char = std::ptr::null_mut();
    let h = mlc_open(path_c.as_ptr(), kind, &mut err);
    handle_or_throw(h, err, "@open")
}

/// @close: close a stream/file handle.
pub unsafe fn close(handle: u64) {
    let mut err: *mut c_char = std::ptr::null_mut();
    mlc_close(handle as i64, &mut err);
    check_err(err);
}

/// @close on a registered temp-file path: unlink it.
pub unsafe fn unlink_tmp(path: &str) {
    let path_c = cstr_arg(path, "@close");
    let mut err: *mut c_char = std::ptr::null_mut();
    mlc_unlink_tmp(path_c.as_ptr(), &mut err);
    check_err(err);
}

/// @fschema: read a file's element schema string without opening it. A missing
/// schema (null result) reads as the empty string, which `cstr_take` yields.
pub unsafe fn fschema(path: &str) -> String {
    let path_c = cstr_arg(path, "@fschema");
    let mut err: *mut c_char = std::ptr::null_mut();
    let s = mlc_fschema(path_c.as_ptr(), &mut err);
    check_err(err);
    cstr_take(s)
}

/// @flen: total element count of an IFile.
pub unsafe fn ifile_length(handle: u64) -> i64 {
    let mut err: *mut c_char = std::ptr::null_mut();
    let n = mlc_ifile_length(handle as i64, &mut err);
    check_err(err);
    n
}

/// @next: materialise an IStream's current sub-packet as `[a]` and advance the
/// cursor. An empty list at EOF is a valid (non-null) voidstar.
pub unsafe fn next<T: FromVoidstar>(schema: &Schema, handle: u64) -> T {
    let mut err: *mut c_char = std::ptr::null_mut();
    let voidstar = mlc_next(handle as i64, &mut err);
    read_voidstar(voidstar, err, schema, "@next")
}

/// @streamLayout: per-sub-packet layout of an IFile as `[(U64,U64,U64)]`.
pub unsafe fn stream_layout<T: FromVoidstar>(schema: &Schema, handle: u64) -> T {
    let mut err: *mut c_char = std::ptr::null_mut();
    let voidstar = mlc_stream_layout(handle as i64, &mut err);
    read_voidstar(voidstar, err, schema, "@streamLayout")
}

/// @stream: derive an IStream handle from an IFile handle.
pub unsafe fn stream(ifile_handle: u64) -> u64 {
    let mut err: *mut c_char = std::ptr::null_mut();
    let h = mlc_stream(ifile_handle as i64, &mut err);
    handle_or_throw(h, err, "@stream")
}

/// @write: emit one sub-packet of a value to an OStream handle. `mlc_write`
/// copies the staged voidstar synchronously and takes no schema, so the
/// `with_voidstar` closure ignores its CSchema argument.
pub unsafe fn write<T: ToVoidstar>(schema: &Schema, level: i64, value: &T, handle: u64) {
    let rc = with_voidstar(value, schema, |vs, _cs, err| {
        mlc_write(level as u8, handle as i64, vs, err)
    });
    if rc != 0 {
        morloc_throw("@write: runtime write failed");
    }
}

/// @append: open an existing stream file for append, returning a fresh handle.
pub unsafe fn append(schema: &Schema, path: &str) -> u64 {
    let path_c = cstr_arg(path, "@append");
    let mut err: *mut c_char = std::ptr::null_mut();
    let h = with_schema_str(schema, |s| mlc_append(s, path_c.as_ptr(), &mut err));
    handle_or_throw(h, err, "@append")
}

/// @concat: byte-level concatenation of N stream files into `dest`.
pub unsafe fn concat(paths: &[String], dest: &str) {
    // Bind the CStrings to a named local so the *const c_char array does not
    // dangle (the classic `.map(|s| CString::new(s).as_ptr())` use-after-free).
    let path_cs: Vec<CString> = paths.iter().map(|p| cstr_arg(p, "@concat")).collect();
    let ptrs: Vec<*const c_char> = path_cs.iter().map(|c| c.as_ptr()).collect();
    let dest_c = cstr_arg(dest, "@concat");
    let mut err: *mut c_char = std::ptr::null_mut();
    let rc = mlc_concat(ptrs.as_ptr(), ptrs.len(), dest_c.as_ptr(), &mut err);
    check_err(err);
    if rc != 0 {
        morloc_throw("@concat: runtime concat failed");
    }
}

/// @flush: force buffered elements out as a sub-packet.
pub unsafe fn flush(handle: u64) {
    let mut err: *mut c_char = std::ptr::null_mut();
    mlc_flush(handle as i64, &mut err);
    check_err(err);
}

/// @tell: current @stdout element count.
pub unsafe fn tell() -> u64 {
    let mut err: *mut c_char = std::ptr::null_mut();
    let n = mlc_tell(&mut err);
    check_err(err);
    n
}

/// @tmpfile: create + register a temp file, returning its path.
pub unsafe fn tmpfile() -> String {
    let mut err: *mut c_char = std::ptr::null_mut();
    let s = mlc_tmpfile(&mut err);
    check_err(err);
    if s.is_null() {
        morloc_throw("@tmpfile: runtime returned null");
    }
    cstr_take(s)
}

/// @open (OStream): open a file for writing with the element schema.
pub unsafe fn open_ostream(schema: &Schema, path: &str) -> u64 {
    let path_c = cstr_arg(path, "@open");
    let mut err: *mut c_char = std::ptr::null_mut();
    let h = with_schema_str(schema, |s| mlc_open_ostream(s, path_c.as_ptr(), &mut err));
    handle_or_throw(h, err, "@open")
}

/// @open (IStream): open a file for streamed reading with the element schema.
pub unsafe fn open_istream(schema: &Schema, path: &str) -> u64 {
    let path_c = cstr_arg(path, "@open");
    let mut err: *mut c_char = std::ptr::null_mut();
    let h = with_schema_str(schema, |s| mlc_open_istream(s, path_c.as_ptr(), &mut err));
    handle_or_throw(h, err, "@open")
}

/// @stdin: open the process stdin as an IStream of the element schema.
pub unsafe fn open_stdin(schema: &Schema) -> u64 {
    let mut err: *mut c_char = std::ptr::null_mut();
    let h = with_schema_str(schema, |s| mlc_open_stdin(s, &mut err));
    handle_or_throw(h, err, "@stdin")
}

/// @stdout: open the process stdout as an OStream of the element schema.
pub unsafe fn open_stdout(schema: &Schema) -> u64 {
    let mut err: *mut c_char = std::ptr::null_mut();
    let h = with_schema_str(schema, |s| mlc_open_stdout(s, &mut err));
    handle_or_throw(h, err, "@stdout")
}

/// @stderr: open the process stderr as an OStream of the element schema.
pub unsafe fn open_stderr(schema: &Schema) -> u64 {
    let mut err: *mut c_char = std::ptr::null_mut();
    let h = with_schema_str(schema, |s| mlc_open_stderr(s, &mut err));
    handle_or_throw(h, err, "@stderr")
}

/// @ifile_walk: unified IFile pattern walker. `path` encodes the walk-step
/// chain; `args` are the runtime bracket bounds (optional ints, absent slots
/// take the default). `T` is the materialised result type.
pub unsafe fn ifile_walk<T: FromVoidstar>(
    schema: &Schema,
    handle: u64,
    path: &str,
    args: &[Option<i64>],
) -> T {
    let packed: Vec<IFileWalkArg> = args
        .iter()
        .map(|a| match a {
            Some(v) => IFileWalkArg { has: 1, _pad: [0; 7], value: *v },
            None => IFileWalkArg { has: 0, _pad: [0; 7], value: 0 },
        })
        .collect();
    let path_c = cstr_arg(path, "@ifile_walk");
    let args_ptr = if packed.is_empty() { std::ptr::null() } else { packed.as_ptr() };
    let mut err: *mut c_char = std::ptr::null_mut();
    let voidstar = mlc_ifile_walk(
        handle as i64,
        path_c.as_ptr(),
        args_ptr,
        packed.len() as u64,
        &mut err,
    );
    read_voidstar(voidstar, err, schema, "@ifile_walk")
}

// ---- fail packets (I1: always C-allocated via make_fail_packet) -----------

/// Free a C-allocated error string whose message we intend to ignore.
#[inline]
unsafe fn discard_err(err: *mut c_char) {
    if !err.is_null() {
        libc::free(err as *mut c_void);
    }
}

unsafe fn cstr_take(err: *mut c_char) -> String {
    if err.is_null() {
        return String::new();
    }
    let s = std::ffi::CStr::from_ptr(err).to_string_lossy().into_owned();
    libc::free(err as *mut c_void);
    s
}

unsafe fn fail_packet_from_c(err: *mut c_char, fallback: &str) -> *mut u8 {
    let msg = if err.is_null() { fallback.to_string() } else { cstr_take(err) };
    fail_packet(&msg)
}

/// Build a C-allocated fail packet (I1). Used by the generated dispatch guard.
///
/// # Safety
/// Calls into the libmorloc C ABI.
pub unsafe fn fail_packet(msg: &str) -> *mut u8 {
    match CString::new(msg) {
        Ok(c) => make_fail_packet(c.as_ptr()),
        Err(_) => make_fail_packet(b"morloc error (message contained NUL)\0".as_ptr() as *const c_char),
    }
}

// ---------------------------------------------------------------------------
// Dispatch guard (I2): run a manifold body, converting a MorlocThrow panic to
// a catchable fail packet and aborting on any other (bug) panic. Generated
// dispatch arms wrap their body in this.
// ---------------------------------------------------------------------------
pub fn dispatch_guard<F>(f: F) -> *mut u8
where
    F: FnOnce() -> *mut u8 + std::panic::UnwindSafe,
{
    match std::panic::catch_unwind(f) {
        Ok(p) => p,
        Err(payload) => {
            if let Some(MorlocThrow(msg)) = payload.downcast_ref::<MorlocThrow>() {
                // Append the manifold trace accumulated during unwind, so the
                // message + traceback crosses the pool boundary as one string.
                let full = TRACEBACK.with(|t| {
                    let mut tb = t.borrow_mut();
                    let s = format!("{}{}", msg, tb);
                    tb.clear();
                    s
                });
                unsafe { fail_packet(&full) }
            } else {
                eprintln!("MORLOC_INTERNAL_ABORT: Rust pool panicked (non-throw payload)");
                std::process::abort();
            }
        }
    }
}

/// Install a panic hook that suppresses the default backtrace for MorlocThrow
/// (I2/E3), keeping obs.err readable when `@throw` fires in a loop. Genuine
/// bug panics still print. Call once at pool startup.
pub fn install_panic_hook() {
    let default = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        if let Some(p) = info.payload().downcast_ref::<MorlocThrow>() {
            let _ = p;
            return; // user @throw: silent, surfaced as a fail packet
        }
        default(info);
    }));
}

// ---------------------------------------------------------------------------
// Tests: round-trip the identical walk against a local buffer (buffer-relative
// relptrs via TEST_BASE), so no libmorloc.so / SHM state is required (I: the
// plan's Stage-1 standalone-testability note).
// ---------------------------------------------------------------------------
#[cfg(test)]
mod tests {
    use super::*;
    use morloc_runtime_types::schema::parse_schema;

    /// Serialize `value` into a fresh buffer and read it back, exercising the
    /// same shm_size/write/read the production pool uses.
    unsafe fn roundtrip<T: ToVoidstar + FromVoidstar>(schema_str: &str, value: &T) -> T {
        let schema = parse_schema(schema_str).expect("parse schema");
        let _recur = RecurScope::enter(&schema);
        let total = value.shm_size(&schema).max(1);
        let mut buf = vec![0u8; total + 64]; // slack for alignment bumps
        let base = buf.as_mut_ptr();
        TEST_BASE.with(|b| b.set(Some(base as usize)));
        let mut cursor = base.add(schema.width);
        value.write(base, &mut cursor, &schema);
        let out = <T as FromVoidstar>::read(&schema, base, base);
        TEST_BASE.with(|b| b.set(None));
        out
    }

    #[test]
    fn scalars() {
        unsafe {
            assert_eq!(roundtrip::<i64>("i8", &-42i64), -42);
            assert_eq!(roundtrip::<i32>("i4", &1234i32), 1234);
            assert_eq!(roundtrip::<u64>("u8", &99u64), 99);
            assert_eq!(roundtrip::<f64>("f8", &3.5f64), 3.5);
            assert_eq!(roundtrip::<f32>("f4", &2.5f32), 2.5);
            assert!(roundtrip::<bool>("b", &true));
            assert!(!roundtrip::<bool>("b", &false));
        }
    }

    #[test]
    fn strings() {
        unsafe {
            assert_eq!(roundtrip::<String>("s", &"hello".to_string()), "hello");
            assert_eq!(roundtrip::<String>("s", &String::new()), "");
            assert_eq!(roundtrip::<String>("s", &"unicode: \u{00e9}\u{00e8}".to_string()),
                       "unicode: \u{00e9}\u{00e8}");
        }
    }

    #[test]
    fn vectors() {
        unsafe {
            let v = vec![1i64, 2, 3, 4];
            assert_eq!(roundtrip::<Vec<i64>>("ai8", &v), v);
            let empty: Vec<i64> = vec![];
            assert_eq!(roundtrip::<Vec<i64>>("ai8", &empty), empty);
            let vs = vec!["a".to_string(), "bb".to_string(), "ccc".to_string()];
            assert_eq!(roundtrip::<Vec<String>>("as", &vs), vs);
            let vv = vec![vec![1i64, 2], vec![3], vec![]];
            assert_eq!(roundtrip::<Vec<Vec<i64>>>("aai8", &vv), vv);
        }
    }

    #[test]
    fn options() {
        unsafe {
            assert_eq!(roundtrip::<Option<i64>>("?i8", &Some(7i64)), Some(7));
            assert_eq!(roundtrip::<Option<i64>>("?i8", &None), None);
            assert_eq!(roundtrip::<Option<String>>("?s", &Some("x".to_string())), Some("x".to_string()));
        }
    }

    #[test]
    fn tuples() {
        unsafe {
            let t = (1i64, "hi".to_string());
            assert_eq!(roundtrip::<(i64, String)>("t2i8s", &t), t);
            let t3 = (1i64, 2.5f64, vec![9i64, 8]);
            assert_eq!(roundtrip::<(i64, f64, Vec<i64>)>("t3i8f8ai8", &t3), t3);
        }
    }

    #[test]
    fn nested() {
        unsafe {
            let v: Vec<(i64, Option<String>)> =
                vec![(1, Some("a".to_string())), (2, None), (3, Some("ccc".to_string()))];
            assert_eq!(roundtrip::<Vec<(i64, Option<String>)>>("at2i8?s", &v), v);
        }
    }

    // A recursive record `LL { head: Int, tail: ?LL }` -- exactly the shape the
    // translator emits (resolve_recur + RecurScope::enter at the top; Box at the
    // cycle-break optional). Schema: &2LL m2 4head j 4tail ?^2LL.
    #[derive(Debug, PartialEq, Clone)]
    struct LL {
        head: i64,
        tail: Option<Box<LL>>,
    }
    impl ToVoidstar for LL {
        fn shm_size(&self, schema: &Schema) -> usize {
            let schema = resolve_recur(schema);
            let _g = RecurScope::enter(schema);
            let mut total = schema.width;
            let fs0 = resolve_recur(&schema.parameters[0]);
            let e0 = self.head.shm_size(fs0);
            if e0 > fs0.width { total += e0 - fs0.width; }
            let fs1 = resolve_recur(&schema.parameters[1]);
            let e1 = self.tail.shm_size(fs1);
            if e1 > fs1.width { total += e1 - fs1.width; }
            total
        }
        unsafe fn write(&self, dest: *mut u8, cursor: &mut *mut u8, schema: &Schema) {
            let schema = resolve_recur(schema);
            let _g = RecurScope::enter(schema);
            self.head.write(dest.add(schema.offsets[0]), cursor, resolve_recur(&schema.parameters[0]));
            self.tail.write(dest.add(schema.offsets[1]), cursor, resolve_recur(&schema.parameters[1]));
        }
    }
    impl FromVoidstar for LL {
        unsafe fn read(schema: &Schema, data: *const u8, base: *const u8) -> Self {
            let schema = resolve_recur(schema);
            let _g = RecurScope::enter(schema);
            LL {
                head: <i64 as FromVoidstar>::read(
                    resolve_recur(&schema.parameters[0]), data.add(schema.offsets[0]), base),
                tail: <Option<Box<LL>> as FromVoidstar>::read(
                    resolve_recur(&schema.parameters[1]), data.add(schema.offsets[1]), base),
            }
        }
    }

    #[test]
    fn recursive_record() {
        unsafe {
            const SCHEMA: &str = "&2LLm24headj4tail?^2LL";
            // head uses `j` (MORLOC_INT inline bignum), tail is the cycle-break.
            let single = LL { head: 5, tail: None };
            assert_eq!(roundtrip::<LL>(SCHEMA, &single), single);

            // 1 -> 2 -> 3 chain through the recursive optional (Box at cycles, I7).
            let chain = LL {
                head: 1,
                tail: Some(Box::new(LL {
                    head: 2,
                    tail: Some(Box::new(LL { head: 3, tail: None })),
                })),
            };
            assert_eq!(roundtrip::<LL>(SCHEMA, &chain), chain);

            // Nested under a Vec -- the case the C++ write path mishandles;
            // here each element's RecurScope::enter makes the back-ref resolve.
            let list = vec![
                LL { head: 10, tail: None },
                LL { head: 20, tail: Some(Box::new(LL { head: 21, tail: None })) },
            ];
            assert_eq!(roundtrip::<Vec<LL>>(&format!("a{SCHEMA}"), &list), list);
        }
    }
}
