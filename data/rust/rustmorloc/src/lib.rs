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
use std::collections::{HashMap, VecDeque};
use std::ffi::{c_char, c_void, CString};
use morloc_runtime_types::cschema::CSchema;
use arrow_array::ffi::{from_ffi, to_ffi, FFI_ArrowArray, FFI_ArrowSchema};
use arrow_array::{Array as _, RecordBatch, StructArray};
use morloc_runtime_types::packet::{
    PACKET_COMPRESSION_NONE as PKT_COMPRESSION_NONE,
    PACKET_ENCRYPTION_NONE as PKT_ENCRYPTION_NONE,
    PACKET_FORMAT_VOIDSTAR as PKT_FORMAT_VOIDSTAR,
    PACKET_FORMAT_ARROW as PKT_FORMAT_ARROW,
    PACKET_SOURCE_MESG as PKT_SOURCE_MESG,
    PACKET_SOURCE_RPTR as PKT_SOURCE_RPTR,
    PKT_COMPRESSION_OFF, PKT_ENCRYPTION_OFF, PKT_FORMAT_OFF, PKT_HEADER_SIZE,
    PKT_LENGTH_OFF, PKT_OFFSET_OFF, PKT_SOURCE_OFF,
};
use morloc_runtime_types::shm_types::{align_up, encode_relptr, relptr_offset, Array, RelPtr, RELNULL};

// Re-export the schema surface a generated pool needs (also brings Schema/
// SerialType into scope here), so pool.rs has exactly one direct rlib
// dependency: rustmorloc, pinned by path in the bare-rustc build.
// morloc_runtime_types then resolves as rustmorloc's transitive dep by exact
// metadata hash, sidestepping crate-name ambiguity across rust-deps.
pub use morloc_runtime_types::schema::{parse_schema, Schema, SerialType};
// The Arrow crates a table-typed pool needs, so a module can map `Table` to
// `rustmorloc::arrow_array::RecordBatch` without declaring the crates itself.
pub use arrow_array;
pub use arrow_schema;


// ---------------------------------------------------------------------------
// C ABI (resolved at final link of the pool binary against libmorloc.so).
// NOT linked into this rlib; an rlib may carry undefined references.
// ---------------------------------------------------------------------------
extern "C" {
    fn morloc_log_next_id() -> u64;
    fn morloc_log_emit(tmpl: *const c_char, group: *const c_char,
                       runtime_seconds: f64, call_id: u64);
    fn morloc_bench_record(key: *const c_char, seconds: f64);
    fn shmalloc(size: usize, errmsg: *mut *mut c_char) -> *mut c_void;
    fn shfree(ptr: *mut c_void, errmsg: *mut *mut c_char) -> bool;
    fn shincref(ptr: *mut c_void, errmsg: *mut *mut c_char) -> bool;
    fn abs2rel(ptr: *mut c_void, errmsg: *mut *mut c_char) -> isize;
    fn rel2abs(ptr: isize, errmsg: *mut *mut c_char) -> *mut c_void;
    fn make_data_packet_auto(voidstar: *mut c_void, relptr: isize,
                             schema: *const CSchema, errmsg: *mut *mut c_char) -> *mut u8;
    fn get_morloc_data_packet_value(data: *const u8, schema: *const CSchema,
                                    errmsg: *mut *mut c_char) -> *mut u8;
    // Tables: Arrow C Data Interface <-> SHM table block (see arrow_ffi.rs).
    fn arrow_to_shm_typed(array: *mut FFI_ArrowArray, schema: *const FFI_ArrowSchema,
                          declared: *const CSchema, errmsg: *mut *mut c_char) -> isize;
    fn arrow_from_shm(header: *const c_void, out_schema: *mut FFI_ArrowSchema,
                      out_array: *mut FFI_ArrowArray, errmsg: *mut *mut c_char) -> i32;
    fn arrow_validate(header: *const c_void, schema: *const CSchema, errmsg: *mut *mut c_char) -> i32;
    fn make_arrow_data_packet(relptr: isize, schema: *const CSchema) -> *mut u8;
    fn make_inline_data_packet(voidstar: *mut c_void, schema: *const CSchema, errmsg: *mut *mut c_char) -> *mut u8;
    fn arrow_borrow_register(base: *const u8, rel: isize);
    fn arrow_borrow_clear();
    fn make_fail_packet(msg: *const c_char) -> *mut u8;
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
    // Remote (SLURM/nexus-dispatched) call. Builds the remote packet, resolves
    // the `_remote` cache dir, rewrites args to self-contained form, and
    // dispatches to the nexus. Renamed via link_name so the wrapper below can
    // own the plain `remote_call` name.
    #[link_name = "remote_call"]
    fn remote_call_ffi(midx: i32, socket_basename: *const c_char, cache_path: *const c_char,
                       resources: *const Resources, arg_packets: *const *const u8, nargs: usize,
                       errmsg: *mut *mut c_char) -> *mut u8;
    // On-disk content-addressed result cache (the `a@fn` / @cache path).
    fn morloc_cache_key_compute(midx: u32, arg_packets: *const *const u8,
                                arg_schemas: *const *const c_char, n_args: usize,
                                errmsg: *mut *mut c_char) -> u64;
    fn morloc_cache_lookup(key: u64, label: *const c_char, size_out: *mut usize,
                           errmsg: *mut *mut c_char) -> *mut u8;
    fn morloc_cache_store(key: u64, label: *const c_char, data: *const u8, size: usize,
                          schema_str: *const c_char, errmsg: *mut *mut c_char) -> bool;
    fn morloc_cache_record_hit();
    fn morloc_cache_record_miss();
    fn morloc_cache_record_store();
    fn morloc_packet_size(packet: *const u8, errmsg: *mut *mut c_char) -> usize;
}

/// C-ABI `resources_t` for a remote call: memory (GB), time (walltime seconds),
/// cpus, gpus. A missing field defaults to -1 (memory/time/cpus) or 0 (gpus),
/// matching the C++ pool's `lcRemoteCall`.
#[repr(C)]
struct Resources {
    memory: i32,
    time: i32,
    cpus: i32,
    gpus: i32,
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


// ---------------------------------------------------------------------------
// Error carrier for @throw (I2 typed panic payload). The pool host's dispatch
// wrapper (generated) downcasts this to a fail packet; any other panic payload
// is a genuine bug and aborts.
// ---------------------------------------------------------------------------
pub struct MorlocThrow(pub String);

/// Raise a catchable morloc error (`@throw`) from sourced Rust.
pub fn morloc_throw(msg: impl Into<String>) -> ! {
    std::panic::panic_any(MorlocThrow(msg.into()));
}

/// `@throw` in a value position of generated code, typed as the value it
/// stands in for. Generated code binds every sub-expression to a typed name
/// and serializes the last one, so the raise must carry that type: a `!`
/// there leaves the next statement unreachable and gives `put_value` no
/// `ToVoidstar` to resolve.
pub fn morloc_throw_as<T>(msg: impl Into<String>) -> T {
    morloc_throw(msg)
}

/// Terminate on a failure of the machinery that carries values between pools:
/// IPC, packet construction and decode. None of these are attributable to user
/// data or foreign-function behavior, and none leave the pool able to continue,
/// so they must not reach `mlc_try`. Aborting rather than returning a fail
/// packet is what keeps them fatal across pools: a caller's socket read fails,
/// which it classifies as infrastructure in turn.
pub fn morloc_infra_abort(msg: impl AsRef<str>) -> ! {
    eprintln!("morloc internal error (Rust pool): {}", msg.as_ref());
    std::process::abort()
}

/// `@try body`: run `body` and convert the outcome to data. `ok` wraps the
/// value, `err` the caught message; codegen supplies both because only it
/// knows how this `Try` is represented in Rust.
///
/// Only a `MorlocThrow` payload becomes an `Err` arm. Any other panic is a
/// genuine bug and resumes unwinding, which mirrors the C++ split between
/// MorlocException and an internal abort.
pub fn mlc_try<T, R, F, OK, ERR>(body: F, ok: OK, err: ERR) -> R
where
    F: MorlocFn0<T>,
    OK: FnOnce(T) -> R,
    ERR: FnOnce(String) -> R,
{
    // The body is a SUSPENSION, so it is taken as one: that accepts an inline
    // thunk (through the `Fn` blanket) and an `Rc<dyn MorlocFn0>` held in a
    // variable alike, rather than only the former.
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| body.call0())) {
        Ok(v) => ok(v),
        Err(payload) => match payload.downcast::<MorlocThrow>() {
            Ok(thrown) => {
                // The caught throw's partial trace must not leak into a
                // later error's traceback.
                TRACEBACK.with(|t| t.borrow_mut().clear());
                err(thrown.0)
            }
            Err(other) => std::panic::resume_unwind(other),
        },
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
    static RECUR_ENV: RefCell<Vec<(String, *const Schema)>> = const { RefCell::new(Vec::new()) };
}

/// Push a named declaration onto the recur env; false when `schema` is not a
/// declaration (unnamed, or itself a back-reference).
fn recur_env_push(schema: &Schema) -> bool {
    match &schema.name {
        Some(name) if schema.serial_type != SerialType::Recur => {
            RECUR_ENV.with(|e| {
                let mut v = e.borrow_mut();
                // A node reached through a transparent wrapper (Box) carries
                // the declaration its parent just pushed.
                if v.last().map(|(_, p)| *p) == Some(schema as *const Schema) {
                    return false;
                }
                v.push((name.clone(), schema as *const Schema));
                true
            })
        }
        _ => false,
    }
}

fn recur_env_pop() {
    RECUR_ENV.with(|e| e.borrow_mut().pop());
}

fn recur_env_depth() -> usize {
    RECUR_ENV.with(|e| e.borrow().len())
}

fn recur_env_truncate(depth: usize) {
    RECUR_ENV.with(|e| e.borrow_mut().truncate(depth));
}

pub struct RecurScope {
    pushed: bool,
}
impl RecurScope {
    /// Push `schema`'s named declaration (if any) onto the recur env for the
    /// lifetime of the returned guard, so a back-reference met by a walk that
    /// starts under it resolves.
    pub fn enter(schema: &Schema) -> RecurScope {
        RecurScope { pushed: recur_env_push(schema) }
    }
}
impl Drop for RecurScope {
    fn drop(&mut self) {
        if self.pushed {
            recur_env_pop();
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
        e.borrow().iter().rev().find(|(n, _)| n == name).map(|(_, p)| *p)
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
/// Holds the deferred-release list so that the blocks are released when the
/// thread ends as well as at the next dispatch. A worker is retired only
/// after going idle for longer than the dispatch that would otherwise have
/// flushed it, so releasing here is never earlier than the release it stands
/// in for; it simply happens on a thread that has no next dispatch to do it.
/// Without this a retired worker takes its last dispatch's blocks with it.
struct ShmTracker(Cell<Vec<*mut c_void>>);

impl Drop for ShmTracker {
    fn drop(&mut self) {
        let v = self.0.take();
        for ptr in &v {
            let mut err: *mut c_char = std::ptr::null_mut();
            unsafe {
                shfree(*ptr, &mut err);
                discard_err(err);
            }
        }
    }
}

thread_local! {
    static SHM_TRACKER: ShmTracker = const { ShmTracker(Cell::new(Vec::new())) };
}

fn track(ptr: *mut c_void) {
    SHM_TRACKER.with(|t| {
        let mut v = t.0.take();
        v.push(ptr);
        t.0.set(v);
    });
}

/// Free all deferred SHM blocks from the previous dispatch. Generated
/// `local_dispatch`/`remote_dispatch` call this at entry (cpp: pool.cpp:979).
pub fn dispatch_flush() {
    unsafe { arrow_borrow_clear() };
    SHM_TRACKER.with(|t| {
        let v = t.0.take();
        for ptr in &v {
            let mut err: *mut c_char = std::ptr::null_mut();
            unsafe {
                shfree(*ptr, &mut err);
                discard_err(err);
            }
        }
        t.0.set(Vec::new());
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

/// RAII wrapper for a labeled manifold: emits the start line on construction,
/// the pass line and the benchmark record when the body returns, and the fail
/// line if the body unwinds instead.
///
/// A guard rather than a `catch_unwind` because the failure path is exactly
/// what `Drop` already models: `catch_unwind` would demand `UnwindSafe` of
/// every manifold body, which a body holding raw pointers cannot promise.
///
/// The template strings are NUL-terminated literals emitted by the compiler,
/// so they are passed straight through without an allocation. An absent
/// template (the user nulled that subfield) is an empty string, and empty
/// means "emit nothing".
pub struct LogGuard {
    group: &'static str,
    pass_tmpl: &'static str,
    fail_tmpl: &'static str,
    bench_key: &'static str,
    call_id: u64,
    t0: std::time::Instant,
}

impl LogGuard {
    #[inline]
    pub fn new(
        group: &'static str,
        start_tmpl: &'static str,
        pass_tmpl: &'static str,
        fail_tmpl: &'static str,
        bench_key: &'static str,
    ) -> LogGuard {
        let call_id = unsafe { morloc_log_next_id() };
        if !start_tmpl.is_empty() {
            emit_log(start_tmpl, group, 0.0, call_id);
        }
        LogGuard {
            group,
            pass_tmpl,
            fail_tmpl,
            bench_key,
            call_id,
            t0: std::time::Instant::now(),
        }
    }
}

impl Drop for LogGuard {
    /// Both outcomes are reported from `Drop` rather than from an explicit
    /// call at the end of the body: a generated manifold body ends in
    /// `return <expr>;`, so any statement placed after it is unreachable.
    /// Dropping happens on every path out, and `thread::panicking()` is what
    /// separates the two. Only the success path records a timing -- a call
    /// that unwound did not do the work being measured.
    #[inline]
    fn drop(&mut self) {
        let dt = self.t0.elapsed().as_secs_f64();
        if std::thread::panicking() {
            if !self.fail_tmpl.is_empty() {
                emit_log(self.fail_tmpl, self.group, dt, self.call_id);
            }
            return;
        }
        if !self.pass_tmpl.is_empty() {
            emit_log(self.pass_tmpl, self.group, dt, self.call_id);
        }
        if !self.bench_key.is_empty() {
            if let Ok(k) = CString::new(self.bench_key) {
                unsafe { morloc_bench_record(k.as_ptr(), dt) };
            }
        }
    }
}

fn emit_log(tmpl: &str, group: &str, seconds: f64, call_id: u64) {
    let (t, g) = match (CString::new(tmpl), CString::new(group)) {
        (Ok(t), Ok(g)) => (t, g),
        _ => return,
    };
    unsafe { morloc_log_emit(t.as_ptr(), g.as_ptr(), seconds, call_id) };
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
// The marshalling traits.
//
// A value is marshalled in three passes -- size, write, read -- and each is an
// explicit-stack walk, so a value's depth is bounded by memory rather than by
// the worker thread's stack. `shm_size`, `write` and `read` are the entry
// points; for a compound type their default bodies run a walk, and the type
// supplies the walk's steps. A leaf (a scalar or a string) marks `IS_LEAF`,
// implements the entry points directly, and is handled inline by whichever
// step reaches it.
//
// `shm_size` returns the FULL size (inline width + variable region, incl.
// worst-case alignment padding). `write` fills the inline slot at `dest` and
// appends variable data at `*cursor`, advancing it. `read` reconstructs a
// native value; `base` is the inline-packet/test buffer base or null for SHM.
//
// Frames are only needed when the schema can describe a value of unbounded
// depth, which is when it holds a back-reference. Otherwise the walk runs in
// direct mode: a compound child is stepped by calling its step from the
// parent's, and the depth is the schema's own height. A framed read is
// bottom-up: a node's step pushes a finish frame and then its compound
// children; the finish frame pops the children's values off a typed value
// stack and pushes the node's own.
// ---------------------------------------------------------------------------
pub trait ToVoidstar {
    /// A scalar: sized and written directly, never given a frame.
    const IS_LEAF: bool = false;

    fn shm_size(&self, schema: &Schema) -> usize
    where
        Self: Sized,
    {
        let mut w = SizeWalk::new(schema);
        w.child(self, schema, false);
        w.run()
    }
    /// # Safety
    /// `dest` must point at a `schema.width`-byte inline slot and `*cursor`
    /// into a buffer with at least `self.shm_size(schema)` bytes remaining.
    unsafe fn write(&self, dest: *mut u8, cursor: &mut *mut u8, schema: &Schema)
    where
        Self: Sized,
    {
        let mut w = WriteWalk::new(schema, cursor);
        w.child(self, dest, schema);
        w.run();
    }
    /// Add this node's own bytes to the walk and hand it the compound
    /// children. `schema` is resolved; `idx` is the element to visit for a
    /// sequence stepped one element per visit, else 0.
    fn size_step(&self, w: &mut SizeWalk, schema: &Schema, idx: usize) {
        let _ = (w, schema, idx);
        morloc_infra_abort("size_step reached a type that has no walk step")
    }
    /// Write this node's own slot and hand the walk the compound children.
    ///
    /// # Safety
    /// As for `write`.
    unsafe fn write_step(&self, w: &mut WriteWalk, dest: *mut u8, schema: &Schema, idx: usize) {
        let _ = (w, dest, schema, idx);
        morloc_infra_abort("write_step reached a type that has no walk step")
    }
    /// A table exports itself as an Arrow C Data Interface pair instead of
    /// walking the voidstar layout; every other type has no Arrow form.
    fn arrow_export(&self) -> Option<(FFI_ArrowArray, FFI_ArrowSchema)> {
        None
    }
}
pub trait FromVoidstar: Sized {
    /// A scalar: read directly, never given a frame.
    const IS_LEAF: bool = false;

    /// # Safety
    /// `data` must point at a valid `schema`-shaped inline slot; `base` is the
    /// relptr resolution base (see `resolve`).
    unsafe fn read(schema: &Schema, data: *const u8, base: *const u8) -> Self {
        let mut w = ReadWalk::new(schema, base);
        w.read_root::<Self>(schema, data)
    }
    /// Framed read, first half: push this node's finish frame, then its
    /// compound children. `schema` is resolved.
    ///
    /// # Safety
    /// As for `read`.
    unsafe fn read_step(w: &mut ReadWalk, schema: &Schema, data: *const u8, idx: usize) {
        let _ = (w, schema, data, idx);
        morloc_infra_abort("read_step reached a type that has no walk step")
    }
    /// Build the value: leaf fields are read here, compound children come
    /// from `w.child_read` (popped in a framed walk, read on the spot in a
    /// direct one, in the order the step pushed them).
    ///
    /// # Safety
    /// As for `read`.
    unsafe fn read_finish(w: &mut ReadWalk, schema: &Schema, data: *const u8) -> Self {
        let _ = (w, schema, data);
        morloc_infra_abort("read_finish reached a type that has no walk step")
    }
    /// A table builds itself from an imported Arrow C Data Interface pair;
    /// every other type has no Arrow form.
    ///
    /// # Safety
    /// `array` and `schema` must be valid, unreleased structs.
    unsafe fn arrow_import(array: FFI_ArrowArray, schema: &FFI_ArrowSchema) -> Option<Self> {
        let _ = (array, schema);
        None
    }
}

// ---- walks ----------------------------------------------------------------

/// True iff a back-reference occurs anywhere under `schema`.
pub fn schema_has_recur(schema: &Schema) -> bool {
    schema.serial_type == SerialType::Recur || schema.parameters.iter().any(schema_has_recur)
}

/// Unwinds the recur env to its depth at the walk's start, so a panic
/// (`morloc_throw`) mid-walk leaves no entries behind.
struct EnvMark(usize);
impl EnvMark {
    fn new() -> EnvMark {
        EnvMark(recur_env_depth())
    }
}
impl Drop for EnvMark {
    fn drop(&mut self) {
        recur_env_truncate(self.0);
    }
}

type SizeStep = unsafe fn(&mut SizeWalk, &Schema, *const u8, usize);

#[derive(Clone, Copy)]
struct SizeFrame {
    step: Option<SizeStep>, // None: pop the recur env
    schema: *const Schema,
    obj: *const u8,
    idx: usize,
    inline_slot: bool, // the parent already counted this node's width
    env_pushed: bool,
}

pub struct SizeWalk {
    stack: Vec<SizeFrame>,
    cur: SizeFrame,
    keep: Vec<Box<dyn std::any::Any>>,
    pub total: isize,
    /// The root has no back-reference: nothing needs a frame.
    pub direct: bool,
}

unsafe fn size_thunk<T: ToVoidstar>(w: &mut SizeWalk, s: &Schema, obj: *const u8, idx: usize) {
    (*(obj as *const T)).size_step(w, s, idx)
}

impl SizeWalk {
    pub fn new(root: &Schema) -> SizeWalk {
        SizeWalk {
            stack: Vec::new(),
            cur: SizeFrame { step: None, schema: std::ptr::null(), obj: std::ptr::null(), idx: 0, inline_slot: false, env_pushed: false },
            keep: Vec::new(),
            total: 0,
            direct: !schema_has_recur(root),
        }
    }

    /// A child of a schema that cannot describe unbounded depth is stepped
    /// by call.
    pub fn flat(&self, schema: &Schema) -> bool {
        self.direct || !schema_has_recur(schema)
    }

    /// Account for a child. A leaf is summed here; a compound child is
    /// stepped by call or by frame. `inline_slot` says the child's fixed
    /// width lies inside the parent's (tuple and record fields), so only its
    /// tail is added.
    pub fn child<T: ToVoidstar>(&mut self, v: &T, schema: &Schema, inline_slot: bool) {
        if T::IS_LEAF {
            let s = resolve_recur(schema);
            self.total += v.shm_size(s) as isize;
            if inline_slot {
                self.total -= s.width as isize;
            }
        } else if self.flat(schema) {
            if inline_slot {
                self.total -= schema.width as isize;
            }
            v.size_step(self, schema, 0);
        } else {
            self.stack.push(SizeFrame {
                step: Some(size_thunk::<T>),
                schema,
                obj: v as *const T as *const u8,
                idx: 0,
                inline_slot,
                env_pushed: false,
            });
        }
    }

    /// A child the step itself produced (a reified closure origin): the walk
    /// keeps it alive until it is reached.
    pub fn child_owned<T: ToVoidstar + 'static>(&mut self, v: T, schema: &Schema, inline_slot: bool) {
        self.keep.push(Box::new(v));
        let r: *const T = self.keep.last().and_then(|b| b.downcast_ref::<T>()).unwrap();
        // SAFETY: the box lives in `keep` for the rest of the walk.
        self.child(unsafe { &*r }, schema, inline_slot);
    }

    /// Visit the current node again for element `idx`.
    pub fn resume(&mut self, idx: usize) {
        let mut f = self.cur;
        f.idx = idx;
        f.inline_slot = false;
        f.env_pushed = true;
        self.stack.push(f);
    }

    /// A variant slot plus the out-of-line payload of one arm.
    pub fn variant_payload<T: ToVoidstar>(&mut self, schema: &Schema, arm: &Schema, payload: &T) {
        let a = resolve_recur(arm);
        self.total += (schema.width + (a.alignment().max(1) - 1)) as isize;
        self.child(payload, a, false);
    }

    pub fn run(&mut self) -> usize {
        let _mark = EnvMark::new();
        while let Some(mut f) = self.stack.pop() {
            let step = match f.step {
                None => {
                    recur_env_pop();
                    continue;
                }
                Some(step) => step,
            };
            // SAFETY: frames only hold schemas of the tree the walk was
            // started on, which outlives the walk.
            let s = resolve_recur(unsafe { &*f.schema });
            if !f.env_pushed && recur_env_push(s) {
                self.stack.push(SizeFrame { step: None, ..f });
                f.env_pushed = true;
            }
            if f.inline_slot {
                self.total -= s.width as isize;
            }
            self.cur = f;
            unsafe { step(self, s, f.obj, f.idx) };
        }
        self.total.max(0) as usize
    }
}

type WriteStep = unsafe fn(&mut WriteWalk, &Schema, *mut u8, *const u8, usize);

#[derive(Clone, Copy)]
struct WriteFrame {
    step: Option<WriteStep>,
    schema: *const Schema,
    dest: *mut u8,
    obj: *const u8,
    idx: usize,
    aux: *mut u8, // a sequence's element region, carried across its visits
    env_pushed: bool,
}

pub struct WriteWalk<'a> {
    stack: Vec<WriteFrame>,
    cur: WriteFrame,
    keep: Vec<Box<dyn std::any::Any>>,
    pub cursor: &'a mut *mut u8,
    pub direct: bool,
}

unsafe fn write_thunk<T: ToVoidstar>(w: &mut WriteWalk, s: &Schema, dest: *mut u8, obj: *const u8, idx: usize) {
    (*(obj as *const T)).write_step(w, dest, s, idx)
}

impl<'a> WriteWalk<'a> {
    pub fn new(root: &Schema, cursor: &'a mut *mut u8) -> WriteWalk<'a> {
        WriteWalk {
            stack: Vec::new(),
            cur: WriteFrame { step: None, schema: std::ptr::null(), dest: std::ptr::null_mut(), obj: std::ptr::null(), idx: 0, aux: std::ptr::null_mut(), env_pushed: false },
            keep: Vec::new(),
            cursor,
            direct: !schema_has_recur(root),
        }
    }

    pub fn flat(&self, schema: &Schema) -> bool {
        self.direct || !schema_has_recur(schema)
    }

    /// Write a child into its slot: a leaf now, a compound node by call or
    /// by frame.
    ///
    /// # Safety
    /// `dest` must be the child's slot in a buffer sized by `shm_size`.
    pub unsafe fn child<T: ToVoidstar>(&mut self, v: &T, dest: *mut u8, schema: &Schema) {
        if T::IS_LEAF {
            v.write(dest, self.cursor, resolve_recur(schema));
        } else if self.flat(schema) {
            v.write_step(self, dest, schema, 0);
        } else {
            self.stack.push(WriteFrame {
                step: Some(write_thunk::<T>),
                schema,
                dest,
                obj: v as *const T as *const u8,
                idx: 0,
                aux: std::ptr::null_mut(),
                env_pushed: false,
            });
        }
    }

    /// A child the step itself produced (a reified closure origin): the walk
    /// keeps it alive until it is reached.
    ///
    /// # Safety
    /// As for `child`.
    pub unsafe fn child_owned<T: ToVoidstar + 'static>(&mut self, v: T, dest: *mut u8, schema: &Schema) {
        self.keep.push(Box::new(v));
        let r: *const T = self.keep.last().and_then(|b| b.downcast_ref::<T>()).unwrap();
        self.child(&*r, dest, schema);
    }

    /// Visit the current node again for element `idx`, carrying `aux`.
    pub fn resume(&mut self, idx: usize, aux: *mut u8) {
        let mut f = self.cur;
        f.idx = idx;
        f.aux = aux;
        f.env_pushed = true;
        self.stack.push(f);
    }

    /// The pointer the current node carried over from its previous visit.
    pub fn aux(&self) -> *mut u8 {
        self.cur.aux
    }

    /// Take an aligned slot of `inner`'s width from the cursor.
    ///
    /// # Safety
    /// The buffer must have room for it (see `shm_size`).
    pub unsafe fn alloc(&mut self, inner: &Schema) -> *mut u8 {
        let align = inner.alignment().max(1);
        *self.cursor = align_up(*self.cursor as usize, align) as *mut u8;
        let slot = *self.cursor;
        *self.cursor = slot.add(inner.width);
        slot
    }

    /// A variant slot for an arm with fields: the tag, determined padding,
    /// and a pointer to the payload written at the cursor.
    ///
    /// # Safety
    /// `dest` must point at a writable slot of at least the schema's width,
    /// and the buffer must have room for the payload (see `shm_size`).
    pub unsafe fn variant_payload<T: ToVoidstar>(&mut self, dest: *mut u8, arm: &Schema, tag: u8, payload: &T) {
        *dest = tag;
        core::ptr::write_bytes(dest.add(1), 0, VARIANT_PAYLOAD - 1);
        let a = resolve_recur(arm);
        let slot = self.alloc(a);
        core::ptr::write_unaligned(dest.add(VARIANT_PAYLOAD) as *mut RelPtr, to_rel(slot));
        self.child(payload, slot, a);
    }

    pub fn run(&mut self) {
        let _mark = EnvMark::new();
        while let Some(mut f) = self.stack.pop() {
            let step = match f.step {
                None => {
                    recur_env_pop();
                    continue;
                }
                Some(step) => step,
            };
            let s = resolve_recur(unsafe { &*f.schema });
            if !f.env_pushed && recur_env_push(s) {
                self.stack.push(WriteFrame { step: None, ..f });
                f.env_pushed = true;
            }
            self.cur = f;
            unsafe { step(self, s, f.dest, f.obj, f.idx) };
        }
    }
}

/// Finished values of a framed read, each written in place at an aligned
/// offset with its destructor on record, so a panic mid-walk drops exactly
/// the values still on the stack.
pub struct ValueStack {
    buf: *mut u8,
    cap: usize,
    len: usize,
    ledger: Vec<ValueEntry>,
}

struct ValueEntry {
    off: usize,
    size: usize,
    drop: unsafe fn(*mut u8),
}

const VALUE_STACK_ALIGN: usize = 16;

unsafe fn drop_thunk<T>(p: *mut u8) {
    core::ptr::drop_in_place(p as *mut T)
}

impl ValueStack {
    fn new() -> ValueStack {
        ValueStack { buf: std::ptr::null_mut(), cap: 0, len: 0, ledger: Vec::new() }
    }

    fn reserve(&mut self, end: usize) {
        if end <= self.cap {
            return;
        }
        let new_cap = end.max(self.cap * 2).max(256);
        let new_layout = std::alloc::Layout::from_size_align(new_cap, VALUE_STACK_ALIGN).unwrap();
        // SAFETY: the buffer is always allocated with this alignment, so a
        // realloc keeps every offset aligned as it was.
        let p = unsafe {
            if self.cap == 0 {
                std::alloc::alloc(new_layout)
            } else {
                let old = std::alloc::Layout::from_size_align(self.cap, VALUE_STACK_ALIGN).unwrap();
                std::alloc::realloc(self.buf, old, new_cap)
            }
        };
        if p.is_null() {
            std::alloc::handle_alloc_error(new_layout);
        }
        self.buf = p;
        self.cap = new_cap;
    }

    pub fn push<T>(&mut self, v: T) {
        let align = std::mem::align_of::<T>();
        if align > VALUE_STACK_ALIGN {
            morloc_infra_abort("value stack: over-aligned type");
        }
        let off = align_up(self.len, align);
        let end = off + std::mem::size_of::<T>();
        self.reserve(end.max(1));
        // SAFETY: [off, end) is inside the buffer and aligned for T.
        unsafe { core::ptr::write(self.buf.add(off) as *mut T, v) };
        self.ledger.push(ValueEntry { off, size: std::mem::size_of::<T>(), drop: drop_thunk::<T> });
        self.len = end;
    }

    pub fn pop<T>(&mut self) -> T {
        let e = match self.ledger.pop() {
            Some(e) => e,
            None => morloc_infra_abort("value stack: pop on an empty stack"),
        };
        if e.size != std::mem::size_of::<T>() {
            morloc_infra_abort("value stack: popped a value of another type");
        }
        // SAFETY: the entry records a live T written at `off`.
        let v = unsafe { core::ptr::read(self.buf.add(e.off) as *const T) };
        self.len = e.off;
        v
    }

    /// The value on top, in place.
    pub fn top_mut<T>(&mut self) -> &mut T {
        let e = match self.ledger.last() {
            Some(e) => e,
            None => morloc_infra_abort("value stack: top of an empty stack"),
        };
        if e.size != std::mem::size_of::<T>() {
            morloc_infra_abort("value stack: top is a value of another type");
        }
        unsafe { &mut *(self.buf.add(e.off) as *mut T) }
    }
}

impl Drop for ValueStack {
    fn drop(&mut self) {
        for e in self.ledger.drain(..).rev() {
            unsafe { (e.drop)(self.buf.add(e.off)) };
        }
        if self.cap > 0 {
            let layout = std::alloc::Layout::from_size_align(self.cap, VALUE_STACK_ALIGN).unwrap();
            unsafe { std::alloc::dealloc(self.buf, layout) };
        }
    }
}

type ReadStep = unsafe fn(&mut ReadWalk, &Schema, *const u8, usize);
type ReadFinish = unsafe fn(&mut ReadWalk, &Schema, *const u8);

#[derive(Clone, Copy)]
enum ReadFrame {
    Step { step: ReadStep, schema: *const Schema, data: *const u8, idx: usize, env_pushed: bool },
    Finish { finish: ReadFinish, schema: *const Schema, data: *const u8 },
    PopEnv,
}

pub struct ReadWalk {
    stack: Vec<ReadFrame>,
    cur: ReadFrame,
    pub values: ValueStack,
    pub base: *const u8,
    pub direct: bool,
}

unsafe fn read_step_thunk<T: FromVoidstar>(w: &mut ReadWalk, s: &Schema, data: *const u8, idx: usize) {
    T::read_step(w, s, data, idx)
}

unsafe fn read_finish_thunk<T: FromVoidstar>(w: &mut ReadWalk, s: &Schema, data: *const u8) {
    let v = T::read_finish(w, s, data);
    w.values.push(v);
}

impl ReadWalk {
    pub fn new(root: &Schema, base: *const u8) -> ReadWalk {
        ReadWalk {
            stack: Vec::new(),
            cur: ReadFrame::PopEnv,
            values: ValueStack::new(),
            base,
            direct: !schema_has_recur(root),
        }
    }

    pub fn flat(&self, schema: &Schema) -> bool {
        self.direct || !schema_has_recur(schema)
    }

    /// # Safety
    /// As for `FromVoidstar::read`.
    pub unsafe fn read_root<T: FromVoidstar>(&mut self, schema: &Schema, data: *const u8) -> T {
        if self.flat(schema) {
            T::read_finish(self, resolve_recur(schema), data)
        } else {
            self.child_step::<T>(schema, data);
            self.run();
            self.values.pop::<T>()
        }
    }

    /// Push this node's finish frame; the step then pushes the children the
    /// finish will pop.
    pub fn push_finish<T: FromVoidstar>(&mut self, schema: &Schema, data: *const u8) {
        self.stack.push(ReadFrame::Finish { finish: read_finish_thunk::<T>, schema, data });
    }

    /// Give a child a frame when its schema can describe unbounded depth.
    /// A leaf, or a child whose schema has no back-reference, needs none:
    /// the parent's finish reads it on the spot.
    pub fn child_step<T: FromVoidstar>(&mut self, schema: &Schema, data: *const u8) {
        if T::IS_LEAF || self.flat(schema) {
            return;
        }
        self.stack.push(ReadFrame::Step { step: read_step_thunk::<T>, schema, data, idx: 0, env_pushed: false });
    }

    /// A child's value: a leaf is read now, a child with no back-reference
    /// in its schema is read on the spot, and a framed child is popped --
    /// children framed first come off first, so a finish takes them in the
    /// order its step framed them.
    ///
    /// # Safety
    /// As for `FromVoidstar::read`.
    pub unsafe fn child_read<T: FromVoidstar>(&mut self, schema: &Schema, data: *const u8) -> T {
        if T::IS_LEAF {
            T::read(resolve_recur(schema), data, self.base)
        } else if self.flat(schema) {
            T::read_finish(self, resolve_recur(schema), data)
        } else {
            self.values.pop::<T>()
        }
    }

    pub fn resume(&mut self, idx: usize) {
        if let ReadFrame::Step { step, schema, data, .. } = self.cur {
            self.stack.push(ReadFrame::Step { step, schema, data, idx, env_pushed: true });
        }
    }

    /// The out-of-line payload of a variant slot.
    ///
    /// # Safety
    /// `data` must point at a variant slot whose payload pointer is live.
    pub unsafe fn payload_ptr(&self, data: *const u8) -> *const u8 {
        let rel = core::ptr::read_unaligned(data.add(VARIANT_PAYLOAD) as *const RelPtr);
        resolve(rel, self.base)
    }

    pub fn run(&mut self) {
        let _mark = EnvMark::new();
        while let Some(f) = self.stack.pop() {
            match f {
                ReadFrame::PopEnv => recur_env_pop(),
                ReadFrame::Finish { finish, schema, data } => {
                    unsafe { finish(self, &*schema, data) };
                }
                ReadFrame::Step { step, schema, data, idx, env_pushed } => {
                    let s = resolve_recur(unsafe { &*schema });
                    let mut pushed = env_pushed;
                    if !pushed && recur_env_push(s) {
                        self.stack.push(ReadFrame::PopEnv);
                        pushed = true;
                    }
                    self.cur = ReadFrame::Step { step, schema, data, idx, env_pushed: pushed };
                    unsafe { step(self, s, data, idx) };
                }
            }
        }
    }
}

// ---- tables ---------------------------------------------------------------
// A table never takes the voidstar path: `put_value` and `get_value` route a
// Table-typed schema through the Arrow C Data Interface, so the walk methods
// are unreachable for it.
impl ToVoidstar for RecordBatch {
    const IS_LEAF: bool = true;
    fn shm_size(&self, _schema: &Schema) -> usize {
        morloc_infra_abort("a table cannot be written through the voidstar path")
    }
    unsafe fn write(&self, _dest: *mut u8, _cursor: &mut *mut u8, _schema: &Schema) {
        morloc_infra_abort("a table cannot be written through the voidstar path")
    }
    fn arrow_export(&self) -> Option<(FFI_ArrowArray, FFI_ArrowSchema)> {
        let data = StructArray::from(self.clone()).into_data();
        match to_ffi(&data) {
            Ok(pair) => Some(pair),
            Err(e) => morloc_throw(format!("exporting table: {}", e)),
        }
    }
}
impl FromVoidstar for RecordBatch {
    const IS_LEAF: bool = true;
    unsafe fn read(_schema: &Schema, _data: *const u8, _base: *const u8) -> Self {
        morloc_infra_abort("a table cannot be read through the voidstar path")
    }
    unsafe fn arrow_import(array: FFI_ArrowArray, schema: &FFI_ArrowSchema) -> Option<Self> {
        match from_ffi(array, schema) {
            Ok(data) => Some(RecordBatch::from(StructArray::from(data))),
            Err(e) => morloc_throw(format!("importing table: {}", e)),
        }
    }
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
            const IS_LEAF: bool = true;
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
            const IS_LEAF: bool = true;
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
            const IS_LEAF: bool = true;
            #[inline]
            fn shm_size(&self, schema: &Schema) -> usize { schema.width }
            #[inline]
            unsafe fn write(&self, dest: *mut u8, _cursor: &mut *mut u8, _schema: &Schema) {
                core::ptr::write_unaligned(dest as *mut $t, *self); // I8
            }
        }
        impl FromVoidstar for $t {
            const IS_LEAF: bool = true;
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
    const IS_LEAF: bool = true;
    #[inline]
    fn shm_size(&self, schema: &Schema) -> usize { schema.width }
    #[inline]
    unsafe fn write(&self, dest: *mut u8, _cursor: &mut *mut u8, _schema: &Schema) {
        core::ptr::write_unaligned(dest, if *self { 1u8 } else { 0u8 });
    }
}
impl FromVoidstar for bool {
    const IS_LEAF: bool = true;
    #[inline]
    unsafe fn read(_schema: &Schema, data: *const u8, _base: *const u8) -> Self {
        core::ptr::read_unaligned(data) == 1
    }
}

// ---- Unit () (NIL, the value of `<E> ()` effect results) -------------------
// NIL occupies a 1-byte inline slot (schema.width) with no variable region;
// nothing meaningful is written or read.
impl ToVoidstar for () {
    const IS_LEAF: bool = true;
    #[inline]
    fn shm_size(&self, _schema: &Schema) -> usize { 0 }
    #[inline]
    unsafe fn write(&self, _dest: *mut u8, _cursor: &mut *mut u8, _schema: &Schema) {}
}
impl FromVoidstar for () {
    const IS_LEAF: bool = true;
    #[inline]
    unsafe fn read(_schema: &Schema, _data: *const u8, _base: *const u8) -> Self {}
}

// ---- String (Str, I5: UTF-8 text by contract) -----------------------------
impl ToVoidstar for String {
    const IS_LEAF: bool = true;
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
    const IS_LEAF: bool = true;
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

// ---- sequences (Array wire form): Vec (List) and VecDeque (Deque) ----------
// Both serialize identically -- a contiguous Array of elements written and read
// one at a time (no bulk memcpy, so a VecDeque's ring buffer is fine); they
// differ only in the container constructor and back-insert method. `$push` is
// amortized O(1) for both, so neither adds a copy over the other.
//
// In a framed walk a sequence is visited once per element: the step handles
// element `idx` and re-pushes its own frame beneath the element's, so the
// frame stack stays proportional to depth rather than to element count. A
// framed read keeps the growing container on the value stack and moves each
// finished element into it on the next visit.
macro_rules! seq_impl {
    ($container:ident, $push:ident) => {
impl<T: ToVoidstar> ToVoidstar for $container<T> {
    fn size_step(&self, w: &mut SizeWalk, schema: &Schema, idx: usize) {
        let elem = resolve_recur(&schema.parameters[0]);
        if idx == 0 {
            // width slot + worst-case cursor alignment padding + element data
            w.total += (schema.width + (elem.array_data_alignment() - 1)) as isize;
            if elem.is_primitive_numeric() {
                w.total += (self.len() * elem.width) as isize;
                return;
            }
            if T::IS_LEAF || w.flat(elem) {
                for x in self {
                    w.child(x, elem, false);
                }
                return;
            }
        }
        if idx < self.len() {
            if idx + 1 < self.len() {
                w.resume(idx + 1);
            }
            w.child(&self[idx], elem, false);
        }
    }
    unsafe fn write_step(&self, w: &mut WriteWalk, dest: *mut u8, schema: &Schema, idx: usize) {
        let n = self.len();
        let elem = resolve_recur(&schema.parameters[0]);
        let width = elem.width;
        let start = if idx == 0 {
            if n == 0 {
                core::ptr::write_unaligned(dest as *mut Array, Array { size: 0, data: RELNULL });
                return;
            }
            // align cursor for element data (bumps to 64 for primitive numerics)
            *w.cursor = align_up(*w.cursor as usize, elem.array_data_alignment()) as *mut u8;
            let start = *w.cursor;
            *w.cursor = start.add(n * width);
            core::ptr::write_unaligned(dest as *mut Array, Array { size: n, data: to_rel(start) });
            if T::IS_LEAF || w.flat(elem) {
                for (i, x) in self.iter().enumerate() {
                    w.child(x, start.add(i * width), elem);
                }
                return;
            }
            start
        } else {
            w.aux()
        };
        if idx + 1 < n {
            w.resume(idx + 1, start);
        }
        w.child(&self[idx], start.add(idx * width), elem);
    }
}
impl<T: FromVoidstar> FromVoidstar for $container<T> {
    unsafe fn read_step(w: &mut ReadWalk, schema: &Schema, data: *const u8, idx: usize) {
        let a = core::ptr::read_unaligned(data as *const Array);
        let elem = resolve_recur(&schema.parameters[0]);
        if idx == 0 {
            if T::IS_LEAF || a.size == 0 || w.flat(elem) {
                let v = Self::read_finish(w, schema, data);
                w.values.push(v);
                return;
            }
            w.values.push($container::<T>::with_capacity(a.size));
        } else {
            let x = w.values.pop::<T>();
            w.values.top_mut::<$container<T>>().$push(x);
        }
        if idx < a.size {
            let start = resolve(a.data, w.base);
            w.resume(idx + 1);
            w.child_step::<T>(elem, start.add(idx * elem.width));
        }
    }
    unsafe fn read_finish(w: &mut ReadWalk, schema: &Schema, data: *const u8) -> Self {
        let a = core::ptr::read_unaligned(data as *const Array);
        if a.size == 0 {
            return $container::new();
        }
        let elem = resolve_recur(&schema.parameters[0]);
        let start = resolve(a.data, w.base);
        let width = elem.width;
        let mut out = $container::with_capacity(a.size);
        for i in 0..a.size {
            out.$push(w.child_read::<T>(elem, start.add(i * width)));
        }
        out
    }
}
    };
}
seq_impl!(Vec, push);
seq_impl!(VecDeque, push_back);

// ---- Option (?T) ----------------------------------------------------------
// The slot is a relptr. Absent -> RELNULL. Present -> an aligned slot for the
// inner T at the cursor, its relptr in this slot, then T's body.
impl<T: ToVoidstar> ToVoidstar for Option<T> {
    fn size_step(&self, w: &mut SizeWalk, schema: &Schema, _idx: usize) {
        match self {
            None => w.total += schema.width as isize,
            Some(v) => {
                let inner = resolve_recur(&schema.parameters[0]);
                w.total += (schema.width + (inner.alignment().max(1) - 1)) as isize;
                w.child(v, inner, false);
            }
        }
    }
    unsafe fn write_step(&self, w: &mut WriteWalk, dest: *mut u8, schema: &Schema, _idx: usize) {
        match self {
            None => core::ptr::write_unaligned(dest as *mut RelPtr, RELNULL),
            Some(v) => {
                let inner = resolve_recur(&schema.parameters[0]);
                let slot = w.alloc(inner);
                core::ptr::write_unaligned(dest as *mut RelPtr, to_rel(slot));
                w.child(v, slot, inner);
            }
        }
    }
}
impl<T: FromVoidstar> FromVoidstar for Option<T> {
    unsafe fn read_step(w: &mut ReadWalk, schema: &Schema, data: *const u8, _idx: usize) {
        w.push_finish::<Self>(schema, data);
        let rel = core::ptr::read_unaligned(data as *const RelPtr);
        if rel != RELNULL {
            w.child_step::<T>(&schema.parameters[0], resolve(rel, w.base));
        }
    }
    unsafe fn read_finish(w: &mut ReadWalk, schema: &Schema, data: *const u8) -> Self {
        let rel = core::ptr::read_unaligned(data as *const RelPtr);
        if rel == RELNULL {
            return None;
        }
        Some(w.child_read::<T>(&schema.parameters[0], resolve(rel, w.base)))
    }
}

// ---- Variant slots (payload-bearing `data`) --------------------------------
//
// A variant is a tag byte, seven bytes of padding, and a relative pointer to
// the arm's fields -- the same slot shape as Optional, with a tag in front.
// The walks carry the payload (`variant_payload` on each); these cover the
// nullary arm and the tag, so that layout stays here rather than in generated
// pool code: the offsets, the alignment of the out-of-line payload and the
// relative-pointer encoding are the runtime's business, and a pool cannot
// reach them anyway (RelPtr and its helpers are not part of this crate's
// public API).

/// Byte offset of a variant's payload pointer within its slot.
const VARIANT_PAYLOAD: usize = 8;

/// Write a variant slot for an arm with no fields: the tag, determined
/// padding, and a null payload pointer.
///
/// # Safety
/// `dest` must point at a writable slot of at least the schema's width.
pub unsafe fn write_variant_nullary(dest: *mut u8, tag: u8) {
    *dest = tag;
    core::ptr::write_bytes(dest.add(1), 0, VARIANT_PAYLOAD - 1);
    core::ptr::write_unaligned(dest.add(VARIANT_PAYLOAD) as *mut RelPtr, RELNULL);
}

/// The tag a variant slot carries.
///
/// # Safety
/// `data` must point at a variant slot.
pub unsafe fn read_variant_tag(data: *const u8) -> u8 {
    *data
}

// ---- Box (cycle-break indirection, I7) ------------------------------------
// Transparent on the wire: the box's step is its pointee's, so a sequence
// stepped one element per visit through a Box resumes correctly.
impl<T: ToVoidstar> ToVoidstar for Box<T> {
    const IS_LEAF: bool = T::IS_LEAF;
    fn shm_size(&self, schema: &Schema) -> usize {
        (**self).shm_size(schema)
    }
    unsafe fn write(&self, dest: *mut u8, cursor: &mut *mut u8, schema: &Schema) {
        (**self).write(dest, cursor, schema)
    }
    fn size_step(&self, w: &mut SizeWalk, schema: &Schema, idx: usize) {
        (**self).size_step(w, schema, idx)
    }
    unsafe fn write_step(&self, w: &mut WriteWalk, dest: *mut u8, schema: &Schema, idx: usize) {
        (**self).write_step(w, dest, schema, idx)
    }
}
impl<T: FromVoidstar> FromVoidstar for Box<T> {
    const IS_LEAF: bool = T::IS_LEAF;
    unsafe fn read(schema: &Schema, data: *const u8, base: *const u8) -> Self {
        Box::new(T::read(schema, data, base))
    }
    unsafe fn read_step(w: &mut ReadWalk, schema: &Schema, data: *const u8, _idx: usize) {
        w.push_finish::<Self>(schema, data);
        w.child_step::<T>(schema, data);
    }
    unsafe fn read_finish(w: &mut ReadWalk, schema: &Schema, data: *const u8) -> Self {
        Box::new(w.child_read::<T>(schema, data))
    }
}

// ---- RecBox (deferred-release cycle-break indirection) --------------------
//
// A generated `data` type holds each constructor's fields behind a `RecBox`,
// and a generated record holds any field that mentions the record behind
// one, so a value may hold its own type again at any depth. Dropping the last
// owner of a deep chain would otherwise run one drop frame per level and
// overflow the stack at a depth the heap could easily hold. While one drop is
// draining, every further last-owner drop on the thread hands its block to
// the drain's worklist and returns; the outermost drop pops and frees until
// the list is empty. Depth then costs heap.
//
// The box is reference counted: cloning shares the pointee, which keeps a
// projection out of a value (which clones the projected field) at a constant
// cost instead of a copy of the whole subtree. Values never cross threads,
// so a non-atomic count is enough.
pub mod rec_drain {
    use std::cell::Cell;

    /// A block waiting to be freed: the raw `Rc` pointer and the function
    /// that reconstitutes and drops it with its real type.
    pub type Entry = (*mut u8, unsafe fn(*mut u8));

    thread_local! {
        static ACTIVE: Cell<bool> = const { Cell::new(false) };
        // Allocated on first use and never freed, so no destructor runs at
        // thread exit and the queue is usable while other thread-locals are
        // being torn down.
        static QUEUE: Cell<*mut Vec<Entry>> = const { Cell::new(std::ptr::null_mut()) };
    }

    pub enum State {
        /// A drain is running on this thread: hand it the block.
        Active,
        /// No drain: this drop starts one.
        Inactive,
        /// Thread-local storage is gone (thread teardown): drop directly.
        Unavailable,
    }

    pub fn state() -> State {
        match ACTIVE.try_with(|a| a.get()) {
            Ok(true) => State::Active,
            Ok(false) => State::Inactive,
            Err(_) => State::Unavailable,
        }
    }

    /// Queue a block for the running drain. Fails only when the worklist
    /// cannot grow; the caller then drops synchronously.
    pub fn push(e: Entry) -> Result<(), ()> {
        QUEUE
            .try_with(|q| {
                let mut p = q.get();
                if p.is_null() {
                    p = Box::into_raw(Box::new(Vec::new()));
                    q.set(p);
                }
                // SAFETY: the queue is only ever touched from this thread and
                // no reference to it is held across a drop call.
                unsafe { (*p).try_reserve(1).map_err(|_| ())?; (*p).push(e); }
                Ok(())
            })
            .unwrap_or(Err(()))
    }

    /// Marks a drain as running for its lifetime and drains the queue when
    /// it ends, including while unwinding from a panic inside a drop.
    pub struct Guard;

    impl Guard {
        pub fn begin() -> Guard {
            let _ = ACTIVE.try_with(|a| a.set(true));
            Guard
        }
    }

    impl Drop for Guard {
        fn drop(&mut self) {
            let _ = QUEUE.try_with(|q| {
                let p = q.get();
                if p.is_null() {
                    return;
                }
                // SAFETY: as in `push`; the entry is popped before its drop
                // runs, and that drop may push more entries.
                while let Some((ptr, f)) = unsafe { (*p).pop() } {
                    // A panic in one block's drop must not escape a drop that
                    // may itself be running during unwinding.
                    let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| unsafe { f(ptr) }));
                }
            });
            let _ = ACTIVE.try_with(|a| a.set(false));
        }
    }
}

pub struct RecBox<T>(std::mem::ManuallyDrop<std::rc::Rc<T>>);

impl<T> RecBox<T> {
    pub fn new(v: T) -> Self {
        RecBox(std::mem::ManuallyDrop::new(std::rc::Rc::new(v)))
    }

    /// The value, moved out when this is the only owner and copied
    /// otherwise.
    pub fn into_inner(self) -> T
    where
        T: Clone,
    {
        let mut me = std::mem::ManuallyDrop::new(self);
        // SAFETY: `me` is never dropped, so the Rc is taken exactly once.
        let rc = unsafe { std::mem::ManuallyDrop::take(&mut me.0) };
        std::rc::Rc::try_unwrap(rc).unwrap_or_else(|rc| (*rc).clone())
    }
}

impl<T> From<T> for RecBox<T> {
    fn from(v: T) -> Self {
        RecBox::new(v)
    }
}

impl<T> std::ops::Deref for RecBox<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.0
    }
}

impl<T> Clone for RecBox<T> {
    fn clone(&self) -> Self {
        RecBox(std::mem::ManuallyDrop::new(std::rc::Rc::clone(&self.0)))
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for RecBox<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (**self).fmt(f)
    }
}

impl<T: PartialEq> PartialEq for RecBox<T> {
    fn eq(&self, other: &Self) -> bool {
        **self == **other
    }
}

unsafe fn rec_drop_thunk<T>(p: *mut u8) {
    drop(std::rc::Rc::from_raw(p as *const T));
}

impl<T> Drop for RecBox<T> {
    fn drop(&mut self) {
        // SAFETY: the field is taken exactly once, here.
        let rc = unsafe { std::mem::ManuallyDrop::take(&mut self.0) };
        // A shared block only loses this owner's count; the last owner
        // frees it, and only that drop can recurse.
        if std::rc::Rc::strong_count(&rc) != 1 {
            drop(rc);
            return;
        }
        match rec_drain::state() {
            rec_drain::State::Active => {
                let raw = std::rc::Rc::into_raw(rc) as *mut u8;
                if rec_drain::push((raw, rec_drop_thunk::<T>)).is_err() {
                    // SAFETY: `raw` came from `into_raw` just above.
                    unsafe { rec_drop_thunk::<T>(raw) };
                }
            }
            rec_drain::State::Inactive => {
                let _g = rec_drain::Guard::begin();
                drop(rc); // the pointee's RecBoxes queue rather than recurse
            }
            rec_drain::State::Unavailable => drop(rc),
        }
    }
}

// Transparent on the wire, exactly as Box.
impl<T: ToVoidstar> ToVoidstar for RecBox<T> {
    const IS_LEAF: bool = T::IS_LEAF;
    fn shm_size(&self, schema: &Schema) -> usize {
        (**self).shm_size(schema)
    }
    unsafe fn write(&self, dest: *mut u8, cursor: &mut *mut u8, schema: &Schema) {
        (**self).write(dest, cursor, schema)
    }
    fn size_step(&self, w: &mut SizeWalk, schema: &Schema, idx: usize) {
        (**self).size_step(w, schema, idx)
    }
    unsafe fn write_step(&self, w: &mut WriteWalk, dest: *mut u8, schema: &Schema, idx: usize) {
        (**self).write_step(w, dest, schema, idx)
    }
}
impl<T: FromVoidstar> FromVoidstar for RecBox<T> {
    const IS_LEAF: bool = T::IS_LEAF;
    unsafe fn read(schema: &Schema, data: *const u8, base: *const u8) -> Self {
        RecBox::new(T::read(schema, data, base))
    }
    unsafe fn read_step(w: &mut ReadWalk, schema: &Schema, data: *const u8, _idx: usize) {
        w.push_finish::<Self>(schema, data);
        w.child_step::<T>(schema, data);
    }
    unsafe fn read_finish(w: &mut ReadWalk, schema: &Schema, data: *const u8) -> Self {
        RecBox::new(w.child_read::<T>(schema, data))
    }
}

// ---- tuples ---------------------------------------------------------------
macro_rules! tuple_impl {
    ($($T:ident $idx:tt),+) => {
        impl<$($T: ToVoidstar),+> ToVoidstar for ($($T,)+) {
            fn size_step(&self, w: &mut SizeWalk, schema: &Schema, _idx: usize) {
                w.total += schema.width as isize;
                $(
                    w.child(&self.$idx, &schema.parameters[$idx], true);
                )+
            }
            unsafe fn write_step(&self, w: &mut WriteWalk, dest: *mut u8, schema: &Schema, _idx: usize) {
                $(
                    w.child(&self.$idx, dest.add(schema.offsets[$idx]), &schema.parameters[$idx]);
                )+
            }
        }
        impl<$($T: FromVoidstar),+> FromVoidstar for ($($T,)+) {
            unsafe fn read_step(w: &mut ReadWalk, schema: &Schema, data: *const u8, _idx: usize) {
                w.push_finish::<Self>(schema, data);
                $(
                    w.child_step::<$T>(&schema.parameters[$idx], data.add(schema.offsets[$idx]));
                )+
            }
            unsafe fn read_finish(w: &mut ReadWalk, schema: &Schema, data: *const u8) -> Self {
                ($(
                    w.child_read::<$T>(&schema.parameters[$idx], data.add(schema.offsets[$idx])),
                )+)
            }
        }
    };
}

// A one-element tuple. Unused while tuples came only from morloc's `(a, b)`
// syntax, which has no one-element form -- but a `data` arm carrying a
// single field is boxed as `(T,)`, so the impl is needed.
tuple_impl!(A 0);
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

/// Raise on a function value that has no origin to send. A closure morloc
/// built carries the manifold to call back into; a callable host code made
/// has no such identity, and neither does a closure whose context is not
/// wholly native (those cross by the serial path instead). Every site that
/// needs an origin goes through here, so the failure names its cause once.
pub fn require_origin(origin: Option<&ClosureOrigin>) -> ClosureOrigin {
    match origin {
        Some(o) => o.clone(),
        None => morloc_throw(
            "cannot send a function value that morloc did not create: it has no \
             manifold to call back into. Apply it in the pool that received it, \
             or have the host return the data it would compute."
                .to_string(),
        ),
    }
}

macro_rules! morloc_fn {
    ($trait:ident, $closure:ident, $fnptr:ident, $call:ident, $reify:ident, $( ($A:ident, $a:ident) ),+ ) => {
        pub trait $trait<$($A,)+ R> {
            fn $call(&self, $($a: &$A,)+) -> R;
            fn $reify(&self) -> Option<&ClosureOrigin>;
        }
        // A plain native closure. Host code declares a higher-order parameter
        // as either `F: Fn(&A..) -> R` or `impl MorlocFnN<A.., R>`; this impl
        // is what lets one generated form satisfy both. It has no origin, so a
        // value reaching morloc this way cannot be sent onward.
        impl<$($A,)+ R, F: Fn($(&$A,)+) -> R> $trait<$($A,)+ R> for F {
            #[inline]
            fn $call(&self, $($a: &$A,)+) -> R { self($($a,)+) }
            fn $reify(&self) -> Option<&ClosureOrigin> { None }
        }
        // A function value is held as `Rc<dyn $trait>`, and that is itself a
        // function value, so it can be passed wherever one is taken.
        impl<$($A,)+ R, T: $trait<$($A,)+ R> + ?Sized> $trait<$($A,)+ R> for std::rc::Rc<T> {
            #[inline]
            fn $call(&self, $($a: &$A,)+) -> R { (**self).$call($($a,)+) }
            fn $reify(&self) -> Option<&ClosureOrigin> { (**self).$reify() }
        }
        /// A morloc-built function value: the environment it captured, the
        /// manifold call, and how to reify that environment. `call` and `mk`
        /// are non-capturing, so they are plain function pointers reading the
        /// one copy of the captures -- the whole value is a single allocation
        /// and the captures are copied once.
        pub struct $closure<C, $($A,)+ R> {
            caps: C,
            call: fn(&C, $(&$A,)+) -> R,
            mk: Option<fn(&C) -> ClosureOrigin>,
            origin: std::cell::OnceCell<ClosureOrigin>,
        }
        impl<C, $($A,)+ R> $closure<C, $($A,)+ R> {
            /// A closure that can be reified: the origin is built on first
            /// use and cached, so one that never crosses pays nothing.
            pub fn new(caps: C, call: fn(&C, $(&$A,)+) -> R, mk: fn(&C) -> ClosureOrigin) -> Self {
                Self { caps, call, mk: Some(mk), origin: std::cell::OnceCell::new() }
            }
            /// A closure with no dispatch entry, so nothing can call back
            /// into it and it has no origin to offer.
            pub fn local(caps: C, call: fn(&C, $(&$A,)+) -> R) -> Self {
                Self { caps, call, mk: None, origin: std::cell::OnceCell::new() }
            }
            /// A closure reflected from another pool. It answers with the
            /// origin it ARRIVED with, so a value crossing A -> B -> C calls
            /// back to A rather than to B.
            pub fn proxy(caps: C, call: fn(&C, $(&$A,)+) -> R, origin: ClosureOrigin) -> Self {
                let cell = std::cell::OnceCell::new();
                let _ = cell.set(origin);
                Self { caps, call, mk: None, origin: cell }
            }
        }
        impl<C, $($A,)+ R> $trait<$($A,)+ R> for $closure<C, $($A,)+ R> {
            #[inline]
            fn $call(&self, $($a: &$A,)+) -> R { (self.call)(&self.caps, $($a,)+) }
            fn $reify(&self) -> Option<&ClosureOrigin> {
                if self.origin.get().is_none() {
                    let mk = self.mk?;
                    let _ = self.origin.set(mk(&self.caps));
                }
                self.origin.get()
            }
        }
        /// A capture-free closure as a plain function pointer. Coercion happens
        /// at this call, so a caller never has to name the result type -- which
        /// it could not do anyway, since the arity of a partially applied
        /// manifold's morloc type counts its captured context arguments.
        pub fn $fnptr<$($A,)+ R>(f: fn($(&$A,)+) -> R) -> fn($(&$A,)+) -> R { f }

        /// A function pointer is already the thinnest form there is, so the
        /// adapter is the identity. Anchoring this on a CONCRETE self type is
        /// what lets it coexist with the trait-object impl: the two self types
        /// are disjoint, so there is no overlap to reason about, and both
        /// parameters appear in the self type, so neither is unconstrained.
        impl<$($A,)+ R> ThinFn for fn($(&$A,)+) -> R {
            type Out = Self;
            fn thin(&self) -> Self { *self }
        }

        /// The ONE thin `Fn` adapter. Host code may declare a higher-order
        /// parameter as `F: Fn(&A..) -> R`, which a trait object cannot
        /// satisfy, so a function value is handed over as one of these.
        ///
        /// It is a trait rather than a family of named functions so the
        /// ARITY comes from the value's own type: a caller cannot read the
        /// arity off the morloc type at the call, because a partially
        /// applied manifold's type counts its captured context arguments
        /// too. The adapter has no origin, so a value that reaches morloc
        /// back through a host parameter cannot be sent onward.
        impl<$($A: 'static,)+ R: 'static> ThinFn for std::rc::Rc<dyn $trait<$($A,)+ R>> {
            type Out = Box<dyn Fn($(&$A,)+) -> R>;
            fn thin(&self) -> Self::Out {
                let v = self.clone();
                Box::new(move |$($a,)+| v.$call($($a,)+))
            }
        }
    };
}

// A function value of no arguments: a suspension. Written out because the
// macro's argument list is non-empty.
pub trait MorlocFn0<R> {
    fn call0(&self) -> R;
    fn reify0(&self) -> Option<&ClosureOrigin>;
}
impl<R, F: Fn() -> R> MorlocFn0<R> for F {
    #[inline]
    fn call0(&self) -> R { self() }
    fn reify0(&self) -> Option<&ClosureOrigin> { None }
}
impl<R, T: MorlocFn0<R> + ?Sized> MorlocFn0<R> for std::rc::Rc<T> {
    #[inline]
    fn call0(&self) -> R { (**self).call0() }
    fn reify0(&self) -> Option<&ClosureOrigin> { (**self).reify0() }
}
pub struct Closure0<C, R> {
    caps: C,
    call: fn(&C) -> R,
    mk: Option<fn(&C) -> ClosureOrigin>,
    origin: std::cell::OnceCell<ClosureOrigin>,
}
impl<C, R> Closure0<C, R> {
    pub fn new(caps: C, call: fn(&C) -> R, mk: fn(&C) -> ClosureOrigin) -> Self {
        Self { caps, call, mk: Some(mk), origin: std::cell::OnceCell::new() }
    }
    pub fn local(caps: C, call: fn(&C) -> R) -> Self {
        Self { caps, call, mk: None, origin: std::cell::OnceCell::new() }
    }
    pub fn proxy(caps: C, call: fn(&C) -> R, origin: ClosureOrigin) -> Self {
        let cell = std::cell::OnceCell::new();
        let _ = cell.set(origin);
        Self { caps, call, mk: None, origin: cell }
    }
}
impl<C, R> MorlocFn0<R> for Closure0<C, R> {
    #[inline]
    fn call0(&self) -> R { (self.call)(&self.caps) }
    fn reify0(&self) -> Option<&ClosureOrigin> {
        if self.origin.get().is_none() {
            let mk = self.mk?;
            let _ = self.origin.set(mk(&self.caps));
        }
        self.origin.get()
    }
}
/// A function value rendered as a plain `Fn`, for host code that declares a
/// higher-order parameter that way. See the impls generated beside each
/// `MorlocFnN`.
pub trait ThinFn {
    type Out;
    fn thin(&self) -> Self::Out;
}
pub fn fn_ptr0<R>(f: fn() -> R) -> fn() -> R { f }
impl<R> ThinFn for fn() -> R {
    type Out = Self;
    fn thin(&self) -> Self { *self }
}
impl<R: 'static> ThinFn for std::rc::Rc<dyn MorlocFn0<R>> {
    type Out = Box<dyn Fn() -> R>;
    fn thin(&self) -> Self::Out {
        let v = self.clone();
        Box::new(move || v.call0())
    }
}

morloc_fn!(MorlocFn1, Closure1, fn_ptr1, call1, reify1, (A1, a1));
morloc_fn!(MorlocFn2, Closure2, fn_ptr2, call2, reify2, (A1, a1), (A2, a2));
morloc_fn!(MorlocFn3, Closure3, fn_ptr3, call3, reify3, (A1, a1), (A2, a2), (A3, a3));
morloc_fn!(MorlocFn4, Closure4, fn_ptr4, call4, reify4, (A1, a1), (A2, a2), (A3, a3), (A4, a4));
morloc_fn!(MorlocFn5, Closure5, fn_ptr5, call5, reify5, (A1, a1), (A2, a2), (A3, a3), (A4, a4), (A5, a5));
morloc_fn!(MorlocFn6, Closure6, fn_ptr6, call6, reify6, (A1, a1), (A2, a2), (A3, a3), (A4, a4), (A5, a5), (A6, a6));
morloc_fn!(MorlocFn7, Closure7, fn_ptr7, call7, reify7, (A1, a1), (A2, a2), (A3, a3), (A4, a4), (A5, a5), (A6, a6), (A7, a7));
morloc_fn!(MorlocFn8, Closure8, fn_ptr8, call8, reify8, (A1, a1), (A2, a2), (A3, a3), (A4, a4), (A5, a5), (A6, a6), (A7, a7), (A8, a8));

// ---------------------------------------------------------------------------
// Packet bridge (production). `put_value` serializes a native value into a
// C-allocated data packet (I1); `get_value` reconstructs a native value from
// an argument packet. Both bracket the walk in a recur scope.
// ---------------------------------------------------------------------------

/// # Safety
/// `schema` must describe `value`'s wire type.
pub unsafe fn put_value<T: ToVoidstar>(value: &T, schema: &Schema) -> *mut u8 {
    put_value_as(value, schema, false)
}

/// `put_value`, with `self_contained` asking for a packet that carries
/// the value inside it rather than a reference to a shared-memory block:
/// for a value that must outlive this dispatch's blocks, such as a
/// closure's captured value applied back later from another pool.
///
/// # Safety
/// `schema` must describe `value`'s wire type.
pub unsafe fn put_value_as<T: ToVoidstar>(value: &T, schema: &Schema, self_contained: bool) -> *mut u8 {
    let _recur = RecurScope::enter(schema);
    if schema.serial_type == SerialType::Table {
        return match arrow_put(value, schema) {
            Ok(relptr) => {
                let cs = cschema_of(schema);
                let mut err: *mut c_char = std::ptr::null_mut();
                let packet = if self_contained {
                    let block = rel2abs(relptr, &mut err);
                    if block.is_null() {
                        return fail_packet_from_c(err, "rel2abs failed in put_value");
                    }
                    make_inline_data_packet(block, cs, &mut err)
                } else {
                    make_arrow_data_packet(relptr, cs)
                };
                if packet.is_null() {
                    return fail_packet_from_c(err, "table packet construction failed in put_value");
                }
                packet
            }
            Err(err) => fail_packet_from_c(err, "arrow_to_shm failed in put_value"),
        };
    }
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
    let packet = if self_contained {
        make_inline_data_packet(root as *mut c_void, cs, &mut err)
    } else {
        make_data_packet_auto(root as *mut c_void, relptr, cs, &mut err)
    };
    if packet.is_null() {
        return fail_packet_from_c(err, "packet construction failed in put_value"); // guard shfree
    }
    // Defer the root's free to the next dispatch (I3). Safe for both RPTR
    // packets (data still referenced) and inline packets (data already copied
    // into the C-allocated packet; freeing later is harmless).
    guard.commit();
    track(root as *mut c_void);
    packet
}

/// Lay a table out as a block of this pool's own, released with this
/// dispatch, and return its reference. `Err` carries the C error string.
unsafe fn arrow_put<T: ToVoidstar>(value: &T, schema: &Schema) -> Result<isize, *mut c_char> {
    let (mut array, ffi_schema) = match value.arrow_export() {
        Some(pair) => pair,
        None => morloc_infra_abort("Table-typed value is not an Arrow record batch"),
    };
    let cs = cschema_of(schema);
    let mut err: *mut c_char = std::ptr::null_mut();
    // Consumes `array`; its Drop is then a no-op.
    let relptr = arrow_to_shm_typed(&mut array, &ffi_schema, cs, &mut err);
    drop(array);
    drop(ffi_schema);
    if !err.is_null() {
        return Err(err);
    }
    let root = rel2abs(relptr, &mut err);
    discard_err(err);
    if !root.is_null() {
        track(root);
    }
    Ok(relptr)
}

/// # Safety
/// `packet` must be a valid data packet whose schema matches `schema`.
pub unsafe fn get_value<T: FromVoidstar>(packet: *const u8, schema: &Schema) -> T {
    let _recur = RecurScope::enter(schema);
    let source = *packet.add(PKT_SOURCE_OFF);
    let format = *packet.add(PKT_FORMAT_OFF);

    if schema.serial_type == SerialType::Table {
        // A table is a block. It arrives by reference (an Arrow packet) or
        // in a form the runtime materializes into a block of this pool's
        // own (a cached result read back from a file, a captured value
        // carried inline).
        if format == PKT_FORMAT_ARROW && source != PKT_SOURCE_RPTR {
            morloc_infra_abort("Arrow packet does not name a shared-memory block");
        }
        let materialized = source != PKT_SOURCE_RPTR;
        let cs = cschema_of(schema);
        let mut err: *mut c_char = std::ptr::null_mut();
        let block = get_morloc_data_packet_value(packet, cs, &mut err);
        if !err.is_null() {
            morloc_throw(cstr_take(err));
        }
        // A materialized block is released here unless the tracker takes
        // it; a referenced one belongs to its sender until acquired.
        let guard = ShmGuard(if materialized { Some(block as *mut c_void) } else { None });
        if arrow_validate(block as *const c_void, cs, &mut err) != 0 {
            morloc_throw(cstr_take(err));
        }
        // Hold the block for as long as the batch references its buffers,
        // releasing it at the next dispatch. A table that arrived by
        // reference needs one taken on this pool's behalf; the sender
        // donated one before sending, so a refusal means the block is gone
        // and the view would read scrubbed memory.
        if !materialized {
            let acquired = shincref(block as *mut c_void, &mut err);
            discard_err(err);
            err = std::ptr::null_mut();
            if !acquired {
                morloc_infra_abort("received table's shared-memory block is no longer live");
            }
        }
        guard.commit();
        track(block as *mut c_void);
        let rel = abs2rel(block as *mut c_void, &mut err);
        if err.is_null() {
            arrow_borrow_register(block, rel);
        }
        discard_err(err);
        err = std::ptr::null_mut();
        let mut ffi_schema = FFI_ArrowSchema::empty();
        let mut array = FFI_ArrowArray::empty();
        if arrow_from_shm(block as *const c_void, &mut ffi_schema, &mut array, &mut err) != 0 {
            morloc_throw(cstr_take(err));
        }
        return match <T as FromVoidstar>::arrow_import(array, &ffi_schema) {
            Some(v) => v,
            None => morloc_infra_abort("Table-typed value requested as a non-Arrow type"),
        };
    }
    if format == PKT_FORMAT_ARROW {
        morloc_infra_abort("Arrow packet received for a non-table type");
    }

    let compression = *packet.add(PKT_COMPRESSION_OFF);
    let encryption = *packet.add(PKT_ENCRYPTION_OFF);
    // A payload that is compressed or encrypted cannot be walked where it
    // lies. Those fall through to the general path, which expands the body
    // and re-enters. Testing for the plain values rather than against the
    // known transforms keeps a future one from being read as raw bytes.
    if source == PKT_SOURCE_MESG
        && format == PKT_FORMAT_VOIDSTAR
        && compression == PKT_COMPRESSION_NONE
        && encryption == PKT_ENCRYPTION_NONE
    {
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
        // A value that arrived by reference needs a reference of this
        // pool's own so the sender's flush cannot reclaim it while it is
        // read or forwarded. The sender donated one before sending, so a
        // refusal means the block is already gone.
        let acquired = shincref(voidstar as *mut c_void, &mut err);
        discard_err(err);
        if !acquired {
            morloc_infra_abort("received value's shared-memory block is no longer live");
        }
        track(voidstar as *mut c_void);
    } else {
        // A payload that did not arrive by reference was materialized into a
        // block of this pool's own, and nothing else will free it. Hand it to
        // the tracker, which is also panic-safe: the read below can throw and
        // a throwing dispatch answers with a fail packet rather than ending
        // the pool, so a block dropped there would be lost once per request.
        track(voidstar as *mut c_void);
    }
    <T as FromVoidstar>::read(schema, voidstar, std::ptr::null())
}

/// Serialize a captured value into a SELF-CONTAINED wire packet: the packet
/// embeds its voidstar rather than a SHM relptr that would dangle once the
/// producing manifold's SHM is reclaimed (a crossing closure is applied
/// back later, from another pool). Returns the owned packet bytes. Port of
/// the C++ pool's `_mlc_reify_capture`.
///
/// # Safety
/// `schema` must describe `value`'s wire type.
pub unsafe fn reify_capture<T: ToVoidstar>(value: &T, schema: &Schema) -> Vec<u8> {
    let packet = put_value_as(value, schema, true);
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
        morloc_infra_abort(cstr_take(err));
    }

    pool_mark_busy();
    let result = send_and_receive_over_socket(socket_path.as_ptr(), packet, &mut err);
    pool_mark_idle();
    libc::free(packet as *mut c_void);
    if !err.is_null() {
        morloc_infra_abort(cstr_take(err));
    }

    finalize_call_result(result)
}

/// Shared post-processing for a packet returned by a cross-pool or remote call:
/// a fail packet becomes a catchable throw (I2); an RPTR (SHM-backed) result is
/// increfed + tracked so the peer's next dispatch flush cannot reclaim data
/// this pool still references (I3).
unsafe fn finalize_call_result(result: *mut u8) -> *mut u8 {
    let mut fail_err: *mut c_char = std::ptr::null_mut();
    let fail_msg = get_morloc_data_packet_error_message(result, &mut fail_err);
    discard_err(fail_err);
    if !fail_msg.is_null() {
        let msg = cstr_take(fail_msg);
        libc::free(result as *mut c_void);
        morloc_throw(msg);
    }

    if *result.add(PKT_SOURCE_OFF) == PKT_SOURCE_RPTR {
        let meta = core::ptr::read_unaligned(result.add(PKT_OFFSET_OFF) as *const u32) as usize;
        let rel = core::ptr::read_unaligned(result.add(PKT_HEADER_SIZE + meta) as *const RelPtr);
        let mut rerr: *mut c_char = std::ptr::null_mut();
        let voidstar = rel2abs(rel, &mut rerr);
        discard_err(rerr);
        if !voidstar.is_null() {
            // The callee took a reference before sending; it is ours now.
            // Inherit it rather than adding another.
            track(voidstar);
        }
    }

    result
}

/// Issue a remote (SLURM/nexus-dispatched) call to manifold `mid`. The args are
/// already-serialized argument packets (as in `foreign_call`); the runtime
/// rewrites them to self-contained form and dispatches to the nexus. Mirrors
/// the C++ pool's `remote_call` path.
///
/// # Safety
/// `args` must be valid argument packets for the remote manifold `mid`.
#[allow(clippy::too_many_arguments)]
pub unsafe fn remote_call(
    mid: u32,
    socket_basename: &str,
    cache_dir: &str,
    mem: i32,
    time: i32,
    cpus: i32,
    gpus: i32,
    args: &[*const u8],
) -> *mut u8 {
    let socket_c = match CString::new(socket_basename) {
        Ok(s) => s,
        Err(_) => morloc_throw("remote_call: socket name contains an interior NUL"),
    };
    let cache_c = match CString::new(cache_dir) {
        Ok(s) => s,
        Err(_) => morloc_throw("remote_call: cache dir contains an interior NUL"),
    };
    let resources = Resources { memory: mem, time, cpus, gpus };
    let mut err: *mut c_char = std::ptr::null_mut();
    pool_mark_busy();
    let result = remote_call_ffi(
        mid as i32,
        socket_c.as_ptr(),
        cache_c.as_ptr(),
        &resources,
        args.as_ptr(),
        args.len(),
        &mut err,
    );
    pool_mark_idle();
    if !err.is_null() {
        morloc_throw(cstr_take(err));
    }
    finalize_call_result(result)
}

// ---- on-disk result cache (`a@fn` / @cache) -------------------------------
//
// Content-addressed cache: key = hash(manifold id + serialized args), value =
// the result packet. On a hit the cached packet is returned directly (no
// recompute); on a miss the body runs and its packet is stored. Mirrors the
// C++ pool's cache wrapping (morloc_cache_* C ABI).

/// Compute the cache key over the manifold id and the serialized arg packets.
///
/// # Safety
/// `packets` must be valid argument packets whose wire types match `schemas`.
pub unsafe fn cache_key(mid: u32, packets: &[*const u8], schemas: &[&str]) -> u64 {
    // Small per-call CString build of the arg schemas; negligible on a cache
    // path (the recompute it guards dominates), so kept simple over c"" literals.
    let schema_cs: Vec<CString> = schemas.iter().map(|s| cstr_arg(s, "@cache")).collect();
    let ptrs: Vec<*const c_char> = schema_cs.iter().map(|c| c.as_ptr()).collect();
    let mut err: *mut c_char = std::ptr::null_mut();
    let key = morloc_cache_key_compute(mid, packets.as_ptr(), ptrs.as_ptr(), packets.len(), &mut err);
    check_err(err);
    key
}

/// Look up a cached result packet, recording a hit or miss. A null result is a
/// miss (the caller then computes and calls `cache_store`).
///
/// # Safety
/// Calls into the libmorloc C ABI.
pub unsafe fn cache_lookup(key: u64, label: &str) -> *mut u8 {
    let label_c = cstr_arg(label, "@cache");
    let mut size: usize = 0;
    let mut err: *mut c_char = std::ptr::null_mut();
    let cached = morloc_cache_lookup(key, label_c.as_ptr(), &mut size, &mut err);
    check_err(err);
    if cached.is_null() {
        morloc_cache_record_miss();
    } else {
        morloc_cache_record_hit();
    }
    cached
}

/// Store a freshly-computed result packet under `key`, recording the store.
///
/// # Safety
/// `data` must be a valid result packet whose wire type matches `schema`.
pub unsafe fn cache_store(key: u64, label: &str, data: *const u8, schema: &str) {
    let label_c = cstr_arg(label, "@cache");
    let schema_c = cstr_arg(schema, "@cache");
    let mut err: *mut c_char = std::ptr::null_mut();
    let size = morloc_packet_size(data, &mut err);
    check_err(err);
    let ok = morloc_cache_store(key, label_c.as_ptr(), data, size, schema_c.as_ptr(), &mut err);
    if !ok {
        if !err.is_null() {
            morloc_throw(cstr_take(err));
        }
        morloc_throw("@cache: cache_store failed");
    }
    morloc_cache_record_store();
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

/// String interpolation: alternate `fragments[0], insertions[0], fragments[1],
/// insertions[1], ..., fragments[n]`. `fragments` has exactly one more element
/// than `insertions` (the compiler guarantees this). Mirrors the C++ pool's
/// `interweave_strings`. Both sides are borrowed (`&str`) and appended in place,
/// so no argument is copied before the single sized allocation.
pub fn interweave_strings(fragments: &[&str], insertions: &[&str]) -> String {
    let cap: usize = fragments.iter().map(|s| s.len()).sum::<usize>()
        + insertions.iter().map(|s| s.len()).sum::<usize>();
    let mut out = String::with_capacity(cap);
    for (i, ins) in insertions.iter().enumerate() {
        out.push_str(fragments[i]);
        out.push_str(ins);
    }
    out.push_str(fragments[fragments.len() - 1]);
    out
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

    /// Lay a flattened voidstar into a packet body and read it back through
    /// `get_value`, which is what a pool actually calls. `roundtrip` covers
    /// the walk; this covers the decision to walk in place, including the
    /// header fields that decide it.
    unsafe fn inline_packet_roundtrip<T: ToVoidstar + FromVoidstar>(
        schema_str: &str,
        value: &T,
        compression: u8,
    ) -> T {
        let schema = parse_schema(schema_str).expect("parse schema");
        let body = {
            let _recur = RecurScope::enter(&schema);
            let total = value.shm_size(&schema).max(1);
            let mut buf = vec![0u8; total + 64];
            let base = buf.as_mut_ptr();
            TEST_BASE.with(|b| b.set(Some(base as usize)));
            let mut cursor = base.add(schema.width);
            value.write(base, &mut cursor, &schema);
            TEST_BASE.with(|b| b.set(None));
            buf
        };

        let mut packet = vec![0u8; PKT_HEADER_SIZE + body.len()];
        packet[PKT_SOURCE_OFF] = PKT_SOURCE_MESG;
        packet[PKT_FORMAT_OFF] = PKT_FORMAT_VOIDSTAR;
        packet[PKT_COMPRESSION_OFF] = compression;
        packet[PKT_ENCRYPTION_OFF] = PKT_ENCRYPTION_NONE;
        // No metadata block; the body starts immediately after the header.
        packet[PKT_OFFSET_OFF..PKT_OFFSET_OFF + 4].copy_from_slice(&0u32.to_le_bytes());
        packet[PKT_LENGTH_OFF..PKT_LENGTH_OFF + 8]
            .copy_from_slice(&(body.len() as u64).to_le_bytes());
        packet[PKT_HEADER_SIZE..].copy_from_slice(&body);

        // Relptrs in the body are relative to wherever the body begins, so
        // point the walk's base at its position inside the packet.
        let base = packet.as_ptr().add(PKT_HEADER_SIZE);
        TEST_BASE.with(|b| b.set(Some(base as usize)));
        let out = get_value::<T>(packet.as_ptr(), &schema);
        TEST_BASE.with(|b| b.set(None));
        out
    }

    #[test]
    fn inline_packets_read_in_place() {
        unsafe {
            assert_eq!(inline_packet_roundtrip("i8", &42i64, PKT_COMPRESSION_NONE), 42i64);
            assert_eq!(
                inline_packet_roundtrip("s", &"hello".to_string(), PKT_COMPRESSION_NONE),
                "hello".to_string(),
            );
            assert_eq!(
                inline_packet_roundtrip("s", &String::new(), PKT_COMPRESSION_NONE),
                String::new(),
            );
            let nested: Vec<Vec<i64>> = vec![vec![1, 2], vec![], vec![3]];
            assert_eq!(
                inline_packet_roundtrip("aai8", &nested, PKT_COMPRESSION_NONE),
                nested,
            );
            let strs: Vec<String> = vec!["a".into(), "".into(), "ccc".into()];
            assert_eq!(
                inline_packet_roundtrip("as", &strs, PKT_COMPRESSION_NONE),
                strs,
            );
        }
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
    // translator emits, cycle-break box included. Schema:
    // &2LL m2 4head j 4tail ?^2LL. The default drop is iterative through
    // RecBox, so a deep chain tests the walk and the release together.
    #[derive(Debug, PartialEq, Clone)]
    struct LL {
        head: i64,
        tail: Option<RecBox<LL>>,
    }
    impl ToVoidstar for LL {
        fn size_step(&self, w: &mut SizeWalk, schema: &Schema, _idx: usize) {
            w.total += schema.width as isize;
            w.child(&self.head, &schema.parameters[0], true);
            w.child(&self.tail, &schema.parameters[1], true);
        }
        unsafe fn write_step(&self, w: &mut WriteWalk, dest: *mut u8, schema: &Schema, _idx: usize) {
            w.child(&self.head, dest.add(schema.offsets[0]), &schema.parameters[0]);
            w.child(&self.tail, dest.add(schema.offsets[1]), &schema.parameters[1]);
        }
    }
    impl FromVoidstar for LL {
        unsafe fn read_step(w: &mut ReadWalk, schema: &Schema, data: *const u8, _idx: usize) {
            w.push_finish::<Self>(schema, data);
            w.child_step::<i64>(&schema.parameters[0], data.add(schema.offsets[0]));
            w.child_step::<Option<RecBox<LL>>>(&schema.parameters[1], data.add(schema.offsets[1]));
        }
        unsafe fn read_finish(w: &mut ReadWalk, schema: &Schema, data: *const u8) -> Self {
            LL {
                head: w.child_read::<i64>(&schema.parameters[0], data.add(schema.offsets[0])),
                tail: w.child_read::<Option<RecBox<LL>>>(&schema.parameters[1], data.add(schema.offsets[1])),
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
                tail: Some(RecBox::new(LL {
                    head: 2,
                    tail: Some(RecBox::new(LL { head: 3, tail: None })),
                })),
            };
            assert_eq!(roundtrip::<LL>(SCHEMA, &chain), chain);

            // Nested under a Vec -- the case the C++ write path mishandles;
            // here each element's RecurScope::enter makes the back-ref resolve.
            let list = vec![
                LL { head: 10, tail: None },
                LL { head: 20, tail: Some(RecBox::new(LL { head: 21, tail: None })) },
            ];
            assert_eq!(roundtrip::<Vec<LL>>(&format!("a{SCHEMA}"), &list), list);
        }
    }

    // A chain far deeper than a 2 MiB test thread's stack could walk one
    // frame per level: the size, write and read passes must stay flat.
    #[test]
    fn deep_recursive_record() {
        // Built, walked, counted and dropped on a thread whose stack could
        // not hold one frame per level.
        on_small_stack(|| unsafe {
            const SCHEMA: &str = "&2LLm24headj4tail?^2LL";
            const DEPTH: i64 = 1_000_000;
            let mut chain = LL { head: 0, tail: None };
            for i in 1..=DEPTH {
                chain = LL { head: i, tail: Some(RecBox::new(chain)) };
            }
            let got = roundtrip::<LL>(SCHEMA, &chain);
            let mut n = 0;
            let mut cur = &got;
            while let Some(next) = &cur.tail {
                n += 1;
                cur = next;
            }
            assert_eq!(n, DEPTH);
            assert_eq!(got.head, DEPTH);
            assert_eq!(cur.head, 0);
            // A shared tail: the chain is released through both owners.
            let shared = LL { head: -1, tail: got.tail.clone() };
            drop(got);
            drop(shared);
            drop(chain);
        });
    }

    /// Run on a thread far smaller than any pool worker's, so a walk or a
    /// drop that spends a frame per level fails here first.
    fn on_small_stack<F: FnOnce() + Send + 'static>(f: F) {
        std::thread::Builder::new()
            .stack_size(256 * 1024)
            .spawn(f)
            .unwrap()
            .join()
            .unwrap();
    }

    // The enum shape the translator emits for a payload-bearing `data`:
    // every arm's fields behind one RecBox holding a tuple.
    #[derive(Debug, PartialEq, Clone)]
    enum Tree {
        Leaf,
        Node(RecBox<(i64, Tree, Tree)>),
    }
    impl ToVoidstar for Tree {
        fn size_step(&self, w: &mut SizeWalk, schema: &Schema, _idx: usize) {
            match self {
                Self::Leaf => w.total += schema.width as isize,
                Self::Node(mlc_b) => w.variant_payload(schema, &schema.parameters[1], mlc_b),
            }
        }
        unsafe fn write_step(&self, w: &mut WriteWalk, dest: *mut u8, schema: &Schema, _idx: usize) {
            match self {
                Self::Leaf => write_variant_nullary(dest, 0),
                Self::Node(mlc_b) => w.variant_payload(dest, &schema.parameters[1], 1, mlc_b),
            }
        }
    }
    impl FromVoidstar for Tree {
        unsafe fn read_step(w: &mut ReadWalk, schema: &Schema, data: *const u8, _idx: usize) {
            w.push_finish::<Self>(schema, data);
            match read_variant_tag(data) {
                0 => {}
                1 => {
                    let p = w.payload_ptr(data);
                    w.child_step::<RecBox<(i64, Tree, Tree)>>(&schema.parameters[1], p);
                }
                t => panic!("Tree: no constructor for tag {}", t),
            }
        }
        unsafe fn read_finish(w: &mut ReadWalk, schema: &Schema, data: *const u8) -> Self {
            match read_variant_tag(data) {
                0 => Self::Leaf,
                1 => {
                    let p = w.payload_ptr(data);
                    Self::Node(w.child_read::<RecBox<(i64, Tree, Tree)>>(&schema.parameters[1], p))
                }
                t => panic!("Tree: no constructor for tag {}", t),
            }
        }
    }

    #[test]
    fn deep_recursive_variant() {
        on_small_stack(|| unsafe {
            const SCHEMA: &str = "&4Treev24Leaf04Node3i8^4Tree^4Tree";
            const DEPTH: i64 = 1_000_000;
            let mut chain = Tree::Leaf;
            for i in 1..=DEPTH {
                chain = Tree::Node(RecBox::new((i, Tree::Leaf, chain)));
            }
            let got = roundtrip::<Tree>(SCHEMA, &chain);
            let mut n = 0;
            let mut cur = &got;
            while let Tree::Node(b) = cur {
                n += 1;
                cur = &b.2;
            }
            assert_eq!(n, DEPTH);
            // Projecting the spine clones the box, not the subtree.
            let tail = match &got { Tree::Node(b) => b.2.clone(), Tree::Leaf => Tree::Leaf };
            drop(got);
            drop(tail);
            drop(chain);
        });
    }

    // A recursive record whose compound fields have no back-reference of
    // their own on either side of the one that does: the finish must pop
    // each child's value in field order whichever way it was read.
    #[derive(Debug, PartialEq, Clone)]
    struct Doc {
        items: Vec<(i64, String)>,
        next: Option<Box<Doc>>,
        tag: (i64, i64),
    }
    impl Drop for Doc {
        fn drop(&mut self) {
            let mut next = self.next.take();
            while let Some(mut b) = next {
                next = b.next.take();
            }
        }
    }
    impl ToVoidstar for Doc {
        fn size_step(&self, w: &mut SizeWalk, schema: &Schema, _idx: usize) {
            w.total += schema.width as isize;
            w.child(&self.items, &schema.parameters[0], true);
            w.child(&self.next, &schema.parameters[1], true);
            w.child(&self.tag, &schema.parameters[2], true);
        }
        unsafe fn write_step(&self, w: &mut WriteWalk, dest: *mut u8, schema: &Schema, _idx: usize) {
            w.child(&self.items, dest.add(schema.offsets[0]), &schema.parameters[0]);
            w.child(&self.next, dest.add(schema.offsets[1]), &schema.parameters[1]);
            w.child(&self.tag, dest.add(schema.offsets[2]), &schema.parameters[2]);
        }
    }
    impl FromVoidstar for Doc {
        unsafe fn read_step(w: &mut ReadWalk, schema: &Schema, data: *const u8, _idx: usize) {
            w.push_finish::<Self>(schema, data);
            w.child_step::<Vec<(i64, String)>>(&schema.parameters[0], data.add(schema.offsets[0]));
            w.child_step::<Option<Box<Doc>>>(&schema.parameters[1], data.add(schema.offsets[1]));
            w.child_step::<(i64, i64)>(&schema.parameters[2], data.add(schema.offsets[2]));
        }
        unsafe fn read_finish(w: &mut ReadWalk, schema: &Schema, data: *const u8) -> Self {
            Doc {
                items: w.child_read::<Vec<(i64, String)>>(&schema.parameters[0], data.add(schema.offsets[0])),
                next: w.child_read::<Option<Box<Doc>>>(&schema.parameters[1], data.add(schema.offsets[1])),
                tag: w.child_read::<(i64, i64)>(&schema.parameters[2], data.add(schema.offsets[2])),
            }
        }
    }

    #[test]
    fn flat_fields_beside_a_recursive_one() {
        unsafe {
            const SCHEMA: &str = "&3Docm35itemsat2i8s4next?^3Doc3tagt2i8i8";
            let mut doc = Doc { items: vec![(0, "z".to_string())], next: None, tag: (0, 0) };
            for i in 1..=3 {
                doc = Doc {
                    items: vec![(i, format!("a{i}")), (i + 10, format!("b{i}"))],
                    next: Some(Box::new(doc)),
                    tag: (i, -i),
                };
            }
            assert_eq!(roundtrip::<Doc>(SCHEMA, &doc), doc);
        }
    }

    // --- function values -------------------------------------------------

    fn origin_of(n: i64) -> ClosureOrigin {
        ("rust".to_string(), n, vec![vec![1u8]])
    }

    #[test]
    fn closure_calls_through_a_trait_object() {
        let f: std::rc::Rc<dyn MorlocFn1<i64, i64>> = std::rc::Rc::new(Closure1::local(
            (7i64,),
            |c: &(i64,), a: &i64| c.0 + a,
        ));
        assert_eq!(f.call1(&5), 12);
        // an Rc of a function value is a function value
        let g: std::rc::Rc<dyn MorlocFn1<i64, i64>> = std::rc::Rc::new(f.clone());
        assert_eq!(g.call1(&5), 12);
    }

    #[test]
    fn a_local_closure_has_no_origin() {
        let f = Closure0::local((), |_: &()| 1i64);
        assert!(f.reify0().is_none());
    }

    #[test]
    fn the_origin_is_built_once_and_cached() {
        // `mk` is a plain fn pointer, so the count lives beside the captures
        let f = Closure0::new(
            (std::cell::Cell::new(0u32),),
            |_: &(std::cell::Cell<u32>,)| 1i64,
            |c: &(std::cell::Cell<u32>,)| {
                c.0.set(c.0.get() + 1);
                origin_of(42)
            },
        );
        assert_eq!(f.reify0(), Some(&origin_of(42)));
        assert_eq!(f.reify0(), Some(&origin_of(42)));
        assert_eq!(f.reify0(), Some(&origin_of(42)));
        assert_eq!(f.caps.0.get(), 1, "the origin must be built exactly once");
    }

    #[test]
    fn a_proxy_answers_with_the_origin_it_arrived_with() {
        // a value crossing A -> B -> C must call back to A, not to B
        let f = Closure1::proxy((), |_: &(), a: &i64| *a, origin_of(3));
        assert_eq!(f.reify1(), Some(&origin_of(3)));
        assert_eq!(f.reify1(), Some(&origin_of(3)));
    }

}
