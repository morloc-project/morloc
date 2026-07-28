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

use std::cell::Cell;
use std::ffi::{c_char, c_void, CString};
use morloc_runtime_types::schema::{Schema, SerialType};
use morloc_runtime_types::shm_types::{align_up, encode_relptr, relptr_offset, Array, RelPtr, RELNULL};

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
                             schema: *const CSchemaOpaque, errmsg: *mut *mut c_char) -> *mut u8;
    fn get_morloc_data_packet_value(data: *const u8, schema: *const CSchemaOpaque,
                                    errmsg: *mut *mut c_char) -> *mut u8;
    fn make_fail_packet(msg: *const c_char) -> *mut u8;
}

// The C-ABI functions take a `const Schema*` (C struct). We bridge our Rust
// `Schema` to it via `morloc_runtime_types::cschema::CSchema`. Treated opaquely
// at the extern boundary; the real layout lives in the rlib.
#[repr(C)]
struct CSchemaOpaque {
    _private: [u8; 0],
}

use morloc_runtime_types::cschema::CSchema;

#[inline]
fn cschema_of(schema: &Schema) -> *mut CSchemaOpaque {
    CSchema::from_rust(schema) as *mut CSchemaOpaque
}
#[inline]
unsafe fn cschema_free(cs: *mut CSchemaOpaque) {
    CSchema::free(cs as *mut CSchema);
}

// Packet header byte offsets (wire format is locked; see
// morloc-runtime-types::packet PacketHeader, `#[repr(C, packed)]`, 32 bytes).
const PKT_HEADER_SIZE: usize = 32;
const PKT_SOURCE_OFF: usize = 12; // command.data.source
const PKT_FORMAT_OFF: usize = 13; // command.data.format
const PKT_OFFSET_OFF: usize = 20; // u32 metadata-block length
const PKT_SOURCE_MESG: u8 = 0x00;
const PKT_SOURCE_RPTR: u8 = 0x02;
const PKT_FORMAT_VOIDSTAR: u8 = 0x04;

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
            unsafe { shfree(*ptr, &mut err); }
            if !err.is_null() {
                unsafe { libc::free(err as *mut c_void); }
            }
        }
        t.set(Vec::new());
    });
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
            unsafe { shfree(ptr, &mut err); }
            if !err.is_null() {
                unsafe { libc::free(err as *mut c_void); }
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
macro_rules! int_impl {
    ($t:ty) => {
        impl ToVoidstar for $t {
            #[inline]
            fn shm_size(&self, schema: &Schema) -> usize { schema.width }
            #[inline]
            unsafe fn write(&self, dest: *mut u8, _cursor: &mut *mut u8, schema: &Schema) {
                if schema.serial_type == SerialType::Int {
                    // Inline BigInt [size=1, value] (16 bytes); v1 never emits
                    // multi-limb (I6 handles consume).
                    core::ptr::write_unaligned(dest as *mut i64, 1);
                    core::ptr::write_unaligned((dest as *mut i64).add(1), *self as i64);
                } else {
                    core::ptr::write_unaligned(dest as *mut $t, *self); // I8 unaligned
                }
            }
        }
        impl FromVoidstar for $t {
            #[inline]
            unsafe fn read(schema: &Schema, data: *const u8, _base: *const u8) -> Self {
                read_int(schema, data) as $t
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
    cschema_free(cs);
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
    cschema_free(cs);
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

// ---- fail packets (I1: always C-allocated via make_fail_packet) -----------
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
                unsafe { fail_packet(msg) }
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
