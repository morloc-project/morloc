//! Debug-trace runtime.
//!
//! When the compiler emits a manifold with `morloc make --debug`, the
//! generated body is wrapped in a try/catch (per-language). On any
//! thrown exception the catch block calls
//! [`morloc_debug_record_frame`] with the manifold's midx and a list of
//! its args (each pre-serialized to a packet with a schema string).
//! This module:
//!
//! 1. Materializes each arg's payload to msgpack bytes.
//! 2. Content-addresses the bytes via xxh64.
//! 3. Atomically writes `<debug_dir>/inputs/<hash>.pkt` (only if the
//!    file doesn't already exist -- 5GB shared inputs hit disk once).
//! 4. Appends a frame entry to the thread-local trace.
//!
//! ## Runtime limits
//!
//! Three env vars cap how much state debug-trace accumulates per
//! dispatch (all also settable via nexus CLI flags):
//!
//! * `MORLOC_DEBUG_DIR` -- root for `inputs/<hash>.pkt`. Falls back
//!   to `$MORLOC_RUN_DIR/debug`. Unset = no disk writes (frames are
//!   still recorded in memory by hash).
//!
//! * `MORLOC_DEBUG_CACHE_DEPTH` (default 1) -- maximum number of
//!   `.pkt` files written to disk per dispatch, in the order catch
//!   blocks fire (innermost first). Counter only advances on a
//!   successful write. `0` disables the limit.
//!
//! * `MORLOC_DEBUG_CACHE_MAX` (default 0 = unlimited) -- per-arg
//!   serialized-byte cap. Args whose msgpack form exceeds this are
//!   recorded with hash only (no disk write).
//!
//! * `MORLOC_DEBUG_RECURSION_CAP` (default 3, 0 = unlimited) --
//!   per-midx call cap. Once the same midx's catch has fired this
//!   many times in one dispatch, further entries are dropped
//!   entirely (no frame entry, no disk write). Guard against
//!   pathological deep recursion of one function.
//!
//! ## Lifetime
//!
//! All thread-local state -- frames, dump counter, per-midx counters
//! -- is per-dispatch. The pool's outer dispatcher calls
//! [`morloc_debug_flush_dispatch`] at the start of each new top-level
//! call (same site that calls `shm_tracker_flush`), so state never
//! leaks between unrelated invocations of the same pool process.
//!
//! ## Preserved-manifold limitation
//!
//! Inside a `Preserved` manifold the catch block references native
//! bindings (`n0`, `n1`, ...). If `morloc.get_value` fails before
//! the binding is created, the catch dumps a not-yet-bound name
//! and the inner defensive try/except swallows the resulting
//! NameError -- so the frame is recorded as "(serialize failed)"
//! and the original exception still propagates.

use crate::cschema::CSchema;
use libc::{c_char, c_void};
use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::path::PathBuf;
use std::ptr;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::OnceLock;

/// Why the catch couldn't (or didn't) write an arg to disk. Used to
/// pick the right one-line message in the rendered trace -- collapsing
/// these into a single "depth cap reached or write unavailable" string
/// hides the actual root cause.
enum DumpStatus {
    Written(PathBuf),
    /// `bytes.len() > MORLOC_DEBUG_CACHE_MAX`.
    TooLarge,
    /// Disk-write counter already at `MORLOC_DEBUG_CACHE_DEPTH`.
    DepthCapped,
    /// `try_write` returned None -- mkdir/atomic-write IO error.
    WriteFailed,
    /// `serialize_and_hash` returned None (no hash either).
    SerializeFailed,
}

struct ArgDump {
    schema: String,
    /// Zero only when status == SerializeFailed.
    hash: u64,
    status: DumpStatus,
}

struct FrameEntry {
    midx: u32,
    args: Vec<ArgDump>,
}

thread_local! {
    /// Frames appended in the order catch blocks fire (innermost first).
    static FRAMES: RefCell<Vec<FrameEntry>> = const { RefCell::new(Vec::new()) };
    /// Disk-write counter -- only successful writes advance.
    static DUMPED_COUNT: RefCell<u64> = const { RefCell::new(0) };
    /// Per-midx fire count for the recursion cap.
    static MIDX_COUNTERS: RefCell<HashMap<u32, u32>> =
        RefCell::new(HashMap::new());
}

// ── Limit resolution (process-wide, cached) ─────────────────────────────

static CACHE_DEPTH: OnceLock<u64> = OnceLock::new();
static CACHE_MAX: OnceLock<u64> = OnceLock::new();
static RECURSION_CAP: OnceLock<u32> = OnceLock::new();
static OVERFLOW_REPORTED: AtomicU64 = AtomicU64::new(0);

fn cache_depth() -> u64 {
    *CACHE_DEPTH.get_or_init(|| {
        std::env::var("MORLOC_DEBUG_CACHE_DEPTH")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or(1)
    })
}

fn cache_max() -> u64 {
    *CACHE_MAX.get_or_init(|| {
        std::env::var("MORLOC_DEBUG_CACHE_MAX")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or(0)
    })
}

fn recursion_cap() -> u32 {
    *RECURSION_CAP.get_or_init(|| {
        std::env::var("MORLOC_DEBUG_RECURSION_CAP")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or(3)
    })
}

fn resolve_debug_dir() -> Option<PathBuf> {
    if let Ok(d) = std::env::var("MORLOC_DEBUG_DIR") {
        if !d.is_empty() {
            return Some(PathBuf::from(d));
        }
    }
    if let Ok(d) = std::env::var("MORLOC_RUN_DIR") {
        if !d.is_empty() {
            return Some(PathBuf::from(d).join("debug"));
        }
    }
    // Fallback: a hidden dir in the invocation CWD. The wraps only fire
    // when the binary was compiled with --debug, so non-debug builds
    // never reach this branch and don't litter the CWD. If `current_dir`
    // fails (chrooted / removed CWD) we give up here and let the catch
    // record a hash-only frame.
    std::env::current_dir().ok().map(|cwd| cwd.join(".morloc-debug"))
}

fn ensure_inputs_dir() -> Option<PathBuf> {
    let base = resolve_debug_dir()?;
    let inputs = base.join("inputs");
    std::fs::create_dir_all(&inputs).ok()?;
    Some(inputs)
}

/// Serialize one packet as a self-contained voidstar data packet
/// and hash its content structurally. Returns `(content_hash, bytes)`
/// where the bytes are ready to write to disk as `<hash>.pkt` and
/// `morloc dump` can load directly. Never throws.
unsafe fn serialize_and_hash(
    packet: *const u8,
    schema_cstr: *const c_char,
) -> Option<(u64, Vec<u8>)> {
    extern "C" {
        fn parse_schema(schema_str: *const c_char, errmsg: *mut *mut c_char) -> *mut CSchema;
        fn free_schema(schema: *mut CSchema);
        fn get_morloc_data_packet_value(
            data: *const u8,
            schema: *const CSchema,
            errmsg: *mut *mut c_char,
        ) -> *mut u8;
        fn hash_voidstar(
            data: *const c_void,
            schema: *const CSchema,
            seed: u64,
            errmsg: *mut *mut c_char,
        ) -> u64;
    }

    if packet.is_null() || schema_cstr.is_null() {
        return None;
    }
    let mut err: *mut c_char = ptr::null_mut();
    let schema = parse_schema(schema_cstr, &mut err);
    if schema.is_null() {
        if !err.is_null() { libc::free(err as *mut c_void); }
        return None;
    }
    let voidstar = get_morloc_data_packet_value(packet, schema, &mut err);
    if voidstar.is_null() {
        if !err.is_null() { libc::free(err as *mut c_void); }
        free_schema(schema);
        return None;
    }
    let mut hash_err: *mut c_char = ptr::null_mut();
    let content_hash = hash_voidstar(
        voidstar as *const c_void, schema, 0, &mut hash_err,
    );
    if !hash_err.is_null() {
        libc::free(hash_err as *mut c_void);
        free_schema(schema);
        return None;
    }
    let mut pkt_err: *mut c_char = ptr::null_mut();
    let bytes = crate::cache::build_persistence_data_packet(
        voidstar as *const u8, schema, &mut pkt_err,
    );
    free_schema(schema);
    if !pkt_err.is_null() { libc::free(pkt_err as *mut c_void); }
    let bytes = bytes?;
    Some((content_hash, bytes))
}

/// Try to write `bytes` to `<debug_dir>/inputs/<hash>.pkt`. Returns
/// the path on success (or when the file already exists due to
/// content-addressed dedup). Returns `None` when the debug dir is
/// unavailable or the write failed.
fn try_write(hash_val: u64, bytes: &[u8]) -> Option<PathBuf> {
    let dir = ensure_inputs_dir()?;
    let path = dir.join(format!("{:016x}.pkt", hash_val));
    if path.exists() {
        return Some(path);
    }
    crate::utility::write_atomic_path(&path, bytes).ok()?;
    Some(path)
}

/// Record one frame's args after an exception. Called from the
/// per-language catch block. Never throws -- failures degrade
/// gracefully to "(serialize failed)" entries.
///
/// Safety: `packets` and `schemas` must each be an array of `n`
/// pointers. Each `packets[i]` is a NUL-or-packet pointer; each
/// `schemas[i]` is a NUL or NUL-terminated schema string.
#[no_mangle]
pub unsafe extern "C" fn morloc_debug_record_frame(
    midx: u32,
    packets: *const *const u8,
    schemas: *const *const c_char,
    n: usize,
) {
    // Recursion cap: drop everything for this fire when exceeded.
    // Cap of 0 disables the limit.
    let cap = recursion_cap();
    let count = MIDX_COUNTERS.with(|m| {
        let mut map = m.borrow_mut();
        let c = map.entry(midx).or_insert(0);
        *c += 1;
        *c
    });
    if cap != 0 && count > cap {
        // Report once per midx that overflowed so the trace can warn
        // the user. We don't push a frame entry past the cap.
        OVERFLOW_REPORTED.fetch_or(1u64 << (midx as u64 & 63), Ordering::Relaxed);
        return;
    }

    let depth_limit = cache_depth();
    let size_limit = cache_max();
    let mut args: Vec<ArgDump> = Vec::with_capacity(n);
    for i in 0..n {
        let packet = *packets.add(i);
        let schema_cstr = *schemas.add(i);
        let schema = if schema_cstr.is_null() {
            String::new()
        } else {
            CStr::from_ptr(schema_cstr).to_string_lossy().into_owned()
        };
        match serialize_and_hash(packet, schema_cstr) {
            None => args.push(ArgDump {
                schema,
                hash: 0,
                status: DumpStatus::SerializeFailed,
            }),
            Some((hash_val, bytes)) => {
                // Resolve disposition: too-large beats depth-cap beats
                // attempted-write. Only successful writes advance the
                // depth counter.
                let status = if size_limit != 0 && (bytes.len() as u64) > size_limit {
                    DumpStatus::TooLarge
                } else if depth_limit != 0
                    && DUMPED_COUNT.with(|d| *d.borrow()) >= depth_limit
                {
                    DumpStatus::DepthCapped
                } else {
                    match try_write(hash_val, &bytes) {
                        Some(p) => {
                            DUMPED_COUNT.with(|d| *d.borrow_mut() += 1);
                            DumpStatus::Written(p)
                        }
                        None => DumpStatus::WriteFailed,
                    }
                };
                args.push(ArgDump { schema, hash: hash_val, status });
            }
        }
    }
    FRAMES.with(|f| f.borrow_mut().push(FrameEntry { midx, args }));
}

/// Reset per-dispatch state. Called by the pool's outer dispatcher
/// at the start of each top-level call (same site as
/// `shm_tracker_flush`). Without this, recursion counters and the
/// disk-write counter would carry across unrelated invocations of
/// the same pool process.
#[no_mangle]
pub extern "C" fn morloc_debug_flush_dispatch() {
    FRAMES.with(|f| f.borrow_mut().clear());
    DUMPED_COUNT.with(|d| *d.borrow_mut() = 0);
    MIDX_COUNTERS.with(|m| m.borrow_mut().clear());
    OVERFLOW_REPORTED.store(0, Ordering::Relaxed);
}

/// Render the accumulated trace as a multi-line C string and clear
/// the per-thread state. Caller takes ownership and must free with
/// `libc::free`. Returns NULL when no frames were recorded.
#[no_mangle]
pub unsafe extern "C" fn morloc_debug_drain_frames() -> *mut c_char {
    let frames: Vec<FrameEntry> = FRAMES.with(|f| std::mem::take(&mut *f.borrow_mut()));
    let overflow = OVERFLOW_REPORTED.swap(0, Ordering::Relaxed);
    if frames.is_empty() && overflow == 0 {
        return ptr::null_mut();
    }
    let mut out = String::with_capacity(256);
    out.push_str("morloc trace (innermost first):\n");
    for (i, frame) in frames.iter().enumerate() {
        out.push_str(&format!("  frame {} mid={}\n", i, frame.midx));
        for (j, arg) in frame.args.iter().enumerate() {
            let schema = if arg.schema.is_empty() {
                "?"
            } else {
                arg.schema.as_str()
            };
            match &arg.status {
                DumpStatus::Written(p) => out.push_str(&format!(
                    "    arg[{}] :: {} -> {}\n",
                    j, schema, p.display()
                )),
                DumpStatus::TooLarge => out.push_str(&format!(
                    "    arg[{}] :: {} (hash={:016x}, size exceeded MORLOC_DEBUG_CACHE_MAX)\n",
                    j, schema, arg.hash
                )),
                DumpStatus::DepthCapped => out.push_str(&format!(
                    "    arg[{}] :: {} (hash={:016x}, depth cap reached -- raise --debug-cache-depth to dump more)\n",
                    j, schema, arg.hash
                )),
                DumpStatus::WriteFailed => out.push_str(&format!(
                    "    arg[{}] :: {} (hash={:016x}, write failed -- check that the debug dir is writable)\n",
                    j, schema, arg.hash
                )),
                DumpStatus::SerializeFailed => out.push_str(&format!(
                    "    arg[{}] :: {} (serialize failed)\n", j, schema
                )),
            }
        }
    }
    if overflow != 0 {
        out.push_str(
            "  (one or more manifolds exceeded MORLOC_DEBUG_RECURSION_CAP; \
            entries beyond the cap were dropped)\n",
        );
    }
    CString::new(out)
        .map(|c| c.into_raw())
        .unwrap_or(ptr::null_mut())
}

#[cfg(test)]
fn push_test_frame(midx: u32, args: Vec<ArgDump>) {
    FRAMES.with(|f| f.borrow_mut().push(FrameEntry { midx, args }));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn drain_returns_null_when_empty() {
        FRAMES.with(|f| f.borrow_mut().clear());
        OVERFLOW_REPORTED.store(0, Ordering::Relaxed);
        unsafe {
            assert!(morloc_debug_drain_frames().is_null());
        }
    }

    #[test]
    fn drain_renders_and_clears() {
        FRAMES.with(|f| f.borrow_mut().clear());
        push_test_frame(
            42,
            vec![ArgDump {
                schema: "f8".into(),
                hash: 0xdeadbeefcafebabe,
                status: DumpStatus::WriteFailed,
            }],
        );
        unsafe {
            let p = morloc_debug_drain_frames();
            assert!(!p.is_null());
            let s = CStr::from_ptr(p).to_string_lossy().into_owned();
            libc::free(p as *mut c_void);
            assert!(s.contains("mid=42"));
            assert!(s.contains("hash=deadbeefcafebabe"));
        }
        FRAMES.with(|f| assert!(f.borrow().is_empty()));
    }

    #[test]
    fn flush_resets_state() {
        FRAMES.with(|f| f.borrow_mut().clear());
        DUMPED_COUNT.with(|d| *d.borrow_mut() = 5);
        MIDX_COUNTERS.with(|m| m.borrow_mut().insert(7, 99));
        morloc_debug_flush_dispatch();
        DUMPED_COUNT.with(|d| assert_eq!(*d.borrow(), 0));
        MIDX_COUNTERS.with(|m| assert!(m.borrow().is_empty()));
    }
}
