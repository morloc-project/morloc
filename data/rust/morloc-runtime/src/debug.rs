//! Debug-trace runtime.
//!
//! When the compiler emits a manifold with `morloc make --debug`, the
//! generated body is wrapped in a try/catch (per-language). On any
//! thrown exception the catch block calls
//! [`morloc_debug_record_frame`] with the manifold's midx and a list of
//! its args (each pre-serialized to a packet with a schema string).
//! This module:
//!
//! 1. Content-addresses each arg's value via xxh64.
//! 2. Writes `<debug_dir>/inputs/<hash>.pkt` as a data packet, unless
//!    that file already exists or the dispatch's write budget is spent;
//!    the packet is built only when it will be written.
//! 3. Appends a frame entry to the thread-local trace.
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
//!   blocks fire (innermost first). Counter only advances on a new
//!   write; an arg whose file already exists points at it for free.
//!   `0` disables the limit.
//!
//! * `MORLOC_DEBUG_CACHE_MAX` (default 0 = unlimited) -- per-arg
//!   serialized-byte cap. Args whose packet exceeds this are recorded
//!   with hash only (no disk write).
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

use libc::{c_char, c_void};
use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::path::PathBuf;
use std::ptr;
use morloc_runtime_types::publish_once::PublishOnce;

/// NULL or empty C string -> `String::new()`; otherwise the borrowed
/// UTF-8 (lossy). Used everywhere in this module to turn optional
/// codegen-supplied metadata into owned Strings for FrameEntry.
unsafe fn cstr_to_string(p: *const c_char) -> String {
    if p.is_null() {
        String::new()
    } else {
        CStr::from_ptr(p).to_string_lossy().into_owned()
    }
}

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
    /// No debug dir, or the atomic write failed.
    WriteFailed,
    /// The arg could not be decoded, hashed or packed.
    SerializeFailed,
}

struct ArgDump {
    schema: String,
    /// Zero when the arg could not be decoded.
    hash: u64,
    status: DumpStatus,
}

struct FrameEntry {
    midx: u32,
    /// Sanitized morloc-level name; empty for anonymous/synthesized nodes.
    name: String,
    /// @path:line:col@ of the manifold; empty when unknown.
    srcloc: String,
    /// "py" / "cpp" / "r"; identifies which pool the frame was recorded in.
    lang: String,
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
    /// Set when a manifold's catch fired past the recursion cap.
    static OVERFLOW_REPORTED: RefCell<bool> = const { RefCell::new(false) };
}

// ── Limit resolution (process-wide, cached) ─────────────────────────────

static CACHE_DEPTH: PublishOnce<u64> = PublishOnce::new();
static CACHE_MAX: PublishOnce<u64> = PublishOnce::new();
static RECURSION_CAP: PublishOnce<u32> = PublishOnce::new();

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

fn inputs_dir() -> Option<PathBuf> {
    resolve_debug_dir().map(|base| base.join("inputs"))
}

/// An arg's value, decoded from its packet, with its content hash. The
/// persistence packet is built from it only when it will be written.
struct DecodedArg {
    hash: u64,
    voidstar: *const u8,
    schema: *mut crate::cschema::CSchema,
}

impl Drop for DecodedArg {
    fn drop(&mut self) {
        unsafe { crate::ffi::free_schema(self.schema) };
    }
}

impl DecodedArg {
    /// The self-contained data packet `morloc dump` and `@load` read.
    unsafe fn persistence_bytes(&self) -> Option<Vec<u8>> {
        let mut err: *mut c_char = ptr::null_mut();
        let bytes = crate::cache::build_persistence_data_packet(self.voidstar, self.schema, &mut err);
        if !err.is_null() { libc::free(err as *mut c_void); }
        bytes
    }
}

/// Decode one packet and hash its content structurally. Never throws.
unsafe fn decode_and_hash(
    packet: *const u8,
    schema_cstr: *const c_char,
) -> Option<DecodedArg> {
    use crate::ffi::parse_schema;
    use crate::ffi::free_schema;
    use crate::packet_ffi::get_morloc_data_packet_value;
    use crate::cache::hash_voidstar;

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
    let hash = hash_voidstar(voidstar as *const c_void, schema, 0, &mut hash_err);
    if !hash_err.is_null() {
        libc::free(hash_err as *mut c_void);
        free_schema(schema);
        return None;
    }
    Some(DecodedArg { hash, voidstar: voidstar as *const u8, schema })
}

/// What becomes of an arg whose hash is known, before its packet is built.
#[derive(Debug, PartialEq, Eq)]
enum Disposition {
    /// Its file is already on disk: point at it, spending no budget.
    Existing,
    /// The dispatch's write budget is spent.
    Capped,
    /// Build the packet and write it.
    Write,
}

fn dump_disposition(file_exists: bool, dumped: u64, depth_limit: u64) -> Disposition {
    if file_exists {
        Disposition::Existing
    } else if depth_limit != 0 && dumped >= depth_limit {
        Disposition::Capped
    } else {
        Disposition::Write
    }
}

fn dump_file(inputs: &std::path::Path, hash_val: u64) -> PathBuf {
    inputs.join(format!("{:016x}.pkt", hash_val))
}

/// Record one frame's args after an exception. Called from the
/// per-language catch block. Never throws -- failures degrade
/// gracefully to "(serialize failed)" entries.
///
/// `name`, `srcloc`, and `lang` may be NULL or `""` when the
/// compiler had nothing to attach; all three are treated the same.
///
/// Safety: `packets` and `schemas` must each be an array of `n`
/// pointers. Each `packets[i]` is a NUL-or-packet pointer; each
/// `schemas[i]` is a NUL or NUL-terminated schema string.
pub(crate) unsafe fn morloc_debug_record_frame(
    midx: u32,
    name: *const c_char,
    srcloc: *const c_char,
    lang: *const c_char,
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
        // The trace warns that frames were dropped; none is pushed past the cap.
        OVERFLOW_REPORTED.with(|o| *o.borrow_mut() = true);
        return;
    }

    let depth_limit = cache_depth();
    let size_limit = cache_max();
    let inputs = inputs_dir();
    let mut args: Vec<ArgDump> = Vec::with_capacity(n);
    for i in 0..n {
        let packet = *packets.add(i);
        let schema_cstr = *schemas.add(i);
        let schema = cstr_to_string(schema_cstr);
        let Some(arg) = decode_and_hash(packet, schema_cstr) else {
            args.push(ArgDump { schema, hash: 0, status: DumpStatus::SerializeFailed });
            continue;
        };
        let path = inputs.as_deref().map(|d| dump_file(d, arg.hash));
        let exists = path.as_ref().map_or(false, |p| p.exists());
        let dumped = DUMPED_COUNT.with(|d| *d.borrow());
        let status = match (dump_disposition(exists, dumped, depth_limit), path) {
            (Disposition::Existing, Some(p)) => DumpStatus::Written(p),
            (Disposition::Capped, _) => DumpStatus::DepthCapped,
            (_, None) => DumpStatus::WriteFailed,
            (Disposition::Write, Some(p)) => match arg.persistence_bytes() {
                None => DumpStatus::SerializeFailed,
                Some(bytes) if size_limit != 0 && (bytes.len() as u64) > size_limit => DumpStatus::TooLarge,
                Some(bytes) => match inputs
                    .as_deref()
                    .map_or(Ok(()), std::fs::create_dir_all)
                    .and_then(|()| crate::utility::write_atomic_path(&p, &bytes))
                {
                    Ok(_) => {
                        DUMPED_COUNT.with(|d| *d.borrow_mut() += 1);
                        DumpStatus::Written(p)
                    }
                    Err(_) => DumpStatus::WriteFailed,
                },
            },
        };
        args.push(ArgDump { schema, hash: arg.hash, status });
    }
    FRAMES.with(|f| {
        f.borrow_mut().push(FrameEntry {
            midx,
            name: cstr_to_string(name),
            srcloc: cstr_to_string(srcloc),
            lang: cstr_to_string(lang),
            args,
        })
    });
}

/// Reset per-dispatch state. Called by the pool's outer dispatcher
/// at the start of each top-level call (same site as
/// `shm_tracker_flush`). Without this, recursion counters and the
/// disk-write counter would carry across unrelated invocations of
/// the same pool process.
pub(crate) fn morloc_debug_flush_dispatch() {
    FRAMES.with(|f| f.borrow_mut().clear());
    DUMPED_COUNT.with(|d| *d.borrow_mut() = 0);
    MIDX_COUNTERS.with(|m| m.borrow_mut().clear());
    OVERFLOW_REPORTED.with(|o| *o.borrow_mut() = false);
}


/// Render the accumulated trace as a multi-line C string and clear
/// the per-thread state. Caller takes ownership and must free with
/// `libc::free`. Returns NULL when no frames were recorded.
pub(crate) unsafe fn morloc_debug_drain_frames() -> *mut c_char {
    let frames: Vec<FrameEntry> = FRAMES.with(|f| std::mem::take(&mut *f.borrow_mut()));
    let overflow = OVERFLOW_REPORTED.with(|o| std::mem::take(&mut *o.borrow_mut()));
    if frames.is_empty() && !overflow {
        return ptr::null_mut();
    }
    let mut out = String::with_capacity(256);
    // The header names the pool this drain came from -- every frame in
    // one drain call was recorded in the same pool, so a single tag on
    // the header is sufficient; the per-frame [lang] would be redundant
    // and is elided below.
    let header_lang = frames.first().map(|f| f.lang.as_str()).unwrap_or("");
    if header_lang.is_empty() {
        out.push_str("morloc trace:\n");
    } else {
        out.push_str(&format!("morloc trace [{} pool]:\n", header_lang));
    }
    let mut capped = std::collections::HashSet::new();
    for frame in frames.iter() {
        // Anonymous/synthesized nodes (compositions, IntrMap, tuple
        // projections, lambdas) render '_' so the header shape is
        // uniform across every frame. Missing srcloc collapses
        // cleanly. Language is on the pool header, not per-frame.
        let name = if frame.name.is_empty() { "_" } else { frame.name.as_str() };
        let loc = if frame.srcloc.is_empty() {
            String::new()
        } else {
            format!(", {}", frame.srcloc)
        };
        out.push_str(&format!(
            "  at {} (mid={}{})\n",
            name, frame.midx, loc
        ));
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
                DumpStatus::DepthCapped => {
                    capped.insert(arg.hash);
                    out.push_str(&format!("    arg[{}] :: {}\n", j, schema))
                }
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
    if !capped.is_empty() {
        out.push_str(&format!(
            "  ({} {} not saved; raise --debug-cache-depth to save more)\n",
            capped.len(),
            if capped.len() == 1 { "value" } else { "values" }
        ));
    }
    if overflow {
        out.push_str(
            "  (one or more manifolds exceeded MORLOC_DEBUG_RECURSION_CAP; \
            entries beyond the cap were dropped)\n",
        );
    }
    CString::new(out)
        .map(|c| crate::error::malloc_cstr(&c))
        .unwrap_or(ptr::null_mut())
}

#[cfg(test)]
fn push_test_frame(midx: u32, args: Vec<ArgDump>) {
    FRAMES.with(|f| {
        f.borrow_mut().push(FrameEntry {
            midx,
            name: String::new(),
            srcloc: String::new(),
            lang: String::new(),
            args,
        })
    });
}

#[cfg(test)]
fn push_test_frame_named(
    midx: u32,
    name: &str,
    srcloc: &str,
    args: Vec<ArgDump>,
) {
    FRAMES.with(|f| {
        f.borrow_mut().push(FrameEntry {
            midx,
            name: name.to_string(),
            srcloc: srcloc.to_string(),
            lang: String::new(),
            args,
        })
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn drain_returns_null_when_empty() {
        FRAMES.with(|f| f.borrow_mut().clear());
        OVERFLOW_REPORTED.with(|o| *o.borrow_mut() = false);
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

    #[test]
    fn drain_renders_name_and_srcloc() {
        FRAMES.with(|f| f.borrow_mut().clear());
        push_test_frame_named(42, "myfunc", "main.loc:12:5", vec![]);
        push_test_frame_named(7, "", "", vec![]);
        push_test_frame_named(3, "namedonly", "", vec![]);
        push_test_frame_named(4, "", "other.loc:1:1", vec![]);
        unsafe {
            let p = morloc_debug_drain_frames();
            assert!(!p.is_null());
            let s = CStr::from_ptr(p).to_string_lossy().into_owned();
            libc::free(p as *mut c_void);
            // Anonymous frames render '_' in the name slot; missing
            // srcloc drops the trailing ", <loc>". Empty lang on all
            // frames means the header is the plain "morloc trace:".
            assert!(s.contains("morloc trace:\n"));
            assert!(s.contains("at myfunc (mid=42, main.loc:12:5)"));
            assert!(s.contains("at _ (mid=7)"));
            assert!(s.contains("at namedonly (mid=3)"));
            assert!(s.contains("at _ (mid=4, other.loc:1:1)"));
        }
    }

    #[test]
    fn capped_args_are_bare_and_counted_once() {
        FRAMES.with(|f| f.borrow_mut().clear());
        let capped = |hash| ArgDump { schema: "f8".into(), hash, status: DumpStatus::DepthCapped };
        push_test_frame(1, vec![capped(1)]);
        push_test_frame(2, vec![capped(1), capped(2)]);
        unsafe {
            let p = morloc_debug_drain_frames();
            let s = CStr::from_ptr(p).to_string_lossy().into_owned();
            libc::free(p as *mut c_void);
            assert!(s.contains("    arg[0] :: f8\n"));
            assert_eq!(s.matches("not saved").count(), 1);
            assert!(s.contains("(2 values not saved; raise --debug-cache-depth to save more)"));
        }
    }

    #[test]
    fn an_existing_dump_spends_no_budget() {
        assert_eq!(dump_disposition(true, 1, 1), Disposition::Existing);
        assert_eq!(dump_disposition(true, 0, 1), Disposition::Existing);
        assert_eq!(dump_disposition(false, 1, 1), Disposition::Capped);
        assert_eq!(dump_disposition(false, 0, 1), Disposition::Write);
        assert_eq!(dump_disposition(false, 9, 0), Disposition::Write);
    }

    #[test]
    fn an_overflow_note_stays_on_its_thread() {
        FRAMES.with(|f| f.borrow_mut().clear());
        OVERFLOW_REPORTED.with(|o| *o.borrow_mut() = false);
        std::thread::spawn(|| OVERFLOW_REPORTED.with(|o| *o.borrow_mut() = true))
            .join()
            .unwrap();
        unsafe {
            assert!(morloc_debug_drain_frames().is_null());
        }
    }

    #[test]
    fn drain_names_pool_from_first_frame_lang() {
        FRAMES.with(|f| f.borrow_mut().clear());
        FRAMES.with(|f| {
            f.borrow_mut().push(FrameEntry {
                midx: 1,
                name: "foo".into(),
                srcloc: "main.loc:1:1".into(),
                lang: "cpp".into(),
                args: vec![],
            })
        });
        unsafe {
            let p = morloc_debug_drain_frames();
            assert!(!p.is_null());
            let s = CStr::from_ptr(p).to_string_lossy().into_owned();
            libc::free(p as *mut c_void);
            assert!(s.contains("morloc trace [cpp pool]:\n"));
            // No per-frame [lang] tag anymore.
            assert!(!s.contains("[cpp]"));
        }
    }
}

mod c_abi {
    use super::*;

    #[no_mangle]
    pub unsafe extern "C" fn morloc_debug_record_frame(midx: u32, name: *const c_char, srcloc: *const c_char, lang: *const c_char, packets: *const *const u8, schemas: *const *const c_char, n: usize) {
        super::morloc_debug_record_frame(midx, name, srcloc, lang, packets, schemas, n)
    }

    #[no_mangle]
    pub extern "C" fn morloc_debug_flush_dispatch() {
        super::morloc_debug_flush_dispatch()
    }

    #[no_mangle]
    pub unsafe extern "C" fn morloc_debug_drain_frames() -> *mut c_char {
        super::morloc_debug_drain_frames()
    }
}
