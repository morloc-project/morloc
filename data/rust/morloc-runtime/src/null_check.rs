//! NUL-in-Str guard for cross-pool dispatch.
//!
//! This module re-exports the stateless half (`env_skip_null_check` and
//! `first_null_in_json_text`) from `morloc-runtime-types::null_check` and adds
//! the value-walking guard the pools call here because it calls
//! `shm::rel2abs`, which reads the process-global `VOLUMES` of this
//! crate's `shm` module.
//!
//! The dispatch path uses these checks to reject NUL-bearing Strs at
//! the boundary into languages whose lang.yaml sets
//! `allow_string_null = false` (currently R and C), rather than letting
//! the NUL propagate into user-language code where it would crash
//! inside something like base R's `nchar` with a confusing diagnostic.
//!
//! The check is opt-out at runtime via the `MORLOC_SKIP_NULL_CHECK=1`
//! environment variable or per-program via the manifest's
//! `unsafe_skip_null_check` flag (set by `morloc make
//! --unsafe-skip-null-check`).

pub use morloc_runtime_types::null_check::*;

use crate::shm::{self, AbsPtr, Array};
use crate::cschema::CSchema;
use morloc_runtime_types::shm_types::relptr_offset;
use std::os::raw::{c_char, c_void};

/// Resolve a relative pointer the way C's `resolve_relptr` does: against
/// `base` when it is non-null, through the volume table otherwise.
unsafe fn resolve(data: crate::shm::RelPtr, base: *const c_void) -> Option<AbsPtr> {
    if base.is_null() {
        shm::rel2abs(data).ok()
    } else {
        Some((base as *mut u8).add(relptr_offset(data)))
    }
}

/// C entry point for the cross-pool NUL guard.
///
/// Codegen decides whether a given deserialization needs this: it knows the
/// receiving language and whether the value's type carries a `Str` at all, so
/// the call is only emitted where it can fire. The runtime opt-out
/// (`MORLOC_SKIP_NULL_CHECK`) is honoured here rather than at the call site so
/// every binder gets it for free.
///
/// The schema arrives as the C-ABI `CSchema`, which is what a binder holds
/// after `parse_schema`. It is walked in place: converting to the Rust
/// `Schema` would allocate a tree per call on a path this guard exists to keep
/// cheap, and the native `Schema` is not layout-compatible with the C one, so
/// reading it as one would be undefined.
///
/// Returns a heap-allocated description of the offending slot, which the
/// caller must `free`, or NULL when the value carries no interior NUL.
#[no_mangle]
pub unsafe extern "C" fn morloc_first_null_in_value(
    voidstar: *const c_void,
    schema: *const CSchema,
    base: *const c_void,
) -> *mut c_char {
    if voidstar.is_null() || schema.is_null() || env_skip_null_check() {
        return std::ptr::null_mut();
    }
    let mut path = String::new();
    match walk_c(voidstar as AbsPtr, schema, base, &mut path) {
        None => std::ptr::null_mut(),
        Some(p) => match std::ffi::CString::new(p) {
            Ok(c) => libc::strdup(c.as_ptr()),
            Err(_) => std::ptr::null_mut(),
        },
    }
}

// SerialType discriminants as they cross the C ABI. Mirrors
// morloc_runtime_types::schema::SerialType; only the variants this walk cares
// about are named.
const CT_STRING: u32 = 13;
const CT_ARRAY: u32 = 14;
const CT_TUPLE: u32 = 15;
const CT_MAP: u32 = 16;
const CT_OPTIONAL: u32 = 17;
const CT_VARIANT: u32 = 26;
const CT_RECUR: u32 = 20;

/// Where a node's name sits in the reported path.
#[derive(Clone, Copy)]
enum Seg {
    Root,
    Index(usize),
    Field(usize),
    Key(*const c_char),
    Some,
    Ctor(*const c_char),
}

/// One node still to scan: its schema and value, the next child to visit,
/// the path length before this node's own segment, and that segment.
#[derive(Clone, Copy)]
struct Todo {
    schema: *const CSchema,
    data: AbsPtr,
    idx: usize,
    path_len: usize,
    seg: Seg,
}

/// Every back-reference in a C schema tree with its declaration: the
/// nearest enclosing node of the same name.
unsafe fn index_recur(s: *const CSchema, decls: &mut Vec<*const CSchema>, out: &mut Vec<(*const CSchema, *const CSchema)>) {
    if s.is_null() {
        return;
    }
    let node = &*s;
    if node.serial_type == CT_RECUR {
        let name = if node.name.is_null() { None } else { Some(std::ffi::CStr::from_ptr(node.name)) };
        let target = decls
            .iter()
            .rev()
            .find(|d| !(***d).name.is_null() && Some(std::ffi::CStr::from_ptr((***d).name)) == name)
            .copied()
            .unwrap_or(std::ptr::null());
        out.push((s, target));
        return;
    }
    let declares = !node.name.is_null();
    if declares {
        decls.push(s);
    }
    if !node.parameters.is_null() {
        for i in 0..node.size {
            index_recur(*node.parameters.add(i), decls, out);
        }
    }
    if declares {
        decls.pop();
    }
}

/// Walk a C-ABI schema-typed value looking for the first String slot with an
/// interior NUL. The pending nodes live on a heap stack, so a recursive
/// value of any depth is scanned in bounded stack space; a container leaves
/// a continuation for itself beneath the child it steps into.
unsafe fn walk_c(
    ptr: AbsPtr,
    schema: *const CSchema,
    base: *const c_void,
    path: &mut String,
) -> Option<String> {
    let mut recur: Vec<(*const CSchema, *const CSchema)> = Vec::new();
    let mut decls = Vec::new();
    index_recur(schema, &mut decls, &mut recur);
    let mut stack: Vec<Todo> = vec![Todo { schema, data: ptr, idx: 0, path_len: 0, seg: Seg::Root }];
    while let Some(mut t) = stack.pop() {
        path.truncate(t.path_len);
        match t.seg {
            Seg::Root => {}
            Seg::Index(i) => {
                path.push('[');
                path.push_str(&i.to_string());
                path.push(']');
            }
            Seg::Field(i) => {
                path.push('.');
                path.push_str(&i.to_string());
            }
            Seg::Key(k) => {
                path.push('.');
                path.push_str(&std::ffi::CStr::from_ptr(k).to_string_lossy());
            }
            Seg::Some => path.push_str("(some)"),
            Seg::Ctor(k) => {
                path.push('.');
                path.push_str(&std::ffi::CStr::from_ptr(k).to_string_lossy());
            }
        }
        // A back-reference is scanned as its declaration.
        let mut s = &*t.schema;
        if s.serial_type == CT_RECUR {
            let target = recur.iter().find(|(r, _)| *r == t.schema).map_or(std::ptr::null(), |(_, d)| *d);
            if target.is_null() {
                return None;
            }
            t.schema = target;
            s = &*target;
        }
        let here = path.len();
        match s.serial_type {
            CT_STRING => {
                if let Some(r) = check_string(t.data, base, path) {
                    return Some(r);
                }
            }
            CT_ARRAY => {
                let arr = &*(t.data as *const Array);
                if arr.size == 0 || s.size == 0 || s.parameters.is_null() {
                    continue;
                }
                let elem = *s.parameters;
                if elem.is_null() {
                    continue;
                }
                let elem_width = (*elem).width;
                let Some(abs) = resolve(arr.data, base) else { continue };
                if t.idx < arr.size {
                    let i = t.idx;
                    stack.push(Todo { idx: i + 1, ..t });
                    stack.push(Todo { schema: elem, data: abs.add(i * elem_width), idx: 0, path_len: here, seg: Seg::Index(i) });
                }
            }
            CT_TUPLE | CT_MAP => {
                if s.parameters.is_null() || s.offsets.is_null() {
                    continue;
                }
                if t.idx < s.size {
                    let i = t.idx;
                    stack.push(Todo { idx: i + 1, ..t });
                    let p = *s.parameters.add(i);
                    if p.is_null() {
                        continue;
                    }
                    let off = *s.offsets.add(i);
                    let seg = if s.serial_type == CT_MAP && !s.keys.is_null() && !(*s.keys.add(i)).is_null() {
                        Seg::Key(*s.keys.add(i))
                    } else {
                        Seg::Field(i)
                    };
                    stack.push(Todo { schema: p, data: t.data.add(off), idx: 0, path_len: here, seg });
                }
            }
            CT_OPTIONAL => {
                // The slot is one relative pointer; absent is RELNULL.
                let rel = *(t.data as *const shm::RelPtr);
                if rel == shm::RELNULL || s.parameters.is_null() {
                    continue;
                }
                let inner = *s.parameters;
                if inner.is_null() {
                    continue;
                }
                let Some(abs) = resolve(rel, base) else { continue };
                stack.push(Todo { schema: inner, data: abs, idx: 0, path_len: here, seg: Seg::Some });
            }
            CT_VARIANT => {
                // A tag byte, then a relative pointer to the arm's fields.
                let tag = *(t.data as *const u8) as usize;
                if tag >= s.size || s.parameters.is_null() {
                    continue;
                }
                let arm = *s.parameters.add(tag);
                if arm.is_null() {
                    continue;
                }
                let rel = *(t.data.add(8) as *const shm::RelPtr);
                if rel == shm::RELNULL {
                    continue;
                }
                let Some(abs) = resolve(rel, base) else { continue };
                let seg = if !s.keys.is_null() && !(*s.keys.add(tag)).is_null() {
                    Seg::Ctor(*s.keys.add(tag))
                } else {
                    Seg::Field(tag)
                };
                stack.push(Todo { schema: arm, data: abs, idx: 0, path_len: here, seg });
            }
            // Stream handles carry a path or a slot id, neither of which can
            // hold an interior NUL. Tables are Arrow buffers, not walked here.
            // Everything else is numeric, boolean, or nil: no string bytes.
            _ => {}
        }
    }
    None
}

unsafe fn check_string(ptr: AbsPtr, base: *const c_void, path: &mut String) -> Option<String> {
    let arr = &*(ptr as *const Array);
    if arr.size == 0 {
        return None;
    }
    let abs = resolve(arr.data, base)?;
    // memchr returns non-null on hit. libc is already a runtime dep.
    let hit = libc::memchr(abs as *const libc::c_void, 0, arr.size);
    if hit.is_null() {
        None
    } else {
        let offset = (hit as usize) - (abs as usize);
        Some(format!("{} (byte {} of {})", abbreviate(path), offset, arr.size))
    }
}

/// A path with its middle elided when it is too long to read: a NUL at the
/// bottom of a deep chain would otherwise be reported through a path as
/// long as the chain, whose end, the part that names the slot, is the part
/// a bounded message buffer cuts off.
fn abbreviate(path: &str) -> String {
    const KEEP: usize = 96;
    if path.len() <= 3 * KEEP {
        return path.to_string();
    }
    let starts_segment = |c: char| c == '.' || c == '[';
    let floor = |mut i: usize| {
        while !path.is_char_boundary(i) {
            i -= 1;
        }
        i
    };
    let head_end = path[..floor(KEEP)].rfind(starts_segment).unwrap_or(floor(KEEP));
    let tail_from = floor(path.len() - KEEP);
    let tail_start = tail_from + path[tail_from..].find(starts_segment).unwrap_or(0);
    let elided = path[head_end..tail_start].chars().filter(|&c| starts_segment(c)).count();
    format!("{}[..{} segments..]{}", &path[..head_end], elided, &path[tail_start..])
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::schema::{Schema, SerialType};
    use crate::shm;

    // The schema walker requires SHM to be initialised so that rel2abs
    // succeeds. Tests use the crate-wide helper and hold its guard.
    #[must_use]
    fn setup() -> std::sync::RwLockReadGuard<'static, ()> {
        crate::init_test_shm()
    }

    unsafe fn write_str_to_shm(bytes: &[u8]) -> shm::RelPtr {
        let abs = shm::shmemcpy(bytes.as_ptr(), bytes.len()).expect("shmemcpy");
        shm::abs2rel(abs).expect("abs2rel")
    }

    unsafe fn make_string_slot(bytes: &[u8]) -> Box<Array> {
        let data = if bytes.is_empty() {
            shm::RELNULL
        } else {
            write_str_to_shm(bytes)
        };
        Box::new(Array { size: bytes.len(), data })
    }

    /// The guard as the pools call it: over the C schema, shared memory.
    unsafe fn first_null(ptr: AbsPtr, s: &Schema) -> Option<String> {
        let cs = CSchema::from_rust(s);
        let r = morloc_first_null_in_value(ptr as *const c_void, cs, std::ptr::null());
        CSchema::free(cs);
        if r.is_null() {
            None
        } else {
            let out = std::ffi::CStr::from_ptr(r).to_string_lossy().into_owned();
            libc::free(r as *mut c_void);
            Some(out)
        }
    }

    #[test]
    fn plain_string_no_nul() {
        let _shm = setup();
        let s = Schema::primitive(SerialType::String);
        unsafe {
            let arr = make_string_slot(b"hello");
            let r = first_null(&*arr as *const Array as AbsPtr, &s);
            assert!(r.is_none(), "expected no NUL hit, got {:?}", r);
        }
    }

    #[test]
    fn plain_string_with_nul() {
        let _shm = setup();
        let s = Schema::primitive(SerialType::String);
        unsafe {
            let arr = make_string_slot(b"abc\0def");
            let r = first_null(&*arr as *const Array as AbsPtr, &s);
            assert!(r.is_some(), "expected NUL hit");
            let path = r.unwrap();
            assert!(path.contains("byte 3"), "path={}", path);
        }
    }

    #[test]
    fn empty_string_no_nul() {
        let _shm = setup();
        let s = Schema::primitive(SerialType::String);
        unsafe {
            let arr = make_string_slot(b"");
            let r = first_null(&*arr as *const Array as AbsPtr, &s);
            assert!(r.is_none());
        }
    }

    /// A NUL reachable only through an optional, a variant payload, or a
    /// recursive record is found, and reported at its path.
    #[test]
    fn nul_behind_optional_variant_and_back_reference() {
        let _shm = setup();
        let nul = format!("[\"a\", \"b{}c\"]", "\\u0000");
        for (schema, json, want) in [
            ("?s", "\"x\\u0000\"", "(some)"),
            ("m11a?s", "{\"a\":\"x\\u0000\"}", ".a(some)"),
            ("v23Nil04Cons2i4s", "{\"Cons\":[1,\"x\\u0000\"]}", ".Cons.1"),
            ("&2LLm24heads4tail?^2LL", "{\"head\":\"ok\",\"tail\":{\"head\":\"o\\u0000k\",\"tail\":null}}", ".tail(some).head"),
            ("a?s", &nul, "[1](some)"),
        ] {
            let s = crate::schema::parse_schema(schema).unwrap();
            let ptr = crate::json::read_json_with_schema(json, &s).unwrap();
            let r = unsafe { first_null(ptr, &s) };
            let path = r.unwrap_or_else(|| panic!("{schema}: no NUL found"));
            assert!(path.starts_with(want), "{schema}: path={path}, want prefix {want}");
        }
        // And a clean value of each shape is clean.
        let s = crate::schema::parse_schema("&2LLm24heads4tail?^2LL").unwrap();
        let ptr = crate::json::read_json_with_schema("{\"head\":\"ok\",\"tail\":{\"head\":\"ok\",\"tail\":null}}", &s).unwrap();
        assert!(unsafe { first_null(ptr, &s) }.is_none());
    }

    /// The guard walks a chain far deeper than one frame per level could.
    #[test]
    fn nul_at_the_bottom_of_a_deep_chain() {
        crate::deep_tests::on_small_stack(|| {
            let _shm = setup();
            let s = crate::schema::parse_schema("&2LLm24heads4tail?^2LL").unwrap();
            let depth = crate::deep_tests::DEPTH;
            let mut text = String::new();
            for _ in 0..depth {
                text.push_str("{\"head\":\"ok\",\"tail\":");
            }
            text.push_str("{\"head\":\"x\\u0000\",\"tail\":null}");
            text.push_str(&"}".repeat(depth));
            let ptr = crate::json::read_json_with_schema(&text, &s).unwrap();
            let path = unsafe { first_null(ptr, &s) }.expect("NUL at the bottom");
            // The middle of the path is elided, so the slot's name and the
            // NUL's position fit any message buffer.
            assert!(path.len() < 400, "{}", path.len());
            assert!(path.starts_with(".tail(some).tail(some)"), "{path}");
            assert!(path.contains("(some)[..99984 segments..].tail(some)"), "{path}");
            assert!(path.ends_with(".tail(some).head (byte 1 of 2)"), "{path}");
        });
    }

    #[test]
    fn env_skip_default_off() {
        // Don't set anything; expect false.
        std::env::remove_var("MORLOC_SKIP_NULL_CHECK");
        assert!(!env_skip_null_check());
    }

    #[test]
    fn json_scan_clean_string() {
        assert_eq!(first_null_in_json_text(r#""hello""#).unwrap(), None);
    }

    // Helper: build the 6-byte JSON escape sequence for U+0000 without
    // putting a literal NUL byte in this source file. We assemble the
    // escape at runtime so the file stays plain ASCII.
    fn nul_escape() -> &'static str {
        "\\u0000"
    }

    #[test]
    fn json_scan_string_with_unicode_nul() {
        let json = format!("\"abc{}def\"", nul_escape());
        let r = first_null_in_json_text(&json).unwrap();
        assert!(r.is_some());
        let path = r.unwrap();
        assert!(path.contains("byte 3"), "path={}", path);
    }

    #[test]
    fn json_scan_nested_struct() {
        let json = format!(
            "{{\"a\":\"ok\",\"b\":[{{\"c\":\"ab{}c\"}}]}}",
            nul_escape()
        );
        let r = first_null_in_json_text(&json).unwrap();
        assert!(r.is_some());
        let path = r.unwrap();
        // Path should walk through .b[0].c
        assert!(path.contains(".b"), "path={}", path);
        assert!(path.contains("[0]"), "path={}", path);
        assert!(path.contains(".c"), "path={}", path);
    }

    #[test]
    fn json_scan_array_of_strings() {
        let json = format!(
            "[\"safe\",\"also safe\",\"x{}y\"]",
            nul_escape()
        );
        let r = first_null_in_json_text(&json).unwrap();
        assert!(r.is_some());
        assert!(r.unwrap().contains("[2]"));
    }

    #[test]
    fn json_scan_skips_non_string_leaves() {
        assert_eq!(first_null_in_json_text(r#"{"n":42,"f":3.14,"b":true}"#).unwrap(), None);
    }
}
