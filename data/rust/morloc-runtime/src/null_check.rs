//! NUL-in-Str guard for cross-pool dispatch.
//!
//! This module re-exports the stateless half (`env_skip_null_check` and
//! `first_null_in_json`) from `morloc-runtime-types::null_check` and adds
//! the SHM-walking variant (`first_null_in_strings`) here because it
//! calls `shm::rel2abs`, which reads the process-global `VOLUMES` of
//! this crate's `shm` module.
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

use crate::schema::{Schema, SerialType};
use crate::shm::{self, AbsPtr, Array};

/// Walk a schema-typed value and return the access path to the first
/// String slot containing an interior NUL byte, or None if every
/// String in the value is NUL-free.
///
/// The path uses morloc-style accessors: `args[i]`, `.field`, `[i]` for
/// list indexing, `(some)` for the inhabited Optional slot. The
/// dispatch site prefixes a top-level `args[i]` segment based on which
/// arg it is scanning.
///
/// Safety: `ptr` must point to a value laid out according to `schema`
/// in shared memory accessible to this process; `schema` and `ptr`
/// must remain valid for the duration of the call.
pub unsafe fn first_null_in_strings(ptr: AbsPtr, schema: &Schema) -> Option<String> {
    let mut path = String::new();
    walk(ptr, schema, &mut path)
}

unsafe fn walk(ptr: AbsPtr, schema: &Schema, path: &mut String) -> Option<String> {
    match schema.serial_type {
        SerialType::String => check_string(ptr, path),
        SerialType::IFile | SerialType::OStream | SerialType::IStream => {
            // Stream-handle fields are tagged unions: TAG_PATH carries a
            // file path (no embedded NULs by POSIX rule), TAG_HANDLE
            // carries a bare slot id (no string content). Neither can
            // surface an interior NUL to the cross-pool boundary, so
            // skip the walk entirely.
            None
        }
        SerialType::Array => {
            // schema.parameters[0] is the element schema; the slot at
            // `ptr` is an Array { size, data: relptr }.
            let arr = &*(ptr as *const Array);
            if arr.size == 0 {
                return None;
            }
            let elem = schema.parameters.get(0)?;
            let elem_width = elem.width;
            let abs = shm::rel2abs(arr.data).ok()?;
            for i in 0..arr.size {
                let saved_len = path.len();
                path.push('[');
                path.push_str(&i.to_string());
                path.push(']');
                let r = walk(abs.add(i * elem_width), elem, path);
                if r.is_some() {
                    return r;
                }
                path.truncate(saved_len);
            }
            None
        }
        SerialType::Tuple => {
            for (i, p) in schema.parameters.iter().enumerate() {
                let off = *schema.offsets.get(i)?;
                let saved_len = path.len();
                path.push('.');
                path.push_str(&i.to_string());
                let r = walk(ptr.add(off), p, path);
                if r.is_some() {
                    return r;
                }
                path.truncate(saved_len);
            }
            None
        }
        SerialType::Map => {
            // Record-style: parallel arrays of keys and parameters.
            for (i, p) in schema.parameters.iter().enumerate() {
                let off = *schema.offsets.get(i)?;
                let saved_len = path.len();
                path.push('.');
                if let Some(k) = schema.keys.get(i) {
                    path.push_str(k);
                } else {
                    path.push_str(&i.to_string());
                }
                let r = walk(ptr.add(off), p, path);
                if r.is_some() {
                    return r;
                }
                path.truncate(saved_len);
            }
            None
        }
        SerialType::Optional => {
            // Tag byte at offset 0; inner value at offsets[0] when tag != 0.
            let tag = *(ptr as *const u8);
            if tag == 0 {
                return None;
            }
            let inner = schema.parameters.get(0)?;
            let off = *schema.offsets.get(0)?;
            let saved_len = path.len();
            path.push_str("(some)");
            let r = walk(ptr.add(off), inner, path);
            if r.is_some() {
                return r;
            }
            path.truncate(saved_len);
            None
        }
        // Tables are Arrow buffers; we do not yet inspect their string columns
        // for interior NULs. Skipped for now; a future pass can plumb the
        // arrow_ffi machinery to do this if/when it becomes a concern.
        SerialType::Table => None,
        // Numeric / Bool / Nil / Int(BigInt) primitives carry no String bytes.
        _ => None,
    }
}

unsafe fn check_string(ptr: AbsPtr, path: &mut String) -> Option<String> {
    let arr = &*(ptr as *const Array);
    if arr.size == 0 {
        return None;
    }
    let abs = shm::rel2abs(arr.data).ok()?;
    // memchr returns non-null on hit. libc is already a runtime dep.
    let hit = libc::memchr(abs as *const libc::c_void, 0, arr.size);
    if hit.is_null() {
        None
    } else {
        let offset = (hit as usize) - (abs as usize);
        Some(format!("{} (byte {} of {})", path, offset, arr.size))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::shm;

    // The schema walker requires SHM to be initialised so that rel2abs
    // succeeds. Tests use the crate-wide one-shot helper.
    fn setup() {
        crate::init_test_shm();
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

    #[test]
    fn plain_string_no_nul() {
        setup();
        let s = Schema::primitive(SerialType::String);
        unsafe {
            let arr = make_string_slot(b"hello");
            let r = first_null_in_strings(&*arr as *const Array as AbsPtr, &s);
            assert!(r.is_none(), "expected no NUL hit, got {:?}", r);
        }
    }

    #[test]
    fn plain_string_with_nul() {
        setup();
        let s = Schema::primitive(SerialType::String);
        unsafe {
            let arr = make_string_slot(b"abc\0def");
            let r = first_null_in_strings(&*arr as *const Array as AbsPtr, &s);
            assert!(r.is_some(), "expected NUL hit");
            let path = r.unwrap();
            assert!(path.contains("byte 3"), "path={}", path);
        }
    }

    #[test]
    fn empty_string_no_nul() {
        setup();
        let s = Schema::primitive(SerialType::String);
        unsafe {
            let arr = make_string_slot(b"");
            let r = first_null_in_strings(&*arr as *const Array as AbsPtr, &s);
            assert!(r.is_none());
        }
    }

    #[test]
    fn env_skip_default_off() {
        // Don't set anything; expect false.
        std::env::remove_var("MORLOC_SKIP_NULL_CHECK");
        assert!(!env_skip_null_check());
    }

    #[test]
    fn json_scan_clean_string() {
        let v: serde_json::Value = serde_json::from_str(r#""hello""#).unwrap();
        assert!(first_null_in_json(&v).is_none());
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
        let v: serde_json::Value = serde_json::from_str(&json).unwrap();
        let r = first_null_in_json(&v);
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
        let v: serde_json::Value = serde_json::from_str(&json).unwrap();
        let r = first_null_in_json(&v);
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
        let v: serde_json::Value = serde_json::from_str(&json).unwrap();
        let r = first_null_in_json(&v);
        assert!(r.is_some());
        assert!(r.unwrap().contains("[2]"));
    }

    #[test]
    fn json_scan_skips_non_string_leaves() {
        let v: serde_json::Value = serde_json::from_str(r#"{"n":42,"f":3.14,"b":true}"#).unwrap();
        assert!(first_null_in_json(&v).is_none());
    }
}
