//! FFI shims between the nexus stdio server and libmorloc.so.
//!
//! The stdio server runs inside the nexus binary but needs to touch
//! runtime state (the SHM slot registry, the SHM allocator). These
//! thin wrappers translate between the server's Rust types and the
//! libmorloc.so C ABI.

use std::os::raw::{c_char, c_void};

use morloc_runtime_types::cschema::CSchema;
use morloc_runtime_types::schema::{parse_schema, SerialType};
use morloc_runtime_types::packet::read_schema_from_meta;

// ── Runtime FFI ────────────────────────────────────────────────────────────

extern "C" {
    /// Materialise one voidstar `Array<T>` in SHM from a self-contained
    /// `MORLOC_DATA_PACKET` sub-packet's bytes. `elem_schema` is the
    /// element schema `T` (streams are list-shaped, so the sub-packet's
    /// value is `[T]`). Returns the SHM voidstar, or null on error.
    /// Caller owns the voidstar and frees it with `shfree`.
    fn mlc_materialize_subpacket_from_bytes(
        bytes: *const u8,
        n: u64,
        elem_schema: *const CSchema,
        errmsg: *mut *mut c_char,
    ) -> *mut c_void;

    /// Render a voidstar as JSON lines (one element per line for a list
    /// schema) directly to fd 1. `schema` is the full `[T]` value schema.
    fn print_voidstar_jsonl(
        data: *const c_void,
        schema: *const CSchema,
        errmsg: *mut *mut c_char,
    ) -> i32;

    /// Emit a `Str`/`[Str]` voidstar as verbatim bytes to fd 1 (the `-f raw`
    /// format). `schema` is the full `[Str]` value schema.
    fn print_voidstar_raw(
        data: *const c_void,
        schema: *const CSchema,
        errmsg: *mut *mut c_char,
    ) -> i32;

    /// Render a voidstar to a freshly malloc'd JSON string. `schema` is
    /// the full value schema. Caller frees the returned buffer.
    fn voidstar_to_json_string(
        voidstar: *const c_void,
        schema: *const CSchema,
        errmsg: *mut *mut c_char,
    ) -> *mut c_char;

    /// Re-emit a `MORLOC_DATA_PACKET`'s bytes to `fd`, recompressing the
    /// payload at `compression_level`. Signature must match the extern
    /// in `dispatch.rs` so the clashing-extern check stays quiet.
    fn normalize_data_packet_to_fd(
        packet: *const u8,
        packet_size: usize,
        compression_level: u8,
        fd: libc::c_int,
        errmsg: *mut *mut c_char,
    ) -> i64;

    /// Free an SHM voidstar block. Signature matches `view.rs`'s extern.
    fn shfree(ptr: *mut c_void, errmsg: *mut *mut c_char) -> bool;

    /// Runtime accessor: return the stdio_kind byte for a stdio-bound
    /// slot, or a negative error code. `kind_out` is filled on success.
    /// Returns 0 on success, negative on error. The caller frees
    /// `*errmsg` if it comes back non-null.
    fn mlc_stdio_slot_kind(
        handle: i64,
        kind_out: *mut u8,
        errmsg: *mut *mut c_char,
    ) -> i32;

    /// Runtime accessor: allocate a MORLOC_STREAM_PACKET header block
    /// (32-byte header + schema metadata, padded to alignment) for the
    /// given stdio slot. Returns a malloc'd buffer and its length via
    /// out params. On error returns 1 and sets errmsg. Caller frees
    /// `*buf_out` via libc::free.
    fn mlc_stdio_build_stream_header(
        handle: i64,
        buf_out: *mut *mut u8,
        len_out: *mut usize,
        errmsg: *mut *mut c_char,
    ) -> i32;

    /// Runtime accessor: return the slot's cached element schema string
    /// as a libc::malloc'd NUL-terminated buffer for schema-mismatch
    /// checks at stdin read time.
    fn mlc_stdio_slot_schema(
        handle: i64,
        out_buf: *mut *mut c_char,
        errmsg: *mut *mut c_char,
    ) -> i32;

    /// SHM allocator: allocate `size` bytes in the shared arena and
    /// return an absolute pointer plus its RelPtr. Both returned so the
    /// caller can memcpy in place before handing off the RelPtr.
    fn shmalloc(size: usize, errmsg: *mut *mut c_char) -> *mut c_void;

    /// SHM address translation: relptr -> absolute pointer. Returns
    /// null on failure. `errmsg` is set if provided.
    fn rel2abs(relptr: isize, errmsg: *mut *mut c_char) -> *mut c_void;

    /// SHM address translation: absolute pointer -> relptr. Returns
    /// -1 on failure. RelPtr is `isize` in the runtime type system;
    /// matches the extern in `nexus/src/dispatch.rs` to keep the
    /// compiler's clashing-extern-declaration check quiet.
    fn abs2rel(ptr: *mut c_void, errmsg: *mut *mut c_char) -> isize;
}

fn take_err(errmsg: *mut c_char) -> String {
    if errmsg.is_null() {
        return "unknown error (no errmsg from libmorloc.so)".into();
    }
    unsafe {
        let msg = std::ffi::CStr::from_ptr(errmsg).to_string_lossy().into_owned();
        libc::free(errmsg as *mut c_void);
        msg
    }
}

/// Return the stdio_kind (`STDIO_KIND_STDIN` / STDOUT / STDERR) for a
/// stdio-bound handle, or `Ok(None)` if the handle isn't stdio-bound.
pub unsafe fn stdio_kind_of(handle: i64) -> Result<Option<u8>, String> {
    let mut kind: u8 = 0;
    let mut err: *mut c_char = std::ptr::null_mut();
    let rc = mlc_stdio_slot_kind(handle, &mut kind, &mut err);
    if rc == 0 {
        Ok(Some(kind))
    } else if rc == -1 {
        // Convention: -1 means "not a stdio slot" (no error emitted).
        if !err.is_null() { libc::free(err as *mut c_void); }
        Ok(None)
    } else {
        Err(take_err(err))
    }
}

/// Fetch the slot's cached element schema string. Used at stdin
/// read time to compare against the SCHEMA_STRING metadata block
/// carried in the incoming stream-file header, so a program that
/// opens `@stdin :: IStream Int` fails loudly when the upstream
/// wrote an `IStream Str`.
pub unsafe fn stdio_slot_schema(handle: i64) -> Result<String, String> {
    let mut buf: *mut c_char = std::ptr::null_mut();
    let mut err: *mut c_char = std::ptr::null_mut();
    let rc = mlc_stdio_slot_schema(handle, &mut buf, &mut err);
    if rc != 0 || buf.is_null() {
        return Err(take_err(err));
    }
    let s = std::ffi::CStr::from_ptr(buf).to_string_lossy().into_owned();
    libc::free(buf as *mut c_void);
    Ok(s)
}

/// Emit the `MORLOC_STREAM_PACKET` prefix for the given stdio slot to
/// `fd`. Used lazily on the first `WRITE_STDIO` per slot.
pub unsafe fn write_stream_header_for_slot(fd: i32, handle: i64) -> Result<(), String> {
    let mut buf: *mut u8 = std::ptr::null_mut();
    let mut len: usize = 0;
    let mut err: *mut c_char = std::ptr::null_mut();
    let rc = mlc_stdio_build_stream_header(handle, &mut buf, &mut len, &mut err);
    if rc != 0 || buf.is_null() {
        return Err(take_err(err));
    }
    let slice = std::slice::from_raw_parts(buf, len);
    let r = write_all_fd(fd, slice);
    libc::free(buf as *mut c_void);
    r
}

/// Read `size` bytes from the SHM block at `relptr` and write them to
/// `fd`. Used by `WRITE_STDIO`. The bytes are the raw sub-packet
/// (header + metadata + payload) the pool assembled in SHM.
pub unsafe fn write_shm_bytes_to_fd(fd: i32, relptr: i64, size: u64) -> Result<(), String> {
    let mut err: *mut c_char = std::ptr::null_mut();
    let abs = rel2abs(relptr as isize, &mut err);
    if abs.is_null() {
        return Err(take_err(err));
    }
    let slice = std::slice::from_raw_parts(abs as *const u8, size as usize);
    write_all_fd(fd, slice)
}

/// Copy `bytes` into a fresh SHM block; return `(relptr, size)` for
/// hand-off to the pool via the socket response.
pub unsafe fn copy_into_shm(bytes: &[u8]) -> Result<(i64, u64), String> {
    let mut err: *mut c_char = std::ptr::null_mut();
    let abs = shmalloc(bytes.len(), &mut err);
    if abs.is_null() {
        return Err(take_err(err));
    }
    std::ptr::copy_nonoverlapping(bytes.as_ptr(), abs as *mut u8, bytes.len());
    let rel = abs2rel(abs, &mut err);
    if rel < 0 {
        return Err(take_err(err));
    }
    Ok((rel as i64, bytes.len() as u64))
}

fn write_all_fd(fd: i32, bytes: &[u8]) -> Result<(), String> {
    let mut off = 0usize;
    while off < bytes.len() {
        let n = unsafe {
            libc::write(
                fd,
                bytes.as_ptr().add(off) as *const c_void,
                bytes.len() - off,
            )
        };
        if n < 0 {
            let e = std::io::Error::last_os_error();
            if e.raw_os_error() == Some(libc::EINTR) { continue; }
            return Err(format!("write(fd {}): {}", fd, e));
        }
        if n == 0 {
            return Err(format!("write(fd {}) returned 0", fd));
        }
        off += n as usize;
    }
    Ok(())
}

/// Write raw bytes to `fd`. Public wrapper over the internal writer for
/// the transcoding paths (JSON brackets, footer pass-through).
pub unsafe fn write_bytes_to_fd(fd: i32, bytes: &[u8]) -> Result<(), String> {
    write_all_fd(fd, bytes)
}

// ── stdout format transcoding ───────────────────────────────────────────────
//
// When a command streams through `@stdout`, each sub-packet arrives here
// as a self-contained voidstar `MORLOC_DATA_PACKET` in SHM. Instead of
// appending the raw stream bytes to fd 1, we re-encode per the nexus's
// `-f` output format so streamed output honours `-f`/`-z` exactly like a
// returned value.

/// RAII guard for an SHM voidstar we own; `Drop` calls `shfree`. Mirrors
/// `view.rs::ShmVoidstarGuard`. Each transcoded sub-packet frees its
/// materialised voidstar before the next arrives, so a long stream holds
/// at most one sub-packet's value in SHM at a time.
struct ShmVoidstar(*mut c_void);

impl Drop for ShmVoidstar {
    fn drop(&mut self) {
        if !self.0.is_null() {
            let mut err: *mut c_char = std::ptr::null_mut();
            unsafe { shfree(self.0, &mut err); }
            if !err.is_null() {
                unsafe { libc::free(err as *mut c_void); }
            }
            self.0 = std::ptr::null_mut();
        }
    }
}

/// Copy `size` bytes of the SHM block at `relptr` into an owned `Vec`.
pub unsafe fn read_shm_bytes(relptr: i64, size: u64) -> Result<Vec<u8>, String> {
    let mut err: *mut c_char = std::ptr::null_mut();
    let abs = rel2abs(relptr as isize, &mut err);
    if abs.is_null() {
        return Err(take_err(err));
    }
    let slice = std::slice::from_raw_parts(abs as *const u8, size as usize);
    Ok(slice.to_vec())
}

/// Extract the `[T]` value schema string from a sub-packet's
/// `SCHEMA_STRING` metadata.
pub fn subpacket_value_schema_str(bytes: &[u8]) -> Result<String, String> {
    match read_schema_from_meta(bytes) {
        Ok(Some(s)) => Ok(s),
        Ok(None) => Err("sub-packet carries no SCHEMA_STRING metadata".into()),
        Err(e) => Err(format!("sub-packet schema parse: {}", e)),
    }
}

/// Parse a `[T]` value schema string into the value + element `CSchema`
/// pair needed by the renderers. Streams are list-shaped, so the value
/// schema must be an `Array` with one parameter.
fn build_value_and_elem_cschema(
    value_schema_str: &str,
) -> Result<(*mut CSchema, *mut CSchema), String> {
    let value_schema = parse_schema(value_schema_str)
        .map_err(|e| format!("value schema '{}': {}", value_schema_str, e))?;
    if value_schema.serial_type != SerialType::Array || value_schema.parameters.is_empty() {
        return Err(format!(
            "streamed stdout output requires list-shaped data, but the \
             sub-packet schema '{}' is not a list",
            value_schema_str,
        ));
    }
    let elem_schema = value_schema.parameters[0].clone();
    let value_c = CSchema::from_rust(&value_schema);
    let elem_c = CSchema::from_rust(&elem_schema);
    Ok((value_c, elem_c))
}

/// Materialise a sub-packet into an SHM voidstar guarded for cleanup,
/// alongside the value/element `CSchema` pair. Frees both `CSchema`s and
/// the voidstar when the returned guard/schemas are dropped by the
/// caller via `free_cschemas`.
unsafe fn materialize(
    bytes: &[u8],
    value_schema_str: &str,
) -> Result<(ShmVoidstar, *mut CSchema, *mut CSchema), String> {
    let (value_c, elem_c) = build_value_and_elem_cschema(value_schema_str)?;
    let mut err: *mut c_char = std::ptr::null_mut();
    let vs = mlc_materialize_subpacket_from_bytes(
        bytes.as_ptr(),
        bytes.len() as u64,
        elem_c,
        &mut err,
    );
    if vs.is_null() {
        CSchema::free(value_c);
        CSchema::free(elem_c);
        return Err(format!("materialise sub-packet: {}", take_err(err)));
    }
    Ok((ShmVoidstar(vs), value_c, elem_c))
}

unsafe fn free_cschemas(value_c: *mut CSchema, elem_c: *mut CSchema) {
    CSchema::free(value_c);
    CSchema::free(elem_c);
}

/// Materialise a sub-packet and emit it via `render` (a `print_voidstar_*`
/// FFI), labelling any error with `what`. Shared by the jsonl and raw stream
/// emitters, which differ only in the renderer and the label.
unsafe fn emit_subpacket_with(
    bytes: &[u8],
    value_schema_str: &str,
    what: &str,
    render: unsafe extern "C" fn(*const c_void, *const CSchema, *mut *mut c_char) -> i32,
) -> Result<(), String> {
    let (vs, value_c, elem_c) = materialize(bytes, value_schema_str)?;
    let mut err: *mut c_char = std::ptr::null_mut();
    let rc = render(vs.0, value_c, &mut err);
    free_cschemas(value_c, elem_c);
    drop(vs);
    if rc != 0 {
        return Err(format!("{}: {}", what, take_err(err)));
    }
    Ok(())
}

/// Emit one sub-packet as JSON lines to fd 1 (constant memory).
pub unsafe fn emit_subpacket_jsonl(bytes: &[u8], value_schema_str: &str) -> Result<(), String> {
    emit_subpacket_with(bytes, value_schema_str, "jsonl emit", print_voidstar_jsonl)
}

/// Emit one sub-packet's `[Str]` bodies verbatim to fd 1 (the `-f raw`
/// format for `render` handlers).
pub unsafe fn emit_subpacket_raw(bytes: &[u8], value_schema_str: &str) -> Result<(), String> {
    emit_subpacket_with(bytes, value_schema_str, "raw emit", print_voidstar_raw)
}

/// Render one sub-packet's `[T]` as a JSON array string and return the
/// comma-joined element body with the outer `[` / `]` stripped, so the
/// caller can splice it into a single streamed JSON array across
/// sub-packets. Returns an empty string for an empty sub-packet.
pub unsafe fn subpacket_json_inner(bytes: &[u8], value_schema_str: &str) -> Result<String, String> {
    let (vs, value_c, elem_c) = materialize(bytes, value_schema_str)?;
    let mut err: *mut c_char = std::ptr::null_mut();
    let s_ptr = voidstar_to_json_string(vs.0, value_c, &mut err);
    free_cschemas(value_c, elem_c);
    drop(vs);
    if s_ptr.is_null() {
        return Err(format!("json render: {}", take_err(err)));
    }
    let s = std::ffi::CStr::from_ptr(s_ptr).to_string_lossy().into_owned();
    libc::free(s_ptr as *mut c_void);
    let trimmed = s.trim();
    if !trimmed.starts_with('[') || !trimmed.ends_with(']') {
        return Err(format!(
            "streamed JSON sub-packet did not render as an array (got `{}`)",
            trimmed,
        ));
    }
    Ok(trimmed[1..trimmed.len() - 1].trim().to_string())
}

/// Re-emit one sub-packet's `MORLOC_DATA_PACKET` bytes to `fd`,
/// recompressing at `level`. Used by `-f packet` / `-f voidstar` so the
/// nexus `-z` governs the on-wire stream (the pool ships uncompressed).
pub unsafe fn emit_subpacket_as_packet(fd: i32, bytes: &[u8], level: u8) -> Result<(), String> {
    let mut err: *mut c_char = std::ptr::null_mut();
    let n = normalize_data_packet_to_fd(
        bytes.as_ptr(),
        bytes.len(),
        level,
        fd as libc::c_int,
        &mut err,
    );
    if n < 0 {
        return Err(format!("packet re-emit: {}", take_err(err)));
    }
    Ok(())
}
