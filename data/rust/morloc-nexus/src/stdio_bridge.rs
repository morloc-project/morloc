//! FFI shims between the nexus stdio server and libmorloc.so.
//!
//! The stdio server runs inside the nexus binary but needs to touch
//! runtime state (the SHM slot registry, the SHM allocator). These
//! thin wrappers translate between the server's Rust types and the
//! libmorloc.so C ABI.

use std::os::raw::{c_char, c_void};

// ── Runtime FFI ────────────────────────────────────────────────────────────

extern "C" {
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
