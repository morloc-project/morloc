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
    /// Write a `[Str]`/`[[U8]]`/`[U8]` sub-packet's bodies to `fd` where they
    /// lie; returns a `morloc_runtime_types::RAW_*` code (`*errmsg` on failure).
    /// The sub-packet is given as `head` (header and metadata) and
    /// `payload` (its payload region, with or without the alignment padding).
    fn mlc_write_raw_subpacket(
        head: *const u8, head_len: u64, payload: *const u8, payload_len: u64, fd: i32, errmsg: *mut *mut c_char,
    ) -> i32;

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
/// `fd`. Used lazily on the first write per slot.
pub unsafe fn write_stream_header_for_slot(fd: i32, handle: i64) -> Result<u64, WriteError> {
    let mut buf: *mut u8 = std::ptr::null_mut();
    let mut len: usize = 0;
    let mut err: *mut c_char = std::ptr::null_mut();
    let rc = mlc_stdio_build_stream_header(handle, &mut buf, &mut len, &mut err);
    if rc != 0 || buf.is_null() {
        return Err(WriteError::Other(take_err(err)));
    }
    let slice = std::slice::from_raw_parts(buf, len);
    let r = write_all_fd(fd, slice);
    libc::free(buf as *mut c_void);
    r.map(|()| len as u64)
}

/// Read a packet into a fresh SHM block: `prefix`, then `rest` more bytes
/// that `fill` reads in place (`Ok(false)`: input ended first). Returns
/// `(relptr, size)` for hand-off to the pool via the socket response; on
/// any error the block is freed.
pub unsafe fn read_into_shm(
    prefix: &[u8],
    rest: usize,
    fill: impl FnOnce(&mut [u8]) -> Result<bool, String>,
    short: &str,
) -> Result<(i64, u64), String> {
    let total = prefix
        .len()
        .checked_add(rest)
        .ok_or_else(|| "stdin packet size overflow".to_string())?;
    let mut err: *mut c_char = std::ptr::null_mut();
    let abs = shmalloc(total, &mut err);
    if abs.is_null() {
        return Err(take_err(err));
    }
    let block = std::slice::from_raw_parts_mut(abs as *mut u8, total);
    block[..prefix.len()].copy_from_slice(prefix);
    let r = match fill(&mut block[prefix.len()..]) {
        Ok(true) => {
            let rel = abs2rel(abs, &mut err);
            if rel < 0 { Err(take_err(err)) } else { Ok((rel as i64, total as u64)) }
        }
        Ok(false) => Err(short.to_string()),
        Err(e) => Err(e),
    };
    if r.is_err() {
        let mut ferr: *mut c_char = std::ptr::null_mut();
        if !shfree(abs, &mut ferr) {
            drop(take_err(ferr));
        }
    }
    r
}

/// Error from a stdout/stderr write. `BrokenPipe` is the `<IO>` condition
/// the downstream consumer closing the pipe (`EPIPE`); the nexus must not
/// let `@catch` swallow it. `Other` carries every non-pipe failure. A
/// `String` converts to `Other` so the transcoding helpers (whose schema
/// / render steps produce `String` errors) compose with `?` into a
/// `Result<_, WriteError>` without per-call mapping.
#[derive(Debug)]
pub(crate) enum WriteError {
    BrokenPipe,
    Other(String),
}

impl From<String> for WriteError {
    fn from(s: String) -> Self { WriteError::Other(s) }
}

/// One frame from the custodian (a sub-packet or a footer), as the parts
/// whose concatenation it is (SLOT-16).
#[derive(Clone, Copy)]
pub struct Frame<'a>(pub &'a [&'a [u8]]);

impl<'a> Frame<'a> {
    pub fn len(&self) -> usize {
        self.0.iter().map(|p| p.len()).sum()
    }

    /// The first 32 bytes, the packet header.
    pub fn header(&self) -> Option<[u8; 32]> {
        let mut h = [0u8; 32];
        let mut at = 0;
        for p in self.0 {
            let n = p.len().min(32 - at);
            h[at..at + n].copy_from_slice(&p[..n]);
            at += n;
            if at == 32 {
                return Some(h);
            }
        }
        None
    }

    /// The frame as one slice, copied only when it has several parts.
    pub fn joined(&self) -> std::borrow::Cow<'a, [u8]> {
        match self.0 {
            [] => std::borrow::Cow::Borrowed(&[]),
            [one] => std::borrow::Cow::Borrowed(one),
            parts => std::borrow::Cow::Owned(parts.concat()),
        }
    }
}

fn write_all_fd(fd: i32, bytes: &[u8]) -> Result<(), WriteError> {
    write_parts_fd(fd, &[bytes])
}

fn write_parts_fd(fd: i32, parts: &[&[u8]]) -> Result<(), WriteError> {
    writev_all_with(fd, parts, |iov| unsafe { libc::writev(fd, iov.as_ptr(), iov.len() as i32) })
}

/// Write every part in order through `write`, which returns as `writev`
/// does, resuming after a short write.
fn writev_all_with(
    fd: i32,
    parts: &[&[u8]],
    mut write: impl FnMut(&[libc::iovec]) -> isize,
) -> Result<(), WriteError> {
    const MAX_IOV: usize = 64;
    // Darwin refuses a writev of more than INT_MAX bytes.
    const MAX_BYTES: usize = 1 << 30;
    let mut i = 0;
    let mut skip = 0usize;
    let mut iov: Vec<libc::iovec> = Vec::with_capacity(parts.len().min(MAX_IOV));
    loop {
        while i < parts.len() && parts[i].len() == skip {
            i += 1;
            skip = 0;
        }
        if i == parts.len() {
            return Ok(());
        }
        iov.clear();
        let mut total = 0usize;
        for (k, p) in parts[i..].iter().take(MAX_IOV).enumerate() {
            let p = if k == 0 { &p[skip..] } else { p };
            let take = p.len().min(MAX_BYTES - total);
            iov.push(libc::iovec { iov_base: p.as_ptr() as *mut c_void, iov_len: take });
            total += take;
            if total == MAX_BYTES {
                break;
            }
        }
        let n = write(&iov);
        if n < 0 {
            let e = std::io::Error::last_os_error();
            if e.raw_os_error() == Some(libc::EINTR) { continue; }
            if e.raw_os_error() == Some(libc::EPIPE) {
                return Err(WriteError::BrokenPipe);
            }
            return Err(WriteError::Other(format!("write(fd {}): {}", fd, e)));
        }
        if n == 0 {
            return Err(WriteError::Other(format!("write(fd {}) returned 0", fd)));
        }
        let mut n = n as usize;
        while n > 0 {
            let left = parts[i].len() - skip;
            if n < left {
                skip += n;
                break;
            }
            n -= left;
            i += 1;
            skip = 0;
        }
    }
}

/// Write raw bytes to `fd`. Public wrapper over the internal writer for
/// the transcoding paths (JSON brackets, footer pass-through).
pub unsafe fn write_bytes_to_fd(fd: i32, bytes: &[u8]) -> Result<(), WriteError> {
    write_all_fd(fd, bytes)
}

/// Write a frame's parts to `fd`, in order, without joining them.
pub unsafe fn write_frame_to_fd(fd: i32, frame: Frame<'_>) -> Result<(), WriteError> {
    write_parts_fd(fd, frame.0)
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
) -> Result<(), WriteError> {
    let (vs, value_c, elem_c) = materialize(bytes, value_schema_str)?;
    let mut err: *mut c_char = std::ptr::null_mut();
    let rc = render(vs.0, value_c, &mut err);
    free_cschemas(value_c, elem_c);
    drop(vs);
    // The print_voidstar_* renderers return PRINT_RESULT_PIPE_CLOSED on a
    // broken pipe (json_ffi), which must surface as BrokenPipe rather than
    // a generic error so @catch cannot swallow it.
    if rc == morloc_runtime_types::PRINT_RESULT_PIPE_CLOSED {
        return Err(WriteError::BrokenPipe);
    }
    if rc != 0 {
        return Err(WriteError::Other(format!("{}: {}", what, take_err(err))));
    }
    Ok(())
}

/// Emit one sub-packet as JSON lines to fd 1 (constant memory).
pub unsafe fn emit_subpacket_jsonl(bytes: &[u8], value_schema_str: &str) -> Result<(), WriteError> {
    emit_subpacket_with(bytes, value_schema_str, "jsonl emit", print_voidstar_jsonl)
}

/// Emit one sub-packet's `[Str]` bodies verbatim to fd 1 (the `-f raw`
/// format for `render` handlers). `head_len` is the length of its header
/// and metadata.
pub unsafe fn emit_subpacket_raw(frame: Frame<'_>, head_len: usize) -> Result<(), WriteError> {
    use std::io::Write;
    let mut out = std::io::stdout().lock();
    if let Err(e) = out.flush() {
        return Err(if e.kind() == std::io::ErrorKind::BrokenPipe { WriteError::BrokenPipe } else { WriteError::Other(format!("raw emit: {e}")) });
    }
    let joined;
    let (head, payload) = match frame.0 {
        [head, payload, ..] if head.len() == head_len => (*head, *payload),
        _ => {
            joined = frame.joined();
            if joined.len() < head_len {
                return Err(WriteError::Other("raw emit: a sub-packet is shorter than its header".into()));
            }
            joined.split_at(head_len)
        }
    };
    let mut err: *mut c_char = std::ptr::null_mut();
    match mlc_write_raw_subpacket(
        head.as_ptr(), head.len() as u64, payload.as_ptr(), payload.len() as u64, libc::STDOUT_FILENO, &mut err,
    ) {
        morloc_runtime_types::RAW_WRITTEN => Ok(()),
        morloc_runtime_types::RAW_PIPE_CLOSED => Err(WriteError::BrokenPipe),
        morloc_runtime_types::RAW_NOT_HANDLED => {
            drop(out);
            let bytes = frame.joined();
            let schema = subpacket_value_schema_str(&bytes)?;
            emit_subpacket_with(&bytes, &schema, "raw emit", print_voidstar_raw)
        }
        _ => Err(WriteError::Other(format!("raw emit: {}", take_err(err)))),
    }
}

/// A sub-packet's `[T]` rendered as JSON, held in the runtime's malloc'd
/// string; `inner` is the comma-joined element body with the outer `[` /
/// `]` stripped, so the caller can splice it into a single streamed JSON
/// array across sub-packets.
pub struct JsonInner {
    ptr: *mut c_char,
    body: std::ops::Range<usize>,
}

impl JsonInner {
    pub fn inner(&self) -> &[u8] {
        // SAFETY: `body` lies within the string `ptr` holds.
        unsafe {
            std::slice::from_raw_parts((self.ptr as *const u8).add(self.body.start), self.body.len())
        }
    }
}

impl Drop for JsonInner {
    fn drop(&mut self) {
        unsafe { libc::free(self.ptr as *mut c_void) };
    }
}

/// The byte range of a rendered JSON array's element body: the text
/// between its outer brackets, whitespace-trimmed.
fn json_array_body(s: &str) -> Result<std::ops::Range<usize>, String> {
    let trimmed = s.trim();
    if !trimmed.starts_with('[') || !trimmed.ends_with(']') {
        return Err(format!(
            "streamed JSON sub-packet did not render as an array (got `{}`)",
            trimmed,
        ));
    }
    let body = trimmed[1..trimmed.len() - 1].trim();
    let start = body.as_ptr() as usize - s.as_ptr() as usize;
    Ok(start..start + body.len())
}

/// Render one sub-packet's `[T]` as JSON; empty `inner` for an empty
/// sub-packet.
pub unsafe fn subpacket_json_inner(bytes: &[u8], value_schema_str: &str) -> Result<JsonInner, String> {
    let (vs, value_c, elem_c) = materialize(bytes, value_schema_str)?;
    let mut err: *mut c_char = std::ptr::null_mut();
    let s_ptr = voidstar_to_json_string(vs.0, value_c, &mut err);
    free_cschemas(value_c, elem_c);
    drop(vs);
    if s_ptr.is_null() {
        return Err(format!("json render: {}", take_err(err)));
    }
    let mut out = JsonInner { ptr: s_ptr, body: 0..0 };
    let s = std::ffi::CStr::from_ptr(s_ptr)
        .to_str()
        .map_err(|e| format!("json render: output is not valid UTF-8 (byte {})", e.valid_up_to()))?;
    out.body = json_array_body(s)?;
    Ok(out)
}


#[cfg(test)]
mod json_array_body_tests {
    use super::json_array_body;

    fn copied(s: &str) -> String {
        let trimmed = s.trim();
        trimmed[1..trimmed.len() - 1].trim().to_string()
    }

    #[test]
    fn the_body_slice_matches_the_trimmed_copy() {
        for s in ["[]", "[1,2,3]", " [ 1 , 2 ]\n", "[\"a\",\"b\"]", "\t[[1],[2]] ", "[ ]", "[\"\u{e9}\"]"] {
            let r = json_array_body(s).unwrap();
            assert_eq!(&s[r], copied(s), "input {s:?}");
        }
    }

    #[test]
    fn a_non_array_is_refused() {
        for s in ["", "1", "\"x\"", "[1", "1]", "{}"] {
            assert!(json_array_body(s).is_err(), "input {s:?}");
        }
    }
}

#[cfg(test)]
mod frame_write_tests {
    use super::{write_frame_to_fd, writev_all_with, Frame, WriteError};

    fn set_errno(e: i32) {
        #[cfg(target_os = "linux")]
        unsafe { *libc::__errno_location() = e };
        #[cfg(target_os = "macos")]
        unsafe { *libc::__error() = e };
    }

    fn sample() -> (Vec<u8>, Vec<u8>, Vec<u8>) {
        let head: Vec<u8> = (0..72u8).collect();
        let payload: Vec<u8> = (0..200_003u32).map(|i| (i * 7 % 251) as u8).collect();
        (head, payload, vec![0u8; 5])
    }

    fn read_all(fd: i32) -> Vec<u8> {
        let mut got = Vec::new();
        let mut buf = [0u8; 65536];
        loop {
            let n = unsafe { libc::read(fd, buf.as_mut_ptr() as *mut libc::c_void, buf.len()) };
            if n <= 0 {
                break;
            }
            got.extend_from_slice(&buf[..n as usize]);
        }
        got
    }

    #[test]
    fn a_frame_written_in_parts_is_the_joined_frame() {
        let (head, payload, pad) = sample();
        let joined = [&head[..], &payload, &pad].concat();
        for parts in [
            vec![&head[..], &payload[..], &pad[..]],
            vec![&head[..], &payload[..], &[][..]],
            vec![&joined[..]],
            vec![&[][..], &joined[..10], &[][..], &joined[10..]],
        ] {
            let want: Vec<u8> = parts.concat();
            let frame = Frame(&parts);
            assert_eq!(frame.len(), want.len());
            assert_eq!(&frame.header().unwrap()[..], &want[..32]);
            assert!(frame.joined() == want);
            let mut fds = [0i32; 2];
            assert_eq!(unsafe { libc::pipe(fds.as_mut_ptr()) }, 0);
            let reader = fds[0];
            let t = std::thread::spawn(move || read_all(reader));
            unsafe { write_frame_to_fd(fds[1], frame) }.unwrap();
            unsafe { libc::close(fds[1]) };
            let got = t.join().unwrap();
            unsafe { libc::close(fds[0]) };
            assert!(got == want, "the bytes written differ from the joined frame");
        }
        assert!(Frame(&[&head[..20], &head[20..31]]).header().is_none());
        assert!(matches!(Frame(&[&joined[..]]).joined(), std::borrow::Cow::Borrowed(_)));
    }

    #[test]
    fn short_writes_and_interrupts_resume_where_they_stopped() {
        let (head, payload, pad) = sample();
        let parts: Vec<&[u8]> = vec![&head, &[], &payload, &pad];
        let want = parts.concat();
        for cap in [1usize, 3, 71, 72, 73, 4096, 1 << 20] {
            let mut got = Vec::new();
            let mut calls = 0;
            writev_all_with(-1, &parts, |iov| {
                calls += 1;
                if calls % 3 == 0 {
                    set_errno(libc::EINTR);
                    return -1;
                }
                let mut left = cap;
                for v in iov {
                    let n = v.iov_len.min(left);
                    got.extend_from_slice(unsafe { std::slice::from_raw_parts(v.iov_base as *const u8, n) });
                    left -= n;
                    if left == 0 {
                        break;
                    }
                }
                (cap - left) as isize
            })
            .unwrap();
            assert!(got == want, "cap {cap}: the bytes written differ from the parts");
        }
    }

    #[test]
    fn a_closed_reader_is_a_broken_pipe_and_a_stuck_writer_an_error() {
        let (head, payload, pad) = sample();
        let mut fds = [0i32; 2];
        assert_eq!(unsafe { libc::pipe(fds.as_mut_ptr()) }, 0);
        unsafe { libc::close(fds[0]) };
        let r = unsafe { write_frame_to_fd(fds[1], Frame(&[&head, &payload, &pad])) };
        unsafe { libc::close(fds[1]) };
        assert!(matches!(r, Err(WriteError::BrokenPipe)), "{r:?}");
        let r = writev_all_with(-1, &[&head[..]], |_| 0);
        assert!(matches!(r, Err(WriteError::Other(_))), "{r:?}");
        let r = writev_all_with(-1, &[&head[..]], |_| {
            set_errno(libc::EIO);
            -1
        });
        assert!(matches!(r, Err(WriteError::Other(_))), "{r:?}");
    }
}
