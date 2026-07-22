//! Intrinsic functions for morloc: save/load/show/read/hash.
//! Replaces intrinsics.c. These are thin wrappers around serialization functions.

use std::ffi::{c_char, c_void, CStr, CString};
use std::ptr;

use crate::cschema::CSchema;
use crate::error::{clear_errmsg, set_errmsg, MorlocError};

// ── FFI wrapper helpers ────────────────────────────────────────────────────

/// Decode a `*const c_char` FFI argument as a `&str`. Returns a typed
/// error carrying the function and argument name so `wrap_c_call` can
/// surface a uniform "fn: null <arg>" / "fn: <arg> not UTF-8" message.
#[inline]
unsafe fn cstr_arg<'a>(
    p: *const c_char,
    fn_name: &str,
    arg_name: &str,
) -> Result<&'a str, MorlocError> {
    if p.is_null() {
        return Err(MorlocError::Other(format!("{}: null {}", fn_name, arg_name)));
    }
    CStr::from_ptr(p)
        .to_str()
        .map_err(|_| MorlocError::Other(format!("{}: {} not UTF-8", fn_name, arg_name)))
}

/// Bracket an FFI body with `clear_errmsg` on entry, `set_errmsg` on
/// error, and a caller-supplied error sentinel as the return value.
/// The body uses `?` to short-circuit any `MorlocError`.
#[inline]
unsafe fn wrap_c_call<T, F>(errmsg: *mut *mut c_char, sentinel: T, f: F) -> T
where
    F: FnOnce() -> Result<T, MorlocError>,
{
    clear_errmsg(errmsg);
    match f() {
        Ok(v) => v,
        Err(e) => {
            set_errmsg(errmsg, &e);
            sentinel
        }
    }
}

// ── mlc_save: serialize to msgpack file ────────────────────────────────────

// `level` is accepted for ABI uniformity with mlc_save_voidstar but is
// ignored here: the msgpack file is not a morloc packet and has no header
// in which to record a compression algorithm. Compressing the file as a
// whole is the deferred "compressed JSON/MPK input" workstream.
#[no_mangle]
pub unsafe extern "C" fn mlc_save(
    data: *const c_void,
    schema: *const CSchema,
    _level: u8,
    path: *const c_char,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);

    extern "C" {
        fn pack_with_schema(
            mlc: *const c_void, schema: *const CSchema,
            mpk: *mut *mut c_char, mpk_size: *mut usize,
            errmsg: *mut *mut c_char,
        ) -> i32;
        fn write_atomic(
            filename: *const c_char, data: *const u8, size: usize,
            errmsg: *mut *mut c_char,
        ) -> i32;
    }

    let mut err: *mut c_char = ptr::null_mut();
    let mut mpk: *mut c_char = ptr::null_mut();
    let mut mpk_size: usize = 0;

    let rc = pack_with_schema(data, schema, &mut mpk, &mut mpk_size, &mut err);
    if rc != 0 {
        *errmsg = err;
        return 1;
    }

    let wrc = write_atomic(path, mpk as *const u8, mpk_size, &mut err);
    libc::free(mpk as *mut c_void);
    if wrc != 0 {
        *errmsg = err;
        return 1;
    }
    0
}

// ── mlc_save_json: serialize to JSON file ──────────────────────────────────

// `level` accepted but ignored; see mlc_save for the rationale.
#[no_mangle]
pub unsafe extern "C" fn mlc_save_json(
    data: *const c_void,
    schema: *const CSchema,
    _level: u8,
    path: *const c_char,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);

    extern "C" {
        fn voidstar_to_json_string(
            data: *const c_void, schema: *const CSchema,
            errmsg: *mut *mut c_char,
        ) -> *mut c_char;
        fn write_atomic(
            filename: *const c_char, data: *const u8, size: usize,
            errmsg: *mut *mut c_char,
        ) -> i32;
    }

    let mut err: *mut c_char = ptr::null_mut();
    let json = voidstar_to_json_string(data, schema, &mut err);
    if json.is_null() {
        *errmsg = err;
        return 1;
    }

    let json_len = libc::strlen(json);
    let wrc = write_atomic(path, json as *const u8, json_len, &mut err);
    libc::free(json as *mut c_void);
    if wrc != 0 {
        *errmsg = err;
        return 1;
    }
    0
}

// ── Voidstar data-packet emit ─────────────────────────────────────────────
//
// `mlc_save_voidstar` and `mlc_write_voidstar_data_packet_to_fd` both
// build a MORLOC_DATA_PACKET (header + SCHEMA_STRING + optional
// FRAME_INDEX + payload) from a voidstar. `build_voidstar_data_packet_parts`
// does the shared work: flatten voidstar, build metadata, optionally
// zstd-compress, assemble the 32-byte header. Emit strategies differ:
// `mlc_save_voidstar` writes to a temp file then renames for atomicity;
// the fd variant writes linearly to a caller-supplied fd.

// RAII wrapper for a libc::malloc'd buffer. Kept alive alongside
// VoidstarDataPacketParts so payload_ptr stays valid on the uncompressed
// path.
struct CBlob(*mut u8);
impl Drop for CBlob {
    fn drop(&mut self) {
        if !self.0.is_null() {
            unsafe { libc::free(self.0 as *mut c_void); }
        }
    }
}

// Emit-ready components of a voidstar DATA packet. Holds the flattened
// blob and optional compressed bytes so payload_ptr() borrows remain
// valid for the parts' lifetime.
struct VoidstarDataPacketParts {
    hdr_bytes: [u8; 32],
    metadata: Vec<u8>,
    payload_len: usize,
    // Uncompressed payload lives in blob; compressed payload lives in
    // comp_bytes. Exactly one of the two is the payload source.
    #[allow(dead_code)]
    blob: CBlob,
    comp_bytes: Option<Vec<u8>>,
}

impl VoidstarDataPacketParts {
    fn payload_ptr(&self) -> *const u8 {
        match &self.comp_bytes {
            Some(b) => b.as_ptr(),
            None => self.blob.0 as *const u8,
        }
    }
}

// Build packet parts. Returns None on error with errmsg set.
unsafe fn build_voidstar_data_packet_parts(
    data: *const c_void,
    schema: *const CSchema,
    level: u8,
    errmsg: *mut *mut c_char,
) -> Option<VoidstarDataPacketParts> {
    extern "C" {
        fn flatten_voidstar_to_buffer(
            data: *const c_void, schema: *const CSchema,
            out_buf: *mut *mut u8, out_size: *mut usize,
            errmsg: *mut *mut c_char,
        ) -> i32;
    }

    // Resolve level first so a bad value aborts before we allocate.
    let clvl = match crate::compression::CompressionLevel::from_u8(level) {
        Ok(lvl) => lvl,
        Err(e) => { set_errmsg(errmsg, &e); return None; }
    };

    let mut blob: *mut u8 = ptr::null_mut();
    let mut blob_size: usize = 0;
    let mut err: *mut c_char = ptr::null_mut();
    if flatten_voidstar_to_buffer(data, schema, &mut blob, &mut blob_size, &mut err) != 0 {
        *errmsg = err;
        return None;
    }
    let blob = CBlob(blob);

    // Self-describing SCHEMA_STRING lets IFile / `morloc-nexus file`
    // read the payload schema straight from the packet metadata.
    let schema_rs = crate::cschema::CSchema::to_rust(schema);
    let schema_str = morloc_runtime_types::schema::schema_to_string(&schema_rs);
    let mut schema_body = schema_str.as_bytes().to_vec();
    schema_body.push(0); // NUL-terminator expected by decode_schema_entry
    let base_meta = crate::packet::append_metadata_entry(
        &[],
        crate::packet::METADATA_TYPE_SCHEMA_STRING,
        &schema_body,
    );

    let (metadata, comp_bytes, compression_byte, payload_len) = if clvl.is_none() {
        (base_meta, None, crate::packet::PACKET_COMPRESSION_NONE, blob_size)
    } else {
        let raw_slice = std::slice::from_raw_parts(blob.0 as *const u8, blob_size);
        match crate::compression::compress_payload_zstd(raw_slice, clvl) {
            Ok((bytes, frames)) => {
                let body = crate::packet::encode_frame_index_entry(&frames);
                let metadata = crate::packet::append_metadata_entry(
                    &base_meta,
                    crate::packet::METADATA_TYPE_FRAME_INDEX,
                    &body,
                );
                let payload_len = bytes.len();
                (metadata, Some(bytes), crate::packet::PACKET_COMPRESSION_ZSTD, payload_len)
            }
            Err(e) => { set_errmsg(errmsg, &e); return None; }
        }
    };

    let mut header = crate::packet::PacketHeader::data_mesg(
        crate::packet::PACKET_FORMAT_VOIDSTAR,
        payload_len as u64,
    );
    header.offset = metadata.len() as u32;
    let mut hdr_bytes = header.to_bytes();
    // Byte 15 is the data-command compression slot: 4 magic + 2 plain +
    // 2 version + 2 flavor + 2 mode + 1 cmd_type + 1 source + 1 format.
    hdr_bytes[15] = compression_byte;

    Some(VoidstarDataPacketParts { hdr_bytes, metadata, payload_len, blob, comp_bytes })
}

// Write parts sequentially to fd. On failure returns the C errmsg
// pointer produced by write_binary_fd; caller owns cleanup.
unsafe fn write_data_packet_parts_to_fd(
    fd: libc::c_int,
    parts: &VoidstarDataPacketParts,
) -> Result<(), *mut c_char> {
    extern "C" {
        fn write_binary_fd(
            fd: i32, buf: *const c_char, count: usize,
            errmsg: *mut *mut c_char,
        ) -> i32;
    }
    let mut err: *mut c_char = ptr::null_mut();
    if write_binary_fd(fd, parts.hdr_bytes.as_ptr() as *const c_char, parts.hdr_bytes.len(), &mut err) != 0 {
        return Err(err);
    }
    if !parts.metadata.is_empty()
        && write_binary_fd(fd, parts.metadata.as_ptr() as *const c_char, parts.metadata.len(), &mut err) != 0
    {
        return Err(err);
    }
    if parts.payload_len > 0
        && write_binary_fd(fd, parts.payload_ptr() as *const c_char, parts.payload_len, &mut err) != 0
    {
        return Err(err);
    }
    Ok(())
}

// ── Temp-file gather (whole-form with:/render:) ────────────────────────────
//
// @tmpfile creates a fresh empty file in the morloc tmpdir and registers it
// in a per-call (thread-local) list. The whole-form handler synthesis gathers
// a stream into this file, applies the handler, then removes it with
// @close(path) (mlc_unlink_tmp). Any temp left registered at the end of the
// pool call is swept by sweep_call_temps (called from pool_dispatch_packet),
// so a handler that raises mid-call cannot leak the file. The list is
// thread-local: each pool worker sees only the temps of the call it is running,
// so concurrent dispatches never unlink each other's files.

thread_local! {
    static CALL_TEMPS: std::cell::RefCell<Vec<std::path::PathBuf>> =
        const { std::cell::RefCell::new(Vec::new()) };
}

/// @tmpfile :: <IO, Err> Str. Create a fresh empty file in the morloc tmpdir,
/// register it for removal at end-of-call, and return its path.
#[no_mangle]
pub unsafe extern "C" fn mlc_tmpfile(errmsg: *mut *mut c_char) -> *mut c_char {
    clear_errmsg(errmsg);
    let dir = crate::packet::file_packet_tmpdir()
        .unwrap_or_else(|| std::env::temp_dir().to_string_lossy().into_owned());
    let _ = std::fs::create_dir_all(&dir);
    let template = format!("{}/morloc-gather-XXXXXX\0", dir);
    let mut buf: Vec<u8> = template.into_bytes();
    let fd = libc::mkstemp(buf.as_mut_ptr() as *mut c_char);
    if fd < 0 {
        set_errmsg(errmsg, &MorlocError::Io(std::io::Error::last_os_error()));
        return ptr::null_mut();
    }
    libc::close(fd);
    buf.pop(); // drop the NUL mkstemp left in place
    let path = String::from_utf8_lossy(&buf).into_owned();
    CALL_TEMPS.with(|t| t.borrow_mut().push(std::path::PathBuf::from(&path)));
    match CString::new(path) {
        Ok(cs) => cs.into_raw(),
        Err(_) => {
            set_errmsg(errmsg, &MorlocError::Other("mlc_tmpfile: path contained NUL".into()));
            ptr::null_mut()
        }
    }
}

/// @close on a Str path: unlink a registered temp file and drop it from the
/// call list. Errors if the path was not created by @tmpfile in this call --
/// @close is not a general file-removal tool.
#[no_mangle]
pub unsafe extern "C" fn mlc_unlink_tmp(
    path: *const c_char,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    let path_str = match cstr_arg(path, "mlc_unlink_tmp", "path") {
        Ok(s) => s,
        Err(e) => {
            set_errmsg(errmsg, &e);
            return 1;
        }
    };
    let pb = std::path::PathBuf::from(path_str);
    let registered = CALL_TEMPS.with(|t| {
        let mut v = t.borrow_mut();
        match v.iter().position(|p| p == &pb) {
            Some(i) => {
                v.remove(i);
                true
            }
            None => false,
        }
    });
    if !registered {
        set_errmsg(errmsg, &MorlocError::Other(format!(
            "@close: '{}' is not a registered temp file; @close only removes \
             files created by the whole-list with:/render: gather",
            path_str
        )));
        return 1;
    }
    if let Err(e) = std::fs::remove_file(&pb) {
        if e.kind() != std::io::ErrorKind::NotFound {
            set_errmsg(errmsg, &MorlocError::Io(e));
            return 1;
        }
    }
    0
}

/// Remove any temp files still registered for the current call. Called from
/// pool_dispatch_packet after each dispatch returns, so a raising handler that
/// skipped its @close(path) cannot leak the gather file.
pub fn sweep_call_temps() {
    CALL_TEMPS.with(|t| {
        for p in t.borrow_mut().drain(..) {
            let _ = std::fs::remove_file(&p);
        }
    });
}

// ── mlc_save_voidstar: serialize to binary voidstar packet file ────────────
#[no_mangle]
pub unsafe extern "C" fn mlc_save_voidstar(
    data: *const c_void,
    schema: *const CSchema,
    level: u8,
    path: *const c_char,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);

    let parts = match build_voidstar_data_packet_parts(data, schema, level, errmsg) {
        Some(p) => p,
        None => return 1,
    };

    // Temp file in the target's parent dir for atomic rename on success.
    let path_str = CStr::from_ptr(path).to_string_lossy();
    let parent = std::path::Path::new(path_str.as_ref()).parent();
    let dir = match parent {
        Some(p) if !p.as_os_str().is_empty() => p.to_string_lossy().into_owned(),
        _ => ".".to_string(),
    };
    let tmp_template = format!("{}/morloc-tmp_XXXXXX\0", dir);
    let mut tmp_buf: Vec<u8> = tmp_template.into_bytes();
    let fd = libc::mkstemp(tmp_buf.as_mut_ptr() as *mut c_char);
    if fd < 0 {
        set_errmsg(errmsg, &MorlocError::Io(std::io::Error::last_os_error()));
        return 1;
    }

    if let Err(e) = write_data_packet_parts_to_fd(fd, &parts) {
        libc::close(fd);
        libc::unlink(tmp_buf.as_ptr() as *const c_char);
        *errmsg = e;
        return 1;
    }

    libc::fsync(fd);
    libc::close(fd);

    if libc::rename(tmp_buf.as_ptr() as *const c_char, path) != 0 {
        libc::unlink(tmp_buf.as_ptr() as *const c_char);
        set_errmsg(errmsg, &MorlocError::Io(std::io::Error::last_os_error()));
        return 1;
    }
    0
}

// ── mlc_write_voidstar_data_packet_to_fd ──────────────────────────────────
//
// Companion to `mlc_save_voidstar` for cases where the destination is
// stdout or an already-opened fd (e.g. `morloc-nexus view --pattern`
// with data-packet output, which has no source packet to hand
// `normalize_data_packet_to_fd`). Returns total bytes written, or -1
// on error.
#[no_mangle]
pub unsafe extern "C" fn mlc_write_voidstar_data_packet_to_fd(
    data: *const c_void,
    schema: *const CSchema,
    level: u8,
    fd: libc::c_int,
    errmsg: *mut *mut c_char,
) -> i64 {
    clear_errmsg(errmsg);

    let parts = match build_voidstar_data_packet_parts(data, schema, level, errmsg) {
        Some(p) => p,
        None => return -1,
    };

    if let Err(e) = write_data_packet_parts_to_fd(fd, &parts) {
        *errmsg = e;
        return -1;
    }

    (parts.hdr_bytes.len() + parts.metadata.len() + parts.payload_len) as i64
}

// ── mlc_load: load from file (auto-detect format) ─────────────────────────

#[no_mangle]
pub unsafe extern "C" fn mlc_load(
    path: *const c_char,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut c_void {
    clear_errmsg(errmsg);

    extern "C" {
        fn file_exists(filename: *const c_char) -> bool;
        fn read_binary_file(
            filename: *const c_char, file_size: *mut usize,
            errmsg: *mut *mut c_char,
        ) -> *mut u8;
        fn load_morloc_data_file(
            path: *const c_char, data: *mut u8, data_size: usize,
            schema: *const CSchema, errmsg: *mut *mut c_char,
        ) -> *mut c_void;
    }

    if !file_exists(path) {
        return ptr::null_mut();
    }

    // Stream-packet path: a MORLOC_STREAM_PACKET file holds its `[a]`
    // list value across many sub-packets (constant-memory gathers write
    // one sub-packet per flush). The data-packet loaders below only
    // understand single self-contained packets, so detect the stream
    // shape first and materialise every sub-packet into one voidstar
    // `[a]` list. Non-stream files fall through to the existing paths.
    match crate::cli::peek_packet_header_via_pread(path) {
        Some(h) if h.is_stream() => {
            let path_str = CStr::from_ptr(path).to_string_lossy().into_owned();
            match crate::stream::shared_load_stream_file_as_array(&path_str) {
                Ok(ptr) => return ptr as *mut c_void,
                Err(e) => {
                    set_errmsg(errmsg, &e);
                    return ptr::null_mut();
                }
            }
        }
        _ => { /* not a stream packet; fall through */ }
    }

    // Layer 2 fast path: large MESG+VOIDSTAR packets `pread` straight
    // into a fresh SHM allocation, skipping the libc::malloc + read +
    // shmalloc + memcpy chain. On failure (e.g. schema mismatch), the
    // error is propagated to the caller via errmsg, not swallowed to
    // stderr -- @catch downstream needs the classified failure to
    // intercept.
    match crate::cli::try_load_voidstar_packet_via_mmap(path, schema) {
        Ok(Some(ptr)) => return ptr as *mut c_void,
        Ok(None) => { /* fall through */ }
        Err(e) => {
            set_errmsg(errmsg, &e);
            return ptr::null_mut();
        }
    }
    // Compressed-VOIDSTAR fast path: stream-decompress a zstd frame
    // directly into a fresh SHM allocation. Same memory shape as the
    // uncompressed mmap path -- one X of the decompressed payload --
    // and avoids the multi-Vec / libc::malloc round-trip the generic
    // fallback below takes.
    match crate::cli::try_load_compressed_voidstar_via_shm(path, schema) {
        Ok(Some(ptr)) => return ptr as *mut c_void,
        Ok(None) => { /* fall through */ }
        Err(e) => {
            set_errmsg(errmsg, &e);
            return ptr::null_mut();
        }
    }

    let mut err: *mut c_char = ptr::null_mut();
    let mut file_size: usize = 0;
    let data = read_binary_file(path, &mut file_size, &mut err);
    if data.is_null() {
        // Propagate the file-read error verbatim so the caller sees
        // (for example) "no such file or directory" rather than a
        // generic NULL return; @catch relies on this text.
        if !err.is_null() {
            *errmsg = err;
        }
        return ptr::null_mut();
    }

    // Decompression is now handled inside `load_morloc_data_file`,
    // which is the single choke point all packet-load paths share.
    // Any error message it sets flows through to the caller's errmsg.
    let result = load_morloc_data_file(path, data, file_size, schema, &mut err);
    if result.is_null() && !err.is_null() {
        *errmsg = err;
    }
    result
}

// ── mlc_hash: hash voidstar data ───────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn mlc_hash(
    data: *const c_void,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut c_char {
    clear_errmsg(errmsg);

    let mut err: *mut c_char = ptr::null_mut();
    let hash = crate::cache::hash_voidstar(data, schema, 0, &mut err);
    if !err.is_null() {
        *errmsg = err;
        return ptr::null_mut();
    }

    let hex = format!("{:016x}", hash);
    match CString::new(hex) {
        Ok(cs) => cs.into_raw(),
        Err(_) => {
            set_errmsg(errmsg, &MorlocError::Other("CString error".into()));
            ptr::null_mut()
        }
    }
}

// ── mlc_show: serialize to JSON string ─────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn mlc_show(
    data: *const c_void,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut c_char {
    clear_errmsg(errmsg);

    extern "C" {
        fn voidstar_to_json_string(
            data: *const c_void, schema: *const CSchema,
            errmsg: *mut *mut c_char,
        ) -> *mut c_char;
    }

    voidstar_to_json_string(data, schema, errmsg)
}

// ── mlc_read: deserialize from JSON string ─────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn mlc_read(
    json_str: *const c_char,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut c_void {
    clear_errmsg(errmsg);

    extern "C" {
        fn read_json_with_schema(
            dest: *mut u8, json: *mut c_char, schema: *const CSchema,
            errmsg: *mut *mut c_char,
        ) -> *mut u8;
    }

    let json_copy = libc::strdup(json_str);
    if json_copy.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("strdup failed".into()));
        return ptr::null_mut();
    }

    let mut err: *mut c_char = ptr::null_mut();
    let result = read_json_with_schema(ptr::null_mut(), json_copy, schema, &mut err);
    libc::free(json_copy as *mut c_void);
    if result.is_null() {
        if !err.is_null() {
            libc::free(err as *mut c_void);
        }
    }
    result as *mut c_void
}

// write_voidstar_binary is provided by packet.c (still C)
// It will be ported when packet.c is ported to Rust.

// Unused Rust implementation kept for future use
#[allow(dead_code)]
unsafe fn _write_voidstar_binary_rust(
    fd: i32,
    data: *const c_void,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> isize {
    clear_errmsg(errmsg);

    extern "C" {
        fn flatten_voidstar_to_buffer(
            data: *const c_void, schema: *const CSchema,
            out_buf: *mut *mut u8, out_size: *mut usize,
            errmsg: *mut *mut c_char,
        ) -> i32;
        fn write_binary_fd(
            fd: i32, buf: *const c_char, count: usize,
            errmsg: *mut *mut c_char,
        ) -> i32;
    }

    let mut err: *mut c_char = ptr::null_mut();
    let mut blob: *mut u8 = ptr::null_mut();
    let mut blob_size: usize = 0;

    if flatten_voidstar_to_buffer(data, schema, &mut blob, &mut blob_size, &mut err) != 0 {
        *errmsg = err;
        return -1;
    }

    if write_binary_fd(fd, blob as *const c_char, blob_size, &mut err) != 0 {
        libc::free(blob as *mut c_void);
        *errmsg = err;
        return -1;
    }

    libc::free(blob as *mut c_void);
    blob_size as isize
}

// ── @open / @close / @fschema ──────────────────────────────────────────────
//
// Thin FFI shims around `crate::stream`. The kind byte selects which
// concrete open routine to invoke. OStream is routed through the typed
// entry point `mlc_open_ostream(schema_str, path)` because the writer
// needs the element schema at open time.

/// Probe a file for a valid morloc packet header without allocating
/// SHM or setting up any cursor state. Returns 0 if the file exists,
/// is readable, and starts with a well-formed morloc packet header.
/// Returns non-zero (with errmsg set) on any failure: missing file,
/// permission denied, short read, magic mismatch. Used by @open's
/// IFile branch for eager packet-format validation.
#[no_mangle]
pub unsafe extern "C" fn mlc_probe_packet(
    path: *const c_char,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    if path.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("mlc_probe_packet: null path".into()));
        return -1;
    }
    let path_str = match CStr::from_ptr(path).to_str() {
        Ok(s) => s,
        Err(_) => {
            set_errmsg(errmsg, &MorlocError::Other(
                "mlc_probe_packet: path is not valid UTF-8".into(),
            ));
            return -1;
        }
    };
    use std::io::Read;
    let mut file = match std::fs::File::open(path_str) {
        Ok(f) => f,
        Err(e) => {
            set_errmsg(errmsg, &MorlocError::UserThrow(format!(
                "@open probe: cannot open '{path_str}': {e}"
            )));
            return -1;
        }
    };
    let mut hdr_buf = [0u8; 32];
    if let Err(e) = file.read_exact(&mut hdr_buf) {
        set_errmsg(errmsg, &MorlocError::UserThrow(format!(
            "@open probe: '{path_str}' is not a valid morloc packet (short read: {e})"
        )));
        return -1;
    }
    match morloc_runtime_types::packet::PacketHeader::from_bytes(&hdr_buf) {
        Ok(_) => 0,
        Err(e) => {
            set_errmsg(errmsg, &MorlocError::UserThrow(format!(
                "@open probe: '{path_str}' is not a valid morloc packet: {e}"
            )));
            -1
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn mlc_open(
    path: *const c_char,
    kind: u8,
    errmsg: *mut *mut c_char,
) -> i64 {
    clear_errmsg(errmsg);
    if path.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("mlc_open: null path".into()));
        return -1;
    }
    let path_str = match CStr::from_ptr(path).to_str() {
        Ok(s) => s,
        Err(_) => {
            set_errmsg(errmsg, &MorlocError::Other(
                "mlc_open: path is not valid UTF-8".into(),
            ));
            return -1;
        }
    };
    match crate::stream::open_dispatch(path_str, kind) {
        Ok(handle) => handle,
        Err(e) => {
            set_errmsg(errmsg, &e);
            -1
        }
    }
}

/// `@open path :: <IO> (OStream T)` -- typed open. The codegen has the
/// element schema string for T in hand at compile time and threads it
/// through. The file is created or silently overwritten (`O_CREAT`,
/// UNIX convention -- use `@append` for append semantics), a
/// non-blocking exclusive flock is acquired to reject live concurrent
/// writers, and the stream header is written with the schema metadata
/// block.
#[no_mangle]
pub unsafe extern "C" fn mlc_open_ostream(
    schema_str: *const c_char,
    path: *const c_char,
    errmsg: *mut *mut c_char,
) -> i64 {
    clear_errmsg(errmsg);
    if path.is_null() || schema_str.is_null() {
        set_errmsg(errmsg, &MorlocError::Other(
            "mlc_open_ostream: null path or schema".into(),
        ));
        return -1;
    }
    let path_str = match CStr::from_ptr(path).to_str() {
        Ok(s) => s,
        Err(_) => {
            set_errmsg(errmsg, &MorlocError::Other(
                "mlc_open_ostream: path is not valid UTF-8".into(),
            ));
            return -1;
        }
    };
    let s_str = match CStr::from_ptr(schema_str).to_str() {
        Ok(s) => s,
        Err(_) => {
            set_errmsg(errmsg, &MorlocError::Other(
                "mlc_open_ostream: schema is not valid UTF-8".into(),
            ));
            return -1;
        }
    };
    match crate::stream::shared_open_ostream_with_schema(path_str, s_str) {
        Ok(h) => h,
        Err(e) => {
            set_errmsg(errmsg, &e);
            -1
        }
    }
}

/// `@stdin :: <IO> IStream a` -- typed intrinsic. Element schema is
/// passed by the codegen from `T`. Nexus owns fd 0; this call
/// registers a slot that routes `mlc_next` through the pool-nexus
/// RPC socket. At most one `@stdin` per nexus (enforced via CAS on
/// the registry's stdin claim slot).
#[no_mangle]
pub unsafe extern "C" fn mlc_open_stdin(
    schema_str: *const c_char,
    errmsg: *mut *mut c_char,
) -> i64 {
    open_stdio_ffi(
        schema_str,
        morloc_runtime_types::packet::MLC_KIND_ISTREAM,
        crate::stream::STDIO_KIND_STDIN,
        "mlc_open_stdin",
        errmsg,
    )
}

/// `@stdout :: <IO> OStream a` -- typed intrinsic. Nexus owns fd 1;
/// `mlc_write` routes through the pool-nexus RPC socket. At most one
/// `@stdout` per nexus.
#[no_mangle]
pub unsafe extern "C" fn mlc_open_stdout(
    schema_str: *const c_char,
    errmsg: *mut *mut c_char,
) -> i64 {
    open_stdio_ffi(
        schema_str,
        morloc_runtime_types::packet::MLC_KIND_OSTREAM,
        crate::stream::STDIO_KIND_STDOUT,
        "mlc_open_stdout",
        errmsg,
    )
}

/// `@stderr :: <IO> OStream a` -- typed intrinsic. Nexus owns fd 2;
/// `mlc_write` routes through the pool-nexus RPC socket. At most one
/// `@stderr` per nexus.
#[no_mangle]
pub unsafe extern "C" fn mlc_open_stderr(
    schema_str: *const c_char,
    errmsg: *mut *mut c_char,
) -> i64 {
    open_stdio_ffi(
        schema_str,
        morloc_runtime_types::packet::MLC_KIND_OSTREAM,
        crate::stream::STDIO_KIND_STDERR,
        "mlc_open_stderr",
        errmsg,
    )
}

/// Shared FFI shim behind the three `mlc_open_std{in,out,err}` entry
/// points. Validates the schema string and dispatches to
/// `stream::open_stdio` with the appropriate kind pair.
unsafe fn open_stdio_ffi(
    schema_str: *const c_char,
    kind: u8,
    stdio_kind: u8,
    fn_name: &str,
    errmsg: *mut *mut c_char,
) -> i64 {
    clear_errmsg(errmsg);
    if schema_str.is_null() {
        set_errmsg(errmsg, &MorlocError::Other(format!(
            "{}: null schema", fn_name,
        )));
        return -1;
    }
    let s_str = match CStr::from_ptr(schema_str).to_str() {
        Ok(s) => s,
        Err(_) => {
            set_errmsg(errmsg, &MorlocError::Other(format!(
                "{}: schema is not valid UTF-8", fn_name,
            )));
            return -1;
        }
    };
    match crate::stream::open_stdio(kind, stdio_kind, s_str) {
        Ok(h) => h,
        Err(e) => { set_errmsg(errmsg, &e); -1 }
    }
}

#[no_mangle]
pub unsafe extern "C" fn mlc_close(
    handle: i64,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    match crate::stream::shared_close_handle(handle) {
        Ok(()) => 0,
        Err(e) => {
            set_errmsg(errmsg, &e);
            1
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn mlc_fschema(
    path: *const c_char,
    errmsg: *mut *mut c_char,
) -> *mut c_char {
    clear_errmsg(errmsg);
    if path.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("mlc_fschema: null path".into()));
        return ptr::null_mut();
    }
    let path_str = match CStr::from_ptr(path).to_str() {
        Ok(s) => s,
        Err(_) => {
            set_errmsg(errmsg, &MorlocError::Other(
                "mlc_fschema: path is not valid UTF-8".into(),
            ));
            return ptr::null_mut();
        }
    };
    match crate::stream::read_schema_from_file(path_str) {
        Ok(schema) => match CString::new(schema) {
            Ok(cs) => cs.into_raw(),
            Err(e) => {
                set_errmsg(errmsg, &MorlocError::Other(format!(
                    "mlc_fschema: schema string contains NUL: {}", e
                )));
                ptr::null_mut()
            }
        },
        Err(e) => {
            set_errmsg(errmsg, &e);
            ptr::null_mut()
        }
    }
}

// ── IFile pattern access (unified walker) ─────────────────────────────────

/// One runtime argument to `mlc_ifile_walk`. The `has` byte encodes
/// presence (0 = absent / use default, 1 = use `value`); `value` carries
/// the i64 payload (an index for `.[]` steps; one of start/stop/step for
/// `.[:]` steps). Layout is fixed: `repr(C)` packed into 16 bytes (the
/// 7 padding bytes keep `value` aligned to its natural 8-byte boundary
/// and let per-language wrappers memcpy directly into the array).
#[repr(C)]
#[derive(Copy, Clone)]
pub struct IFileWalkArg {
    pub has: u8,
    pub _pad: [u8; 7],
    pub value: i64,
}

/// Unified IFile pattern walker. The `path` string encodes a single
/// walk-step chain consumed by `crate::stream::ifile_walk`:
///
///   * `.<int>`  - tuple/record-index field step
///   * `.<name>` - record-field-key step
///   * `.[]`     - bracket-index leaf, consumes 1 arg (the index)
///   * `.[:]`    - bracket-slice leaf, consumes 3 args (start, stop, step)
///
/// Field steps consume no runtime args; bracket steps consume their
/// args from `args` in DFS order. Runtime args are presence-tagged
/// (see `IFileWalkArg`): bracket-index requires `has = 1`; bracket-
/// slice accepts any combination of present/absent bounds.
///
/// Returns an AbsPtr to a freshly-allocated SHM block holding the
/// materialized value (with sub-allocations also in SHM). The caller
/// is responsible for freeing via `shfree` -- the active `eval_arena`
/// does this automatically when its scope ends.
#[no_mangle]
pub unsafe extern "C" fn mlc_ifile_walk(
    handle: i64,
    path: *const c_char,
    args_ptr: *const IFileWalkArg,
    n_args: u64,
    errmsg: *mut *mut c_char,
) -> *mut c_void {
    clear_errmsg(errmsg);
    if path.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("mlc_ifile_walk: null path".into()));
        return ptr::null_mut();
    }
    let path_str = match CStr::from_ptr(path).to_str() {
        Ok(s) => s,
        Err(_) => {
            set_errmsg(errmsg, &MorlocError::Other(
                "mlc_ifile_walk: path is not valid UTF-8".into(),
            ));
            return ptr::null_mut();
        }
    };
    let args_slice: &[IFileWalkArg] = if n_args == 0 || args_ptr.is_null() {
        &[]
    } else {
        std::slice::from_raw_parts(args_ptr, n_args as usize)
    };
    match crate::stream::shared_ifile_walk(handle, path_str, args_slice) {
        Ok(p) => p as *mut c_void,
        Err(e) => {
            set_errmsg(errmsg, &e);
            ptr::null_mut()
        }
    }
}

/// `@next` on an IStream handle: materialise the current sub-packet,
/// advance the cursor, return an AbsPtr to a freshly-allocated SHM
/// Array<a> ready to be wrapped by the per-language `from_voidstar`.
/// On EOF returns an Array with size 0 and `data == RELNULL`; the
/// pool surfaces this as an empty list.
#[no_mangle]
pub unsafe extern "C" fn mlc_next(
    handle: i64,
    errmsg: *mut *mut c_char,
) -> *mut c_void {
    clear_errmsg(errmsg);
    match crate::stream::shared_next_subpacket(handle) {
        Ok(p) => p as *mut c_void,
        Err(e) => {
            set_errmsg(errmsg, &e);
            ptr::null_mut()
        }
    }
}

/// `@write level value handle`: append the elements of `value`
/// (a SHM voidstar `Array<T>`) to the OStream's internal SHM buffer.
/// The buffer flushes as one sub-packet when it fills (default 16
/// MiB, overridable via `MORLOC_WRITE_BUFFER_BYTES`), at `@flush`,
/// or at `@close`. The compression level is locked in by the first
/// write and enforced uniform on subsequent writes.
///
/// Element atomicity: a single element wider than the buffer is
/// written as its own oversize sub-packet. Lists that partly fit
/// flush, then resume buffering in the fresh buffer.
#[no_mangle]
pub unsafe extern "C" fn mlc_write(
    level: u8,
    handle: i64,
    payload_voidstar: *const c_void,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    match crate::stream::shared_write_subpacket(handle, level, payload_voidstar as crate::shm::AbsPtr) {
        Ok(()) => 0,
        Err(e) => {
            set_errmsg(errmsg, &e);
            1
        }
    }
}

/// `@flush handle`: force any buffered elements to be written as a
/// sub-packet now, without closing the stream. No-op when the buffer
/// is empty. Useful for tests that need deterministic sub-packet
/// boundaries and for user code wanting to make progress visible to
/// concurrent readers.
#[no_mangle]
pub unsafe extern "C" fn mlc_flush(
    handle: i64,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    match crate::stream::shared_flush_buffer(handle) {
        Ok(()) => 0,
        Err(e) => {
            set_errmsg(errmsg, &e);
            1
        }
    }
}

/// @tell: number of elements written to the process's @stdout OStream so far
/// (0 if none open). Never fails.
#[no_mangle]
pub unsafe extern "C" fn mlc_tell(errmsg: *mut *mut c_char) -> u64 {
    clear_errmsg(errmsg);
    crate::stream::stdout_element_count()
}

/// `@append schema_str path`: open an existing stream file for append.
/// The schema must match the file's stored schema (mismatches error
/// before any bytes are written). Returns a fresh OSTREAM handle whose
/// cursor sits at the resume offset (end of the last complete sub-packet).
#[no_mangle]
pub unsafe extern "C" fn mlc_append(
    schema_str: *const c_char,
    path: *const c_char,
    errmsg: *mut *mut c_char,
) -> i64 {
    clear_errmsg(errmsg);
    if path.is_null() || schema_str.is_null() {
        set_errmsg(errmsg, &MorlocError::Other(
            "mlc_append: null path or schema".into(),
        ));
        return -1;
    }
    let path_str = match CStr::from_ptr(path).to_str() {
        Ok(s) => s,
        Err(_) => {
            set_errmsg(errmsg, &MorlocError::Other(
                "mlc_append: path is not valid UTF-8".into(),
            ));
            return -1;
        }
    };
    let s_str = match CStr::from_ptr(schema_str).to_str() {
        Ok(s) => s,
        Err(_) => {
            set_errmsg(errmsg, &MorlocError::Other(
                "mlc_append: schema is not valid UTF-8".into(),
            ));
            return -1;
        }
    };
    match crate::stream::shared_append_to_path(path_str, s_str) {
        Ok(h) => h,
        Err(e) => {
            set_errmsg(errmsg, &e);
            -1
        }
    }
}

/// `@concat paths dest`: byte-level concat of stream files into `dest`.
#[no_mangle]
pub unsafe extern "C" fn mlc_concat(
    paths: *const *const c_char,
    n_paths: usize,
    dest: *const c_char,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    if dest.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("mlc_concat: null dest".into()));
        return 1;
    }
    let dest_str = match CStr::from_ptr(dest).to_str() {
        Ok(s) => s,
        Err(_) => {
            set_errmsg(errmsg, &MorlocError::Other(
                "mlc_concat: dest is not valid UTF-8".into(),
            ));
            return 1;
        }
    };
    if n_paths == 0 {
        set_errmsg(errmsg, &MorlocError::Other("mlc_concat: empty paths".into()));
        return 1;
    }
    if paths.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("mlc_concat: null paths".into()));
        return 1;
    }
    let mut path_strs: Vec<&str> = Vec::with_capacity(n_paths);
    for i in 0..n_paths {
        let p = *paths.add(i);
        if p.is_null() {
            set_errmsg(errmsg, &MorlocError::Other(format!(
                "mlc_concat: null path at index {}", i
            )));
            return 1;
        }
        match CStr::from_ptr(p).to_str() {
            Ok(s) => path_strs.push(s),
            Err(_) => {
                set_errmsg(errmsg, &MorlocError::Other(format!(
                    "mlc_concat: path at index {} not valid UTF-8", i
                )));
                return 1;
            }
        }
    }
    match crate::stream::concat_files(&path_strs, dest_str) {
        Ok(()) => 0,
        Err(e) => {
            set_errmsg(errmsg, &e);
            1
        }
    }
}

/// `@stream :: IFile a -> <IO> IStream a`: open a fresh IStream slot
/// bound to the same path as the source IFile.
#[no_mangle]
pub unsafe extern "C" fn mlc_stream(
    ifile_handle: i64,
    errmsg: *mut *mut c_char,
) -> i64 {
    clear_errmsg(errmsg);
    match crate::stream::shared_derive_istream(ifile_handle) {
        Ok(h) => h,
        Err(e) => {
            set_errmsg(errmsg, &e);
            -1
        }
    }
}

/// Total element count of an IFile (`length f`). Cheap: reads the
/// cached value from the StreamDiag block populated at open time.
/// Errors when the file's value type is not a list (the length of a
/// tuple/record IFile is meaningless).
#[no_mangle]
pub unsafe extern "C" fn mlc_ifile_length(
    handle: i64,
    errmsg: *mut *mut c_char,
) -> i64 {
    clear_errmsg(errmsg);
    match crate::stream::shared_handle_length(handle) {
        Ok(n) => n as i64,
        Err(e) => {
            set_errmsg(errmsg, &e);
            -1
        }
    }
}

// ── Cross-pool wire codec for stream handles ──────────────────────────────
//
// The SHM stream registry stores each handle's identity (path, schema,
// cursor, etc.) but the file descriptor + mmap region are per-process
// and not shareable across forks. So to pass a handle across a pool
// boundary the sender's bridge calls `mlc_handle_pack_path` to read out
// the file path (and kind), the path travels on the wire as a UTF-8
// string, and the receiver's bridge calls `mlc_handle_unpack_path` to
// bind a fresh local handle via `open_dispatch`. Each pool's
// `eval_arena` closes the handle it opened on scope exit; the sender's
// `@close` does not affect the receiver, and vice versa.
//
// Return convention: `*mut c_char` is a malloc'd, NUL-terminated UTF-8
// string owned by the caller. The caller frees with libc::free. Errors
// return NULL and set errmsg.

#[no_mangle]
pub unsafe extern "C" fn mlc_handle_pack_path(
    handle: i64,
    out_kind: *mut u8,
    errmsg: *mut *mut c_char,
) -> *mut c_char {
    clear_errmsg(errmsg);
    let kind = match crate::stream::shared_handle_kind(handle) {
        Ok(k) => k,
        Err(e) => {
            set_errmsg(errmsg, &e);
            return ptr::null_mut();
        }
    };
    let path = match crate::stream::shared_handle_path(handle) {
        Ok(p) => p,
        Err(e) => {
            set_errmsg(errmsg, &e);
            return ptr::null_mut();
        }
    };
    if !out_kind.is_null() {
        *out_kind = kind;
    }
    match CString::new(path) {
        Ok(cs) => libc::strdup(cs.as_ptr()),
        Err(_) => {
            set_errmsg(errmsg, &MorlocError::Other(
                "mlc_handle_pack_path: path contains NUL byte".into(),
            ));
            ptr::null_mut()
        }
    }
}

// Suballoc-cost lookup for the per-language `get_shm_size` sizing pass on
// embedded stream-handle fields. Since bridge codecs now uniformly emit
// TAG_HANDLE (the intra-nexus fast path -- bare slot id in the inline
// 16-byte field, no suballoc), every kind returns 0. The cross-nexus
// TAG_PATH rewrite happens at the SLURM boundary via
// `handle_scan::rewrite_data_packet_for_persistence`, not through the bridge
// codec. Returns -1 on error (kept for ABI parity with the earlier
// variant).
#[no_mangle]
pub unsafe extern "C" fn mlc_handle_path_len(
    _handle: i64,
    errmsg: *mut *mut c_char,
) -> i64 {
    clear_errmsg(errmsg);
    0
}

#[no_mangle]
pub unsafe extern "C" fn mlc_handle_unpack_path(
    path: *const c_char,
    kind: u8,
    errmsg: *mut *mut c_char,
) -> i64 {
    clear_errmsg(errmsg);
    if path.is_null() {
        set_errmsg(errmsg, &MorlocError::Other(
            "mlc_handle_unpack_path: null path".into(),
        ));
        return -1;
    }
    let path_str = match CStr::from_ptr(path).to_str() {
        Ok(s) => s,
        Err(_) => {
            set_errmsg(errmsg, &MorlocError::Other(
                "mlc_handle_unpack_path: path is not valid UTF-8".into(),
            ));
            return -1;
        }
    };
    // Route through the shared-registry `open_dispatch`. The fresh
    // slot's `call_id` is tagged from the thread-local set by the
    // daemon dispatch loop, so the per-call sweeper cleans it up at
    // scope exit. (Replaces the prior `eval_arena::record_slot_if_active`
    // bookkeeping under the process-local registry.)
    match crate::stream::open_dispatch(path_str, kind) {
        Ok(handle) => handle,
        Err(e) => {
            set_errmsg(errmsg, &e);
            -1
        }
    }
}

// ── Voidstar wire-layout helpers for stream handles ───────────────────────
//
// A stream-handle field on the wire is the 16-byte tagged form documented
// in `morloc_runtime_types::stream_handle`. The tag byte at offset 0
// picks the encoding (`TAG_PATH = 0`: payload is a relptr to a path
// suballoc; `TAG_HANDLE = 1`: payload is a bare slot id). Per-language
// bridges call the entries below to lay down or read back the field
// without hand-rolling the byte layout.

/// Write a stream-handle field at `dest` using `tag` to pick the
/// encoding. For `TAG_PATH`, the path bytes (with a `u64` length prefix)
/// are written into the suballoc region at `*cursor` and `*cursor` is
/// advanced. For `TAG_HANDLE`, only the inline field is touched.
///
/// `dest` points at a 16-byte stream-handle field slot. `cursor` points
/// at the bridge's voidstar suballoc cursor; only consulted when
/// `tag == TAG_PATH`. Returns 0 on success, 1 on error.
#[no_mangle]
pub unsafe extern "C" fn mlc_write_stream_field(
    handle: i64,
    dest: *mut c_void,
    cursor: *mut *mut c_void,
    tag: u8,
    errmsg: *mut *mut c_char,
) -> i32 {
    use crate::shm;
    use morloc_runtime_types::stream_handle as sh;
    clear_errmsg(errmsg);
    if dest.is_null() {
        set_errmsg(errmsg, &MorlocError::Other(
            "mlc_write_stream_field: null dest".into(),
        ));
        return 1;
    }
    let field_ptr = dest as *mut u8;
    match tag {
        sh::TAG_PATH => {
            if cursor.is_null() {
                set_errmsg(errmsg, &MorlocError::Other(
                    "mlc_write_stream_field: null cursor for TAG_PATH".into(),
                ));
                return 1;
            }
            let path = match crate::stream::handle_path(handle) {
                Ok(p) => p,
                Err(e) => {
                    set_errmsg(errmsg, &e);
                    return 1;
                }
            };
            let path_bytes = path.as_bytes();
            if path_bytes.is_empty() {
                sh::write_field(field_ptr, sh::TAG_PATH, sh::RELNULL_PAYLOAD);
                return 0;
            }
            let cur = *cursor as *mut u8;
            let rel = match shm::abs2rel(cur) {
                Ok(r) => r,
                Err(e) => {
                    set_errmsg(errmsg, &e);
                    return 1;
                }
            };
            sh::write_path_suballoc(cur, path_bytes);
            sh::write_field(field_ptr, sh::TAG_PATH, rel as u64);
            *cursor = cur.add(sh::path_suballoc_size(path_bytes.len()))
                as *mut c_void;
            0
        }
        sh::TAG_HANDLE => {
            sh::write_field(field_ptr, sh::TAG_HANDLE, handle as u64);
            0
        }
        _ => {
            set_errmsg(errmsg, &MorlocError::Other(format!(
                "mlc_write_stream_field: unsupported tag {}", tag,
            )));
            1
        }
    }
}

/// Legacy entry point: writes a stream-handle field. Always emits
/// `TAG_HANDLE` (the intra-nexus fast path -- bare slot id, no path
/// suballoc). This is correct for every pool-to-pool call within one
/// nexus because both sides attach to the same SHM registry.
///
/// Packets that cross a nexus boundary (SLURM egress in
/// `slurm_ffi::remote_call`) are rewritten to `TAG_PATH` form by
/// `handle_scan::rewrite_data_packet_for_persistence` before they leave the
/// local registry -- the boundary is a much better place to know
/// "does the receiver share my SHM?" than every bridge marshal site.
///
/// Forwarded to `mlc_write_stream_field` until per-language bridges
/// migrate to call that directly with an explicit tag.
#[no_mangle]
pub unsafe extern "C" fn mlc_write_handle_voidstar(
    handle: i64,
    dest: *mut c_void,
    cursor: *mut *mut c_void,
    errmsg: *mut *mut c_char,
) -> i32 {
    use morloc_runtime_types::stream_handle as sh;
    mlc_write_stream_field(handle, dest, cursor, sh::TAG_HANDLE, errmsg)
}

// ── Batched variants for [IFile a] arrays ─────────────────────────────────
//
// Per-element `mlc_handle_path_len` and `mlc_write_handle_voidstar` acquire
// the registry mutex N times for an N-handle vector. These batched entries
// fold the lock into one acquisition for the whole array, and also drop
// the per-element `String::clone()` that the singular path lookups have
// to take (we only ever need `.len()` or `.as_bytes()`).
//
// The bridges use these for the array overloads of get_shm_size and
// to_voidstar; the singular path stays for cases where the handle is
// embedded inside a wider record.

#[no_mangle]
pub unsafe extern "C" fn mlc_handles_path_lens(
    handles: *const i64,
    n: usize,
    out_lens: *mut i64,
    errmsg: *mut *mut c_char,
) -> i64 {
    clear_errmsg(errmsg);
    if n == 0 {
        return 0;
    }
    if handles.is_null() {
        set_errmsg(errmsg, &MorlocError::Other(
            "mlc_handles_path_lens: null handles".into(),
        ));
        return -1;
    }
    let hs = std::slice::from_raw_parts(handles, n);
    let outs = if out_lens.is_null() {
        None
    } else {
        Some(std::slice::from_raw_parts_mut(out_lens, n))
    };
    match crate::stream::shared_handles_path_lens(hs, outs) {
        Ok(sum) => sum as i64,
        Err(e) => {
            set_errmsg(errmsg, &e);
            -1
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn mlc_write_handles_voidstar(
    handles: *const i64,
    n: usize,
    dest: *mut c_void,
    elem_stride: usize,
    cursor: *mut *mut c_void,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    if n == 0 {
        return 0;
    }
    if handles.is_null() || dest.is_null() || cursor.is_null() {
        set_errmsg(errmsg, &MorlocError::Other(
            "mlc_write_handles_voidstar: null handles, dest, or cursor".into(),
        ));
        return 1;
    }
    let hs = std::slice::from_raw_parts(handles, n);
    let mut cur = *cursor as *mut u8;
    match crate::stream::shared_write_handles_voidstar(hs, dest as *mut u8, elem_stride, &mut cur) {
        Ok(()) => {
            *cursor = cur as *mut c_void;
            0
        }
        Err(e) => {
            set_errmsg(errmsg, &e);
            1
        }
    }
}

/// Read a stream-handle field from voidstar and produce a local handle.
///
/// `field_ptr` points at the 16-byte stream-handle field. `base_ptr` is
/// the relptr base for `TAG_PATH` payloads: NULL means the payload is
/// SHM-relative; non-NULL means payload is a base-relative offset
/// (used when reading from mmap'd file regions). `kind` selects the
/// receiver's open kind (`MLC_KIND_IFILE` / `MLC_KIND_OSTREAM` /
/// `MLC_KIND_ISTREAM`) when a `TAG_PATH` field has to be opened locally.
///
/// On `TAG_HANDLE`, the bare slot id in the payload is returned after
/// generation-checking it against the local SHM registry — a foreign
/// nexus's handle that accidentally arrives here surfaces as a clean
/// generation-mismatch error.
///
/// Returns -1 and sets errmsg on any failure.
#[no_mangle]
pub unsafe extern "C" fn mlc_read_stream_field(
    field_ptr: *const c_void,
    base_ptr: *const c_void,
    kind: u8,
    errmsg: *mut *mut c_char,
) -> i64 {
    use crate::shm;
    use morloc_runtime_types::stream_handle as sh;
    clear_errmsg(errmsg);
    if field_ptr.is_null() {
        set_errmsg(errmsg, &MorlocError::Other(
            "mlc_read_stream_field: null field_ptr".into(),
        ));
        return -1;
    }
    let field_bytes = field_ptr as *const u8;
    let tag = sh::read_tag(field_bytes);
    let payload = sh::read_payload(field_bytes);
    match tag {
        sh::TAG_PATH => {
            if payload == sh::RELNULL_PAYLOAD {
                set_errmsg(errmsg, &MorlocError::Other(
                    "mlc_read_stream_field: path-form field has RELNULL payload".into(),
                ));
                return -1;
            }
            // Resolve the suballoc {size: u64, bytes...}. With a non-NULL
            // base_ptr the payload is a payload-relative offset (used
            // when reading from mmap'd file regions); otherwise it's an
            // SHM-relative relptr.
            let suballoc_abs: *const u8 = if base_ptr.is_null() {
                match shm::rel2abs(payload as shm::RelPtr) {
                    Ok(p) => p as *const u8,
                    Err(e) => {
                        set_errmsg(errmsg, &e);
                        return -1;
                    }
                }
            } else {
                (base_ptr as *const u8).add(payload as usize)
            };
            let path_len = sh::read_path_size(suballoc_abs);
            const PATH_MAX: u64 = 4096;
            if path_len == 0 {
                set_errmsg(errmsg, &MorlocError::Other(
                    "mlc_read_stream_field: empty path".into(),
                ));
                return -1;
            }
            if path_len >= PATH_MAX {
                set_errmsg(errmsg, &MorlocError::Other(format!(
                    "mlc_read_stream_field: path too long ({} bytes, max {})",
                    path_len, PATH_MAX - 1,
                )));
                return -1;
            }
            let path_bytes = std::slice::from_raw_parts(
                suballoc_abs.add(8), path_len as usize,
            );
            let path_str = match std::str::from_utf8(path_bytes) {
                Ok(s) => s,
                Err(_) => {
                    set_errmsg(errmsg, &MorlocError::Other(
                        "mlc_read_stream_field: path is not valid UTF-8".into(),
                    ));
                    return -1;
                }
            };
            match crate::stream::open_dispatch(path_str, kind) {
                Ok(handle) => {
                    crate::eval_arena::record_slot_if_active(handle);
                    handle
                }
                Err(e) => {
                    set_errmsg(errmsg, &e);
                    -1
                }
            }
        }
        sh::TAG_HANDLE => {
            // Intra-nexus shared-slot fast path. Verify the slot is live
            // at the claimed generation; anything else means the handle
            // crossed a nexus boundary it shouldn't have (or the slot
            // was reused), and we surface a generation-mismatch error
            // instead of returning a wild value.
            let handle = payload as i64;
            let (gen_claim, slot_idx) = crate::stream::unpack_handle(handle);
            let slot = match crate::stream::slot_ref(slot_idx) {
                Some(s) => s,
                None => {
                    set_errmsg(errmsg, &MorlocError::Other(format!(
                        "mlc_read_stream_field: handle-form payload references \
                         slot {} which is out of range in this registry; the \
                         handle likely originated in a different nexus",
                         slot_idx,
                    )));
                    return -1;
                }
            };
            use std::sync::atomic::Ordering;
            let gen_now = slot.generation.load(Ordering::Acquire)
                & crate::stream::GENERATION_MASK;
            if gen_now != gen_claim {
                set_errmsg(errmsg, &MorlocError::Other(format!(
                    "mlc_read_stream_field: handle-form generation mismatch \
                     (claim {}, slot {}); handle is stale or from another nexus",
                     gen_claim, gen_now,
                )));
                return -1;
            }
            handle
        }
        _ => {
            set_errmsg(errmsg, &MorlocError::Other(format!(
                "mlc_read_stream_field: unsupported encoding tag {}", tag,
            )));
            -1
        }
    }
}

/// Legacy entry point: reads a stream-handle field assumed to be in path
/// form. Forwards to `mlc_read_stream_field`. The tag check in the new
/// codec means a TAG_HANDLE field passed through this entry still works;
/// the name "handle_voidstar" is historical.
#[no_mangle]
pub unsafe extern "C" fn mlc_read_handle_voidstar(
    arr: *const c_void,
    base_ptr: *const c_void,
    kind: u8,
    errmsg: *mut *mut c_char,
) -> i64 {
    mlc_read_stream_field(arr, base_ptr, kind, errmsg)
}

// ── stdio-server bridge FFI ────────────────────────────────────────────────

/// Return the `STDIO_KIND_*` byte for a stdio-bound slot. Used by the
/// nexus stdio server to route a `WRITE_STDIO` request to fd 1 or fd 2.
///
/// Returns 0 on success (kind byte written to `*kind_out`), -1 if the
/// handle is not stdio-bound (no errmsg), or a positive error code
/// with `*errmsg` set for any other failure.
#[no_mangle]
pub unsafe extern "C" fn mlc_stdio_slot_kind(
    handle: i64,
    kind_out: *mut u8,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    match crate::stream::shared_handle_stdio_kind(handle) {
        Ok(Some(k)) => { *kind_out = k; 0 }
        Ok(None) => -1,
        Err(e) => { set_errmsg(errmsg, &e); 1 }
    }
}

/// Build a MORLOC_STREAM_PACKET prefix (32-byte header + SCHEMA_STRING
/// metadata block) from a stdio OStream slot's cached element schema.
/// Used by the nexus stdio server on the first `WRITE_STDIO` per slot
/// to emit the stream header before any sub-packet bytes flow.
///
/// On success returns 0 and sets `*buf_out` to a libc::malloc-owned
/// buffer of `*len_out` bytes; caller frees via `libc::free`. Returns
/// non-zero and sets `*errmsg` on failure.
#[no_mangle]
pub unsafe extern "C" fn mlc_stdio_build_stream_header(
    handle: i64,
    buf_out: *mut *mut u8,
    len_out: *mut usize,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    *buf_out = ptr::null_mut();
    *len_out = 0;
    let schema_str = match crate::stream::shared_handle_schema_str(handle) {
        Ok(s) => s,
        Err(e) => { set_errmsg(errmsg, &e); return 1; }
    };
    let schema = match morloc_runtime_types::schema::parse_schema(&schema_str) {
        Ok(s) => s,
        Err(e) => {
            set_errmsg(errmsg, &MorlocError::Other(format!(
                "mlc_stdio_build_stream_header: parse_schema({:?}): {}",
                schema_str, e,
            )));
            return 1;
        }
    };
    let bytes = morloc_runtime_types::packet::make_stream_header_block(&schema);
    let n = bytes.len();
    let dst = libc::malloc(n) as *mut u8;
    if dst.is_null() {
        set_errmsg(errmsg, &MorlocError::Other(
            "mlc_stdio_build_stream_header: malloc failed".into(),
        ));
        return 1;
    }
    std::ptr::copy_nonoverlapping(bytes.as_ptr(), dst, n);
    *buf_out = dst;
    *len_out = n;
    0
}

/// Return a stdio slot's cached value-schema string as a
/// libc::malloc'd NUL-terminated buffer. Streams are list-shaped so
/// this is the full `[a]` schema. Used by the nexus stdio server to
/// validate that an incoming STREAM_PACKET on stdin commits to the
/// same wire type the opener declared.
///
/// Returns 0 on success; caller frees `*out_buf` via libc::free.
/// Returns non-zero and sets errmsg on failure.
#[no_mangle]
pub unsafe extern "C" fn mlc_stdio_slot_schema(
    handle: i64,
    out_buf: *mut *mut c_char,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    *out_buf = ptr::null_mut();
    let schema_str = match crate::stream::shared_handle_schema_str(handle) {
        Ok(s) => s,
        Err(e) => { set_errmsg(errmsg, &e); return 1; }
    };
    let bytes = schema_str.as_bytes();
    let n = bytes.len();
    let dst = libc::malloc(n + 1) as *mut c_char;
    if dst.is_null() {
        set_errmsg(errmsg, &MorlocError::Other(
            "mlc_stdio_slot_schema: malloc failed".into(),
        ));
        return 1;
    }
    std::ptr::copy_nonoverlapping(bytes.as_ptr() as *const c_char, dst, n);
    *dst.add(n) = 0;
    *out_buf = dst;
    0
}

// ── `morloc-nexus view` conversion helpers ────────────────────────────────

/// Convert a MORLOC_STREAM_PACKET file to another MORLOC_STREAM_PACKET
/// file. Backs `morloc-nexus view -s` on stream input. `schema_override`
/// may be null (use the input's stored element schema) or point to a
/// UTF-8 schema string. `compression_level` in [0, 22] selects the
/// output's per-sub-packet zstd level (0 = uncompressed).
///
/// Writes the sub-packet count to `*out_subpackets` on success.
/// Returns 0 on success, non-zero on failure with `errmsg` set.
#[no_mangle]
pub unsafe extern "C" fn mlc_view_stream_to_stream(
    in_path: *const c_char,
    out_path: *const c_char,
    compression_level: u8,
    schema_override: *const c_char,
    out_subpackets: *mut u64,
    errmsg: *mut *mut c_char,
) -> i32 {
    const FN: &str = "mlc_view_stream_to_stream";
    wrap_c_call(errmsg, 1, || {
        let in_str = cstr_arg(in_path, FN, "in_path")?;
        let out_str = cstr_arg(out_path, FN, "out_path")?;
        let schema_opt = if schema_override.is_null() {
            None
        } else {
            Some(cstr_arg(schema_override, FN, "schema")?)
        };
        let n = crate::stream::shared_view_stream_to_stream(
            in_str, out_str, compression_level, schema_opt,
        )?;
        if !out_subpackets.is_null() {
            *out_subpackets = n;
        }
        Ok(0)
    })
}

/// Convert a MORLOC_DATA_PACKET file to a MORLOC_STREAM_PACKET file.
/// Backs `morloc-nexus view -s` on data-packet input. Opens the input
/// as an IFile (refuses compressed DATA -- matches the runtime's own
/// IFile-open invariant), chunks the array via bracket-slice, and
/// writes each chunk to a fresh OStream.
///
/// Writes the sub-packet count to `*out_subpackets` on success.
/// Returns 0 on success, non-zero on failure with `errmsg` set.
#[no_mangle]
pub unsafe extern "C" fn mlc_view_data_to_stream(
    in_path: *const c_char,
    out_path: *const c_char,
    compression_level: u8,
    out_subpackets: *mut u64,
    errmsg: *mut *mut c_char,
) -> i32 {
    const FN: &str = "mlc_view_data_to_stream";
    wrap_c_call(errmsg, 1, || {
        let in_str = cstr_arg(in_path, FN, "in_path")?;
        let out_str = cstr_arg(out_path, FN, "out_path")?;
        let n = crate::stream::shared_view_data_to_stream(
            in_str, out_str, compression_level,
        )?;
        if !out_subpackets.is_null() {
            *out_subpackets = n;
        }
        Ok(0)
    })
}

/// Open a footer-less MORLOC_STREAM_PACKET file as an IFile, trusting
/// caller-supplied forward-scan output. Backs `morloc-nexus view
/// --pattern` on a truncated stream: the caller runs a forward scan
/// (either via `morloc_runtime_types::packet::forward_scan_subpackets`
/// or the nexus-side seek+read walker) and hands the offsets +
/// element_count here.
///
/// If the file has a clean final footer, this function delegates to
/// `mlc_open_ifile` and ignores the caller's offsets -- the on-disk
/// footer is authoritative.
#[no_mangle]
pub unsafe extern "C" fn mlc_open_ifile_recovered(
    path: *const c_char,
    subpacket_offsets: *const u64,
    subpacket_counts: *const u64,
    n_offsets: u64,
    element_count: u64,
    errmsg: *mut *mut c_char,
) -> i64 {
    const FN: &str = "mlc_open_ifile_recovered";
    wrap_c_call(errmsg, -1, || {
        let path_str = cstr_arg(path, FN, "path")?;
        let offsets_slice: &[u64] = if n_offsets == 0 || subpacket_offsets.is_null() {
            &[]
        } else {
            std::slice::from_raw_parts(subpacket_offsets, n_offsets as usize)
        };
        let counts_slice: &[u64] = if n_offsets == 0 || subpacket_counts.is_null() {
            &[]
        } else {
            std::slice::from_raw_parts(subpacket_counts, n_offsets as usize)
        };
        crate::stream::shared_open_ifile_recovered(
            path_str, offsets_slice, counts_slice, element_count,
        )
    })
}

/// Materialise a stream sub-packet from a caller-supplied byte buffer
/// into a fresh SHM voidstar `Array<T>`. Given the sub-packet's full
/// bytes (32-byte header + metadata + payload) and its element
/// schema, returns the SHM AbsPtr to the newly-allocated array. The
/// caller owns the block and is responsible for freeing via
/// `mlc_shfree` (or the active `eval_arena` will reclaim it on scope
/// drop).
///
/// Used by `morloc-nexus view -` when the input is a
/// MORLOC_STREAM_PACKET on stdin: nexus reads one sub-packet at a
/// time from fd 0, calls this to materialise each into a voidstar,
/// and hands the voidstar to the appropriate emitter
/// (`print_voidstar_jsonl`, `mlc_write`, `mlc_save_voidstar`).
///
/// Handles the same compression byte cases as
/// `materialize_subpacket_at_offset`'s mmap-based path
/// (`PACKET_COMPRESSION_NONE` and `PACKET_COMPRESSION_ZSTD`).
#[no_mangle]
pub unsafe extern "C" fn mlc_materialize_subpacket_from_bytes(
    bytes: *const u8,
    n: u64,
    elem_schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut c_void {
    const FN: &str = "mlc_materialize_subpacket_from_bytes";
    wrap_c_call(errmsg, ptr::null_mut(), || {
        if bytes.is_null() {
            return Err(MorlocError::Other(format!("{}: null bytes", FN)));
        }
        if elem_schema.is_null() {
            return Err(MorlocError::Other(format!("{}: null schema", FN)));
        }
        let slice = std::slice::from_raw_parts(bytes, n as usize);
        let rs = CSchema::to_rust(elem_schema);
        let p = crate::stream::shared_materialize_subpacket_from_bytes(slice, &rs)?;
        Ok(p as *mut c_void)
    })
}
