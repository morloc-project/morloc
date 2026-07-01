//! Intrinsic functions for morloc: save/load/show/read/hash.
//! Replaces intrinsics.c. These are thin wrappers around serialization functions.

use std::ffi::{c_char, c_void, CStr, CString};
use std::ptr;

use crate::cschema::CSchema;
use crate::error::{clear_errmsg, set_errmsg, MorlocError};

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

// ── mlc_save_voidstar: serialize to binary voidstar packet file ────────────

// On disk: 32-byte morloc packet header + flattened voidstar payload. When
// `level > 0` the payload region is zstd-compressed and the header's
// compression byte is set to PACKET_COMPRESSION_ZSTD; level == 0 writes
// the payload uncompressed.
#[no_mangle]
pub unsafe extern "C" fn mlc_save_voidstar(
    data: *const c_void,
    schema: *const CSchema,
    level: u8,
    path: *const c_char,
    errmsg: *mut *mut c_char,
) -> i32 {
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

    // Resolve the compression level upfront so a bad level aborts before
    // we touch the filesystem.
    let clvl = match crate::compression::CompressionLevel::from_u8(level) {
        Ok(lvl) => lvl,
        Err(e) => { set_errmsg(errmsg, &e); return 1; }
    };

    // Get directory for temp file
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

    // Write packet header placeholder
    let header_size = std::mem::size_of::<crate::packet::PacketHeader>();
    let zeros = vec![0u8; header_size];
    if write_binary_fd(fd, zeros.as_ptr() as *const c_char, header_size, &mut err) != 0 {
        libc::close(fd);
        libc::unlink(tmp_buf.as_ptr() as *const c_char);
        *errmsg = err;
        return 1;
    }

    // Flatten voidstar
    let mut blob: *mut u8 = ptr::null_mut();
    let mut blob_size: usize = 0;
    if flatten_voidstar_to_buffer(data, schema, &mut blob, &mut blob_size, &mut err) != 0 {
        libc::close(fd);
        libc::unlink(tmp_buf.as_ptr() as *const c_char);
        *errmsg = err;
        return 1;
    }

    // Build the SCHEMA_STRING metadata entry. Self-describing files
    // let IFile (and tooling like `morloc-nexus file`) read the payload
    // schema directly. We always emit this -- the size cost is tiny
    // and makes saved files unambiguous to inspect after the fact.
    let schema_rs = crate::cschema::CSchema::to_rust(schema);
    let schema_str =
        morloc_runtime_types::schema::schema_to_string(&schema_rs);
    let mut schema_body = schema_str.as_bytes().to_vec();
    schema_body.push(0); // NUL-terminator matches read_schema_from_meta's
                        // decode_schema_entry expectation.
    let base_meta = crate::packet::append_metadata_entry(
        &[],
        crate::packet::METADATA_TYPE_SCHEMA_STRING,
        &schema_body,
    );

    // If a compression level is selected, swap the flattened buffer for
    // its zstd multi-frame form before writing the payload region. The
    // raw blob is freed either way; the compressed bytes live in `comp`
    // for the rest of the function when level > 0. The frame index is
    // appended to the metadata block (after SCHEMA_STRING) between the
    // header and the payload. The block is padded to
    // METADATA_BLOCK_ALIGNMENT so the payload starts on the same
    // aligned offset other emit paths use (32-byte alignment).
    struct CompressedOut {
        bytes: Vec<u8>,
        metadata_block: Vec<u8>,
    }
    let comp: Option<CompressedOut> = if clvl.is_none() {
        None
    } else {
        let raw_slice = std::slice::from_raw_parts(blob as *const u8, blob_size);
        match crate::compression::compress_payload_zstd(raw_slice, clvl) {
            Ok((bytes, frames)) => {
                let body = crate::packet::encode_frame_index_entry(&frames);
                let metadata_block = crate::packet::append_metadata_entry(
                    &base_meta,
                    crate::packet::METADATA_TYPE_FRAME_INDEX,
                    &body,
                );
                Some(CompressedOut { bytes, metadata_block })
            }
            Err(e) => {
                libc::free(blob as *mut c_void);
                libc::close(fd);
                libc::unlink(tmp_buf.as_ptr() as *const c_char);
                set_errmsg(errmsg, &e);
                return 1;
            }
        }
    };

    let (payload_ptr, payload_len, compression_byte, metadata_block): (*const u8, usize, u8, &[u8]) =
        match &comp {
            Some(c) => (
                c.bytes.as_ptr(),
                c.bytes.len(),
                crate::packet::PACKET_COMPRESSION_ZSTD,
                c.metadata_block.as_slice(),
            ),
            None => (
                blob as *const u8,
                blob_size,
                crate::packet::PACKET_COMPRESSION_NONE,
                base_meta.as_slice(),
            ),
        };
    let metadata_size = metadata_block.len();

    // Metadata (SCHEMA_STRING + optional FRAME_INDEX + padding) sits
    // between the header placeholder and the payload.
    if !metadata_block.is_empty() {
        if write_binary_fd(
            fd,
            metadata_block.as_ptr() as *const c_char,
            metadata_block.len(),
            &mut err,
        ) != 0 {
            libc::free(blob as *mut c_void);
            libc::close(fd);
            libc::unlink(tmp_buf.as_ptr() as *const c_char);
            *errmsg = err;
            return 1;
        }
    }

    if write_binary_fd(fd, payload_ptr as *const c_char, payload_len, &mut err) != 0 {
        libc::free(blob as *mut c_void);
        drop(comp);
        libc::close(fd);
        libc::unlink(tmp_buf.as_ptr() as *const c_char);
        *errmsg = err;
        return 1;
    }
    libc::free(blob as *mut c_void);
    drop(comp);

    // Seek back and write the real header. data_mesg fills in everything
    // except the compression byte; patch byte 15 in the serialized header
    // when level > 0 to record PACKET_COMPRESSION_ZSTD. (Byte 15 is the
    // data-command compression slot: 4 magic + 2 plain + 2 version + 2
    // flavor + 2 mode + 1 cmd_type + 1 source + 1 format = 15.)
    libc::lseek(fd, 0, libc::SEEK_SET);
    let mut header = crate::packet::PacketHeader::data_mesg(
        crate::packet::PACKET_FORMAT_VOIDSTAR,
        payload_len as u64,
    );
    header.offset = metadata_size as u32;
    let mut hdr_bytes = header.to_bytes();
    hdr_bytes[15] = compression_byte;
    write_binary_fd(fd, hdr_bytes.as_ptr() as *const c_char, hdr_bytes.len(), &mut err);

    libc::fsync(fd);
    libc::close(fd);

    // Atomic rename
    if libc::rename(tmp_buf.as_ptr() as *const c_char, path) != 0 {
        libc::unlink(tmp_buf.as_ptr() as *const c_char);
        set_errmsg(errmsg, &MorlocError::Io(std::io::Error::last_os_error()));
        return 1;
    }

    0
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

    // Layer 2 fast path: large MESG+VOIDSTAR packets `pread` straight
    // into a fresh SHM allocation, skipping the libc::malloc + read +
    // shmalloc + memcpy chain.
    match crate::cli::try_load_voidstar_packet_via_mmap(path, schema) {
        Ok(Some(ptr)) => return ptr as *mut c_void,
        Ok(None) => { /* fall through */ }
        Err(e) => {
            let path_str = CStr::from_ptr(path).to_string_lossy();
            eprintln!("@load warning ({}): {}", path_str, e);
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
            let path_str = CStr::from_ptr(path).to_string_lossy();
            eprintln!("@load warning ({}): {}", path_str, e);
            return ptr::null_mut();
        }
    }

    let mut err: *mut c_char = ptr::null_mut();
    let mut file_size: usize = 0;
    let data = read_binary_file(path, &mut file_size, &mut err);
    if data.is_null() {
        if !err.is_null() {
            let path_str = CStr::from_ptr(path).to_string_lossy();
            let err_str = CStr::from_ptr(err).to_string_lossy();
            eprintln!("@load warning ({}): {}", path_str, err_str);
            libc::free(err as *mut libc::c_void);
        }
        return ptr::null_mut();
    }

    // Decompression is now handled inside `load_morloc_data_file`,
    // which is the single choke point all packet-load paths share.
    let result = load_morloc_data_file(path, data, file_size, schema, &mut err);
    if result.is_null() && !err.is_null() {
        let path_str = CStr::from_ptr(path).to_string_lossy();
        let err_str = CStr::from_ptr(err).to_string_lossy();
        eprintln!("@load warning ({}): {}", path_str, err_str);
        libc::free(err as *mut libc::c_void);
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
/// through. The file is created (`O_CREAT | O_EXCL`), flock-acquired,
/// and the stream header is written with the schema metadata block.
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
// `handle_scan::rewrite_data_packet_for_remote`, not through the bridge
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
/// `handle_scan::rewrite_data_packet_for_remote` before they leave the
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
