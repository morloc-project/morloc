//! CLI argument handling and voidstar utility functions.
//! Replaces cli.c.

use std::ffi::{c_char, c_void, CStr};
use std::ptr;
use std::sync::atomic::{AtomicBool, Ordering};

use crate::cschema::CSchema;
use crate::error::{clear_errmsg, set_errmsg, MorlocError};
use crate::packet;
use crate::shm;

/// Extensions that mark an argument as path-shaped for the
/// "looks-like-a-path" early-failure check (in addition to the
/// `/` separator). A superset of the extensions
/// [`load_morloc_data_file`] dispatches on by name (`.json`,
/// `.mpk`, `.msgpack`, `.csv`, `.tsv`); `.arrow` and `.parquet`
/// are recognized as paths here even though that loader detects
/// them by magic bytes instead of extension.
pub const DATA_FILE_EXTENSIONS: &[&str] = &[
    ".json", ".mpk", ".msgpack", ".csv", ".tsv", ".arrow", ".parquet",
];

/// True if `arg` has a recognized data-file extension or contains a
/// path separator AND is not unambiguously inline JSON content. Used
/// to decide whether a non-existent argument was a path typo (hard
/// error) or a literal value (treat as inline).
///
/// JSON-content prefixes (`"` `{` `[` digit `-digit`) always win: a
/// JSON-quoted string like `"path/to/file"` is a string literal for
/// the JSON parser to handle, not a path the CLI should resolve.
fn arg_looks_like_file_path(arg: &str) -> bool {
    let trimmed = arg.trim_start();
    let mut it = trimmed.chars();
    match it.next() {
        Some('"') | Some('{') | Some('[') => return false,
        Some(c) if c.is_ascii_digit() => return false,
        Some('-') => {
            // `-<digit>` is a negative-number literal; other `-...`
            // (e.g. `--flag`) is not a path and shouldn't trigger
            // the not-found error either.
            return false;
        }
        _ => {}
    }
    if arg.contains('/') || arg.contains('\\') { return true; }
    DATA_FILE_EXTENSIONS.iter().any(|ext| {
        arg.len() >= ext.len()
            && arg.as_bytes()[arg.len() - ext.len()..].eq_ignore_ascii_case(ext.as_bytes())
    })
}

/// Process-wide first-wins guard for stdin. Each invocation of a
/// nexus command may consume stdin at most once -- a second attempt
/// errors clearly instead of silently reading zero bytes after the
/// first reader drained it.
///
/// The flag is reset by the dispatch entry point so a long-running
/// nexus that handles multiple commands in sequence (currently not a
/// thing, but cheap insurance) doesn't carry state across commands.
static STDIN_CLAIMED: AtomicBool = AtomicBool::new(false);

/// Reset the stdin-claim flag. Called at the start of each top-level
/// dispatch so per-command isolation holds even if the runtime is
/// reused across commands in one process.
pub fn reset_stdin_claim() {
    STDIN_CLAIMED.store(false, Ordering::Relaxed);
}

/// Try to claim stdin for the current argument. Returns `Ok(())` on
/// first claim, `Err(...)` if another argument has already consumed
/// stdin in this dispatch.
fn claim_stdin() -> Result<(), MorlocError> {
    if STDIN_CLAIMED.swap(true, Ordering::Relaxed) {
        Err(MorlocError::Other(
            "stdin can only be read by a single argument per command".into()
        ))
    } else {
        Ok(())
    }
}

/// Consume a C errmsg pointer and rebuild a `MorlocError` whose
/// message is `ctx: <inner>`. Frees the original C string. If the
/// inner pointer is null, the wrapped error reads `ctx`.
unsafe fn wrap_errmsg_with(err: *mut c_char, ctx: &str) -> MorlocError {
    if err.is_null() {
        return MorlocError::Other(ctx.into());
    }
    let inner = CStr::from_ptr(err).to_string_lossy().into_owned();
    libc::free(err as *mut c_void);
    MorlocError::Other(format!("{}: {}", ctx, inner))
}

/// Wrap a C errmsg with `ctx` and write the result to `errmsg`.
/// One-liner over `set_errmsg` + `wrap_errmsg_with` for the common
/// "tag this error with where it came from" pattern.
unsafe fn wrap_and_set_errmsg(err: *mut c_char, ctx: &str, errmsg: *mut *mut c_char) {
    set_errmsg(errmsg, &wrap_errmsg_with(err, ctx));
}

/// How a CLI argument should be sourced. The classification is the
/// single source of truth for "is this stdin, a file, or inline" --
/// every entry point that reads a CLI argument routes through
/// [`classify_arg_source`] so the rules stay in one place.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArgSource {
    /// `-` or `/dev/stdin`. The caller still needs to call
    /// [`claim_stdin`] before actually reading -- the classifier
    /// only labels the source.
    Stdin,
    /// `file_exists(arg)` returned true. Content type is decided
    /// by [`load_morloc_data_file`] (extension + magic-byte sniff).
    File,
    /// Neither stdin nor an existing file, and the bytes are not
    /// path-shaped. Hand the literal bytes to the JSON/inline
    /// parser for the target schema.
    Inline,
}

/// Single classifier for "what does this CLI argument represent."
/// The four-way decision -- stdin / existing file / path-shaped
/// typo / inline content -- is split out here so every entry point
/// (positional args, bundles, per-field overrides) reaches the
/// same conclusion for the same bytes.
///
/// Returns `Err("file '...' not found")` when the bytes look like a
/// path (per [`arg_looks_like_file_path`]) but `file_exists` says
/// otherwise -- almost always a user typo. JSON-content prefixes
/// (`"` `{` `[` digit `-`) bypass the path check.
pub unsafe fn classify_arg_source(arg: *const c_char) -> Result<ArgSource, MorlocError> {
    extern "C" { fn file_exists(filename: *const c_char) -> bool; }

    let stdin_path = b"/dev/stdin\0";
    let dash_path = b"-\0";
    let is_stdin = libc::strcmp(arg, stdin_path.as_ptr() as *const c_char) == 0
        || libc::strcmp(arg, dash_path.as_ptr() as *const c_char) == 0;
    if is_stdin { return Ok(ArgSource::Stdin); }
    if file_exists(arg) { return Ok(ArgSource::File); }
    let arg_str = CStr::from_ptr(arg).to_string_lossy();
    if arg_looks_like_file_path(arg_str.as_ref()) {
        return Err(MorlocError::Other(format!("file '{}' not found", arg_str)));
    }
    Ok(ArgSource::Inline)
}

// ── argument_t lifecycle ───────────────────────────────────────────────────

// argument_t is defined in eval.h (C). We use it opaquely via libc pointers.
// The struct: { value: *mut c_char, fields: *mut *mut c_char, default_fields: *mut *mut c_char, size: usize }
#[repr(C)]
pub struct ArgumentT {
    pub value: *mut c_char,
    pub fields: *mut *mut c_char,
    pub default_fields: *mut *mut c_char,
    pub size: usize,
}

#[no_mangle]
pub unsafe extern "C" fn initialize_positional(value: *mut c_char) -> *mut ArgumentT {
    let arg = libc::calloc(1, std::mem::size_of::<ArgumentT>()) as *mut ArgumentT;
    if arg.is_null() {
        return ptr::null_mut();
    }
    (*arg).value = if value.is_null() {
        ptr::null_mut()
    } else {
        libc::strdup(value)
    };
    (*arg).size = 0;
    arg
}

#[no_mangle]
pub unsafe extern "C" fn initialize_unrolled(
    size: usize,
    default_value: *mut c_char,
    fields: *mut *mut c_char,
    default_fields: *mut *mut c_char,
) -> *mut ArgumentT {
    let arg = libc::calloc(1, std::mem::size_of::<ArgumentT>()) as *mut ArgumentT;
    if arg.is_null() {
        return ptr::null_mut();
    }
    (*arg).value = if default_value.is_null() {
        ptr::null_mut()
    } else {
        libc::strdup(default_value)
    };
    (*arg).size = size;

    (*arg).fields = libc::calloc(size, std::mem::size_of::<*mut c_char>()) as *mut *mut c_char;
    for i in 0..size {
        let f = *fields.add(i);
        if !f.is_null() {
            *(*arg).fields.add(i) = libc::strdup(f);
        }
    }

    (*arg).default_fields =
        libc::calloc(size, std::mem::size_of::<*mut c_char>()) as *mut *mut c_char;
    for i in 0..size {
        let d = *default_fields.add(i);
        if !d.is_null() {
            *(*arg).default_fields.add(i) = libc::strdup(d);
        }
    }

    arg
}

#[no_mangle]
pub unsafe extern "C" fn free_argument_t(arg: *mut ArgumentT) {
    if arg.is_null() {
        return;
    }
    if !(*arg).value.is_null() {
        libc::free((*arg).value as *mut c_void);
    }
    if !(*arg).fields.is_null() {
        for i in 0..(*arg).size {
            let f = *(*arg).fields.add(i);
            if !f.is_null() {
                libc::free(f as *mut c_void);
            }
        }
        libc::free((*arg).fields as *mut c_void);
    }
    if !(*arg).default_fields.is_null() {
        for i in 0..(*arg).size {
            let d = *(*arg).default_fields.add(i);
            if !d.is_null() {
                libc::free(d as *mut c_void);
            }
        }
        libc::free((*arg).default_fields as *mut c_void);
    }
    libc::free(arg as *mut c_void);
}

// ── adjust_voidstar_relptrs ────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn adjust_voidstar_relptrs(
    data: *mut c_void,
    schema: *const CSchema,
    base_rel: shm::RelPtr,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    let rs = CSchema::to_rust(schema);
    match crate::voidstar::adjust_relptrs(data as *mut u8, &rs, base_rel) {
        Ok(_) => 0,
        Err(e) => {
            set_errmsg(errmsg, &e);
            1
        }
    }
}

// ── read_voidstar_binary ───────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn read_voidstar_binary(
    blob: *const u8,
    blob_size: usize,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut c_void {
    clear_errmsg(errmsg);
    let rs = CSchema::to_rust(schema);

    let base = match shm::shmalloc(blob_size) {
        Ok(p) => p,
        Err(e) => {
            set_errmsg(errmsg, &e);
            return ptr::null_mut();
        }
    };
    std::ptr::copy_nonoverlapping(blob, base, blob_size);

    let base_rel = match shm::abs2rel(base) {
        Ok(r) => r,
        Err(e) => {
            let _ = shm::shfree(base);
            set_errmsg(errmsg, &e);
            return ptr::null_mut();
        }
    };

    if let Err(e) = crate::voidstar::adjust_relptrs(base, &rs, base_rel) {
        let _ = shm::shfree(base);
        set_errmsg(errmsg, &e);
        return ptr::null_mut();
    }

    base as *mut c_void
}

// ── load_morloc_data_file ──────────────────────────────────────────────────
// This function is complex and calls many C functions (read_json_with_schema,
// unpack_with_schema). Keep delegating to C for now via extern declarations.

/// Resolve the (already SHM-allocated) Arrow JSON read into a raw abs pointer
/// usable as the result of `load_morloc_data_file`. On error returns null and
/// sets `errmsg`. The caller is responsible for `data` (and frees on error).
unsafe fn arrow_load_json(
    data: *mut u8,
    data_size: usize,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut c_void {
    let buf = libc::realloc(data as *mut c_void, data_size + 1) as *mut u8;
    if buf.is_null() {
        libc::free(data as *mut c_void);
        set_errmsg(errmsg, &MorlocError::Other("realloc failed".into()));
        return ptr::null_mut();
    }
    *buf.add(data_size) = 0;
    let mut err: *mut c_char = ptr::null_mut();
    let relptr = crate::arrow_ffi::read_json_to_arrow_shm(buf as *const c_char, schema, &mut err);
    libc::free(buf as *mut c_void);
    if !err.is_null() {
        *errmsg = err;
        return ptr::null_mut();
    }
    match shm::rel2abs(relptr) {
        Ok(p) => p as *mut c_void,
        Err(e) => { set_errmsg(errmsg, &e); ptr::null_mut() }
    }
}

#[no_mangle]
pub unsafe extern "C" fn load_morloc_data_file(
    path: *const c_char,
    data: *mut u8,
    data_size: usize,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut c_void {
    clear_errmsg(errmsg);

    extern "C" {
        fn read_json_with_schema(
            dest: *mut u8, json: *mut c_char, schema: *const CSchema,
            errmsg: *mut *mut c_char,
        ) -> *mut u8;
        fn unpack_with_schema(
            mpk: *const c_char, mpk_size: usize, schema: *const CSchema,
            mlcptr: *mut *mut c_void, errmsg: *mut *mut c_char,
        ) -> i32;
    }

    if data_size == 0 {
        set_errmsg(errmsg, &MorlocError::Other("Cannot parse 0-length data".into()));
        return ptr::null_mut();
    }

    // Detect arrow-table targets: the JSON / Arrow-IPC paths produce an
    // ArrowShm directly. Other formats (Parquet, CSV) are still deferred.
    let arrow_target = !schema.is_null() && {
        let rs = CSchema::to_rust(schema);
        crate::arrow_ffi::is_arrow_table_schema(&rs)
    };

    // Arrow IPC file format is detected purely by content magic, no extension
    // sniffing. The IPC reader handles both file (ARROW1 magic) and stream
    // formats; only the file form is distinguishable up front, so we match
    // the magic here and let the reader fall through to stream form for
    // headerless content if requested elsewhere.
    if arrow_target && data_size >= 8 {
        let head = std::slice::from_raw_parts(data, 8);
        let full = std::slice::from_raw_parts(data, data_size);
        if crate::arrow_ipc_reader::is_arrow_file_magic(head) {
            let mut err: *mut c_char = ptr::null_mut();
            let relptr = crate::arrow_ipc_reader::read_arrow_ipc_to_shm(
                data, data_size, schema, &mut err,
            );
            libc::free(data as *mut c_void);
            if !err.is_null() { *errmsg = err; return ptr::null_mut(); }
            return match shm::rel2abs(relptr) {
                Ok(p) => p as *mut c_void,
                Err(e) => { set_errmsg(errmsg, &e); ptr::null_mut() }
            };
        }
        if crate::arrow_ipc_reader::is_parquet_magic(full) {
            let mut err: *mut c_char = ptr::null_mut();
            let relptr = crate::arrow_ipc_reader::read_parquet_to_shm(
                data, data_size, schema, &mut err,
            );
            libc::free(data as *mut c_void);
            if !err.is_null() { *errmsg = err; return ptr::null_mut(); }
            return match shm::rel2abs(relptr) {
                Ok(p) => p as *mut c_void,
                Err(e) => { set_errmsg(errmsg, &e); ptr::null_mut() }
            };
        }
    }

    let path_str = CStr::from_ptr(path).to_string_lossy();
    let mut err: *mut c_char = ptr::null_mut();

    // 1. Extension-based dispatch
    if arrow_target && (path_str.ends_with(".csv") || path_str.ends_with(".tsv")) {
        let delim: u8 = if path_str.ends_with(".tsv") { b'\t' } else { b',' };
        let mut err: *mut c_char = ptr::null_mut();
        let relptr = crate::arrow_ipc_reader::read_csv_to_shm(
            data, data_size, delim, schema, &mut err,
        );
        libc::free(data as *mut c_void);
        if !err.is_null() { *errmsg = err; return ptr::null_mut(); }
        return match shm::rel2abs(relptr) {
            Ok(p) => p as *mut c_void,
            Err(e) => { set_errmsg(errmsg, &e); ptr::null_mut() }
        };
    }
    if path_str.ends_with(".json") {
        if arrow_target {
            return arrow_load_json(data, data_size, schema, errmsg);
        }
        let json_buf = libc::realloc(data as *mut c_void, data_size + 1) as *mut u8;
        if json_buf.is_null() {
            libc::free(data as *mut c_void);
            set_errmsg(errmsg, &MorlocError::Other("realloc failed".into()));
            return ptr::null_mut();
        }
        *json_buf.add(data_size) = 0;
        let result = read_json_with_schema(ptr::null_mut(), json_buf as *mut c_char, schema, &mut err);
        if !err.is_null() {
            libc::free(json_buf as *mut c_void);
            *errmsg = err;
            return ptr::null_mut();
        }
        libc::free(json_buf as *mut c_void);
        return result as *mut c_void;
    }

    if path_str.ends_with(".mpk") || path_str.ends_with(".msgpack") {
        let mut result: *mut c_void = ptr::null_mut();
        unpack_with_schema(data as *const c_char, data_size, schema, &mut result, &mut err);
        libc::free(data as *mut c_void);
        if !err.is_null() {
            *errmsg = err;
            return ptr::null_mut();
        }
        return result;
    }

    // 2. Check for morloc packet header
    if data_size >= 32 {
        let magic = *(data as *const u32);
        if magic == packet::PACKET_MAGIC {
            let header_bytes: &[u8; 32] = &*(data as *const [u8; 32]);
            if let Ok(header) = packet::PacketHeader::from_bytes(header_bytes) {
                if !header.is_data() {
                    libc::free(data as *mut c_void);
                    set_errmsg(errmsg, &MorlocError::Other(format!("Expected data packet in '{}'", path_str)));
                    return ptr::null_mut();
                }
                let offset = { header.offset } as usize;
                let length = { header.length } as usize;
                let payload = data.add(32 + offset);
                let format = { header.command.data.format };

                if format == packet::PACKET_FORMAT_VOIDSTAR {
                    let result = read_voidstar_binary(payload, length, schema, &mut err);
                    libc::free(data as *mut c_void);
                    if !err.is_null() { *errmsg = err; return ptr::null_mut(); }
                    return result;
                } else if format == packet::PACKET_FORMAT_MSGPACK {
                    let mut result: *mut c_void = ptr::null_mut();
                    unpack_with_schema(payload as *const c_char, length, schema, &mut result, &mut err);
                    libc::free(data as *mut c_void);
                    if !err.is_null() { *errmsg = err; return ptr::null_mut(); }
                    return result;
                } else {
                    libc::free(data as *mut c_void);
                    set_errmsg(errmsg, &MorlocError::Other(format!("Unsupported format 0x{:02x} in '{}'", format, path_str)));
                    return ptr::null_mut();
                }
            }
        }
    }

    // 3. Try JSON
    let first_byte = *data;
    let may_be_json = matches!(first_byte,
        b'\'' | b'"' | b'[' | b'{' | b't' | b'f' | b'n' |
        b'\t' | b'\n' | b'\r' | b' ' |
        b'0'..=b'9' | b'-'
    );

    if (data_size > 1 && may_be_json) || (data_size == 1 && first_byte >= b'0' && first_byte <= b'9') {
        if arrow_target {
            return arrow_load_json(data, data_size, schema, errmsg);
        }
        let json_buf = libc::realloc(data as *mut c_void, data_size + 1) as *mut u8;
        if !json_buf.is_null() {
            *json_buf.add(data_size) = 0;
            let result = read_json_with_schema(ptr::null_mut(), json_buf as *mut c_char, schema, &mut err);
            if err.is_null() && !result.is_null() {
                libc::free(json_buf as *mut c_void);
                return result as *mut c_void;
            }
            if !err.is_null() { libc::free(err as *mut c_void); err = ptr::null_mut(); }
            // Fall through to try msgpack
            // Note: data pointer may have been invalidated by realloc
            // Use json_buf as the data pointer going forward
            let mut result: *mut c_void = ptr::null_mut();
            unpack_with_schema(json_buf as *const c_char, data_size, schema, &mut result, &mut err);
            libc::free(json_buf as *mut c_void);
            if !err.is_null() { *errmsg = err; return ptr::null_mut(); }
            return result;
        }
    }

    // 4. Try msgpack
    let mut result: *mut c_void = ptr::null_mut();
    unpack_with_schema(data as *const c_char, data_size, schema, &mut result, &mut err);
    libc::free(data as *mut c_void);
    if !err.is_null() { *errmsg = err; return ptr::null_mut(); }
    result
}

// ── upload_packet (static helper) ────────────────────────────────────────────

/// Copy a voidstar packet into SHM, adjusting relptrs.
///
/// # Safety
/// `dest` must point to schema.width writable bytes in SHM.
/// `data` must point to a valid voidstar blob within [data, data_end].
unsafe fn upload_packet(
    dest: *mut u8,
    data: *const u8,
    data_end: usize,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    let rs = CSchema::to_rust(schema);

    match upload_packet_inner(dest, data, data_end, schema, &rs) {
        Ok(_) => 0,
        Err(e) => {
            set_errmsg(errmsg, &e);
            1
        }
    }
}

unsafe fn upload_packet_inner(
    dest: *mut u8,
    data: *const u8,
    data_end: usize,
    schema: *const CSchema,
    rs: &crate::schema::Schema,
) -> Result<(), MorlocError> {
    use crate::schema::SerialType;

    match rs.serial_type {
        SerialType::String | SerialType::Array => {
            if (data as usize + rs.width - 1) <= data_end {
                return Err(MorlocError::Packet("Data is too small to store an array header".into()));
            }
            ptr::copy_nonoverlapping(data, dest, rs.width);
            let arr = &mut *(dest as *mut shm::Array);
            let arr_data_offset = arr.data as usize;
            let arr_data = data.add(arr_data_offset);
            let elem_width = rs.parameters[0].width;
            let arr_size = arr.size * elem_width;

            if (arr_data as usize + arr_size - 1) > data_end {
                return Err(MorlocError::Packet("Data is too small to contain array values".into()));
            }

            let data_ptr = shm::shmemcpy(arr_data, arr_size)?;

            if !rs.is_fixed_width() {
                let elem_schema = &rs.parameters[0];
                // Need the C schema for each element
                let elem_c_schema = (*schema).parameters;
                if !elem_c_schema.is_null() {
                    let elem_cs = *elem_c_schema;
                    for i in 0..arr.size {
                        upload_packet_inner(
                            data_ptr.add(i * elem_width),
                            arr_data.add(i * elem_width),
                            data_end,
                            elem_cs,
                            elem_schema,
                        )?;
                    }
                }
            }

            arr.data = shm::abs2rel(data_ptr)?;
        }
        SerialType::Tuple | SerialType::Map => {
            for i in 0..rs.parameters.len() {
                let elem_cs = if (*schema).parameters.is_null() {
                    return Err(MorlocError::Packet("NULL parameters in schema".into()));
                } else {
                    *(*schema).parameters.add(i)
                };
                upload_packet_inner(
                    dest.add(rs.offsets[i]),
                    data.add(rs.offsets[i]),
                    data_end,
                    elem_cs,
                    &rs.parameters[i],
                )?;
            }
        }
        _ => {
            if (data as usize + rs.width - 1) > data_end {
                return Err(MorlocError::Packet("Given data packet is too small".into()));
            }
            ptr::copy_nonoverlapping(data, dest, rs.width);
        }
    }
    Ok(())
}

// ── parse_cli_data_argument_singular ─────────────────────────────────────────

unsafe fn parse_cli_data_argument_singular(
    mut dest: *mut u8,
    arg: *mut c_char,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);

    extern "C" {
        fn read_json_with_schema(
            dest: *mut u8, json: *mut c_char, schema: *const CSchema,
            errmsg: *mut *mut c_char,
        ) -> *mut u8;
        fn read_binary_fd(file: *mut libc::FILE, file_size: *mut usize, errmsg: *mut *mut c_char) -> *mut u8;
    }

    let rs = CSchema::to_rust(schema);
    let mut err: *mut c_char = ptr::null_mut();
    let mut fd: *mut libc::FILE = ptr::null_mut();

    let source = match classify_arg_source(arg) {
        Ok(s) => s,
        Err(e) => { set_errmsg(errmsg, &e); return ptr::null_mut(); }
    };
    match source {
        ArgSource::Stdin => {
            if let Err(e) = claim_stdin() {
                set_errmsg(errmsg, &e);
                return ptr::null_mut();
            }
            fd = libc::fdopen(libc::STDIN_FILENO, b"rb\0".as_ptr() as *const c_char);
        }
        ArgSource::File => {
            fd = libc::fopen(arg, b"rb\0".as_ptr() as *const c_char);
            if fd.is_null() {
                set_errmsg(errmsg, &MorlocError::Other(
                    format!("The argument '{}' is a filename, but it can't be read",
                        CStr::from_ptr(arg).to_string_lossy())
                ));
                return ptr::null_mut();
            }
        }
        ArgSource::Inline => {}
    }

    if fd.is_null() {
        // Literal JSON data
        if crate::arrow_ffi::is_arrow_table_schema(&rs) {
            let relptr = crate::arrow_ffi::read_json_to_arrow_shm(arg as *const c_char, schema, &mut err);
            if !err.is_null() {
                *errmsg = err;
                return ptr::null_mut();
            }
            return match shm::rel2abs(relptr) {
                Ok(p) => p,
                Err(e) => { set_errmsg(errmsg, &e); ptr::null_mut() }
            };
        }
        if dest.is_null() {
            match shm::shcalloc(1, rs.width) {
                Ok(p) => dest = p,
                Err(e) => { set_errmsg(errmsg, &e); return ptr::null_mut(); }
            }
        }
        dest = read_json_with_schema(dest, arg, schema, &mut err);
        if !err.is_null() {
            *errmsg = err;
            return ptr::null_mut();
        }
        return dest;
    }

    // File or stdin
    let source_label: String = match source {
        ArgSource::Stdin => "stdin".to_string(),
        ArgSource::File => format!("file '{}'", CStr::from_ptr(arg).to_string_lossy()),
        ArgSource::Inline => unreachable!("fd is non-null"),
    };
    let mut data_size: usize = 0;
    let data = read_binary_fd(fd, &mut data_size, &mut err);
    // Don't close stdin
    if fd != libc::fdopen(libc::STDIN_FILENO, b"rb\0".as_ptr() as *const c_char) {
        libc::fclose(fd);
    }
    if !err.is_null() {
        if !data.is_null() { libc::free(data as *mut c_void); }
        wrap_and_set_errmsg(err, &source_label, errmsg);
        return ptr::null_mut();
    }

    // Special case: RPTR packets
    if data_size >= 32 {
        let magic = *(data as *const u32);
        if magic == packet::PACKET_MAGIC {
            let header = &*(data as *const packet::PacketHeader);
            let source = header.command.data.source;
            let format = header.command.data.format;
            if source == packet::PACKET_SOURCE_RPTR && format == packet::PACKET_FORMAT_VOIDSTAR {
                if dest.is_null() {
                    match shm::shcalloc(1, rs.width) {
                        Ok(p) => dest = p,
                        Err(e) => {
                            libc::free(data as *mut c_void);
                            set_errmsg(errmsg, &e);
                            return ptr::null_mut();
                        }
                    }
                }
                let voidstar_ptr = data.add(32 + header.offset as usize);
                if upload_packet(dest, voidstar_ptr, voidstar_ptr as usize + data_size - 1, schema, &mut err) != 0 {
                    libc::free(data as *mut c_void);
                    wrap_and_set_errmsg(err, &source_label, errmsg);
                    return ptr::null_mut();
                }
                libc::free(data as *mut c_void);
                return dest;
            }
        }
    }

    // All other formats: canonical file loader (takes ownership of data)
    dest = load_morloc_data_file(arg, data, data_size, schema, &mut err) as *mut u8;
    if !err.is_null() {
        wrap_and_set_errmsg(err, &source_label, errmsg);
        return ptr::null_mut();
    }
    dest
}

// ── parse_cli_data_argument_unrolled ─────────────────────────────────────────

/// Load a record bundle (`--group=...` value) and report which fields it
/// supplied.
///
/// The returned vector aligns with `schema.parameters`. `Some(p)` means
/// the bundle provided that field; `None` means it was absent and the
/// caller should fall back to the next source (user override, manifest
/// default, Optional-RELNULL, or error).
///
/// Format detection:
///   - Inline / file content whose first non-whitespace byte is `{`
///     uses the partial JSON path. Missing keys yield `None`.
///   - Anything else (JSON arrays, mpack, voidstar binary packets,
///     morloc binary, arrow, etc.) is treated as a complete record:
///     it is loaded in full and every slot is reported `Some`.
///
/// Field pointers for complete-format bundles alias into the loaded
/// record's SHM allocation -- both blocks stay alive in the eval arena
/// until the dispatch scope drops, so the alias is safe.
unsafe fn load_bundle_partial(
    arg: *mut c_char,
    schema: *const CSchema,
    rs: &crate::schema::Schema,
) -> Result<Vec<Option<shm::AbsPtr>>, MorlocError> {
    let source = classify_arg_source(arg)?;

    let inline_bytes = if source == ArgSource::Inline {
        Some(CStr::from_ptr(arg).to_bytes())
    } else {
        None
    };

    // Read file/stdin content up front: the partial-JSON dispatcher
    // needs to peek at the leading non-whitespace byte, and stdin
    // can only be read once anyway. For inline values the bytes are
    // already addressable via the C string.
    let file_bytes: Option<Vec<u8>> = match source {
        ArgSource::File => {
            let p = CStr::from_ptr(arg).to_string_lossy().into_owned();
            Some(std::fs::read(&p).map_err(|e| MorlocError::Other(
                format!("read bundle file '{}': {}", p, e)
            ))?)
        }
        ArgSource::Stdin => {
            claim_stdin()?;
            let mut buf = Vec::new();
            use std::io::Read;
            std::io::stdin().read_to_end(&mut buf).map_err(|e| MorlocError::Other(
                format!("read bundle stdin: {}", e)
            ))?;
            Some(buf)
        }
        ArgSource::Inline => None,
    };

    // Take a peek at the leading non-whitespace byte WITHOUT keeping
    // a live borrow of file_bytes (we want to move it below).
    let first: Option<u8> = {
        let slice: &[u8] = match (inline_bytes, file_bytes.as_deref()) {
            (Some(b), _) => b,
            (None, Some(b)) => b,
            (None, None) => &[],
        };
        slice.iter().find(|&&b| !b.is_ascii_whitespace()).copied()
    };

    if first == Some(b'{') {
        // JSON object form: partial-load. Re-derive the slice in a
        // new scope; this borrow is independent of the one above.
        let slice: &[u8] = match (inline_bytes, file_bytes.as_deref()) {
            (Some(b), _) => b,
            (None, Some(b)) => b,
            (None, None) => &[],
        };
        let s = std::str::from_utf8(slice).map_err(|e| MorlocError::Other(
            format!("bundle JSON is not valid UTF-8: {}", e)
        ))?;
        return crate::json::load_record_fields_from_json(s, rs);
    }

    // Complete-form bundle. For inline arguments we hand off to
    // singular unchanged (its inline-JSON branch handles JSON arrays
    // and primitives without needing the bytes we already peeked at).
    // For files and stdin the bytes are already in `file_bytes` and
    // re-reading is either wasteful (file) or impossible (stdin
    // already consumed); we feed them to `load_morloc_data_file`
    // directly, which dispatches on path extension or content magic.
    let mut err: *mut c_char = ptr::null_mut();
    let full: *mut u8 = if let Some(bytes) = file_bytes {
        let n = bytes.len();
        let buf = libc::malloc(n.max(1)) as *mut u8;
        if buf.is_null() {
            return Err(MorlocError::Other("malloc failed for bundle bytes".into()));
        }
        ptr::copy_nonoverlapping(bytes.as_ptr(), buf, n);
        // `load_morloc_data_file` takes ownership of `buf` -- it
        // frees on every path. The path string only affects
        // extension-based format hints; for stdin we pass the
        // original "-" / "/dev/stdin", which has no extension and
        // falls through to magic-byte detection.
        let result = load_morloc_data_file(arg, buf, n, schema, &mut err);
        result as *mut u8
    } else {
        parse_cli_data_argument_singular(ptr::null_mut(), arg, schema, &mut err)
    };
    if !err.is_null() {
        let msg = CStr::from_ptr(err).to_string_lossy().into_owned();
        libc::free(err as *mut c_void);
        return Err(MorlocError::Other(msg));
    }
    if full.is_null() {
        return Err(MorlocError::Other("bundle load returned null".into()));
    }

    let mut out = Vec::with_capacity(rs.parameters.len());
    for i in 0..rs.parameters.len() {
        out.push(Some(full.add(rs.offsets[i])));
    }
    Ok(out)
}

unsafe fn parse_cli_data_argument_unrolled(
    mut dest: *mut u8,
    default_value: *mut c_char,
    fields: *mut *mut c_char,
    default_fields: *mut *mut c_char,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);
    let rs = CSchema::to_rust(schema);
    let mut err: *mut c_char = ptr::null_mut();

    use crate::schema::SerialType;
    match rs.serial_type {
        SerialType::Tuple | SerialType::Map => {}
        _ => {
            set_errmsg(errmsg, &MorlocError::Other(
                "Only record and tuple types may be unrolled".into()));
            return ptr::null_mut();
        }
    }

    if dest.is_null() {
        match shm::shcalloc(1, rs.width) {
            Ok(p) => dest = p,
            Err(e) => { set_errmsg(errmsg, &e); return ptr::null_mut(); }
        }
    }

    let n = rs.parameters.len();

    // Source 1: bundle (per-field voidstar, may be absent or partial).
    let bundle_fields: Vec<Option<shm::AbsPtr>> = if default_value.is_null() {
        vec![None; n]
    } else {
        match load_bundle_partial(default_value, schema, &rs) {
            Ok(v) => v,
            Err(e) => { set_errmsg(errmsg, &e); return ptr::null_mut(); }
        }
    };

    // Tuple bundles must be complete -- positional encoding has no
    // notion of missing slots. JSON object form is record-only;
    // load_bundle_partial enforces that at parse time.
    if matches!(rs.serial_type, SerialType::Tuple) {
        if let Some((i, _)) = bundle_fields.iter().enumerate().find(|(_, f)| f.is_none()) {
            set_errmsg(errmsg, &MorlocError::Other(
                format!("Tuple bundle is missing slot {}", i)
            ));
            return ptr::null_mut();
        }
    }

    let field_label = |i: usize| -> String {
        match rs.keys.get(i) {
            Some(k) => format!("field '{}'", k),
            None => format!("slot {}", i),
        }
    };

    // Source 2: per-field user CLI values, each loaded independently
    // through the standard pipeline (so any format the user provides
    // works: inline JSON, file path, stdin, mpack file, etc.).
    let mut user_fields: Vec<Option<shm::AbsPtr>> = vec![None; n];
    for i in 0..n {
        let field_val = *fields.add(i);
        if !field_val.is_null() {
            let elem_cs = *(*schema).parameters.add(i);
            let fs = &rs.parameters[i];
            let p = match shm::shcalloc(1, fs.width) {
                Ok(p) => p,
                Err(e) => { set_errmsg(errmsg, &e); return ptr::null_mut(); }
            };
            let loaded = parse_cli_data_argument_singular(p, field_val, elem_cs, &mut err);
            if !err.is_null() {
                wrap_and_set_errmsg(err, &field_label(i), errmsg);
                return ptr::null_mut();
            }
            user_fields[i] = Some(loaded);
        }
    }

    // Source 3: manifest per-field defaults (always JSON literals in
    // the manifest). Eager: parse them upfront so the merge step is a
    // pure picker. Cheap -- defaults are tiny. A bad default raises a
    // hard error pointing at the field, so manifest-data shape bugs
    // surface here rather than as silent zeros.
    let mut default_field_ptrs: Vec<Option<shm::AbsPtr>> = vec![None; n];
    for i in 0..n {
        let default_field = *default_fields.add(i);
        if !default_field.is_null() {
            let elem_cs = *(*schema).parameters.add(i);
            let fs = &rs.parameters[i];
            let p = match shm::shcalloc(1, fs.width) {
                Ok(p) => p,
                Err(e) => { set_errmsg(errmsg, &e); return ptr::null_mut(); }
            };
            let loaded = parse_cli_data_argument_singular(p, default_field, elem_cs, &mut err);
            if !err.is_null() {
                wrap_and_set_errmsg(
                    err,
                    &format!("default for {}", field_label(i)),
                    errmsg,
                );
                return ptr::null_mut();
            }
            default_field_ptrs[i] = Some(loaded);
        }
    }

    // Merge into dest with precedence user > bundle > default >
    // Optional-RELNULL > error. memcpy of fs.width bytes copies the
    // field header (and any inline value); relptrs inside the header
    // continue to reference the source's SHM block, which stays
    // tracked by the eval arena for the rest of the dispatch.
    for i in 0..n {
        let elem_dest = dest.add(rs.offsets[i]);
        let fs = &rs.parameters[i];
        let src = user_fields[i]
            .or(bundle_fields[i])
            .or(default_field_ptrs[i]);
        match src {
            Some(p) => ptr::copy_nonoverlapping(p, elem_dest, fs.width),
            None if matches!(fs.serial_type, SerialType::Optional) => {
                *(elem_dest as *mut shm::RelPtr) = shm::RELNULL;
            }
            None => {
                let key = rs.keys.get(i).map(|s| s.as_str()).unwrap_or("?");
                set_errmsg(errmsg, &MorlocError::Other(
                    format!("Required field '{}' is missing and has no default", key)
                ));
                return ptr::null_mut();
            }
        }
    }

    dest
}

// ── parse_cli_data_argument ──────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn parse_cli_data_argument(
    dest: *mut u8,
    arg: *const ArgumentT,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);
    let mut err: *mut c_char = ptr::null_mut();

    let result = if (*arg).fields.is_null() {
        parse_cli_data_argument_singular(dest, (*arg).value, schema, &mut err)
    } else {
        parse_cli_data_argument_unrolled(
            dest, (*arg).value, (*arg).fields, (*arg).default_fields, schema, &mut err,
        )
    };

    if !err.is_null() {
        *errmsg = err;
        return ptr::null_mut();
    }
    if result.is_null() {
        return ptr::null_mut();
    }

    let relptr = match shm::abs2rel(result) {
        Ok(r) => r,
        Err(e) => { set_errmsg(errmsg, &e); return ptr::null_mut(); }
    };

    // Arrow tables need a PACKET_FORMAT_ARROW header so the receiver routes
    // to arrow_from_shm rather than voidstar deserialization. For non-arrow
    // payloads we route through make_data_packet_auto, which inlines small
    // values (<= MORLOC_INLINE_THRESHOLD bytes) into PACKET_SOURCE_MESG +
    // PACKET_FORMAT_VOIDSTAR packets and ships larger ones as
    // PACKET_SOURCE_RPTR. Inlining avoids the cross-process SHM round-trip
    // and the rel2abs lookup on the receiver, which dominates latency for
    // tiny args (the common case for scalar inputs and small lists).
    let arrow = !schema.is_null() && {
        let rs = CSchema::to_rust(schema);
        crate::arrow_ffi::is_arrow_table_schema(&rs)
    };

    if arrow {
        crate::packet_ffi::make_arrow_data_packet(relptr, schema)
    } else {
        crate::packet_ffi::make_data_packet_auto(
            result as *mut c_void,
            relptr,
            schema,
            errmsg,
        )
    }
}

// ── make_call_packet_from_cli ────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn make_call_packet_from_cli(
    dest: *mut u8,
    mid: u32,
    args: *mut *mut ArgumentT,   // NULL-terminated
    arg_schema_strs: *mut *mut c_char, // NULL-terminated
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);
    let mut err: *mut c_char = ptr::null_mut();

    // Count and parse schemas
    let mut nschemas: usize = 0;
    while !(*arg_schema_strs.add(nschemas)).is_null() {
        nschemas += 1;
    }

    let mut schemas: Vec<*mut CSchema> = Vec::with_capacity(nschemas);
    for i in 0..nschemas {
        let schema = crate::ffi::parse_schema(*arg_schema_strs.add(i), &mut err);
        if !err.is_null() {
            for s in &schemas { CSchema::free(*s); }
            *errmsg = err;
            return ptr::null_mut();
        }
        schemas.push(schema);
    }

    // Count args
    let mut nargs: usize = 0;
    while !(*args.add(nargs)).is_null() {
        nargs += 1;
    }

    // Parse each argument into a data packet
    let mut packet_args: Vec<*const u8> = Vec::with_capacity(nargs);
    for i in 0..nargs {
        let packet = parse_cli_data_argument(dest, *args.add(i), schemas[i], &mut err);
        if !err.is_null() {
            for p in &packet_args { libc::free(*p as *mut c_void); }
            for s in &schemas { CSchema::free(*s); }
            *errmsg = err;
            return ptr::null_mut();
        }
        packet_args.push(packet as *const u8);
    }

    // Build call packet
    let call_packet = crate::packet_ffi::make_morloc_local_call_packet(
        mid, packet_args.as_ptr(), nargs, &mut err,
    );

    for p in &packet_args { libc::free(*p as *mut c_void); }
    for s in &schemas { CSchema::free(*s); }

    if !err.is_null() {
        *errmsg = err;
        return ptr::null_mut();
    }
    call_packet
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn path_predicate_extensions() {
        assert!(arg_looks_like_file_path("a.json"));
        assert!(arg_looks_like_file_path("a.JSON"));
        assert!(arg_looks_like_file_path("data.mpk"));
        assert!(arg_looks_like_file_path("table.parquet"));
        assert!(arg_looks_like_file_path("table.arrow"));
    }

    #[test]
    fn path_predicate_separators() {
        assert!(arg_looks_like_file_path("./algconf"));
        assert!(arg_looks_like_file_path("dir/sub/file"));
        assert!(arg_looks_like_file_path("/abs/path"));
    }

    #[test]
    fn path_predicate_inline_values() {
        // Bare inline JSON-ish values should NOT be treated as paths.
        assert!(!arg_looks_like_file_path("42"));
        assert!(!arg_looks_like_file_path("\"hello\""));
        assert!(!arg_looks_like_file_path("[1,2,3]"));
        assert!(!arg_looks_like_file_path("{\"m\":1}"));
        assert!(!arg_looks_like_file_path("true"));
        assert!(!arg_looks_like_file_path("-"));
        assert!(!arg_looks_like_file_path(""));
    }

    #[test]
    fn path_predicate_quoted_path_is_json_string() {
        // A JSON-quoted string is a Str literal for the JSON parser,
        // even if its contents look like a path.
        assert!(!arg_looks_like_file_path("\"logs/run/inputs/0001.pkt\""));
        assert!(!arg_looks_like_file_path("\"a/b/c.json\""));
    }

    #[test]
    fn stdin_claim_first_wins() {
        reset_stdin_claim();
        assert!(claim_stdin().is_ok());
        let second = claim_stdin();
        assert!(second.is_err(), "second claim should fail");
        assert!(second.unwrap_err().to_string().contains("stdin"));
        reset_stdin_claim();
        assert!(claim_stdin().is_ok(), "reset releases the claim");
        reset_stdin_claim();
    }
}
