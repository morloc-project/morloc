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
    /// The token names a file to be opened. Content type is decided
    /// by [`load_morloc_data_file`] (extension + magic-byte sniff).
    /// Reached when the token doesn't parse as inline JSON but does
    /// name an existing path on disk.
    File,
    /// The bytes are inline content (literal JSON for the target
    /// schema). Reached when the token has a JSON-literal shape
    /// (`"`/`{`/`[`/digit/`-digit` prefix, or the bare keywords
    /// `null`/`true`/`false`).
    Inline,
}

/// Classified CLI argument: the source kind plus a C-string pointer
/// into (a possibly offset position within) the original token. The
/// pointer is safe to pass to `fopen` / `read_json_with_schema` and is
/// guaranteed to be NUL-terminated because we only advance into the
/// original NUL-terminated string -- never past it.
pub struct Classified {
    pub kind: ArgSource,
    pub effective: *const c_char,
}

/// True iff the (trimmed) token has the prefix of a JSON literal, or
/// is one of the three bare keywords. Used by the new classifier to
/// preempt the filesystem check so a file literally named `null`,
/// `42`, etc. doesn't shadow the JSON keyword. Strings (which start
/// with `"`), objects (`{`), arrays (`[`), numbers (digit, or
/// `-digit`), and the keywords are all recognized here.
fn is_json_literal_shape(s: &str) -> bool {
    let t = s.trim_start();
    let mut it = t.chars();
    match it.next() {
        Some('"') | Some('{') | Some('[') => return true,
        Some(c) if c.is_ascii_digit() => return true,
        Some('-') => return it.next().map_or(false, |c| c.is_ascii_digit()),
        _ => {}
    }
    matches!(t.trim_end(), "null" | "true" | "false")
}

/// Single classifier for "what does this CLI argument represent."
/// The decision order, in priority:
///
///   1. `-` / `/dev/stdin` (Stdin).
///   2. Token has a JSON-literal shape (Inline) -- this preempts the
///      filesystem check, so a file literally named `null` or `42`
///      doesn't shadow the JSON keyword.
///   3. `file_exists(token)` (File).
///   4. Path-shaped but missing -> `Err("file '...' not found")`.
///   5. Anything else -> error pointing the user at `source: file` in
///      the argument's docstring.
pub unsafe fn classify_arg_source(arg: *const c_char) -> Result<Classified, MorlocError> {
    extern "C" { fn file_exists(filename: *const c_char) -> bool; }

    let stdin_path = b"/dev/stdin\0";
    let dash_path = b"-\0";
    let is_stdin = libc::strcmp(arg, stdin_path.as_ptr() as *const c_char) == 0
        || libc::strcmp(arg, dash_path.as_ptr() as *const c_char) == 0;
    if is_stdin {
        return Ok(Classified { kind: ArgSource::Stdin, effective: arg });
    }

    let arg_str = CStr::from_ptr(arg).to_string_lossy();

    if is_json_literal_shape(&arg_str) {
        return Ok(Classified { kind: ArgSource::Inline, effective: arg });
    }

    if file_exists(arg) {
        return Ok(Classified { kind: ArgSource::File, effective: arg });
    }

    if arg_looks_like_file_path(arg_str.as_ref()) {
        return Err(MorlocError::Other(format!("file '{}' not found", arg_str)));
    }

    Err(MorlocError::Other(format!(
        "argument '{}' is not inline JSON (would have to start with \", {{, [, a digit, \
         -digit, or be the bare keyword null/true/false) and is not an existing file path. \
         If the argument is always a path, set `source: file` in its docstring; if it is \
         always a literal, set `source: inline`.",
        arg_str
    )))
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

// ── Stream-packet peek helpers ────────────────────────────────────────────

/// Cheap file-shape sniff: returns the packet header if `path` names a
/// file whose first 32 bytes parse as a morloc packet, or `None` for
/// any failure (missing file, short read, wrong magic, unparseable
/// header). "Not a morloc packet" is not an error.
pub(crate) unsafe fn peek_packet_header_via_pread(
    path: *const c_char,
) -> Option<packet::PacketHeader> {
    if path.is_null() {
        return None;
    }
    let fd = libc::open(path, libc::O_RDONLY);
    if fd < 0 {
        return None;
    }
    let mut header_bytes = [0u8; 32];
    let n = libc::pread(
        fd,
        header_bytes.as_mut_ptr() as *mut libc::c_void,
        32,
        0,
    );
    libc::close(fd);
    if n != 32 {
        return None;
    }
    let magic = u32::from_le_bytes(header_bytes[..4].try_into().unwrap());
    if magic != packet::PACKET_MAGIC {
        return None;
    }
    packet::PacketHeader::from_bytes(&header_bytes).ok()
}

/// C ABI shim over `peek_packet_header_via_pread`. Returns 1 iff `path`
/// is a `PACKET_TYPE_STREAM` file, 0 otherwise. Language runtimes call
/// this from their FILE+DATA unwrap branches to choose between the
/// legacy data-indirection path and the stream-ingest loop.
#[no_mangle]
pub unsafe extern "C" fn file_is_stream_packet(path: *const c_char) -> i32 {
    match peek_packet_header_via_pread(path) {
        Some(header) if header.is_stream() => 1,
        _ => 0,
    }
}

/// Wire `try_load_stream_packet_file` into an entry point: on stream
/// hit return the packet; on non-stream fall through; on error stamp
/// `errmsg` and return null. Deduplicates the three CLI entry-point
/// insertions.
macro_rules! stream_fast_path_or_fallthrough {
    ($path:expr, $schema:expr, $errmsg:expr) => {
        match try_load_stream_packet_file($path, $schema) {
            Ok(Some(pkt)) => return pkt,
            Ok(None) => {}
            Err(e) => {
                set_errmsg($errmsg, &e);
                return ptr::null_mut();
            }
        }
    };
}

// ── try_load_stream_packet_file ───────────────────────────────────────────

/// Nexus-side fast path for stream-packet CLI file arguments. Dispatches
/// on the receiver schema: `Array` → FILE+DATA indirection (pool iterates
/// chunk-by-chunk), `IStream` → pre-opened `TAG_HANDLE` field, other →
/// targeted error. Returns `Ok(None)` when the file is not a stream so
/// callers can fall through to the legacy loader chain.
pub(crate) unsafe fn try_load_stream_packet_file(
    path: *const c_char,
    schema: *const CSchema,
) -> Result<Option<*mut u8>, MorlocError> {
    use crate::schema::SerialType;
    use morloc_runtime_types::stream_handle as sh;

    match peek_packet_header_via_pread(path) {
        Some(h) if h.is_stream() => {}
        _ => return Ok(None),
    };

    if schema.is_null() {
        return Err(MorlocError::Other(
            "stream file passed as CLI argument with null schema".into(),
        ));
    }

    let rs = CSchema::to_rust(schema);
    let path_str = CStr::from_ptr(path).to_string_lossy().into_owned();

    match rs.serial_type {
        SerialType::Array => {
            // Small indirection packet: payload = path bytes.
            let pkt = crate::packet_ffi::make_data_indirection_packet(path, schema);
            if pkt.is_null() {
                return Err(MorlocError::Other(format!(
                    "make_data_indirection_packet failed for stream file '{}'",
                    path_str,
                )));
            }
            Ok(Some(pkt))
        }
        SerialType::IStream => {
            // Open the file now; embed the slot id as TAG_HANDLE.
            let handle = crate::stream::shared_open_istream(&path_str)?;
            let field_ptr = match shm::shcalloc(1, sh::STREAM_HANDLE_FIELD_SIZE) {
                Ok(p) => p as *mut u8,
                Err(e) => {
                    let _ = crate::stream::shared_discard_handle(handle);
                    return Err(e);
                }
            };
            sh::write_field(field_ptr, sh::TAG_HANDLE, handle as u64);
            let mut inner_err: *mut c_char = ptr::null_mut();
            let pkt = wrap_voidstar_as_packet(
                field_ptr as *mut c_void,
                schema,
                &mut inner_err,
            );
            if pkt.is_null() {
                let _ = shm::shfree(field_ptr as shm::AbsPtr);
                let _ = crate::stream::shared_discard_handle(handle);
                let msg = if inner_err.is_null() {
                    format!("wrap_voidstar_as_packet failed for stream file '{}'", path_str)
                } else {
                    let s = CStr::from_ptr(inner_err).to_string_lossy().into_owned();
                    libc::free(inner_err as *mut c_void);
                    s
                };
                return Err(MorlocError::Other(msg));
            }
            Ok(Some(pkt))
        }
        other => {
            // Look up the file's schema string for a targeted error.
            let file_schema = crate::stream::read_schema_from_file(&path_str)
                .unwrap_or_else(|_| "<unknown>".to_string());
            Err(MorlocError::Other(format!(
                "'{}' is a stream-packet file (schema \"{}\"). The receiver's \
                 type has serial_type {:?}; only `[a]` (Array) and \
                 `IStream a` can accept a stream-packet file.",
                path_str, file_schema, other,
            )))
        }
    }
}

// ── Layer 2/3: try_load_voidstar_packet_via_mmap ──────────────────────────

/// Layer 2/3 fast path. For inputs that are regular files containing a
/// large uncompressed MESG+VOIDSTAR morloc packet, allocate SHM for the
/// payload, `pread` the payload directly into the SHM region (skipping
/// the wasted libc::malloc + read + memcpy round-trip the legacy path
/// does), and rewrite the embedded relptrs in place so they resolve in
/// the consumer's VOLUMES table.
///
/// Cross-process correctness: the returned pointer lives in real
/// `/dev/shm` (or the file-backed fallback). A peer process that
/// receives a relptr into this payload can `shm_open` the segment via
/// the relptr's encoded volume index -- the foundation Layer 1 added.
///
/// Layer 3 interaction: if the on-disk packet carries
/// `METADATA_TYPE_VOL_INDEX` (a producer hint), the relptrs in the
/// payload have that hint already baked into their high bits; the
/// relptr walk uses a delta of `dest_rel - encode_relptr(hint, 0)` so
/// the hint bits cancel and the consumer's slot/offset is added in
/// the same single pass. Hint = 0 collapses to the legacy
/// "buffer-relative -> SHM-relative" add walk.
///
/// Returns:
/// * `Ok(Some(ptr))` -- fast path took, `ptr` is the payload in SHM
/// * `Ok(None)`     -- file isn't eligible (too small, not a packet,
///                     wrong format, open/fstat failed). Caller should
///                     fall back to the read+process path.
/// * `Err(e)`        -- the file IS eligible but allocation or the
///                     in-place relptr walk failed.
pub(crate) unsafe fn try_load_voidstar_packet_via_mmap(
    path: *const c_char,
    schema: *const CSchema,
) -> Result<Option<*mut u8>, MorlocError> {
    // Below this size the existing libc::malloc + read + shmalloc +
    // memcpy path is fast enough that the SHM-direct path's syscalls
    // don't amortize. 1 MiB is the crossover on commodity SSDs.
    const FAST_PATH_THRESHOLD: usize = 1024 * 1024;

    if path.is_null() {
        return Ok(None);
    }

    let fd = libc::open(path, libc::O_RDONLY);
    if fd < 0 {
        return Ok(None);
    }

    let mut sb: libc::stat = std::mem::zeroed();
    if libc::fstat(fd, &mut sb) != 0 {
        libc::close(fd);
        return Ok(None);
    }
    if (sb.st_mode & libc::S_IFMT) != libc::S_IFREG {
        libc::close(fd);
        return Ok(None);
    }
    let file_size = sb.st_size as usize;
    if file_size < 32 || file_size < FAST_PATH_THRESHOLD {
        libc::close(fd);
        return Ok(None);
    }

    let mut header_bytes = [0u8; 32];
    let n = libc::pread(fd, header_bytes.as_mut_ptr() as *mut libc::c_void, 32, 0);
    if n != 32 {
        libc::close(fd);
        return Ok(None);
    }

    let magic = u32::from_le_bytes(header_bytes[..4].try_into().unwrap());
    if magic != packet::PACKET_MAGIC {
        libc::close(fd);
        return Ok(None);
    }
    let header = match packet::PacketHeader::from_bytes(&header_bytes) {
        Ok(h) => h,
        Err(_) => {
            libc::close(fd);
            return Ok(None);
        }
    };
    if !header.is_data() {
        libc::close(fd);
        return Ok(None);
    }
    let source = header.command.data.source;
    let format = header.command.data.format;
    let compression = header.command.data.compression;
    if source != packet::PACKET_SOURCE_MESG || format != packet::PACKET_FORMAT_VOIDSTAR {
        libc::close(fd);
        return Ok(None);
    }
    if compression != 0 {
        // Compressed payloads need a separate decompressed buffer; the
        // legacy path handles them.
        libc::close(fd);
        return Ok(None);
    }
    let metadata_size = header.offset as usize;
    let payload_offset = 32 + metadata_size;
    let payload_size = header.length as usize;
    if payload_offset.saturating_add(payload_size) > file_size {
        libc::close(fd);
        return Ok(None);
    }

    // Pull the metadata block out so we can check for a vol_idx hint
    // AND validate the packet's stored schema against the caller's
    // requested schema without a full mmap. Falls back to hint = 0
    // (legacy Layer 2) if metadata is absent, malformed, or doesn't
    // include the block. Schema mismatch surfaces as UserThrow (via
    // libmorloc's errmsg convention -- see check_packet_schema_matches).
    let vol_idx_hint: u16 = if metadata_size > 0 {
        let mut meta = vec![0u8; metadata_size];
        let nm = libc::pread(
            fd,
            meta.as_mut_ptr() as *mut libc::c_void,
            metadata_size,
            32,
        );
        if nm as usize != metadata_size {
            libc::close(fd);
            return Ok(None);
        }
        let path_str = CStr::from_ptr(path).to_string_lossy();
        if let Err(e) = check_packet_schema_matches(&meta, schema, &path_str) {
            libc::close(fd);
            return Err(e);
        }
        let mut full = Vec::with_capacity(32 + metadata_size);
        full.extend_from_slice(&header_bytes);
        full.extend_from_slice(&meta);
        packet::read_vol_index_from_meta(&full).ok().flatten().unwrap_or(0)
    } else {
        0
    };

    // Allocate the SHM destination for the payload and pread the file's
    // payload region straight into it. The shmalloc-tracking arena
    // hooks in automatically; the caller's normal "shfree on drop"
    // semantics apply.
    let dest = match shm::shmalloc(payload_size) {
        Ok(p) => p,
        Err(e) => {
            libc::close(fd);
            return Err(e);
        }
    };

    // pread in a loop until payload_size bytes have been read, in case
    // short reads occur on large files.
    let mut total: usize = 0;
    while total < payload_size {
        let n = libc::pread(
            fd,
            dest.add(total) as *mut libc::c_void,
            payload_size - total,
            (payload_offset + total) as libc::off_t,
        );
        if n < 0 {
            let e = std::io::Error::last_os_error();
            if e.kind() == std::io::ErrorKind::Interrupted {
                continue;
            }
            libc::close(fd);
            let _ = shm::shfree(dest);
            return Err(MorlocError::Io(e));
        }
        if n == 0 {
            // Unexpected EOF mid-payload.
            libc::close(fd);
            let _ = shm::shfree(dest);
            return Err(MorlocError::Other(format!(
                "short read on packet payload: got {} of {} bytes",
                total, payload_size
            )));
        }
        total += n as usize;
    }
    libc::close(fd);

    // Compute the delta that transforms each producer relptr P =
    // (hint << 48) | p_o into the consumer-side relptr T = (dest_slot
    // << 48) | (dest_offset + p_o). Single pass works for both Layer 3
    // (hint > 0) and the legacy buffer-relative (hint = 0) cases.
    let dest_rel = match shm::abs2rel(dest) {
        Ok(r) => r,
        Err(e) => {
            let _ = shm::shfree(dest);
            return Err(e);
        }
    };
    let producer_base = shm::encode_relptr(vol_idx_hint as usize, 0);
    let delta = (dest_rel as i64).wrapping_sub(producer_base as i64) as shm::RelPtr;

    let rs = CSchema::to_rust(schema);
    if let Err(e) = crate::voidstar::adjust_relptrs(dest, &rs, delta) {
        let _ = shm::shfree(dest);
        return Err(e);
    }

    Ok(Some(dest))
}

// ── rebase_voidstar_in_shm (shared by the compressed ingest fast paths) ───

/// Apply the producer->consumer relptr rebase to the voidstar in `dest`
/// in-place. Computes `delta = abs2rel(dest) - encode_relptr(hint, 0)`
/// and walks the structure adjusting each relptr by `delta`. On any
/// failure, frees `dest` before returning the error so callers do not
/// need a separate cleanup path.
unsafe fn rebase_voidstar_in_shm(
    dest: *mut u8,
    schema: *const CSchema,
    vol_idx_hint: u16,
) -> Result<(), MorlocError> {
    let dest_rel = match shm::abs2rel(dest) {
        Ok(r) => r,
        Err(e) => {
            let _ = shm::shfree(dest);
            return Err(e);
        }
    };
    let producer_base = shm::encode_relptr(vol_idx_hint as usize, 0);
    let delta = (dest_rel as i64).wrapping_sub(producer_base as i64) as shm::RelPtr;
    let rs = CSchema::to_rust(schema);
    if let Err(e) = crate::voidstar::adjust_relptrs(dest, &rs, delta) {
        let _ = shm::shfree(dest);
        return Err(e);
    }
    Ok(())
}

// ── try_load_compressed_voidstar_via_shm ──────────────────────────────────

/// Compressed analogue of [`try_load_voidstar_packet_via_mmap`]. For
/// files containing a large MESG+VOIDSTAR packet whose payload is a
/// single zstd frame carrying `pledgedSrcSize` in its header, allocates
/// SHM at the *decompressed* size and stream-decompresses straight into
/// the SHM region -- skipping the legacy chain's libc::malloc read
/// buffer, the growing-Vec decoder output, the `Vec`-rebuild inside
/// `decompress_packet`, and the malloc->SHM memcpy in
/// `read_voidstar_binary_with_hint`. Peak resident set is one X of the
/// decompressed payload (the SHM region) plus the BufReader window and
/// libzstd's internal buffers.
///
/// Returns `Ok(None)` -- triggering the caller's fall-through to the
/// legacy path -- when any of the eligibility checks fail (wrong magic,
/// not MESG+VOIDSTAR, not zstd-compressed, frame missing
/// `pledgedSrcSize`, file too small, fstat/pread failures).
pub(crate) unsafe fn try_load_compressed_voidstar_via_shm(
    path: *const c_char,
    schema: *const CSchema,
) -> Result<Option<*mut u8>, MorlocError> {
    // Same crossover as the uncompressed fast path: below this size the
    // legacy buffered chain is competitive and the syscall overhead of
    // the streaming path is not amortized.
    const FAST_PATH_THRESHOLD: usize = 1024 * 1024;

    if path.is_null() {
        return Ok(None);
    }

    let fd = libc::open(path, libc::O_RDONLY);
    if fd < 0 {
        return Ok(None);
    }

    // Guards from here on must close `fd` on every return until ownership
    // is handed to the File::from_raw_fd wrapper later.
    let mut sb: libc::stat = std::mem::zeroed();
    if libc::fstat(fd, &mut sb) != 0 {
        libc::close(fd);
        return Ok(None);
    }
    if (sb.st_mode & libc::S_IFMT) != libc::S_IFREG {
        libc::close(fd);
        return Ok(None);
    }
    let file_size = sb.st_size as usize;
    if file_size < 32 || file_size < FAST_PATH_THRESHOLD {
        libc::close(fd);
        return Ok(None);
    }

    let mut header_bytes = [0u8; 32];
    let n = libc::pread(fd, header_bytes.as_mut_ptr() as *mut libc::c_void, 32, 0);
    if n != 32 {
        libc::close(fd);
        return Ok(None);
    }

    let magic = u32::from_le_bytes(header_bytes[..4].try_into().unwrap());
    if magic != packet::PACKET_MAGIC {
        libc::close(fd);
        return Ok(None);
    }
    let header = match packet::PacketHeader::from_bytes(&header_bytes) {
        Ok(h) => h,
        Err(_) => {
            libc::close(fd);
            return Ok(None);
        }
    };
    if !header.is_data() {
        libc::close(fd);
        return Ok(None);
    }
    let source = header.command.data.source;
    let format = header.command.data.format;
    let compression = header.command.data.compression;
    if source != packet::PACKET_SOURCE_MESG
        || format != packet::PACKET_FORMAT_VOIDSTAR
        || compression != packet::PACKET_COMPRESSION_ZSTD
    {
        libc::close(fd);
        return Ok(None);
    }
    let metadata_size = header.offset as usize;
    let payload_offset = 32 + metadata_size;
    let payload_size = header.length as usize; // compressed-frame length on disk
    if payload_offset.saturating_add(payload_size) > file_size {
        libc::close(fd);
        return Ok(None);
    }

    // Read the entire metadata block in one pread so we can parse out
    // the frame index and the vol_idx hint without making multiple
    // round-trips to the page cache.
    let metadata_bytes: Vec<u8> = if metadata_size > 0 {
        let mut meta = vec![0u8; metadata_size];
        let nm = libc::pread(
            fd,
            meta.as_mut_ptr() as *mut libc::c_void,
            metadata_size,
            32,
        );
        if nm as usize != metadata_size {
            libc::close(fd);
            return Ok(None);
        }
        meta
    } else {
        Vec::new()
    };
    // `read_*_from_meta` expects a full packet (header + metadata); we
    // splice the header bytes we already read with the metadata bytes
    // we just pulled.
    let mut full_header_and_meta = Vec::with_capacity(32 + metadata_size);
    full_header_and_meta.extend_from_slice(&header_bytes);
    full_header_and_meta.extend_from_slice(&metadata_bytes);

    // Validate the packet's stored schema against the caller's
    // requested schema before committing to the (expensive)
    // decompression + rebase path.
    if metadata_size > 0 {
        let path_str = CStr::from_ptr(path).to_string_lossy();
        if let Err(e) = check_packet_schema_matches(&metadata_bytes, schema, &path_str) {
            libc::close(fd);
            return Err(e);
        }
    }

    let vol_idx_hint: u16 = packet::read_vol_index_from_meta(&full_header_and_meta)
        .ok()
        .flatten()
        .unwrap_or(0);

    // The frame index is mandatory under the multi-frame format. A
    // compressed packet without one is not the new format -- skip and
    // let the legacy path raise the hard error.
    let frames = match packet::read_frame_index_from_meta(&full_header_and_meta) {
        Ok(Some(f)) if !f.is_empty() => f,
        _ => {
            libc::close(fd);
            return Ok(None);
        }
    };

    // From here we will return Err on failure rather than Ok(None) --
    // we've committed to the fast path and the file is genuinely
    // eligible; a downstream failure is a real error, not "try the
    // legacy path".

    let total_uncompressed: usize =
        frames.iter().map(|f| f.uncompressed_size as usize).sum();
    let total_compressed: usize =
        frames.iter().map(|f| f.compressed_size as usize).sum();
    if total_compressed != payload_size {
        libc::close(fd);
        return Err(MorlocError::Packet(format!(
            "frame index sums to {} compressed bytes but header.length = {}",
            total_compressed, payload_size
        )));
    }

    let t0 = std::time::Instant::now();
    crate::morloc_trace!(
        "[fastpath] entering: compressed_payload={} MiB, total_uncompressed={} MiB, frames={}",
        payload_size / (1 << 20),
        total_uncompressed / (1 << 20),
        frames.len()
    );

    // Read the compressed payload region into a userspace buffer
    // BEFORE shmalloc'ing the SHM destination. The decompress step
    // needs both alive at once, but separating allocation in time
    // lowers the worst-case allocator footprint by ~compressed-size
    // (we never hold both `compressed_buf` and the unstarted
    // `shmalloc(total_uncompressed)` reservation simultaneously
    // during the slow shmalloc fault-in phase).
    let t_decomp = std::time::Instant::now();
    let mut compressed_buf = vec![0u8; payload_size];
    let mut total_read = 0usize;
    while total_read < payload_size {
        let n = libc::pread(
            fd,
            compressed_buf.as_mut_ptr().add(total_read) as *mut libc::c_void,
            payload_size - total_read,
            (payload_offset + total_read) as libc::off_t,
        );
        if n <= 0 {
            libc::close(fd);
            return Err(MorlocError::Io(std::io::Error::last_os_error()));
        }
        total_read += n as usize;
    }
    libc::close(fd);

    let dest = match shm::shmalloc(total_uncompressed) {
        Ok(p) => p,
        Err(e) => return Err(e),
    };
    crate::morloc_trace!(
        "[fastpath] read compressed + shmalloc {} MiB took {:.2?}",
        total_uncompressed / (1 << 20),
        t0.elapsed()
    );

    // Decompress all frames in parallel into disjoint slices of the
    // SHM destination. Workers each own a non-overlapping subrange by
    // construction; no inter-worker synchronization on dest.
    let dst_slice = std::slice::from_raw_parts_mut(dest, total_uncompressed);
    if let Err(e) = crate::compression::parallel_decompress_frames(
        &frames,
        &compressed_buf,
        dst_slice,
    ) {
        let _ = shm::shfree(dest);
        return Err(e);
    }
    drop(compressed_buf);
    crate::morloc_trace!(
        "[fastpath] zstd decompress ({} frames, {} workers): wrote {} MiB in {:.2?} ({:.0} MB/s)",
        frames.len(),
        crate::compression::frame_workers().min(frames.len()),
        total_uncompressed / (1 << 20),
        t_decomp.elapsed(),
        (total_uncompressed as f64 / (1 << 20) as f64) / t_decomp.elapsed().as_secs_f64()
    );

    let t_relptr = std::time::Instant::now();
    rebase_voidstar_in_shm(dest, schema, vol_idx_hint)?;
    crate::morloc_trace!("[fastpath] adjust_relptrs took {:.2?}", t_relptr.elapsed());
    crate::morloc_trace!("[fastpath] total ingest: {:.2?}", t0.elapsed());

    Ok(Some(dest))
}

// ── try_decompress_voidstar_bytes_to_shm ──────────────────────────────────

/// In-memory analogue of [`try_load_compressed_voidstar_via_shm`]. For
/// callers (stdin ingest, `--call-packet`, `try_packet_strict`) that
/// have already pulled the compressed bytes off the wire, this lands a
/// MESG+VOIDSTAR+ZSTD payload directly into a fresh SHM allocation
/// without the legacy chain's `Vec`+libc::malloc round-trip.
///
/// Reads from `data` (borrowed, not owned -- caller still owns the
/// buffer on every return path). On `Ok(Some(_))` the SHM region holds
/// the decompressed, relptr-adjusted voidstar; the caller should free
/// its compressed buffer.
///
/// Returns `Ok(None)` -- triggering fall-through to the legacy path --
/// when the header is not MESG+VOIDSTAR+ZSTD or otherwise ineligible.
/// When `pledgedSrcSize` is absent the helper still wins versus the
/// legacy chain by skipping the libc::malloc round-trip (decompress
/// into a Vec, then `shmalloc + memcpy`, drop the Vec immediately).
unsafe fn try_decompress_voidstar_bytes_to_shm(
    data: *const u8,
    data_size: usize,
    schema: *const CSchema,
) -> Result<Option<*mut c_void>, MorlocError> {
    if data.is_null() || data_size < 32 {
        return Ok(None);
    }

    let header_bytes_ref = std::slice::from_raw_parts(data, 32);
    let mut header_bytes = [0u8; 32];
    header_bytes.copy_from_slice(header_bytes_ref);
    let magic = u32::from_le_bytes(header_bytes[..4].try_into().unwrap());
    if magic != packet::PACKET_MAGIC {
        return Ok(None);
    }
    let header = match packet::PacketHeader::from_bytes(&header_bytes) {
        Ok(h) => h,
        Err(_) => return Ok(None),
    };
    if !header.is_data() {
        return Ok(None);
    }
    let source = header.command.data.source;
    let format = header.command.data.format;
    let compression = header.command.data.compression;
    if source != packet::PACKET_SOURCE_MESG
        || format != packet::PACKET_FORMAT_VOIDSTAR
        || compression != packet::PACKET_COMPRESSION_ZSTD
    {
        return Ok(None);
    }

    let metadata_size = header.offset as usize;
    let payload_offset = 32 + metadata_size;
    let payload_size = header.length as usize;
    if payload_offset.saturating_add(payload_size) > data_size {
        return Ok(None);
    }

    let compressed = std::slice::from_raw_parts(data.add(payload_offset), payload_size);
    let full_meta = std::slice::from_raw_parts(data, payload_offset);
    let hint = packet::read_vol_index_from_meta(full_meta)
        .ok()
        .flatten()
        .unwrap_or(0);

    // Frame index is mandatory under the multi-frame format. Without
    // one we have no way to know per-frame sizes; bail to the legacy
    // path (which will raise a hard error -- old single-frame packets
    // are no longer readable under the cutover).
    let frames = match packet::read_frame_index_from_meta(full_meta) {
        Ok(Some(f)) if !f.is_empty() => f,
        _ => return Ok(None),
    };
    let total_uncompressed: usize =
        frames.iter().map(|f| f.uncompressed_size as usize).sum();
    let total_compressed: usize =
        frames.iter().map(|f| f.compressed_size as usize).sum();
    if total_compressed != payload_size {
        return Err(MorlocError::Packet(format!(
            "frame index sums to {} compressed bytes but header.length = {}",
            total_compressed, payload_size
        )));
    }

    let dest = shm::shmalloc(total_uncompressed)?;
    let dst_slice = std::slice::from_raw_parts_mut(dest, total_uncompressed);
    if let Err(e) = crate::compression::parallel_decompress_frames(
        &frames,
        compressed,
        dst_slice,
    ) {
        let _ = shm::shfree(dest);
        return Err(e);
    }

    rebase_voidstar_in_shm(dest, schema, hint)?;
    Ok(Some(dest as *mut c_void))
}

// ── read_voidstar_binary ───────────────────────────────────────────────────

/// Legacy C ABI: reads a VOIDSTAR payload assuming the producer encoded
/// relptrs with vol_idx = 0 (no Layer-3 hint). Forwards to
/// `read_voidstar_binary_with_hint` with hint = 0. Kept for ABI
/// stability with C callers that don't know about the Layer-3 metadata.
///
/// **Callers reading a payload extracted from a morloc packet should
/// prefer `read_voidstar_binary_with_hint` and pass the vol_idx hint
/// recovered from `METADATA_TYPE_VOL_INDEX`** -- otherwise relptrs
/// produced by the Layer-3 emitter (`stream_packet_to_fd` for
/// uncompressed RPTR+VOIDSTAR) will carry stale vol_idx bits that this
/// reader cannot cancel.
#[no_mangle]
pub unsafe extern "C" fn read_voidstar_binary(
    blob: *const u8,
    blob_size: usize,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut c_void {
    read_voidstar_binary_with_hint(blob, blob_size, schema, 0, errmsg)
}

/// Read a VOIDSTAR payload into SHM, accounting for an optional Layer-3
/// `vol_idx_hint` baked into the producer's relptr high bits.
///
/// Delta computation matches the fast-path mmap loader's:
///     delta = abs2rel(dest) - encode_relptr(hint, 0)
/// so for `hint = 0` this reduces to the legacy "buffer-relative ->
/// SHM-relative" add walk, and for `hint > 0` the hint bits cancel
/// against the consumer's slot in a single pass.
#[no_mangle]
pub unsafe extern "C" fn read_voidstar_binary_with_hint(
    blob: *const u8,
    blob_size: usize,
    schema: *const CSchema,
    vol_idx_hint: u16,
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
    let producer_base = shm::encode_relptr(vol_idx_hint as usize, 0);
    let delta = (base_rel as i64).wrapping_sub(producer_base as i64) as shm::RelPtr;

    if let Err(e) = crate::voidstar::adjust_relptrs(base, &rs, delta) {
        let _ = shm::shfree(base);
        set_errmsg(errmsg, &e);
        return ptr::null_mut();
    }

    base as *mut c_void
}

// ── Form decoders ──────────────────────────────────────────────────────────
//
// Each decoder takes the pre-resolved bytes (the source layer has already
// produced them from argv / file / stdin) and emits a voidstar value
// matching the schema. All decoders take ownership of `data` like
// `load_morloc_data_file` does: the function is responsible for freeing it
// on every path.

/// Strict morloc-packet decode. If `data` does not begin with the packet
/// magic (4 bytes), return null with an explanatory error. Otherwise
/// delegate to `load_morloc_data_file`, which already handles every
/// packet variant (MESG/RPTR, compressed/uncompressed).
///
/// Takes ownership of `data` and frees it on every return path.
#[no_mangle]
pub unsafe extern "C" fn try_packet_strict(
    data: *mut u8,
    data_size: usize,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut c_void {
    clear_errmsg(errmsg);
    if data.is_null() || data_size < 4 {
        set_errmsg(
            errmsg,
            &MorlocError::Other(
                "form: packet requires at least 4 bytes of input".into(),
            ),
        );
        if !data.is_null() {
            libc::free(data as *mut c_void);
        }
        return ptr::null_mut();
    }
    let magic = *(data as *const u32);
    if magic != packet::PACKET_MAGIC {
        set_errmsg(
            errmsg,
            &MorlocError::Other(
                "form: packet requires the input to begin with the morloc packet magic".into(),
            ),
        );
        libc::free(data as *mut c_void);
        return ptr::null_mut();
    }
    load_morloc_data_file(ptr::null(), data, data_size, schema, errmsg)
}

/// Format a single field as a JSON value per the field's serial type.
/// `String` fields are JSON-escaped; numeric and other fields are
/// passed through verbatim (JSON's grammar parses bare numerics,
/// booleans, and `null`).
fn format_field_as_json(field: &str, st: crate::schema::SerialType) -> String {
    use crate::schema::SerialType;
    match st {
        SerialType::String | SerialType::IFile
        | SerialType::OStream | SerialType::IStream => {
            // IFile's CLI string view is a file path; escape like a String.
            serde_json::to_string(field).unwrap_or_else(|_| "\"\"".to_string())
        }
        _ => field.to_string(),
    }
}

/// Apply a per-element check against a raw line. Currently only the
/// `path:<perm>` check is supported; the line is interpreted as a
/// filesystem path.
///
/// Path modes follow the Python `open()` convention adapted to a
/// pre-open filesystem predicate:
///
///   r  = exists and readable
///   w  = (exists and writable) OR (does not exist and parent writable)
///   x  = does not exist, parent writable  (exclusive create)
///   rw = exists, readable, writable
unsafe fn apply_list_check(line: &str, check: &ListElemCheck) -> Result<(), String> {
    match check {
        ListElemCheck::Path(perm) => match perm.as_str() {
            "r"  => list_check_r(line),
            "w"  => list_check_w(line),
            "x"  => list_check_x(line),
            "rw" => list_check_r(line).and_then(|_| list_check_w_existing(line)),
            other => Err(format!(
                "path check: unknown mode '{}'; expected one of: r, w, x, rw",
                other
            )),
        },
    }
}

unsafe fn list_c_path(line: &str) -> Result<std::ffi::CString, String> {
    std::ffi::CString::new(line)
        .map_err(|_| "path check: line contains NUL byte".to_string())
}

unsafe fn list_check_r(line: &str) -> Result<(), String> {
    let cp = list_c_path(line)?;
    if libc::access(cp.as_ptr(), libc::R_OK) == 0 {
        Ok(())
    } else {
        Err(format!("path check :r: '{}' is not readable", line))
    }
}

unsafe fn list_check_w_existing(line: &str) -> Result<(), String> {
    let cp = list_c_path(line)?;
    if libc::access(cp.as_ptr(), libc::W_OK) == 0 {
        Ok(())
    } else {
        Err(format!("path check :rw: '{}' is not writable", line))
    }
}

unsafe fn list_check_w(line: &str) -> Result<(), String> {
    let cp = list_c_path(line)?;
    if libc::access(cp.as_ptr(), libc::W_OK) == 0 {
        return Ok(());
    }
    let err = std::io::Error::last_os_error();
    if err.raw_os_error() != Some(libc::ENOENT) {
        return Err(format!(
            "path check :w: '{}' is not writable ({})",
            line, err
        ));
    }
    list_check_parent_writable(line, "w")
}

unsafe fn list_check_x(line: &str) -> Result<(), String> {
    if std::path::Path::new(line).exists() {
        return Err(format!(
            "path check :x: '{}' already exists",
            line
        ));
    }
    list_check_parent_writable(line, "x")
}

unsafe fn list_check_parent_writable(line: &str, mode: &str) -> Result<(), String> {
    let parent = std::path::Path::new(line).parent();
    let parent_dir = match parent {
        Some(p) if !p.as_os_str().is_empty() => p.to_path_buf(),
        _ => std::path::PathBuf::from("."),
    };
    let parent_c = std::ffi::CString::new(parent_dir.to_string_lossy().as_ref())
        .map_err(|_| "path check: parent directory path contains NUL byte".to_string())?;
    if libc::access(parent_c.as_ptr(), libc::W_OK) == 0 {
        Ok(())
    } else {
        Err(format!(
            "path check :{}: parent of '{}' is not writable",
            mode, line
        ))
    }
}

/// Assemble a row's per-column JSON into either a JSON array (for a
/// tuple element schema) or a JSON object using the schema's `keys`
/// (for a record / Map element schema). The wire representation of
/// records is positional, but JSON's representation is key-valued, so
/// the schema's `keys` drive the rebuild on the tabular path.
fn assemble_row_json(
    field_jsons: &[String],
    row_schema: &crate::schema::Schema,
) -> Result<String, MorlocError> {
    use crate::schema::SerialType;
    match row_schema.serial_type {
        SerialType::Tuple => Ok(format!("[{}]", field_jsons.join(","))),
        SerialType::Map => {
            if row_schema.keys.len() != field_jsons.len() {
                return Err(MorlocError::Other(format!(
                    "form: list table fallback: record schema has {} keys but row produced {} values",
                    row_schema.keys.len(),
                    field_jsons.len(),
                )));
            }
            let parts: Vec<String> = row_schema
                .keys
                .iter()
                .zip(field_jsons.iter())
                .map(|(k, v)| {
                    let key_json =
                        serde_json::to_string(k).unwrap_or_else(|_| "\"\"".to_string());
                    format!("{}:{}", key_json, v)
                })
                .collect();
            Ok(format!("{{{}}}", parts.join(",")))
        }
        other => Err(MorlocError::Other(format!(
            "form: list table fallback: row schema must be tuple or record; got {:?}",
            other
        ))),
    }
}

/// Per-line whitespace-delimited split for the table fallback. Each
/// line is split via Rust's `split_whitespace` (one or more characters
/// from `[\s] \ [\n]`, since lines have already been split on `\n`).
/// Empty cells cannot be expressed; column count mismatches surface as
/// an error.
fn split_whitespace_rows(
    lines: &[&str],
    row_schema: &crate::schema::Schema,
) -> Result<Vec<String>, MorlocError> {
    let arity = row_schema.parameters.len();
    if arity == 0 {
        return Err(MorlocError::Other(
            "form: list whitespace fallback: row element type has arity 0".into(),
        ));
    }
    let mut out: Vec<String> = Vec::with_capacity(lines.len());
    for (line_no, line) in lines.iter().enumerate() {
        let fields: Vec<&str> = line.split_whitespace().collect();
        if fields.len() != arity {
            return Err(MorlocError::Other(format!(
                "form: list whitespace fallback: line {} has {} whitespace-separated columns; expected {}",
                line_no + 1,
                fields.len(),
                arity,
            )));
        }
        let json_fields: Vec<String> = fields
            .iter()
            .zip(row_schema.parameters.iter())
            .map(|(f, s)| format_field_as_json(f, s.serial_type))
            .collect();
        out.push(assemble_row_json(&json_fields, row_schema)?);
    }
    Ok(out)
}

/// Parse the input bytes as a headerless delimited table whose row
/// schema is `row_schema` (either a Tuple or a record / Map). Uses
/// `arrow_csv::ReaderBuilder` (the same parser the Arrow Table path
/// uses) with every column typed as `Utf8` so the parser only handles
/// delimiting and quoting; per-slot type formatting is done after the
/// fact via `format_field_as_json`, and `assemble_row_json` wraps each
/// row as either a JSON array (tuple) or a JSON object (record) using
/// `row_schema.keys`.
///
/// `form: list` is **headerless by convention**: rows are raw
/// tuples, not table records, so there is no meaningful place for a
/// column-name row. If a user wants a full-featured table with
/// column names, they should declare the argument as `Table` (which
/// runs through the Arrow Table path and honors headers). Keeping
/// this rule static (no per-arg `--csv-header` toggle) avoids a
/// second knob that would double the surface area of an already
/// weakly-typed loader.
///
/// Returns the per-row JSON strings on success.
fn parse_delimited_rows(
    bytes: &[u8],
    delimiter: u8,
    row_schema: &crate::schema::Schema,
) -> Result<Vec<String>, MorlocError> {
    use arrow_array::{Array, RecordBatch, StringArray};
    use arrow_csv::ReaderBuilder;
    use arrow_schema::{DataType, Field, Schema as ArrowSchema};
    use std::io::Cursor;

    let arity = row_schema.parameters.len();
    if arity == 0 {
        return Err(MorlocError::Other(
            "form: list table fallback: row schema has arity 0".into(),
        ));
    }

    let fields: Vec<Field> = (0..arity)
        .map(|i| Field::new(format!("col{}", i), DataType::Utf8, true))
        .collect();
    let arrow_schema = std::sync::Arc::new(ArrowSchema::new(fields));

    let reader = ReaderBuilder::new(arrow_schema.clone())
        .with_header(false)
        .with_delimiter(delimiter)
        .build(Cursor::new(bytes))
        .map_err(|e| {
            MorlocError::Other(format!(
                "form: list table fallback: failed to open CSV/TSV reader: {}",
                e
            ))
        })?;

    let batches: Vec<RecordBatch> = reader.collect::<Result<Vec<_>, _>>().map_err(|e| {
        MorlocError::Other(format!(
            "form: list table fallback: failed to read delimited rows: {}",
            e
        ))
    })?;

    let mut out: Vec<String> = Vec::new();
    for batch in batches {
        if batch.num_columns() != arity {
            return Err(MorlocError::Other(format!(
                "form: list table fallback: batch has {} columns; expected {}",
                batch.num_columns(),
                arity
            )));
        }
        let cols: Vec<&StringArray> = batch
            .columns()
            .iter()
            .map(|c| {
                c.as_any().downcast_ref::<StringArray>().expect(
                    "ReaderBuilder produced non-Utf8 array despite all-Utf8 schema",
                )
            })
            .collect();
        for row in 0..batch.num_rows() {
            let mut row_fields: Vec<String> = Vec::with_capacity(arity);
            for (col_idx, col) in cols.iter().enumerate() {
                let raw = if col.is_null(row) { "" } else { col.value(row) };
                row_fields.push(format_field_as_json(
                    raw,
                    row_schema.parameters[col_idx].serial_type,
                ));
            }
            out.push(assemble_row_json(&row_fields, row_schema)?);
        }
    }
    Ok(out)
}

/// List form (default per-element config): UTF-8 validate the input,
/// split on `\r?\n`, format each line as an element-level JSON value,
/// and feed the resulting JSON array through `read_json_with_schema`.
/// See `try_list_with_config` for the underlying implementation that
/// honors `list.source:` / `list.check.*:` overrides.
#[no_mangle]
pub unsafe extern "C" fn try_list(
    data: *mut u8,
    data_size: usize,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut c_void {
    try_list_with_config(data, data_size, schema, &ListConfig::default(), errmsg)
}

/// Implementation of `form: list` with the per-element configuration
/// (`list.source:` / `list.form:` / `list.check.*:`) plumbed in. For
/// `Str` elements the line content is JSON-escaped; for everything
/// else the line is treated as JSON for that element type (bare
/// numbers, booleans, or JSON-shaped composite values).
///
/// `list_cfg.source` decides how each line is resolved to bytes:
/// `Inline` uses the line content directly, `File` opens the line as a
/// path and reads the file, `Auto` runs the runtime classifier.
/// `list_cfg.form` then decides how the resolved bytes are interpreted
/// (auto-classified, packet only, bytes hybrid, or bytes-only).
///
/// When the element type is a tuple, the JSON path is tried first.
/// If that fails, the lines are reparsed via TAB, comma, and finally
/// whitespace; whichever yields rows whose rebuilt JSON parses cleanly
/// wins.
///
/// Takes ownership of `data` and frees it on every return path.
unsafe fn try_list_with_config(
    data: *mut u8,
    data_size: usize,
    schema: *const CSchema,
    list_cfg: &ListConfig,
    errmsg: *mut *mut c_char,
) -> *mut c_void {
    use crate::schema::SerialType;

    extern "C" {
        fn read_json_with_schema(
            dest: *mut u8,
            json: *mut c_char,
            schema: *const CSchema,
            errmsg: *mut *mut c_char,
        ) -> *mut u8;
    }

    // RAII guard: `data` is owned by this function (per the contract);
    // every return path frees it via the drop. Subsequent `&[u8]` /
    // `&str` borrows of `data` are valid for as long as the guard
    // lives, which is until the function returns.
    struct LibcBuf(*mut u8);
    impl Drop for LibcBuf {
        fn drop(&mut self) {
            if !self.0.is_null() {
                unsafe { libc::free(self.0 as *mut c_void); }
            }
        }
    }
    let _data_guard = LibcBuf(data);

    clear_errmsg(errmsg);

    let rs = CSchema::to_rust(schema);
    if rs.serial_type != SerialType::Array {
        set_errmsg(
            errmsg,
            &MorlocError::Other(
                "form: list requires a list-typed wire schema".into(),
            ),
        );
        return ptr::null_mut();
    }
    if rs.parameters.is_empty() {
        set_errmsg(
            errmsg,
            &MorlocError::Other(
                "form: list: array schema is missing its element type".into(),
            ),
        );
        return ptr::null_mut();
    }
    let elem_serial = rs.parameters[0].serial_type;
    // The TAB/comma/whitespace fallback only makes sense when each
    // line is the literal value being interpreted (the default
    // inline source). For Auto/File per-element sources, each line is
    // a path/inline-value reference that has already been resolved to
    // a JSON-encoded element, so a row-table interpretation of the
    // raw text would be misleading. Both tuples and records (Map)
    // are valid row shapes; they share the same on-wire positional
    // representation and `assemble_row_json` picks the appropriate
    // JSON form per the element schema.
    let tabular_row_eligible =
        matches!(elem_serial, SerialType::Tuple | SerialType::Map);
    let tuple_fallback_available =
        tabular_row_eligible && list_cfg.source == ListElemSource::Inline;

    let slice = if data.is_null() {
        &[][..]
    } else {
        std::slice::from_raw_parts(data, data_size)
    };
    let text: &str = match std::str::from_utf8(slice) {
        Ok(t) => t,
        Err(e) => {
            set_errmsg(
                errmsg,
                &MorlocError::Other(format!(
                    "form: list: input is not valid UTF-8 ({})",
                    e
                )),
            );
            return ptr::null_mut();
        }
    };

    // Split on '\n'. Tolerate trailing '\r' on each line and drop a
    // single trailing empty line (the common case of a file ending
    // with '\n'). Blank lines mid-file are preserved.
    let mut split_lines: Vec<&str> = text.split('\n').collect();
    if split_lines.last().map(|s| s.is_empty()).unwrap_or(false) {
        split_lines.pop();
    }
    let lines: Vec<&str> = split_lines
        .iter()
        .map(|s| s.strip_suffix('\r').unwrap_or(s))
        .collect();

    // Per-line checks. The only check kind in v1 is `path:<perm>`, so
    // the check applies to the raw line content as a path.
    if !list_cfg.checks.is_empty() {
        for (line_no, line) in lines.iter().enumerate() {
            for check in &list_cfg.checks {
                if let Err(e) = apply_list_check(line, check) {
                    set_errmsg(
                        errmsg,
                        &MorlocError::Other(format!(
                            "form: list: element {}: {}",
                            line_no + 1,
                            e
                        )),
                    );
                    return ptr::null_mut();
                }
            }
        }
    }

    // Per-element source resolution. When `list.source` is Auto or
    // File, each line is treated as an argv token: Auto runs the same
    // classifier the top-level `auto` source uses; File forces the line
    // to be a path. Each line's resulting voidstar is round-tripped
    // through JSON so the existing outer-array path can collect them
    // uniformly.
    // When the element type is `au1` (Vec<UInt8>), each line whose bytes
    // are not JSON-shaped is treated as the raw element bytes: the line
    // `hello` becomes `[104,101,108,108,111]`. The schema unambiguously
    // identifies this case; the user doesn't need to write a separate
    // `list.form: bytes`.
    let is_au1_element = elem_serial == SerialType::Array
        && !rs.parameters[0].parameters.is_empty()
        && rs.parameters[0].parameters[0].serial_type == SerialType::Uint8;

    let parts: Vec<String> = if list_cfg.source == ListElemSource::Inline {
        let mut p: Vec<String> = Vec::with_capacity(lines.len());
        for line in &lines {
            let v = if is_au1_element && !line.trim_start().starts_with('[') {
                let bytes_str: Vec<String> =
                    line.as_bytes().iter().map(|b| b.to_string()).collect();
                format!("[{}]", bytes_str.join(","))
            } else {
                match elem_serial {
                    SerialType::String | SerialType::IFile
        | SerialType::OStream | SerialType::IStream =>
                        format_field_as_json(line, elem_serial),
                    _ => line.to_string(),
                }
            };
            p.push(v);
        }
        p
    } else {
        let elem_c_schema = CSchema::from_rust(&rs.parameters[0]);
        let mut p: Vec<String> = Vec::with_capacity(lines.len());
        let mut elem_err: Option<MorlocError> = None;
        for (line_no, line) in lines.iter().enumerate() {
            let line_c = match std::ffi::CString::new(line.as_bytes()) {
                Ok(c) => c,
                Err(_) => {
                    elem_err = Some(MorlocError::Other(format!(
                        "form: list: element {}: argv contains a NUL byte",
                        line_no + 1
                    )));
                    break;
                }
            };
            let mut line_err: *mut c_char = ptr::null_mut();
            // Per-line dispatch is driven by `list_cfg.form`. For the
            // default `Auto`, we route the line as an argv through the
            // classify-or-force-File pipeline (existing behavior). For
            // an explicit bytes-family form, the line is treated as a
            // path (compile-time validation guarantees
            // `list_cfg.source == File` here): open the file, read its
            // bytes, and dispatch through the matching decoder.
            let voidstar = match list_cfg.form {
                ListElemForm::Auto => match list_cfg.source {
                    ListElemSource::File => parse_cli_data_argument_classified(
                        ptr::null_mut(),
                        Classified { kind: ArgSource::File, effective: line_c.as_ptr() },
                        elem_c_schema,
                        &mut line_err,
                    ),
                    ListElemSource::Auto => parse_cli_data_argument_singular(
                        ptr::null_mut(),
                        line_c.as_ptr() as *mut c_char,
                        elem_c_schema,
                        &mut line_err,
                    ),
                    ListElemSource::Inline => unreachable!(),
                },
                ListElemForm::Bytes | ListElemForm::BytesOnly | ListElemForm::Packet => {
                    let (buf, sz) = match read_path_into_libc(line_c.as_ptr()) {
                        Ok(pair) => pair,
                        Err(reason) => {
                            elem_err = Some(MorlocError::Other(format!(
                                "form: list: element {}: {}",
                                line_no + 1, reason
                            )));
                            drop(line_c);
                            break;
                        }
                    };
                    let voidstar_c = match list_cfg.form {
                        ListElemForm::Bytes =>
                            try_bytes_hybrid(buf, sz, elem_c_schema, &mut line_err),
                        ListElemForm::BytesOnly =>
                            try_bytes_only(buf, sz, elem_c_schema, &mut line_err),
                        ListElemForm::Packet =>
                            try_packet_strict(buf, sz, elem_c_schema, &mut line_err),
                        ListElemForm::Auto => unreachable!(),
                    };
                    voidstar_c as *mut u8
                }
            };
            drop(line_c);
            if voidstar.is_null() {
                let m = if line_err.is_null() {
                    "unknown error".to_string()
                } else {
                    let s = CStr::from_ptr(line_err).to_string_lossy().into_owned();
                    libc::free(line_err as *mut c_void);
                    s
                };
                elem_err = Some(MorlocError::Other(format!(
                    "form: list: element {}: {}",
                    line_no + 1,
                    m
                )));
                break;
            }
            match crate::json::voidstar_to_json_string(
                voidstar as crate::shm::AbsPtr,
                &rs.parameters[0],
            ) {
                Ok(js) => p.push(js),
                Err(e) => {
                    elem_err = Some(MorlocError::Other(format!(
                        "form: list: element {}: voidstar->json failed: {}",
                        line_no + 1,
                        e
                    )));
                    break;
                }
            }
        }
        CSchema::free(elem_c_schema);
        if let Some(e) = elem_err {
            set_errmsg(errmsg, &e);
            return ptr::null_mut();
        }
        p
    };

    let json_array = format!("[{}]", parts.join(","));

    let json_c = match std::ffi::CString::new(json_array) {
        Ok(c) => c,
        Err(_) => {
            set_errmsg(
                errmsg,
                &MorlocError::Other(
                    "form: list: built JSON contains an interior NUL byte".into(),
                ),
            );
            return ptr::null_mut();
        }
    };

    let dest = match shm::shcalloc(1, rs.width) {
        Ok(p) => p,
        Err(e) => {
            set_errmsg(errmsg, &e);
            return ptr::null_mut();
        }
    };

    let mut err: *mut c_char = ptr::null_mut();
    let result = read_json_with_schema(
        dest,
        json_c.as_ptr() as *mut c_char,
        schema,
        &mut err,
    );
    drop(json_c);
    if err.is_null() {
        return result as *mut c_void;
    }
    // JSON pass failed.
    let _ = shm::shfree(dest);
    if !tuple_fallback_available {
        *errmsg = err;
        return ptr::null_mut();
    }

    // Pass 2-4 (delimited table via arrow-csv + whitespace split):
    // drop the prior error and retry the lines as a headerless
    // delimited table. Try TAB, comma, and whitespace in order; the
    // first delimiter whose rows pass through `read_json_with_schema`
    // wins. arrow-csv handles quoting/escaping for TAB and comma;
    // whitespace uses `split_whitespace` per line (no quoting; runs of
    // non-newline whitespace collapse to a single delimiter, but empty
    // cells cannot be expressed -- that's a documented limitation).
    libc::free(err as *mut c_void);

    let row_schema = &rs.parameters[0];
    let mut last_err: Option<MorlocError> = None;

    enum Delim {
        Csv(u8),
        Whitespace,
    }
    let attempts: [Delim; 3] = [Delim::Csv(b'\t'), Delim::Csv(b','), Delim::Whitespace];

    for delim in &attempts {
        let row_jsons = match delim {
            Delim::Csv(d) => parse_delimited_rows(text.as_bytes(), *d, row_schema),
            Delim::Whitespace => split_whitespace_rows(&lines, row_schema),
        };
        let rows = match row_jsons {
            Ok(v) => v,
            Err(e) => {
                last_err = Some(e);
                continue;
            }
        };
        let outer = format!("[{}]", rows.join(","));
        let outer_c = match std::ffi::CString::new(outer) {
            Ok(c) => c,
            Err(_) => {
                last_err = Some(MorlocError::Other(
                    "form: list table fallback: rebuilt JSON contains an interior NUL byte".into(),
                ));
                continue;
            }
        };
        let dest_n = match shm::shcalloc(1, rs.width) {
            Ok(p) => p,
            Err(e) => {
                last_err = Some(e);
                continue;
            }
        };
        let mut err_n: *mut c_char = ptr::null_mut();
        let result_n = read_json_with_schema(
            dest_n,
            outer_c.as_ptr() as *mut c_char,
            schema,
            &mut err_n,
        );
        drop(outer_c);
        if err_n.is_null() {
            return result_n as *mut c_void;
        }
        let _ = shm::shfree(dest_n);
        let msg = CStr::from_ptr(err_n).to_string_lossy().into_owned();
        libc::free(err_n as *mut c_void);
        last_err = Some(MorlocError::Other(msg));
    }

    let final_err = last_err.unwrap_or_else(|| {
        MorlocError::Other(
            "form: list: no delimiter (tab, comma, whitespace) matched the expected row schema".into(),
        )
    });
    // Very common user mistake: the first line is a column-name
    // header. `form: list` is headerless by convention (see
    // `parse_delimited_rows`); detect the shape and append a hint.
    let hinted = augment_with_header_hint(final_err, &lines, row_schema);
    set_errmsg(errmsg, &hinted);
    ptr::null_mut()
}

/// If the first line's cells look like column names (non-numeric
/// text where the row schema expects a numeric type in that
/// position), return the original error with a header-row hint
/// appended. Otherwise return the error unchanged. The probe is
/// deliberately conservative: only fires when at least one cell is
/// clearly a name (letters, no digit prefix) opposite an expected
/// numeric slot, so a genuine text row is not mistaken for a header.
fn augment_with_header_hint(
    err: MorlocError,
    lines: &[&str],
    row_schema: &crate::schema::Schema,
) -> MorlocError {
    let first = match lines.iter().find(|l| !l.trim().is_empty()) {
        Some(l) => l.trim(),
        None => return err,
    };
    let arity = row_schema.parameters.len();
    if arity < 2 {
        return err;
    }
    // Try the same delimiters as `parse_delimited_rows` /
    // `split_whitespace_rows`; use the first split whose cell count
    // matches the expected row arity.
    let by_tab: Vec<&str> = first.split('\t').collect();
    let by_comma: Vec<&str> = first.split(',').collect();
    let by_ws: Vec<&str> = first.split_whitespace().collect();
    let cells: &[&str] = if by_tab.len() == arity {
        &by_tab
    } else if by_comma.len() == arity {
        &by_comma
    } else if by_ws.len() == arity {
        &by_ws
    } else {
        return err;
    };
    // Heuristic: at least one cell is a plain identifier (letters
    // only) opposite an expected numeric column.
    let looks_like_header = cells
        .iter()
        .zip(row_schema.parameters.iter())
        .any(|(cell, col)| {
            let cell_trim = cell.trim();
            let is_name = !cell_trim.is_empty()
                && cell_trim.chars().next().map_or(false, |c| c.is_ascii_alphabetic())
                && cell_trim.chars().all(|c| !c.is_ascii_digit());
            is_numeric_serial(&col.serial_type) && is_name
        });
    if !looks_like_header {
        return err;
    }
    MorlocError::Other(format!(
        "{}\n  hint: the first row (`{}`) looks like a column-name \
         header. `form: list` is headerless -- remove the header row, \
         or declare the argument as `Table` if you need column names.",
        err, first,
    ))
}

/// True iff the serial type is a numeric scalar (signed / unsigned
/// integer, floating-point, or arbitrary-precision `Int`). Distinct
/// from 'is_fixed_width_scalar', which admits `Bool` and excludes
/// `Int`.
fn is_numeric_serial(t: &crate::schema::SerialType) -> bool {
    use crate::schema::SerialType::*;
    matches!(
        t,
        Sint8 | Sint16 | Sint32 | Sint64
        | Uint8 | Uint16 | Uint32 | Uint64
        | Float32 | Float64
        | Int
    )
}

/// True iff the element type is a numeric scalar eligible for
/// `form: bytes` / `form: bytes-only`. `Schema::is_fixed_width` also
/// admits `Nil` (width 0) and `Tuple` of fixed-width leaves; we want
/// strictly the numeric scalars in v1.
fn is_fixed_width_scalar(elem: &crate::schema::SerialType) -> bool {
    use crate::schema::SerialType::*;
    matches!(elem,
        Bool
        | Sint8 | Sint16 | Sint32 | Sint64
        | Uint8 | Uint16 | Uint32 | Uint64
        | Float32 | Float64
    )
}

fn fixed_width_scalar_name(elem: &crate::schema::SerialType) -> &'static str {
    use crate::schema::SerialType::*;
    match elem {
        Bool => "Bool",
        Sint8 => "I8",   Sint16 => "I16", Sint32 => "I32", Sint64 => "I64",
        Uint8 => "U8",  Uint16 => "U16",Uint32 => "U32",Uint64 => "U64",
        Float32 => "F32", Float64 => "F64",
        _ => "non-scalar",
    }
}

/// Raw-bytes form (`form: bytes-only`): copy `data` into a freshly-
/// allocated array voidstar. The schema must be `a<scalar>` where
/// `<scalar>` is a fixed-width scalar (Bool, sized Int/UInt, sized
/// Float). The total byte count must be a multiple of the element
/// width. For `[Bool]`, every byte must be 0 or 1.
///
/// Takes ownership of `data` and frees it on every return path.
unsafe fn try_bytes_only(
    data: *mut u8,
    data_size: usize,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut c_void {
    use crate::schema::SerialType;
    use crate::shm::Array;
    clear_errmsg(errmsg);

    let rs = CSchema::to_rust(schema);
    let is_array_of_scalar = rs.serial_type == SerialType::Array
        && rs.parameters.len() == 1;
    if !is_array_of_scalar {
        set_errmsg(errmsg, &MorlocError::Other(
            "form: bytes requires the wire schema to be a<scalar>".into(),
        ));
        if !data.is_null() { libc::free(data as *mut c_void); }
        return ptr::null_mut();
    }
    let elem_type = rs.parameters[0].serial_type;
    if !is_fixed_width_scalar(&elem_type) {
        set_errmsg(errmsg, &MorlocError::Other(format!(
            "form: bytes requires a fixed-width scalar element; got `{}`",
            fixed_width_scalar_name(&elem_type),
        )));
        if !data.is_null() { libc::free(data as *mut c_void); }
        return ptr::null_mut();
    }
    let elem_width = rs.parameters[0].width;

    if data_size % elem_width != 0 {
        set_errmsg(errmsg, &MorlocError::Other(format!(
            "form: bytes: input has {} bytes, expected a multiple of {} \
             (the element width for `[{}]`)",
            data_size, elem_width, fixed_width_scalar_name(&elem_type),
        )));
        if !data.is_null() { libc::free(data as *mut c_void); }
        return ptr::null_mut();
    }

    if elem_type == SerialType::Bool {
        for i in 0..data_size {
            let b = *data.add(i);
            if b > 1 {
                set_errmsg(errmsg, &MorlocError::Other(format!(
                    "form: bytes: byte at offset {} is 0x{:02x}; [Bool] requires 0 or 1",
                    i, b,
                )));
                libc::free(data as *mut c_void);
                return ptr::null_mut();
            }
        }
    }

    let elem_count = data_size / elem_width;
    let header_size = std::mem::size_of::<Array>();
    let total = header_size.saturating_add(data_size);
    let base = match shm::shmalloc(total) {
        Ok(p) => p,
        Err(e) => {
            set_errmsg(errmsg, &e);
            if !data.is_null() { libc::free(data as *mut c_void); }
            return ptr::null_mut();
        }
    };
    let payload_ptr = base.add(header_size);
    let payload_rel = match shm::abs2rel(payload_ptr) {
        Ok(r) => r,
        Err(e) => {
            let _ = shm::shfree(base);
            set_errmsg(errmsg, &e);
            if !data.is_null() { libc::free(data as *mut c_void); }
            return ptr::null_mut();
        }
    };
    let arr = base as *mut Array;
    (*arr).size = elem_count;
    (*arr).data = payload_rel;
    if data_size > 0 && !data.is_null() {
        std::ptr::copy_nonoverlapping(data, payload_ptr, data_size);
    }
    if !data.is_null() { libc::free(data as *mut c_void); }
    base as *mut c_void
}

/// Hybrid bytes form (`form: bytes`): sniff packet magic; if present,
/// decode as a packet. Otherwise fall back to `try_bytes_only`. This is
/// the friendly default for binary file input; the user's
/// `bytes-only` opts out of the sniff.
///
/// Takes ownership of `data` and frees it on every return path.
unsafe fn try_bytes_hybrid(
    data: *mut u8,
    data_size: usize,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut c_void {
    clear_errmsg(errmsg);
    if data_size >= 4 {
        let magic = *(data as *const u32);
        if magic == packet::PACKET_MAGIC {
            return load_morloc_data_file(ptr::null(), data, data_size, schema, errmsg);
        }
    }
    try_bytes_only(data, data_size, schema, errmsg)
}

// ── Schema-check helper for packet loads ──────────────────────────────────

/// Compare the schema string embedded in a morloc packet's metadata
/// block to the caller's requested schema. Returns `Ok(())` on match
/// or when there's nothing to compare against (missing metadata entry,
/// null requested schema). Returns `Err(MorlocError::UserThrow(_))`
/// on mismatch so `@catch` can intercept.
///
/// Without this check, `load_morloc_data_file`'s format-specific
/// decoders would silently reinterpret payload bytes under the wrong
/// schema (e.g. reading a Str-packet's Array header as an Int64),
/// producing garbage instead of a catchable failure.
///
/// Comparison is byte-for-byte on the canonical `schema_to_string`
/// form. Both sides go through the same renderer, which no longer
/// emits language-specific `<hint>` prefixes (hints are pool-local
/// codegen state, not wire data), so cross-language save/load pairs
/// compare equal for identical wire types.
unsafe fn check_packet_schema_matches(
    meta_bytes: &[u8],
    requested_schema: *const CSchema,
    path_hint: &str,
) -> Result<(), MorlocError> {
    use morloc_runtime_types::packet::{
        decode_schema_entry, iter_metadata, METADATA_TYPE_SCHEMA_STRING,
    };
    use morloc_runtime_types::schema::schema_to_string;

    let stored = iter_metadata(meta_bytes)
        .find_map(|(kind, data)| {
            if kind == METADATA_TYPE_SCHEMA_STRING {
                Some(decode_schema_entry(data))
            } else {
                None
            }
        });

    let stored = match stored {
        Some(s) => s,
        None => return Ok(()), // no schema entry to compare against
    };

    if requested_schema.is_null() {
        return Ok(());
    }

    let requested_rs = CSchema::to_rust(requested_schema);
    let requested = schema_to_string(&requested_rs);

    if stored != requested {
        let path_prefix = if path_hint.is_empty() {
            String::new()
        } else {
            format!(" reading '{}'", path_hint)
        };
        return Err(MorlocError::UserThrow(format!(
            "@load: schema mismatch{}: file has schema `{}`, requested `{}`",
            path_prefix, stored, requested,
        )));
    }
    Ok(())
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
/// The single entry point that loads a morloc data file's bytes into
/// SHM. Every caller that wants to *use* on-disk bytes as a morloc
/// value (CLI argument parsing, @load intrinsic, bundle reader,
/// --call-packet input) funnels through here. The function:
///
///  1. Decompresses the bytes if they are a compressed morloc packet
///     (the only function that does so; all other callers should be
///     funneling through this one rather than duplicating the step).
///  2. Dispatches on extension / content magic and produces either
///     an SHM-resident voidstar or a parsed JSON/MSGPACK/Arrow result.
///
/// Sites that read packet bytes for *forwarding* (cache.rs returning
/// cached blobs to a downstream packet builder, slurm_ffi shipping
/// arg packets to a remote worker) do not go through this function
/// because they do not unpack -- they pass the bytes through. Those
/// paths are uncompressed by convention (their writers are
/// uncompressed, e.g. `put_cache_packet`) so they currently need no
/// decompression. If a writer ever starts emitting compressed cache
/// or slurm artifacts, the read sites in those files would need their
/// own decompress call -- not this function.
pub unsafe extern "C" fn load_morloc_data_file(
    path: *const c_char,
    mut data: *mut u8,
    mut data_size: usize,
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

    // SHM-direct path for compressed MESG+VOIDSTAR packets. When the
    // bytes carry a header marking them as MESG+VOIDSTAR+ZSTD we can
    // decompress straight into a fresh SHM allocation and return early,
    // skipping the legacy chain's Vec + libc::malloc + memcpy round-trip
    // entirely. The helper is a no-op (returns Ok(None)) for anything
    // else, so other compressed formats (JSON/msgpack/Arrow) still flow
    // through the generic decompression below.
    if !data.is_null() && data_size >= 32 {
        match try_decompress_voidstar_bytes_to_shm(data, data_size, schema) {
            Ok(Some(voidstar)) => {
                // Free the compressed input now that the decompressed
                // payload lives in SHM. This drops peak resident set to
                // 1X before the caller hands the voidstar to the pool.
                libc::free(data as *mut c_void);
                return voidstar;
            }
            Ok(None) => { /* fall through to generic decompression */ }
            Err(e) => {
                libc::free(data as *mut c_void);
                set_errmsg(errmsg, &e);
                return ptr::null_mut();
            }
        }
    }

    // Decompress in place if the buffer is a compressed morloc packet.
    // This is the single choke point: every caller that hands a buffer
    // to load_morloc_data_file gets decompression for free, so we do
    // not duplicate the decompress logic at each call site.
    //
    // `data` may be swapped for a fresh libc allocation containing the
    // decompressed bytes; the function's contract that the caller has
    // transferred ownership of `data` is preserved -- we libc::free the
    // original and adopt the new buffer for the rest of the call.
    if !data.is_null() && data_size > 0 {
        let raw_slice = std::slice::from_raw_parts(data, data_size);
        match crate::compression::decompress_packet_if_needed(raw_slice) {
            Ok(std::borrow::Cow::Borrowed(_)) => { /* no-op */ }
            Ok(std::borrow::Cow::Owned(bytes)) => {
                let new_size = bytes.len();
                let new_buf = libc::malloc(new_size) as *mut u8;
                if new_buf.is_null() {
                    libc::free(data as *mut c_void);
                    set_errmsg(errmsg, &MorlocError::Other(
                        "malloc failed during packet decompression".into()));
                    return ptr::null_mut();
                }
                ptr::copy_nonoverlapping(bytes.as_ptr(), new_buf, new_size);
                libc::free(data as *mut c_void);
                data = new_buf;
                data_size = new_size;
            }
            Err(e) => {
                libc::free(data as *mut c_void);
                set_errmsg(errmsg, &e);
                return ptr::null_mut();
            }
        }
    }

    // Primitive parsing rule for `Str` (and `?Str`). When the schema
    // reduces to the bare string primitive and the bytes don't start
    // with a JSON-string opening quote, treat the bytes as the literal
    // Str value. Without this, `source: file` on a Str arg would fail
    // because file contents like `hello world` aren't JSON-shaped.
    if !schema.is_null() && data_size > 0 {
        use crate::schema::SerialType;
        let rs = CSchema::to_rust(schema);
        let bare_str = match rs.serial_type {
            SerialType::String | SerialType::IFile
        | SerialType::OStream | SerialType::IStream => true,
            SerialType::Optional => rs
                .parameters
                .first()
                .map(|p| matches!(p.serial_type, SerialType::String | SerialType::IFile
                    | SerialType::OStream | SerialType::IStream))
                .unwrap_or(false),
            _ => false,
        };
        if bare_str {
            let bytes = std::slice::from_raw_parts(data, data_size);
            let first_non_ws = bytes
                .iter()
                .find(|b| !b.is_ascii_whitespace())
                .copied();
            let looks_json_string = first_non_ws == Some(b'"');
            if !looks_json_string {
                match std::str::from_utf8(bytes) {
                    Ok(s) => {
                        // Strip a single trailing newline (the common
                        // case of a text file ending with `\n`).
                        let s = s.strip_suffix('\n').unwrap_or(s);
                        let s = s.strip_suffix('\r').unwrap_or(s);
                        let json_quoted = serde_json::to_string(s).unwrap_or_default();
                        let json_c = match std::ffi::CString::new(json_quoted) {
                            Ok(c) => c,
                            Err(_) => {
                                libc::free(data as *mut c_void);
                                set_errmsg(
                                    errmsg,
                                    &MorlocError::Other(
                                        "Str contents contain an interior NUL byte"
                                            .into(),
                                    ),
                                );
                                return ptr::null_mut();
                            }
                        };
                        let dest = match shm::shcalloc(1, rs.width) {
                            Ok(p) => p,
                            Err(e) => {
                                libc::free(data as *mut c_void);
                                set_errmsg(errmsg, &e);
                                return ptr::null_mut();
                            }
                        };
                        let mut err: *mut c_char = ptr::null_mut();
                        let result = read_json_with_schema(
                            dest,
                            json_c.as_ptr() as *mut c_char,
                            schema,
                            &mut err,
                        );
                        drop(json_c);
                        libc::free(data as *mut c_void);
                        if !err.is_null() {
                            let _ = shm::shfree(dest);
                            *errmsg = err;
                            return ptr::null_mut();
                        }
                        return result as *mut c_void;
                    }
                    Err(_) => {
                        // Not valid UTF-8: fall through to the existing
                        // handlers, which will surface the format error.
                    }
                }
            }
        }
    }

    // Primitive parsing rule for `Real` / `Float32` (and their optional
    // forms). The JSON wire layer encodes IEEE 754 specials as
    // lowercase quoted strings (`"inf"` / `"-inf"` / `"nan"`); this
    // hook recognizes the bare literals at the file / stdin boundary
    // and produces the JSON-string form before handing off to the
    // standard parser. The `\-inf` form is also accepted -- the leading
    // backslash is the shell-safe escape for the `-i`/`-n`/`-f` flag
    // cluster.
    if !schema.is_null() && data_size > 0 {
        use crate::schema::SerialType;
        let rs = CSchema::to_rust(schema);
        let bare_float = match rs.serial_type {
            SerialType::Float32 | SerialType::Float64 => true,
            SerialType::Optional => rs
                .parameters
                .first()
                .map(|p| {
                    p.serial_type == SerialType::Float32
                        || p.serial_type == SerialType::Float64
                })
                .unwrap_or(false),
            _ => false,
        };
        if bare_float {
            let bytes = std::slice::from_raw_parts(data, data_size);
            if let Ok(s) = std::str::from_utf8(bytes) {
                let trimmed = s.trim();
                // `\-inf` is the shell-safe spelling of `-inf` (the
                // leading backslash bypasses clap's flag-cluster
                // interpretation of the bare `-i`/`-n`/`-f`).
                let stripped = trimmed.strip_prefix('\\').unwrap_or(trimmed);
                let inner: Option<&str> = match stripped {
                    "inf" => Some("inf"),
                    "-inf" => Some("-inf"),
                    "nan" => Some("nan"),
                    _ => None,
                };
                if let Some(token) = inner {
                    let json_quoted = format!("\"{}\"", token);
                    let json_c = match std::ffi::CString::new(json_quoted) {
                        Ok(c) => c,
                        Err(_) => {
                            libc::free(data as *mut c_void);
                            set_errmsg(
                                errmsg,
                                &MorlocError::Other(
                                    "float-special JSON contains an interior NUL byte"
                                        .into(),
                                ),
                            );
                            return ptr::null_mut();
                        }
                    };
                    let dest = match shm::shcalloc(1, rs.width) {
                        Ok(p) => p,
                        Err(e) => {
                            libc::free(data as *mut c_void);
                            set_errmsg(errmsg, &e);
                            return ptr::null_mut();
                        }
                    };
                    let mut err: *mut c_char = ptr::null_mut();
                    let result = read_json_with_schema(
                        dest,
                        json_c.as_ptr() as *mut c_char,
                        schema,
                        &mut err,
                    );
                    drop(json_c);
                    libc::free(data as *mut c_void);
                    if !err.is_null() {
                        let _ = shm::shfree(dest);
                        *errmsg = err;
                        return ptr::null_mut();
                    }
                    return result as *mut c_void;
                }
            }
        }
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

    // Callers that resolved bytes without a path (e.g. `try_packet_strict`
    // on a per-line list element) pass a null `path` pointer. Skip the
    // extension-based dispatch entirely in that case and fall straight
    // through to the magic-byte-driven packet / format detection below.
    let path_str: std::borrow::Cow<str> = if path.is_null() {
        std::borrow::Cow::Borrowed("")
    } else {
        CStr::from_ptr(path).to_string_lossy()
    };
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
                // Compare the packet's stored schema descriptor to the
                // caller's requested schema. Mismatch is user-attributable
                // (loading the wrong type from a file) so surface as
                // UserThrow via errmsg; @catch intercepts.
                if offset > 0 {
                    let meta_slice = std::slice::from_raw_parts(data.add(32), offset);
                    if let Err(e) = check_packet_schema_matches(meta_slice, schema, &path_str) {
                        libc::free(data as *mut c_void);
                        set_errmsg(errmsg, &e);
                        return ptr::null_mut();
                    }
                }
                let payload = data.add(32 + offset);
                let format = { header.command.data.format };

                if format == packet::PACKET_FORMAT_VOIDSTAR {
                    // Pull the Layer-3 vol_idx hint out of the metadata
                    // block if present; the legacy reader otherwise
                    // miscounts the producer's high bits.
                    let packet_slice = std::slice::from_raw_parts(data, data_size);
                    let hint = packet::read_vol_index_from_meta(packet_slice)
                        .ok().flatten().unwrap_or(0);
                    let result = read_voidstar_binary_with_hint(
                        payload, length, schema, hint, &mut err,
                    );
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
        SerialType::IFile | SerialType::OStream | SerialType::IStream => {
            // Tagged stream-handle field: copy the 16-byte inline, then
            // for TAG_PATH copy the `{size, bytes}` suballoc and rebase
            // the payload relptr. TAG_HANDLE has no suballoc.
            use morloc_runtime_types::stream_handle as sh;
            if (data as usize + rs.width - 1) <= data_end {
                return Err(MorlocError::Packet(
                    "Data is too small to store a stream-handle field".into(),
                ));
            }
            ptr::copy_nonoverlapping(data, dest, rs.width);
            let dest_field = dest;
            let tag = sh::read_tag(dest_field);
            let payload = sh::read_payload(dest_field);
            if tag == sh::TAG_PATH && payload != shm::RELNULL as u64 {
                let src_suballoc = data.add(payload as usize);
                if (src_suballoc as usize + 7) > data_end {
                    return Err(MorlocError::Packet(
                        "Data is too small to contain stream-handle path header".into(),
                    ));
                }
                let path_len = sh::read_path_size(src_suballoc) as usize;
                let total = sh::path_suballoc_size(path_len);
                if (src_suballoc as usize + total - 1) > data_end {
                    return Err(MorlocError::Packet(
                        "Data is too small to contain stream-handle path bytes".into(),
                    ));
                }
                let new_block = shm::shmemcpy(src_suballoc, total)?;
                sh::write_field(
                    dest_field,
                    sh::TAG_PATH,
                    shm::abs2rel(new_block)? as u64,
                );
            }
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
    dest: *mut u8,
    arg: *mut c_char,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);
    let classified = match classify_arg_source(arg) {
        Ok(c) => c,
        Err(e) => { set_errmsg(errmsg, &e); return ptr::null_mut(); }
    };
    parse_cli_data_argument_classified(dest, classified, schema, errmsg)
}

/// Body of [`parse_cli_data_argument_singular`] for callers that already
/// know the source classification (e.g. the `form: list` per-line
/// dispatcher, which forces `File` mode without going through the auto
/// classifier).
unsafe fn parse_cli_data_argument_classified(
    mut dest: *mut u8,
    classified: Classified,
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

    let effective = classified.effective;
    match classified.kind {
        ArgSource::Stdin => {
            if let Err(e) = claim_stdin() {
                set_errmsg(errmsg, &e);
                return ptr::null_mut();
            }
            fd = libc::fdopen(libc::STDIN_FILENO, b"rb\0".as_ptr() as *const c_char);
        }
        ArgSource::File => {
            // Layer 2 fast path: if the file is a regular file holding
            // a large MESG+VOIDSTAR morloc packet, pread its payload
            // straight into a fresh SHM allocation and skip the
            // libc::malloc + read + shmalloc + memcpy chain.
            match try_load_voidstar_packet_via_mmap(effective, schema) {
                Ok(Some(payload_ptr)) => return payload_ptr,
                Ok(None) => { /* fall through */ }
                Err(e) => {
                    set_errmsg(errmsg, &e);
                    return ptr::null_mut();
                }
            }
            // Compressed-VOIDSTAR fast path: same shape as the
            // uncompressed mmap fast path but stream-decompresses the
            // file's zstd frame directly into SHM. Holds the file in
            // a BufReader and the SHM destination only; no Vec or
            // libc::malloc round-trip for the decompressed payload.
            match try_load_compressed_voidstar_via_shm(effective, schema) {
                Ok(Some(payload_ptr)) => return payload_ptr,
                Ok(None) => { /* fall through to read path */ }
                Err(e) => {
                    set_errmsg(errmsg, &e);
                    return ptr::null_mut();
                }
            }
            fd = libc::fopen(effective, b"rb\0".as_ptr() as *const c_char);
            if fd.is_null() {
                set_errmsg(errmsg, &MorlocError::Other(
                    format!("The argument '{}' is a filename, but it can't be read",
                        CStr::from_ptr(effective).to_string_lossy())
                ));
                return ptr::null_mut();
            }
        }
        ArgSource::Inline => {}
    }

    if fd.is_null() {
        // Literal JSON data
        if crate::arrow_ffi::is_arrow_table_schema(&rs) {
            let relptr = crate::arrow_ffi::read_json_to_arrow_shm(effective as *const c_char, schema, &mut err);
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
        dest = read_json_with_schema(dest, effective as *mut c_char, schema, &mut err);
        if !err.is_null() {
            *errmsg = err;
            return ptr::null_mut();
        }
        return dest;
    }

    // File or stdin
    let source_label: String = match classified.kind {
        ArgSource::Stdin => "stdin".to_string(),
        ArgSource::File => format!("file '{}'", CStr::from_ptr(effective).to_string_lossy()),
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

    // Decompression is handled inside `load_morloc_data_file` (the
    // single choke point all packet-load paths go through). The RPTR
    // special case below runs before that, so a compressed RPTR
    // packet (rare -- the producer normalizes RPTR to MESG before
    // writing) falls through to `load_morloc_data_file` and is
    // decompressed there. The branch below fires only for plain
    // uncompressed RPTR bytes.

    // Special case: RPTR packets (uncompressed only; a compressed RPTR
    // packet would have garbage bytes where the relptr is supposed to
    // sit. In practice the on-disk normalize step rewrites RPTR to
    // MESG before any compression, so this branch is the live path for
    // RPTR-on-disk; the compression guard is defense in depth.)
    if data_size >= 32 {
        let magic = *(data as *const u32);
        if magic == packet::PACKET_MAGIC {
            let header = &*(data as *const packet::PacketHeader);
            let source = header.command.data.source;
            let format = header.command.data.format;
            let compression = header.command.data.compression;
            if source == packet::PACKET_SOURCE_RPTR
                && format == packet::PACKET_FORMAT_VOIDSTAR
                && compression == 0 {
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
    dest = load_morloc_data_file(effective, data, data_size, schema, &mut err) as *mut u8;
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
    let classified = classify_arg_source(arg)?;
    let effective = classified.effective;

    let inline_bytes = if classified.kind == ArgSource::Inline {
        Some(CStr::from_ptr(effective).to_bytes())
    } else {
        None
    };

    // Read file/stdin content up front: the partial-JSON dispatcher
    // needs to peek at the leading non-whitespace byte, and stdin
    // can only be read once anyway. For inline values the bytes are
    // already addressable via the C string.
    let file_bytes: Option<Vec<u8>> = match classified.kind {
        ArgSource::File => {
            let p = CStr::from_ptr(effective).to_string_lossy().into_owned();
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
    per_field_shapes: Option<&[FieldShape]>,
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

    // Source 2: per-field user CLI values. Shape-carrying entries route
    // through the shape-driven decoder; the rest use the classifier.
    let mut user_fields: Vec<Option<shm::AbsPtr>> = vec![None; n];
    for i in 0..n {
        let field_val = *fields.add(i);
        if !field_val.is_null() {
            let elem_cs = *(*schema).parameters.add(i);
            let fs = &rs.parameters[i];
            let field_shape = per_field_shapes.and_then(|s| s.get(i)).copied();
            let loaded = if let Some(fshape) = field_shape.filter(|s| s.is_non_default()) {
                let list_cfg = match parse_list_config(fshape.list_config_json) {
                    Ok(c) => c,
                    Err(e) => {
                        set_errmsg(errmsg, &e);
                        return ptr::null_mut();
                    }
                };
                let voidstar = dispatch_shape_core(
                    field_val, elem_cs,
                    fshape.source_code, fshape.form_code, &list_cfg,
                    &mut err,
                );
                if voidstar.is_null() {
                    if !err.is_null() {
                        wrap_and_set_errmsg(err, &field_label(i), errmsg);
                    } else {
                        set_errmsg(errmsg, &MorlocError::Other(
                            format!("{}: shape dispatch returned null", field_label(i))));
                    }
                    return ptr::null_mut();
                }
                voidstar as *mut u8
            } else {
                let p = match shm::shcalloc(1, fs.width) {
                    Ok(p) => p,
                    Err(e) => { set_errmsg(errmsg, &e); return ptr::null_mut(); }
                };
                let out = parse_cli_data_argument_singular(p, field_val, elem_cs, &mut err);
                if !err.is_null() {
                    wrap_and_set_errmsg(err, &field_label(i), errmsg);
                    return ptr::null_mut();
                }
                out
            };
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

// ── parse_cli_data_argument_list ─────────────────────────────────────────────

/// Assemble a single list-typed packet from N CLI arguments. Used by
/// the nexus when a manifest arg is marked `many: true`. Each input
/// `argument_t` carries one CLI token (or whitespace-quoted literal,
/// already JSON-wrapped for `literal: true` cases). Per-element source
/// resolution goes through the same `parse_cli_data_argument_singular`
/// path as scalars, so every supported wire format (inline JSON, file
/// JSON, mpack file, morloc voidstar binary file, RPTR packet file,
/// stdin) is accepted for every element with no JSON-conversion cost.
///
/// The resulting packet wraps a morloc list whose element schema is
/// `list_schema->parameters[0]`.
#[no_mangle]
pub unsafe extern "C" fn parse_cli_data_argument_list(
    mut dest: *mut u8,
    args: *const *const ArgumentT,
    n: usize,
    list_schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);

    if list_schema.is_null() {
        set_errmsg(errmsg, &MorlocError::NullPointer);
        return ptr::null_mut();
    }

    let rs = CSchema::to_rust(list_schema);

    use crate::schema::SerialType;
    if !matches!(rs.serial_type, SerialType::Array) {
        set_errmsg(errmsg, &MorlocError::Other(
            "parse_cli_data_argument_list requires a list (Array) schema".into()));
        return ptr::null_mut();
    }
    if rs.parameters.is_empty() || (*list_schema).parameters.is_null() {
        set_errmsg(errmsg, &MorlocError::Other(
            "list schema has no element schema".into()));
        return ptr::null_mut();
    }

    let elem_rs = &rs.parameters[0];
    let elem_cs: *const CSchema = *(*list_schema).parameters.add(0);
    let ew = elem_rs.width;
    let hdr = std::mem::size_of::<shm::Array>();

    // Allocate header at `dest` (or fresh shmalloc) and a separate
    // shmalloc'd block for the element data area. Matches the
    // dest-Some branch of `json.rs::json_to_voidstar_inner` for the
    // SerialType::Array case so the dispatch / arena bookkeeping is
    // identical for many-args and JSON-array-parsed args.
    if dest.is_null() {
        match shm::shmalloc(hdr) {
            Ok(p) => dest = p,
            Err(e) => { set_errmsg(errmsg, &e); return ptr::null_mut(); }
        }
    }
    // Zero the header slot before writing -- a partial write leaves
    // garbage in the second field if we error out mid-loop.
    ptr::write_bytes(dest, 0, hdr);

    let data_ptr: *mut u8 = if n > 0 {
        match shm::shmalloc(n * ew) {
            Ok(p) => {
                ptr::write_bytes(p, 0, n * ew);
                p
            }
            Err(e) => { set_errmsg(errmsg, &e); return ptr::null_mut(); }
        }
    } else {
        ptr::null_mut()
    };
    let data_rel = if data_ptr.is_null() {
        shm::RELNULL
    } else {
        match shm::abs2rel(data_ptr) {
            Ok(r) => r,
            Err(e) => { set_errmsg(errmsg, &e); return ptr::null_mut(); }
        }
    };

    // Parse each element into its slot in the data area. Mirrors
    // `parse_cli_data_argument_unrolled`'s per-field loop: shcalloc a
    // scratch buffer of the element width, parse into it, then
    // memcpy the parsed bytes into the destination slot. Relptrs in
    // the element header continue to reference SHM blocks owned by
    // the eval arena and stay alive for the rest of dispatch.
    for i in 0..n {
        let arg_t = *args.add(i);
        if arg_t.is_null() {
            set_errmsg(errmsg, &MorlocError::Other(
                format!("element {} in many-arg list is NULL", i)));
            return ptr::null_mut();
        }
        let val = (*arg_t).value;
        if val.is_null() {
            set_errmsg(errmsg, &MorlocError::Other(
                format!("element {} in many-arg list has no value", i)));
            return ptr::null_mut();
        }

        let scratch = match shm::shcalloc(1, ew) {
            Ok(p) => p,
            Err(e) => { set_errmsg(errmsg, &e); return ptr::null_mut(); }
        };
        let mut err: *mut c_char = ptr::null_mut();
        let loaded = parse_cli_data_argument_singular(scratch, val, elem_cs, &mut err);
        if !err.is_null() {
            wrap_and_set_errmsg(err, &format!("element {}", i), errmsg);
            return ptr::null_mut();
        }
        if loaded.is_null() {
            set_errmsg(errmsg, &MorlocError::Other(
                format!("failed to parse element {}", i)));
            return ptr::null_mut();
        }

        let slot = data_ptr.add(i * ew);
        ptr::copy_nonoverlapping(loaded, slot, ew);
    }

    // Write the Array header (size, data relptr) at `dest`.
    let arr = shm::Array { size: n, data: data_rel };
    ptr::copy_nonoverlapping(
        &arr as *const shm::Array as *const u8,
        dest,
        hdr,
    );

    // Wrap into a packet just like `parse_cli_data_argument` does.
    // Arrow-table targets cannot be the element type of a many-arg
    // (the manifest's `many` flag is gated on `[a]`, not Table), so
    // the auto path is always correct here.
    let relptr = match shm::abs2rel(dest) {
        Ok(r) => r,
        Err(e) => { set_errmsg(errmsg, &e); return ptr::null_mut(); }
    };
    crate::packet_ffi::make_data_packet_auto(
        dest as *mut c_void,
        relptr,
        list_schema,
        errmsg,
    )
}

// ── Shape-aware dispatch ───────────────────────────────────────────────────

/// Read all bytes for a CLI argument, honoring the source classifier.
/// For File / Stdin sources, opens the file (or stdin) and reads to a
/// libc-malloc'd buffer. For Inline source, copies the argv string's
/// bytes into a libc-malloc'd buffer. Returns the buffer plus the
/// resolved source kind so callers can fast-fail forms whose semantics
/// require a specific source (e.g. `form: list` insists on
/// File/Stdin). The caller is responsible for freeing the returned
/// buffer.
/// Open a NUL-terminated path, read the whole file into a libc-malloc
/// buffer, and return it. On failure returns a short reason (already
/// formatted but without any caller-specific prefix). The buffer must
/// be `libc::free`'d by the caller on success.
unsafe fn read_path_into_libc(path: *const c_char) -> Result<(*mut u8, usize), String> {
    extern "C" {
        fn read_binary_fd(
            file: *mut libc::FILE,
            file_size: *mut usize,
            errmsg: *mut *mut c_char,
        ) -> *mut u8;
    }
    if path.is_null() {
        return Err("null path".into());
    }
    let fd = libc::fopen(path, b"rb\0".as_ptr() as *const c_char);
    if fd.is_null() {
        return Err(format!(
            "cannot open '{}'",
            CStr::from_ptr(path).to_string_lossy()
        ));
    }
    let mut sz: usize = 0;
    let mut err: *mut c_char = ptr::null_mut();
    let buf = read_binary_fd(fd, &mut sz, &mut err);
    libc::fclose(fd);
    if buf.is_null() {
        let msg = if err.is_null() {
            "read failed".into()
        } else {
            let s = CStr::from_ptr(err).to_string_lossy().into_owned();
            libc::free(err as *mut c_void);
            s
        };
        return Err(msg);
    }
    Ok((buf, sz))
}

unsafe fn read_argv_bytes(
    arg: *mut c_char,
    errmsg: *mut *mut c_char,
) -> Option<(*mut u8, usize, ArgSource)> {
    extern "C" {
        fn read_binary_fd(
            file: *mut libc::FILE,
            file_size: *mut usize,
            errmsg: *mut *mut c_char,
        ) -> *mut u8;
    }

    let classified = match classify_arg_source(arg) {
        Ok(c) => c,
        Err(e) => {
            set_errmsg(errmsg, &e);
            return None;
        }
    };
    let effective = classified.effective;

    let fd: *mut libc::FILE = match classified.kind {
        ArgSource::Stdin => {
            if let Err(e) = claim_stdin() {
                set_errmsg(errmsg, &e);
                return None;
            }
            libc::fdopen(libc::STDIN_FILENO, b"rb\0".as_ptr() as *const c_char)
        }
        ArgSource::File => {
            let fd = libc::fopen(effective, b"rb\0".as_ptr() as *const c_char);
            if fd.is_null() {
                set_errmsg(
                    errmsg,
                    &MorlocError::Other(format!(
                        "The argument '{}' is a filename, but it can't be read",
                        CStr::from_ptr(effective).to_string_lossy()
                    )),
                );
                return None;
            }
            fd
        }
        ArgSource::Inline => {
            // Inline: the argv string itself is the byte payload.
            let arg_bytes = CStr::from_ptr(effective).to_bytes();
            let buf = libc::malloc(arg_bytes.len().max(1)) as *mut u8;
            if buf.is_null() {
                set_errmsg(errmsg, &MorlocError::Other("malloc failed".into()));
                return None;
            }
            if !arg_bytes.is_empty() {
                std::ptr::copy_nonoverlapping(arg_bytes.as_ptr(), buf, arg_bytes.len());
            }
            return Some((buf, arg_bytes.len(), ArgSource::Inline));
        }
    };

    let mut data_size: usize = 0;
    let mut err: *mut c_char = ptr::null_mut();
    let data = read_binary_fd(fd, &mut data_size, &mut err);
    let stdin_fd = libc::fdopen(libc::STDIN_FILENO, b"rb\0".as_ptr() as *const c_char);
    if fd != stdin_fd {
        libc::fclose(fd);
    }
    if !err.is_null() {
        if !data.is_null() {
            libc::free(data as *mut c_void);
        }
        *errmsg = err;
        return None;
    }
    Some((data, data_size, classified.kind))
}

/// Wrap a freshly-decoded voidstar in a morloc data packet. Mirrors the
/// tail of `parse_cli_data_argument`.
unsafe fn wrap_voidstar_as_packet(
    voidstar: *mut c_void,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    if voidstar.is_null() {
        return ptr::null_mut();
    }
    let relptr = match shm::abs2rel(voidstar as *mut u8) {
        Ok(r) => r,
        Err(e) => {
            set_errmsg(errmsg, &e);
            return ptr::null_mut();
        }
    };
    let arrow = !schema.is_null() && {
        let rs = CSchema::to_rust(schema);
        crate::arrow_ffi::is_arrow_table_schema(&rs)
    };
    if arrow {
        crate::packet_ffi::make_arrow_data_packet(relptr, schema)
    } else {
        crate::packet_ffi::make_data_packet_auto(voidstar, relptr, schema, errmsg)
    }
}

/// Per-element source vocabulary for `form: list`. Plumbed through
/// the shaped FFI as a JSON string so the runtime can switch on
/// `list.source:` without growing the FFI signature.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ListElemSource {
    Inline,
    Auto,
    File,
}

/// Per-element value-invariant check. The JSON shape mirrors the
/// outer `Check` enum: `{"kind": "path", "value": "r"}`.
#[derive(Debug, Clone, serde::Deserialize)]
#[serde(tag = "kind", content = "value")]
#[serde(rename_all = "lowercase")]
pub enum ListElemCheck {
    Path(String),
}

/// Per-element form. Mirrors the outer FormAtom; `Auto` is the
/// runtime sentinel for the classifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum ListElemForm {
    Auto,
    Packet,
    Bytes,
    BytesOnly,
}

/// Per-element configuration carried into `try_list` from the nexus
/// dispatch layer.
#[derive(Debug, Clone)]
pub struct ListConfig {
    pub source: ListElemSource,
    pub form: ListElemForm,
    pub checks: Vec<ListElemCheck>,
}

impl Default for ListConfig {
    fn default() -> Self {
        ListConfig {
            source: ListElemSource::Inline,
            form: ListElemForm::Auto,
            checks: Vec::new(),
        }
    }
}

/// Parse the list-config JSON the nexus passes through the shaped FFI.
/// Null pointer or empty string returns defaults. The expected shape is
/// `{"source":"auto|inline|file","form":"auto|packet|bytes|bytes-only","checks":[{"kind":"path","value":"r"}]}`.
unsafe fn parse_list_config(json_ptr: *const c_char) -> Result<ListConfig, MorlocError> {
    if json_ptr.is_null() {
        return Ok(ListConfig::default());
    }
    let s = CStr::from_ptr(json_ptr)
        .to_str()
        .map_err(|_| MorlocError::Other("list config: invalid UTF-8".into()))?;
    if s.is_empty() {
        return Ok(ListConfig::default());
    }
    #[derive(serde::Deserialize)]
    struct Raw {
        #[serde(default = "default_source")]
        source: ListElemSource,
        #[serde(default = "default_form")]
        form: ListElemForm,
        #[serde(default)]
        checks: Vec<ListElemCheck>,
    }
    fn default_source() -> ListElemSource { ListElemSource::Inline }
    fn default_form() -> ListElemForm { ListElemForm::Auto }
    let raw: Raw = serde_json::from_str(s)
        .map_err(|e| MorlocError::Other(format!("list config: invalid JSON ({})", e)))?;
    Ok(ListConfig { source: raw.source, form: raw.form, checks: raw.checks })
}

/// Shape-driven decode: argv -> voidstar. Caller supplies a non-group
/// `arg_value` and wraps the returned voidstar into a packet if needed.
/// Stream fast paths are applied by the top-level shaped entry point,
/// not here. See [`parse_cli_data_argument_shaped`] for the source /
/// form code vocabulary.
unsafe fn dispatch_shape_core(
    arg_value: *mut c_char,
    schema: *const CSchema,
    source_code: u8,
    form_code: u8,
    list_cfg: &ListConfig,
    errmsg: *mut *mut c_char,
) -> *mut c_void {
    // `form: auto` delegates to the existing classified pipeline,
    // which handles inline JSON literals (NUL-terminated argv strings
    // fed to `read_json_with_schema`), file reads, and stdin -- no
    // need to round-trip through `load_morloc_data_file` on a
    // libc-malloc'd buffer.
    if form_code == 0 {
        let classified = match source_code {
            1 => Classified { kind: ArgSource::Inline, effective: arg_value },
            2 => Classified { kind: ArgSource::File, effective: arg_value },
            _ => match classify_arg_source(arg_value) {
                Ok(c) => c,
                Err(e) => { set_errmsg(errmsg, &e); return ptr::null_mut(); }
            },
        };
        let result = parse_cli_data_argument_classified(
            ptr::null_mut(), classified, schema, errmsg,
        );
        return result as *mut c_void;
    }

    // Non-Auto forms: resolve bytes per the source code, then
    // dispatch through the appropriate decoder.
    let (data, data_size, source_kind) = if source_code == 1 {
        if arg_value.is_null() {
            set_errmsg(errmsg, &MorlocError::Other("inline source: null argv".into()));
            return ptr::null_mut();
        }
        let n = libc::strlen(arg_value);
        let buf = libc::malloc(n.max(1)) as *mut u8;
        if buf.is_null() {
            set_errmsg(errmsg, &MorlocError::Other("malloc failed".into()));
            return ptr::null_mut();
        }
        if n > 0 {
            std::ptr::copy_nonoverlapping(arg_value as *const u8, buf, n);
        }
        (buf, n, ArgSource::Inline)
    } else if source_code == 2 {
        match read_path_into_libc(arg_value) {
            Ok((buf, sz)) => (buf, sz, ArgSource::File),
            Err(reason) => {
                set_errmsg(errmsg,
                    &MorlocError::Other(format!("source: file: {}", reason)));
                return ptr::null_mut();
            }
        }
    } else {
        match read_argv_bytes(arg_value, errmsg) {
            Some(b) => b,
            None => return ptr::null_mut(),
        }
    };

    // `form: list` + inline argv: route through the JSON path when
    // the argv looks like a JSON array (`[...]`). No overlap with
    // realistic filenames: files whose first byte is `[` are not
    // production-realistic. A user hitting the weird edge case can
    // spell the file source explicitly with `source: file` or `-`.
    //
    // Non-JSON inline input on `form: list` (a bare token like
    // `foo`, missing the `[` prefix) still errors, since there's no
    // sensible way to parse a scalar token into a list.
    if form_code == 3 && source_kind == ArgSource::Inline {
        let first_byte = if data.is_null() || data_size == 0 {
            0
        } else {
            *data
        };
        if first_byte == b'[' {
            extern "C" {
                fn read_json_with_schema(
                    dest: *mut u8,
                    json: *mut c_char,
                    schema: *const CSchema,
                    errmsg: *mut *mut c_char,
                ) -> *mut u8;
            }
            // Ensure NUL-termination for the C parser. `read_argv_bytes`
            // returned an exact-length libc allocation without a
            // sentinel; grow by one and write `\0` at the end.
            let terminated = libc::realloc(
                data as *mut c_void,
                data_size + 1,
            ) as *mut u8;
            if terminated.is_null() {
                if !data.is_null() {
                    libc::free(data as *mut c_void);
                }
                set_errmsg(
                    errmsg,
                    &MorlocError::Other(
                        "form: list inline: realloc for NUL terminator failed".into(),
                    ),
                );
                return ptr::null_mut();
            }
            *terminated.add(data_size) = 0;
            let mut json_err: *mut c_char = ptr::null_mut();
            let voidstar = read_json_with_schema(
                ptr::null_mut(),
                terminated as *mut c_char,
                schema,
                &mut json_err,
            );
            libc::free(terminated as *mut c_void);
            if voidstar.is_null() {
                if !json_err.is_null() {
                    *errmsg = json_err;
                } else {
                    set_errmsg(
                        errmsg,
                        &MorlocError::Other(
                            "form: list inline: JSON parse returned null".into(),
                        ),
                    );
                }
                return ptr::null_mut();
            }
            return voidstar as *mut c_void;
        }
        // Not JSON-shaped -- error with a clearer message than the
        // previous "requires file/stdin" wording. Inline non-`[`
        // input can't yield a list.
        if !data.is_null() {
            libc::free(data as *mut c_void);
        }
        set_errmsg(
            errmsg,
            &MorlocError::Other(
                "form: list: inline argv is not a JSON array (must start with `[`). \
                 Pass a path to a file with one element per line, `-` for stdin, \
                 or an inline JSON array like '[1,2,3]'."
                    .into(),
            ),
        );
        return ptr::null_mut();
    }

    let mut atom_err: *mut c_char = ptr::null_mut();
    let voidstar = match form_code {
        1 => try_packet_strict(data, data_size, schema, &mut atom_err),
        2 => try_bytes_hybrid(data, data_size, schema, &mut atom_err),
        3 => try_list_with_config(data, data_size, schema, list_cfg, &mut atom_err),
        4 => try_bytes_only(data, data_size, schema, &mut atom_err),
        other => {
            if !data.is_null() {
                libc::free(data as *mut c_void);
            }
            set_errmsg(
                errmsg,
                &MorlocError::Other(format!("unknown form code: {}", other)),
            );
            return ptr::null_mut();
        }
    };

    if voidstar.is_null() {
        if !atom_err.is_null() {
            *errmsg = atom_err;
        } else {
            set_errmsg(errmsg, &MorlocError::Other("shape dispatch returned null".into()));
        }
        return ptr::null_mut();
    }
    voidstar as *mut c_void
}

/// Per-field shape config for the unrolled record path. The
/// `list_config_json` pointer is either null (defaults apply) or a
/// C-string with the same JSON shape [`parse_list_config`] accepts.
#[derive(Debug, Clone, Copy)]
struct FieldShape {
    source_code: u8,
    form_code: u8,
    list_config_json: *const c_char,
}

impl FieldShape {
    fn is_non_default(&self) -> bool {
        self.source_code != 0
            || self.form_code != 0
            || !self.list_config_json.is_null()
    }
}

/// Shape-aware CLI argument dispatch.
///
/// `source_code` selects how the argv string is resolved to bytes:
///   - 0 (Auto): run the runtime classifier (stdin marker / JSON-shape
///     sniff / file-exists)
///   - 1 (Inline): the argv string IS the bytes
///   - 2 (File): the argv string is a path; open it directly
///
/// `form_code` selects how the resolved bytes are interpreted:
///   - 0 (Auto): structured-format classifier (JSON / MsgPack / packet
///     / Arrow / Parquet)
///   - 1 (Packet): strict packet only
///   - 2 (Bytes): sniff packet magic, fall back to packed raw bytes
///   - 3 (List): UTF-8 split on newlines; per-element pipeline
///   - 4 (BytesOnly): packed raw bytes, never sniff
///
/// `list_config_json` carries the per-element overrides when
/// `form_code == 3`; null / empty means defaults.
#[no_mangle]
pub unsafe extern "C" fn parse_cli_data_argument_shaped(
    _dest: *mut u8,
    arg: *const ArgumentT,
    schema: *const CSchema,
    source_code: u8,
    form_code: u8,
    list_config_json: *const c_char,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);

    if arg.is_null() {
        set_errmsg(
            errmsg,
            &MorlocError::Other("parse_cli_data_argument_shaped: null arg".into()),
        );
        return ptr::null_mut();
    }

    let list_cfg = match parse_list_config(list_config_json) {
        Ok(c) => c,
        Err(e) => {
            set_errmsg(errmsg, &e);
            return ptr::null_mut();
        }
    };

    // Group / record args do not flow through the shape pipeline.
    if !(*arg).fields.is_null() {
        return parse_cli_data_argument(ptr::null_mut(), arg, schema, errmsg);
    }

    // Stream-packet fast path (top-level only; the shape core skips it).
    if form_code == 0 {
        let classified_for_fast_path = match source_code {
            2 => Some(Classified { kind: ArgSource::File, effective: (*arg).value }),
            1 => None,
            _ => classify_arg_source((*arg).value).ok(),
        };
        if let Some(c) = classified_for_fast_path {
            if c.kind == ArgSource::File {
                stream_fast_path_or_fallthrough!(c.effective, schema, errmsg);
            }
        }
    } else if form_code == 1 && source_code == 2 {
        stream_fast_path_or_fallthrough!((*arg).value, schema, errmsg);
    }

    let voidstar = dispatch_shape_core(
        (*arg).value, schema, source_code, form_code, &list_cfg, errmsg,
    );
    if voidstar.is_null() {
        return ptr::null_mut();
    }
    wrap_voidstar_as_packet(voidstar, schema, errmsg)
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

    // Stream-packet fast path (singular args only): a MORLOC_STREAM_PACKET
    // file short-circuits the voidstar-then-wrap chain below and returns
    // a purpose-built packet directly.
    if (*arg).fields.is_null() && !(*arg).value.is_null() {
        if let Ok(classified) = classify_arg_source((*arg).value) {
            if classified.kind == ArgSource::File {
                stream_fast_path_or_fallthrough!(classified.effective, schema, errmsg);
            }
        }
    }

    let result = if (*arg).fields.is_null() {
        parse_cli_data_argument_singular(dest, (*arg).value, schema, &mut err)
    } else {
        parse_cli_data_argument_unrolled(
            dest, (*arg).value, (*arg).fields, (*arg).default_fields, schema,
            None, &mut err,
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

// ── parse_cli_data_argument_group_shaped ─────────────────────────────────────

/// Shape-aware `Arg::Group` dispatch. The three parallel arrays are
/// indexed by entry position (matching `arg->fields[i]`); a field's
/// shape is "default" when both codes are 0 and the JSON pointer is
/// NULL, in which case that field falls back to the classifier.
#[no_mangle]
pub unsafe extern "C" fn parse_cli_data_argument_group_shaped(
    dest: *mut u8,
    arg: *const ArgumentT,
    schema: *const CSchema,
    source_codes: *const u8,
    form_codes: *const u8,
    list_config_jsons: *const *const c_char,
    n_fields: usize,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);
    let mut err: *mut c_char = ptr::null_mut();

    if arg.is_null() || (*arg).fields.is_null() {
        set_errmsg(errmsg, &MorlocError::Other(
            "parse_cli_data_argument_group_shaped: arg is null or not a group".into()));
        return ptr::null_mut();
    }
    if (*arg).size != n_fields {
        set_errmsg(errmsg, &MorlocError::Other(format!(
            "parse_cli_data_argument_group_shaped: n_fields ({}) does not match arg->size ({})",
            n_fields, (*arg).size,
        )));
        return ptr::null_mut();
    }

    let mut shapes: Vec<FieldShape> = Vec::with_capacity(n_fields);
    for i in 0..n_fields {
        shapes.push(FieldShape {
            source_code: if source_codes.is_null() { 0 } else { *source_codes.add(i) },
            form_code: if form_codes.is_null() { 0 } else { *form_codes.add(i) },
            list_config_json: if list_config_jsons.is_null() {
                ptr::null()
            } else {
                *list_config_jsons.add(i)
            },
        });
    }

    let result = parse_cli_data_argument_unrolled(
        dest, (*arg).value, (*arg).fields, (*arg).default_fields, schema,
        Some(&shapes), &mut err,
    );
    if !err.is_null() {
        *errmsg = err;
        return ptr::null_mut();
    }
    wrap_voidstar_as_packet(result as *mut c_void, schema, errmsg)
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
