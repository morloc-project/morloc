//! `morloc-nexus file` subcommand.
//!
//! Classifies morloc-compatible data files. Strategy:
//!
//!   1. Magic-byte checks for unambiguous formats: morloc packet
//!      (DATA / CALL / PING), Arrow IPC, Parquet.
//!   2. Content-parse checks against a probe window (up to 1024 B):
//!      MessagePack via a recursive marker walker, JSON via
//!      `serde_json`'s streaming parser, CSV via libmorloc's
//!      `morloc_csv_infer` FFI (the same arrow-csv inference morloc's
//!      runtime uses), and UTF-8 text via `str::from_utf8`.
//!   3. Anything left is `Unknown`.
//!
//! The morloc-packet branch is seek-based — even a multi-gigabyte
//! CALL packet costs only ~256 B of I/O per inner arg (header +
//! metadata) because each arg's payload region is skipped with
//! `seek` instead of read. Default behaviour also verifies that the
//! on-disk file size matches the size declared in the outer header
//! (`32 + offset + length`) so truncation is caught without
//! `--validate`. With `--validate`, each file is additionally fed
//! through the exact same loader chain (`initialize_positional` ->
//! `parse_cli_data_argument`) that `run` uses.

use std::ffi::{c_char, OsStr};
use std::fs::File;
use std::io::{Cursor, Read, Seek, SeekFrom};
use std::path::Path;
use std::ptr;

use morloc_runtime_types::packet::{
    decode_schema_entry, iter_metadata, packet_compression_name, packet_entrypoint_name,
    packet_format_name, packet_source_name, PacketHeader, METADATA_TYPE_SCHEMA_STRING,
    PACKET_COMPRESSION_NONE, PACKET_MAGIC, PACKET_TYPE_CALL, PACKET_TYPE_DATA,
    PACKET_TYPE_PING,
};
use rmp::Marker;

use crate::cli::FileArgs;
use crate::loader;
use crate::process::take_c_errmsg;

extern "C" {
    /// Dry-run CSV inference. Validates the bytes parse as a CSV
    /// (header + at least one record) under `delimiter`. On success
    /// writes a libc-allocated, null-terminated JSON string to
    /// `out_info` of the form
    /// `{"columns":[{"name":"a","type":"int"}, ...]}`. The caller
    /// frees it with `libc::free`. On failure, `errmsg` carries the
    /// arrow-csv inference error verbatim. Defined in
    /// `morloc-runtime/src/arrow_ipc_reader.rs::morloc_csv_infer`.
    fn morloc_csv_infer(
        data: *const u8,
        data_len: usize,
        delimiter: u8,
        out_info: *mut *mut c_char,
        errmsg: *mut *mut c_char,
    ) -> bool;
}

/// Per-arg / DATA-packet metadata extracted from a 32-byte header
/// plus the optional schema field from its metadata block.
#[derive(Debug, Clone)]
pub struct DataArg {
    pub source: u8,
    pub format: u8,
    pub compression: u8,
    pub schema: Option<String>,
    pub payload_bytes: u64,
    pub metadata_bytes: u32,
}

/// Outcome of one inner arg walk on a CALL packet.
#[derive(Debug, Clone)]
pub enum ArgEntry {
    Data(DataArg),
    Malformed(String),
}

/// Result of classifying one file.
#[derive(Debug, Clone)]
pub enum Classification {
    MorlocDataPacket {
        arg: DataArg,
        total_bytes: u64,
    },
    MorlocCallPacket {
        midx: u32,
        entrypoint: u8,
        args: Vec<ArgEntry>,
        payload_bytes: u64,
        total_bytes: u64,
    },
    MorlocPingPacket,
    Json,
    MessagePack,
    Csv {
        /// `,` or `\t` (the delimiter `morloc_csv_infer` accepted).
        delimiter: char,
        /// One entry per inferred column.
        columns: Vec<CsvColumn>,
    },
    Arrow,
    Parquet,
    Text {
        /// True iff every probed byte is < 0x80 (pure ASCII).
        ascii_only: bool,
    },
    Empty,
    Unknown,
    Directory,
    Error(String),
}

/// One inferred CSV column, as returned by libmorloc's
/// `morloc_csv_infer`. Type is one of the simplified categories
/// (`int` / `float` / `str` / `bool` / `date` / `time` / `other`).
#[derive(Debug, Clone)]
pub struct CsvColumn {
    pub name: String,
    pub type_category: String,
}

impl Classification {
    fn is_error(&self) -> bool {
        matches!(self, Classification::Error(_))
            || matches!(
                self,
                Classification::MorlocCallPacket { args, .. }
                    if args.iter().any(|a| matches!(a, ArgEntry::Malformed(_)))
            )
    }
}

/// Entry point. Iterates inputs, classifies each, formats the
/// chosen output style, optionally appends `--validate` results, and
/// exits 0/1 based on whether any input produced an error or
/// validation failure.
pub fn run(args: &FileArgs) -> ! {
    let probe_bytes = crate::cli::parse_byte_size("--bytes", &args.bytes) as usize;
    let opts = RenderOpts {
        no_file: args.no_file,
        no_description: args.no_description,
        verbose: args.verbose,
    };
    let mut any_error = false;
    let mut validation_failed = false;
    for path in &args.targets {
        let cls = classify_path(Path::new(path), probe_bytes);
        let mut validated: Option<ValidationOutcome> = None;
        if args.validate && !cls.is_error() && !matches!(cls, Classification::Directory) {
            validated = Some(run_validate(path, &cls, args.schema.as_deref()));
            if let Some(ValidationOutcome::Failed(_)) = validated {
                validation_failed = true;
            }
        }
        if cls.is_error() {
            any_error = true;
        }
        let out = if args.json {
            format_json(&cls, path, validated.as_ref())
        } else {
            format_plain(&cls, path, &opts, validated.as_ref())
        };
        println!("{}", out);
    }
    let code = if any_error || validation_failed { 1 } else { 0 };
    std::process::exit(code);
}

/// Plain-text render options. JSON output ignores these (always emits
/// the full structure).
#[derive(Debug, Clone, Copy)]
pub struct RenderOpts {
    pub no_file: bool,
    pub no_description: bool,
    pub verbose: bool,
}

/// Outcome of optional `--validate` pass.
#[derive(Debug, Clone)]
pub enum ValidationOutcome {
    /// Full schema-aware load succeeded.
    Passed,
    /// Schema-aware load failed; carries the same error string `run`
    /// would print.
    Failed(String),
    /// No schema available (no `--schema`, no embedded schema in
    /// metadata) — only the cheap structural walk was performed.
    StructureOnly,
}

fn run_validate(
    path: &str,
    cls: &Classification,
    user_schema: Option<&str>,
) -> ValidationOutcome {
    // Resolve a schema string: user-supplied wins, then a morloc-packet
    // embedded schema, else fall back to structure-only.
    let schema_str: Option<String> = match user_schema {
        Some(s) => Some(s.to_string()),
        None => match cls {
            Classification::MorlocDataPacket {
                arg: DataArg { schema: Some(s), .. },
                ..
            } => Some(s.clone()),
            _ => None,
        },
    };

    let Some(schema_str) = schema_str else {
        return ValidationOutcome::StructureOnly;
    };

    match loader::load_with_schema(path, &schema_str) {
        Ok(handle) => {
            // Drop the loaded packet immediately; we only care that
            // the load succeeded.
            drop(handle);
            ValidationOutcome::Passed
        }
        Err(e) => ValidationOutcome::Failed(e),
    }
}

// ---------------------------------------------------------------------------
// Classification
// ---------------------------------------------------------------------------

/// Default probe-window size when the caller doesn't supply one.
/// Kept in sync with `FileArgs::bytes`'s clap default ("1k").
pub const DEFAULT_PROBE_BYTES: usize = 1024;

pub(crate) fn classify_path(path: &Path, probe_bytes: usize) -> Classification {
    let mut f = match File::open(path) {
        Ok(f) => f,
        Err(e) => return Classification::Error(format!("cannot open ({})", e)),
    };
    let md = match f.metadata() {
        Ok(m) => m,
        Err(e) => return Classification::Error(format!("stat failed ({})", e)),
    };
    if md.is_dir() {
        return Classification::Directory;
    }
    classify_reader(&mut f, md.len(), path.extension(), probe_bytes)
}

/// Generic classifier over any `Read + Seek`. `file_size` is the
/// authoritative end-of-stream (compared against packet headers for
/// truncation/oversize detection). `probe_bytes` caps content-parse
/// reads. `ext` is currently unused — we rely on content-parse for
/// every format that has no magic.
pub fn classify_reader<R: Read + Seek>(
    f: &mut R,
    file_size: u64,
    _ext: Option<&OsStr>,
    probe_bytes: usize,
) -> Classification {
    // Empty files get their own type so downstream tooling can
    // distinguish "we got nothing" from "we got bytes we couldn't
    // identify".
    if file_size == 0 {
        return Classification::Empty;
    }

    // 1. Read first 8 bytes for the magic checks.
    let mut head = [0u8; 8];
    let head_n = read_at_most(f, &mut head).unwrap_or(0);

    if head_n >= 4 {
        let magic = u32::from_le_bytes([head[0], head[1], head[2], head[3]]);
        if magic == PACKET_MAGIC {
            return classify_morloc_packet(f, file_size, &head[..head_n]);
        }
    }
    if head_n >= 6 && &head[..6] == b"ARROW1" {
        return Classification::Arrow;
    }
    if head_n >= 4 && &head[..4] == b"PAR1" && file_size >= 8 {
        let mut tail = [0u8; 4];
        if f.seek(SeekFrom::Start(file_size - 4)).is_ok()
            && f.read_exact(&mut tail).is_ok()
            && &tail == b"PAR1"
        {
            return Classification::Parquet;
        }
    }

    // 2. Read up to probe_bytes, including the bytes already in `head`.
    //    Cap at file_size so a huge --bytes argument on a tiny file
    //    doesn't allocate a multi-gigabyte buffer.
    let cap = probe_bytes.min(file_size as usize);
    let probe = read_probe_window(f, &head[..head_n], cap);

    // 3. Content-parse checks. Order: msgpack -> json -> csv -> text.
    //    MessagePack is checked first because almost every byte is a
    //    valid msgpack leading marker, but a STRICT walker rejects
    //    the ASCII-as-fixint false positives.
    if looks_like_msgpack(&probe) {
        return Classification::MessagePack;
    }
    if looks_like_json(&probe) {
        return Classification::Json;
    }
    if let Some((delimiter, columns)) = looks_like_csv(&probe) {
        return Classification::Csv { delimiter, columns };
    }
    if let Some(ascii_only) = looks_like_text(&probe) {
        return Classification::Text { ascii_only };
    }

    Classification::Unknown
}

/// Fill a probe buffer up to `target` bytes, using `head` for the
/// bytes already read and seeking to byte `head.len()` for the rest.
/// Returns whatever bytes are available — possibly fewer than
/// `target` for small files.
fn read_probe_window<R: Read + Seek>(f: &mut R, head: &[u8], target: usize) -> Vec<u8> {
    let mut buf = Vec::with_capacity(target);
    buf.extend_from_slice(head);
    if head.len() < target {
        let _ = f.seek(SeekFrom::Start(head.len() as u64));
        let want = target - head.len();
        let mut more = vec![0u8; want];
        let n = read_at_most(f, &mut more).unwrap_or(0);
        buf.extend_from_slice(&more[..n]);
    }
    buf
}

/// True iff `bytes` either parses fully as a single msgpack root
/// value with no trailing data, OR runs out of input mid-value
/// (i.e., the probe window cut a real msgpack value in half).
/// Trailing-data-after-complete-value is rejected because it's the
/// signature of an ASCII text file whose first byte parsed as a
/// 1-byte fixint.
fn looks_like_msgpack(bytes: &[u8]) -> bool {
    if bytes.is_empty() {
        return false;
    }
    let mut cursor = Cursor::new(bytes);
    match skip_msgpack_value(&mut cursor) {
        Ok(()) => cursor.position() as usize == bytes.len(),
        Err(MsgpackProbeError::Eof) => true,
        Err(MsgpackProbeError::Invalid) => false,
    }
}

#[derive(Debug)]
enum MsgpackProbeError {
    Eof,
    Invalid,
}

/// Recursively skip one msgpack value from `cur`. Translates EOF
/// (probe-window boundary) into a distinct error so the caller can
/// accept it as a positive signal.
fn skip_msgpack_value(cur: &mut Cursor<&[u8]>) -> Result<(), MsgpackProbeError> {
    let marker = rmp::decode::read_marker(cur).map_err(|e| io_to_probe(e.0))?;
    match marker {
        Marker::FixPos(_) | Marker::FixNeg(_)
        | Marker::Null | Marker::True | Marker::False => Ok(()),
        Marker::U8 | Marker::I8 => skip_bytes(cur, 1),
        Marker::U16 | Marker::I16 => skip_bytes(cur, 2),
        Marker::U32 | Marker::I32 | Marker::F32 => skip_bytes(cur, 4),
        Marker::U64 | Marker::I64 | Marker::F64 => skip_bytes(cur, 8),
        Marker::FixStr(n) => skip_bytes(cur, n as usize),
        Marker::Str8 | Marker::Bin8 => {
            let n = read_be_uint(cur, 1)?;
            skip_bytes(cur, n as usize)
        }
        Marker::Str16 | Marker::Bin16 => {
            let n = read_be_uint(cur, 2)?;
            skip_bytes(cur, n as usize)
        }
        Marker::Str32 | Marker::Bin32 => {
            let n = read_be_uint(cur, 4)?;
            skip_bytes(cur, n as usize)
        }
        Marker::FixArray(n) => skip_n_values(cur, n as usize),
        Marker::Array16 => {
            let n = read_be_uint(cur, 2)?;
            skip_n_values(cur, n as usize)
        }
        Marker::Array32 => {
            let n = read_be_uint(cur, 4)?;
            skip_n_values(cur, n as usize)
        }
        Marker::FixMap(n) => skip_n_values(cur, 2 * n as usize),
        Marker::Map16 => {
            let n = read_be_uint(cur, 2)?;
            skip_n_values(cur, 2 * n as usize)
        }
        Marker::Map32 => {
            let n = read_be_uint(cur, 4)?;
            skip_n_values(cur, 2 * n as usize)
        }
        Marker::FixExt1 => skip_bytes(cur, 1 + 1),
        Marker::FixExt2 => skip_bytes(cur, 1 + 2),
        Marker::FixExt4 => skip_bytes(cur, 1 + 4),
        Marker::FixExt8 => skip_bytes(cur, 1 + 8),
        Marker::FixExt16 => skip_bytes(cur, 1 + 16),
        Marker::Ext8 => {
            let n = read_be_uint(cur, 1)?;
            skip_bytes(cur, 1 + n as usize)
        }
        Marker::Ext16 => {
            let n = read_be_uint(cur, 2)?;
            skip_bytes(cur, 1 + n as usize)
        }
        Marker::Ext32 => {
            let n = read_be_uint(cur, 4)?;
            skip_bytes(cur, 1 + n as usize)
        }
        Marker::Reserved => Err(MsgpackProbeError::Invalid),
    }
}

fn skip_n_values(cur: &mut Cursor<&[u8]>, n: usize) -> Result<(), MsgpackProbeError> {
    for _ in 0..n {
        skip_msgpack_value(cur)?;
    }
    Ok(())
}

fn skip_bytes(cur: &mut Cursor<&[u8]>, n: usize) -> Result<(), MsgpackProbeError> {
    let pos = cur.position() as usize;
    let end = pos.checked_add(n).ok_or(MsgpackProbeError::Invalid)?;
    if end > cur.get_ref().len() {
        return Err(MsgpackProbeError::Eof);
    }
    cur.set_position(end as u64);
    Ok(())
}

fn read_be_uint(cur: &mut Cursor<&[u8]>, width: usize) -> Result<u64, MsgpackProbeError> {
    let pos = cur.position() as usize;
    let end = pos + width;
    let bytes = cur.get_ref();
    if end > bytes.len() {
        return Err(MsgpackProbeError::Eof);
    }
    let mut n: u64 = 0;
    for &b in &bytes[pos..end] {
        n = (n << 8) | b as u64;
    }
    cur.set_position(end as u64);
    Ok(n)
}

fn io_to_probe(e: std::io::Error) -> MsgpackProbeError {
    if e.kind() == std::io::ErrorKind::UnexpectedEof {
        MsgpackProbeError::Eof
    } else {
        MsgpackProbeError::Invalid
    }
}

/// True iff `bytes` either parses fully as one JSON value (with or
/// without trailing data — the streaming parser stops after one
/// value), OR fails because the parser ran out of input mid-value.
fn looks_like_json(bytes: &[u8]) -> bool {
    if bytes.is_empty() {
        return false;
    }
    let mut iter = serde_json::Deserializer::from_slice(bytes).into_iter::<serde_json::Value>();
    match iter.next() {
        Some(Ok(_)) => true,
        Some(Err(e)) if e.is_eof() => true,
        _ => false,
    }
}

/// Call libmorloc's `morloc_csv_infer` over the probe with the two
/// common delimiters. Returns `(delimiter, columns)` from the first
/// one that infers >= 2 columns — the 2-column floor filters out
/// single-column "everything is one field" false positives on text
/// files that contain neither commas nor tabs.
fn looks_like_csv(bytes: &[u8]) -> Option<(char, Vec<CsvColumn>)> {
    if bytes.is_empty() {
        return None;
    }
    for &(delim_byte, delim_char) in &[(b'\t', '\t'), (b',', ',')] {
        if let Some(columns) = csv_infer_columns(bytes, delim_byte) {
            if columns.len() >= 2 {
                return Some((delim_char, columns));
            }
        }
    }
    None
}

/// Invoke the FFI and decode its JSON reply into `Vec<CsvColumn>`.
/// `None` is returned on any failure path (inference rejected, JSON
/// malformed, missing fields).
fn csv_infer_columns(bytes: &[u8], delimiter: u8) -> Option<Vec<CsvColumn>> {
    let mut info: *mut c_char = ptr::null_mut();
    let mut errmsg: *mut c_char = ptr::null_mut();
    let ok = unsafe {
        morloc_csv_infer(bytes.as_ptr(), bytes.len(), delimiter, &mut info, &mut errmsg)
    };
    let _ = take_c_errmsg(errmsg);
    if !ok || info.is_null() {
        return None;
    }
    let json = unsafe { std::ffi::CStr::from_ptr(info) }
        .to_string_lossy()
        .into_owned();
    unsafe { libc::free(info as *mut std::ffi::c_void) };
    let parsed: serde_json::Value = serde_json::from_str(&json).ok()?;
    let arr = parsed.get("columns")?.as_array()?;
    let mut out = Vec::with_capacity(arr.len());
    for entry in arr {
        let name = entry.get("name")?.as_str()?.to_string();
        let type_category = entry.get("type")?.as_str()?.to_string();
        out.push(CsvColumn { name, type_category });
    }
    Some(out)
}

/// True iff the probe is valid UTF-8 (accepting truncation at the
/// boundary) and contains no NUL byte. Returns the encoding flag:
/// `Some(true)` for pure ASCII (every byte < 0x80), `Some(false)`
/// for UTF-8 with non-ASCII codepoints. `None` if the probe is not
/// text.
fn looks_like_text(bytes: &[u8]) -> Option<bool> {
    if bytes.is_empty() {
        return None;
    }
    if bytes.contains(&0x00) {
        return None;
    }
    match std::str::from_utf8(bytes) {
        Ok(_) => Some(bytes.iter().all(|&b| b < 0x80)),
        Err(e) if e.error_len().is_none() => {
            // Probe boundary cut a multibyte sequence; everything up to
            // `valid_up_to` is good. Treat as text.
            Some(bytes[..e.valid_up_to()].iter().all(|&b| b < 0x80))
        }
        Err(_) => None,
    }
}

/// Walk a morloc packet. `head` carries the bytes already read by
/// the sniff in `classify_reader`; the rest of the header is read
/// from `f` without seeking back.
fn classify_morloc_packet<R: Read + Seek>(
    f: &mut R,
    file_size: u64,
    head: &[u8],
) -> Classification {
    let mut hdr_bytes = [0u8; 32];
    let from_head = head.len().min(32);
    hdr_bytes[..from_head].copy_from_slice(&head[..from_head]);
    if from_head < 32 {
        if let Err(e) = f.read_exact(&mut hdr_bytes[from_head..]) {
            return Classification::Error(format!(
                "morloc packet truncated (header read failed: {})",
                e
            ));
        }
    }
    let hdr = match PacketHeader::from_bytes(&hdr_bytes) {
        Ok(h) => h,
        Err(e) => return Classification::Error(format!("invalid morloc packet ({})", e)),
    };

    let outer_offset = { hdr.offset } as u64;
    let outer_length = { hdr.length };
    let declared_total = 32u64
        .checked_add(outer_offset)
        .and_then(|n| n.checked_add(outer_length))
        .unwrap_or(u64::MAX);
    if file_size != declared_total {
        let kind = if file_size < declared_total {
            "truncated"
        } else {
            "oversize"
        };
        return Classification::Error(format!(
            "{} morloc data packet: header declares {} bytes, file is {} bytes",
            kind, declared_total, file_size
        ));
    }

    match hdr.command_type() {
        PACKET_TYPE_DATA => {
            let arg = match read_data_arg(f, &hdr, outer_offset, outer_length) {
                Ok(a) => a,
                Err(e) => return Classification::Error(e),
            };
            Classification::MorlocDataPacket {
                arg,
                total_bytes: declared_total,
            }
        }
        PACKET_TYPE_CALL => {
            let midx = unsafe { hdr.command.call.midx };
            let entrypoint = unsafe { hdr.command.call.entrypoint };
            let args = walk_call_args(f, outer_length);
            Classification::MorlocCallPacket {
                midx,
                entrypoint,
                args,
                payload_bytes: outer_length,
                total_bytes: declared_total,
            }
        }
        PACKET_TYPE_PING => Classification::MorlocPingPacket,
        other => Classification::Error(format!(
            "unknown morloc packet command type: 0x{:02x}",
            other
        )),
    }
}

/// Read DATA fields from an already-parsed header + read the
/// metadata block (without touching the payload). The reader is left
/// at byte `32 + metadata_offset` afterwards.
fn read_data_arg<R: Read + Seek>(
    f: &mut R,
    hdr: &PacketHeader,
    metadata_offset: u64,
    payload_length: u64,
) -> Result<DataArg, String> {
    let (source, format, compression) = unsafe {
        let d = hdr.command.data;
        (d.source, d.format, d.compression)
    };
    let metadata = if metadata_offset == 0 {
        Vec::new()
    } else {
        let mut buf = vec![0u8; metadata_offset as usize];
        f.read_exact(&mut buf)
            .map_err(|e| format!("metadata read failed: {}", e))?;
        buf
    };
    let schema = extract_schema_from_metadata(&metadata);
    Ok(DataArg {
        source,
        format,
        compression,
        schema,
        payload_bytes: payload_length,
        metadata_bytes: metadata_offset as u32,
    })
}

/// Walk the inner arg packets of a CALL packet, seeking past each
/// arg's payload. The reader must be positioned at byte 32 (CALL
/// packets have outer offset = 0; the args start immediately after
/// the outer header).
///
/// Per-arg I/O cost is constant in the arg's payload size:
/// `read_exact(32) + read_exact(arg_offset) + seek(arg_length)`.
/// On payload overrun, append a `Malformed` entry and stop the walk.
fn walk_call_args<R: Read + Seek>(f: &mut R, outer_length: u64) -> Vec<ArgEntry> {
    let mut args = Vec::new();
    // Reader is currently at offset 32 (right after the outer header).
    // The arg payload bytes occupy `outer_length` total. Track the
    // running position relative to the start of the payload.
    let mut consumed: u64 = 0;
    loop {
        if consumed == outer_length {
            return args;
        }
        if consumed > outer_length {
            args.push(ArgEntry::Malformed(format!(
                "size overruns parent (read {} bytes; payload is {})",
                consumed, outer_length
            )));
            return args;
        }
        // Need at least 32 bytes of header.
        if outer_length - consumed < 32 {
            args.push(ArgEntry::Malformed(format!(
                "remaining payload {} B is less than a 32-byte header",
                outer_length - consumed
            )));
            return args;
        }
        let mut hdr_bytes = [0u8; 32];
        if let Err(e) = f.read_exact(&mut hdr_bytes) {
            args.push(ArgEntry::Malformed(format!("header read failed: {}", e)));
            return args;
        }
        let hdr = match PacketHeader::from_bytes(&hdr_bytes) {
            Ok(h) => h,
            Err(e) => {
                args.push(ArgEntry::Malformed(format!("invalid arg header: {}", e)));
                return args;
            }
        };
        // Inner args must be DATA packets — the type system forbids
        // CALL-of-CALL.
        if hdr.command_type() != PACKET_TYPE_DATA {
            args.push(ArgEntry::Malformed(format!(
                "expected DATA arg, got command type 0x{:02x}",
                hdr.command_type()
            )));
            return args;
        }
        let arg_offset = { hdr.offset } as u64;
        let arg_length = { hdr.length };
        let consumed_after = consumed
            .checked_add(32)
            .and_then(|n| n.checked_add(arg_offset))
            .and_then(|n| n.checked_add(arg_length))
            .unwrap_or(u64::MAX);
        if consumed_after > outer_length {
            args.push(ArgEntry::Malformed(format!(
                "size overruns parent (would consume {} bytes; payload is {})",
                consumed_after, outer_length
            )));
            return args;
        }
        let arg = match read_data_arg(f, &hdr, arg_offset, arg_length) {
            Ok(a) => a,
            Err(e) => {
                args.push(ArgEntry::Malformed(e));
                return args;
            }
        };
        // Skip the arg's payload entirely.
        if arg_length > 0 {
            if let Err(e) = f.seek(SeekFrom::Current(arg_length as i64)) {
                args.push(ArgEntry::Malformed(format!("payload seek failed: {}", e)));
                return args;
            }
        }
        args.push(ArgEntry::Data(arg));
        consumed = consumed_after;
    }
}

/// Scan a metadata block for the first SCHEMA_STRING entry. Returns
/// `None` if absent or if the block is empty/corrupt.
fn extract_schema_from_metadata(meta: &[u8]) -> Option<String> {
    iter_metadata(meta)
        .find(|(kind, _)| *kind == METADATA_TYPE_SCHEMA_STRING)
        .map(|(_, data)| decode_schema_entry(data))
}

// ---------------------------------------------------------------------------
// Sniff helpers
// ---------------------------------------------------------------------------

fn read_at_most<R: Read>(r: &mut R, buf: &mut [u8]) -> std::io::Result<usize> {
    let mut total = 0;
    while total < buf.len() {
        match r.read(&mut buf[total..]) {
            Ok(0) => break,
            Ok(n) => total += n,
            Err(e) if e.kind() == std::io::ErrorKind::Interrupted => continue,
            Err(e) => return Err(e),
        }
    }
    Ok(total)
}

// ---------------------------------------------------------------------------
// Rendering
// ---------------------------------------------------------------------------

/// Compact, key=value-style renderer for `morloc-nexus file` plain
/// output. Each rendered file consumes exactly one line in default
/// mode (the one-line-per-file guarantee). `-v / --verbose` opts
/// into multi-line output for call packets (one indented line per
/// arg) and CSV files (one indented line per column).
///
/// Line shape: `[<path>: ]<type>[ key=value ...]`. The path prefix
/// is suppressed by `--no-file`; the description fields are
/// suppressed by `--no-description`. `--validate` appends one extra
/// `validated=...` field.
pub fn format_plain(
    cls: &Classification,
    path: &str,
    opts: &RenderOpts,
    validated: Option<&ValidationOutcome>,
) -> String {
    let prefix = if opts.no_file { String::new() } else { format!("{}: ", path) };
    let mut head = format!("{}{}", prefix, type_token(cls));
    if !opts.no_description {
        let fields = description_fields(cls, validated);
        if !fields.is_empty() {
            head.push(' ');
            head.push_str(&fields.join(" "));
        }
    }
    if opts.verbose {
        for line in verbose_extra_lines(cls) {
            head.push('\n');
            head.push_str(&line);
        }
    }
    head
}

/// Short, spaceless type label. The leading token a downstream
/// `awk '{print $2}'` picks up.
fn type_token(cls: &Classification) -> &'static str {
    match cls {
        Classification::MorlocDataPacket { .. } => "data-packet",
        Classification::MorlocCallPacket { .. } => "call-packet",
        Classification::MorlocPingPacket => "ping-packet",
        Classification::Json => "json",
        Classification::MessagePack => "msgpack",
        Classification::Csv { .. } => "csv",
        Classification::Arrow => "arrow-ipc",
        Classification::Parquet => "parquet",
        Classification::Text { .. } => "text",
        Classification::Empty => "empty",
        Classification::Unknown => "data",
        Classification::Directory => "directory",
        Classification::Error(_) => "error",
    }
}

/// Description fields rendered after the type token. Empty for kinds
/// with no extra info (e.g. ping, arrow, parquet).
fn description_fields(
    cls: &Classification,
    validated: Option<&ValidationOutcome>,
) -> Vec<String> {
    let mut fields = match cls {
        Classification::MorlocDataPacket { arg, total_bytes } => {
            data_arg_fields(arg, Some(*total_bytes))
        }
        Classification::MorlocCallPacket {
            midx, entrypoint, args, payload_bytes, total_bytes,
        } => vec![
            format!("midx={}", midx),
            format!("entrypoint={}", packet_entrypoint_name(*entrypoint)),
            format!("nargs={}", args.len()),
            format!("payload={}", payload_bytes),
            format!("total={}", total_bytes),
        ],
        Classification::Csv { delimiter, columns } => vec![
            format!("columns={}", columns.len()),
            format!("delimiter={}", quote_delimiter(*delimiter)),
        ],
        Classification::Text { ascii_only } => vec![format!(
            "encoding={}",
            if *ascii_only { "ascii" } else { "utf-8" }
        )],
        Classification::Error(msg) => vec![format!("message={}", json_quote(msg))],
        _ => Vec::new(),
    };
    if let Some(v) = validated {
        fields.push(format_validated_field(v));
    }
    fields
}

/// Extra indented lines emitted only in `--verbose` mode. One per
/// CALL-packet arg, or one per CSV column.
fn verbose_extra_lines(cls: &Classification) -> Vec<String> {
    match cls {
        Classification::MorlocCallPacket { args, .. } => args
            .iter()
            .enumerate()
            .map(|(i, e)| match e {
                ArgEntry::Data(d) => {
                    let f = data_arg_fields(d, None);
                    format!("  arg[{}]: data-packet {}", i, f.join(" "))
                }
                ArgEntry::Malformed(reason) => {
                    format!("  arg[{}]: malformed message={}", i, json_quote(reason))
                }
            })
            .collect(),
        Classification::Csv { columns, .. } => columns
            .iter()
            .map(|c| format!("  {}:{}", c.name, c.type_category))
            .collect(),
        _ => Vec::new(),
    }
}

fn data_arg_fields(arg: &DataArg, total_bytes: Option<u64>) -> Vec<String> {
    let mut fields = vec![
        format!("source={}", packet_source_name(arg.source)),
        format!("format={}", packet_format_name(arg.format)),
    ];
    if arg.compression != PACKET_COMPRESSION_NONE {
        fields.push(format!(
            "compression={}",
            packet_compression_name(arg.compression)
        ));
    }
    if let Some(s) = &arg.schema {
        fields.push(format!("schema={}", json_quote(s)));
    }
    fields.push(format!("payload={}", arg.payload_bytes));
    if arg.metadata_bytes != 0 {
        fields.push(format!("metadata={}", arg.metadata_bytes));
    }
    if let Some(t) = total_bytes {
        fields.push(format!("total={}", t));
    }
    fields
}

fn format_validated_field(v: &ValidationOutcome) -> String {
    match v {
        ValidationOutcome::Passed => "validated=yes".to_string(),
        ValidationOutcome::Failed(err) => format!("validated=no error={}", json_quote(err)),
        ValidationOutcome::StructureOnly => "validated=structure-only".to_string(),
    }
}

/// JSON-quote a string for use as a key=value field. Handles quotes,
/// backslashes, and control chars so the output stays awk-parseable.
fn json_quote(s: &str) -> String {
    serde_json::Value::String(s.to_string()).to_string()
}

/// Render the delimiter byte as a JSON-quoted string literal so a
/// downstream consumer can split on `delimiter="<value>"` without
/// guessing.
fn quote_delimiter(d: char) -> String {
    let s: String = d.to_string();
    json_quote(&s)
}

pub fn format_json(
    cls: &Classification,
    path: &str,
    validated: Option<&ValidationOutcome>,
) -> String {
    let mut v = serde_json::Map::new();
    v.insert("path".into(), serde_json::Value::String(path.into()));
    match cls {
        Classification::MorlocDataPacket { arg, total_bytes } => {
            v.insert("kind".into(), "morloc-packet".into());
            v.insert("subkind".into(), "data".into());
            insert_data_arg_fields(&mut v, arg);
            v.insert("total_bytes".into(), serde_json::json!(total_bytes));
        }
        Classification::MorlocCallPacket {
            midx,
            entrypoint,
            args,
            payload_bytes,
            total_bytes,
        } => {
            v.insert("kind".into(), "morloc-packet".into());
            v.insert("subkind".into(), "call".into());
            v.insert("midx".into(), serde_json::json!(midx));
            v.insert("entrypoint".into(), packet_entrypoint_name(*entrypoint).into());
            v.insert("nargs".into(), serde_json::json!(args.len()));
            v.insert("payload_bytes".into(), serde_json::json!(payload_bytes));
            v.insert("total_bytes".into(), serde_json::json!(total_bytes));
            let args_json: Vec<serde_json::Value> = args
                .iter()
                .map(|e| match e {
                    ArgEntry::Data(d) => {
                        let mut m = serde_json::Map::new();
                        m.insert("subkind".into(), "data".into());
                        insert_data_arg_fields(&mut m, d);
                        serde_json::Value::Object(m)
                    }
                    ArgEntry::Malformed(r) => {
                        serde_json::json!({"subkind": "malformed", "reason": r})
                    }
                })
                .collect();
            v.insert("args".into(), serde_json::Value::Array(args_json));
        }
        Classification::MorlocPingPacket => {
            v.insert("kind".into(), "morloc-packet".into());
            v.insert("subkind".into(), "ping".into());
        }
        Classification::Json => {
            v.insert("kind".into(), "json".into());
        }
        Classification::MessagePack => {
            v.insert("kind".into(), "messagepack".into());
        }
        Classification::Csv { delimiter, columns } => {
            v.insert("kind".into(), "csv".into());
            v.insert(
                "delimiter".into(),
                serde_json::Value::String(delimiter.to_string()),
            );
            let col_array: Vec<serde_json::Value> = columns
                .iter()
                .map(|c| {
                    serde_json::json!({"name": c.name, "type": c.type_category})
                })
                .collect();
            v.insert(
                "column_count".into(),
                serde_json::json!(columns.len()),
            );
            v.insert("columns".into(), serde_json::Value::Array(col_array));
        }
        Classification::Arrow => {
            v.insert("kind".into(), "arrow-ipc".into());
        }
        Classification::Parquet => {
            v.insert("kind".into(), "parquet".into());
        }
        Classification::Text { ascii_only } => {
            v.insert("kind".into(), "text".into());
            v.insert(
                "encoding".into(),
                if *ascii_only { "ascii" } else { "utf-8" }.into(),
            );
        }
        Classification::Empty => {
            v.insert("kind".into(), "empty".into());
        }
        Classification::Unknown => {
            v.insert("kind".into(), "data".into());
        }
        Classification::Directory => {
            v.insert("kind".into(), "directory".into());
        }
        Classification::Error(msg) => {
            v.insert("kind".into(), "error".into());
            v.insert("error".into(), serde_json::Value::String(msg.clone()));
        }
    }
    if let Some(val) = validated {
        match val {
            ValidationOutcome::Passed => {
                v.insert("validated".into(), serde_json::Value::Bool(true));
            }
            ValidationOutcome::Failed(err) => {
                v.insert("validated".into(), serde_json::Value::Bool(false));
                v.insert("error".into(), serde_json::Value::String(err.clone()));
            }
            ValidationOutcome::StructureOnly => {
                v.insert("validated".into(), "structure-only".into());
            }
        }
    }
    serde_json::Value::Object(v).to_string()
}

fn insert_data_arg_fields(m: &mut serde_json::Map<String, serde_json::Value>, arg: &DataArg) {
    m.insert("source".into(), packet_source_name(arg.source).into());
    m.insert("format".into(), packet_format_name(arg.format).into());
    m.insert("compression".into(), packet_compression_name(arg.compression).into());
    if let Some(s) = &arg.schema {
        m.insert("schema".into(), serde_json::Value::String(s.clone()));
    }
    m.insert("payload_bytes".into(), serde_json::json!(arg.payload_bytes));
    if arg.metadata_bytes != 0 {
        m.insert("metadata_bytes".into(), serde_json::json!(arg.metadata_bytes));
    }
}

#[cfg(test)]
mod tests;
