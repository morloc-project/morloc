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
    decode_footer_status, decode_schema_entry, decode_stream_tail, decode_subpacket_index,
    iter_metadata, packet_compression_name, packet_entrypoint_name, packet_format_name,
    packet_source_name, PacketHeader, StreamDiag, StreamDiagView, FOOTER_STATUS_CLOSED,
    FOOTER_STATUS_FAILED, FOOTER_STATUS_PAUSED, FOOTER_STATUS_REPAIRED,
    METADATA_TYPE_FOOTER_FINAL, METADATA_TYPE_FOOTER_STATUS, METADATA_TYPE_SCHEMA_STRING,
    METADATA_TYPE_STREAM_DIAG, METADATA_TYPE_SUBPACKET_INDEX, PACKET_COMPRESSION_NONE,
    PACKET_MAGIC, PACKET_TYPE_CALL, PACKET_TYPE_DATA, PACKET_TYPE_PING, PACKET_TYPE_STREAM,
    STREAM_TAIL_SIZE,
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

/// Trailer state of a stream file. `Missing` means the 8-byte EOF tail
/// did not carry the STREAM_TAIL_MAGIC, so no footer packet is present.
/// `Temp` is a footer that carries only the diagnostic block (writer
/// still in progress). `Final` also carries a status byte set by the
/// writer at close time; the subpacket count lives on `diag`.
/// `Missing` means the on-disk EOF tail is absent (writer crashed or
/// stdout redirected without close); the `walk` field, when non-None,
/// carries what a bounded forward walk of sub-packet headers found.
/// The on-disk state is unchanged either way -- `file` never repairs
/// the packet.
#[derive(Debug, Clone)]
pub enum FooterInfo {
    Missing { walk: Option<WalkSummary> },
    Temp { diag: StreamDiagView },
    Final { diag: StreamDiagView, status: u8 },
}

/// Result of a bounded forward walk of sub-packet headers on a
/// footer-less stream file. Purely diagnostic -- the walker only
/// reads; nothing is written back to the file.
#[derive(Debug, Clone, Copy)]
pub struct WalkSummary {
    pub subpacket_count: u64,
    pub element_count: u64,
    /// True iff the walk stopped at a partial trailing sub-packet
    /// (writer crashed mid-write).
    pub partial: bool,
    /// Number of sub-packets whose element count was not tallied
    /// (compressed payload, count-decoding skipped for perf).
    pub compressed_uncounted: u64,
    /// True iff the walk hit the sub-packet count cap and stopped
    /// early. When true, `subpacket_count` reflects the cap and
    /// `element_count` covers only the sub-packets actually walked.
    pub scan_capped: bool,
}

/// Default sub-packet-count cap for the footer-less forward scan.
/// Overridable via `MORLOC_FILE_MAX_SCAN_SUBPACKETS`. Keeps `file`'s
/// fast-classification contract intact even on pathological inputs
/// (e.g. writer that flushes after every element).
pub const DEFAULT_FILE_MAX_SCAN_SUBPACKETS: u64 = 10_000;

fn file_max_scan_subpackets() -> u64 {
    std::env::var("MORLOC_FILE_MAX_SCAN_SUBPACKETS")
        .ok()
        .and_then(|s| s.parse::<u64>().ok())
        .unwrap_or(DEFAULT_FILE_MAX_SCAN_SUBPACKETS)
}

/// Result of classifying one file.
#[derive(Debug, Clone)]
pub enum Classification {
    MorlocDataPacket {
        arg: DataArg,
    },
    MorlocCallPacket {
        midx: u32,
        entrypoint: u8,
        args: Vec<ArgEntry>,
        payload_bytes: u64,
    },
    MorlocStreamPacket {
        schema: Option<String>,
        footer: FooterInfo,
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

    match hdr.command_type() {
        PACKET_TYPE_DATA => {
            if let Err(e) = check_declared_total(outer_offset, outer_length, file_size) {
                return Classification::Error(e);
            }
            match read_data_arg(f, &hdr, outer_offset, outer_length) {
                Ok(arg) => Classification::MorlocDataPacket { arg },
                Err(e) => Classification::Error(e),
            }
        }
        PACKET_TYPE_CALL => {
            if let Err(e) = check_declared_total(outer_offset, outer_length, file_size) {
                return Classification::Error(e);
            }
            let midx = unsafe { hdr.command.call.midx };
            let entrypoint = unsafe { hdr.command.call.entrypoint };
            let args = walk_call_args(f, outer_length);
            Classification::MorlocCallPacket {
                midx,
                entrypoint,
                args,
                payload_bytes: outer_length,
            }
        }
        PACKET_TYPE_PING => {
            if let Err(e) = check_declared_total(outer_offset, outer_length, file_size) {
                return Classification::Error(e);
            }
            Classification::MorlocPingPacket
        }
        PACKET_TYPE_STREAM => classify_stream_packet(f, file_size, outer_offset),
        other => Classification::Error(format!(
            "unknown morloc packet command type: 0x{:02x}",
            other
        )),
    }
}

/// Confirm the on-disk file size matches the header-declared total
/// (`32 + offset + length`). STREAM packets skip this: their length
/// field is the sentinel `u64::MAX` and the wire shape is instead
/// defined by an EOF tail.
fn check_declared_total(
    outer_offset: u64,
    outer_length: u64,
    file_size: u64,
) -> Result<(), String> {
    let declared_total = 32u64
        .checked_add(outer_offset)
        .and_then(|n| n.checked_add(outer_length))
        .unwrap_or(u64::MAX);
    if file_size == declared_total {
        return Ok(());
    }
    let kind = if file_size < declared_total { "truncated" } else { "oversize" };
    Err(format!(
        "{} morloc data packet: header declares {} bytes, file is {} bytes",
        kind, declared_total, file_size
    ))
}

/// Inspect a STREAM packet: header metadata (schema) + the 8-byte EOF
/// tail + (when present) the footer packet. Always O(1) in file size:
/// three seeks and three small reads at worst. Never forward-walks the
/// body sub-packets. Tolerant of truncation past the header: a stream
/// whose writer died before it could finish the metadata block still
/// classifies as `stream-packet state=missing` with whatever schema
/// bytes made it to disk.
fn classify_stream_packet<R: Read + Seek>(
    f: &mut R,
    file_size: u64,
    metadata_offset: u64,
) -> Classification {
    // 1. Header metadata carries the element schema. Clamp the read to
    //    the bytes that actually exist past the 32-byte header, so a
    //    truncated file doesn't fault out here -- iter_metadata stops
    //    at the first non-`mmh` byte, so a valid SCHEMA_STRING entry
    //    at the start of the block is still recoverable.
    let after_header = file_size.saturating_sub(32);
    let want_meta = (metadata_offset.min(after_header)) as usize;
    let schema = if want_meta == 0 {
        None
    } else {
        let mut meta = vec![0u8; want_meta];
        if let Err(e) = f.read_exact(&mut meta) {
            // The read is clamped to bytes we know are on disk, so a
            // failure here is a real I/O problem, not just truncation.
            return Classification::Error(format!(
                "stream header metadata read failed: {}", e
            ));
        }
        extract_schema_from_metadata(&meta)
    };

    // 2. Test for the EOF tail. If the file is smaller than a stream
    //    header + metadata + tail there is no meaningful footer to
    //    look for.
    let footer_result = if file_size < 32 + metadata_offset + STREAM_TAIL_SIZE as u64 {
        FooterInfo::Missing { walk: None }
    } else {
        match read_stream_footer(f, file_size) {
            Ok(info) => info,
            Err(e) => return Classification::Error(e),
        }
    };

    // 3. If the tail was absent, forward-walk sub-packet headers to
    //    surface some diagnostics. The scan is capped at
    //    `MORLOC_FILE_MAX_SCAN_SUBPACKETS` (default 10,000) so
    //    pathologically flush-heavy inputs cannot make `file` hang;
    //    hitting the cap sets `scan_capped=true` on the summary and
    //    the on-disk state stays `missing` regardless.
    let footer = if matches!(footer_result, FooterInfo::Missing { .. }) {
        let body_start = 32 + metadata_offset;
        let cap = file_max_scan_subpackets();
        match forward_scan_stream_body(f, file_size, body_start, cap) {
            Ok(scan) if !scan.subpacket_offsets.is_empty() || scan.partial || scan.scan_capped => {
                FooterInfo::Missing {
                    walk: Some(WalkSummary {
                        subpacket_count: scan.subpacket_offsets.len() as u64,
                        element_count: scan.element_count,
                        partial: scan.partial,
                        compressed_uncounted: scan.compressed_uncounted,
                        scan_capped: scan.scan_capped,
                    }),
                }
            }
            _ => FooterInfo::Missing { walk: None },
        }
    } else {
        footer_result
    };

    Classification::MorlocStreamPacket { schema, footer }
}

/// Forward-walk sub-packet headers via seek+read on a `Read + Seek`.
/// Mirrors `morloc_runtime_types::packet::forward_scan_subpackets_capped`
/// (the `&[u8]` canonical version); the two must produce identical
/// results on identical bytes. `cap` bounds the number of sub-packet
/// headers read so `file`'s fast-classification contract holds even
/// on pathological flush-every-element inputs; on hit the scan sets
/// `scan_capped=true` and stops.
fn forward_scan_stream_body<R: Read + Seek>(
    f: &mut R,
    file_size: u64,
    body_start: u64,
    cap: u64,
) -> Result<morloc_runtime_types::packet::ForwardScan, String> {
    use morloc_runtime_types::packet::{
        ForwardScan, PACKET_COMPRESSION_NONE,
    };

    if body_start > file_size {
        return Err(format!(
            "forward-scan body_start {} exceeds file size {}",
            body_start, file_size
        ));
    }
    let mut out = ForwardScan::default();
    let mut cur = body_start;
    let mut hdr_bytes = [0u8; 32];
    let mut size_bytes = [0u8; 8];

    while cur + 32 <= file_size {
        if out.subpacket_offsets.len() as u64 >= cap {
            out.scan_capped = true;
            break;
        }
        f.seek(SeekFrom::Start(cur))
            .map_err(|e| format!("forward-scan seek to {}: {}", cur, e))?;
        f.read_exact(&mut hdr_bytes)
            .map_err(|e| format!("forward-scan header read at {}: {}", cur, e))?;
        let header = match PacketHeader::from_bytes(&hdr_bytes) {
            Ok(h) => h,
            Err(_) => break,
        };
        if header.is_footer() || !header.is_data() {
            break;
        }
        let offset = header.offset as u64;
        let length = header.length as u64;
        let total = match 32u64
            .checked_add(offset)
            .and_then(|x| x.checked_add(length))
        {
            Some(t) => t,
            None => return Err("forward-scan sub-packet size overflow".into()),
        };
        if cur + total > file_size {
            out.partial = true;
            break;
        }
        let data = unsafe { header.command.data };
        let is_voidstar = data.format
            == morloc_runtime_types::packet::PACKET_FORMAT_VOIDSTAR;
        let is_uncompressed = data.compression == PACKET_COMPRESSION_NONE;
        if is_voidstar && is_uncompressed {
            let payload_start = cur + 32 + offset;
            if payload_start + 8 > file_size {
                out.partial = true;
                break;
            }
            f.seek(SeekFrom::Start(payload_start))
                .map_err(|e| format!("forward-scan payload seek: {}", e))?;
            f.read_exact(&mut size_bytes)
                .map_err(|e| format!("forward-scan array-size read: {}", e))?;
            let elem_count = u64::from_le_bytes(size_bytes);
            out.element_count = out.element_count.saturating_add(elem_count);
        } else {
            out.compressed_uncounted =
                out.compressed_uncounted.saturating_add(1);
        }
        out.subpacket_offsets.push(cur);
        cur += total;
    }
    out.bytes_scanned = cur.saturating_sub(body_start);
    Ok(out)
}

/// Read the EOF tail and, if present, the footer packet. Returns
/// `FooterInfo::Missing` when the tail magic is absent.
fn read_stream_footer<R: Read + Seek>(
    f: &mut R,
    file_size: u64,
) -> Result<FooterInfo, String> {
    let tail_off = file_size - STREAM_TAIL_SIZE as u64;
    f.seek(SeekFrom::Start(tail_off))
        .map_err(|e| format!("seek to stream tail failed: {}", e))?;
    let mut tail = [0u8; STREAM_TAIL_SIZE];
    f.read_exact(&mut tail)
        .map_err(|e| format!("stream tail read failed: {}", e))?;
    let Some(footer_len) = decode_stream_tail(&tail) else {
        return Ok(FooterInfo::Missing { walk: None });
    };
    let footer_len = footer_len as u64;

    // Footer packet sits at `file_size - 8 - footer_len`. Sanity-check
    // it fits inside the file before seeking.
    let Some(footer_off) = tail_off.checked_sub(footer_len) else {
        return Err(format!(
            "stream tail declares footer_len={}, but only {} bytes precede the tail",
            footer_len, tail_off
        ));
    };
    if footer_len < 32 {
        return Err(format!(
            "stream footer too small: declared {} B (< 32 B header)",
            footer_len
        ));
    }

    f.seek(SeekFrom::Start(footer_off))
        .map_err(|e| format!("seek to stream footer failed: {}", e))?;
    let mut hdr_bytes = [0u8; 32];
    f.read_exact(&mut hdr_bytes)
        .map_err(|e| format!("stream footer header read failed: {}", e))?;
    let footer_hdr = PacketHeader::from_bytes(&hdr_bytes)
        .map_err(|e| format!("stream footer header invalid: {}", e))?;
    if !footer_hdr.is_footer() {
        return Err(format!(
            "stream tail points to a non-FOOTER packet (cmd_type 0x{:02x})",
            footer_hdr.command_type()
        ));
    }
    let body_len = { footer_hdr.offset } as u64;
    let expected_body = footer_len - 32;
    if body_len != expected_body {
        return Err(format!(
            "stream footer body length mismatch: header offset={}, tail declares {}",
            body_len, expected_body
        ));
    }
    let mut body = vec![0u8; body_len as usize];
    if body_len > 0 {
        f.read_exact(&mut body)
            .map_err(|e| format!("stream footer body read failed: {}", e))?;
    }

    let mut diag: Option<StreamDiagView> = None;
    let mut is_final = false;
    let mut status: u8 = FOOTER_STATUS_CLOSED;
    let mut indexed_subpackets: Option<u64> = None;
    for (kind, data) in iter_metadata(&body) {
        match kind {
            METADATA_TYPE_STREAM_DIAG => {
                let d = StreamDiag::from_bytes(data)
                    .map_err(|e| format!("stream footer diag block invalid: {}", e))?;
                diag = Some(d.snapshot());
            }
            METADATA_TYPE_FOOTER_FINAL => is_final = true,
            METADATA_TYPE_FOOTER_STATUS => status = decode_footer_status(data),
            METADATA_TYPE_SUBPACKET_INDEX => {
                let index = decode_subpacket_index(data)
                    .map_err(|e| format!("stream footer subpacket index invalid: {}", e))?;
                indexed_subpackets = Some(index.len() as u64);
            }
            _ => {}
        }
    }
    let Some(mut diag) = diag else {
        return Err("stream footer missing required STREAM_DIAG block".into());
    };
    // Prefer the exact SUBPACKET_INDEX length when present; older
    // footers may omit it, in which case diag's running total stands.
    if let Some(n) = indexed_subpackets {
        diag.subpacket_count = n;
    }
    if is_final {
        Ok(FooterInfo::Final { diag, status })
    } else {
        Ok(FooterInfo::Temp { diag })
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
        Classification::MorlocStreamPacket { .. } => "stream-packet",
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

/// String name for a `FOOTER_STATUS_*` byte. Falls through to a hex
/// literal for unknown values so the plain output stays informative.
fn footer_status_name(status: u8) -> String {
    match status {
        FOOTER_STATUS_CLOSED => "closed".to_string(),
        FOOTER_STATUS_PAUSED => "paused".to_string(),
        FOOTER_STATUS_FAILED => "failed".to_string(),
        FOOTER_STATUS_REPAIRED => "repaired".to_string(),
        other => format!("0x{:02x}", other),
    }
}

/// Description fields rendered after the type token. Empty for kinds
/// with no extra info (e.g. ping, arrow, parquet).
fn description_fields(
    cls: &Classification,
    validated: Option<&ValidationOutcome>,
) -> Vec<String> {
    let mut fields = match cls {
        Classification::MorlocDataPacket { arg } => data_arg_fields(arg),
        Classification::MorlocCallPacket {
            midx, entrypoint, args, payload_bytes,
        } => vec![
            format!("midx={}", midx),
            format!("entrypoint={}", packet_entrypoint_name(*entrypoint)),
            format!("nargs={}", args.len()),
            format!("payload_full_size={}", payload_bytes),
        ],
        Classification::MorlocStreamPacket { schema, footer } => {
            stream_packet_fields(schema.as_deref(), footer)
        }
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
/// CALL-packet arg, or one per CSV column, plus per-field diag lines
/// for stream footers.
fn verbose_extra_lines(cls: &Classification) -> Vec<String> {
    match cls {
        Classification::MorlocCallPacket { args, .. } => args
            .iter()
            .enumerate()
            .map(|(i, e)| match e {
                ArgEntry::Data(d) => {
                    let f = data_arg_fields(d);
                    format!("  arg[{}]: data-packet {}", i, f.join(" "))
                }
                ArgEntry::Malformed(reason) => {
                    format!("  arg[{}]: malformed message={}", i, json_quote(reason))
                }
            })
            .collect(),
        Classification::MorlocStreamPacket { footer, .. } => stream_verbose_lines(footer),
        Classification::Csv { columns, .. } => columns
            .iter()
            .map(|c| format!("  {}:{}", c.name, c.type_category))
            .collect(),
        _ => Vec::new(),
    }
}

/// Fields emitted on the single default line for a stream packet. The
/// three footer states share a common preamble (schema=, state=)
/// and diverge in the fields carried after that. Sizes come from the
/// StreamDiag block; on-disk file size is deliberately omitted --
/// `ls -s` / `wc -c` already report it.
fn stream_packet_fields(
    schema: Option<&str>,
    footer: &FooterInfo,
) -> Vec<String> {
    let mut fields = Vec::new();
    if let Some(s) = schema {
        fields.push(format!("schema={}", json_quote(s)));
    }
    let diag = match footer {
        FooterInfo::Missing { walk } => {
            // On-disk state: the EOF tail is absent. `file` never
            // repairs the file; the walk (when present) is purely
            // diagnostic.
            fields.push("state=missing".to_string());
            if let Some(w) = walk {
                fields.push(format!("subpackets={}{}",
                    w.subpacket_count,
                    if w.scan_capped { "+" } else { "" },
                ));
                if w.compressed_uncounted > 0 {
                    fields.push(format!(
                        "elements={}{} (excludes {} compressed sub-packet{})",
                        w.element_count,
                        if w.scan_capped { "+" } else { "" },
                        w.compressed_uncounted,
                        if w.compressed_uncounted == 1 { "" } else { "s" },
                    ));
                } else {
                    fields.push(format!(
                        "elements={}{}",
                        w.element_count,
                        if w.scan_capped { "+" } else { "" },
                    ));
                }
                if w.partial {
                    fields.push("partial=true".to_string());
                }
                if w.scan_capped {
                    fields.push(format!(
                        "footer_note={}",
                        json_quote(
                            "no footer at EOF; forward-scan hit sub-packet cap"
                        )
                    ));
                } else {
                    fields.push(format!(
                        "footer_note={}",
                        json_quote(
                            "no footer at EOF; forward-scan walked sub-packet headers"
                        )
                    ));
                }
            } else {
                fields.push(format!(
                    "footer_note={}",
                    json_quote("no footer at EOF (writer in progress, corrupt, or empty body)")
                ));
            }
            return fields;
        }
        FooterInfo::Temp { diag } => {
            fields.push("state=temp".to_string());
            diag
        }
        FooterInfo::Final { diag, status } => {
            fields.push("state=final".to_string());
            fields.push(format!("status={}", footer_status_name(*status)));
            diag
        }
    };
    fields.push(format!("subpackets={}", diag.subpacket_count));
    fields.push(format!("elements={}", diag.element_count));
    fields.push(format!("payload_full_size={}", diag.bytes_uncompressed_total));
    fields.push(format!("payload_wire_size={}", diag.bytes_compressed_total));
    fields
}

/// Verbose (`-v`) extra lines for a stream packet. The `Missing` state
/// has nothing extra to show; `Temp`/`Final` dump the remaining
/// `StreamDiag` fields plus the tail-window.
fn stream_verbose_lines(footer: &FooterInfo) -> Vec<String> {
    let diag = match footer {
        FooterInfo::Missing { .. } => return Vec::new(),
        FooterInfo::Temp { diag } | FooterInfo::Final { diag, .. } => diag,
    };
    let mut out = Vec::new();
    out.push(format!("  diag_version={}", diag.diag_version));
    out.push(format!("  writer_pid={}", diag.writer_pid));
    out.push(format!(
        "  n_oversize_subpackets={}", diag.n_oversize_subpackets
    ));
    out.push(format!("  writer_start_time={}", diag.writer_start_time));
    out.push(format!("  first_flush_time={}", diag.first_flush_time));
    out.push(format!("  last_flush_time={}", diag.last_flush_time));
    out.push(format!(
        "  largest_packet_uncompressed={}", diag.largest_packet_uncompressed
    ));
    out.push(format!("  largest_packet_idx={}", diag.largest_packet_idx));
    if !diag.tail.is_empty() {
        let joined = diag.tail
            .iter()
            .map(|v| v.to_string())
            .collect::<Vec<_>>()
            .join(",");
        out.push(format!("  tail_window=[{}]", joined));
    }
    out
}

fn data_arg_fields(arg: &DataArg) -> Vec<String> {
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
    fields.push(format!("payload_full_size={}", arg.payload_bytes));
    if arg.metadata_bytes != 0 {
        fields.push(format!("metadata={}", arg.metadata_bytes));
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
        Classification::MorlocDataPacket { arg } => {
            v.insert("kind".into(), "morloc-packet".into());
            v.insert("subkind".into(), "data".into());
            insert_data_arg_fields(&mut v, arg);
        }
        Classification::MorlocCallPacket {
            midx,
            entrypoint,
            args,
            payload_bytes,
        } => {
            v.insert("kind".into(), "morloc-packet".into());
            v.insert("subkind".into(), "call".into());
            v.insert("midx".into(), serde_json::json!(midx));
            v.insert("entrypoint".into(), packet_entrypoint_name(*entrypoint).into());
            v.insert("nargs".into(), serde_json::json!(args.len()));
            v.insert("payload_full_size".into(), serde_json::json!(payload_bytes));
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
        Classification::MorlocStreamPacket { schema, footer } => {
            v.insert("kind".into(), "morloc-packet".into());
            v.insert("subkind".into(), "stream".into());
            if let Some(s) = schema {
                v.insert("schema".into(), serde_json::Value::String(s.clone()));
            }
            v.insert("footer".into(), stream_footer_json(footer));
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

/// JSON serialization of `FooterInfo` for the `--json` renderer. Mirror
/// the plain fields but under a nested `"footer"` object, and expose the
/// full `StreamDiag` under `"diag"` so JSON consumers can extract any
/// field without needing `-v`.
fn stream_footer_json(footer: &FooterInfo) -> serde_json::Value {
    let mut m = serde_json::Map::new();
    let diag = match footer {
        FooterInfo::Missing { walk } => {
            m.insert("state".into(), "missing".into());
            if let Some(w) = walk {
                let mut wm = serde_json::Map::new();
                wm.insert("subpacket_count".into(), serde_json::json!(w.subpacket_count));
                wm.insert("element_count".into(), serde_json::json!(w.element_count));
                wm.insert("partial".into(), serde_json::json!(w.partial));
                wm.insert(
                    "compressed_uncounted".into(),
                    serde_json::json!(w.compressed_uncounted),
                );
                wm.insert("scan_capped".into(), serde_json::json!(w.scan_capped));
                m.insert("walk".into(), serde_json::Value::Object(wm));
            }
            return serde_json::Value::Object(m);
        }
        FooterInfo::Temp { diag } => {
            m.insert("state".into(), "temp".into());
            diag
        }
        FooterInfo::Final { diag, status } => {
            m.insert("state".into(), "final".into());
            m.insert("status".into(), footer_status_name(*status).into());
            diag
        }
    };
    m.insert("subpacket_count".into(), serde_json::json!(diag.subpacket_count));
    m.insert("diag".into(), stream_diag_json(diag));
    serde_json::Value::Object(m)
}

fn stream_diag_json(d: &StreamDiagView) -> serde_json::Value {
    serde_json::json!({
        "diag_version": d.diag_version,
        "writer_pid": d.writer_pid,
        "n_oversize_subpackets": d.n_oversize_subpackets,
        "writer_start_time": d.writer_start_time,
        "subpacket_count": d.subpacket_count,
        "element_count": d.element_count,
        "bytes_uncompressed_total": d.bytes_uncompressed_total,
        "bytes_compressed_total": d.bytes_compressed_total,
        "largest_packet_uncompressed": d.largest_packet_uncompressed,
        "largest_packet_idx": d.largest_packet_idx,
        "first_flush_time": d.first_flush_time,
        "last_flush_time": d.last_flush_time,
        "tail": d.tail,
    })
}

fn insert_data_arg_fields(m: &mut serde_json::Map<String, serde_json::Value>, arg: &DataArg) {
    m.insert("source".into(), packet_source_name(arg.source).into());
    m.insert("format".into(), packet_format_name(arg.format).into());
    m.insert("compression".into(), packet_compression_name(arg.compression).into());
    if let Some(s) = &arg.schema {
        m.insert("schema".into(), serde_json::Value::String(s.clone()));
    }
    m.insert("payload_full_size".into(), serde_json::json!(arg.payload_bytes));
    if arg.metadata_bytes != 0 {
        m.insert("metadata_bytes".into(), serde_json::json!(arg.metadata_bytes));
    }
}

#[cfg(test)]
mod tests;
