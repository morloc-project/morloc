//! `morloc-nexus view` subcommand.
//!
//! Reads a data file (morloc packet, .json, .mpk, .arrow, .parquet,
//! or .csv) and re-emits it in a chosen output format. The
//! implementation reuses the same FFI chain `run` uses for argument
//! ingress (`initialize_positional` -> `parse_cli_data_argument` ->
//! C `load_morloc_data_file`) and the same output emitter
//! (`dispatch::print_result_c`), so format support and conversion
//! semantics cannot drift away from a real morloc program run.

use std::ffi::{c_char, c_void, CString};
use std::path::Path;
use std::ptr;

use morloc_runtime_types::cschema::CSchema;
use morloc_runtime_types::packet::{PacketHeader, PACKET_FORMAT_ARROW, MLC_KIND_IFILE};
use morloc_runtime_types::pattern::{
    check_pattern_against_schema, parse_pattern, path_to_walker_input, EncodedArg,
};
use morloc_runtime_types::schema::{parse_schema, Schema};

use crate::cli::{OutputForm, ViewArgs};
use crate::dispatch::{print_result_c, NexusConfig};
use crate::file::{classify_path, Classification, DataArg, DEFAULT_PROBE_BYTES};
use crate::loader::load_with_schema;
use crate::process::{self, redirect_stdout_to, take_c_errmsg};

/// One runtime argument to `mlc_ifile_walk`. Must match the runtime's
/// `IFileWalkArg` layout byte-for-byte.
#[repr(C)]
#[derive(Copy, Clone)]
struct IFileWalkArg {
    has: u8,
    _pad: [u8; 7],
    value: i64,
}

extern "C" {
    fn mlc_open(
        path: *const c_char,
        kind: u8,
        errmsg: *mut *mut c_char,
    ) -> i64;
    fn mlc_ifile_walk(
        handle: i64,
        path: *const c_char,
        args_ptr: *const IFileWalkArg,
        n_args: u64,
        errmsg: *mut *mut c_char,
    ) -> *mut c_void;
    fn mlc_close(
        handle: i64,
        errmsg: *mut *mut c_char,
    ) -> i32;
    fn mlc_save_voidstar(
        data: *const c_void,
        schema: *const CSchema,
        level: u8,
        path: *const c_char,
        errmsg: *mut *mut c_char,
    ) -> i32;
    fn mlc_materialize_subpacket_from_bytes(
        bytes: *const u8,
        n: u64,
        elem_schema: *const CSchema,
        errmsg: *mut *mut c_char,
    ) -> *mut c_void;
    fn mlc_open_ostream(
        schema_str: *const c_char,
        path: *const c_char,
        errmsg: *mut *mut c_char,
    ) -> i64;
    fn mlc_write(
        level: u8,
        handle: i64,
        payload_voidstar: *const c_void,
        errmsg: *mut *mut c_char,
    ) -> i32;
    fn print_voidstar_jsonl(
        data: *const c_void,
        schema: *const CSchema,
        errmsg: *mut *mut c_char,
    ) -> i32;
    fn shfree(ptr: *mut c_void, errmsg: *mut *mut c_char) -> bool;
}

/// RAII guard for a caller-owned SHM voidstar. `Drop` calls `shfree`.
/// Use for voidstars whose ownership is ours to clean up on
/// normal-completion paths -- typically stdin sub-packet materialisations
/// where each iteration must free its own value before the next read.
///
/// Note: `process::clean_exit` calls `std::process::exit`, which skips
/// destructors. Explicit `drop(guard)` before `clean_exit` is required
/// on error paths that want the SHM released before termination.
struct ShmVoidstarGuard(*mut c_void);

impl ShmVoidstarGuard {
    fn new(p: *mut c_void) -> Self { Self(p) }
    fn ptr(&self) -> *mut c_void { self.0 }
}

impl Drop for ShmVoidstarGuard {
    fn drop(&mut self) {
        if !self.0.is_null() {
            unsafe {
                let mut errmsg: *mut c_char = ptr::null_mut();
                shfree(self.0, &mut errmsg);
            }
            self.0 = ptr::null_mut();
        }
    }
}

/// The user-visible packet-emission mode. Set by exactly one of the
/// mutex flags `-s`/`-d`/`-p`; `None` means no packet flag was passed
/// (the output form controls the emission format directly).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PacketMode {
    /// `-s`: emit MORLOC_STREAM_PACKET. Requires the emitted value
    /// (or the pattern result) to be a list.
    Stream,
    /// `-d`: emit MORLOC_DATA_PACKET.
    Data,
    /// `-p`: mirror the input packet's shape on output. Errors when
    /// the input is not a morloc packet.
    Preserve,
}

/// Resolved output selection: the effective format and, when set,
/// the packet-type override.
#[derive(Debug, Clone, Copy)]
pub struct OutputSelection {
    pub form: OutputForm,
    pub packet_mode: Option<PacketMode>,
}

/// Resolve the user's CLI flags into a concrete `OutputSelection`.
/// Applies the validation rules from the plan:
///  - At most one of `-s`/`-d`/`-p` (clap enforces this via ArgGroup).
///  - A packet flag with an explicit `-f <not packet>` -> error.
///  - A packet flag with no `-f` -> `-f packet`.
///  - No packet flag, no `-f` -> `-f json` (current default).
///  - Bare `-f packet` (no packet flag) -> DATA (no PacketMode set).
///
/// The `-p` on non-packet-input check needs the classification and
/// happens later in `run` after `classify_path`.
pub fn resolve_output_selection(args: &ViewArgs) -> Result<OutputSelection, String> {
    let packet_mode = if args.stream_packet {
        Some(PacketMode::Stream)
    } else if args.data_packet {
        Some(PacketMode::Data)
    } else if args.preserve_packet {
        Some(PacketMode::Preserve)
    } else {
        None
    };
    let form = match (args.output_form, packet_mode) {
        (Some(f), Some(_)) if f != OutputForm::Packet => {
            return Err(format!(
                "packet-type flag requires `-f packet` (or omit `-f`); got `-f {}`",
                output_form_name(f),
            ));
        }
        (Some(f), _) => f,
        (None, Some(_)) => OutputForm::Packet,
        (None, None) => OutputForm::Json,
    };
    Ok(OutputSelection { form, packet_mode })
}

/// Output formats that emit non-textual bytes and should not be
/// written to an interactive terminal without `--force`. Text formats
/// (`json`, `jsonl`, `csv`) are safe to display.
fn output_form_is_binary(f: OutputForm) -> bool {
    matches!(
        f,
        OutputForm::Mpk
            | OutputForm::Voidstar
            | OutputForm::Packet
            | OutputForm::Arrow
            | OutputForm::Parquet
    )
}

/// True iff stdout (fd 1) is an interactive terminal.
fn stdout_is_tty() -> bool {
    unsafe { libc::isatty(1) != 0 }
}

/// Short human-readable name for an `OutputForm`, matching the
/// clap ValueEnum surface (`json`, `mpk`, `packet`, ...).
fn output_form_name(f: OutputForm) -> &'static str {
    match f {
        OutputForm::Json => "json",
        OutputForm::Jsonl => "jsonl",
        OutputForm::Mpk => "mpk",
        OutputForm::Voidstar => "voidstar",
        OutputForm::Packet => "packet",
        OutputForm::Arrow => "arrow",
        OutputForm::Parquet => "parquet",
        OutputForm::Csv => "csv",
    }
}

extern "C" {
    fn get_morloc_data_packet_value(
        data: *const u8,
        schema: *const CSchema,
        errmsg: *mut *mut c_char,
    ) -> *mut u8;
}

/// Global list of temp files created by `drain_stdin_to_temp`. Cleaned
/// up in `cleanup_temp_files_atexit` which we register once via
/// `libc::atexit`. Only used from a single thread (view::run) so a
/// bare Mutex is sufficient.
static STDIN_TEMP_FILES: std::sync::Mutex<Vec<String>> =
    std::sync::Mutex::new(Vec::new());
static STDIN_ATEXIT_REGISTERED: std::sync::atomic::AtomicBool =
    std::sync::atomic::AtomicBool::new(false);

extern "C" fn cleanup_stdin_temp_files() {
    if let Ok(list) = STDIN_TEMP_FILES.lock() {
        for path in list.iter() {
            let _ = std::fs::remove_file(path);
        }
    }
}

fn ensure_atexit_registered() {
    use std::sync::atomic::Ordering;
    if STDIN_ATEXIT_REGISTERED
        .compare_exchange(false, true, Ordering::AcqRel, Ordering::Acquire)
        .is_ok()
    {
        unsafe {
            libc::atexit(cleanup_stdin_temp_files);
        }
    }
}

/// Dispatch `-` (stdin) input. Peeks the first 32 bytes to detect
/// packet type: MORLOC_STREAM_PACKET is streamed through the sub-
/// packet loop below (and the process exits from there); anything
/// else drains to a temp file so the file-path discipline the rest
/// of view is built around keeps working, and the returned path is
/// used by the caller. On drain we prepend the already-peeked bytes
/// to the file so the downstream classifier sees the true first
/// bytes.
fn handle_stdin_input(args: &ViewArgs, selection: &OutputSelection) -> String {
    use std::io::Read;

    let mut prefix = [0u8; 32];
    let mut stdin = std::io::stdin().lock();
    let mut got = 0usize;
    while got < 32 {
        match stdin.read(&mut prefix[got..]) {
            Ok(0) => break,
            Ok(n) => got += n,
            Err(e) => {
                eprintln!("Error: stdin read: {}", e);
                process::clean_exit(1);
            }
        }
    }
    // Short read (< 32 bytes) can't be any packet; drain what we have.
    if got < 32 {
        return match drain_stdin_to_temp_with_prefix(&prefix[..got], &mut stdin) {
            Ok(p) => p,
            Err(e) => {
                eprintln!("Error: draining stdin: {}", e);
                process::clean_exit(1);
            }
        };
    }
    let header = PacketHeader::from_bytes(&prefix).ok();
    let is_stream = header.as_ref().map(|h| h.is_stream()).unwrap_or(false);

    if is_stream && stdin_stream_should_dispatch(args, selection) {
        // Never returns; exits the process.
        stream_stdin_dispatch(args, selection, &prefix, &mut stdin);
    }
    // Fall back to temp-file drain.
    match drain_stdin_to_temp_with_prefix(&prefix, &mut stdin) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Error: draining stdin: {}", e);
            process::clean_exit(1);
        }
    }
}

/// True iff the streaming stdin path can handle the requested output
/// form and packet mode. `-f jsonl` and `-s` are the flagship
/// streaming outputs; other forms fall back to the temp-file path.
fn stdin_stream_should_dispatch(
    args: &ViewArgs,
    selection: &OutputSelection,
) -> bool {
    // `--pattern` on stdin goes through the buffered temp-file path so
    // the walker sees a seekable input. Streaming the pattern over an
    // uncached stream would need a walker variant that consumes
    // sub-packets in order without random access.
    if args.pattern.is_some() {
        return false;
    }
    match (selection.form, selection.packet_mode) {
        (OutputForm::Jsonl, None) => true,
        (OutputForm::Packet, Some(PacketMode::Stream)) => true,
        (OutputForm::Packet, Some(PacketMode::Preserve)) => true,
        _ => false,
    }
}

/// Streaming stdin dispatcher. Reads the stream header's metadata
/// block (element schema), then loops sub-packets until footer/EOF,
/// materialising each into a fresh SHM voidstar and handing it to the
/// appropriate emitter. Never returns.
fn stream_stdin_dispatch(
    args: &ViewArgs,
    selection: &OutputSelection,
    header_prefix: &[u8; 32],
    stdin: &mut std::io::StdinLock<'_>,
) -> ! {
    use std::io::Read;
    use morloc_runtime_types::packet::{
        iter_metadata, METADATA_TYPE_SCHEMA_STRING, decode_schema_entry,
    };

    let stream_hdr = match PacketHeader::from_bytes(header_prefix) {
        Ok(h) => h,
        Err(e) => {
            eprintln!("Error: stdin stream header invalid: {}", e);
            process::clean_exit(1);
        }
    };
    // Read the stream header's metadata block.
    let meta_len = { stream_hdr.offset } as usize;
    let mut meta = vec![0u8; meta_len];
    if let Err(e) = stdin.read_exact(&mut meta) {
        eprintln!("Error: stdin stream header metadata read: {}", e);
        process::clean_exit(1);
    }
    // Element schema string from the first SCHEMA_STRING entry.
    let mut elem_schema_str: Option<String> = None;
    for (kind, data) in iter_metadata(&meta) {
        if kind == METADATA_TYPE_SCHEMA_STRING {
            elem_schema_str = Some(decode_schema_entry(data));
            break;
        }
    }
    let elem_schema_str = match elem_schema_str {
        Some(s) => s,
        None => {
            eprintln!("Error: stdin stream missing SCHEMA_STRING metadata");
            process::clean_exit(1);
        }
    };
    let elem_schema = match parse_schema(&elem_schema_str) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error: stdin stream element schema '{}': {}",
                     elem_schema_str, e);
            process::clean_exit(1);
        }
    };
    let elem_c_schema = CSchema::from_rust(&elem_schema);
    // For jsonl emission, we need the value schema `[T]`, since the
    // voidstar we hand to `print_voidstar_jsonl` IS an Array.
    let value_schema_str = format!("a{}", elem_schema_str);
    let value_schema = match parse_schema(&value_schema_str) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error: stdin stream value schema '{}': {}",
                     value_schema_str, e);
            process::clean_exit(1);
        }
    };
    let value_c_schema = CSchema::from_rust(&value_schema);

    // For -s output, open the destination OStream. `-o FILE` is
    // required; enforced by dispatch_packet_conversion elsewhere but
    // repeated here since we bypass that path.
    let ostream_handle = if matches!(
        selection.packet_mode,
        Some(PacketMode::Stream) | Some(PacketMode::Preserve),
    ) {
        let out_path = match &args.output_path {
            Some(p) => p.clone(),
            None => {
                eprintln!(
                    "Error: -s on stdin requires -o FILE for the destination"
                );
                process::clean_exit(1);
            }
        };
        let out_c = match std::ffi::CString::new(out_path) {
            Ok(c) => c,
            Err(_) => {
                eprintln!("Error: output path contains NUL");
                process::clean_exit(1);
            }
        };
        let schema_c = std::ffi::CString::new(elem_schema_str.clone()).unwrap();
        let mut errmsg: *mut c_char = ptr::null_mut();
        let h = unsafe {
            mlc_open_ostream(schema_c.as_ptr(), out_c.as_ptr(), &mut errmsg)
        };
        if h < 0 {
            let msg = take_c_errmsg(errmsg)
                .unwrap_or_else(|| "unknown error".to_string());
            eprintln!("Error: opening output OStream: {}", msg);
            process::clean_exit(1);
        }
        Some(h)
    } else {
        None
    };

    // Sub-packet loop.
    let mut hdr_buf = [0u8; 32];
    loop {
        // Read the next sub-packet's 32-byte header.
        match read_exact_or_eof(stdin, &mut hdr_buf) {
            Ok(true) => {}
            Ok(false) => break, // clean EOF
            Err(e) => {
                eprintln!("Error: sub-packet header read: {}", e);
                process::clean_exit(1);
            }
        }
        let sub_hdr = match PacketHeader::from_bytes(&hdr_buf) {
            Ok(h) => h,
            Err(e) => {
                eprintln!("Error: sub-packet header invalid: {}", e);
                process::clean_exit(1);
            }
        };
        // Footer packet: end of stream. Consume its body + EOF tail
        // for cleanliness, but the exact bytes don't matter for us.
        if sub_hdr.is_footer() {
            break;
        }
        if !sub_hdr.is_data() {
            eprintln!(
                "Error: unexpected packet type in stream: cmd_type={}",
                unsafe { sub_hdr.command.cmd_type.cmd_type },
            );
            process::clean_exit(1);
        }
        let sub_meta_len = { sub_hdr.offset } as usize;
        let sub_payload_len = { sub_hdr.length } as usize;
        let sub_total = 32 + sub_meta_len + sub_payload_len;
        // Assemble the full sub-packet bytes for the materialiser.
        let mut full = Vec::with_capacity(sub_total);
        full.extend_from_slice(&hdr_buf);
        full.resize(sub_total, 0);
        if let Err(e) = stdin.read_exact(&mut full[32..]) {
            eprintln!("Error: sub-packet body read: {}", e);
            process::clean_exit(1);
        }

        // Materialise into an SHM Array<T> voidstar. The guard shfrees
        // on scope exit whether we succeed, hit an emit error before
        // exiting, or fall off the loop iteration.
        let mut errmsg: *mut c_char = ptr::null_mut();
        let voidstar = unsafe {
            mlc_materialize_subpacket_from_bytes(
                full.as_ptr(),
                full.len() as u64,
                elem_c_schema,
                &mut errmsg,
            )
        };
        if voidstar.is_null() {
            let msg = take_c_errmsg(errmsg)
                .unwrap_or_else(|| "unknown error".to_string());
            eprintln!("Error: materialise sub-packet: {}", msg);
            process::clean_exit(1);
        }
        let sub_vs = ShmVoidstarGuard::new(voidstar);

        // Emit per selected output form.
        match (selection.form, ostream_handle) {
            (OutputForm::Jsonl, None) => {
                let rc = unsafe {
                    print_voidstar_jsonl(sub_vs.ptr(), value_c_schema, &mut errmsg)
                };
                if rc != 0 {
                    let msg = take_c_errmsg(errmsg)
                        .unwrap_or_else(|| "unknown error".to_string());
                    drop(sub_vs);
                    eprintln!("Error: jsonl emit: {}", msg);
                    process::clean_exit(1);
                }
            }
            (OutputForm::Packet, Some(h)) => {
                let rc = unsafe {
                    mlc_write(args.compression_level, h, sub_vs.ptr(), &mut errmsg)
                };
                if rc != 0 {
                    let msg = take_c_errmsg(errmsg)
                        .unwrap_or_else(|| "unknown error".to_string());
                    drop(sub_vs);
                    eprintln!("Error: OStream write: {}", msg);
                    process::clean_exit(1);
                }
            }
            _ => unreachable!(
                "stream_stdin_dispatch reached with unsupported form"
            ),
        }
        // sub_vs drops here, freeing the sub-packet's voidstar before
        // we read the next.
    }

    // Close output if applicable.
    if let Some(h) = ostream_handle {
        let mut errmsg: *mut c_char = ptr::null_mut();
        let rc = unsafe { mlc_close(h, &mut errmsg) };
        if rc != 0 {
            let msg = take_c_errmsg(errmsg)
                .unwrap_or_else(|| "unknown error".to_string());
            eprintln!("Error: OStream close: {}", msg);
            process::clean_exit(1);
        }
    }

    process::clean_exit(0);
}

/// `read_exact` variant that returns `Ok(false)` on clean EOF (zero
/// bytes read on the first call). Any partial fill still fails.
fn read_exact_or_eof(
    r: &mut impl std::io::Read,
    buf: &mut [u8],
) -> std::io::Result<bool> {
    let mut filled = 0;
    while filled < buf.len() {
        let n = r.read(&mut buf[filled..])?;
        if n == 0 {
            if filled == 0 {
                return Ok(false);
            }
            return Err(std::io::Error::new(
                std::io::ErrorKind::UnexpectedEof,
                "short read on sub-packet header",
            ));
        }
        filled += n;
    }
    Ok(true)
}

/// Drain stdin into a temp file, prepending the already-peeked
/// `prefix` bytes so the downstream classifier sees the true first
/// bytes of stdin. Wraps `drain_stdin_to_temp` for the no-prefix case.
fn drain_stdin_to_temp_with_prefix(
    prefix: &[u8],
    stdin: &mut std::io::StdinLock<'_>,
) -> Result<String, String> {
    use std::io::Read;

    let tmpdir = std::env::var("TMPDIR").unwrap_or_else(|_| "/tmp".to_string());
    let template = format!("{}/morloc-nexus-view-XXXXXX", tmpdir);
    let mut buf: Vec<u8> = template.into_bytes();
    buf.push(0);
    let fd = unsafe { libc::mkstemp(buf.as_mut_ptr() as *mut c_char) };
    if fd < 0 {
        return Err(format!(
            "mkstemp failed: {}",
            std::io::Error::last_os_error(),
        ));
    }
    buf.pop();
    let path = match std::str::from_utf8(&buf) {
        Ok(s) => s.to_string(),
        Err(_) => {
            unsafe { libc::close(fd); }
            return Err("mkstemp produced a non-UTF-8 path".into());
        }
    };
    if let Ok(mut list) = STDIN_TEMP_FILES.lock() {
        list.push(path.clone());
    }
    ensure_atexit_registered();

    // Write prefix first, then drain remaining stdin.
    let mut cursor = 0usize;
    while cursor < prefix.len() {
        let w = unsafe {
            libc::write(fd, prefix.as_ptr().add(cursor) as *const _,
                       prefix.len() - cursor)
        };
        if w < 0 {
            let e = std::io::Error::last_os_error();
            unsafe { libc::close(fd); }
            let _ = std::fs::remove_file(&path);
            return Err(format!("prefix write failed: {}", e));
        }
        cursor += w as usize;
    }
    let mut chunk = [0u8; 64 * 1024];
    loop {
        let n = match stdin.read(&mut chunk) {
            Ok(0) => break,
            Ok(n) => n,
            Err(e) => {
                unsafe { libc::close(fd); }
                let _ = std::fs::remove_file(&path);
                return Err(format!("stdin read failed: {}", e));
            }
        };
        let mut written = 0;
        while written < n {
            let w = unsafe {
                libc::write(fd, chunk.as_ptr().add(written) as *const _,
                           n - written)
            };
            if w < 0 {
                let e = std::io::Error::last_os_error();
                unsafe { libc::close(fd); }
                let _ = std::fs::remove_file(&path);
                return Err(format!("temp file write failed: {}", e));
            }
            written += w as usize;
        }
    }
    unsafe {
        libc::fsync(fd);
        libc::close(fd);
    }
    Ok(path)
}

/// Entry point. Classifies the input file once (which doubles as the
/// truncation/oversize gate), resolves the schema, loads via the
/// shared loader, and hands a voidstar to [`print_result_c`].
pub fn run(args: &ViewArgs) -> ! {
    redirect_stdout_to(args.output_path.as_deref());

    let selection = match resolve_output_selection(args) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error: {}", e);
            process::clean_exit(1);
        }
    };

    // `-` reads the input from stdin. If the incoming bytes are a
    // MORLOC_STREAM_PACKET, iterate sub-packets on-the-fly so text-
    // streaming outputs (`-f jsonl`) and stream-shape packet outputs
    // (`-s`) run in constant memory. Otherwise drain to a temp file
    // and fall through to the file-path discipline the rest of view
    // is built around.
    let mut effective_args = args.clone();
    if args.target == "-" {
        effective_args.target = handle_stdin_input(args, &selection);
    }
    let args = &effective_args;

    let cls = classify_path(Path::new(&args.target), DEFAULT_PROBE_BYTES);
    if let Classification::Error(e) = &cls {
        eprintln!("Error: {}: {}", args.target, e);
        process::clean_exit(1);
    }

    // `-p` requires a morloc-packet input; nothing to preserve on a
    // raw JSON/CSV/etc file.
    if matches!(selection.packet_mode, Some(PacketMode::Preserve))
        && !matches!(
            cls,
            Classification::MorlocDataPacket { .. }
                | Classification::MorlocStreamPacket { .. }
        )
    {
        eprintln!(
            "Error: --preserve-packet requires a packet input; input is {}",
            classification_kind_name(&cls),
        );
        process::clean_exit(1);
    }

    // Binary-to-tty guardrail. Applies to the buffered path (`-f mpk`,
    // `-f voidstar`, `-f arrow`, `-f parquet`) when `-o FILE` is unset
    // and stdout is an interactive terminal. Packet outputs already
    // require `-o FILE` via `dispatch_packet_conversion`, so this
    // check only fires on the buffered arms. `--force` overrides.
    if args.output_path.is_none()
        && !args.force
        && output_form_is_binary(selection.form)
        && stdout_is_tty()
    {
        eprintln!(
            "Error: {}",
            crate::convert::refuse(
                crate::convert::GuardrailKind::BinaryToTty,
                "",
            )
        );
        process::clean_exit(1);
    }

    // Patterns route through the runtime IFile walker directly,
    // bypassing the buffered loader.
    if args.pattern.is_some() {
        dispatch_pattern(args, &cls, selection);
    }

    // Packet-mode dispatch. `-p` is rewritten in-place to `-s` or `-d`
    // by dispatch_packet_conversion based on input shape. Text-format
    // outputs fall through to the loader-based path below.
    if let Some(mode) = selection.packet_mode {
        dispatch_packet_conversion(args, &cls, mode);
    }

    // Size guardrail for the buffered path on stream inputs. The
    // loader materialises the entire stream into a voidstar; refuse
    // without `--force` when the projected uncompressed payload
    // exceeds the size threshold. `--force` also lifts this.
    if let Classification::MorlocStreamPacket { footer, .. } = &cls {
        if !args.force {
            let projected = crate::convert::projected_stream_uncompressed_bytes(
                footer,
                Path::new(&args.target),
            );
            let threshold = crate::convert::size_threshold_bytes();
            if projected > threshold {
                eprintln!(
                    "Error: {}",
                    crate::convert::refuse(
                        crate::convert::GuardrailKind::Size,
                        &format!(
                            "buffered `-f {}` on stream input would need ~{} bytes of RAM \
                             (projected uncompressed payload, threshold {} B). \
                             Consider `--pattern .[a:b]` for a subset, or a stream-shape \
                             output.",
                            output_form_name(selection.form),
                            projected,
                            threshold,
                        )
                    )
                );
                process::clean_exit(1);
            }
        }
    }

    let schema_str = match resolve_schema_str(args, &cls) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error: {}", e);
            process::clean_exit(1);
        }
    };

    let loaded = match load_input_as_voidstar(args, &cls, &schema_str) {
        Ok(l) => l,
        Err(e) => {
            eprintln!("Error: {}", e);
            process::clean_exit(1);
        }
    };

    let config = NexusConfig {
        output_format: selection.form.to_internal(),
        compression_level: args.compression_level,
        output_path: args.output_path.clone(),
        ..NexusConfig::default()
    };

    let packet_slice: &[u8] = loaded
        .packet_bytes
        .as_ref()
        .map(|p| p.as_slice())
        .unwrap_or(&[]);
    print_result_c(
        loaded.voidstar,
        loaded.c_schema,
        packet_slice,
        loaded.is_arrow,
        &config,
    );
    unreachable!("print_result_c always exits via process::clean_exit")
}

/// Apply `args.pattern` to the input file and emit the result.
/// Never returns; always exits via `process::clean_exit`.
///
/// Flow:
///   1. Parse the pattern string (Rust `parse_pattern`).
///   2. Resolve the input schema from `--schema` or embedded metadata.
///   3. Type-check the pattern against the input schema; error out
///      with the walker's specific error kind (`ExpectedRecord`,
///      `ExpectedIndexable`, etc.) if it does not match.
///   4. Open the input as an IFile via `mlc_open`; call
///      `mlc_ifile_walk` with the placeholder-form path + encoded
///      args; get back a voidstar of the result.
///   5. Dispatch on output form. Text formats and DATA packet output
///      go through `print_result_c` with a freshly-built CSchema
///      matching the pattern's result schema. STREAM packet output
///      (`-s`) rejects here; converting the pattern's voidstar back
///      to a stream would need a temp DATA packet round-trip.
fn dispatch_pattern(
    args: &ViewArgs,
    cls: &Classification,
    selection: OutputSelection,
) -> ! {
    let pattern_str = args.pattern.as_deref().expect("pattern is set");

    // Parse.
    let path = match parse_pattern(pattern_str) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Error: pattern parse: {}", e);
            process::clean_exit(1);
        }
    };

    // Resolve input schema string.
    let input_schema_str = match resolve_schema_str(args, cls) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error: {}", e);
            process::clean_exit(1);
        }
    };
    let input_schema = match parse_schema(&input_schema_str) {
        Ok(s) => s,
        Err(e) => {
            eprintln!(
                "Error: failed to parse input schema '{}': {}",
                input_schema_str, e,
            );
            process::clean_exit(1);
        }
    };

    // Type-check against the input schema.
    let result_schema = match check_pattern_against_schema(&path, &input_schema) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error: {}", e);
            process::clean_exit(1);
        }
    };

    // Pattern + `-s` would need a voidstar -> stream writer that
    // doesn't exist; the pattern result is a single voidstar and
    // print_result_c can only emit it as DATA or text.
    if matches!(selection.packet_mode, Some(PacketMode::Stream)) {
        eprintln!(
            "Error: --pattern with -s is unsupported. Use -d for a \
             data-packet result, or omit -s/-d/-p for text output."
        );
        process::clean_exit(2);
    }

    // Non-packet inputs can't be walked by `mlc_ifile_walk`; the
    // walker only accepts a real morloc packet on disk.
    if !matches!(
        cls,
        Classification::MorlocDataPacket { .. }
            | Classification::MorlocStreamPacket { .. }
    ) {
        eprintln!(
            "Error: --pattern requires a morloc-packet input; \
             got {}",
            classification_kind_name(cls),
        );
        process::clean_exit(1);
    }

    // Encode the pattern for the runtime walker.
    let (placeholder_path, encoded_args) = path_to_walker_input(&path);
    let arg_vec: Vec<IFileWalkArg> = encoded_args
        .iter()
        .map(|EncodedArg { has, value }| IFileWalkArg {
            has: *has,
            _pad: [0u8; 7],
            value: *value,
        })
        .collect();

    // `mlc_open` refuses footer-less streams. Recovering them here
    // would need `mlc_open_ifile_recovered` with forward-scan output.
    let loaded = match walk_ifile_to_loaded(
        &args.target,
        &placeholder_path,
        &arg_vec,
        &result_schema,
    ) {
        Ok(l) => l,
        Err(e) => {
            eprintln!("Error: {}", e);
            process::clean_exit(1);
        }
    };

    redirect_stdout_to(args.output_path.as_deref());

    let config = NexusConfig {
        output_format: selection.form.to_internal(),
        compression_level: args.compression_level,
        output_path: args.output_path.clone(),
        ..NexusConfig::default()
    };
    print_result_c(
        loaded.voidstar,
        loaded.c_schema,
        &[],
        false,
        &config,
    );
    unreachable!("print_result_c always exits via process::clean_exit")
}

/// Load the input file into a `(voidstar, c_schema, packet_bytes,
/// is_arrow)` bundle suitable for `print_result_c`.
///
/// For DATA-packet and non-packet inputs this delegates to the shared
/// `load_with_schema` chain (C `parse_cli_data_argument`) that
/// existing `morloc-nexus run` uses.
///
/// For STREAM-packet inputs the C loader refuses ("Expected data
/// packet"), so we instead open the stream as an IFile and materialise
/// the whole list via a `.[:]` walk. The returned schema is the
/// value-level `[T]`, matching what the emitter chain expects. Peak
/// memory is one voidstar of the whole list; the `--force` size gate
/// is applied by the caller before this function runs.
struct LoadedInput {
    voidstar: *mut u8,
    c_schema: *mut CSchema,
    /// Original packet bytes when the input came through the C loader
    /// (needed for arrow-format detection). Empty when the input was
    /// loaded via the IFile walker.
    packet_bytes: Option<crate::loader::LoadedPacket>,
    is_arrow: bool,
}

/// Open `target` as an IFile, walk it with the given placeholder path
/// + args, and return a `LoadedInput` carrying the resulting voidstar
/// and a CSchema for `result_schema`. Used for both the identity-slice
/// stream loader and the general `--pattern` walker.
fn walk_ifile_to_loaded(
    target: &str,
    placeholder_path: &str,
    walker_args: &[IFileWalkArg],
    result_schema: &Schema,
) -> Result<LoadedInput, String> {
    let target_c = CString::new(target.to_string())
        .map_err(|_| "target path contains NUL".to_string())?;
    let path_c = CString::new(placeholder_path.to_string())
        .map_err(|_| "walker path contains NUL".to_string())?;
    let mut errmsg: *mut c_char = ptr::null_mut();
    let handle = unsafe { mlc_open(target_c.as_ptr(), MLC_KIND_IFILE, &mut errmsg) };
    if handle < 0 {
        let msg = take_c_errmsg(errmsg)
            .unwrap_or_else(|| "unknown error".to_string());
        return Err(format!("failed to open '{}' as IFile: {}", target, msg));
    }
    let voidstar = unsafe {
        mlc_ifile_walk(
            handle,
            path_c.as_ptr(),
            walker_args.as_ptr(),
            walker_args.len() as u64,
            &mut errmsg,
        )
    };
    if voidstar.is_null() {
        let msg = take_c_errmsg(errmsg)
            .unwrap_or_else(|| "unknown error".to_string());
        let _ = unsafe { mlc_close(handle, &mut errmsg) };
        return Err(format!("pattern walk on '{}' failed: {}", target, msg));
    }
    let c_schema = CSchema::from_rust(result_schema);
    Ok(LoadedInput {
        voidstar: voidstar as *mut u8,
        c_schema,
        packet_bytes: None,
        is_arrow: false,
    })
}

/// Identity-slice walker args: three `has=0` placeholders for the
/// `.[:]` pattern's (start, stop, step) slots.
fn identity_slice_args() -> [IFileWalkArg; 3] {
    [
        IFileWalkArg { has: 0, _pad: [0u8; 7], value: 0 },
        IFileWalkArg { has: 0, _pad: [0u8; 7], value: 0 },
        IFileWalkArg { has: 0, _pad: [0u8; 7], value: 0 },
    ]
}

fn load_input_as_voidstar(
    args: &ViewArgs,
    cls: &Classification,
    schema_str: &str,
) -> Result<LoadedInput, String> {
    if matches!(cls, Classification::MorlocStreamPacket { .. }) {
        let parsed = parse_schema(schema_str).map_err(|e| {
            format!("failed to parse schema '{}': {}", schema_str, e)
        })?;
        walk_ifile_to_loaded(
            &args.target,
            ".[:]",
            &identity_slice_args(),
            &parsed,
        )
    } else {
        let loaded = load_with_schema(&args.target, schema_str)?;
        let mut errmsg: *mut c_char = ptr::null_mut();
        let voidstar = unsafe {
            get_morloc_data_packet_value(
                loaded.packet, loaded.c_schema, &mut errmsg,
            )
        };
        if voidstar.is_null() {
            let msg = take_c_errmsg(errmsg)
                .unwrap_or_else(|| "unknown error".to_string());
            return Err(format!(
                "failed to extract value from '{}': {}", args.target, msg
            ));
        }
        let is_arrow = packet_format_is_arrow(loaded.as_slice());
        // Keep loaded alive by moving it into the returned struct;
        // its Drop frees the packet + CSchema when the caller is done.
        let c_schema = loaded.c_schema;
        Ok(LoadedInput {
            voidstar,
            c_schema,
            packet_bytes: Some(loaded),
            is_arrow,
        })
    }
}

/// Route the four packet-conversion arms. Always exits via
/// `process::clean_exit`.
fn dispatch_packet_conversion(
    args: &ViewArgs,
    cls: &Classification,
    mode: PacketMode,
) -> ! {
    use crate::convert;
    use crate::file::FooterInfo;

    // The runtime FFI writes packets by mmap + pwrite + rename, so
    // packet output needs a real path.
    let out_path = match &args.output_path {
        Some(p) => Path::new(p).to_owned(),
        None => {
            eprintln!("Error: packet output requires -o FILE");
            process::clean_exit(1);
        }
    };
    let in_path = Path::new(&args.target).to_owned();

    // Resolve `-p` (preserve) into a concrete stream/data mode from
    // the input classification.
    let effective_mode = match mode {
        PacketMode::Preserve => {
            if convert::is_morloc_stream_packet(cls) {
                PacketMode::Stream
            } else if convert::is_morloc_data_packet(cls) {
                PacketMode::Data
            } else {
                eprintln!(
                    "Error: --preserve-packet requires a packet input"
                );
                process::clean_exit(1);
            }
        }
        other => other,
    };

    // Dispatch by (input kind, output mode) pair.
    let result: Result<String, String> = match (
        convert::is_morloc_data_packet(cls),
        convert::is_morloc_stream_packet(cls),
        effective_mode,
    ) {
        (true, _, PacketMode::Data) => {
            // DATA -> DATA: byte-copy.
            convert::data_to_data_copy(&in_path, &out_path)
                .map(|_| "copied data-packet".to_string())
        }
        (true, _, PacketMode::Stream) => {
            // DATA -> STREAM. Guardrail: the IFile inline region for
            // huge DATA files is materialised into RAM; refuse
            // without --force when the file exceeds the buffer
            // threshold.
            let working_set = convert::projected_data_working_set_bytes(&in_path);
            let threshold = convert::size_threshold_bytes();
            if !args.force && working_set > threshold {
                eprintln!(
                    "Error: {}",
                    convert::refuse(
                        convert::GuardrailKind::Size,
                        &format!(
                            "DATA -> STREAM conversion would keep ~{} bytes resident \
                             (input '{}' is {} B, threshold {} B)",
                            working_set,
                            in_path.display(),
                            working_set,
                            threshold,
                        )
                    )
                );
                process::clean_exit(1);
            }
            convert::data_to_stream(&in_path, &out_path, args.compression_level)
                .map(|n| format!("wrote {} sub-packet(s)", n))
        }
        (_, true, PacketMode::Stream) => {
            // STREAM -> STREAM.
            let footer = convert::stream_footer(cls);
            // Refuse footer-less input for the byte-verbatim fast
            // path? For now, the runtime's IStream forward-walks
            // regardless of footer state, so no additional check
            // is needed here.
            let _ = footer;
            convert::stream_to_stream(
                &in_path,
                &out_path,
                args.compression_level,
                args.schema.as_deref(),
            )
            .map(|n| format!("wrote {} sub-packet(s)", n))
        }
        (_, true, PacketMode::Data) => {
            // STREAM -> DATA. Existing loader materialises the value
            // in memory, then --output-file writes a DATA packet.
            // Guardrail: refuse without --force when the projected
            // uncompressed payload exceeds the threshold.
            let fallback_footer = FooterInfo::Missing { walk: None };
            let footer = convert::stream_footer(cls).unwrap_or(&fallback_footer);
            let projected = convert::projected_stream_uncompressed_bytes(footer, &in_path);
            let threshold = convert::size_threshold_bytes();
            if !args.force && projected > threshold {
                eprintln!(
                    "Error: {}",
                    convert::refuse(
                        convert::GuardrailKind::Size,
                        &format!(
                            "STREAM -> DATA materialisation would need ~{} bytes of RAM \
                             (projected uncompressed payload, threshold {} B). \
                             Consider `-s` (stream-preserving) or `@open IFile`.",
                            projected, threshold,
                        )
                    )
                );
                process::clean_exit(1);
            }
            // print_result_c inside the helper exits the process;
            // callers of stream_to_data_via_walker do not return.
            stream_to_data_via_walker(args, cls, &in_path, &out_path);
        }
        (false, false, _) => {
            eprintln!(
                "Error: packet-type output requires a morloc packet input; input is {}",
                classification_kind_name(cls),
            );
            process::clean_exit(1);
        }
        // The Preserve arm was resolved above.
        (_, _, PacketMode::Preserve) => unreachable!(),
    };

    match result {
        Ok(msg) => {
            eprintln!(
                "morloc-nexus view: {} ({})",
                out_path.display(),
                msg,
            );
            process::clean_exit(0);
        }
        Err(e) => {
            eprintln!("Error: {}", e);
            process::clean_exit(1);
        }
    }
}

/// STREAM -> DATA via the IFile walker. Opens the stream as an IFile,
/// materialises the whole list with `.[:]`, and hands the voidstar to
/// `print_result_c` in `OutputFormat::Packet` mode. Writes directly to
/// `out_path` (no `.partial` scratch): the buffered path is already
/// gated by the `--force` size threshold, so a partial write is
/// bounded and retryable.
fn stream_to_data_via_walker(
    args: &ViewArgs,
    cls: &Classification,
    _in_path: &Path,
    out_path: &Path,
) -> ! {
    let schema_str = match resolve_schema_str(args, cls) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error: {}", e);
            process::clean_exit(1);
        }
    };
    let loaded = match load_input_as_voidstar(args, cls, &schema_str) {
        Ok(l) => l,
        Err(e) => {
            eprintln!("Error: {}", e);
            process::clean_exit(1);
        }
    };
    // Use `mlc_save_voidstar` to serialise the walker result as a
    // MORLOC_DATA_PACKET file. The dispatch.rs `Packet` output arm
    // expects the ORIGINAL packet bytes (`full_packet`), which we
    // don't have when the input came through the IFile walker; going
    // through the save FFI directly avoids that entire code path.
    let out_c = match process::path_to_cstring(out_path) {
        Ok(c) => c,
        Err(msg) => {
            eprintln!("Error: {}", msg);
            process::clean_exit(1);
        }
    };
    let mut errmsg: *mut c_char = ptr::null_mut();
    let rc = unsafe {
        mlc_save_voidstar(
            loaded.voidstar as *const c_void,
            loaded.c_schema,
            args.compression_level,
            out_c.as_ptr(),
            &mut errmsg,
        )
    };
    if rc != 0 {
        let msg = take_c_errmsg(errmsg)
            .unwrap_or_else(|| "unknown error".to_string());
        eprintln!(
            "Error: failed to write data packet to '{}': {}",
            out_path.display(), msg,
        );
        process::clean_exit(1);
    }
    // Keep `loaded` alive until this point so its LoadedPacket Drop
    // doesn't free the CSchema/packet bytes we just handed to the
    // FFI. When packet_bytes is None (IFile-walker path), the
    // voidstar/CSchema are ours to keep -- eval_arena frees them on
    // process exit.
    drop(loaded);
    process::clean_exit(0);
}

/// Short human-readable label for a `Classification` used in
/// user-facing error messages.
fn classification_kind_name(cls: &Classification) -> &'static str {
    match cls {
        Classification::MorlocDataPacket { .. } => "morloc data-packet",
        Classification::MorlocCallPacket { .. } => "morloc call-packet",
        Classification::MorlocStreamPacket { .. } => "morloc stream-packet",
        Classification::MorlocPingPacket => "morloc ping-packet",
        Classification::Json => "json",
        Classification::MessagePack => "msgpack",
        Classification::Csv { .. } => "csv",
        Classification::Arrow => "arrow",
        Classification::Parquet => "parquet",
        Classification::Text { .. } => "text",
        Classification::Empty => "empty",
        Classification::Unknown => "raw bytes",
        Classification::Directory => "directory",
        Classification::Error(_) => "error",
    }
}

/// Resolve the schema string view will use to decode the input. Order:
///  1. `--schema` flag.
///  2. Schema embedded in a morloc-packet's metadata, already
///     extracted by [`classify_path`] in `run`.
///  3. Error directing the user to `--schema`.
///
/// View does not implement schema-less passthrough — every conversion
/// goes through the typed loader so behavior matches `run`. Users who
/// want a byte-identity copy already have `cp`.
fn resolve_schema_str(args: &ViewArgs, cls: &Classification) -> Result<String, String> {
    if let Some(s) = &args.schema {
        parse_schema(s).map_err(|e| format!("failed to parse --schema '{}': {}", s, e))?;
        return Ok(s.clone());
    }
    if let Classification::MorlocDataPacket {
        arg: DataArg { schema: Some(s), .. },
        ..
    } = cls
    {
        return Ok(s.clone());
    }
    // Stream packets store the ELEMENT schema in metadata. The value
    // schema at the view/loader level is `[T]`, so wrap with the
    // Array marker (`a`) that the schema parser understands.
    if let Classification::MorlocStreamPacket { schema: Some(s), .. } = cls {
        return Ok(format!("a{}", s));
    }
    Err(format!(
        "no schema available for '{}'. \
         Inputs without a morloc-packet header need an explicit --schema STRING.",
        args.target
    ))
}

/// True iff the loaded packet's DATA-command format byte is ARROW.
/// Used by `print_result_c` to route to the table-only output path.
fn packet_format_is_arrow(packet: &[u8]) -> bool {
    if packet.len() < 32 {
        return false;
    }
    let mut hdr_bytes = [0u8; 32];
    hdr_bytes.copy_from_slice(&packet[..32]);
    let hdr = match PacketHeader::from_bytes(&hdr_bytes) {
        Ok(h) => h,
        Err(_) => return false,
    };
    hdr.is_data() && unsafe { hdr.command.data.format } == PACKET_FORMAT_ARROW
}

#[cfg(test)]
mod tests {
    use super::*;

    fn default_view_args() -> ViewArgs {
        ViewArgs {
            target: String::new(),
            output_form: None,
            compression_level: 0,
            output_path: None,
            schema: None,
            stream_packet: false,
            data_packet: false,
            preserve_packet: false,
            force: false,
            pattern: None,
        }
    }

    #[test]
    fn no_flags_defaults_to_json() {
        let args = default_view_args();
        let sel = resolve_output_selection(&args).unwrap();
        assert_eq!(sel.form, OutputForm::Json);
        assert!(sel.packet_mode.is_none());
    }

    #[test]
    fn bare_stream_flag_implies_packet_form() {
        let mut args = default_view_args();
        args.stream_packet = true;
        let sel = resolve_output_selection(&args).unwrap();
        assert_eq!(sel.form, OutputForm::Packet);
        assert_eq!(sel.packet_mode, Some(PacketMode::Stream));
    }

    #[test]
    fn bare_data_flag_implies_packet_form() {
        let mut args = default_view_args();
        args.data_packet = true;
        let sel = resolve_output_selection(&args).unwrap();
        assert_eq!(sel.form, OutputForm::Packet);
        assert_eq!(sel.packet_mode, Some(PacketMode::Data));
    }

    #[test]
    fn bare_preserve_flag_implies_packet_form() {
        let mut args = default_view_args();
        args.preserve_packet = true;
        let sel = resolve_output_selection(&args).unwrap();
        assert_eq!(sel.form, OutputForm::Packet);
        assert_eq!(sel.packet_mode, Some(PacketMode::Preserve));
    }

    #[test]
    fn packet_flag_with_non_packet_form_errors() {
        let mut args = default_view_args();
        args.stream_packet = true;
        args.output_form = Some(OutputForm::Json);
        let err = resolve_output_selection(&args).unwrap_err();
        assert!(
            err.contains("packet-type flag requires"),
            "unexpected error: {}",
            err
        );
    }

    #[test]
    fn packet_flag_with_explicit_packet_form_is_ok() {
        let mut args = default_view_args();
        args.stream_packet = true;
        args.output_form = Some(OutputForm::Packet);
        let sel = resolve_output_selection(&args).unwrap();
        assert_eq!(sel.form, OutputForm::Packet);
        assert_eq!(sel.packet_mode, Some(PacketMode::Stream));
    }

    #[test]
    fn bare_output_form_is_respected() {
        let mut args = default_view_args();
        args.output_form = Some(OutputForm::Csv);
        let sel = resolve_output_selection(&args).unwrap();
        assert_eq!(sel.form, OutputForm::Csv);
        assert!(sel.packet_mode.is_none());
    }
}
