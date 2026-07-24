//! Command dispatch: routing parsed CLI args to language pools.
//!
//! Argument parsing lives in [`crate::cli`] (top-level mode + always-
//! on flags) and [`crate::phase2`] (manifest-driven per-program flags
//! + capability gates). This module owns the execution backend: take
//! a fully-parsed [`Vec<ArgValue>`], optionally spawn pool daemons,
//! and either dispatch the call packet over a Unix socket (remote
//! command) or evaluate the embedded NexusExpr tree in-process (pure
//! command).
//!
//! Links against `libmorloc.so` for packet construction (call-packet
//! builder, schema parser, pool send/receive) and for evaluating
//! pure commands via `morloc_eval`.

use crate::manifest::{Arg, Check, Command, FormAtom, Manifest, SourceAtom};
use crate::process::{self, PoolSocket};

/// Output format enum.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OutputFormat {
    Json,
    /// JSON-lines: one element per line. For list-shaped inputs the
    /// list is streamed element-by-element; on scalars a single line
    /// is emitted. Reachable from `morloc-nexus view -f jsonl`.
    Jsonl,
    MessagePack,
    VoidStar,
    Packet,
    /// Apache Arrow IPC file format (file with ARROW1 magic). Only valid
    /// for commands whose return type is a Table.
    Arrow,
    /// Apache Parquet file. Only valid for commands whose return type is a
    /// Table.
    Parquet,
    /// CSV (comma-separated). Header row from the column schema. Only
    /// valid for commands whose return type is a Table.
    Csv,
    /// Verbatim bytes: `Str`/`[Str]` element bodies emitted with no framing
    /// or JSON quoting. The default output format for `render` terminals.
    Raw,
}

/// Nexus configuration parsed from CLI options.
#[derive(Debug, Clone)]
pub struct NexusConfig {
    pub print_flag: bool,
    pub keep_null: bool,
    pub packet_path: Option<String>,
    pub socket_base: Option<String>,
    pub output_path: Option<String>,
    pub output_format: OutputFormat,
    pub daemon_flag: bool,
    pub router_flag: bool,
    pub unix_socket_path: Option<String>,
    pub tcp_port: Option<i32>,
    pub http_port: Option<i32>,
    pub port_file_path: Option<String>,
    pub fdb_path: Option<String>,
    pub eval_timeout: i32,
    /// Base directory under which a per-run subdir (named by run_id)
    /// is materialized. Activates rundir creation, log tee, and
    /// `summary.json`. Falls back to the `MORLOC_LOG_DIR` env var.
    /// `None` => no rundir, no tee, no default summary.
    pub log_dir: Option<String>,
    /// Explicit `summary.json` output path. When set the summary file
    /// goes here regardless of whether `log_dir` is set. Falls back
    /// to the `MORLOC_SUMMARY` env var.
    pub summary_path: Option<String>,
    /// When true, suppress every nexus-and-pool-emitted log line
    /// (prologue, epilogue, per-label start/done). Drives the
    /// `MORLOC_QUIET` env var that the runtime's log emitter checks.
    pub quiet: bool,
    /// Cap on disk writes per dispatch when `--debug` is compiled in.
    /// `None` = use the runtime default (1). 0 = no cap.
    pub debug_cache_depth: Option<u64>,
    /// Per-arg msgpack-byte cap. Args exceeding the cap record their
    /// hash but skip the disk write. `None` = unlimited.
    pub debug_cache_max: Option<u64>,
    /// Per-midx cap on catch fires before further entries are dropped
    /// for that midx. `None` = use the runtime default (3). 0 = no cap.
    pub debug_recursion_cap: Option<u32>,
    /// zstd compression preset for packets written to --output-file
    /// (output-form packet). 0 = no compression.
    pub compression_level: u8,
}

impl Default for NexusConfig {
    fn default() -> Self {
        NexusConfig {
            print_flag: false,
            keep_null: false,
            packet_path: None,
            socket_base: None,
            output_path: None,
            output_format: OutputFormat::Json,
            daemon_flag: false,
            router_flag: false,
            unix_socket_path: None,
            tcp_port: None,
            http_port: None,
            port_file_path: None,
            fdb_path: None,
            eval_timeout: 30,
            log_dir: None,
            summary_path: None,
            quiet: false,
            debug_cache_depth: None,
            debug_cache_max: None,
            debug_recursion_cap: None,
            compression_level: 0,
        }
    }
}

/// Collapse runs of adjacent identical lines in an error message.
///
/// When an error bubbles up through deep recursion across pool boundaries,
/// each layer may prepend the same wrapper string to the inner cause. The
/// result is N copies of the same line burying the root cause. This pass
/// keeps the first occurrence of each adjacent duplicate and drops the rest.
/// Non-adjacent duplicates are preserved (they may be legitimate repeats).
fn collapse_duplicate_lines(msg: &str) -> String {
    let mut out: Vec<&str> = Vec::new();
    for line in msg.split_inclusive('\n') {
        if out.last().map_or(false, |prev| *prev == line) {
            continue;
        }
        out.push(line);
    }
    out.concat()
}

const STDOUT_BUF_CAPACITY: usize = 64 * 1024;

/// Hex dump: 16 lowercase 2-digit bytes per row, single space between bytes.
fn write_hex_dump(bytes: &[u8]) -> std::io::Result<()> {
    use std::io::Write;
    let mut w = std::io::BufWriter::with_capacity(STDOUT_BUF_CAPACITY, std::io::stdout().lock());
    for (i, b) in bytes.iter().enumerate() {
        if i > 0 && i % 16 == 0 { w.write_all(b"\n")?; }
        write!(w, "{:02x} ", b)?;
    }
    if !bytes.is_empty() { w.write_all(b"\n")?; }
    w.flush()
}

/// Grouped hex dump: 2-digit uppercase, 4-byte groups joined by single
/// space, six groups per line.
fn write_hex_dump_grouped(bytes: &[u8]) -> std::io::Result<()> {
    use std::io::Write;
    let mut w = std::io::BufWriter::with_capacity(STDOUT_BUF_CAPACITY, std::io::stdout().lock());
    for (i, b) in bytes.iter().enumerate() {
        if i > 0 && i % 4 == 0 {
            w.write_all(if i % 24 == 0 { b"\n" } else { b" " })?;
        }
        write!(w, "{:02X}", b)?;
    }
    if !bytes.is_empty() { w.write_all(b"\n")?; }
    w.flush()
}

/// Emit a uniform error when pool communication fails, then exit.
///
/// The pool's stderr was inherited by the nexus, so any traceback the pool
/// printed before dying is already on the user's terminal. This helper
/// reports the communication error plus the pool's exit status (if it has
/// been reaped) so the user can correlate the two.
///
/// Race condition: the pool process may still be writing its error output
/// (traceback, panic message, etc.) to stderr when the nexus detects the
/// broken connection. If we call clean_exit immediately, it sends SIGTERM
/// to the pool process group, which can kill the pool before its stderr
/// buffer is flushed. We insert a brief drain window to let any in-flight
/// stderr from the dying pool reach the terminal before tearing everything
/// down. This is best-effort: a pool killed by SIGKILL (OOM killer, etc.)
/// won't have pending output, and a pool stuck in a blocking syscall won't
/// flush within the window. But for the common case of a Python exception
/// traceback, this is enough.
fn die_with_pool_error(
    socket: &PoolSocket,
    pool_index: usize,
    context: &str,
    comm_err: &std::io::Error,
) -> ! {
    // Give the dying pool process time to flush its stderr/stdout before
    // we tear down the process group. Without this, a Python traceback or
    // error message that is still in a pipe buffer gets lost when
    // clean_exit sends SIGTERM/SIGKILL to the pool's process group.
    std::thread::sleep(std::time::Duration::from_millis(100));

    // Translate opaque kernel-level IO failures into actionable text.
    // `read_exact` returns UnexpectedEof with the std-lib message "failed
    // to fill whole buffer" whenever the peer closes the socket -- which,
    // in our case, is exactly what happens when the pool crashes. Most
    // users have never seen that phrase and cannot act on it. BrokenPipe
    // is the same situation on the write side.
    let comm_msg: String = match comm_err.kind() {
        std::io::ErrorKind::UnexpectedEof | std::io::ErrorKind::BrokenPipe => format!(
            "pool '{}' closed the connection before sending a response \
             (likely crashed; see stderr above for any traceback)",
            socket.lang
        ),
        _ => format!("{}", comm_err),
    };

    let mut full = format!("{}: {}", context, comm_msg);
    if let Some(info) = process::pool_death_info(pool_index) {
        full.push_str(&format!("\nPool '{}' {}", socket.lang, info));
    }
    crate::runlog::die_with_error(&full);
}


/// Wrap a string in JSON quotes (for literal string arguments).
pub fn quoted(s: &str) -> String {
    // JSON-escape the string
    let escaped = serde_json::to_string(s).unwrap_or_else(|_| format!("\"{}\"", s));
    escaped
}

/// Build the per-element argument_t's for a `many: true` slot and
/// invoke the C-side list assembler. `literal == true` JSON-quotes
/// each token so the per-element classifier treats it as inline
/// content rather than a file path. Holds the CStrings in a local
/// `keepalive` vec for the duration of the call (the C side only
/// strdup's its inputs).
unsafe fn assemble_many_packet(
    tokens: &[String],
    literal: bool,
    c_schema: *const morloc_runtime_types::cschema::CSchema,
    errmsg: *mut *mut std::ffi::c_char,
    initialize_positional: unsafe extern "C" fn(*mut std::ffi::c_char) -> *mut std::ffi::c_void,
    free_argument_t: unsafe extern "C" fn(*mut std::ffi::c_void),
    parse_cli_data_argument_list: unsafe extern "C" fn(
        *mut u8,
        *const *const std::ffi::c_void,
        usize,
        *const morloc_runtime_types::cschema::CSchema,
        *mut *mut std::ffi::c_char,
    ) -> *mut u8,
) -> *mut u8 {
    let keepalive: Vec<std::ffi::CString> = tokens.iter()
        .map(|tok| {
            let s = if literal { quoted(tok) } else { tok.clone() };
            std::ffi::CString::new(s).unwrap()
        })
        .collect();
    let c_args: Vec<*mut std::ffi::c_void> = keepalive.iter()
        .map(|c_str| initialize_positional(c_str.as_ptr() as *mut std::ffi::c_char))
        .collect();
    let pkt = parse_cli_data_argument_list(
        std::ptr::null_mut(),
        c_args.as_ptr() as *const *const std::ffi::c_void,
        c_args.len(),
        c_schema,
        errmsg,
    );
    for a in &c_args { free_argument_t(*a); }
    drop(keepalive);
    pkt
}

/// Run pre-parsed args through dispatch. Separated from any
/// particular parser frontend so the daemon-spawn + NUL-check +
/// run_remote/run_pure backend stays parser-agnostic.
pub fn dispatch_command_parsed(
    parsed_args: Vec<ArgValue>,
    config: &NexusConfig,
    manifest: &Manifest,
    cmd: &Command,
    sockets: &mut [PoolSocket],
) {
    // Bind `{name}` for the run-scope log before emitting the
    // prologue; clean_exit fires the matching epilogue at process
    // exit.
    crate::runlog::record_command(&cmd.name);
    crate::runlog::emit_prologue();

    // Start daemons for remote commands
    if !cmd.is_pure() {
        if let Err(e) = process::start_daemons(sockets, &cmd.needed_pools) {
            eprintln!("Error: {}", e);
            process::clean_exit(1);
        }
    }

    // Execute the command
    if cmd.is_pure() {
        run_pure_command(cmd, &parsed_args, config);
    } else {
        // NUL-in-Str guard. If the target pool's language opts out of
        // interior-NUL strings (e.g. R), scan every parsed arg for an
        // embedded NUL and fail fast with a clean morloc-level
        // diagnostic before invoking the pool. Bypassed when the
        // program-level --unsafe-skip-null-check was set or when the
        // env var MORLOC_SKIP_NULL_CHECK=1 is in effect.
        let target_pool = &manifest.pools[cmd.pool_index];
        let skip = manifest.unsafe_skip_null_check
            || morloc_runtime_types::null_check::env_skip_null_check();
        if !skip && !target_pool.allow_string_null {
            for (i, av) in parsed_args.iter().enumerate() {
                let payload = match av {
                    ArgValue::Value(s) => Some(s.as_str()),
                    _ => None,
                };
                if let Some(s) = payload {
                    // The CLI ArgValue contains JSON-quoted form for
                    // strings (e.g. `"abc def"`). Decode via
                    // serde_json so any   escapes become real NUL
                    // bytes; non-JSON values just fail to parse and
                    // are skipped.
                    if let Ok(jv) = serde_json::from_str::<serde_json::Value>(s) {
                        if let Some(p) =
                            morloc_runtime_types::null_check::first_null_in_json(&jv)
                        {
                            eprintln!(
                                "Error: {} does not support embedded NUL bytes in strings (at args[{}]{})",
                                target_pool.lang, i, p
                            );
                            process::clean_exit(1);
                        }
                    }
                }
            }
        }
        run_remote_command(cmd, &parsed_args, sockets, config);
    }
}

/// CLI-shape directives pulled out of a manifest arg. Single-atom
/// fields under the simplified grammar (no OR-chains).
#[derive(Debug, Clone)]
pub struct ArgShape<'a> {
    pub source: SourceAtom,
    pub form: FormAtom,
    pub checks: &'a [Check],
    pub list_source: SourceAtom,
    pub list_form: FormAtom,
    pub list_checks: &'a [Check],
}

impl<'a> ArgShape<'a> {
    /// Extract the shape view from a manifest arg, returning `None` for
    /// variants (Flag, Group) that have no shape vocabulary.
    pub fn from_arg(arg: &'a Arg) -> Option<ArgShape<'a>> {
        match arg {
            Arg::Positional { source, form, checks, list_source, list_form, list_checks, .. } |
            Arg::Optional   { source, form, checks, list_source, list_form, list_checks, .. } => {
                Some(ArgShape {
                    source: *source,
                    form: *form,
                    checks: checks.as_slice(),
                    list_source: *list_source,
                    list_form: *list_form,
                    list_checks: list_checks.as_slice(),
                })
            }
            Arg::Flag { .. } | Arg::Group { .. } => None,
        }
    }

    /// True when every field is at its compiler-emitted default
    /// (`source = Auto`, `form = Auto`, no checks, `list_source =
    /// Inline`, `list_form = Auto`, no list_checks). The default shape
    /// carries the same semantics as the existing
    /// `parse_cli_data_argument` path, so the caller can skip the
    /// shape-aware route entirely.
    pub fn is_default(&self) -> bool {
        self.source == SourceAtom::Auto
            && self.form == FormAtom::Auto
            && self.checks.is_empty()
            && self.list_source == SourceAtom::Inline
            && self.list_form == FormAtom::Auto
            && self.list_checks.is_empty()
    }
}

/// If the wire schema is a list (`aT` or `aT:<dim>`), return its
/// element schema as a string. Returns `None` for non-list schemas.
pub fn element_schema_str(schema_str: &str) -> Option<String> {
    let mut s = schema_str.trim();
    if let Some(rest) = s.strip_prefix('<') {
        if let Some(end) = rest.find('>') {
            s = &rest[end + 1..];
        }
    }
    let rest = s.strip_prefix('a')?;
    let rest = if let Some(after_colon) = rest.strip_prefix(':') {
        after_colon.trim_start_matches(|c: char| c.is_ascii_digit())
    } else {
        rest
    };
    Some(rest.to_string())
}

/// True if the rendered wire schema reduces to a scalar `Float32`
/// (`f4`) or `Float64` (`f8`) primitive (with optional `?` and hint
/// wrapping). Used to decide whether an argv token like `inf`,
/// `-inf`, or `nan` should be reinterpreted as an IEEE 754 special.
pub fn schema_is_float_scalar(schema_str: &str) -> bool {
    let mut s = schema_str.trim();
    if let Some(rest) = s.strip_prefix('?') {
        s = rest;
    }
    if let Some(rest) = s.strip_prefix('<') {
        if let Some(end) = rest.find('>') {
            s = &rest[end + 1..];
        }
    }
    s == "f4" || s == "f8"
}

/// If the argv string is a recognized IEEE 754 special-value literal,
/// return its JSON-quoted form (`"inf"`, `"-inf"`, `"nan"`). The morloc
/// JSON wire layer accepts these quoted lowercase variants per the
/// non-finite-Real convention; this helper lets a user type
/// `inf` / `nan` / `\-inf` on the command line. `\-inf` is the
/// shell-safe spelling of `-inf` — the leading backslash bypasses
/// clap's flag-cluster interpretation of the bare `-i`/`-n`/`-f`
/// sequence.
pub fn maybe_float_special_to_json(argv: &str) -> Option<String> {
    let trimmed = argv.trim();
    let stripped = trimmed.strip_prefix('\\').unwrap_or(trimmed);
    let inner = match stripped {
        "inf" => "inf",
        "-inf" => "-inf",
        "nan" => "nan",
        _ => return None,
    };
    Some(format!("\"{}\"", inner))
}

/// Process inline byte-string escape sequences. The default mapping is
/// "argv char -> UTF-8 bytes"; the recognized escapes let users encode
/// arbitrary byte values that wouldn't otherwise survive the shell or
/// argv layer.
///
/// Recognized escapes (errors on anything else):
///   `\\`  literal backslash
///   `\n`  byte 0x0A
///   `\t`  byte 0x09
///   `\r`  byte 0x0D
///   `\0`  byte 0x00
///   `\xNN` byte with the given two-digit hex value
///
/// A `\0` escape produces a real NUL byte in the output buffer even
/// though argv itself cannot contain NUL.
pub fn process_inline_bytes_escapes(argv: &str) -> Result<Vec<u8>, String> {
    let mut out: Vec<u8> = Vec::with_capacity(argv.len());
    let mut chars = argv.chars().peekable();
    while let Some(c) = chars.next() {
        if c != '\\' {
            let mut buf = [0u8; 4];
            let s = c.encode_utf8(&mut buf);
            out.extend_from_slice(s.as_bytes());
            continue;
        }
        match chars.next() {
            Some('\\') => out.push(b'\\'),
            Some('n') => out.push(b'\n'),
            Some('t') => out.push(b'\t'),
            Some('r') => out.push(b'\r'),
            Some('0') => out.push(0),
            Some('x') => {
                let h1 = chars
                    .next()
                    .ok_or_else(|| "incomplete `\\x` escape: needs two hex digits".to_string())?;
                let h2 = chars
                    .next()
                    .ok_or_else(|| "incomplete `\\x` escape: needs two hex digits".to_string())?;
                if !h1.is_ascii_hexdigit() || !h2.is_ascii_hexdigit() {
                    return Err(format!(
                        "invalid `\\x` escape: `\\x{}{}` must be two hex digits",
                        h1, h2
                    ));
                }
                let byte = (h1.to_digit(16).unwrap() as u8) * 16
                    + (h2.to_digit(16).unwrap() as u8);
                out.push(byte);
            }
            Some(other) => {
                return Err(format!(
                    "unrecognized escape: `\\{}`; supported: \\\\ \\n \\t \\r \\0 \\xNN",
                    other
                ));
            }
            None => return Err("trailing backslash with no escape character".to_string()),
        }
    }
    Ok(out)
}

/// If `argv` is the Unix stdin/stdout shorthand `-` and the arg
/// carries a `check.path` directive, return the OS path that selects
/// the corresponding standard stream (so the pool's `fopen` etc. can
/// open it as a regular file). Returns `None` when no substitution
/// applies; the caller should keep the original value.
///
/// Mapping (based on the path mode):
/// * `r`  -> `/dev/stdin`
/// * `w`  -> `/dev/stdout`
/// * `x`  -> `None` (exclusive-create against stdout is nonsensical --
///   `/dev/stdout` already exists, so the check would fail after
///   substitution. The user should use `w` for stdout output.)
/// * `rw` -> `None` (ambiguous; stdin vs stdout can't both be it).
pub fn substitute_stdio_dash(argv: &str, checks: &[Check]) -> Option<String> {
    if argv != "-" {
        return None;
    }
    for c in checks {
        match c {
            Check::Path(perm) => {
                return match perm.as_str() {
                    "r" => Some("/dev/stdin".to_string()),
                    "w" => Some("/dev/stdout".to_string()),
                    _ => None,
                };
            }
        }
    }
    None
}

/// Apply value-invariant checks against the raw argv token. Returns
/// `Err` on the first failing check; the error message identifies the
/// check kind so the user can locate the docstring field.
///
/// Path modes follow the Python `open()` convention adapted to a
/// pre-open filesystem predicate:
///
///   r  = exists and readable
///   w  = (exists and writable) OR (does not exist and parent writable)
///   x  = does not exist, parent writable  (exclusive create)
///   rw = exists, readable, writable
pub fn apply_checks(argv: &str, checks: &[Check]) -> Result<(), String> {
    for c in checks {
        match c {
            Check::Path(perm) => apply_path_check(argv, perm)?,
        }
    }
    Ok(())
}

fn apply_path_check(argv: &str, perm: &str) -> Result<(), String> {
    match perm {
        "r"  => check_r(argv),
        "w"  => check_w(argv),
        "x"  => check_x(argv),
        "rw" => check_r(argv).and_then(|_| check_w_existing(argv)),
        other => Err(format!(
            "check.path: unknown mode '{}'; expected one of: r, w, x, rw",
            other
        )),
    }
}

fn c_path_of(argv: &str) -> Result<std::ffi::CString, String> {
    std::ffi::CString::new(argv).map_err(|_| {
        format!("check.path: argv '{}' contains an interior NUL byte", argv)
    })
}

fn check_r(argv: &str) -> Result<(), String> {
    let cp = c_path_of(argv)?;
    let rc = unsafe { libc::access(cp.as_ptr(), libc::R_OK) };
    if rc == 0 {
        Ok(())
    } else {
        Err(format!(
            "check.path: r requires path '{}' to exist and be readable",
            argv
        ))
    }
}

fn check_w_existing(argv: &str) -> Result<(), String> {
    let cp = c_path_of(argv)?;
    let rc = unsafe { libc::access(cp.as_ptr(), libc::W_OK) };
    if rc == 0 {
        Ok(())
    } else {
        Err(format!(
            "check.path: rw requires path '{}' to exist and be writable",
            argv
        ))
    }
}

/// `w`: path is writable-or-creatable. Passes when the path exists and
/// is writable, or when it does not exist and the parent directory is
/// writable. Matches Python `open(_, 'w')` semantics.
fn check_w(argv: &str) -> Result<(), String> {
    let cp = c_path_of(argv)?;
    let rc = unsafe { libc::access(cp.as_ptr(), libc::W_OK) };
    if rc == 0 {
        return Ok(());
    }
    // Fall through to the create case only for the "does not exist"
    // errno. If the file exists but is unwritable, report that;
    // don't mask a permission error as a creatable-path check.
    let err = std::io::Error::last_os_error();
    if err.raw_os_error() != Some(libc::ENOENT) {
        return Err(format!(
            "check.path: w requires path '{}' to be writable ({})",
            argv, err
        ));
    }
    check_parent_writable(argv, "w")
}

/// `x`: exclusive create. Path must not exist and the parent directory
/// must be writable. Matches Python `open(_, 'x')` and
/// `open(O_CREAT|O_EXCL)` semantics.
fn check_x(argv: &str) -> Result<(), String> {
    if std::path::Path::new(argv).exists() {
        return Err(format!(
            "check.path: x requires path '{}' to not yet exist",
            argv
        ));
    }
    check_parent_writable(argv, "x")
}

fn check_parent_writable(argv: &str, mode: &str) -> Result<(), String> {
    let parent = std::path::Path::new(argv).parent();
    let parent_dir = match parent {
        Some(p) if !p.as_os_str().is_empty() => p.to_path_buf(),
        _ => std::path::PathBuf::from("."),
    };
    let parent_c = std::ffi::CString::new(parent_dir.to_string_lossy().as_ref())
        .map_err(|_| {
            "check.path: parent directory path contains a NUL byte".to_string()
        })?;
    let rc = unsafe { libc::access(parent_c.as_ptr(), libc::W_OK) };
    if rc == 0 {
        Ok(())
    } else {
        Err(format!(
            "check.path: {} requires parent directory of '{}' to be writable",
            mode, argv
        ))
    }
}

/// `-` -> `/dev/stdio` substitution, `check.path:` validation, and
/// the `literal: true` quoting shim -- the pre-wrap pipeline every
/// user-supplied argv token flows through before reaching the runtime.
/// Errors abort via `die_with_error` with the caller-supplied prefix.
pub fn preprocess_cli_value(
    val: String,
    checks: &[Check],
    quoted_flag: bool,
    error_prefix: &str,
) -> String {
    let v = substitute_stdio_dash(&val, checks).unwrap_or(val);
    // The stdio sentinels are opened by the runtime (stdin via the nexus
    // RPC channel, stdout as a device); "read from stdin" is not
    // "validate a user-named path", and `access("/dev/stdin", R_OK)` can
    // spuriously fail, so skip the value-invariant checks on them.
    let is_stdio_sentinel = v == "/dev/stdin" || v == "/dev/stdout";
    if !is_stdio_sentinel {
        if let Err(e) = apply_checks(&v, checks) {
            crate::runlog::die_with_error(&format!("{}: {}", error_prefix, e));
        }
    }
    if quoted_flag { quoted(&v) } else { v }
}

/// Apply the shape directives to a raw argv token: run any nexus-side
/// transforms (inline bytes -> JSON array) and value-invariant checks
/// (`check.path`). Any error aborts via `die_with_error`.
///
/// Special case: when `source: inline` and `form: bytes` (or
/// `bytes-only`), the argv is escape-processed to a `Vec<u8>` and
/// JSON-encoded as `[byte, byte, ...]`. The runtime then sees a JSON
/// inline value and decodes it through `read_json_with_schema`. This
/// is the DNA-style ASCII-bytes shortcut on `[UInt8]`.
pub fn apply_shape_to_argv(raw: String, shape: Option<&ArgShape>, i: usize) -> String {
    let shape = match shape {
        Some(s) if !s.is_default() => s,
        _ => return raw,
    };

    let inline_bytes_shortcut = shape.source == SourceAtom::Inline
        && (shape.form == FormAtom::Bytes || shape.form == FormAtom::BytesOnly);
    if inline_bytes_shortcut {
        let bytes = match process_inline_bytes_escapes(&raw) {
            Ok(b) => b,
            Err(e) => crate::runlog::die_with_error(&format!("argument #{}: {}", i, e)),
        };
        let parts: Vec<String> = bytes.iter().map(|b| b.to_string()).collect();
        return format!("[{}]", parts.join(","));
    }

    raw
}

/// Return the `(source_code, form_code)` pair to pass through the
/// shape-aware FFI, or `None` when the runtime can use the cheaper
/// default path. The default case includes the `source: inline +
/// form: bytes/bytes-only` shortcut on `[UInt8]`, which is already
/// JSON-encoded by `apply_shape_to_argv`.
pub fn shaped_dispatch_codes(shape: &ArgShape) -> Option<(u8, u8)> {
    let inline_bytes_shortcut = shape.source == SourceAtom::Inline
        && (shape.form == FormAtom::Bytes || shape.form == FormAtom::BytesOnly);
    if shape.is_default() || inline_bytes_shortcut {
        return None;
    }
    Some((source_atom_code(shape.source), form_atom_code(shape.form)))
}

/// Serialize the per-element shape portion of an ArgShape into the
/// JSON the runtime's `parse_cli_data_argument_shaped` expects.
/// Returns `None` when every per-element field is at its default, so
/// the runtime can skip the per-element pipeline entirely.
pub fn build_list_config_json(shape: &ArgShape) -> Option<String> {
    let list_source_is_default = shape.list_source == SourceAtom::Inline;
    let list_form_is_default = shape.list_form == FormAtom::Auto;
    if list_source_is_default && list_form_is_default && shape.list_checks.is_empty() {
        return None;
    }
    let source_str = source_atom_name(shape.list_source);
    let form_str = form_atom_name(shape.list_form);
    let checks_parts: Vec<String> = shape
        .list_checks
        .iter()
        .map(|c| match c {
            Check::Path(p) => format!(
                "{{\"kind\":\"path\",\"value\":{}}}",
                serde_json::to_string(p).unwrap_or_else(|_| "\"\"".to_string())
            ),
        })
        .collect();
    Some(format!(
        "{{\"source\":\"{}\",\"form\":\"{}\",\"checks\":[{}]}}",
        source_str,
        form_str,
        checks_parts.join(",")
    ))
}

/// Wire-stable u8 code for a source atom. Must match the dispatch
/// arms in the runtime.
pub fn source_atom_code(atom: SourceAtom) -> u8 {
    match atom {
        SourceAtom::Auto => 0,
        SourceAtom::Inline => 1,
        SourceAtom::File => 2,
    }
}

/// Wire-stable u8 code for a form atom. Must match the dispatch arms
/// in `parse_cli_data_argument_shaped` (morloc-runtime).
pub fn form_atom_code(atom: FormAtom) -> u8 {
    match atom {
        FormAtom::Auto => 0,
        FormAtom::Packet => 1,
        FormAtom::Bytes => 2,
        FormAtom::List => 3,
        FormAtom::BytesOnly => 4,
    }
}

fn source_atom_name(atom: SourceAtom) -> &'static str {
    match atom {
        SourceAtom::Auto => "auto",
        SourceAtom::Inline => "inline",
        SourceAtom::File => "file",
    }
}

fn form_atom_name(atom: FormAtom) -> &'static str {
    match atom {
        FormAtom::Auto => "auto",
        FormAtom::Packet => "packet",
        FormAtom::Bytes => "bytes",
        FormAtom::List => "list",
        FormAtom::BytesOnly => "bytes-only",
    }
}

/// Parsed CLI argument value for a manifest arg slot.
#[derive(Debug)]
pub enum ArgValue {
    /// A value string (already quoted if needed).
    Value(String),
    /// Null/absent value.
    Null,
    /// Group argument with per-entry values.
    Group {
        grp_val: Option<String>,
        fields: Vec<Option<String>>,
        defaults: Vec<Option<String>>,
    },
    /// Variadic argument: the user supplied N tokens that the C-side
    /// `parse_cli_data_argument_list` will assemble into a single
    /// list packet. `literal` is the `literal: true` flag -- when
    /// set, each token is JSON-quoted in Rust before reaching C so
    /// the per-element classifier treats it as inline content rather
    /// than a file path.
    Many { tokens: Vec<String>, literal: bool },
}


/// Route a single positional value through the C-side parser. Picks
/// the shape-aware FFI when the arg has any non-default
/// `source:` / `form:` / `list.*` configuration; otherwise falls back
/// to the default `parse_cli_data_argument` path. Returns the C-owned
/// packet pointer (null on error -- caller drains `errmsg`).
unsafe fn dispatch_one_arg(
    shape: Option<&ArgShape>,
    c_arg: *mut std::ffi::c_void,
    c_schema: *const morloc_runtime_types::cschema::CSchema,
    errmsg: &mut *mut std::ffi::c_char,
) -> *mut u8 {
    extern "C" {
        fn parse_cli_data_argument(
            dest: *mut u8, arg: *const std::ffi::c_void,
            schema: *const morloc_runtime_types::cschema::CSchema,
            errmsg: *mut *mut std::ffi::c_char,
        ) -> *mut u8;
        fn parse_cli_data_argument_shaped(
            dest: *mut u8, arg: *const std::ffi::c_void,
            schema: *const morloc_runtime_types::cschema::CSchema,
            source_code: u8, form_code: u8,
            list_config_json: *const std::ffi::c_char,
            errmsg: *mut *mut std::ffi::c_char,
        ) -> *mut u8;
    }
    let shape_codes = shape.and_then(shaped_dispatch_codes);
    let list_cfg_json: Option<std::ffi::CString> = shape
        .and_then(build_list_config_json)
        .and_then(|s| std::ffi::CString::new(s).ok());
    let list_cfg_ptr: *const std::ffi::c_char = list_cfg_json
        .as_ref()
        .map(|c| c.as_ptr())
        .unwrap_or(std::ptr::null());
    let pkt = if let Some((src_code, form_code)) = shape_codes {
        parse_cli_data_argument_shaped(
            std::ptr::null_mut(), c_arg, c_schema,
            src_code, form_code, list_cfg_ptr, errmsg,
        )
    } else {
        parse_cli_data_argument(std::ptr::null_mut(), c_arg, c_schema, errmsg)
    };
    drop(list_cfg_json);
    pkt
}

// -- Command execution ------------------------------------------------------

/// Execute a remote command by sending a call packet to the pool.
fn run_remote_command(
    cmd: &Command,
    args: &[ArgValue],
    sockets: &[PoolSocket],
    config: &NexusConfig,
) {
    use morloc_runtime_types::packet;
    use morloc_runtime_types::schema::parse_schema;
    use std::io::{Read, Write};
    use std::os::unix::net::UnixStream;

    // C library functions from libmorloc.so. Per-arg parsing
    // (`parse_cli_data_argument` / `..._shaped`) is encapsulated in
    // `dispatch_one_arg`.
    extern "C" {
        fn parse_cli_data_argument_list(
            dest: *mut u8,
            args: *const *const std::ffi::c_void,
            n: usize,
            list_schema: *const morloc_runtime_types::cschema::CSchema,
            errmsg: *mut *mut std::ffi::c_char,
        ) -> *mut u8;
        fn initialize_positional(value: *mut std::ffi::c_char) -> *mut std::ffi::c_void;
        fn free_argument_t(arg: *mut std::ffi::c_void);
        fn morloc_packet_size(packet: *const u8, errmsg: *mut *mut std::ffi::c_char) -> usize;
        fn make_morloc_local_call_packet(
            midx: u32, arg_packets: *const *const u8, nargs: usize,
            errmsg: *mut *mut std::ffi::c_char,
        ) -> *mut u8;
        fn get_morloc_data_packet_value(
            data: *const u8, schema: *const morloc_runtime_types::cschema::CSchema,
            errmsg: *mut *mut std::ffi::c_char,
        ) -> *mut u8;
    }

    let socket = &sockets[cmd.pool_index];

    // Parse return schema
    let return_schema = match parse_schema(&cmd.ret.schema) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error: failed to parse return schema '{}': {}", cmd.ret.schema, e);
            process::clean_exit(1);
        }
    };

    // The parsed `args` list and `cmd.args` are index-aligned 1:1 in
    // declaration order: parse_command_args pushes one ArgValue for
    // EVERY arg (including flags). The Haskell compiler emits one
    // schema per arg position too. Walk both lists in lockstep; for
    // flags, schema_str() returns None and the flag's ArgValue is
    // already a ready-to-send "true"/"false" string that doesn't need
    // packet conversion -- but the original v1 dispatch path still
    // ran flags through parse_cli_data_argument with the flag's bool
    // schema, so we mirror that to keep the wire format consistent.
    let mut arg_packets: Vec<(*mut u8, usize)> = Vec::new();
    for (i, (arg_val, arg_def)) in args.iter().zip(cmd.args.iter()).enumerate() {
        let schema_str = arg_def.schema_str().unwrap_or("b");
        let schema = match parse_schema(schema_str) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Error: failed to parse arg schema #{}: {}", i, e);
                process::clean_exit(1);
            }
        };

        let c_schema = morloc_runtime_types::cschema::CSchema::from_rust(&schema);
        let mut errmsg: *mut std::ffi::c_char = std::ptr::null_mut();

        // Variadic args (many: true): compile-time validation rejects
        // every form/source modifier; only the IEEE 754 special-value
        // wrap runs per token.
        let mut many_override: Option<(Vec<String>, bool)> = None;
        if let ArgValue::Many { tokens, literal } = arg_val {
            let elem_is_float = element_schema_str(schema_str)
                .as_deref()
                .map(schema_is_float_scalar)
                .unwrap_or(false);
            if elem_is_float {
                let rewritten: Vec<String> = tokens
                    .iter()
                    .map(|t| maybe_float_special_to_json(t).unwrap_or_else(|| t.to_string()))
                    .collect();
                if rewritten.iter().zip(tokens.iter()).any(|(r, t)| r != t) {
                    many_override = Some((rewritten, *literal));
                }
            }
        }
        let c_pkt = match arg_val {
            ArgValue::Many { tokens, literal } => unsafe {
                let (toks_ref, lit) = match &many_override {
                    Some((rt, lit)) => (rt.as_slice(), *lit),
                    None => (tokens.as_slice(), *literal),
                };
                assemble_many_packet(
                    toks_ref, lit, c_schema, &mut errmsg,
                    initialize_positional, free_argument_t, parse_cli_data_argument_list,
                )
            },
            ArgValue::Group { grp_val, fields, defaults } => {
                extern "C" {
                    fn initialize_unrolled(
                        size: usize, default_value: *mut std::ffi::c_char,
                        fields: *mut *mut std::ffi::c_char,
                        default_fields: *mut *mut std::ffi::c_char,
                    ) -> *mut std::ffi::c_void;
                    fn parse_cli_data_argument_group_shaped(
                        dest: *mut u8,
                        arg: *const std::ffi::c_void,
                        schema: *const morloc_runtime_types::cschema::CSchema,
                        source_codes: *const u8,
                        form_codes: *const u8,
                        list_config_jsons: *const *const std::ffi::c_char,
                        n_fields: usize,
                        errmsg: *mut *mut std::ffi::c_char,
                    ) -> *mut u8;
                }
                let n = fields.len();
                let entry_shapes: Vec<Option<ArgShape>> = match arg_def {
                    crate::manifest::Arg::Group { entries, .. } => entries.iter()
                        .map(|e| ArgShape::from_arg(&e.arg))
                        .collect(),
                    _ => vec![None; n],
                };
                let any_shaped = entry_shapes.iter().any(|s| s.as_ref()
                    .map(|sh| !sh.is_default())
                    .unwrap_or(false));

                // Hold every CString until after the C call so its
                // strdup'd copy inside argument_t is the only owner
                // that lives past this scope.
                let grp_val_keep: Option<std::ffi::CString> =
                    grp_val.as_ref().map(|s| std::ffi::CString::new(s.as_str()).unwrap());
                let fields_keep: Vec<Option<std::ffi::CString>> = fields.iter().enumerate()
                    .map(|(k, f)| f.as_ref().map(|s| {
                        let shape_ref = entry_shapes.get(k).and_then(|o| o.as_ref());
                        let processed = if shape_ref.map(|sh| sh.is_default()).unwrap_or(true) {
                            s.clone()
                        } else {
                            apply_shape_to_argv(s.clone(), shape_ref, i)
                        };
                        std::ffi::CString::new(processed).unwrap()
                    }))
                    .collect();
                let defaults_keep: Vec<Option<std::ffi::CString>> = defaults.iter()
                    .map(|d| d.as_ref().map(|s| std::ffi::CString::new(s.as_str()).unwrap()))
                    .collect();

                let grp_val_c: *mut std::ffi::c_char = grp_val_keep
                    .as_ref()
                    .map(|c| c.as_ptr() as *mut std::ffi::c_char)
                    .unwrap_or(std::ptr::null_mut());
                let mut c_fields: Vec<*mut std::ffi::c_char> = fields_keep.iter()
                    .map(|c| c.as_ref().map(|s| s.as_ptr() as *mut std::ffi::c_char)
                              .unwrap_or(std::ptr::null_mut()))
                    .collect();
                let mut c_defaults: Vec<*mut std::ffi::c_char> = defaults_keep.iter()
                    .map(|c| c.as_ref().map(|s| s.as_ptr() as *mut std::ffi::c_char)
                              .unwrap_or(std::ptr::null_mut()))
                    .collect();

                let c_arg = unsafe {
                    initialize_unrolled(n, grp_val_c, c_fields.as_mut_ptr(), c_defaults.as_mut_ptr())
                };

                let pkt = if any_shaped {
                    let mut src_codes: Vec<u8> = Vec::with_capacity(n);
                    let mut frm_codes: Vec<u8> = Vec::with_capacity(n);
                    let mut list_cfg_keep: Vec<Option<std::ffi::CString>> = Vec::with_capacity(n);
                    for shape in &entry_shapes {
                        let (sc, fc) = shape.as_ref().and_then(shaped_dispatch_codes).unwrap_or((0, 0));
                        let list_cfg_json = shape.as_ref().and_then(build_list_config_json);
                        src_codes.push(sc);
                        frm_codes.push(fc);
                        list_cfg_keep.push(list_cfg_json
                            .and_then(|s| std::ffi::CString::new(s).ok()));
                    }
                    let list_cfg_ptrs: Vec<*const std::ffi::c_char> = list_cfg_keep.iter()
                        .map(|o| o.as_ref().map(|c| c.as_ptr()).unwrap_or(std::ptr::null()))
                        .collect();
                    let p = unsafe {
                        parse_cli_data_argument_group_shaped(
                            std::ptr::null_mut(), c_arg, c_schema,
                            src_codes.as_ptr(), frm_codes.as_ptr(),
                            list_cfg_ptrs.as_ptr(), n, &mut errmsg,
                        )
                    };
                    drop(list_cfg_keep);
                    p
                } else {
                    unsafe { dispatch_one_arg(None, c_arg, c_schema, &mut errmsg) }
                };
                unsafe { free_argument_t(c_arg) };
                drop(grp_val_keep);
                drop(fields_keep);
                drop(defaults_keep);
                pkt
            }
            ArgValue::Value(_) | ArgValue::Null => {
                let raw_str = match arg_val {
                    ArgValue::Value(s) => s.clone(),
                    ArgValue::Null => "null".to_string(),
                    _ => unreachable!(),
                };
                let raw_str = if schema_is_float_scalar(schema_str) {
                    maybe_float_special_to_json(&raw_str).unwrap_or(raw_str)
                } else {
                    raw_str
                };
                let shape = ArgShape::from_arg(arg_def);
                let json_str = apply_shape_to_argv(raw_str, shape.as_ref(), i);
                let json_c = std::ffi::CString::new(json_str.as_str()).unwrap();
                let c_arg = unsafe {
                    initialize_positional(json_c.as_ptr() as *mut std::ffi::c_char)
                };
                let pkt = unsafe {
                    dispatch_one_arg(shape.as_ref(), c_arg, c_schema, &mut errmsg)
                };
                unsafe { free_argument_t(c_arg) };
                drop(json_c);
                pkt
            }
        };
        unsafe { morloc_runtime_types::cschema::CSchema::free(c_schema) };

        if c_pkt.is_null() {
            let msg = process::take_c_errmsg(errmsg)
                .unwrap_or_else(|| "unknown error".into());
            crate::runlog::die_with_error(&format!("failed to parse argument #{}: {}", i, msg));
        }

        let pkt_size = unsafe { morloc_packet_size(c_pkt, &mut errmsg) };
        arg_packets.push((c_pkt, pkt_size));
    }

    // Build call packet via C library
    let arg_ptrs: Vec<*const u8> = arg_packets.iter().map(|(p, _)| *p as *const u8).collect();
    let mut errmsg_call: *mut std::ffi::c_char = std::ptr::null_mut();
    let c_call = unsafe {
        make_morloc_local_call_packet(cmd.mid, arg_ptrs.as_ptr(), arg_packets.len(), &mut errmsg_call)
    };
    for (p, _) in &arg_packets {
        unsafe { libc::free(*p as *mut std::ffi::c_void) };
    }
    arg_packets.clear();
    if c_call.is_null() {
        eprintln!("Error: failed to create call packet");
        process::clean_exit(1);
    }

    let call_size = unsafe {
        let mut e: *mut std::ffi::c_char = std::ptr::null_mut();
        morloc_packet_size(c_call, &mut e)
    };
    let call_slice: &[u8] = unsafe { std::slice::from_raw_parts(c_call, call_size) };

    // Send to pool and receive response
    let mut stream = match UnixStream::connect(&socket.socket_path) {
        Ok(s) => s,
        Err(e) => {
            unsafe { libc::free(c_call as *mut std::ffi::c_void) };
            die_with_pool_error(
                socket,
                cmd.pool_index,
                &format!("failed to connect to pool '{}'", socket.lang),
                &e,
            );
        }
    };

    if let Err(e) = stream.write_all(call_slice) {
        unsafe { libc::free(c_call as *mut std::ffi::c_void) };
        die_with_pool_error(
            socket,
            cmd.pool_index,
            &format!("failed to send call packet to pool '{}'", socket.lang),
            &e,
        );
    }
    unsafe { libc::free(c_call as *mut std::ffi::c_void) };

    // Read response header, then the rest directly into the final buffer.
    let mut hdr = [0u8; 32];
    if let Err(e) = stream.read_exact(&mut hdr) {
        die_with_pool_error(
            socket,
            cmd.pool_index,
            &format!("failed to read response header from pool '{}'", socket.lang),
            &e,
        );
    }

    let resp_header = match packet::PacketHeader::from_bytes(&hdr) {
        Ok(h) => h,
        Err(e) => {
            eprintln!("Error: invalid response packet: {}", e);
            process::clean_exit(1);
        }
    };

    let offset = { resp_header.offset } as usize;
    let length = { resp_header.length } as usize;
    let remaining = offset + length;
    let mut full_packet = vec![0u8; 32 + remaining];
    full_packet[..32].copy_from_slice(&hdr);
    if remaining > 0 {
        if let Err(e) = stream.read_exact(&mut full_packet[32..]) {
            die_with_pool_error(
                socket,
                cmd.pool_index,
                &format!("failed to read response body from pool '{}'", socket.lang),
                &e,
            );
        }
    }

    // Check for error
    match packet::get_error_message(&full_packet) {
        Ok(Some(err_msg)) => {
            // The "run failed\n" prefix is stripped from the recorded
            // form -- consumers want the foreign traceback in summary
            // .json's `error` field, not the morloc-level wrapper.
            let collapsed = collapse_duplicate_lines(&err_msg);
            crate::runlog::record_error(&collapsed);
            eprintln!("Error: run failed\n{}", collapsed);
            process::clean_exit(1);
        }
        Ok(None) => {}
        Err(e) => {
            eprintln!("Error: failed to parse response: {}", e);
            process::clean_exit(1);
        }
    }

    // Extract and print via C library for correct voidstar handling
    let c_schema = morloc_runtime_types::cschema::CSchema::from_rust(&return_schema);
    let mut errmsg: *mut std::ffi::c_char = std::ptr::null_mut();
    let result_ptr = unsafe {
        get_morloc_data_packet_value(full_packet.as_ptr(), c_schema, &mut errmsg)
    };
    if result_ptr.is_null() {
        let msg = process::take_c_errmsg(errmsg)
            .unwrap_or_else(|| "unknown error".into());
        eprintln!("Error: failed to extract result: {}", msg);
        unsafe { morloc_runtime_types::cschema::CSchema::free(c_schema) };
        process::clean_exit(1);
    }

    // Check if response is Arrow format
    let is_arrow = resp_header.is_data() && unsafe { resp_header.command.data.format } == packet::PACKET_FORMAT_ARROW;

    // Print using the C library for correct output. Top-level null-ish
    // suppression (Unit and Optional-None producing empty stdout) is
    // applied uniformly: JSON drops the output; `-f packet` skips its
    // trailing DATA_PACKET emission so a program that streams through
    // `@stdout` and returns Unit doesn't get a phantom packet appended
    // past its stream footer. `--keep-null` overrides in both cases.
    print_result_c(result_ptr, c_schema, &full_packet, is_arrow, config);
    unsafe { morloc_runtime_types::cschema::CSchema::free(c_schema) };
}

/// Print using the C library functions for correct voidstar handling.
pub(crate) fn print_result_c(
    ptr: *mut u8,
    schema: *const morloc_runtime_types::cschema::CSchema,
    full_packet: &[u8],
    is_arrow: bool,
    config: &NexusConfig,
) {
    extern "C" {
        fn print_voidstar(
            voidstar: *const std::ffi::c_void,
            schema: *const morloc_runtime_types::cschema::CSchema,
            keep_null: bool,
            errmsg: *mut *mut std::ffi::c_char,
        ) -> i32;
        fn pretty_print_voidstar(
            voidstar: *const std::ffi::c_void,
            schema: *const morloc_runtime_types::cschema::CSchema,
            keep_null: bool,
            errmsg: *mut *mut std::ffi::c_char,
        ) -> i32;
        fn print_arrow_as_json(
            data: *const std::ffi::c_void,
            errmsg: *mut *mut std::ffi::c_char,
        ) -> bool;
        fn print_arrow_as_table(
            data: *const std::ffi::c_void,
            errmsg: *mut *mut std::ffi::c_char,
        ) -> bool;
        fn pack_with_schema(
            mlc: *const std::ffi::c_void,
            schema: *const morloc_runtime_types::cschema::CSchema,
            mpkptr: *mut *mut std::ffi::c_char,
            mpk_size: *mut usize,
            errmsg: *mut *mut std::ffi::c_char,
        ) -> i32;
    }

    use morloc_runtime_types::{PRINT_RESULT_OK, PRINT_RESULT_PIPE_CLOSED};

    let mut errmsg: *mut std::ffi::c_char = std::ptr::null_mut();

    match config.output_format {
        OutputFormat::Json => {
            if is_arrow {
                let ok = unsafe {
                    if config.print_flag {
                        print_arrow_as_table(ptr as *const std::ffi::c_void, &mut errmsg)
                    } else {
                        print_arrow_as_json(ptr as *const std::ffi::c_void, &mut errmsg)
                    }
                };
                if !ok {
                    let msg = process::take_c_errmsg(errmsg)
                        .unwrap_or_else(|| "unknown error".into());
                    eprintln!("Error: {}", msg);
                    process::clean_exit(1);
                }
            } else {
                let rc = unsafe {
                    if config.print_flag {
                        pretty_print_voidstar(ptr as *const std::ffi::c_void, schema, config.keep_null, &mut errmsg)
                    } else {
                        print_voidstar(ptr as *const std::ffi::c_void, schema, config.keep_null, &mut errmsg)
                    }
                };
                match rc {
                    PRINT_RESULT_OK => {}
                    PRINT_RESULT_PIPE_CLOSED => process::exit_broken_pipe(),
                    _ => {
                        let msg = process::take_c_errmsg(errmsg)
                            .unwrap_or_else(|| "unknown error".into());
                        eprintln!("Error: {}", msg);
                        process::clean_exit(1);
                    }
                }
            }
        }
        OutputFormat::MessagePack => {
            let mut mpk_ptr: *mut std::ffi::c_char = std::ptr::null_mut();
            let mut mpk_size: usize = 0;
            let rc = unsafe {
                pack_with_schema(
                    ptr as *const std::ffi::c_void,
                    schema,
                    &mut mpk_ptr,
                    &mut mpk_size,
                    &mut errmsg,
                )
            };
            if rc != 0 {
                eprintln!("Error: msgpack serialization failed");
                process::clean_exit(1);
            }
            if config.print_flag {
                let bytes = unsafe { std::slice::from_raw_parts(mpk_ptr as *const u8, mpk_size) };
                if let Err(e) = write_hex_dump(bytes) {
                    if e.kind() == std::io::ErrorKind::BrokenPipe {
                        if !mpk_ptr.is_null() {
                            unsafe { libc::free(mpk_ptr as *mut std::ffi::c_void) };
                        }
                        process::exit_broken_pipe();
                    }
                    eprintln!("Error: msgpack hex dump failed: {}", e);
                    process::clean_exit(1);
                }
            } else {
                use std::io::Write;
                let bytes = unsafe { std::slice::from_raw_parts(mpk_ptr as *const u8, mpk_size) };
                match std::io::stdout().lock().write_all(bytes) {
                    Ok(()) => {}
                    Err(e) if e.kind() == std::io::ErrorKind::BrokenPipe => {
                        if !mpk_ptr.is_null() {
                            unsafe { libc::free(mpk_ptr as *mut std::ffi::c_void) };
                        }
                        process::exit_broken_pipe();
                    }
                    Err(e) => {
                        eprintln!("Error: msgpack write failed: {}", e);
                        process::clean_exit(1);
                    }
                }
            }
            if !mpk_ptr.is_null() {
                unsafe { libc::free(mpk_ptr as *mut std::ffi::c_void) };
            }
        }
        OutputFormat::VoidStar => {
            // A top-level Unit/None return carries no data. Suppress it
            // (as JSON and Packet already do) so a command that streams
            // its output through @stdout and returns () doesn't get a
            // trailing Unit packet appended past the stream. --keep-null
            // opts back in.
            if !config.keep_null
                && unsafe { morloc_runtime_types::is_top_null(schema, ptr) }
            {
                return;
            }
            extern "C" {
                fn print_morloc_data_packet(
                    packet: *const u8,
                    schema: *const morloc_runtime_types::cschema::CSchema,
                    errmsg: *mut *mut std::ffi::c_char,
                ) -> i32;
            }
            if config.print_flag {
                if let Err(e) = write_hex_dump_grouped(&full_packet) {
                    if e.kind() == std::io::ErrorKind::BrokenPipe {
                        process::exit_broken_pipe();
                    }
                    eprintln!("Error: voidstar hex dump failed: {}", e);
                    process::clean_exit(1);
                }
            } else {
                let mut errmsg2: *mut std::ffi::c_char = std::ptr::null_mut();
                unsafe { print_morloc_data_packet(full_packet.as_ptr(), schema, &mut errmsg2) };
            }
        }
        OutputFormat::Packet => {
            // Top-level Unit/Optional-None suppression: a program that
            // returns `<IO> ()` after streaming its output through
            // `@stdout` would otherwise get a trailing Unit DATA_PACKET
            // appended past the stream footer, clobbering the tail
            // magic. `--keep-null` opts back in.
            if !config.keep_null
                && unsafe { morloc_runtime_types::is_top_null(schema, ptr) }
            {
                return;
            }
            use std::os::unix::io::AsRawFd;
            let stdout = std::io::stdout();
            let lock = stdout.lock();
            let fd = lock.as_raw_fd();
            let mut nerr: *mut std::ffi::c_char = std::ptr::null_mut();
            // Empty full_packet means the caller has only a voidstar
            // (e.g. `view --pattern`, or a stream input walked via IFile);
            // build a fresh DATA_PACKET from it. Passing `&[]` through
            // `normalize_data_packet_to_fd` reads a dangling pointer.
            let n = if full_packet.is_empty() {
                extern "C" {
                    fn mlc_write_voidstar_data_packet_to_fd(
                        data: *const std::ffi::c_void,
                        schema: *const morloc_runtime_types::cschema::CSchema,
                        level: u8,
                        fd: libc::c_int,
                        errmsg: *mut *mut std::ffi::c_char,
                    ) -> i64;
                }
                unsafe {
                    mlc_write_voidstar_data_packet_to_fd(
                        ptr as *const std::ffi::c_void,
                        schema,
                        config.compression_level,
                        fd,
                        &mut nerr,
                    )
                }
            } else {
                extern "C" {
                    fn normalize_data_packet_to_fd(
                        packet: *const u8,
                        packet_size: usize,
                        compression_level: u8,
                        fd: libc::c_int,
                        errmsg: *mut *mut std::ffi::c_char,
                    ) -> i64;
                }
                unsafe {
                    normalize_data_packet_to_fd(
                        full_packet.as_ptr(),
                        full_packet.len(),
                        config.compression_level,
                        fd,
                        &mut nerr,
                    )
                }
            };
            drop(lock);
            if n < 0 {
                let msg = process::take_c_errmsg(nerr)
                    .unwrap_or_else(|| "unknown error".into());
                eprintln!("Error: packet emission failed: {}", msg);
                process::clean_exit(1);
            }
        }
        OutputFormat::Jsonl => {
            // A top-level Unit/None return carries no data. Suppress it
            // (as JSON and Packet already do) so a command that streams
            // its output through @stdout and returns () doesn't get a
            // spurious `null` line appended after the streamed jsonl.
            // --keep-null opts back in.
            if !config.keep_null
                && unsafe { morloc_runtime_types::is_top_null(schema, ptr) }
            {
                return;
            }
            // JSON-lines: one element per line for list schemas, one
            // line total for scalars. Streams element-by-element via
            // the runtime's `print_voidstar_jsonl` -- peak memory is
            // one element's JSON body, not the whole list.
            extern "C" {
                fn print_voidstar_jsonl(
                    data: *const std::ffi::c_void,
                    schema: *const morloc_runtime_types::cschema::CSchema,
                    errmsg: *mut *mut std::ffi::c_char,
                ) -> i32;
            }
            let rc = unsafe {
                print_voidstar_jsonl(
                    ptr as *const std::ffi::c_void,
                    schema,
                    &mut errmsg,
                )
            };
            match rc {
                PRINT_RESULT_OK => {}
                PRINT_RESULT_PIPE_CLOSED => process::exit_broken_pipe(),
                _ => {
                    let msg = process::take_c_errmsg(errmsg)
                        .unwrap_or_else(|| "unknown error".into());
                    eprintln!("Error: {}", msg);
                    process::clean_exit(1);
                }
            }
        }
        OutputFormat::Raw => {
            // Verbatim bytes: emit a Str / [Str] / Vector U8 / [Vector U8] body
            // with no framing -- the output path for `render` handlers.
            // `render.buffer` streams its output and returns (); suppress that
            // top-level Unit so it isn't fed to print_voidstar_raw (which would
            // reject it). Whole-list `render` returns a real Str/Vector U8 value
            // and prints normally. --keep-null opts back in.
            if !config.keep_null
                && unsafe { morloc_runtime_types::is_top_null(schema, ptr) }
            {
                return;
            }
            extern "C" {
                fn print_voidstar_raw(
                    data: *const std::ffi::c_void,
                    schema: *const morloc_runtime_types::cschema::CSchema,
                    errmsg: *mut *mut std::ffi::c_char,
                ) -> i32;
            }
            let rc = unsafe {
                print_voidstar_raw(ptr as *const std::ffi::c_void, schema, &mut errmsg)
            };
            match rc {
                PRINT_RESULT_OK => {}
                PRINT_RESULT_PIPE_CLOSED => process::exit_broken_pipe(),
                _ => {
                    let msg = process::take_c_errmsg(errmsg)
                        .unwrap_or_else(|| "unknown error".into());
                    eprintln!("Error: {}", msg);
                    process::clean_exit(1);
                }
            }
        }
        OutputFormat::Arrow | OutputFormat::Parquet | OutputFormat::Csv => {
            // Table-only output formats. The pool returns an Arrow SHM
            // pointer (PACKET_FORMAT_ARROW); we serialize from there.
            if !is_arrow {
                eprintln!(
                    "Error: --format=arrow|parquet|csv requires a Table return type"
                );
                process::clean_exit(1);
            }
            extern "C" {
                fn write_arrow_ipc_to_buffer(
                    header: *const std::ffi::c_void,
                    out_buf: *mut *mut u8,
                    out_len: *mut usize,
                    errmsg: *mut *mut std::ffi::c_char,
                ) -> i32;
                fn write_parquet_to_buffer(
                    header: *const std::ffi::c_void,
                    out_buf: *mut *mut u8,
                    out_len: *mut usize,
                    errmsg: *mut *mut std::ffi::c_char,
                ) -> i32;
                fn write_csv_to_buffer(
                    header: *const std::ffi::c_void,
                    delimiter: u8,
                    out_buf: *mut *mut u8,
                    out_len: *mut usize,
                    errmsg: *mut *mut std::ffi::c_char,
                ) -> i32;
            }
            let mut buf: *mut u8 = std::ptr::null_mut();
            let mut len: usize = 0;
            let mut err: *mut std::ffi::c_char = std::ptr::null_mut();
            let rc = unsafe {
                match config.output_format {
                    OutputFormat::Arrow => write_arrow_ipc_to_buffer(
                        ptr as *const std::ffi::c_void, &mut buf, &mut len, &mut err,
                    ),
                    OutputFormat::Parquet => write_parquet_to_buffer(
                        ptr as *const std::ffi::c_void, &mut buf, &mut len, &mut err,
                    ),
                    OutputFormat::Csv => write_csv_to_buffer(
                        ptr as *const std::ffi::c_void, b',', &mut buf, &mut len, &mut err,
                    ),
                    _ => unreachable!(),
                }
            };
            if rc != 0 || buf.is_null() {
                let msg = process::take_c_errmsg(err)
                    .unwrap_or_else(|| "unknown error".into());
                eprintln!("Error: {}", msg);
                process::clean_exit(1);
            }
            use std::io::Write;
            let bytes = unsafe { std::slice::from_raw_parts(buf, len) };
            let _ = std::io::stdout().lock().write_all(bytes);
            unsafe { libc::free(buf as *mut std::ffi::c_void) };
        }
    }
    process::clean_exit(0);
}

/// Execute a pure command by evaluating the expression via C library.
fn run_pure_command(cmd: &Command, args: &[ArgValue], config: &NexusConfig) {
    use morloc_runtime_types::schema::parse_schema;

    // Open a per-eval SHM arena. Every shm::shmalloc that fires while
    // this guard is alive (CLI arg ingress via msgpack/voidstar unpack,
    // morloc_eval intermediates, the result tree) is tracked and
    // released when the guard drops at the end of this function. Result
    // serialization (print_result_c -> voidstar_to_json_string) reads
    // SHM and writes a libc-allocated JSON string, which survives the
    // drop. For one-shot CLI this is functionally a no-op (the process
    // exits and atexit's shclose unlinks the volumes anyway), but the
    // explicit lifetime keeps nexus and daemon symmetric and protects
    // any future repeated-call use of run_pure_command.
    //
    // The arena lives in libmorloc.so's `thread_local! ARENA`. We
    // cannot call `eval_arena::enter()` Rust-side because the nexus no
    // longer links morloc-runtime as an rlib -- and even if it did,
    // that path would touch the nexus's own copy of ARENA, missing
    // every SHM block tracked by libmorloc.so's morloc_eval. The C-ABI
    // begin/end pair routes us through libmorloc.so's single arena;
    // EvalArenaGuard below wraps the handle in RAII so the
    // exit-on-Drop pattern carries through.
    //
    // Note: error branches below call process::clean_exit(1) which does
    // NOT run Rust destructors; the arena guard's drop is skipped on
    // those paths. That's acceptable here because clean_exit also tears
    // down the whole process via atexit shclose. No additional handling
    // is needed.
    extern "C" {
        fn morloc_eval_arena_enter(
            errmsg: *mut *mut std::ffi::c_char,
        ) -> *mut std::ffi::c_void;
        fn morloc_eval_arena_exit(handle: *mut std::ffi::c_void);
    }
    struct EvalArenaGuard {
        handle: *mut std::ffi::c_void,
    }
    impl Drop for EvalArenaGuard {
        fn drop(&mut self) {
            if !self.handle.is_null() {
                unsafe { morloc_eval_arena_exit(self.handle) };
            }
        }
    }
    let _arena = {
        let mut err: *mut std::ffi::c_char = std::ptr::null_mut();
        let h = unsafe { morloc_eval_arena_enter(&mut err) };
        if h.is_null() {
            let msg = process::take_c_errmsg(err)
                .unwrap_or_else(|| "unknown error".into());
            eprintln!("Error: eval arena could not be opened: {}", msg);
            process::clean_exit(1);
        }
        EvalArenaGuard { handle: h }
    };

    extern "C" {
        fn build_manifest_expr(
            json_str: *const std::ffi::c_char,
            errmsg: *mut *mut std::ffi::c_char,
        ) -> *mut std::ffi::c_void; // morloc_expression_t*
        fn morloc_eval(
            expr: *mut std::ffi::c_void,
            return_schema: *const morloc_runtime_types::cschema::CSchema,
            arg_voidstar: *const *mut u8,
            arg_schemas: *const *const morloc_runtime_types::cschema::CSchema,
            nargs: usize,
            errmsg: *mut *mut std::ffi::c_char,
        ) -> *mut std::ffi::c_void; // absptr_t
        fn parse_cli_data_argument_list(
            dest: *mut u8,
            args: *const *const std::ffi::c_void,
            n: usize,
            list_schema: *const morloc_runtime_types::cschema::CSchema,
            errmsg: *mut *mut std::ffi::c_char,
        ) -> *mut u8;
        fn initialize_positional(value: *mut std::ffi::c_char) -> *mut std::ffi::c_void;
        fn free_argument_t(arg: *mut std::ffi::c_void);
        fn get_morloc_data_packet_value(
            data: *const u8, schema: *const morloc_runtime_types::cschema::CSchema,
            errmsg: *mut *mut std::ffi::c_char,
        ) -> *mut u8;
        fn make_standard_data_packet(
            relptr: isize,
            schema: *const morloc_runtime_types::cschema::CSchema,
        ) -> *mut u8;
        fn abs2rel(ptr: *mut std::ffi::c_void, errmsg: *mut *mut std::ffi::c_char) -> isize;
    }

    // Build expression tree from manifest JSON
    let expr_json = match &cmd.expr {
        Some(v) => serde_json::to_string(v).unwrap_or_default(),
        None => {
            eprintln!("Error: pure command '{}' has no expression", cmd.name);
            process::clean_exit(1);
        }
    };
    let expr_c = std::ffi::CString::new(expr_json.as_str()).unwrap();
    let mut errmsg: *mut std::ffi::c_char = std::ptr::null_mut();
    let expr = unsafe { build_manifest_expr(expr_c.as_ptr(), &mut errmsg) };
    if expr.is_null() {
        let msg = process::take_c_errmsg(errmsg).unwrap_or_else(|| "unknown error".into());
        eprintln!("Error: failed to build expression: {}", msg);
        process::clean_exit(1);
    }

    // Parse return schema
    let return_schema = match parse_schema(&cmd.ret.schema) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error: failed to parse return schema '{}': {}", cmd.ret.schema, e);
            process::clean_exit(1);
        }
    };
    let c_return_schema = morloc_runtime_types::cschema::CSchema::from_rust(&return_schema);

    // The parsed `args` list and `cmd.args` are index-aligned 1:1 in
    // declaration order: parse_command_args pushes one ArgValue for
    // EVERY arg (including flags). The Haskell compiler emits one
    // schema per arg position too. Walk both lists in lockstep; for
    // flags, the schema_str() accessor returns None and we fall back
    // to the bool schema "b" so the wire format stays consistent.
    let mut c_arg_schemas: Vec<*const morloc_runtime_types::cschema::CSchema> = Vec::new();
    let mut c_arg_voidstars: Vec<*mut u8> = Vec::new();

    for (i, (arg_val, arg_def)) in args.iter().zip(cmd.args.iter()).enumerate() {
        let schema_str = arg_def.schema_str().unwrap_or("b");
        let schema = match parse_schema(schema_str) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Error: failed to parse arg schema #{}: {}", i, e);
                process::clean_exit(1);
            }
        };
        let c_schema = morloc_runtime_types::cschema::CSchema::from_rust(&schema);

        // Many-args go through the dedicated list-assembly C entry
        // point; everything else (Value / Null / Group) goes through
        // the scalar parse_cli_data_argument path. Group is treated
        // as Null at the pure path -- pure morloc expressions cannot
        // currently consume record-unrolled args (the schema covers
        // the whole record; group plumbing lives in the remote path).
        // Variadic args (many: true): compile-time validation rejects
        // every form/source modifier; only the IEEE 754 special-value
        // wrap runs per token.
        let mut many_override: Option<(Vec<String>, bool)> = None;
        if let ArgValue::Many { tokens, literal } = arg_val {
            let elem_is_float = element_schema_str(schema_str)
                .as_deref()
                .map(schema_is_float_scalar)
                .unwrap_or(false);
            if elem_is_float {
                let rewritten: Vec<String> = tokens
                    .iter()
                    .map(|t| maybe_float_special_to_json(t).unwrap_or_else(|| t.to_string()))
                    .collect();
                if rewritten.iter().zip(tokens.iter()).any(|(r, t)| r != t) {
                    many_override = Some((rewritten, *literal));
                }
            }
        }
        let c_pkt = match arg_val {
            ArgValue::Many { tokens, literal } => unsafe {
                let (toks_ref, lit) = match &many_override {
                    Some((rt, lit)) => (rt.as_slice(), *lit),
                    None => (tokens.as_slice(), *literal),
                };
                assemble_many_packet(
                    toks_ref, lit, c_schema, &mut errmsg,
                    initialize_positional, free_argument_t, parse_cli_data_argument_list,
                )
            },
            _ => {
                let raw_str = match arg_val {
                    ArgValue::Value(s) => s.clone(),
                    ArgValue::Null => "null".to_string(),
                    ArgValue::Group { .. } => "null".to_string(),
                    ArgValue::Many { .. } => unreachable!(),
                };
                let raw_str = if schema_is_float_scalar(schema_str) {
                    maybe_float_special_to_json(&raw_str).unwrap_or(raw_str)
                } else {
                    raw_str
                };
                let shape = ArgShape::from_arg(arg_def);
                let json_str = apply_shape_to_argv(raw_str, shape.as_ref(), i);
                let json_c = std::ffi::CString::new(json_str.as_str()).unwrap();
                let c_arg = unsafe {
                    initialize_positional(json_c.as_ptr() as *mut std::ffi::c_char)
                };
                let pkt = unsafe {
                    dispatch_one_arg(shape.as_ref(), c_arg, c_schema, &mut errmsg)
                };
                unsafe { free_argument_t(c_arg) };
                drop(json_c);
                pkt
            }
        };

        if c_pkt.is_null() {
            let msg = process::take_c_errmsg(errmsg).unwrap_or_else(|| "unknown error".into());
            crate::runlog::die_with_error(&format!("failed to parse argument #{}: {}", i, msg));
        }

        let voidstar = unsafe { get_morloc_data_packet_value(c_pkt, c_schema, &mut errmsg) };
        unsafe { libc::free(c_pkt as *mut std::ffi::c_void) };
        if voidstar.is_null() {
            let msg = process::take_c_errmsg(errmsg).unwrap_or_else(|| "unknown error".into());
            eprintln!("Error: failed to extract argument #{}: {}", i, msg);
            process::clean_exit(1);
        }

        c_arg_schemas.push(c_schema);
        c_arg_voidstars.push(voidstar);
    }

    // Call morloc_eval
    let result = unsafe {
        morloc_eval(
            expr,
            c_return_schema,
            c_arg_voidstars.as_ptr(),
            c_arg_schemas.as_ptr(),
            c_arg_voidstars.len(),
            &mut errmsg,
        )
    };

    if result.is_null() {
        let msg = process::take_c_errmsg(errmsg).unwrap_or_else(|| "unknown error".into());
        crate::runlog::die_with_error(&format!("evaluation failed: {}", msg));
    }

    // Convert result to relptr and make a data packet for printing
    let result_relptr = unsafe { abs2rel(result, &mut errmsg) };
    let result_packet = unsafe { make_standard_data_packet(result_relptr, c_return_schema) };

    if result_packet.is_null() {
        eprintln!("Error: failed to create result packet");
        process::clean_exit(1);
    }

    // Get packet as bytes for print_result_c
    extern "C" {
        fn morloc_packet_size(packet: *const u8, errmsg: *mut *mut std::ffi::c_char) -> usize;
    }
    let pkt_size = unsafe { morloc_packet_size(result_packet, &mut errmsg) };
    let pkt_bytes = unsafe { std::slice::from_raw_parts(result_packet, pkt_size).to_vec() };

    // Extract voidstar value from the result packet
    let result_ptr = unsafe { get_morloc_data_packet_value(pkt_bytes.as_ptr(), c_return_schema, &mut errmsg) };

    print_result_c(result_ptr, c_return_schema, &pkt_bytes, false, config);

    // Cleanup
    for cs in &c_arg_schemas {
        unsafe { morloc_runtime_types::cschema::CSchema::free(*cs as *mut morloc_runtime_types::cschema::CSchema) };
    }
    unsafe {
        morloc_runtime_types::cschema::CSchema::free(c_return_schema);
        libc::free(result_packet as *mut std::ffi::c_void);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn collapse_drops_adjacent_duplicates() {
        assert_eq!(collapse_duplicate_lines("A\nA\nB\nA\n"), "A\nB\nA\n");
    }

    #[test]
    fn collapse_preserves_non_adjacent() {
        assert_eq!(collapse_duplicate_lines("A\nB\nA\n"), "A\nB\nA\n");
    }

    #[test]
    fn collapse_collapses_long_run() {
        let input = "boom\n".repeat(20);
        assert_eq!(collapse_duplicate_lines(&input), "boom\n");
    }

    #[test]
    fn collapse_handles_no_trailing_newline() {
        assert_eq!(collapse_duplicate_lines("A\nA"), "A\nA");
    }

    #[test]
    fn collapse_handles_empty() {
        assert_eq!(collapse_duplicate_lines(""), "");
    }

    #[test]
    fn apply_checks_path_rejects_missing_file() {
        // Sanity guard: a non-`-` path that does not exist still
        // fails the readability probe.
        let checks = vec![Check::Path("r".into())];
        assert!(apply_checks("/no/such/path/should/exist/x", &checks).is_err());
    }

    #[test]
    fn substitute_stdio_dash_maps_to_dev_stdio() {
        // r -> /dev/stdin, w -> /dev/stdout; the substitution lets
        // the callee `fopen` the standard stream as a regular file.
        let r = vec![Check::Path("r".into())];
        assert_eq!(
            substitute_stdio_dash("-", &r).as_deref(),
            Some("/dev/stdin"),
        );
        let w = vec![Check::Path("w".into())];
        assert_eq!(
            substitute_stdio_dash("-", &w).as_deref(),
            Some("/dev/stdout"),
        );
    }

    #[test]
    fn substitute_stdio_dash_skips_exclusive_create() {
        // `x` requires the path to not exist; /dev/stdout does exist,
        // so substitution would immediately fail the check. Better to
        // leave `-` alone and let the check report the real problem.
        let x = vec![Check::Path("x".into())];
        assert!(substitute_stdio_dash("-", &x).is_none());
    }

    #[test]
    fn substitute_stdio_dash_leaves_non_dash_untouched() {
        // Anything other than the literal `-` is a real path; leave it.
        let r = vec![Check::Path("r".into())];
        assert!(substitute_stdio_dash("foo.txt", &r).is_none());
        assert!(substitute_stdio_dash("/dev/stdin", &r).is_none());
        assert!(substitute_stdio_dash("", &r).is_none());
    }

    #[test]
    fn substitute_stdio_dash_skips_mixed_and_unchecked() {
        // `rw` is ambiguous (stdin or stdout?); no substitution.
        // No check.path at all also means no substitution.
        let rw = vec![Check::Path("rw".into())];
        assert!(substitute_stdio_dash("-", &rw).is_none());
        let none: Vec<Check> = vec![];
        assert!(substitute_stdio_dash("-", &none).is_none());
    }

    #[test]
    fn apply_checks_w_accepts_creatable_path() {
        // Python-style `w`: (exists AND writable) OR
        // (does not exist AND parent writable). A fresh path under
        // /tmp should pass because /tmp is writable, even though the
        // path itself does not exist yet.
        let unique = format!(
            "/tmp/morloc_apply_checks_w_{}_{}.tmp",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .map(|d| d.as_nanos())
                .unwrap_or(0),
        );
        // Guard: the path must not exist for the test to be meaningful.
        let _ = std::fs::remove_file(&unique);
        let checks = vec![Check::Path("w".into())];
        let res = apply_checks(&unique, &checks);
        assert!(res.is_ok(), "expected w to accept creatable path, got {:?}", res);
    }

    #[test]
    fn apply_checks_x_rejects_existing_path() {
        // `x` demands non-existence. A path that clearly exists
        // (this very source file's parent) must fail.
        let checks = vec![Check::Path("x".into())];
        assert!(apply_checks("/tmp", &checks).is_err());
    }

    #[test]
    fn apply_checks_rejects_unknown_mode() {
        // The old `c` letter was renamed to `x`; ensure the runtime
        // rejects the retired name with a clear error instead of
        // silently no-oping.
        let checks = vec![Check::Path("c".into())];
        let res = apply_checks("/tmp/anything", &checks);
        assert!(res.is_err());
        // And any unsatisfiable combo from the old grammar must also fail.
        let checks = vec![Check::Path("rwc".into())];
        assert!(apply_checks("/tmp/anything", &checks).is_err());
    }
}
