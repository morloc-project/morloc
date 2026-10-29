//! Command dispatch: run a parsed command, by sending a call packet to its
//! pool (remote command) or by evaluating its expression in-process (pure
//! command), and print the result.

use crate::manifest::{Check, Command, Manifest, SourceAtom};
pub use morloc_manifest::cli_value::{
    build_list_config_json, is_float_list, is_float_scalar, maybe_float_special_to_json,
    shaped_dispatch_codes, ArgShape,
};
use crate::process::{self, PoolSocket};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OutputFormat {
    Json,
    /// JSON-lines: one line per list element (per row of a table); a
    /// scalar is one line.
    Jsonl,
    MessagePack,
    VoidStar,
    Packet,
    /// Apache Arrow IPC file. Table results only.
    Arrow,
    /// Apache Parquet file. Table results only.
    Parquet,
    /// Tab-separated values: a table, or a list of records (CLI-17).
    Tsv,
    /// Comma-separated values: a table, or a list of records (CLI-17).
    Csv,
    /// Verbatim bytes: string or byte-vector bodies with no framing or JSON
    /// quoting.
    Raw,
}

#[derive(Debug, Clone)]
pub struct NexusConfig {
    pub print_flag: bool,
    pub keep_null: bool,
    pub packet_path: Option<String>,
    pub socket_base: Option<String>,
    pub output_path: Option<String>,
    pub output_format: OutputFormat,
    pub daemon_flag: bool,
    /// Serve the program as an MCP server (the `mcp` subcommand).
    pub mcp_flag: bool,
    pub router_flag: bool,
    pub unix_socket_path: Option<String>,
    pub tcp_port: Option<i32>,
    pub http_port: Option<i32>,
    /// Bind address for MCP-over-HTTP; `None` is loopback.
    pub mcp_http_host: Option<String>,
    /// Bearer token required on every MCP-over-HTTP request, if any.
    pub mcp_auth_token: Option<String>,
    /// Permit a non-loopback MCP-over-HTTP bind with no token.
    pub mcp_allow_no_auth: bool,
    pub port_file_path: Option<String>,
    pub fdb_path: Option<String>,
    pub eval_timeout: i32,
    /// Sandbox served eval: no IO intrinsics, imports limited to
    /// `eval_allowed_modules`.
    pub eval_sandbox: bool,
    /// Modules sandboxed eval may import (`--eval-allowed-modules`).
    pub eval_allowed_modules: Option<String>,
    /// Expose eval as a capability (an `eval` MCP tool and `/eval` route).
    pub eval_enabled: bool,
    /// Serve eval without a bearer token.
    pub eval_allow_no_auth: bool,
    /// `--log-dir`: base directory of the per-run directory, log tee and
    /// `summary.json`; published as `MORLOC_LOG_DIR`.
    pub log_dir: Option<String>,
    /// `--summary`: the `summary.json` path, with or without `log_dir`;
    /// published as `MORLOC_SUMMARY`.
    pub summary_path: Option<String>,
    /// Suppress morloc's own log lines; published as `MORLOC_QUIET`.
    pub quiet: bool,
    /// Cap on debug-cache disk writes per dispatch; `None` is the runtime
    /// default, 0 is no cap.
    pub debug_cache_depth: Option<u64>,
    /// Per-argument byte cap on debug-cache writes.
    pub debug_cache_max: Option<u64>,
    /// Per-midx cap on debug-cache entries; `None` is the runtime default,
    /// 0 is no cap.
    pub debug_recursion_cap: Option<u32>,
    /// zstd level for packet output; 0 is no compression.
    pub compression_level: u8,
    /// `-z` as given, if it was; published to pools as
    /// `MORLOC_STDOUT_COMPRESSION_LEVEL`.
    pub stdout_compression: Option<u8>,
    /// Every served module: the union of `mcp_programs` and `api_programs`.
    pub programs: Vec<String>,
    /// Modules exposed over the MCP adapter (/mcp). Subset of `programs`.
    pub mcp_programs: Vec<String>,
    /// Modules exposed over the JSON API adapter (/call). Subset of `programs`.
    pub api_programs: Vec<String>,
    /// The part this process plays in a multi-output run, if any.
    pub child: ChildMode,
}

/// The part a nexus process plays in a multi-output run (see
/// `orchestrate`).
#[derive(Clone, Debug, Default)]
pub enum ChildMode {
    #[default]
    None,
    /// Run the parent command once, saving its output (and the arguments
    /// the actions refer to) under `dir`; with `tee`, also write the output
    /// to stdout as an ordinary run would.
    Stage { dir: String, args: Vec<usize>, tee: bool },
    /// Run one internal command on packet files.
    Replay { cmd: String, inputs: Vec<String> },
}

impl Default for NexusConfig {
    fn default() -> Self {
        NexusConfig {
            child: ChildMode::None,
            print_flag: false,
            keep_null: false,
            packet_path: None,
            socket_base: None,
            output_path: None,
            output_format: OutputFormat::Json,
            daemon_flag: false,
            mcp_flag: false,
            router_flag: false,
            unix_socket_path: None,
            tcp_port: None,
            http_port: None,
            mcp_http_host: None,
            mcp_auth_token: None,
            mcp_allow_no_auth: false,
            port_file_path: None,
            fdb_path: None,
            eval_timeout: 30,
            eval_sandbox: false,
            eval_allowed_modules: None,
            eval_enabled: false,
            eval_allow_no_auth: false,
            log_dir: None,
            summary_path: None,
            quiet: false,
            debug_cache_depth: None,
            debug_cache_max: None,
            debug_recursion_cap: None,
            compression_level: 0,
            stdout_compression: None,
            programs: Vec::new(),
            mcp_programs: Vec::new(),
            api_programs: Vec::new(),
        }
    }
}

/// Print a run of adjacent identical lines in an error message once, followed
/// by the number of repeats (DIAG-5), so a trace through deep recursion does
/// not bury the root cause. Non-adjacent duplicates are kept.
fn collapse_duplicate_lines(msg: &str) -> String {
    let mut out = String::with_capacity(msg.len());
    let mut lines = msg.split('\n').peekable();
    while let Some(line) = lines.next() {
        out.push_str(line);
        let mut repeats = 0usize;
        while lines.peek() == Some(&line) {
            lines.next();
            repeats += 1;
        }
        if repeats > 0 {
            let plural = if repeats == 1 { "" } else { "s" };
            out.push_str(&format!("\n  [previous line repeated {repeats} more time{plural}]"));
        }
        if lines.peek().is_some() {
            out.push('\n');
        }
    }
    out
}

const STDOUT_BUF_CAPACITY: usize = 64 * 1024;

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

/// Report a failed pool communication, with the pool's exit status when
/// known, and exit.
fn die_with_pool_error(
    socket: &PoolSocket,
    pool_index: usize,
    context: &str,
    comm_err: &std::io::Error,
) -> ! {
    // A connection dropped by the teardown is not a pool fault; reporting
    // it would replace the run's real outcome (e.g. exit 141 for a closed
    // stdout) with a spurious "pool crashed".
    if process::teardown_in_progress() {
        process::park_until_exit();
    }

    // Best effort: let a dying pool's traceback reach stderr before the
    // teardown kills it.
    std::thread::sleep(std::time::Duration::from_millis(100));

    // The peer closing the socket surfaces as UnexpectedEof ("failed to
    // fill whole buffer") or BrokenPipe; say what that means instead.
    let comm_msg: String = match comm_err.kind() {
        std::io::ErrorKind::UnexpectedEof | std::io::ErrorKind::BrokenPipe => format!(
            "pool '{}' closed the connection before sending a response \
             (likely crashed; see stderr above for any traceback)",
            socket.lang
        ),
        _ => format!("{}", comm_err),
    };

    if matches!(comm_err.kind(), std::io::ErrorKind::UnexpectedEof | std::io::ErrorKind::BrokenPipe) {
        process::wait_for_pool_exit(pool_index, std::time::Duration::from_millis(500));
    }
    let mut full = format!("{}: {}", context, comm_msg);
    if let Some(info) = process::pool_death_info(pool_index) {
        full.push_str(&format!("\nPool '{}' {}", socket.lang, info));
    }
    crate::runlog::die_with_error(&full);
}


/// `s` as a JSON string.
pub fn quoted(s: &str) -> String {
    let escaped = serde_json::to_string(s).unwrap_or_else(|_| format!("\"{}\"", s));
    escaped
}

/// The list packet for a `@many` slot's tokens. `literal` JSON-quotes each
/// token so it is read as inline content rather than as a file path.
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

/// Run a command on its parsed arguments.
pub fn dispatch_command_parsed(
    parsed_args: Vec<ArgValue>,
    config: &NexusConfig,
    manifest: &Manifest,
    cmd: &Command,
    sockets: &mut [PoolSocket],
) {
    crate::runlog::record_command(&cmd.name);
    crate::runlog::emit_prologue();

    if !cmd.is_pure() {
        if let Err(e) = process::start_daemons(sockets, &cmd.needed_pools) {
            eprintln!("Error: {}", e);
            process::clean_exit(1);
        }
    }

    if cmd.is_pure() {
        run_pure_command(cmd, &parsed_args, config, manifest);
    } else {
        let target_pool = &manifest.pools[cmd.pool_index];
        let skip = manifest.unsafe_skip_null_check
            || morloc_runtime_types::null_check::env_skip_null_check();
        if !skip && !target_pool.allow_string_null {
            for (i, av) in parsed_args.iter().enumerate() {
                let payload = match av {
                    ArgValue::Value(s) => Some(s.as_str()),
                    ArgValue::Embed { value, .. } | ArgValue::AsParent { value, .. } => match value.as_ref() {
                        ArgValue::Value(s) => Some(s.as_str()),
                        _ => None,
                    },
                    _ => None,
                };
                if let Some(s) = payload {
                    // A value that is not JSON is a bare word, which
                    // cannot carry a NUL.
                    if let Ok(Some(p)) = morloc_runtime_types::null_check::first_null_in_json_text(s) {
                        eprintln!(
                            "Error: {} does not support embedded NUL bytes in strings (at args[{}]{})",
                            target_pool.lang, i, p
                        );
                        process::clean_exit(1);
                    }
                }
            }
        }
        run_remote_command(cmd, &parsed_args, sockets, config, manifest);
    }
}

/// The device path an argv `-` stands for in an argument that names a file,
/// or `None` to keep the token. Under `check.path`, `r` gives `/dev/stdin`
/// and `w` gives `/dev/stdout`; `x` (which `/dev/stdout` would fail) and
/// `rw` (ambiguous) give `None`. A `@source file` argument, which the auto
/// classifier does not see, gives `/dev/stdin`.
pub fn substitute_stdio_dash(
    argv: &str,
    checks: &[Check],
    source: SourceAtom,
) -> Option<String> {
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
    if source == SourceAtom::File {
        return Some("/dev/stdin".to_string());
    }
    None
}

/// Check the raw argv token against `checks`, failing on the first that
/// does not hold. Path modes:
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

fn check_w(argv: &str) -> Result<(), String> {
    let cp = c_path_of(argv)?;
    let rc = unsafe { libc::access(cp.as_ptr(), libc::W_OK) };
    if rc == 0 {
        return Ok(());
    }
    // Only a missing path may pass as creatable; any other failure is
    // reported as is.
    let err = std::io::Error::last_os_error();
    if err.raw_os_error() != Some(libc::ENOENT) {
        return Err(format!(
            "check.path: w requires path '{}' to be writable ({})",
            argv, err
        ));
    }
    check_parent_writable(argv, "w")
}

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

/// An argv token after `-` substitution, `@check.path` validation and
/// `@literal` quoting. A failed check ends the run, prefixed by
/// `error_prefix`.
pub fn preprocess_cli_value(
    val: String,
    checks: &[Check],
    source: SourceAtom,
    quoted_flag: bool,
    error_prefix: &str,
) -> String {
    let v = substitute_stdio_dash(&val, checks, source).unwrap_or(val);
    // A substituted stream is not a user-named path, and access() on it
    // can fail spuriously.
    let is_stdio_sentinel = v == "/dev/stdin" || v == "/dev/stdout";
    if !is_stdio_sentinel {
        if let Err(e) = apply_checks(&v, checks) {
            crate::runlog::die_with_error(&format!("{}: {}", error_prefix, e));
        }
    }
    if quoted_flag { quoted(&v) } else { v }
}

/// `morloc_manifest::cli_value::shape_argv` for argument `i`, ending the
/// run on a malformed token.
pub fn apply_shape_to_argv(raw: String, shape: Option<&ArgShape>, i: usize) -> String {
    match morloc_manifest::cli_value::shape_argv(raw, shape) {
        Ok(text) => text,
        Err(e) => crate::runlog::die_with_error(&format!("argument #{}: {}", i, e)),
    }
}

/// The parsed CLI value of one manifest arg slot.
#[derive(Debug)]
pub enum ArgValue {
    Value(String),
    Null,
    /// A record argument: its whole value, each field, each field's default.
    Group {
        grp_val: Option<String>,
        fields: Vec<Option<String>>,
        defaults: Vec<Option<String>>,
    },
    /// JSON text sent as is: no shape, check or quoting applies.
    Json(String),
    /// An argument read by one of its `@parse` formats: the format's index,
    /// the path handed to the handler (`-` replaced by `/dev/stdin`), and the
    /// path as the user wrote it. Rewritten into the parse entry's slots
    /// before dispatch.
    Parsed { format: usize, path: String, shown: String },
    /// An argument of a `@parse` entry that no format read: `value` is loaded
    /// as argument `arg` of command `parent` and sent as its packet's bytes.
    Embed { value: Box<ArgValue>, parent: usize, arg: usize },
    /// An argument of a `@parse` entry read as argument `arg` of command
    /// `parent`, whose definition carries its source, form and checks.
    AsParent { value: Box<ArgValue>, parent: usize, arg: usize },
    /// A `@many` argument's tokens, read as one list; `literal` is the
    /// `@literal` flag.
    Many { tokens: Vec<String>, literal: bool },
}


/// One command-line token, read as an argument of wire schema `schema`
/// would be, rendered as JSON.
pub fn cli_token_to_json(token: &str, schema: &str) -> Result<String, String> {
    extern "C" {
        fn cli_token_to_json(
            token: *const std::ffi::c_char,
            schema: *const std::ffi::c_char,
            errmsg: *mut *mut std::ffi::c_char,
        ) -> *mut std::ffi::c_char;
        fn free(p: *mut std::ffi::c_void);
    }
    let tok_c = std::ffi::CString::new(token).map_err(|e| e.to_string())?;
    let sch_c = std::ffi::CString::new(schema).map_err(|e| e.to_string())?;
    let mut errmsg: *mut std::ffi::c_char = std::ptr::null_mut();
    unsafe {
        let out = cli_token_to_json(tok_c.as_ptr(), sch_c.as_ptr(), &mut errmsg);
        if out.is_null() {
            let msg = if errmsg.is_null() {
                "could not read value".to_string()
            } else {
                let m = std::ffi::CStr::from_ptr(errmsg).to_string_lossy().into_owned();
                free(errmsg as *mut std::ffi::c_void);
                m
            };
            return Err(msg);
        }
        let js = std::ffi::CStr::from_ptr(out).to_string_lossy().into_owned();
        free(out as *mut std::ffi::c_void);
        Ok(js)
    }
}

/// Parse one value with the runtime, shaped when `shaped_dispatch_codes`
/// gives codes. Returns null with `errmsg` set on error.
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

fn run_remote_command(
    cmd: &Command,
    args: &[ArgValue],
    sockets: &[PoolSocket],
    config: &NexusConfig,
    manifest: &Manifest,
) {
    use morloc_runtime_types::packet;
    use morloc_runtime_types::schema::parse_schema;
    use std::io::{Read, Write};
    use std::os::unix::net::UnixStream;

    extern "C" {
        fn morloc_packet_size(packet: *const u8, errmsg: *mut *mut std::ffi::c_char) -> usize;
        fn make_morloc_local_call_packet(
            midx: u32, arg_packets: *const *const u8, nargs: usize,
            errmsg: *mut *mut std::ffi::c_char,
        ) -> *mut u8;
        fn get_morloc_data_packet_value(
            data: *const u8, schema: *const morloc_runtime_types::cschema::CSchema,
            errmsg: *mut *mut std::ffi::c_char,
        ) -> *mut u8;
        fn shfree(ptr: *mut std::ffi::c_void, errmsg: *mut *mut std::ffi::c_char) -> bool;
    }

    let socket = &sockets[cmd.pool_index];

    let return_schema = match parse_schema(&cmd.ret.schema) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error: failed to parse return schema '{}': {}", cmd.ret.schema, e);
            process::clean_exit(1);
        }
    };

    let mut arg_packets: Vec<(*mut u8, usize)> = Vec::new();
    for (i, (arg_val, arg_def)) in args.iter().zip(cmd.args.iter()).enumerate() {
        let c_pkt = build_arg_packet(i, arg_val, arg_def, manifest);
        let mut errmsg: *mut std::ffi::c_char = std::ptr::null_mut();
        let pkt_size = unsafe { morloc_packet_size(c_pkt, &mut errmsg) };
        if !link_ifile_arg(i, arg_val, arg_def) {
            crate::stage::save_arg(&cmd.name, i, c_pkt, pkt_size);
        }
        arg_packets.push((c_pkt, pkt_size));
    }

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

    let mut hdr = [0u8; 32];
    if let Err(e) = stream.read_exact(&mut hdr) {
        die_with_pool_error(
            socket,
            cmd.pool_index,
            &format!(
                "failed to read response header from pool '{}' while running '{}' (mid={})",
                socket.lang, cmd.name, cmd.mid
            ),
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
    // A shared-memory result's donated reference is this process's now (SHM-8).
    extern "C" {
        fn morloc_inherit_reply(packet: *const u8);
    }
    unsafe { morloc_inherit_reply(full_packet.as_ptr()) };

    if reply_is_pipe_closed(&full_packet) {
        process::exit_broken_pipe();
    }
    match packet::get_error_message(&full_packet) {
        Ok(Some(err_msg)) => {
            if let Some(report) = crate::parse_arg::failure_report(&err_msg) {
                crate::runlog::record_error(&report);
                eprintln!("{}", report);
                process::clean_exit(1);
            }
            // summary.json records the message without the "run failed" line.
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

    die_on_top_level_err(result_ptr, c_schema);

    // A table-typed result is a block whatever packet form carried it
    // (a reference, or a file a cached result was read back from).
    let is_arrow = return_schema.serial_type == morloc_runtime_types::schema::SerialType::Table;

    print_result_c(result_ptr, c_schema, &full_packet, is_arrow, config);

    unsafe {
        let mut ferr: *mut std::ffi::c_char = std::ptr::null_mut();
        shfree(result_ptr as *mut std::ffi::c_void, &mut ferr);
        if !ferr.is_null() {
            libc::free(ferr as *mut std::ffi::c_void);
        }
    }
    unsafe { morloc_runtime_types::cschema::CSchema::free(c_schema) };
}

/// Save parent argument `i` (0-based) when it is an `IFile` given by path,
/// as a link to the file (see `stage::link_parent_arg`). Returns whether it
/// was one.
fn link_ifile_arg(i: usize, arg_val: &ArgValue, arg_def: &crate::manifest::Arg) -> bool {
    match (arg_val, arg_def.general_schema_str()) {
        (ArgValue::Value(token), Some("F")) => {
            // A quoted argument holds its path as a JSON string.
            let path = serde_json::from_str::<String>(token).unwrap_or_else(|_| token.clone());
            crate::stage::link_parent_arg(i, &path);
            true
        }
        _ => false,
    }
}

/// An `IFile` argument given by path must name an existing file. It is
/// opened only when the command runs, where an error no longer says which
/// argument or path it came from.
fn check_ifile_arg(arg_val: &ArgValue, arg_def: &crate::manifest::Arg) -> Result<(), String> {
    let (ArgValue::Value(token), Some("F")) = (arg_val, arg_def.general_schema_str()) else {
        return Ok(());
    };
    let path = serde_json::from_str::<String>(token).unwrap_or_else(|_| token.clone());
    if path == "-" {
        return Err(
            "an IFile cannot be read from stdin ('-'): it needs a seekable file. \
             Give a file path, or take the argument as an IStream."
                .to_string(),
        );
    }
    std::fs::metadata(&path)
        .map(|_| ())
        .map_err(|e| format!("cannot open IFile '{}': {}", path, e))
}

/// Save a parent-layout argument packet at the parent's position `arg`
/// (0-based) in a stage. No-op outside one.
fn save_parent_packet(arg: usize, pkt: *mut u8) {
    extern "C" {
        fn morloc_packet_size(packet: *const u8, errmsg: *mut *mut std::ffi::c_char) -> usize;
    }
    if pkt.is_null() || crate::stage::arg_target(arg + 1).is_none() {
        return;
    }
    let mut errmsg: *mut std::ffi::c_char = std::ptr::null_mut();
    let size = unsafe { morloc_packet_size(pkt, &mut errmsg) };
    if let Some(msg) = process::take_c_errmsg(errmsg) {
        crate::runlog::die_with_error(&format!("saving argument {}: {}", arg + 1, msg));
    }
    crate::stage::save_parent_arg(arg, pkt, size);
}

/// Build argument `i`'s packet from its CLI value, reading the value as
/// `arg_def` describes. Exits with a diagnostic when the value cannot be read.
fn build_arg_packet(
    i: usize,
    arg_val: &ArgValue,
    arg_def: &crate::manifest::Arg,
    manifest: &Manifest,
) -> *mut u8 {
    use morloc_runtime_types::schema::parse_schema;
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
        fn make_data_packet_from_mpk(
            mpk: *const std::ffi::c_char,
            mpk_size: usize,
            schema: *const morloc_runtime_types::cschema::CSchema,
        ) -> *mut u8;
    }
    // Read as the parent command's argument, whose definition carries the
    // shape, and saved at the parent's position for an action to refer to.
    if let ArgValue::AsParent { value, parent, arg } = arg_val {
        let parent_def = &manifest.commands[*parent].args[*arg];
        let pkt = build_arg_packet(i, value, parent_def, manifest);
        if !link_ifile_arg(*arg, value, parent_def) {
            save_parent_packet(*arg, pkt);
        }
        return pkt;
    }
    if let Err(msg) = check_ifile_arg(arg_val, arg_def) {
        crate::runlog::die_with_error(&format!("failed to parse argument #{}: {}", i, msg));
    }
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

        let mut many_override: Option<(Vec<String>, bool)> = None;
        if let ArgValue::Many { tokens, literal } = arg_val {
            let elem_is_float = is_float_list(&schema);
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
            ArgValue::Json(text) => unsafe { json_arg_packet(text, c_schema, &mut errmsg) },
            ArgValue::Embed { value, parent, arg } => {
                // Loaded as the parent command loads it; its packet travels
                // as a `[U8]` for the entry to decode.
                let parent_def = &manifest.commands[*parent].args[*arg];
                let inner = build_arg_packet(i, value, parent_def, manifest);
                if !link_ifile_arg(*arg, value, parent_def) {
                    save_parent_packet(*arg, inner);
                }
                let mut size_err: *mut std::ffi::c_char = std::ptr::null_mut();
                let size = unsafe { morloc_packet_size(inner, &mut size_err) };
                if let Some(msg) = process::take_c_errmsg(size_err) {
                    crate::runlog::die_with_error(&format!("failed to parse argument #{}: {}", i, msg));
                }
                let mpk = msgpack_u8_array(unsafe { std::slice::from_raw_parts(inner, size) });
                unsafe { libc::free(inner as *mut std::ffi::c_void) };
                unsafe { make_data_packet_from_mpk(mpk.as_ptr() as *const std::ffi::c_char, mpk.len(), c_schema) }
            }
            ArgValue::AsParent { .. } => unreachable!("handled before any schema work"),
            ArgValue::Parsed { .. } => {
                unreachable!("a parsed argument is rewritten for the parse entry before dispatch")
            }
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
                // A null has no token, so it is read without the arg's shape;
                // under `@source file` the shape would open a file named `null`.
                let (raw_str, shape) = match arg_val {
                    ArgValue::Value(s) => (s.clone(), ArgShape::from_arg(arg_def)),
                    ArgValue::Null => ("null".to_string(), None),
                    _ => unreachable!(),
                };
                let raw_str = if is_float_scalar(&schema) {
                    maybe_float_special_to_json(&raw_str).unwrap_or(raw_str)
                } else {
                    raw_str
                };
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

    c_pkt
}

/// `bytes` as a msgpack array of unsigned integers, the `[U8]` wire value.
fn msgpack_u8_array(bytes: &[u8]) -> Vec<u8> {
    let n = bytes.len();
    let mut out = Vec::with_capacity(5 + 2 * n);
    if n < 16 {
        out.push(0x90 | n as u8);
    } else if n <= u16::MAX as usize {
        out.push(0xdc);
        out.extend_from_slice(&(n as u16).to_be_bytes());
    } else {
        out.push(0xdd);
        out.extend_from_slice(&(n as u32).to_be_bytes());
    }
    for &b in bytes {
        if b >= 0x80 {
            out.push(0xcc);
        }
        out.push(b);
    }
    out
}

/// A packet for JSON text read with no shape, check, quoting or source
/// classification.
unsafe fn json_arg_packet(
    text: &str,
    c_schema: *mut morloc_runtime_types::cschema::CSchema,
    errmsg: &mut *mut std::ffi::c_char,
) -> *mut u8 {
    extern "C" {
        fn initialize_positional(value: *mut std::ffi::c_char) -> *mut std::ffi::c_void;
        fn free_argument_t(arg: *mut std::ffi::c_void);
    }
    let json_c = std::ffi::CString::new(text).unwrap();
    let c_arg = initialize_positional(json_c.as_ptr() as *mut std::ffi::c_char);
    let pkt = dispatch_one_arg(None, c_arg, c_schema, errmsg);
    free_argument_t(c_arg);
    pkt
}

/// End the run as failed, printing the whole value as JSON, when the result
/// is a top-level `Err` arm.
fn die_on_top_level_err(
    result_ptr: *mut u8,
    c_schema: *const morloc_runtime_types::cschema::CSchema,
) {
    extern "C" {
        fn morloc_top_level_err(
            voidstar: *const std::ffi::c_void,
            schema: *const morloc_runtime_types::cschema::CSchema,
        ) -> *mut std::ffi::c_char;
    }
    let out = unsafe { morloc_top_level_err(result_ptr as *const std::ffi::c_void, c_schema) };
    if out.is_null() {
        return;
    }
    let rendered = unsafe {
        let text = std::ffi::CStr::from_ptr(out).to_string_lossy().into_owned();
        libc::free(out as *mut std::ffi::c_void);
        text
    };
    crate::runlog::record_error(&rendered);
    eprintln!("Error: run failed\n{}", rendered);
    // No CSchema free: clean_exit does not return.
    process::clean_exit(1);
}

/// Print a result in the configured output format.
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
        ) -> i32;
        fn print_arrow_as_table(
            data: *const std::ffi::c_void,
            errmsg: *mut *mut std::ffi::c_char,
        ) -> i32;
        fn print_arrow_as_jsonl(
            data: *const std::ffi::c_void,
            errmsg: *mut *mut std::ffi::c_char,
        ) -> i32;
        fn pack_with_schema(
            mlc: *const std::ffi::c_void,
            schema: *const morloc_runtime_types::cschema::CSchema,
            mpkptr: *mut *mut std::ffi::c_char,
            mpk_size: *mut usize,
            errmsg: *mut *mut std::ffi::c_char,
        ) -> i32;
    }

    use morloc_runtime_types::{PRINT_RESULT_OK, PRINT_RESULT_PIPE_CLOSED};

    // A command that streamed to stdout and then returns a value prints the
    // completed stream first.
    crate::custody_finish(true);
    crate::stdio_server::finish_stdout();

    // A stage saves the result for the actions, and prints it only when
    // stdout wants the command's own output.
    crate::stage::save_value(full_packet, ptr, schema);
    if !crate::stage::stdout_on() {
        process::clean_exit(0);
    }

    let mut errmsg: *mut std::ffi::c_char = std::ptr::null_mut();

    match config.output_format {
        OutputFormat::Json => {
            if is_arrow {
                let rc = unsafe {
                    if config.print_flag {
                        print_arrow_as_table(ptr as *const std::ffi::c_void, &mut errmsg)
                    } else {
                        print_arrow_as_json(ptr as *const std::ffi::c_void, &mut errmsg)
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
                let bytes = unsafe { std::slice::from_raw_parts(mpk_ptr as *const u8, mpk_size) };
                write_stdout_or_exit(bytes, "msgpack");
            }
            if !mpk_ptr.is_null() {
                unsafe { libc::free(mpk_ptr as *mut std::ffi::c_void) };
            }
        }
        OutputFormat::VoidStar => {
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
            if full_packet.is_empty() {
                // The caller holds only a voidstar.
                extern "C" {
                    fn mlc_write_voidstar_data_packet_to_fd(
                        data: *const std::ffi::c_void,
                        schema: *const morloc_runtime_types::cschema::CSchema,
                        level: u8,
                        fd: libc::c_int,
                        errmsg: *mut *mut std::ffi::c_char,
                    ) -> i64;
                }
                if config.print_flag {
                    eprintln!("Error: a hex dump of `-f voidstar` output needs the result packet");
                    process::clean_exit(1);
                }
                use std::io::Write;
                let _ = std::io::stdout().lock().flush();
                let mut verr: *mut std::ffi::c_char = std::ptr::null_mut();
                let n = unsafe {
                    mlc_write_voidstar_data_packet_to_fd(ptr as *const std::ffi::c_void, schema, 0, libc::STDOUT_FILENO, &mut verr)
                };
                if n == morloc_runtime_types::PACKET_TO_FD_PIPE_CLOSED {
                    process::exit_broken_pipe();
                }
                if n < 0 {
                    let msg = process::take_c_errmsg(verr).unwrap_or_else(|| "unknown error".into());
                    eprintln!("Error: voidstar output failed: {}", msg);
                    process::clean_exit(1);
                }
            } else if config.print_flag {
                if let Err(e) = write_hex_dump_grouped(&full_packet) {
                    if e.kind() == std::io::ErrorKind::BrokenPipe {
                        process::exit_broken_pipe();
                    }
                    eprintln!("Error: voidstar hex dump failed: {}", e);
                    process::clean_exit(1);
                }
            } else {
                let mut errmsg2: *mut std::ffi::c_char = std::ptr::null_mut();
                match unsafe { print_morloc_data_packet(full_packet.as_ptr(), schema, &mut errmsg2) } {
                    0 => {}
                    PRINT_RESULT_PIPE_CLOSED => process::exit_broken_pipe(),
                    _ => {
                        let msg = process::take_c_errmsg(errmsg2).unwrap_or_else(|| "unknown error".into());
                        eprintln!("Error: voidstar output failed: {}", msg);
                        process::clean_exit(1);
                    }
                }
            }
        }
        OutputFormat::Packet => {
            // A trailing Unit packet would land past a stream written to
            // stdout.
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
            // An empty `full_packet` means the caller has only a voidstar.
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
            if n == morloc_runtime_types::PACKET_TO_FD_PIPE_CLOSED {
                process::exit_broken_pipe();
            }
            if n < 0 {
                let msg = process::take_c_errmsg(nerr)
                    .unwrap_or_else(|| "unknown error".into());
                eprintln!("Error: packet emission failed: {}", msg);
                process::clean_exit(1);
            }
        }
        OutputFormat::Jsonl => {
            if is_arrow {
                let rc = unsafe { print_arrow_as_jsonl(ptr as *const std::ffi::c_void, &mut errmsg) };
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
                return;
            }
            if !config.keep_null
                && unsafe { morloc_runtime_types::is_top_null(schema, ptr) }
            {
                return;
            }
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
        OutputFormat::Arrow | OutputFormat::Parquet | OutputFormat::Csv | OutputFormat::Tsv => {
            // A table block is serialized from the block; a list of
            // records may also be written as CSV or TSV (CLI-17).
            let rows_csv = !is_arrow
                && matches!(config.output_format, OutputFormat::Csv | OutputFormat::Tsv);
            if !is_arrow && !rows_csv {
                eprintln!(
                    "Error: --format=arrow|parquet requires a Table return type"
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
                fn write_rows_csv_to_buffer(
                    data: *const std::ffi::c_void,
                    schema: *const std::ffi::c_void,
                    delimiter: u8,
                    out_buf: *mut *mut u8,
                    out_len: *mut usize,
                    errmsg: *mut *mut std::ffi::c_char,
                ) -> i32;
            }
            let mut buf: *mut u8 = std::ptr::null_mut();
            let mut len: usize = 0;
            let mut err: *mut std::ffi::c_char = std::ptr::null_mut();
            let delimiter = if matches!(config.output_format, OutputFormat::Tsv) { b'\t' } else { b',' };
            let rc = unsafe {
                match config.output_format {
                    _ if rows_csv => write_rows_csv_to_buffer(
                        ptr as *const std::ffi::c_void, schema as *const std::ffi::c_void,
                        delimiter, &mut buf, &mut len, &mut err,
                    ),
                    OutputFormat::Arrow => write_arrow_ipc_to_buffer(
                        ptr as *const std::ffi::c_void, &mut buf, &mut len, &mut err,
                    ),
                    OutputFormat::Parquet => write_parquet_to_buffer(
                        ptr as *const std::ffi::c_void, &mut buf, &mut len, &mut err,
                    ),
                    OutputFormat::Tsv => write_csv_to_buffer(
                        ptr as *const std::ffi::c_void, b'\t', &mut buf, &mut len, &mut err,
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
            let bytes = unsafe { std::slice::from_raw_parts(buf, len) };
            write_stdout_or_exit(bytes, "table");
            unsafe { libc::free(buf as *mut std::ffi::c_void) };
        }
    }
    process::clean_exit(0);
}

/// Write `bytes` to stdout and flush; a closed pipe ends the run with its
/// status, any other failure with 1.
fn write_stdout_or_exit(bytes: &[u8], what: &str) {
    use std::io::Write;
    let mut out = std::io::stdout().lock();
    match out.write_all(bytes).and_then(|_| out.flush()) {
        Ok(()) => {}
        Err(e) if e.kind() == std::io::ErrorKind::BrokenPipe => process::exit_broken_pipe(),
        Err(e) => {
            eprintln!("Error: writing the {what} output failed: {e}");
            process::clean_exit(1);
        }
    }
}

fn run_pure_command(cmd: &Command, args: &[ArgValue], config: &NexusConfig, manifest: &Manifest) {
    use morloc_runtime_types::schema::parse_schema;

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

    let return_schema = match parse_schema(&cmd.ret.schema) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error: failed to parse return schema '{}': {}", cmd.ret.schema, e);
            process::clean_exit(1);
        }
    };
    let c_return_schema = morloc_runtime_types::cschema::CSchema::from_rust(&return_schema);

    let mut c_arg_schemas: Vec<*const morloc_runtime_types::cschema::CSchema> = Vec::new();
    let mut c_arg_voidstars: Vec<*mut u8> = Vec::new();

    for (i, (arg_val, arg_def)) in args.iter().zip(cmd.args.iter()).enumerate() {
        if let Err(msg) = check_ifile_arg(arg_val, arg_def) {
            crate::runlog::die_with_error(&format!("failed to parse argument #{}: {}", i, msg));
        }
        let schema_str = arg_def.schema_str().unwrap_or("b");
        let schema = match parse_schema(schema_str) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Error: failed to parse arg schema #{}: {}", i, e);
                process::clean_exit(1);
            }
        };
        let c_schema = morloc_runtime_types::cschema::CSchema::from_rust(&schema);

        let mut many_override: Option<(Vec<String>, bool)> = None;
        if let ArgValue::Many { tokens, literal } = arg_val {
            let elem_is_float = is_float_list(&schema);
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
            ArgValue::Group { .. } => build_arg_packet(i, arg_val, arg_def, manifest),
            _ => {
                let (raw_str, shape) = match arg_val {
                    ArgValue::Value(s) => (s.clone(), ArgShape::from_arg(arg_def)),
                    ArgValue::Null => ("null".to_string(), None),
                    ArgValue::Json(text) => (text.clone(), None),
                    ArgValue::Many { .. } | ArgValue::Group { .. } => unreachable!(),
                    ArgValue::Parsed { .. } | ArgValue::Embed { .. } | ArgValue::AsParent { .. } => unreachable!(),
                };
                let raw_str = if is_float_scalar(&schema) {
                    maybe_float_special_to_json(&raw_str).unwrap_or(raw_str)
                } else {
                    raw_str
                };
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

        if crate::stage::active() {
            extern "C" {
                fn morloc_packet_size(packet: *const u8, errmsg: *mut *mut std::ffi::c_char) -> usize;
            }
            let size = unsafe { morloc_packet_size(c_pkt, &mut errmsg) };
            if !link_ifile_arg(i, arg_val, arg_def) {
                crate::stage::save_arg(&cmd.name, i, c_pkt, size);
            }
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

    let result_relptr = unsafe { abs2rel(result, &mut errmsg) };
    let result_packet = unsafe { make_standard_data_packet(result_relptr, c_return_schema) };

    if result_packet.is_null() {
        eprintln!("Error: failed to create result packet");
        process::clean_exit(1);
    }

    extern "C" {
        fn morloc_packet_size(packet: *const u8, errmsg: *mut *mut std::ffi::c_char) -> usize;
    }
    let pkt_size = unsafe { morloc_packet_size(result_packet, &mut errmsg) };
    let pkt_bytes = unsafe { std::slice::from_raw_parts(result_packet, pkt_size).to_vec() };

    let result_ptr = unsafe { get_morloc_data_packet_value(pkt_bytes.as_ptr(), c_return_schema, &mut errmsg) };
    if result_ptr.is_null() {
        let msg = process::take_c_errmsg(errmsg)
            .unwrap_or_else(|| "unknown error".into());
        eprintln!("Error: failed to extract result: {}", msg);
        process::clean_exit(1);
    }

    die_on_top_level_err(result_ptr, c_return_schema);
    print_result_c(result_ptr, c_return_schema, &pkt_bytes, false, config);

    for cs in &c_arg_schemas {
        unsafe { morloc_runtime_types::cschema::CSchema::free(*cs as *mut morloc_runtime_types::cschema::CSchema) };
    }
    unsafe {
        morloc_runtime_types::cschema::CSchema::free(c_return_schema);
        libc::free(result_packet as *mut std::ffi::c_void);
    }
}

/// Whether a pool's reply says the run's stdout reader closed the pipe, which
/// ends the run as a broken pipe rather than as a failure.
pub(crate) fn reply_is_pipe_closed(packet: &[u8]) -> bool {
    let Some(header) = packet.get(..32) else { return false };
    let header: &[u8; 32] = header.try_into().expect("32 bytes");
    morloc_runtime_types::packet::PacketHeader::from_bytes(header).map_or(false, |h| h.is_pipe_closed())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_reply_that_reports_a_closed_pipe_is_told_from_a_failure() {
        use morloc_runtime_types::packet::{CommandData, PacketHeader, PACKET_STATUS_PIPE_CLOSED, PKT_CMD_OFF};
        let fail = PacketHeader::fail(0).to_bytes();
        assert!(!reply_is_pipe_closed(&fail));
        let mut closed = fail;
        closed[PKT_CMD_OFF + std::mem::offset_of!(CommandData, status)] = PACKET_STATUS_PIPE_CLOSED;
        assert!(reply_is_pipe_closed(&closed));
        assert!(!reply_is_pipe_closed(&closed[..10]));
    }

    fn positional(schema: &str) -> crate::manifest::Arg {
        serde_json::from_str(&format!(
            r#"{{"kind":"pos","key":"_1","schema":"{0}","general_schema":"{0}","quoted":true}}"#,
            schema
        ))
        .unwrap()
    }

    #[test]
    fn ifile_arg_from_stdin_is_refused() {
        let err = check_ifile_arg(&ArgValue::Value("-".into()), &positional("F")).unwrap_err();
        assert!(err.contains("stdin"), "{}", err);
    }

    #[test]
    fn ifile_arg_missing_file_names_the_path() {
        let path = "/no/such/dir/missing.mlc";
        let err = check_ifile_arg(&ArgValue::Value(path.into()), &positional("F")).unwrap_err();
        assert!(err.contains(path), "{}", err);
        let quoted = serde_json::to_string(path).unwrap();
        let err = check_ifile_arg(&ArgValue::Value(quoted), &positional("F")).unwrap_err();
        assert!(err.contains(&format!("'{}'", path)), "{}", err);
    }

    #[test]
    fn ifile_arg_existing_file_passes() {
        let file = std::env::temp_dir().join(format!("ifile_arg_{}", std::process::id()));
        std::fs::write(&file, b"").unwrap();
        let ok = check_ifile_arg(&ArgValue::Value(file.to_string_lossy().into()), &positional("F"));
        std::fs::remove_file(&file).unwrap();
        assert!(ok.is_ok(), "{:?}", ok);
    }

    #[test]
    fn non_ifile_arg_is_not_checked() {
        assert!(check_ifile_arg(&ArgValue::Value("-".into()), &positional("s")).is_ok());
        assert!(check_ifile_arg(&ArgValue::Json("\"-\"".into()), &positional("F")).is_ok());
    }

    #[test]
    fn collapse_counts_adjacent_duplicates() {
        assert_eq!(
            collapse_duplicate_lines("A\nA\nB\nA\n"),
            "A\n  [previous line repeated 1 more time]\nB\nA\n"
        );
    }

    #[test]
    fn collapse_preserves_non_adjacent() {
        assert_eq!(collapse_duplicate_lines("A\nB\nA\n"), "A\nB\nA\n");
    }

    #[test]
    fn collapse_collapses_long_run() {
        let input = "boom\n".repeat(20);
        assert_eq!(collapse_duplicate_lines(&input), "boom\n  [previous line repeated 19 more times]\n");
    }

    #[test]
    fn collapse_handles_no_trailing_newline() {
        assert_eq!(collapse_duplicate_lines("A\nA"), "A\n  [previous line repeated 1 more time]");
    }

    #[test]
    fn collapse_handles_empty() {
        assert_eq!(collapse_duplicate_lines(""), "");
    }

    #[test]
    fn apply_checks_path_rejects_missing_file() {
        let checks = vec![Check::Path("r".into())];
        assert!(apply_checks("/no/such/path/should/exist/x", &checks).is_err());
    }

    #[test]
    fn substitute_stdio_dash_maps_to_dev_stdio() {
        let r = vec![Check::Path("r".into())];
        assert_eq!(
            substitute_stdio_dash("-", &r, SourceAtom::Auto).as_deref(),
            Some("/dev/stdin"),
        );
        let w = vec![Check::Path("w".into())];
        assert_eq!(
            substitute_stdio_dash("-", &w, SourceAtom::Auto).as_deref(),
            Some("/dev/stdout"),
        );
    }

    #[test]
    fn substitute_stdio_dash_skips_exclusive_create() {
        let x = vec![Check::Path("x".into())];
        assert!(substitute_stdio_dash("-", &x, SourceAtom::Auto).is_none());
    }

    #[test]
    fn substitute_stdio_dash_leaves_non_dash_untouched() {
        let r = vec![Check::Path("r".into())];
        assert!(substitute_stdio_dash("foo.txt", &r, SourceAtom::Auto).is_none());
        assert!(substitute_stdio_dash("/dev/stdin", &r, SourceAtom::Auto).is_none());
        assert!(substitute_stdio_dash("", &r, SourceAtom::Auto).is_none());
    }

    #[test]
    fn substitute_stdio_dash_skips_mixed_and_unchecked() {
        let rw = vec![Check::Path("rw".into())];
        assert!(substitute_stdio_dash("-", &rw, SourceAtom::Auto).is_none());
        let none: Vec<Check> = vec![];
        assert!(substitute_stdio_dash("-", &none, SourceAtom::Auto).is_none());
    }

    #[test]
    fn substitute_stdio_dash_maps_declared_file_source() {
        let none: Vec<Check> = vec![];
        assert_eq!(
            substitute_stdio_dash("-", &none, SourceAtom::File).as_deref(),
            Some("/dev/stdin"),
        );
        assert!(substitute_stdio_dash("in.txt", &none, SourceAtom::File).is_none());
        assert!(substitute_stdio_dash("-", &none, SourceAtom::Inline).is_none());
    }

    #[test]
    fn apply_checks_w_accepts_creatable_path() {
        let unique = format!(
            "/tmp/morloc_apply_checks_w_{}_{}.tmp",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .map(|d| d.as_nanos())
                .unwrap_or(0),
        );
        let _ = std::fs::remove_file(&unique);
        let checks = vec![Check::Path("w".into())];
        let res = apply_checks(&unique, &checks);
        assert!(res.is_ok(), "expected w to accept creatable path, got {:?}", res);
    }

    #[test]
    fn apply_checks_x_rejects_existing_path() {
        let checks = vec![Check::Path("x".into())];
        assert!(apply_checks("/tmp", &checks).is_err());
    }

    #[test]
    fn apply_checks_rejects_unknown_mode() {
        let checks = vec![Check::Path("c".into())];
        let res = apply_checks("/tmp/anything", &checks);
        assert!(res.is_err());
        let checks = vec![Check::Path("rwc".into())];
        assert!(apply_checks("/tmp/anything", &checks).is_err());
    }
}
