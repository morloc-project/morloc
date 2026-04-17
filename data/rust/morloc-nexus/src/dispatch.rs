//! Command dispatch: CLI argument parsing and routing to pools.
//!
//! Replaces the dispatch_command, dispatch, run_command, and run_pure_command
//! functions from nexus.c. Uses the C libmorloc for packet construction and
//! serialization until Phase 2/3 replaces those.
//!
//! For Phase 1, the nexus links against the C libmorloc.so for:
//! - make_call_packet_from_cli, parse_cli_data_argument
//! - send_and_receive_over_socket
//! - pack_with_schema, print_voidstar, etc.
//! - morloc_eval for pure commands

use crate::help;
use crate::manifest::{Arg, Command, Manifest};
use crate::process::{self, PoolSocket};

/// Output format enum.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OutputFormat {
    Json,
    MessagePack,
    VoidStar,
    Packet,
}

/// Nexus configuration parsed from CLI options.
#[derive(Debug, Clone)]
pub struct NexusConfig {
    pub help_flag: bool,
    pub print_flag: bool,
    pub packet_path: Option<String>,
    pub socket_base: Option<String>,
    pub output_path: Option<String>,
    pub output_format: OutputFormat,
    pub daemon_flag: bool,
    pub router_flag: bool,
    pub unix_socket_path: Option<String>,
    pub tcp_port: Option<i32>,
    pub http_port: Option<i32>,
    pub fdb_path: Option<String>,
    pub eval_timeout: i32,
}

impl Default for NexusConfig {
    fn default() -> Self {
        NexusConfig {
            help_flag: false,
            print_flag: false,
            packet_path: None,
            socket_base: None,
            output_path: None,
            output_format: OutputFormat::Json,
            daemon_flag: false,
            router_flag: false,
            unix_socket_path: None,
            tcp_port: None,
            http_port: None,
            fdb_path: None,
            eval_timeout: 30,
        }
    }
}

/// Emit a uniform error when pool communication fails, then exit.
///
/// The pool's stderr was inherited by the nexus, so any traceback the pool
/// printed before dying is already on the user's terminal. This helper
/// just reports the communication error plus the pool's exit status (if
/// it has been reaped) so the user can correlate the two.
fn die_with_pool_error(
    socket: &PoolSocket,
    pool_index: usize,
    context: &str,
    comm_err: &dyn std::fmt::Display,
) -> ! {
    eprintln!("Error: {}: {}", context, comm_err);
    if let Some(info) = process::pool_death_info(pool_index) {
        eprintln!("Pool '{}' {}", socket.lang, info);
    }
    process::clean_exit(1);
}

/// Parse nexus-level options from argv. Returns the index of the first
/// non-option argument (the manifest path or subcommand).
pub fn parse_nexus_options(args: &[String], config: &mut NexusConfig) -> usize {
    let mut i = 1; // skip argv[0]
    while i < args.len() {
        let arg = &args[i];
        match arg.as_str() {
            "-h" | "--help" => {
                config.help_flag = true;
                i += 1;
            }
            "-p" | "--print" => {
                config.print_flag = true;
                i += 1;
            }
            "-c" | "--call-packet" => {
                i += 1;
                if i < args.len() {
                    config.packet_path = Some(args[i].clone());
                    i += 1;
                }
            }
            "-s" | "--socket-base" => {
                i += 1;
                if i < args.len() {
                    config.socket_base = Some(args[i].clone());
                    i += 1;
                }
            }
            "-o" | "--output-file" => {
                i += 1;
                if i < args.len() {
                    config.output_path = Some(args[i].clone());
                    i += 1;
                }
            }
            "-f" | "--output-form" => {
                i += 1;
                if i < args.len() {
                    config.output_format = parse_output_format(&args[i]);
                    i += 1;
                }
            }
            "--daemon" => {
                config.daemon_flag = true;
                i += 1;
            }
            "--router" => {
                config.router_flag = true;
                i += 1;
            }
            "--socket" => {
                i += 1;
                if i < args.len() {
                    config.unix_socket_path = Some(args[i].clone());
                    i += 1;
                }
            }
            "--port" => {
                i += 1;
                if i < args.len() {
                    config.tcp_port = args[i].parse().ok();
                    i += 1;
                }
            }
            "--http-port" => {
                i += 1;
                if i < args.len() {
                    config.http_port = args[i].parse().ok();
                    i += 1;
                }
            }
            "--fdb" => {
                i += 1;
                if i < args.len() {
                    config.fdb_path = Some(args[i].clone());
                    i += 1;
                }
            }
            "--eval-timeout" => {
                i += 1;
                if i < args.len() {
                    config.eval_timeout = args[i].parse().unwrap_or(30);
                    i += 1;
                }
            }
            _ => {
                // Handle --key=value forms
                if let Some(val) = arg.strip_prefix("--socket=") {
                    config.unix_socket_path = Some(val.to_string());
                    i += 1;
                } else if let Some(val) = arg.strip_prefix("--port=") {
                    config.tcp_port = val.parse().ok();
                    i += 1;
                } else if let Some(val) = arg.strip_prefix("--http-port=") {
                    config.http_port = val.parse().ok();
                    i += 1;
                } else if let Some(val) = arg.strip_prefix("--fdb=") {
                    config.fdb_path = Some(val.to_string());
                    i += 1;
                } else if let Some(val) = arg.strip_prefix("--eval-timeout=") {
                    config.eval_timeout = val.parse().unwrap_or(30);
                    i += 1;
                } else {
                    // Not a nexus option - stop parsing
                    break;
                }
            }
        }
    }
    i
}

/// Extract daemon/server long options from argv in single-command mode.
/// Removes matched options from the args vector.
pub fn extract_global_options(args: &mut Vec<String>, config: &mut NexusConfig) {
    let mut i = 1;
    while i < args.len() {
        if args[i] == "--" {
            break;
        }

        let mut matched = false;
        let mut consumed = 1;

        match args[i].as_str() {
            "--daemon" => {
                config.daemon_flag = true;
                matched = true;
            }
            "--socket" if i + 1 < args.len() => {
                config.unix_socket_path = Some(args[i + 1].clone());
                consumed = 2;
                matched = true;
            }
            "--port" if i + 1 < args.len() => {
                config.tcp_port = args[i + 1].parse().ok();
                consumed = 2;
                matched = true;
            }
            "--http-port" if i + 1 < args.len() => {
                config.http_port = args[i + 1].parse().ok();
                consumed = 2;
                matched = true;
            }
            "--fdb" if i + 1 < args.len() => {
                config.fdb_path = Some(args[i + 1].clone());
                consumed = 2;
                matched = true;
            }
            "--eval-timeout" if i + 1 < args.len() => {
                config.eval_timeout = args[i + 1].parse().unwrap_or(30);
                consumed = 2;
                matched = true;
            }
            _ => {
                // Check --key=value forms
                if let Some(val) = args[i].strip_prefix("--socket=") {
                    config.unix_socket_path = Some(val.to_string());
                    matched = true;
                } else if let Some(val) = args[i].strip_prefix("--port=") {
                    config.tcp_port = val.parse().ok();
                    matched = true;
                } else if let Some(val) = args[i].strip_prefix("--http-port=") {
                    config.http_port = val.parse().ok();
                    matched = true;
                } else if let Some(val) = args[i].strip_prefix("--fdb=") {
                    config.fdb_path = Some(val.to_string());
                    matched = true;
                } else if let Some(val) = args[i].strip_prefix("--eval-timeout=") {
                    config.eval_timeout = val.parse().unwrap_or(30);
                    matched = true;
                }
            }
        }

        if matched {
            for _ in 0..consumed {
                args.remove(i);
            }
        } else {
            i += 1;
        }
    }
}

fn parse_output_format(s: &str) -> OutputFormat {
    match s {
        "json" => OutputFormat::Json,
        "mpk" => OutputFormat::MessagePack,
        "voidstar" => OutputFormat::VoidStar,
        "packet" => OutputFormat::Packet,
        _ => {
            eprintln!("Invalid output format: {}", s);
            std::process::exit(1);
        }
    }
}

/// Wrap a string in JSON quotes (for literal string arguments).
pub fn quoted(s: &str) -> String {
    // JSON-escape the string
    let escaped = serde_json::to_string(s).unwrap_or_else(|_| format!("\"{}\"", s));
    escaped
}

/// Main dispatch entry point. Routes to the correct command based on argv.
pub fn dispatch(
    args: &[String],
    arg_start: usize,
    _shm_basename: &str,
    config: &NexusConfig,
    manifest: &Manifest,
    sockets: &mut [PoolSocket],
    prog_name: &str,
) {
    if arg_start >= args.len() {
        help::print_usage(prog_name, manifest);
    }

    let cmd_name = &args[arg_start];
    let next = arg_start + 1;

    // Check if it matches a group name
    for grp in &manifest.groups {
        if grp.name == *cmd_name {
            if next >= args.len() {
                help::print_group_usage(prog_name, manifest, cmd_name);
            }
            let subcmd = &args[next];
            if subcmd == "-h" || subcmd == "--help" {
                help::print_group_usage(prog_name, manifest, cmd_name);
            }
            // Find command within this group
            for cmd in &manifest.commands {
                if cmd.group.as_deref() == Some(cmd_name.as_str()) && cmd.name == *subcmd {
                    dispatch_command(args, next + 1, config, manifest, cmd, sockets, prog_name);
                    return;
                }
            }
            eprintln!("Unrecognized command '{}' in group '{}'", subcmd, cmd_name);
            process::clean_exit(1);
        }
    }

    // Try ungrouped commands
    for cmd in &manifest.commands {
        if cmd.name == *cmd_name && cmd.group.is_none() {
            dispatch_command(args, next, config, manifest, cmd, sockets, prog_name);
            return;
        }
    }

    eprintln!("Unrecognized command '{}'", cmd_name);
    process::clean_exit(1);
}

/// Dispatch a single command: parse its args, start needed daemons, execute.
pub fn dispatch_command(
    args: &[String],
    arg_start: usize,
    config: &NexusConfig,
    manifest: &Manifest,
    cmd: &Command,
    sockets: &mut [PoolSocket],
    prog_name: &str,
) {
    let single_cmd = manifest.commands.len() == 1 && manifest.groups.is_empty();

    // Parse command-specific arguments
    let (parsed_args, _remaining_start) =
        parse_command_args(args, arg_start, cmd, config, single_cmd, prog_name);

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
        run_remote_command(cmd, &parsed_args, sockets, config);
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
}

/// Parse command-specific arguments from argv.
fn parse_command_args(
    args: &[String],
    pos: usize,
    cmd: &Command,
    _config: &NexusConfig,
    single_cmd: bool,
    prog_name: &str,
) -> (Vec<ArgValue>, usize) {
    let mut parsed = Vec::with_capacity(cmd.args.len());
    // Simple option tracking: collect all --opt=val and -o val
    let mut opt_values: std::collections::HashMap<String, String> = std::collections::HashMap::new();
    let mut flag_values: std::collections::HashMap<String, String> = std::collections::HashMap::new();
    let mut positional_idx = 0;
    let mut positionals: Vec<String> = Vec::new();

    // First pass: separate options from positionals
    let mut i = pos;
    while i < args.len() {
        let arg = &args[i];
        if arg == "--" {
            i += 1;
            // Everything after -- is positional
            while i < args.len() {
                positionals.push(args[i].clone());
                i += 1;
            }
            break;
        }
        if arg == "-h" || arg == "--help" {
            if single_cmd {
                help::print_command_help_single(prog_name, cmd);
            } else {
                help::print_command_help(prog_name, cmd);
            }
        }
        if arg.starts_with("--") && arg.len() > 2 {
            // Long option
            if let Some(eq_pos) = arg.find('=') {
                let key = &arg[2..eq_pos];
                let val = &arg[eq_pos + 1..];
                opt_values.insert(key.to_string(), val.to_string());
                i += 1;
            } else {
                let key = &arg[2..];
                // Check if it's a flag
                if is_flag_opt(cmd, key) {
                    flag_values.insert(key.to_string(), flag_forward_value(cmd, key));
                    i += 1;
                } else if is_rev_flag(cmd, key) {
                    if let Some(orig) = find_flag_by_rev(cmd, key) {
                        flag_values.insert(orig, flag_reverse_value_by_rev(cmd, key));
                    }
                    i += 1;
                } else if i + 1 < args.len() {
                    opt_values.insert(key.to_string(), args[i + 1].clone());
                    i += 2;
                } else {
                    eprintln!("Error: option --{} requires a value", key);
                    process::clean_exit(1);
                }
            }
        } else if arg.starts_with('-') && arg.len() == 2 && arg.as_bytes()[1].is_ascii_alphabetic() {
            let ch = arg.chars().nth(1).unwrap();
            if is_short_flag(cmd, ch) {
                flag_values.insert(
                    short_to_long(cmd, ch).unwrap_or_else(|| ch.to_string()),
                    flag_forward_value_by_short(cmd, ch),
                );
                i += 1;
            } else if i + 1 < args.len() {
                opt_values.insert(
                    short_to_long(cmd, ch).unwrap_or_else(|| ch.to_string()),
                    args[i + 1].clone(),
                );
                i += 2;
            } else {
                eprintln!("Error: option -{} requires a value", ch);
                process::clean_exit(1);
            }
        } else {
            positionals.push(arg.clone());
            i += 1;
        }
    }

    // Second pass: build ArgValue for each manifest arg
    for arg_def in &cmd.args {
        match arg_def {
            Arg::Positional { quoted, .. } => {
                if positional_idx < positionals.len() {
                    let val = if *quoted {
                        self::quoted(&positionals[positional_idx])
                    } else {
                        positionals[positional_idx].clone()
                    };
                    parsed.push(ArgValue::Value(val));
                    positional_idx += 1;
                } else {
                    eprintln!("Error: too few positional arguments");
                    process::clean_exit(1);
                }
            }
            Arg::Optional {
                long_opt,
                short_opt,
                default_val,
                quoted,
                ..
            } => {
                let key = long_opt
                    .as_deref()
                    .or_else(|| short_opt.as_deref())
                    .unwrap_or("");
                let user_val = opt_values.get(key);
                if let Some(val) = user_val {
                    let v = if *quoted { self::quoted(val) } else { val.clone() };
                    parsed.push(ArgValue::Value(v));
                } else if let Some(def) = default_val {
                    parsed.push(ArgValue::Value(def.clone()));
                } else {
                    parsed.push(ArgValue::Null);
                }
            }
            Arg::Flag {
                long_opt,
                default_val,
                ..
            } => {
                let key = long_opt.as_deref().unwrap_or("");
                if let Some(val) = flag_values.get(key) {
                    parsed.push(ArgValue::Value(val.clone()));
                } else {
                    parsed.push(ArgValue::Value(
                        default_val.as_deref().unwrap_or("false").to_string(),
                    ));
                }
            }
            Arg::Group {
                entries,
                group_opt,
                ..
            } => {
                let grp_val = group_opt.as_ref().and_then(|go| {
                    go.long_opt
                        .as_deref()
                        .and_then(|k| opt_values.get(k))
                        .cloned()
                });
                let mut fields = Vec::new();
                let mut defaults = Vec::new();
                for entry in entries {
                    // Look up by long option name or short option character
                    let long_key = entry.arg.long_opt_str().unwrap_or("");
                    let short_key = entry.arg.short_opt_char()
                        .map(|c| c.to_string())
                        .unwrap_or_default();
                    let user = opt_values
                        .get(long_key)
                        .or_else(|| opt_values.get(&short_key))
                        .or_else(|| flag_values.get(long_key))
                        .or_else(|| flag_values.get(&short_key))
                        .map(|v| {
                            if entry.arg.is_quoted() {
                                self::quoted(v)
                            } else {
                                v.clone()
                            }
                        });
                    fields.push(user);
                    defaults.push(entry.arg.default_val().map(|s| s.to_string()));
                }
                parsed.push(ArgValue::Group {
                    grp_val,
                    fields,
                    defaults,
                });
            }
        }
    }

    if positional_idx < positionals.len() {
        eprintln!("Error: too many positional arguments given");
        process::clean_exit(1);
    }

    (parsed, i)
}

// -- Command execution ------------------------------------------------------

/// Execute a remote command by sending a call packet to the pool.
fn run_remote_command(
    cmd: &Command,
    args: &[ArgValue],
    sockets: &[PoolSocket],
    config: &NexusConfig,
) {
    use morloc_runtime::packet;
    use morloc_runtime::schema::{parse_schema, SerialType};
    use std::io::{Read, Write};
    use std::os::unix::net::UnixStream;

    // C library functions from libmorloc.so
    extern "C" {
        fn parse_cli_data_argument(
            dest: *mut u8, arg: *const std::ffi::c_void,
            schema: *const morloc_runtime::cschema::CSchema,
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
            data: *const u8, schema: *const morloc_runtime::cschema::CSchema,
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
    let mut arg_packets: Vec<Vec<u8>> = Vec::new();
    for (i, (arg_val, arg_def)) in args.iter().zip(cmd.args.iter()).enumerate() {
        let schema_str = arg_def.schema_str().unwrap_or("b");
        let schema = match parse_schema(schema_str) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Error: failed to parse arg schema #{}: {}", i, e);
                process::clean_exit(1);
            }
        };

        let c_schema = morloc_runtime::cschema::CSchema::from_rust(&schema);
        let mut errmsg: *mut std::ffi::c_char = std::ptr::null_mut();

        let c_arg;
        match arg_val {
            ArgValue::Group { grp_val, fields, defaults } => {
                // Group arg: use initialize_unrolled (matches C nexus behavior)
                extern "C" {
                    fn initialize_unrolled(
                        size: usize, default_value: *mut std::ffi::c_char,
                        fields: *mut *mut std::ffi::c_char,
                        default_fields: *mut *mut std::ffi::c_char,
                    ) -> *mut std::ffi::c_void;
                }
                let n = fields.len();
                let grp_val_c = grp_val.as_ref()
                    .map(|s| std::ffi::CString::new(s.as_str()).unwrap().into_raw())
                    .unwrap_or(std::ptr::null_mut());
                let mut c_fields: Vec<*mut std::ffi::c_char> = fields.iter()
                    .map(|f| f.as_ref()
                        .map(|s| std::ffi::CString::new(s.as_str()).unwrap().into_raw())
                        .unwrap_or(std::ptr::null_mut()))
                    .collect();
                let mut c_defaults: Vec<*mut std::ffi::c_char> = defaults.iter()
                    .map(|d| d.as_ref()
                        .map(|s| std::ffi::CString::new(s.as_str()).unwrap().into_raw())
                        .unwrap_or(std::ptr::null_mut()))
                    .collect();
                c_arg = unsafe {
                    initialize_unrolled(n, grp_val_c, c_fields.as_mut_ptr(), c_defaults.as_mut_ptr())
                };
            }
            _ => {
                let json_str = match arg_val {
                    ArgValue::Value(s) => s.clone(),
                    ArgValue::Null => "null".to_string(),
                    _ => unreachable!(),
                };
                let json_c = std::ffi::CString::new(json_str.as_str()).unwrap();
                c_arg = unsafe { initialize_positional(json_c.into_raw()) };
            }
        }

        let c_pkt = unsafe { parse_cli_data_argument(std::ptr::null_mut(), c_arg, c_schema, &mut errmsg) };
        unsafe { free_argument_t(c_arg) };
        unsafe { morloc_runtime::cschema::CSchema::free(c_schema) };

        if c_pkt.is_null() {
            let msg = if !errmsg.is_null() {
                let s = unsafe { std::ffi::CStr::from_ptr(errmsg) }.to_string_lossy().into_owned();
                unsafe { libc::free(errmsg as *mut std::ffi::c_void) };
                s
            } else {
                "unknown error".into()
            };
            eprintln!("Error: failed to parse argument #{}: {}", i, msg);
            process::clean_exit(1);
        }

        // Get packet size and copy to Vec
        let pkt_size = unsafe { morloc_packet_size(c_pkt, &mut errmsg) };
        let data_pkt = unsafe { std::slice::from_raw_parts(c_pkt, pkt_size).to_vec() };
        unsafe { libc::free(c_pkt as *mut std::ffi::c_void) };
        arg_packets.push(data_pkt);
    }

    // Build call packet via C library
    let arg_ptrs: Vec<*const u8> = arg_packets.iter().map(|p| p.as_ptr()).collect();
    let mut errmsg_call: *mut std::ffi::c_char = std::ptr::null_mut();
    let c_call = unsafe {
        make_morloc_local_call_packet(cmd.mid, arg_ptrs.as_ptr(), arg_packets.len(), &mut errmsg_call)
    };
    if c_call.is_null() {
        eprintln!("Error: failed to create call packet");
        process::clean_exit(1);
    }

    // Get call packet size
    let call_size = unsafe {
        let mut e: *mut std::ffi::c_char = std::ptr::null_mut();
        morloc_packet_size(c_call, &mut e)
    };
    let call_packet = unsafe { std::slice::from_raw_parts(c_call, call_size).to_vec() };
    unsafe { libc::free(c_call as *mut std::ffi::c_void) };

    // Send to pool and receive response
    let mut stream = match UnixStream::connect(&socket.socket_path) {
        Ok(s) => s,
        Err(e) => {
            die_with_pool_error(
                socket,
                cmd.pool_index,
                &format!("failed to connect to pool '{}'", socket.lang),
                &e,
            );
        }
    };

    if let Err(e) = stream.write_all(&call_packet) {
        die_with_pool_error(
            socket,
            cmd.pool_index,
            &format!("failed to send call packet to pool '{}'", socket.lang),
            &e,
        );
    }

    // Read response header
    let mut resp_header_bytes = [0u8; 32];
    if let Err(e) = stream.read_exact(&mut resp_header_bytes) {
        die_with_pool_error(
            socket,
            cmd.pool_index,
            &format!("failed to read response header from pool '{}'", socket.lang),
            &e,
        );
    }

    let resp_header = match packet::PacketHeader::from_bytes(&resp_header_bytes) {
        Ok(h) => h,
        Err(e) => {
            eprintln!("Error: invalid response packet: {}", e);
            process::clean_exit(1);
        }
    };

    // Read full response (metadata + payload)
    let offset = { resp_header.offset } as usize;
    let length = { resp_header.length } as usize;
    let remaining = offset + length;
    let mut resp_body = vec![0u8; remaining];
    if remaining > 0 {
        if let Err(e) = stream.read_exact(&mut resp_body) {
            die_with_pool_error(
                socket,
                cmd.pool_index,
                &format!("failed to read response body from pool '{}'", socket.lang),
                &e,
            );
        }
    }

    // Reconstruct full packet (header + body)
    let mut full_packet = Vec::with_capacity(32 + remaining);
    full_packet.extend_from_slice(&resp_header_bytes);
    full_packet.extend_from_slice(&resp_body);

    // Check for error
    match packet::get_error_message(&full_packet) {
        Ok(Some(err_msg)) => {
            eprintln!("Error: run failed\n{}", err_msg);
            process::clean_exit(1);
        }
        Ok(None) => {}
        Err(e) => {
            eprintln!("Error: failed to parse response: {}", e);
            process::clean_exit(1);
        }
    }

    // Extract and print via C library for correct voidstar handling
    let c_schema = morloc_runtime::cschema::CSchema::from_rust(&return_schema);
    let mut errmsg: *mut std::ffi::c_char = std::ptr::null_mut();
    let result_ptr = unsafe {
        get_morloc_data_packet_value(full_packet.as_ptr(), c_schema, &mut errmsg)
    };
    if result_ptr.is_null() {
        let msg = if !errmsg.is_null() {
            let s = unsafe { std::ffi::CStr::from_ptr(errmsg) }.to_string_lossy().into_owned();
            unsafe { libc::free(errmsg as *mut std::ffi::c_void) };
            s
        } else {
            "unknown error".into()
        };
        eprintln!("Error: failed to extract result: {}", msg);
        unsafe { morloc_runtime::cschema::CSchema::free(c_schema) };
        process::clean_exit(1);
    }

    // Check if response is Arrow format
    let is_arrow = resp_header.is_data() && unsafe { resp_header.command.data.format } == packet::PACKET_FORMAT_ARROW;

    // Print using the C library for correct output.
    // Suppress "null" for Unit-returning commands (CLI convention).
    if return_schema.serial_type != SerialType::Nil {
        print_result_c(result_ptr, c_schema, &full_packet, is_arrow, config);
    }
    unsafe { morloc_runtime::cschema::CSchema::free(c_schema) };
}

/// Print using the C library functions for correct voidstar handling.
fn print_result_c(
    ptr: *mut u8,
    schema: *const morloc_runtime::cschema::CSchema,
    full_packet: &[u8],
    is_arrow: bool,
    config: &NexusConfig,
) {
    extern "C" {
        fn print_voidstar(
            voidstar: *const std::ffi::c_void,
            schema: *const morloc_runtime::cschema::CSchema,
            errmsg: *mut *mut std::ffi::c_char,
        ) -> bool;
        fn pretty_print_voidstar(
            voidstar: *const std::ffi::c_void,
            schema: *const morloc_runtime::cschema::CSchema,
            errmsg: *mut *mut std::ffi::c_char,
        ) -> bool;
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
            schema: *const morloc_runtime::cschema::CSchema,
            mpkptr: *mut *mut std::ffi::c_char,
            mpk_size: *mut usize,
            errmsg: *mut *mut std::ffi::c_char,
        ) -> i32;
    }

    let mut errmsg: *mut std::ffi::c_char = std::ptr::null_mut();

    match config.output_format {
        OutputFormat::Json => {
            let ok = unsafe {
                if is_arrow && config.print_flag {
                    print_arrow_as_table(ptr as *const std::ffi::c_void, &mut errmsg)
                } else if is_arrow {
                    print_arrow_as_json(ptr as *const std::ffi::c_void, &mut errmsg)
                } else if config.print_flag {
                    pretty_print_voidstar(ptr as *const std::ffi::c_void, schema, &mut errmsg)
                } else {
                    print_voidstar(ptr as *const std::ffi::c_void, schema, &mut errmsg)
                }
            };
            if !ok {
                let msg = if !errmsg.is_null() {
                    let s = unsafe { std::ffi::CStr::from_ptr(errmsg) }.to_string_lossy().into_owned();
                    unsafe { libc::free(errmsg as *mut std::ffi::c_void) };
                    s
                } else {
                    "unknown error".into()
                };
                eprintln!("Error: {}", msg);
                process::clean_exit(1);
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
                for (i, b) in bytes.iter().enumerate() {
                    if i > 0 && i % 16 == 0 { println!(); }
                    print!("{:02x} ", b);
                }
                println!();
            } else {
                use std::io::Write;
                let bytes = unsafe { std::slice::from_raw_parts(mpk_ptr as *const u8, mpk_size) };
                let _ = std::io::stdout().lock().write_all(bytes);
            }
            if !mpk_ptr.is_null() {
                unsafe { libc::free(mpk_ptr as *mut std::ffi::c_void) };
            }
        }
        OutputFormat::VoidStar => {
            extern "C" {
                fn print_morloc_data_packet(
                    packet: *const u8,
                    schema: *const morloc_runtime::cschema::CSchema,
                    errmsg: *mut *mut std::ffi::c_char,
                ) -> i32;
            }
            if config.print_flag {
                // Hex dump
                for (i, b) in full_packet.iter().enumerate() {
                    if i > 0 && i % 4 == 0 {
                        if i % 24 == 0 { println!(); } else { print!(" "); }
                    }
                    print!("{:02X}", b);
                }
                if !full_packet.is_empty() { println!(); }
            } else {
                let mut errmsg2: *mut std::ffi::c_char = std::ptr::null_mut();
                unsafe { print_morloc_data_packet(full_packet.as_ptr(), schema, &mut errmsg2) };
            }
        }
        OutputFormat::Packet => {
            // Packet format: write raw binary packet to stdout (used by SLURM)
            use std::io::Write;
            let _ = std::io::stdout().lock().write_all(&full_packet);
        }
    }
    process::clean_exit(0);
}

/// Print using Rust-native functions (kept for reference, currently unused).
#[allow(dead_code)]
fn print_result(
    ptr: morloc_runtime::shm::AbsPtr,
    schema: &morloc_runtime::Schema,
    config: &NexusConfig,
) {
    use morloc_runtime::{json, mpack};

    match config.output_format {
        OutputFormat::Json => {
            if config.print_flag {
                if let Err(e) = json::pretty_print_voidstar(ptr, schema) {
                    eprintln!("Error: {}", e);
                    process::clean_exit(1);
                }
            } else {
                if let Err(e) = json::print_voidstar(ptr, schema) {
                    eprintln!("Error: {}", e);
                    process::clean_exit(1);
                }
            }
        }
        OutputFormat::MessagePack => {
            let mpk = match mpack::pack_with_schema(ptr, schema) {
                Ok(m) => m,
                Err(e) => {
                    eprintln!("Error: {}", e);
                    process::clean_exit(1);
                }
            };
            if config.print_flag {
                // Hex dump for human-readable msgpack
                for (i, byte) in mpk.iter().enumerate() {
                    if i > 0 && i % 16 == 0 {
                        println!();
                    }
                    print!("{:02x} ", byte);
                }
                println!();
            } else {
                use std::io::Write;
                let stdout = std::io::stdout();
                let mut handle = stdout.lock();
                let _ = handle.write_all(&mpk);
            }
        }
        OutputFormat::VoidStar | OutputFormat::Packet => {
            eprintln!("Error: voidstar/packet output not supported in Rust-native print path");
            process::clean_exit(1);
        }
    }
    process::clean_exit(0);
}

/// Execute a pure command by evaluating the expression via C library.
fn run_pure_command(cmd: &Command, args: &[ArgValue], config: &NexusConfig) {
    use morloc_runtime::schema::{parse_schema, SerialType};

    extern "C" {
        fn build_manifest_expr(
            json_str: *const std::ffi::c_char,
            errmsg: *mut *mut std::ffi::c_char,
        ) -> *mut std::ffi::c_void; // morloc_expression_t*
        fn morloc_eval(
            expr: *mut std::ffi::c_void,
            return_schema: *const morloc_runtime::cschema::CSchema,
            arg_voidstar: *const *mut u8,
            arg_schemas: *const *const morloc_runtime::cschema::CSchema,
            nargs: usize,
            errmsg: *mut *mut std::ffi::c_char,
        ) -> *mut std::ffi::c_void; // absptr_t
        fn parse_cli_data_argument(
            dest: *mut u8, arg: *const std::ffi::c_void,
            schema: *const morloc_runtime::cschema::CSchema,
            errmsg: *mut *mut std::ffi::c_char,
        ) -> *mut u8;
        fn initialize_positional(value: *mut std::ffi::c_char) -> *mut std::ffi::c_void;
        fn free_argument_t(arg: *mut std::ffi::c_void);
        fn get_morloc_data_packet_value(
            data: *const u8, schema: *const morloc_runtime::cschema::CSchema,
            errmsg: *mut *mut std::ffi::c_char,
        ) -> *mut u8;
        fn make_standard_data_packet(
            relptr: isize,
            schema: *const morloc_runtime::cschema::CSchema,
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
        let msg = unsafe_errmsg_to_string(errmsg);
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
    let c_return_schema = morloc_runtime::cschema::CSchema::from_rust(&return_schema);

    // The parsed `args` list and `cmd.args` are index-aligned 1:1 in
    // declaration order: parse_command_args pushes one ArgValue for
    // EVERY arg (including flags). The Haskell compiler emits one
    // schema per arg position too. Walk both lists in lockstep; for
    // flags, the schema_str() accessor returns None and we fall back
    // to the bool schema "b" so the wire format stays consistent.
    let mut c_arg_schemas: Vec<*const morloc_runtime::cschema::CSchema> = Vec::new();
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
        let c_schema = morloc_runtime::cschema::CSchema::from_rust(&schema);

        let json_str = match arg_val {
            ArgValue::Value(s) => s.clone(),
            ArgValue::Null => "null".to_string(),
            ArgValue::Group { .. } => "null".to_string(),
        };

        // Parse CLI arg to data packet, then extract voidstar
        let json_c = std::ffi::CString::new(json_str.as_str()).unwrap();
        let c_arg = unsafe { initialize_positional(json_c.into_raw()) };
        let c_pkt = unsafe { parse_cli_data_argument(std::ptr::null_mut(), c_arg, c_schema, &mut errmsg) };
        unsafe { free_argument_t(c_arg) };

        if c_pkt.is_null() {
            let msg = unsafe_errmsg_to_string(errmsg);
            eprintln!("Error: failed to parse argument #{}: {}", i, msg);
            process::clean_exit(1);
        }

        let voidstar = unsafe { get_morloc_data_packet_value(c_pkt, c_schema, &mut errmsg) };
        unsafe { libc::free(c_pkt as *mut std::ffi::c_void) };
        if voidstar.is_null() {
            let msg = unsafe_errmsg_to_string(errmsg);
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
        let msg = unsafe_errmsg_to_string(errmsg);
        eprintln!("Error: evaluation failed: {}", msg);
        process::clean_exit(1);
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

    if return_schema.serial_type != SerialType::Nil {
        print_result_c(result_ptr, c_return_schema, &pkt_bytes, false, config);
    }

    // Cleanup
    for cs in &c_arg_schemas {
        unsafe { morloc_runtime::cschema::CSchema::free(*cs as *mut morloc_runtime::cschema::CSchema) };
    }
    unsafe {
        morloc_runtime::cschema::CSchema::free(c_return_schema);
        libc::free(result_packet as *mut std::ffi::c_void);
    }
}

fn unsafe_errmsg_to_string(errmsg: *mut std::ffi::c_char) -> String {
    if errmsg.is_null() {
        "unknown error".into()
    } else {
        let s = unsafe { std::ffi::CStr::from_ptr(errmsg) }.to_string_lossy().into_owned();
        unsafe { libc::free(errmsg as *mut std::ffi::c_void) };
        s
    }
}

// -- Helpers for command argument parsing ------------------------------------

fn is_flag_opt(cmd: &Command, long_name: &str) -> bool {
    cmd.args.iter().any(|a| match a {
        Arg::Flag { long_opt, .. } => long_opt.as_deref() == Some(long_name),
        Arg::Group { entries, .. } => entries.iter().any(|e| match &e.arg {
            Arg::Flag { long_opt, .. } => long_opt.as_deref() == Some(long_name),
            _ => false,
        }),
        _ => false,
    })
}

fn is_rev_flag(cmd: &Command, name: &str) -> bool {
    cmd.args.iter().any(|a| match a {
        Arg::Flag { long_rev, .. } => long_rev.as_deref() == Some(name),
        Arg::Group { entries, .. } => entries.iter().any(|e| match &e.arg {
            Arg::Flag { long_rev, .. } => long_rev.as_deref() == Some(name),
            _ => false,
        }),
        _ => false,
    })
}

fn find_flag_by_rev(cmd: &Command, rev_name: &str) -> Option<String> {
    for a in &cmd.args {
        match a {
            Arg::Flag { long_opt, long_rev, .. } => {
                if long_rev.as_deref() == Some(rev_name) {
                    return long_opt.clone();
                }
            }
            Arg::Group { entries, .. } => {
                for e in entries {
                    if let Arg::Flag { long_opt, long_rev, .. } = &e.arg {
                        if long_rev.as_deref() == Some(rev_name) {
                            return long_opt.clone();
                        }
                    }
                }
            }
            _ => {}
        }
    }
    None
}

fn flag_forward_value(cmd: &Command, long_name: &str) -> String {
    for a in &cmd.args {
        if let Arg::Flag {
            long_opt,
            default_val,
            ..
        } = a
        {
            if long_opt.as_deref() == Some(long_name) {
                let def = default_val.as_deref().unwrap_or("false");
                return if def == "true" {
                    "false".into()
                } else {
                    "true".into()
                };
            }
        }
    }
    "true".into()
}

fn flag_forward_value_by_short(cmd: &Command, ch: char) -> String {
    for a in &cmd.args {
        if let Arg::Flag {
            short_opt,
            default_val,
            ..
        } = a
        {
            if short_opt.as_deref().and_then(|s| s.chars().next()) == Some(ch) {
                let def = default_val.as_deref().unwrap_or("false");
                return if def == "true" {
                    "false".into()
                } else {
                    "true".into()
                };
            }
        }
    }
    "true".into()
}

fn flag_reverse_value_by_rev(cmd: &Command, rev_name: &str) -> String {
    // Search top-level and group entries
    let check = |long_rev: &Option<String>, default_val: &Option<String>| -> Option<String> {
        if long_rev.as_deref() == Some(rev_name) {
            let def = default_val.as_deref().unwrap_or("false");
            Some(if def == "true" { "true".into() } else { "false".into() })
        } else {
            None
        }
    };
    for a in &cmd.args {
        match a {
            Arg::Flag { long_rev, default_val, .. } => {
                if let Some(v) = check(long_rev, default_val) { return v; }
            }
            Arg::Group { entries, .. } => {
                for e in entries {
                    if let Arg::Flag { long_rev, default_val, .. } = &e.arg {
                        if let Some(v) = check(long_rev, default_val) { return v; }
                    }
                }
            }
            _ => {}
        }
    }
    "false".into()
}

fn is_short_flag(cmd: &Command, ch: char) -> bool {
    cmd.args.iter().any(|a| match a {
        Arg::Flag { short_opt, .. } => {
            short_opt.as_deref().and_then(|s| s.chars().next()) == Some(ch)
        }
        _ => false,
    })
}

fn short_to_long(cmd: &Command, ch: char) -> Option<String> {
    for a in &cmd.args {
        let (s, l) = match a {
            Arg::Optional {
                short_opt,
                long_opt,
                ..
            } => (short_opt.as_deref(), long_opt.clone()),
            Arg::Flag {
                short_opt,
                long_opt,
                ..
            } => (short_opt.as_deref(), long_opt.clone()),
            Arg::Group { entries, .. } => {
                // Search inside group entries
                for entry in entries {
                    let (es, el) = match &entry.arg {
                        Arg::Optional { short_opt, long_opt, .. } => (short_opt.as_deref(), long_opt.clone()),
                        Arg::Flag { short_opt, long_opt, .. } => (short_opt.as_deref(), long_opt.clone()),
                        _ => (None, None),
                    };
                    if es.and_then(|s| s.chars().next()) == Some(ch) {
                        return el.or_else(|| Some(ch.to_string()));
                    }
                }
                (None, None)
            }
            _ => (None, None),
        };
        if s.and_then(|s| s.chars().next()) == Some(ch) {
            return l;
        }
    }
    None
}
