//! Top-level CLI parser for `morloc-nexus`, derived from `clap`.
//!
//! The nexus has three explicit roles -- `run`, `daemon`, `router` --
//! each a clap subcommand on the top-level `morloc-nexus` binary.
//! Compiled wrapper scripts always hardcode `morloc-nexus run "$0"
//! "$@"`, so a user's `./my-prog -h` resolves to `morloc-nexus run
//! ./my-prog -h` and its clap-generated help shows only the run-side
//! options. Daemon and router options are only reachable through
//! explicit `morloc-nexus daemon` / `morloc-nexus router` invocations.
//!
//! ## Two-pass parse
//!
//! This module handles the top-level parse: select the mode and
//! consume the always-on flags (output formatting, log-dir, quiet,
//! ...) that the user may write before the wrapper target. The
//! trailing slice (`RunArgs::rest` / `DaemonArgs::rest`) then runs
//! through [`crate::phase2`], which builds a manifest-driven
//! `clap::Command` so the per-program subcommands and any flags
//! written after the wrapper target (e.g. `./prog --log-dir X
//! mycmd`) bind correctly. The split is required because the
//! wrapper script intercedes between the user's argv and
//! `morloc-nexus`, and only the manifest knows the user's exported
//! command surface.

use clap::{Args, Parser, Subcommand, ValueEnum};

use crate::dispatch::{NexusConfig, OutputFormat};

/// Top-level CLI. Always parsed via clap-derive; phase-2 manifest
/// parsing happens after the mode is known.
#[derive(Parser, Debug)]
#[command(
    name = "morloc-nexus",
    bin_name = "morloc-nexus",
    term_width = 80,
    arg_required_else_help = true,
    disable_help_subcommand = true,
    about = "Morloc nexus: CLI dispatcher for compiled morloc programs.",
    long_about = "Morloc nexus binary.\n\n\
                  Programs compiled with `morloc make` ship a small shell wrapper that \
                  re-execs into `morloc-nexus run <wrapper>`; users normally interact with \
                  the wrapper rather than this binary directly. Use `daemon` to serve a \
                  compiled program as a long-lived process and `router` to multiplex many \
                  installed programs from one process."
)]
pub struct Nexus {
    #[command(subcommand)]
    pub cmd: Mode,
}

/// The three explicit roles of the nexus.
#[derive(Subcommand, Debug)]
pub enum Mode {
    /// Run one subcommand of a compiled morloc program (one-shot CLI).
    Run(RunArgs),
    /// Serve a compiled morloc program as a long-lived daemon.
    Daemon(DaemonArgs),
    /// Multi-program router across installed modules.
    Router(RouterArgs),
}

/// Output-formatting + run-scope log options shared by `run` and
/// `daemon`. Daemon-only listener options live on [`DaemonArgs`].
#[derive(Args, Debug, Clone, Default)]
pub struct DispatchOptions {
    /// Pretty-print result.
    #[arg(short, long)]
    pub print: bool,

    /// Output file (default: stdout).
    #[arg(short = 'o', long = "output-file", value_name = "FILE")]
    pub output_path: Option<String>,

    /// Output form for the result.
    #[arg(
        short = 'f',
        long = "output-form",
        value_name = "FORM",
        value_enum
    )]
    pub output_form: Option<OutputForm>,

    /// Print top-level () / None as "null" instead of suppressing it.
    #[arg(long = "keep-null")]
    pub keep_null: bool,

    /// Suppress morloc-emitted log lines on stderr.
    #[arg(short, long)]
    pub quiet: bool,

    /// Per-run output dir (logs, summary.json, debug dumps).
    #[arg(long = "log-dir", value_name = "DIR")]
    pub log_dir: Option<String>,

    /// Override the summary.json location.
    #[arg(long, value_name = "PATH")]
    pub summary: Option<String>,

    /// zstd compression preset for `-f packet` output. RPTR (shared
    /// memory) and FILE (temp file) result packets are always
    /// materialized to inline MESG form on disk regardless of this
    /// flag, so the on-disk packet is self-contained. When N > 0 the
    /// MESG payload is additionally zstd-compressed; the schema
    /// metadata block stays in the clear. 0 = no compression
    /// (default), 1 = fastest, 9 = maximum.
    #[arg(short = 'z', long = "compression-level",
          default_value_t = 0, value_name = "N")]
    pub compression_level: u8,
}

/// One-shot CLI invocation of a compiled morloc program.
///
/// The `--debug-*` flags inherited via `common: DispatchOptions`
/// only take effect when the target binary advertises the
/// `"debug_trace"` capability in its manifest. Passing one against
/// a binary built without `morloc make --debug` raises a clear
/// error after manifest load (see [`validate_capabilities`]).
#[derive(Args, Debug)]
pub struct RunArgs {
    /// Path to the program's wrapper script or .manifest file.
    pub target: String,

    #[command(flatten)]
    pub common: DispatchOptions,

    // ---- SLURM call-packet plumbing (internal) ----
    /// Internal: replay a pre-built call packet on a remote worker
    /// node. SLURM workers use this to invoke a single labeled call
    /// without re-entering the top-level CLI.
    #[arg(short = 'c', long = "call-packet", value_name = "FILE", hide = true)]
    pub call_packet: Option<String>,

    /// Internal: socket basename used by call-packet mode to locate
    /// the local pool listener.
    #[arg(short = 's', long = "socket-base", value_name = "NAME", hide = true)]
    pub socket_base: Option<String>,
}

/// Serve a compiled morloc program as a long-lived daemon.
///
/// The `--debug-*` flags inherited via `common: DispatchOptions`
/// only take effect when the target binary advertises the
/// `"debug_trace"` capability in its manifest. Passing one against
/// a binary built without `morloc make --debug` raises a clear
/// error after manifest load (see [`validate_capabilities`]).
#[derive(Args, Debug)]
pub struct DaemonArgs {
    /// Path to the program's wrapper script or .manifest file.
    pub target: String,

    #[command(flatten)]
    pub common: DispatchOptions,

    /// Listen on a Unix domain socket at PATH.
    #[arg(long, value_name = "PATH")]
    pub socket: Option<String>,

    /// Listen on a TCP port (0 = ephemeral).
    #[arg(long, value_name = "PORT")]
    pub port: Option<u16>,

    /// Listen on an HTTP port.
    #[arg(long = "http-port", value_name = "PORT")]
    pub http_port: Option<u16>,

    /// Write bound ports to PATH as JSON.
    #[arg(long = "port-file", value_name = "PATH")]
    pub port_file: Option<String>,

    /// CPU budget for /eval and /typecheck in seconds.
    #[arg(long = "eval-timeout", value_name = "SECS", default_value_t = 30)]
    pub eval_timeout: u32,
}

/// Multi-program router across installed modules. Doesn't take a
/// single program target -- it serves every manifest it finds under
/// the fdb directory.
#[derive(Args, Debug)]
pub struct RouterArgs {
    /// Path to the fdb manifest directory. Defaults to
    /// `$MORLOC_HOME/fdb`.
    #[arg(long, value_name = "PATH")]
    pub fdb: Option<String>,

    /// Listen on a Unix domain socket at PATH.
    #[arg(long, value_name = "PATH")]
    pub socket: Option<String>,

    /// Listen on a TCP port (0 = ephemeral).
    #[arg(long, value_name = "PORT")]
    pub port: Option<u16>,

    /// Listen on an HTTP port.
    #[arg(long = "http-port", value_name = "PORT")]
    pub http_port: Option<u16>,

    /// Write bound ports to PATH as JSON.
    #[arg(long = "port-file", value_name = "PATH")]
    pub port_file: Option<String>,

    /// CPU budget for /eval and /typecheck in seconds.
    #[arg(long = "eval-timeout", value_name = "SECS", default_value_t = 30)]
    pub eval_timeout: u32,
}

/// Output format choices exposed via `-f / --output-form`. The
/// mapping to the internal [`OutputFormat`] keeps the existing string
/// surface (`mpk`, `ipc`/`arrow`, ...) so help text and tests stay
/// aligned.
#[derive(ValueEnum, Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputForm {
    Json,
    Mpk,
    Voidstar,
    Packet,
    Arrow,
    Parquet,
    Csv,
}

impl OutputForm {
    pub fn to_internal(self) -> OutputFormat {
        match self {
            OutputForm::Json => OutputFormat::Json,
            OutputForm::Mpk => OutputFormat::MessagePack,
            OutputForm::Voidstar => OutputFormat::VoidStar,
            OutputForm::Packet => OutputFormat::Packet,
            OutputForm::Arrow => OutputFormat::Arrow,
            OutputForm::Parquet => OutputFormat::Parquet,
            OutputForm::Csv => OutputFormat::Csv,
        }
    }
}

/// Merge a [`DispatchOptions`] block into a [`NexusConfig`]. Used by
/// both `run` and `daemon` so the shared option surface stays in one
/// place.
fn apply_dispatch_options(cfg: &mut NexusConfig, opts: &DispatchOptions) {
    cfg.print_flag = opts.print;
    cfg.output_path = opts.output_path.clone();
    if let Some(form) = opts.output_form {
        cfg.output_format = form.to_internal();
    }
    cfg.keep_null = opts.keep_null;
    cfg.quiet = opts.quiet;
    cfg.log_dir = opts.log_dir.clone();
    cfg.summary_path = opts.summary.clone();
    cfg.compression_level = opts.compression_level;
}

// ============================================================
// Capability-gated nexus options
// ============================================================
//
// Some nexus options only make sense against binaries compiled
// with a specific `morloc make` flag (e.g. `--debug-cache-depth`
// requires `morloc make --debug`). The compiler advertises which
// such feature sets are present by populating
// `manifest.capabilities` with capability tokens (e.g.
// `"debug_trace"`); the nexus filters its accepted-and-displayed
// flag set per-manifest based on those tokens.
//
// The static [`CAPABILITY_FLAGS`] table is the single source of
// truth for which flag belongs to which capability. Adding a new
// gated flag is one entry; adding a new capability with N flags
// is N entries with the same `capability` tag.

/// Value kind for a capability-gated flag. Determines how the flag
/// is registered with clap (value parser) and how its value is
/// extracted from [`clap::ArgMatches`].
#[derive(Debug, Clone, Copy)]
pub enum FlagKind {
    /// `--flag N` where N is a u64.
    U64,
    /// `--flag N` where N is a u32.
    U32,
    /// `--flag VALUE` where VALUE is an arbitrary string (e.g.
    /// `--debug-cache-max 4k`); semantic parsing happens later.
    Str,
}

/// Static definition for one capability-gated nexus flag.
#[derive(Debug, Clone, Copy)]
pub struct CapabilityFlagSpec {
    /// Capability token the manifest must advertise for this flag
    /// to be accepted (and shown in `--help`). Future strings live
    /// here alongside today's `"debug_trace"`.
    pub capability: &'static str,
    /// The long form (without leading `--`).
    pub long: &'static str,
    /// Optional short form character (without leading `-`).
    pub short: Option<char>,
    /// Display value placeholder (`N`, `BYTES`, `PATH`, ...).
    pub value_name: &'static str,
    /// One-line help shown in `--help`.
    pub help: &'static str,
    /// What the user must do at compile time to enable this flag.
    /// Used in both `--help` and the unsupported-flag error.
    pub requires_msg: &'static str,
    /// Value type accepted by the flag.
    pub kind: FlagKind,
}

/// Static table of every capability-gated nexus option. Order is
/// the order they appear in `--help`. Both `run` and `daemon`
/// modes consume this same table; the compiler-side capability
/// emission decides which flags are advertised per binary.
pub const CAPABILITY_FLAGS: &[CapabilityFlagSpec] = &[
    CapabilityFlagSpec {
        capability: "debug_trace",
        long: "debug-cache-depth",
        short: None,
        value_name: "N",
        help: "Max arg dumps per dispatch (default 1; 0 = unlimited)",
        requires_msg: "morloc make --debug",
        kind: FlagKind::U64,
    },
    CapabilityFlagSpec {
        capability: "debug_trace",
        long: "debug-cache-max",
        short: None,
        value_name: "BYTES",
        help: "Per-arg msgpack byte cap (k/m/g suffix accepted)",
        requires_msg: "morloc make --debug",
        kind: FlagKind::Str,
    },
    CapabilityFlagSpec {
        capability: "debug_trace",
        long: "debug-recursion-cap",
        short: None,
        value_name: "N",
        help: "Max trace frames per midx (default 3; 0 = unlimited)",
        requires_msg: "morloc make --debug",
        kind: FlagKind::U32,
    },
];

impl CapabilityFlagSpec {
    /// True iff this flag consumes the next argv token as its value.
    /// All current kinds do; included for future boolean flags.
    pub fn takes_value(&self) -> bool {
        matches!(self.kind, FlagKind::U64 | FlagKind::U32 | FlagKind::Str)
    }

    /// Build a [`clap::Arg`] from the static spec, suitable for
    /// attaching to a runtime-built clap command. The `enabled`
    /// flag controls whether the arg appears in `--help`: when
    /// `false`, the arg is `hide(true)` (still accepted from argv
    /// for the friendlier-error path below to trigger, but not
    /// listed in help against this manifest).
    pub fn to_clap_arg(&self, enabled: bool) -> clap::Arg {
        let mut a = clap::Arg::new(self.long)
            .long(self.long)
            .value_name(self.value_name)
            .action(clap::ArgAction::Set)
            .hide(!enabled);
        if let Some(c) = self.short {
            a = a.short(c);
        }
        let help_text: &'static str = if enabled {
            self.help
        } else {
            self.help
        };
        // Append the "requires" suffix so users always know how to
        // enable a flag they can see in the help.
        let with_requires: &'static str = Box::leak(
            format!("{} [requires {}]", help_text, self.requires_msg).into_boxed_str(),
        );
        a = a.help(with_requires);
        match self.kind {
            FlagKind::U64 => a.value_parser(clap::value_parser!(u64)),
            FlagKind::U32 => a.value_parser(clap::value_parser!(u32)),
            FlagKind::Str => a,
        }
    }
}

/// Augment a clap command with capability-gated flags. Flags
/// whose capability is in `capabilities` are attached visible;
/// those whose capability isn't are attached *hidden*, so clap
/// accepts them from argv (letting [`check_unsupported_flags`]
/// produce a friendly error) but doesn't list them in `--help`.
///
/// When `capabilities` is `None`, all capability flags are
/// attached visible. Used for the no-manifest `--help` path so
/// `morloc-nexus run --help` discloses the entire surface.
pub fn augment_with_capability_flags(
    mut cmd: clap::Command,
    capabilities: Option<&[String]>,
) -> clap::Command {
    for spec in CAPABILITY_FLAGS {
        let enabled = match capabilities {
            None => true,
            Some(caps) => caps.iter().any(|c| c == spec.capability),
        };
        cmd = cmd.arg(spec.to_clap_arg(enabled));
    }
    cmd
}

/// Pre-check the nexus zone for any capability-gated flag whose
/// capability is not advertised by the manifest. If found, exit
/// with a clear "requires X" message; otherwise return without
/// effect. Runs before the augmented clap parse so the user sees
/// a domain-specific error rather than clap's generic "unknown
/// argument" / "unexpected value" wording.
pub fn check_unsupported_capability_flags(
    nexus_zone: &[String],
    capabilities: &[String],
) {
    for tok in nexus_zone {
        // Match long forms (`--name` or `--name=value`); short
        // forms aren't used by today's capability flags but the
        // check stays single-pass for them too.
        let long_name = if let Some(rest) = tok.strip_prefix("--") {
            Some(rest.split('=').next().unwrap_or(rest))
        } else {
            None
        };
        let short_char = if !tok.starts_with("--") && looks_like_flag(tok) {
            tok.chars().nth(1)
        } else {
            None
        };
        for spec in CAPABILITY_FLAGS {
            let matches_long = long_name.map_or(false, |n| n == spec.long);
            let matches_short =
                short_char.is_some() && spec.short == short_char;
            if !matches_long && !matches_short {
                continue;
            }
            let advertised =
                capabilities.iter().any(|c| c == spec.capability);
            if !advertised {
                eprintln!(
                    "Error: --{} is supported only on binaries built with `{}` \
                     (the manifest does not advertise the '{}' capability).",
                    spec.long, spec.requires_msg, spec.capability
                );
                std::process::exit(1);
            }
        }
    }
}

/// Parse a byte-count string with optional k/m/g suffix (1024-based).
fn parse_byte_size(flag: &str, raw: &str) -> u64 {
    let (num_str, mult): (&str, u64) = match raw.chars().last() {
        Some('k') | Some('K') => (&raw[..raw.len() - 1], 1024),
        Some('m') | Some('M') => (&raw[..raw.len() - 1], 1024 * 1024),
        Some('g') | Some('G') => (&raw[..raw.len() - 1], 1024 * 1024 * 1024),
        _ => (raw, 1),
    };
    match num_str.parse::<u64>() {
        Ok(n) => n.saturating_mul(mult),
        Err(_) => {
            eprintln!(
                "error: {} expects a byte count (k/m/g suffix allowed), got '{}'",
                flag, raw
            );
            std::process::exit(2);
        }
    }
}

/// Translate a parsed [`RunArgs`] block into a [`NexusConfig`]. The
/// `target` field is returned separately so the caller can resolve
/// it to a manifest path.
pub fn run_args_to_config(args: &RunArgs) -> (NexusConfig, String) {
    let mut cfg = NexusConfig::default();
    apply_dispatch_options(&mut cfg, &args.common);
    cfg.packet_path = args.call_packet.clone();
    cfg.socket_base = args.socket_base.clone();
    (cfg, args.target.clone())
}

/// Translate a parsed [`DaemonArgs`] block into a [`NexusConfig`].
/// Returns the resolved config and the target path.
pub fn daemon_args_to_config(args: &DaemonArgs) -> (NexusConfig, String) {
    let mut cfg = NexusConfig::default();
    apply_dispatch_options(&mut cfg, &args.common);
    cfg.daemon_flag = true;
    cfg.unix_socket_path = args.socket.clone();
    cfg.tcp_port = args.port.map(|p| p as i32);
    cfg.http_port = args.http_port.map(|p| p as i32);
    cfg.port_file_path = args.port_file.clone();
    cfg.eval_timeout = args.eval_timeout as i32;
    (cfg, args.target.clone())
}

/// Translate a parsed [`RouterArgs`] block into a [`NexusConfig`].
pub fn router_args_to_config(args: &RouterArgs) -> NexusConfig {
    let mut cfg = NexusConfig::default();
    cfg.router_flag = true;
    cfg.fdb_path = args.fdb.clone();
    cfg.unix_socket_path = args.socket.clone();
    cfg.tcp_port = args.port.map(|p| p as i32);
    cfg.http_port = args.http_port.map(|p| p as i32);
    cfg.port_file_path = args.port_file.clone();
    cfg.eval_timeout = args.eval_timeout as i32;
    cfg
}

/// Sentinel comment the compiler emits on line 2 of every generated
/// wrapper script (see `Morloc.CodeGenerator.Nexus.makeWrapperScript`).
/// Used by [`resolve_daemon_target`] to confirm a non-`.manifest`
/// daemon target is actually a morloc wrapper before trusting its
/// embedded JSON payload.
pub const WRAPPER_SENTINEL: &str = "# morloc-program v";

/// Resolve a daemon target argument to a path that
/// [`crate::manifest::read_manifest_payload`] can ingest. The
/// wrapper format embeds the manifest JSON after a `### MANIFEST ###`
/// marker, so a wrapper script and a freestanding `.manifest` file
/// both yield the manifest from the same extraction code path. This
/// resolver gates which paths reach that extractor:
///
/// 1. A `.manifest` extension is trusted (the JSON parser fails
///    fast if it's malformed).
/// 2. Other files are accepted iff their head contains the
///    [`WRAPPER_SENTINEL`] comment that the compiler stamps onto
///    every generated wrapper. This guards against a user passing a
///    random shell script and seeing a confusing "no marker" error
///    from deep inside the manifest extractor.
/// 3. Anything else is rejected with a clear "not a morloc wrapper
///    or manifest file" diagnostic.
///
/// Returns the path that should be fed to `read_manifest_payload`
/// (the same path that was passed in -- the embedded-payload
/// extractor handles both wrapper and freestanding shapes
/// uniformly). Errors are caller-friendly strings ready to print.
pub fn resolve_daemon_target(target: &str) -> Result<String, String> {
    use std::io::Read;

    if target.ends_with(".manifest") {
        // Trust the extension; the manifest parser will fail cleanly
        // if the file is missing or unreadable.
        return Ok(target.to_string());
    }

    let mut buf = [0u8; 4096];
    let n = std::fs::File::open(target)
        .and_then(|mut f| f.read(&mut buf))
        .map_err(|e| format!("cannot open '{}': {}", target, e))?;

    let head = std::str::from_utf8(&buf[..n]).map_err(|_| {
        format!(
            "'{}' is not a morloc wrapper or manifest file (binary content)",
            target
        )
    })?;
    if head.contains(WRAPPER_SENTINEL) {
        Ok(target.to_string())
    } else {
        Err(format!(
            "'{}' is not a morloc wrapper or manifest file \
             (missing sentinel comment and not a *.manifest)",
            target
        ))
    }
}

// ============================================================
// `@` separator between nexus zone and user zone
// ============================================================
//
// `morloc-nexus run` splits its argv into two zones at the first
// unconsumed `@` token. The nexus zone (everything up to and
// including the target, plus pre-target nexus options) is parsed
// against [`RunArgs`]; the user zone is handed to
// [`crate::phase2::parse_run`] for manifest-driven parsing against
// the user's exported command surface.
//
// The split is required because the wrapper script execs
// `morloc-nexus run "$0" "$@"`, so users always pass their morloc
// program's argv *after* the wrapper target. Without a syntactic
// boundary, a flag like `-o` could belong to the nexus or to the
// user's exported function -- and morloc lets users declare
// arbitrary `-x` / `--xxx` flags on their functions, so the
// collision is unavoidable.
//
// `@` was chosen because morloc's docstring grammar restricts user
// flags to POSIX UNIX form (`-x` and `--xxx`), making `@` a
// syntactically impossible spelling for any user-declared flag.

/// Build the union of every value-taking nexus flag the scanner
/// might encounter: the always-on flags declared on the derived
/// [`Nexus`] command (clap introspection) plus every capability-
/// gated flag in [`CAPABILITY_FLAGS`] whose kind takes a value.
/// The union is independent of which capabilities a particular
/// manifest advertises -- a flag's value-taking-ness doesn't
/// change with the manifest, only its acceptance does. Capability
/// filtering happens later in [`augment_with_capability_flags`].
fn value_taking_run_flags() -> ValueTakingFlags {
    use clap::CommandFactory;
    let nexus_cmd = crate::help::strip_styles_recursively(Nexus::command());
    let run_cmd = nexus_cmd
        .find_subcommand("run")
        .expect("Nexus declares a 'run' subcommand");
    let mut longs: std::collections::HashSet<String> =
        std::collections::HashSet::new();
    let mut shorts: std::collections::HashSet<char> =
        std::collections::HashSet::new();
    for arg in run_cmd.get_arguments() {
        if arg.get_action().takes_values() {
            if let Some(l) = arg.get_long() {
                longs.insert(format!("--{}", l));
            }
            if let Some(c) = arg.get_short() {
                shorts.insert(c);
            }
        }
    }
    for spec in CAPABILITY_FLAGS {
        if spec.takes_value() {
            longs.insert(format!("--{}", spec.long));
            if let Some(c) = spec.short {
                shorts.insert(c);
            }
        }
    }
    ValueTakingFlags { longs, shorts }
}

/// The set of `run`-subcommand flags that consume the next argv
/// token as their value. Used by [`split_run_argv_at_separator`].
#[derive(Debug, Clone)]
pub struct ValueTakingFlags {
    longs: std::collections::HashSet<String>,
    shorts: std::collections::HashSet<char>,
}

impl ValueTakingFlags {
    /// Build the table from the static `Nexus` command tree.
    pub fn from_nexus() -> Self {
        value_taking_run_flags()
    }

    /// True when the token names a flag that consumes the next argv
    /// token as its value. Recognizes `--long`, `-x`, and `-xVALUE`
    /// shorthand; rejects `--long=VALUE` (which is self-contained
    /// and handled separately by the scanner) and tokens like `-5`
    /// (negative numbers; short flags must begin with a letter).
    fn token_takes_next(&self, tok: &str) -> bool {
        if !looks_like_flag(tok) {
            return false;
        }
        if let Some(name) = tok.strip_prefix("--") {
            if name.contains('=') {
                return false;
            }
            return self.longs.contains(tok);
        }
        if let Some(rest) = tok.strip_prefix('-') {
            // `-x` (single char) or `-xVALUE` (concatenated form).
            // The concatenated form embeds the value in the same
            // token, so it doesn't consume the next one.
            if rest.len() == 1 {
                return self.shorts.contains(&rest.chars().next().unwrap());
            }
            return false;
        }
        false
    }
}

/// True when `tok` looks like a POSIX-style option flag (either
/// `--long`, `--long=value`, or `-X` where `X` is a letter).
/// Returns `false` for tokens that look like negative numbers
/// (`-5`, `-3.14`) so the scanner routes them to the positional
/// stream instead of consuming them as flags. Short flags are
/// required to begin with a letter; the compiler is expected to
/// reject digit-prefixed short flags in user docstrings so this
/// rule never conflicts with a user-declared option.
pub fn looks_like_flag(tok: &str) -> bool {
    let bytes = tok.as_bytes();
    if bytes.len() < 2 || bytes[0] != b'-' {
        return false;
    }
    if bytes[1] == b'-' {
        // Long form (`--foo` / `--foo=value`). Anything beyond the
        // leading `--` is treated as the option name regardless of
        // its first character; user-defined long names are forbidden
        // by morloc syntax from leading with a digit, so this is a
        // safe relaxation.
        return true;
    }
    // Short form: the leading char after `-` decides. Letters are
    // option chars; digits are negative-number indicators.
    bytes[1].is_ascii_alphabetic()
}

/// Split a `morloc-nexus run` argv tail (everything after `run`)
/// into a nexus zone and a user zone.
///
/// The scanner walks `tail` left-to-right, consuming each token
/// according to clap-derive semantics: a known value-taking flag
/// like `--log-dir` consumes the next token as its value; a
/// `--key=value` form is self-contained; any other token starting
/// with `-` is treated as a boolean flag (single token); any other
/// token is a positional.
///
/// The zone boundary is determined by whichever of these comes
/// first:
///
/// - An unconsumed `@` token: the explicit separator. Tokens
///   before it form the nexus zone; tokens after it form the user
///   zone.
/// - The second non-flag positional: the implicit terminator. The
///   first positional is the wrapper target and belongs to the
///   nexus zone; the second is the start of the user zone (either
///   a subcommand name in multi-export mode or the first user
///   argument in single-export mode).
///
/// If the tail contains zero or one positionals and no `@`, the
/// entire tail is the nexus zone and the user zone is empty.
pub fn split_run_argv_at_separator(
    tail: &[String],
    flags: &ValueTakingFlags,
) -> (Vec<String>, Vec<String>) {
    let mut i = 0;
    let mut target_seen = false;
    while i < tail.len() {
        let tok = &tail[i];
        // Unconsumed `@` is the explicit separator.
        if tok == "@" {
            return (tail[..i].to_vec(), tail[i + 1..].to_vec());
        }
        // Once the wrapper target has been consumed, a help
        // request (`-h` / `--help`) is a request for the program's
        // manifest-driven help, not for the nexus's static help.
        // Route it to the user zone so the manifest-driven parser
        // can render per-program help showing the exported
        // commands and their arguments.
        if target_seen && (tok == "-h" || tok == "--help") {
            return (tail[..i].to_vec(), tail[i..].to_vec());
        }
        // Known value-taking flag consumes the next token.
        if flags.token_takes_next(tok) {
            i += 2;
            continue;
        }
        // Other flag-shaped token (boolean flag, --key=value,
        // short-with-embedded-value, or unknown flag): single
        // token. Negative numbers (`-5`, `-3.14`) are excluded by
        // [`looks_like_flag`] and fall through to the positional
        // branch below.
        if looks_like_flag(tok) {
            i += 1;
            continue;
        }
        // Non-flag positional.
        if !target_seen {
            // First positional is the wrapper target.
            target_seen = true;
            i += 1;
            continue;
        }
        // Second positional is the implicit terminator. The nexus
        // zone ends here; the user zone starts at this token.
        return (tail[..i].to_vec(), tail[i..].to_vec());
    }
    // Walked off the end with no implicit or explicit terminator.
    // Everything is in the nexus zone.
    (tail.to_vec(), Vec::new())
}

/// Outcome of the top-level parse, after capability augmentation
/// and manifest loading (when a target is present).
pub struct ParsedInvocation {
    pub nexus: Nexus,
    /// Manifest loaded from the target. `None` for router mode and
    /// for help-style invocations without a target.
    pub manifest: Option<morloc_manifest::Manifest>,
    /// Canonicalized manifest path. Empty when no target was loaded.
    pub manifest_path: String,
    /// User zone from the `@` split (populated only for `run`).
    pub user_zone: Vec<String>,
    /// Capability flag values extracted from the augmented parse.
    pub capability_values: CapabilityValues,
}

/// Container for values of capability-gated nexus flags resolved
/// from the augmented clap parse. Fields correspond 1:1 with
/// entries in [`CAPABILITY_FLAGS`].
#[derive(Default, Debug, Clone)]
pub struct CapabilityValues {
    pub debug_cache_depth: Option<u64>,
    pub debug_cache_max: Option<u64>,
    pub debug_recursion_cap: Option<u32>,
}

/// Find the first non-flag positional in a pre-scanned nexus tail
/// (everything after `run` / `daemon` in argv). Used to extract the
/// wrapper target before the manifest can be loaded. Returns the
/// token's value, not its index.
fn first_positional<'a>(
    tail: &'a [String],
    flags: &ValueTakingFlags,
) -> Option<&'a str> {
    let mut i = 0;
    while i < tail.len() {
        let tok = &tail[i];
        if flags.token_takes_next(tok) {
            i += 2;
            continue;
        }
        if looks_like_flag(tok) {
            i += 1;
            continue;
        }
        if tok == "@" {
            return None;
        }
        return Some(tok.as_str());
    }
    None
}

/// Top-level argv parse. Routes through one of three paths:
///
/// 1. **Router or top-level help.** Standard clap-derive parse via
///    [`Nexus::parse`]. No manifest is loaded; no capability flags
///    apply.
/// 2. **Run mode.** Pre-scans argv to split nexus / user zones at
///    the `@` separator (or the implicit second-positional
///    terminator). Loads the manifest from the wrapper target,
///    checks the nexus zone for any capability-gated flag whose
///    capability isn't advertised (friendly error), then parses
///    the nexus zone against the `run` subcommand augmented with
///    the advertised capability flags. The augmented help shows
///    only the supported capability flags; top-level
///    `morloc-nexus run --help` (no target) shows all of them.
/// 3. **Daemon mode.** Same as run minus the `@` split (daemon
///    mode has no user zone).
pub fn parse_invocation() -> ParsedInvocation {
    let argv: Vec<String> = std::env::args().collect();
    let mode_str = argv.get(1).map(|s| s.as_str());
    match mode_str {
        Some("run") => parse_run_or_daemon(&argv, true),
        Some("daemon") => parse_run_or_daemon(&argv, false),
        _ => {
            use clap::{CommandFactory, FromArgMatches};
            let cmd = crate::help::strip_styles_recursively(Nexus::command());
            let matches = cmd.get_matches();
            let nexus = Nexus::from_arg_matches(&matches)
                .unwrap_or_else(|e| e.exit());
            ParsedInvocation {
                nexus,
                manifest: None,
                manifest_path: String::new(),
                user_zone: Vec::new(),
                capability_values: CapabilityValues::default(),
            }
        }
    }
}

fn parse_run_or_daemon(argv: &[String], is_run: bool) -> ParsedInvocation {
    use clap::{CommandFactory, FromArgMatches};
    let argv0 = argv
        .first()
        .cloned()
        .unwrap_or_else(|| "morloc-nexus".to_string());
    let mode_token = if is_run { "run" } else { "daemon" };
    let flags = ValueTakingFlags::from_nexus();

    // Pre-scan: in run mode, split at `@` / implicit terminator; in
    // daemon mode there's no user zone, so the entire tail is nexus.
    let (nexus_tail, user_zone): (Vec<String>, Vec<String>) = if is_run {
        let (n, u) = split_run_argv_at_separator(&argv[2..], &flags);
        (n, u)
    } else {
        (argv[2..].to_vec(), Vec::new())
    };

    // Try to find the wrapper target so we can load the manifest
    // before parsing. A missing target is legal for help-style
    // invocations (`morloc-nexus run --help`); we fall back to
    // "all capabilities visible" in that case.
    let target_str = first_positional(&nexus_tail, &flags);
    let (manifest, manifest_path) = match target_str {
        Some(target) => {
            let resolved = if !is_run {
                match resolve_daemon_target(target) {
                    Ok(p) => p,
                    Err(e) => {
                        eprintln!("Error: {}", e);
                        std::process::exit(1);
                    }
                }
            } else {
                target.to_string()
            };
            let payload = match crate::manifest::read_manifest_payload(&resolved) {
                Ok(p) => p,
                Err(e) => {
                    eprintln!("Failed to load manifest '{}': {}", resolved, e);
                    std::process::exit(1);
                }
            };
            let manifest = match crate::manifest::parse_manifest(&payload) {
                Ok(m) => m,
                Err(e) => {
                    eprintln!("Failed to parse manifest '{}': {}", resolved, e);
                    std::process::exit(1);
                }
            };
            // Friendly error for capability-gated flags the manifest
            // doesn't advertise. Fires before the augmented clap
            // parse so the user sees a domain-specific message.
            check_unsupported_capability_flags(&nexus_tail, &manifest.capabilities);
            (Some(manifest), resolved)
        }
        None => (None, String::new()),
    };

    let caps_for_augment: Option<&[String]> = manifest
        .as_ref()
        .map(|m| m.capabilities.as_slice());

    // Build the augmented Nexus command tree: take the static
    // clap-derive command and inject capability flags into the
    // chosen subcommand based on `caps_for_augment`.
    let mut nexus_cmd = crate::help::strip_styles_recursively(Nexus::command());
    {
        let sub = nexus_cmd
            .find_subcommand_mut(mode_token)
            .expect("Nexus declares this subcommand");
        let augmented = augment_with_capability_flags(sub.clone(), caps_for_augment);
        *sub = augmented;
    }

    // Synthesize argv for the augmented command and parse.
    let mut full_argv = Vec::with_capacity(2 + nexus_tail.len());
    full_argv.push(argv0);
    full_argv.push(mode_token.to_string());
    full_argv.extend(nexus_tail);

    let matches = nexus_cmd
        .try_get_matches_from(full_argv)
        .unwrap_or_else(|e| e.exit());
    let nexus = Nexus::from_arg_matches(&matches).unwrap_or_else(|e| {
        eprintln!("Internal parse error: {}", e);
        std::process::exit(1);
    });

    // Extract capability values from the augmented matches.
    let mut capability_values = CapabilityValues::default();
    if let Some(sub_matches) = matches.subcommand_matches(mode_token) {
        capability_values =
            extract_capability_values_from(sub_matches);
    }

    ParsedInvocation {
        nexus,
        manifest,
        manifest_path,
        user_zone,
        capability_values,
    }
}

/// Read every capability-gated flag's value out of one subcommand's
/// `ArgMatches`. Hidden flags whose capability isn't advertised
/// have already been intercepted by
/// [`check_unsupported_capability_flags`]; whatever's present here
/// is supported.
fn extract_capability_values_from(matches: &clap::ArgMatches) -> CapabilityValues {
    let mut out = CapabilityValues::default();
    for spec in CAPABILITY_FLAGS {
        // `contains_id` will panic if the id isn't declared. Use
        // `try_contains_id` via `get_one` patterns that gracefully
        // skip unrecognized ids.
        match spec.long {
            "debug-cache-depth" => {
                out.debug_cache_depth =
                    matches.get_one::<u64>(spec.long).copied();
            }
            "debug-recursion-cap" => {
                out.debug_recursion_cap =
                    matches.get_one::<u32>(spec.long).copied();
            }
            "debug-cache-max" => {
                if let Some(s) = matches.get_one::<String>(spec.long) {
                    out.debug_cache_max =
                        Some(parse_byte_size("--debug-cache-max", s));
                }
            }
            _ => {
                panic!(
                    "capability flag --{} has no extractor; \
                     update extract_capability_values_from",
                    spec.long
                );
            }
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use clap::Parser;

    /// Parse a Nexus invocation from a vector of args (including
    /// argv[0]) so tests don't depend on the environment.
    fn parse(args: &[&str]) -> Result<Nexus, clap::Error> {
        Nexus::try_parse_from(args.iter().copied())
    }

    #[test]
    fn run_minimal() {
        let n = parse(&["morloc-nexus", "run", "main.manifest"]).unwrap();
        match n.cmd {
            Mode::Run(r) => {
                assert_eq!(r.target, "main.manifest");
                assert!(!r.common.print);
                assert!(r.common.output_path.is_none());
            }
            _ => panic!("expected Run mode"),
        }
    }

    #[test]
    fn run_with_dispatch_options_before_target() {
        // Nexus options before the target are absorbed by clap-derive
        // into `RunArgs::common`.
        let n = parse(&[
            "morloc-nexus", "run", "-o", "out.json", "-p", "main.manifest",
        ])
        .unwrap();
        match n.cmd {
            Mode::Run(r) => {
                assert_eq!(r.target, "main.manifest");
                assert_eq!(r.common.output_path.as_deref(), Some("out.json"));
                assert!(r.common.print);
            }
            _ => panic!("expected Run mode"),
        }
    }

    // ------------------------------------------------------------
    // Pre-scan: the `@` separator and value-taking-flag handling
    // ------------------------------------------------------------

    fn s(s: &str) -> String {
        s.to_string()
    }

    #[test]
    fn split_no_at_no_user_zone_after_target() {
        // `run ./prog` -> nexus zone ["./prog"], user zone [].
        let flags = ValueTakingFlags::from_nexus();
        let (nx, uz) = split_run_argv_at_separator(&[s("./prog")], &flags);
        assert_eq!(nx, vec![s("./prog")]);
        assert!(uz.is_empty());
    }

    #[test]
    fn split_no_at_user_zone_after_target() {
        // `run ./prog 7 8` -> nexus ["./prog"], user ["7", "8"].
        let flags = ValueTakingFlags::from_nexus();
        let (nx, uz) =
            split_run_argv_at_separator(&[s("./prog"), s("7"), s("8")], &flags);
        assert_eq!(nx, vec![s("./prog")]);
        assert_eq!(uz, vec![s("7"), s("8")]);
    }

    #[test]
    fn split_at_separates_zones() {
        // `run ./prog @ 7 8` -> nexus ["./prog"], user ["7", "8"].
        let flags = ValueTakingFlags::from_nexus();
        let (nx, uz) = split_run_argv_at_separator(
            &[s("./prog"), s("@"), s("7"), s("8")],
            &flags,
        );
        assert_eq!(nx, vec![s("./prog")]);
        assert_eq!(uz, vec![s("7"), s("8")]);
    }

    #[test]
    fn split_at_with_pre_target_nexus_opts() {
        // `run --log-dir L ./prog @ 7` -> nexus zone has the opts +
        // target; user zone is ["7"].
        let flags = ValueTakingFlags::from_nexus();
        let (nx, uz) = split_run_argv_at_separator(
            &[s("--log-dir"), s("L"), s("./prog"), s("@"), s("7")],
            &flags,
        );
        assert_eq!(nx, vec![s("--log-dir"), s("L"), s("./prog")]);
        assert_eq!(uz, vec![s("7")]);
    }

    #[test]
    fn split_at_consumed_by_flag_is_value_not_separator() {
        // `run --log-dir @ ./prog @ 7` -> first `@` is the value
        // of `--log-dir`; second `@` is the separator.
        let flags = ValueTakingFlags::from_nexus();
        let (nx, uz) = split_run_argv_at_separator(
            &[
                s("--log-dir"),
                s("@"),
                s("./prog"),
                s("@"),
                s("7"),
            ],
            &flags,
        );
        assert_eq!(
            nx,
            vec![s("--log-dir"), s("@"), s("./prog")]
        );
        assert_eq!(uz, vec![s("7")]);
    }

    #[test]
    fn split_short_value_taking_flag_consumes_next() {
        // `-o` is value-taking; `-o file ./prog @ 7` -> the file
        // token is the value of `-o`, not the target.
        let flags = ValueTakingFlags::from_nexus();
        let (nx, uz) = split_run_argv_at_separator(
            &[s("-o"), s("file"), s("./prog"), s("@"), s("7")],
            &flags,
        );
        assert_eq!(nx, vec![s("-o"), s("file"), s("./prog")]);
        assert_eq!(uz, vec![s("7")]);
    }

    #[test]
    fn split_long_equals_form_is_self_contained() {
        // `--log-dir=L` is one token; the next token is the target.
        let flags = ValueTakingFlags::from_nexus();
        let (nx, uz) = split_run_argv_at_separator(
            &[s("--log-dir=L"), s("./prog"), s("@"), s("7")],
            &flags,
        );
        assert_eq!(nx, vec![s("--log-dir=L"), s("./prog")]);
        assert_eq!(uz, vec![s("7")]);
    }

    #[test]
    fn split_at_alone_means_empty_nexus_zone() {
        // `run @ 7 8` -> empty nexus zone, user zone ["7","8"].
        // Covers the shell-variable case (`./prog $opts @ 7 8`
        // where `$opts` is empty).
        let flags = ValueTakingFlags::from_nexus();
        let (nx, uz) = split_run_argv_at_separator(
            &[s("@"), s("7"), s("8")],
            &flags,
        );
        assert!(nx.is_empty());
        assert_eq!(uz, vec![s("7"), s("8")]);
    }

    #[test]
    fn split_at_at_end_leaves_empty_user_zone() {
        // `run ./prog -p @` -> nexus zone has the opts + target;
        // user zone is empty.
        let flags = ValueTakingFlags::from_nexus();
        let (nx, uz) = split_run_argv_at_separator(
            &[s("./prog"), s("-p"), s("@")],
            &flags,
        );
        assert_eq!(nx, vec![s("./prog"), s("-p")]);
        assert!(uz.is_empty());
    }

    #[test]
    fn split_post_target_at_separates_correctly() {
        // `run ./prog --log-dir L @ 7` -> the `--log-dir L` is in
        // the post-target nexus zone (before `@`); user zone ["7"].
        let flags = ValueTakingFlags::from_nexus();
        let (nx, uz) = split_run_argv_at_separator(
            &[s("./prog"), s("--log-dir"), s("L"), s("@"), s("7")],
            &flags,
        );
        assert_eq!(nx, vec![s("./prog"), s("--log-dir"), s("L")]);
        assert_eq!(uz, vec![s("7")]);
    }

    #[test]
    fn split_user_zone_at_is_literal() {
        // Once we're past the separator, additional `@` tokens are
        // user-zone literals.
        let flags = ValueTakingFlags::from_nexus();
        let (nx, uz) = split_run_argv_at_separator(
            &[s("./prog"), s("@"), s("@"), s("7")],
            &flags,
        );
        assert_eq!(nx, vec![s("./prog")]);
        assert_eq!(uz, vec![s("@"), s("7")]);
    }

    #[test]
    fn split_implicit_terminator_with_nexus_opts_then_subcmd() {
        // `run ./prog --log-dir L cmd 7 8` -> the nexus zone holds
        // the target plus the value-taking `--log-dir L`; the
        // second positional (`cmd`) is the implicit terminator and
        // begins the user zone.
        let flags = ValueTakingFlags::from_nexus();
        let (nx, uz) = split_run_argv_at_separator(
            &[
                s("./prog"),
                s("--log-dir"),
                s("L"),
                s("cmd"),
                s("7"),
                s("8"),
            ],
            &flags,
        );
        assert_eq!(nx, vec![s("./prog"), s("--log-dir"), s("L")]);
        assert_eq!(uz, vec![s("cmd"), s("7"), s("8")]);
    }

    #[test]
    fn split_implicit_terminator_no_nexus_opts() {
        // `run ./prog 7 8` -> target, then the first positional
        // after target is the implicit terminator.
        let flags = ValueTakingFlags::from_nexus();
        let (nx, uz) = split_run_argv_at_separator(
            &[s("./prog"), s("7"), s("8")],
            &flags,
        );
        assert_eq!(nx, vec![s("./prog")]);
        assert_eq!(uz, vec![s("7"), s("8")]);
    }

    #[test]
    fn split_implicit_terminator_with_boolean_flag() {
        // `run ./prog -p cmd 7` -> `-p` is a boolean nexus flag, so
        // it stays in the nexus zone; the implicit terminator is
        // at `cmd`.
        let flags = ValueTakingFlags::from_nexus();
        let (nx, uz) = split_run_argv_at_separator(
            &[s("./prog"), s("-p"), s("cmd"), s("7")],
            &flags,
        );
        assert_eq!(nx, vec![s("./prog"), s("-p")]);
        assert_eq!(uz, vec![s("cmd"), s("7")]);
    }

    #[test]
    fn split_post_target_short_help_routes_to_user_zone() {
        // `./prog -h` -> after the wrapper exec becomes
        // `morloc-nexus run ./prog -h`. The `-h` belongs to the
        // program (manifest-driven help), not to the nexus.
        let flags = ValueTakingFlags::from_nexus();
        let (nx, uz) = split_run_argv_at_separator(
            &[s("./prog"), s("-h")],
            &flags,
        );
        assert_eq!(nx, vec![s("./prog")]);
        assert_eq!(uz, vec![s("-h")]);
    }

    #[test]
    fn split_post_target_long_help_routes_to_user_zone() {
        let flags = ValueTakingFlags::from_nexus();
        let (nx, uz) = split_run_argv_at_separator(
            &[s("./prog"), s("--help")],
            &flags,
        );
        assert_eq!(nx, vec![s("./prog")]);
        assert_eq!(uz, vec![s("--help")]);
    }

    #[test]
    fn split_post_target_help_after_nexus_opts() {
        // `./prog --log-dir L -h` -> the `--log-dir L` is consumed
        // in the nexus zone; the `-h` flips into the user zone.
        let flags = ValueTakingFlags::from_nexus();
        let (nx, uz) = split_run_argv_at_separator(
            &[s("./prog"), s("--log-dir"), s("L"), s("-h")],
            &flags,
        );
        assert_eq!(nx, vec![s("./prog"), s("--log-dir"), s("L")]);
        assert_eq!(uz, vec![s("-h")]);
    }

    #[test]
    fn split_negative_number_after_subcommand_in_user_zone() {
        // `./prog cmd -5` -> the implicit terminator at `cmd`
        // sends `cmd -5` to the user zone; `-5` must NOT be
        // mistaken for a short flag (POSIX short flags begin with
        // a letter).
        let flags = ValueTakingFlags::from_nexus();
        let (nx, uz) = split_run_argv_at_separator(
            &[s("./prog"), s("cmd"), s("-5")],
            &flags,
        );
        assert_eq!(nx, vec![s("./prog")]);
        assert_eq!(uz, vec![s("cmd"), s("-5")]);
    }

    #[test]
    fn split_negative_decimal_is_not_a_flag() {
        let flags = ValueTakingFlags::from_nexus();
        let (nx, uz) = split_run_argv_at_separator(
            &[s("./prog"), s("cmd"), s("-3.14")],
            &flags,
        );
        assert_eq!(nx, vec![s("./prog")]);
        assert_eq!(uz, vec![s("cmd"), s("-3.14")]);
    }

    #[test]
    fn looks_like_flag_letter_vs_digit() {
        assert!(looks_like_flag("-h"));
        assert!(looks_like_flag("-A"));
        assert!(looks_like_flag("--help"));
        assert!(looks_like_flag("--log-dir"));
        assert!(looks_like_flag("--key=value"));
        assert!(!looks_like_flag("-5"));
        assert!(!looks_like_flag("-3.14"));
        assert!(!looks_like_flag("-"));
        assert!(!looks_like_flag(""));
        assert!(!looks_like_flag("abc"));
        assert!(!looks_like_flag("@"));
    }

    #[test]
    fn split_pre_target_help_stays_in_nexus_zone() {
        // `morloc-nexus run -h` -- no target yet -- means show the
        // top-level run help. Stays in the nexus zone for
        // clap-derive to handle.
        let flags = ValueTakingFlags::from_nexus();
        let (nx, uz) = split_run_argv_at_separator(&[s("-h")], &flags);
        assert_eq!(nx, vec![s("-h")]);
        assert!(uz.is_empty());
    }

    #[test]
    fn split_explicit_at_overrides_implicit_terminator() {
        // `run ./prog @ 7 cmd 8` -> the explicit `@` separates
        // here; subsequent positionals (`7`, `cmd`, `8`) are all in
        // the user zone, even though `cmd` looks like a subcommand
        // name.
        let flags = ValueTakingFlags::from_nexus();
        let (nx, uz) = split_run_argv_at_separator(
            &[s("./prog"), s("@"), s("7"), s("cmd"), s("8")],
            &flags,
        );
        assert_eq!(nx, vec![s("./prog")]);
        assert_eq!(uz, vec![s("7"), s("cmd"), s("8")]);
    }

    #[test]
    fn daemon_listener_options() {
        let n = parse(&[
            "morloc-nexus",
            "daemon",
            "--http-port",
            "8888",
            "main.manifest",
        ])
        .unwrap();
        match n.cmd {
            Mode::Daemon(d) => {
                let (cfg, target) = daemon_args_to_config(&d);
                assert_eq!(target, "main.manifest");
                assert!(cfg.daemon_flag);
                assert_eq!(cfg.http_port, Some(8888));
                assert_eq!(cfg.eval_timeout, 30);
            }
            _ => panic!("expected Daemon mode"),
        }
    }

    #[test]
    fn router_default_eval_timeout() {
        let n = parse(&["morloc-nexus", "router", "--fdb", "/tmp/fdb"]).unwrap();
        match n.cmd {
            Mode::Router(r) => {
                let cfg = router_args_to_config(&r);
                assert!(cfg.router_flag);
                assert_eq!(cfg.fdb_path.as_deref(), Some("/tmp/fdb"));
                assert_eq!(cfg.eval_timeout, 30);
            }
            _ => panic!("expected Router mode"),
        }
    }

    #[test]
    fn output_form_value_enum() {
        // `--output-form` before the target is consumed by
        // clap-derive into `RunArgs::common`.
        let n = parse(&[
            "morloc-nexus",
            "run",
            "--output-form",
            "mpk",
            "main.manifest",
        ])
        .unwrap();
        match n.cmd {
            Mode::Run(r) => {
                let (cfg, target) = run_args_to_config(&r);
                assert_eq!(target, "main.manifest");
                assert_eq!(cfg.output_format, OutputFormat::MessagePack);
            }
            _ => panic!("expected Run mode"),
        }
    }

    #[test]
    fn no_subcommand_is_an_error() {
        // arg_required_else_help means the empty invocation errors
        // and clap returns a non-zero exit with the auto-generated
        // help text.
        let err = parse(&["morloc-nexus"]).unwrap_err();
        assert_eq!(
            err.kind(),
            clap::error::ErrorKind::DisplayHelpOnMissingArgumentOrSubcommand
        );
    }

    #[test]
    fn unknown_subcommand_is_an_error() {
        let err = parse(&["morloc-nexus", "frobnicate"]).unwrap_err();
        // clap reports this as InvalidSubcommand.
        assert_eq!(
            err.kind(),
            clap::error::ErrorKind::InvalidSubcommand
        );
    }

    // ----------------------------------------------------------------
    // Daemon target resolution
    // ----------------------------------------------------------------

    use std::io::Write;

    /// Write a file under a temp dir and return its path. Caller
    /// owns the `tempfile::TempDir` so the file outlives the test.
    fn write_tmp(dir: &std::path::Path, name: &str, content: &[u8]) -> String {
        let p = dir.join(name);
        let mut f = std::fs::File::create(&p).unwrap();
        f.write_all(content).unwrap();
        p.to_string_lossy().into_owned()
    }

    /// Build a synthetic wrapper that starts with `#!/bin/sh` plus
    /// the sentinel comment plus the manifest marker. The exact
    /// payload after `### MANIFEST ###` doesn't matter for the
    /// resolver -- it only inspects the head.
    fn wrapper_head() -> Vec<u8> {
        format!(
            "#!/bin/sh\n{}{}\nexec morloc-nexus run \"$0\" \"$@\"\n### MANIFEST ###\n{{}}\n",
            WRAPPER_SENTINEL, "0.0.0"
        )
        .into_bytes()
    }

    #[test]
    fn resolves_dot_manifest_unconditionally() {
        // A `.manifest` extension is trusted without reading the
        // file -- the parser later in main() catches any I/O or
        // shape problems.
        let result = resolve_daemon_target("/nonexistent/path/to/file.manifest");
        assert_eq!(result.as_deref(), Ok("/nonexistent/path/to/file.manifest"));
    }

    #[test]
    fn resolves_wrapper_via_sentinel() {
        let tmp = std::env::temp_dir().join(format!(
            "morloc-nexus-resolve-{}",
            std::process::id()
        ));
        std::fs::create_dir_all(&tmp).unwrap();
        let path = write_tmp(&tmp, "morloc-prog", &wrapper_head());
        let result = resolve_daemon_target(&path).unwrap();
        assert_eq!(result, path);
        std::fs::remove_dir_all(&tmp).ok();
    }

    #[test]
    fn rejects_random_shell_script() {
        let tmp = std::env::temp_dir().join(format!(
            "morloc-nexus-reject-shell-{}",
            std::process::id()
        ));
        std::fs::create_dir_all(&tmp).unwrap();
        let path = write_tmp(&tmp, "not-morloc", b"#!/bin/sh\necho hi\n");
        let err = resolve_daemon_target(&path).unwrap_err();
        assert!(
            err.contains("not a morloc wrapper or manifest file"),
            "unexpected error: {}",
            err
        );
        std::fs::remove_dir_all(&tmp).ok();
    }

    #[test]
    fn rejects_missing_file() {
        let err = resolve_daemon_target("/definitely/does/not/exist").unwrap_err();
        assert!(err.contains("cannot open"), "unexpected error: {}", err);
    }
}
