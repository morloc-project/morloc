mod bridge;
mod config;
mod container;
mod doctor;
mod dockerfile;
mod environment;
mod error;
mod freeze;
mod hostprobe;
mod provision;
mod runner;
mod selinux;
mod serve;
mod types;

// The dependency-resolution kernel (envspec/pixi/constraint/langsupport) lives in
// the shared `morloc-deps` crate; re-export it so existing `crate::<mod>` paths
// resolve unchanged.
pub(crate) use morloc_deps::{constraint, envspec, envstore, langsupport, pixi};

use std::collections::HashSet;
use std::fs;
use std::io::{self, IsTerminal, Write};
use std::os::unix::process::CommandExt;
use std::process::{Command, ExitCode, Stdio};

use clap::builder::styling::Style;
use clap::{CommandFactory, FromArgMatches, Parser, Subcommand, ValueEnum};

use crate::config as cfg;
use crate::container::{container_run_passthrough, RunConfig};
use crate::error::{ManagerError, Result};
use crate::selinux::{detect_selinux, volume_suffix, SELinuxMode};
use crate::types::*;

/// Path under the user's home that morloc-manager exports as
/// `MORLOC_BIN_LINK_DIR` to the in-container `morloc init`. The Haskell side
/// (SystemConfig.hs) symlinks newly installed nexus/manager binaries here so
/// they end up on PATH (see the comment in `run_with_config`). Kept as a
/// relative path because the absolute form depends on the in-container $HOME,
/// which morloc-manager computes per-invocation.
const MORLOC_BIN_LINK_REL: &str = ".local/share/morloc/bin";

/// Fixed in-container path the SLURM bridge socket is bind-mounted to.
/// libmorloc.so reads this via `MORLOC_BRIDGE_SOCKET` (set when
/// `morloc-manager run --slurm-bridge` is in effect).
const BRIDGE_SOCK_IN_CONTAINER: &str = "/run/morloc-bridge.sock";

// ======================================================================
// CLI types
// ======================================================================

fn build_help_template() -> String {
    let b = Style::new().bold().render();
    let bu = Style::new().bold().underline().render();
    let r = "\x1b[0m"; // full ANSI reset

    format!(
        "\
{{name}} - {{about}}

{{usage-heading}} {{usage}}

{bu}Development{r}
  {b}setup{r}      Configure the default container engine
  {b}new{r}        Build a new morloc environment
  {b}run{r}        Run a command in the active environment
  {b}rm{r}         Remove a morloc environment
  {b}ls{r}         List morloc environments
  {b}info{r}       Show configuration and installed environments
  {b}select{r}     Select an environment
  {b}update{r}     Rebuild an environment
  {b}nuke{r}       Remove all morloc environments

{bu}Serving{r}
  {b}install{r}    Build and install a module into the active environment
  {b}expose{r}     Choose which installed modules are served (MCP/API/eval)
  {b}start{r}      Serve an environment over the network
  {b}eval{r}       Evaluate a morloc expression against a serve container
  {b}status{r}     List running serve containers
  {b}stop{r}       Stop a running serve container
  {b}logs{r}       Stream logs from a running serve container

{bu}Deployment{r}
  {b}freeze{r}     Export installed state as a frozen artifact
  {b}unfreeze{r}   Build a portable serve image from frozen state
  {b}doctor{r}     Check environment health and diagnose issues

{bu}Options{r}
{{options}}"
    )
}

#[derive(Parser)]
#[command(name = "morloc-manager")]
#[command(about = "container lifecycle manager for Morloc")]
#[command(long_about = "Manage containerized Morloc installations, dependency layers, and deployments")]
#[command(disable_version_flag = true)]
#[command(arg_required_else_help = true)]
#[command(hide_possible_values = true)]
#[command(term_width = 80)]
struct Cli {
    /// Print container commands to stderr before executing
    #[arg(short, long, global = true)]
    verbose: bool,

    /// Output machine-readable JSON instead of human-readable text
    #[arg(long, global = true)]
    json: bool,

    /// Print version and exit
    #[arg(long)]
    version: bool,

    #[command(subcommand)]
    command: Option<Cmd>,
}

#[derive(Subcommand)]
enum Cmd {
    // -- Development --
    /// Configure the default container engine
    #[command(display_order = 0)]
    #[command(after_help = "Examples:\n  morloc-manager setup --engine podman\n  morloc-manager setup --engine docker\n  morloc-manager setup --engine apptainer\n  sudo morloc-manager setup --engine podman --system")]
    Setup {
        /// Container engine: podman, docker, apptainer, or singularity
        #[arg(long, value_enum)]
        engine: Option<EngineArg>,
        /// Apply to system scope (requires root)
        #[arg(long)]
        system: bool,
    },
    /// Build a new morloc environment
    #[command(display_order = 1)]
    #[command(after_help = "Examples:\n  morloc-manager new\n  morloc-manager new myenv --lang py@3.12\n  morloc-manager new myenv --engine podman\n\nThe environment's toolchain is provisioned from its requirements via pixi:\nnative by default on a capable host, or a requirement-derived container image\nwith --engine (podman/docker/apptainer). No hand-authored recipes.")]
    New {
        /// Environment name (default: derived from the morloc version)
        name: Option<String>,
        /// Language toolchain(s) to provision: `lang` or `lang@version`
        /// (e.g. --lang py@3.12 --lang r). Repeatable or comma-separated.
        #[arg(long)]
        lang: Vec<String>,
        /// Backend/engine: podman, docker, apptainer, singularity, or none (native).
        #[arg(long, value_enum)]
        engine: Option<EngineArg>,
        /// Extra OS package to bake into the image (repeatable), e.g.
        /// --system-package jq. Container backend only.
        #[arg(long = "system-package")]
        system_package: Vec<String>,
        /// Create in system scope (requires root)
        #[arg(long)]
        system: bool,
        /// Skip provisioning + morloc init after creation
        #[arg(long)]
        no_init: bool,
        /// Skip interactive prompts, use defaults
        #[arg(long)]
        non_interactive: bool,
    },
    /// Run a command in the active environment
    #[command(display_order = 2)]
    #[command(after_help = "\
Examples:
  morloc-manager run -- morloc --version
  morloc-manager run -- morloc make -o svc svc.loc
  morloc-manager run -- morloc install math
  morloc-manager run --shell

Use -- to separate morloc-manager flags from the container command.
Without --, flags like --version are interpreted by morloc-manager itself.")]
    Run {
        /// Command to run inside the container
        command: Vec<String>,
        /// Start an interactive shell
        #[arg(long)]
        shell: bool,
        /// Pass environment variable to the container (KEY=VALUE)
        #[arg(short, long = "env")]
        env_vars: Vec<String>,
        /// Read environment variables from a file (one KEY=VALUE per line)
        #[arg(long)]
        env_file: Option<String>,
        /// One-shot engine flag, appended to env.flags.yaml `run.<engine>`
        /// for this invocation only (repeatable; not persisted)
        #[arg(short = 'x', long = "engine-arg", allow_hyphen_values = true)]
        engine_arg: Vec<String>,
        /// Expose a SLURM submission bridge inside the container so
        /// labeled remote calls (`big:fn x`) can submit jobs to the
        /// host's sbatch. Requires the active environment to use the
        /// Apptainer engine. Each remote job is launched on its
        /// compute node via `morloc-manager run -- <nexus>
        /// --call-packet ...`, so the same env (same .sif, same
        /// MORLOC_HOME) is used on driver and worker; the
        /// morloc-manager binary must be reachable at the same path
        /// on every compute node (typical: `~/.local/bin` on
        /// NFS-shared $HOME).
        #[arg(long)]
        slurm_bridge: bool,
    },
    /// Remove a morloc environment
    #[command(display_order = 3)]
    #[command(after_help = "Examples:\n  morloc-manager rm myenv\n  sudo morloc-manager rm myenv --system")]
    Rm {
        /// Environment name(s) to remove
        names: Vec<String>,
        /// Remove from system scope (requires root)
        #[arg(long)]
        system: bool,
        /// Remove even if active (deactivates first)
        #[arg(long)]
        force: bool,
    },
    /// Remove all morloc environments
    #[command(display_order = 8)]
    #[command(after_help = "Examples:\n  morloc-manager nuke\n  morloc-manager nuke --yes\n  morloc-manager nuke --images\n  sudo morloc-manager nuke --system\n  sudo morloc-manager nuke --system --images --yes")]
    Nuke {
        /// Remove system-scope environments instead of local (requires root)
        #[arg(long)]
        system: bool,
        /// Also remove base container images
        #[arg(long)]
        images: bool,
        /// Skip confirmation prompt
        #[arg(long)]
        yes: bool,
    },
    /// List morloc environments
    #[command(display_order = 4)]
    #[command(after_help = "Examples:\n  morloc-manager ls\n  morloc-manager ls --system")]
    Ls {
        /// Show only system environments
        #[arg(long)]
        system: bool,
        /// Show only local environments
        #[arg(long)]
        local: bool,
    },
    /// Show configuration and installed environments
    #[command(display_order = 5)]
    #[command(after_help = "Examples:\n  morloc-manager info\n  morloc-manager info myenv")]
    Info {
        /// Environment name (show details for this environment)
        name: Option<String>,
        /// Look up the system-scope environment (when name is shadowed locally)
        #[arg(long)]
        system: bool,
    },
    /// Select an environment
    #[command(display_order = 6)]
    #[command(after_help = "Examples:\n  morloc-manager select myenv\n  sudo morloc-manager select myenv --system")]
    Select {
        /// Environment name
        name: String,
        /// Write to system config instead of local (requires root)
        #[arg(long)]
        system: bool,
    },

    /// Rebuild an environment
    #[command(display_order = 7)]
    #[command(after_help = "Examples:\n  morloc-manager update              # re-solve/rebuild the active environment\n  morloc-manager update myenv\n  morloc-manager update myenv --lang py@3.13   # re-pin, then rebuild\n  morloc-manager update myenv --system-package jq   # add an OS package, then rebuild")]
    Update {
        /// Environment name (default: active environment)
        name: Option<String>,
        /// Re-pin language toolchain(s): `lang` or `lang@version` (repeatable /
        /// comma-separated). Omit to keep the stored pins.
        #[arg(long)]
        lang: Vec<String>,
        /// Extra OS package to bake into the image (repeatable), e.g.
        /// --system-package jq. Added to the environment's persisted set
        /// (additive; not a replace); container backend only.
        #[arg(long = "system-package")]
        system_package: Vec<String>,
        /// Accepted for scripting uniformity with `new` (no effect)
        #[arg(long, hide = true)]
        non_interactive: bool,
    },

    // -- Deployment --
    /// Serve an environment over the network
    #[command(display_order = 20)]
    #[command(after_help = "Examples:\n  morloc-manager start                       # serve the environment's exposed set\n  morloc-manager start myenv -p 9090:8080\n  morloc-manager start --mcp mymodule -p 9000:9000   # serve one module as MCP/HTTP")]
    Start {
        /// Environment name (default: active environment)
        name: Option<String>,
        /// Ad-hoc: serve just this one installed module over MCP, ignoring the
        /// exposed set. Default (no --mcp) serves the environment's exposed
        /// set (managed with `morloc-manager expose`).
        #[arg(long, value_name = "PROGRAM")]
        mcp: Option<String>,
        /// Bearer token required on HTTP requests. Falls back to the
        /// MORLOC_MCP_TOKEN environment variable.
        #[arg(long = "auth-token", value_name = "TOKEN")]
        auth_token: Option<String>,
        /// Expose the MCP server off-loopback (publish on 0.0.0.0 instead of
        /// the default 127.0.0.1). Requires a token and --allow-plaintext.
        #[arg(long)]
        expose: bool,
        /// Acknowledge that an exposed (--expose) MCP endpoint sends its bearer
        /// token and traffic in cleartext (no TLS). Required to expose off-box.
        #[arg(long = "allow-plaintext")]
        allow_plaintext: bool,
        /// Permit an exposed MCP endpoint with no token (an open, unauthenticated
        /// server on the network). Strongly discouraged.
        #[arg(long = "allow-no-auth")]
        allow_no_auth: bool,
        /// DANGEROUS. Serve the loopback MCP endpoint unauthenticated even where
        /// it cannot be confined to the host's loopback (Docker Desktop / podman
        /// machine), leaving it reachable by any container co-resident on the
        /// engine's network. Only use on a host you fully trust.
        #[arg(long = "unsafe")]
        unsafe_serve: bool,
        /// Port mapping HOST:CONTAINER (default: 8080:8080, or 9000:9000 with --mcp)
        #[arg(short, long, value_parser = parse_port)]
        port: Vec<(u16, u16)>,
        /// Pass environment variable to the container (KEY=VALUE)
        #[arg(short, long = "env")]
        env_vars: Vec<String>,
        /// Read environment variables from a file (one KEY=VALUE per line)
        #[arg(long)]
        env_file: Option<String>,
        /// One-shot engine flag, appended to env.flags.yaml `start.<engine>`
        /// for this invocation only (repeatable; not persisted)
        #[arg(short = 'x', long = "engine-arg", allow_hyphen_values = true)]
        engine_arg: Vec<String>,
        /// Replace an already-running serve container
        #[arg(long)]
        force: bool,
    },
    /// Stop a running serve container
    #[command(display_order = 21)]
    #[command(after_help = "Examples:\n  morloc-manager stop              # stop active environment\n  morloc-manager stop myenv")]
    Stop {
        /// Environment name (default: active environment)
        name: Option<String>,
    },
    /// Stream logs from a running serve container
    #[command(display_order = 22)]
    #[command(after_help = "Examples:\n  morloc-manager logs              # logs from only running serve container\n  morloc-manager logs myenv\n  morloc-manager logs -f myenv     # follow mode")]
    Logs {
        /// Environment name (default: auto-detect running container)
        name: Option<String>,
        /// Follow log output
        #[arg(short, long)]
        follow: bool,
    },
    /// Export installed state as a frozen artifact
    #[command(display_order = 23)]
    #[command(after_help = "Examples:\n  morloc-manager freeze\n  morloc-manager freeze myenv\n  morloc-manager freeze -o ./my-freeze\n\nRequires at least one program compiled with 'morloc make --install'.")]
    Freeze {
        /// Environment name (default: active environment)
        name: Option<String>,
        /// Output directory (default: ./morloc-freeze)
        #[arg(short, long)]
        output: Option<String>,
        /// Overwrite existing output directory
        #[arg(long)]
        force: bool,
    },
    /// Build a serve image from frozen state
    #[command(display_order = 24)]
    #[command(after_help = "Examples:\n  morloc-manager unfreeze --from ./morloc-freeze/state.tar.gz -t myservice:v1\n  morloc-manager unfreeze --from ./state.tar.gz -t svc:v1 --engine docker")]
    Unfreeze {
        /// Path to state.tar.gz from freeze
        #[arg(long)]
        from: String,
        /// Image tag
        #[arg(short, long)]
        tag: String,
        /// Base image override
        #[arg(long)]
        base: Option<String>,
        /// Container engine override (default: configured engine).
        /// Images frozen with engine-specific flags may not work with a different engine.
        #[arg(long, value_enum)]
        engine: Option<EngineArg>,
        /// Rebuild image even if it already exists locally
        #[arg(long)]
        rebuild: bool,
    },
    /// Evaluate a morloc expression against a running serve container
    #[command(display_order = 25)]
    #[command(after_help = "Examples:\n  morloc-manager eval 'add 1 2'\n  morloc-manager eval myenv 'map (add 1) [1,2,3]'\n  morloc-manager eval -p 9090 'greet \"world\"'")]
    Eval {
        /// Expression to evaluate (or environment name if two positional args)
        first: String,
        /// Expression to evaluate (when first arg is environment name)
        second: Option<String>,
        /// Port of the serve container (default: 8080)
        #[arg(short, long, default_value = "8080")]
        port: u16,
    },
    /// Build and install a module into the active environment
    #[command(display_order = 4)]
    #[command(after_help = "\
Examples:
  morloc-manager install main.loc              # installs under the module name
  morloc-manager install ./mypkg               # installs a package directory

Sugar for: morloc-manager run -- morloc make --install <file>
       (or: morloc-manager run -- morloc install --build <dir> for a directory)")]
    Install {
        /// Morloc source file (.loc) or package directory to build and install.
        /// A directory is treated as a package (main.loc + package.yaml). The
        /// installed program is named after its module (the `module <name>`
        /// declaration), not the source file, so it is exposed/served by that
        /// module name.
        src: String,
        /// One-shot engine flag, appended to env.flags.yaml `run.<engine>`
        /// for this invocation only (repeatable; not persisted)
        #[arg(short = 'x', long = "engine-arg", allow_hyphen_values = true)]
        engine_arg: Vec<String>,
    },
    /// Declare which installed modules are served, and how (edits state only;
    /// run `start` to realize it). Covers the network views (MCP, API) and the
    /// eval capability; the CLI view is local (`morloc-manager run`).
    #[command(display_order = 23)]
    #[command(after_help = "Examples:\n  morloc-manager expose add dna --as mcp\n  morloc-manager expose add util --as mcp,api\n  morloc-manager expose eval --allow dna,stats\n  morloc-manager expose list\n  morloc-manager expose rm dna")]
    Expose {
        #[command(subcommand)]
        action: ExposeAction,
    },
    /// List running serve containers
    #[command(display_order = 27)]
    #[command(after_help = "Examples:\n  morloc-manager status")]
    Status,
    /// Check environment health and diagnose issues
    #[command(display_order = 26)]
    #[command(after_help = "Examples:\n  morloc-manager doctor\n  morloc-manager doctor myenv\n  morloc-manager doctor --deep")]
    Doctor {
        /// Environment name (default: active)
        name: Option<String>,
        /// Check system-scope environment
        #[arg(long)]
        system: bool,
        /// Run checks inside the container (slower, more thorough)
        #[arg(long)]
        deep: bool,
        /// Treat warnings as errors (non-zero exit on warnings)
        #[arg(long)]
        strict: bool,
        /// Additionally check SLURM-bridge prerequisites (sbatch on
        /// PATH, build.yaml has slurm-support, env image resolvable,
        /// morloc-manager binary path mirrorable, runtime dir
        /// writable). Run this before relying on `--slurm-bridge` on
        /// a new cluster.
        #[arg(long)]
        slurm: bool,
    },
}

#[derive(Subcommand)]
enum ExposeAction {
    /// Add a module to the exposure set over one or more protocols
    Add {
        /// Installed module name
        module: String,
        /// Protocols to expose over (comma-separated: mcp, api)
        #[arg(long = "as", value_enum, value_delimiter = ',', required = true)]
        protocols: Vec<Protocol>,
        /// Environment (default: active)
        #[arg(long)]
        env: Option<String>,
    },
    /// Remove a module from all exposure sets
    Rm {
        /// Module name
        module: String,
        /// Environment (default: active)
        #[arg(long)]
        env: Option<String>,
    },
    /// Show what is exposed
    List {
        /// Environment (default: active)
        #[arg(long)]
        env: Option<String>,
    },
    /// Enable (or update) the eval capability with a sandbox allow-list
    Eval {
        /// Modules eval may import (comma-separated). Independent of the exposed
        /// sets. Omit for an empty allow-list; use --off to disable eval.
        #[arg(long, value_delimiter = ',')]
        allow: Vec<String>,
        /// Disable the eval capability
        #[arg(long)]
        off: bool,
        /// Environment (default: active)
        #[arg(long)]
        env: Option<String>,
    },
}

#[derive(Clone, PartialEq, Eq, ValueEnum)]
enum EngineArg {
    Docker,
    Podman,
    Apptainer,
    /// Alias for Apptainer (the older binary name). Both resolve to
    /// ContainerEngine::Apptainer; the runtime executable is detected later.
    Singularity,
    /// The native (no-container) backend: provision a host toolchain via pixi.
    None,
}

impl From<EngineArg> for ContainerEngine {
    fn from(e: EngineArg) -> Self {
        match e {
            EngineArg::Docker => ContainerEngine::Docker,
            EngineArg::Podman => ContainerEngine::Podman,
            EngineArg::Apptainer | EngineArg::Singularity => ContainerEngine::Apptainer,
            // Native is always dispatched to its own flow before any command
            // converts an engine selection to a container engine.
            EngineArg::None => {
                unreachable!("EngineArg::None (native) must be handled before this conversion")
            }
        }
    }
}

fn parse_port(s: &str) -> std::result::Result<(u16, u16), String> {
    let parts: Vec<&str> = s.splitn(2, ':').collect();
    if parts.len() != 2 {
        return Err(format!("Expected HOST:CONTAINER format, got: {s}"));
    }
    let host: u16 = parts[0]
        .parse()
        .map_err(|_| format!("Invalid host port: {}", parts[0]))?;
    let container: u16 = parts[1]
        .parse()
        .map_err(|_| format!("Invalid container port: {}", parts[1]))?;
    Ok((host, container))
}

/// Parse env vars from --env flags and --env-file, returning (key, value) pairs.
fn collect_env_vars(
    env_flags: &[String],
    env_file: Option<&str>,
) -> Result<Vec<(String, String)>> {
    let mut result = Vec::new();

    if let Some(path) = env_file {
        let contents = std::fs::read_to_string(path).map_err(|e| {
            ManagerError::EnvError(format!("Cannot read env file {path}: {e}"))
        })?;
        for line in contents.lines() {
            let trimmed = line.trim();
            if trimmed.is_empty() || trimmed.starts_with('#') {
                continue;
            }
            if let Some((k, v)) = trimmed.split_once('=') {
                result.push((k.to_string(), v.to_string()));
            }
        }
    }

    for entry in env_flags {
        if let Some((k, v)) = entry.split_once('=') {
            result.push((k.to_string(), v.to_string()));
        } else {
            // Bare key — pass through from host environment
            if let Ok(v) = std::env::var(entry) {
                result.push((entry.clone(), v));
            } else {
                eprintln!("Warning: env var '{entry}' not set in host environment, skipping");
            }
        }
    }

    Ok(result)
}

// ======================================================================
// Main
// ======================================================================

fn main() -> ExitCode {
    #[cfg(unix)]
    {
        use nix::sys::signal::{signal, SigHandler, Signal};
        unsafe { let _ = signal(Signal::SIGPIPE, SigHandler::SigDfl); }
    }

    let matches = match Cli::command()
        .help_template(build_help_template())
        .try_get_matches()
    {
        Ok(m) => m,
        Err(e) => {
            // Detect missing -- separator for the run subcommand
            let rendered = e.to_string();
            if rendered.contains("unrecognized") || rendered.contains("unexpected") {
                let args: Vec<String> = std::env::args().collect();
                if args.len() > 1 && args[1] == "run" {
                    let inner: Vec<&str> = args[2..].iter()
                        .filter(|a| *a != "--shell")
                        .map(|a| a.as_str())
                        .collect();
                    if !inner.is_empty() {
                        eprintln!("Error: unrecognized arguments for 'run'.");
                        eprintln!();
                        eprintln!("Use -- to separate morloc-manager flags from the container command:");
                        eprintln!("  morloc-manager run -- {}", inner.join(" "));
                        return ExitCode::from(2);
                    }
                }
            }
            e.exit();
        }
    };
    let cli = Cli::from_arg_matches(&matches).unwrap();
    if cli.version {
        println!("morloc-manager {}", env!("CARGO_PKG_VERSION"));
        return ExitCode::SUCCESS;
    }
    let Some(cmd) = cli.command else {
        Cli::command()
            .help_template(build_help_template())
            .print_help()
            .ok();
        return ExitCode::from(2);
    };
    match dispatch(cli.verbose, cli.json, cmd) {
        Ok(()) => ExitCode::SUCCESS,
        Err(err) => {
            if cli.json {
                println!("{}", serde_json::json!({"error": format!("{err}")}));
            } else {
                eprintln!("{err}");
            }
            if let ManagerError::EngineError { code, .. } = &err {
                ExitCode::from(*code as u8)
            } else {
                ExitCode::FAILURE
            }
        }
    }
}

fn resolve_scope(system: bool) -> Scope {
    if system { Scope::System } else { Scope::Local }
}

fn check_system_write_access() -> Result<()> {
    let sys_dir = cfg::config_dir(Scope::System);
    if sys_dir.exists() {
        let test_path = sys_dir.join(".write-check");
        match fs::write(&test_path, b"") {
            Ok(_) => { let _ = fs::remove_file(&test_path); Ok(()) }
            Err(_) => Err(ManagerError::ConfigPermissionDenied(format!(
                "{}. System-scope operations require root. Re-run with sudo",
                sys_dir.display()
            )))
        }
    } else {
        match fs::create_dir_all(&sys_dir) {
            Ok(_) => Ok(()),
            Err(_) => Err(ManagerError::ConfigPermissionDenied(format!(
                "{}. System-scope operations require root. Re-run with sudo",
                sys_dir.display()
            )))
        }
    }
}

/// Resolve an environment by explicit name or fall back to the active environment.
fn resolve_env_or_active(name: Option<String>) -> Result<(String, Scope, EnvironmentConfig)> {
    match name {
        Some(n) => {
            let scope = cfg::find_env_scope(&n)?;
            let ec = cfg::read_env_config(scope, &n)?;
            Ok((n, scope, ec))
        }
        None => environment::resolve_active_environment(),
    }
}

fn ensure_engine() -> Result<ContainerEngine> {
    if let Some(cfg) = cfg::read_active_config() {
        return cfg.engine();
    }
    Err(ManagerError::SetupNotComplete(Scope::Local))
}

fn which(name: &str) -> bool {
    Command::new("which")
        .arg(name)
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .map(|s| s.success())
        .unwrap_or(false)
}


/// Render a phase of env.flags.yaml for `info` text output. Only sections
/// with at least one flag are printed; an entirely empty section
/// (the default for a fresh env) is suppressed.
fn print_flag_section(label: &str, section: &EngineFlags) {
    let mut emitted_header = false;
    let mut emit = |engine: &str, list: &[String]| {
        if list.is_empty() {
            return;
        }
        if !emitted_header {
            println!("  {label}:");
            emitted_header = true;
        }
        println!("    {engine}:");
        for flag in list {
            println!("      - {flag}");
        }
    };
    emit("all", &section.all);
    emit("docker", &section.docker);
    emit("podman", &section.podman);
    emit("apptainer", &section.apptainer);
}


fn check_docker_socket(engine: ContainerEngine) {
    use std::path::Path;
    if engine != ContainerEngine::Docker {
        return;
    }
    let socket = Path::new("/var/run/docker.sock");
    if !socket.exists() {
        eprintln!("Warning: Docker socket not found at /var/run/docker.sock");
        eprintln!("  Docker may not be installed or the daemon may not be running.");
    } else if nix::unistd::access(socket, nix::unistd::AccessFlags::R_OK).is_err() {
        eprintln!("Warning: Cannot access Docker socket. You may need to:");
        eprintln!("  sudo usermod -aG docker $USER  # then log out and back in");
    }
}

/// Returns Err with a clear message if Docker is selected but its socket is unreachable.
fn require_docker_socket(engine: ContainerEngine) -> Result<()> {
    use std::path::Path;
    if engine != ContainerEngine::Docker {
        return Ok(());
    }
    let socket = Path::new("/var/run/docker.sock");
    if !socket.exists() {
        return Err(ManagerError::EnvError(
            "Docker socket not found at /var/run/docker.sock. Ensure Docker is installed and the daemon is running.".to_string()
        ));
    }
    if nix::unistd::access(socket, nix::unistd::AccessFlags::R_OK).is_err() {
        return Err(ManagerError::EnvError(
            "Cannot access Docker socket. Add your user to the docker group:\n  \
             sudo usermod -aG docker $USER  # then log out and back in".to_string()
        ));
    }
    Ok(())
}

/// Check if Podman is configured to see rootful images from rootless contexts.
/// Returns true if additionalimagestore is configured (or not needed).
fn check_podman_additional_stores(engine: ContainerEngine) -> bool {
    if engine != ContainerEngine::Podman {
        return true;
    }
    // Root doesn't need additional stores — it owns the store
    if nix::unistd::getuid().is_root() {
        return true;
    }
    let rootful_store = std::path::Path::new("/var/lib/containers/storage");
    if !rootful_store.is_dir() {
        // No rootful store exists, nothing to configure
        return true;
    }
    // Check system and user storage.conf for additionalimagestores
    for path in &[
        "/etc/containers/storage.conf",
        &format!(
            "{}/.config/containers/storage.conf",
            dirs::home_dir()
                .unwrap_or_default()
                .to_string_lossy()
        ),
    ] {
        if let Ok(contents) = fs::read_to_string(path) {
            if contents.contains("/var/lib/containers/storage") {
                return true;
            }
        }
    }
    false
}


// ======================================================================
// Dispatch
// ======================================================================

fn dispatch(verbose: bool, json: bool, cmd: Cmd) -> Result<()> {
    match cmd {
        // ---- setup ----
        Cmd::Setup { engine, system } => {
            // With no --engine, show the current engine settings
            if engine.is_none() {
                let local = cfg::read_config::<Config>(&cfg::config_path(Scope::Local)).ok();
                let sys = cfg::read_config::<Config>(&cfg::config_path(Scope::System)).ok();
                println!("Local engine:   {}",
                    local.as_ref().map(|c| c.backend.label()).unwrap_or("unset"));
                println!("System engine:  {}",
                    sys.as_ref().map(|c| c.backend.label()).unwrap_or("unset"));
                println!();
                println!("Set with: morloc-manager setup --engine <podman|docker|apptainer|singularity>");
                return Ok(());
            }
            if system { check_system_write_access()?; }
            let scope = resolve_scope(system);
            if matches!(engine, Some(EngineArg::None)) {
                let cfg_path = cfg::config_path(scope);
                let base_cfg = cfg::read_config::<Config>(&cfg_path).unwrap_or_default();
                cfg::write_config(&cfg_path, &Config { backend: Backend::Native, ..base_cfg })?;
                eprintln!("Engine set to: native");
                return Ok(());
            }
            let eng: ContainerEngine = engine.unwrap().into();
            check_docker_socket(eng);
            let cfg_path = cfg::config_path(scope);
            let base_cfg = cfg::read_config::<Config>(&cfg_path).unwrap_or_default();
            let new_cfg = Config {
                backend: Backend::Container(eng),
                ..base_cfg
            };
            cfg::write_config(&cfg_path, &new_cfg)?;
            eprintln!("Engine set to: {}", eng.name());
            Ok(())
        }

        // ---- new ----
        Cmd::New {
            name,
            lang,
            engine,
            system_package,
            system,
            no_init,
            non_interactive,
        } => {
            if system { check_system_write_access()?; }
            let scope = resolve_scope(system);

            // Backend fork: the native (no-container) backend has its own flow.
            // Choose it when explicitly requested (--engine none), when it is the
            // configured default, or -- with no explicit or configured choice --
            // when the host is native-capable (host-probed default).
            let go_native = match &engine {
                Some(EngineArg::None) => {
                    let profile = hostprobe::probe_host();
                    if !profile.native_capable {
                        return Err(ManagerError::EnvError(format!(
                            "the native backend is not available on this host: {}",
                            profile.reason
                        )));
                    }
                    true
                }
                Some(_) => false,
                None => match cfg::read_active_config().map(|c| c.backend) {
                    Some(Backend::Native) => true,
                    Some(Backend::Container(_)) => false,
                    None => hostprobe::probe_host().native_capable,
                },
            };
            if go_native {
                if !system_package.is_empty() {
                    return Err(ManagerError::EnvError(
                        "--system-package applies only to container backends; the \
                         native backend has no image to bake packages into"
                            .to_string(),
                    ));
                }
                let interactive = !non_interactive && io::stdin().is_terminal();
                return native_new(scope, name, lang, no_init, interactive, verbose);
            }

            // Resolve engine: explicit flag > config default > auto-detect single > error
            // For --system, prefer system config so the env uses the system engine.
            let resolved_engine = if let Some(e) = engine {
                let eng: ContainerEngine = e.into();
                check_docker_socket(eng);
                eng
            } else if let Some(cfg) = if system {
                // System scope: check system config first, then local
                cfg::read_config::<Config>(&cfg::config_path(Scope::System)).ok()
                    .or_else(|| cfg::read_active_config())
            } else {
                cfg::read_active_config()
            } {
                cfg.engine()?
            } else {
                // No config — try auto-detection. Apptainer/Singularity are
                // included so HPC-only hosts work out of the box.
                let has_podman = which("podman");
                let has_docker = which("docker");
                let has_apptainer = which("apptainer") || which("singularity");
                let candidates: Vec<(ContainerEngine, &str)> = [
                    (ContainerEngine::Podman, "podman"),
                    (ContainerEngine::Docker, "docker"),
                    (ContainerEngine::Apptainer, "apptainer"),
                ]
                .into_iter()
                .filter(|(e, _)| match e {
                    ContainerEngine::Podman => has_podman,
                    ContainerEngine::Docker => has_docker,
                    ContainerEngine::Apptainer => has_apptainer,
                })
                .collect();
                match candidates.as_slice() {
                    [] => return Err(ManagerError::EngineNotFound),
                    [(only, _)] => {
                        if *only == ContainerEngine::Docker {
                            check_docker_socket(ContainerEngine::Docker);
                        }
                        *only
                    }
                    multi => {
                        let scope_flag = if system { " --system" } else { "" };
                        let names: Vec<String> = multi
                            .iter()
                            .map(|(_, n)| (*n).to_string())
                            .collect();
                        let setup_lines: String = multi
                            .iter()
                            .map(|(_, n)| {
                                format!("  morloc-manager setup --engine {n}{scope_flag}\n")
                            })
                            .collect();
                        return Err(ManagerError::EnvError(format!(
                            "Multiple container engines are installed ({}) and no \
                             default is set.\nPick one with:\n{}\
                             Or pass --engine to this command directly.",
                            names.join(", "),
                            setup_lines
                        )));
                    }
                }
            };

            // Ensure config exists (write default if first run)
            if cfg::read_active_config().is_none() {
                let cfg_path = cfg::config_path(scope);
                let new_cfg = Config {
                    active_env: None,
                    backend: Backend::Container(resolved_engine),
                };
                cfg::write_config(&cfg_path, &new_cfg)?;
            }

            // The container backend is requirement-derived: the env's image is
            // built from a generated Dockerfile that runs pixi inside, sharing the
            // native backend's lowering. There is no pull/recipe/base-image path.
            let interactive = !non_interactive && io::stdin().is_terminal();
            if !non_interactive && !interactive {
                eprintln!("Note: No TTY detected, running in non-interactive mode.");
            }
            container_new_derived(
                scope, resolved_engine, name, lang, system_package, no_init, interactive,
            )
        }

        // ---- run ----
        Cmd::Run { command, shell, env_vars, env_file, engine_arg, slurm_bridge } => {
            if !shell && command.is_empty() {
                return Err(ManagerError::NoCommand);
            }
            let user_env = collect_env_vars(&env_vars, env_file.as_deref())?;
            runner::run_in_env(
                None,
                runner::RunRequest {
                    verbose,
                    shell,
                    args: command,
                    user_env,
                    engine_args: engine_arg,
                    phase: Phase::Run,
                    slurm_bridge,
                },
            )
            .map_err(|e| match e {
                ManagerError::EnvironmentNotFound(msg) => ManagerError::EnvironmentNotFound(
                    format!("{msg}. Run 'morloc-manager new' to create an environment")
                ),
                other => other,
            })
        }

        // ---- rm ----
        Cmd::Rm { names, system, force } => {
            if system { check_system_write_access()?; }
            if names.is_empty() {
                return Err(ManagerError::EnvError("No environment names specified".to_string()));
            }
            // Capture current active env for post-removal feedback
            let was_active = cfg::read_active_config().and_then(|c| c.active_env);
            // Attempt each removal; collect failures, continue past errors
            let mut failures: Vec<String> = Vec::new();
            for name in &names {
                let result: Result<()> = (|| {
                    let scope = if system {
                        Scope::System
                    } else {
                        cfg::find_env_scope(name)?
                    };
                    if scope == Scope::System && !system {
                        check_system_write_access()?;
                    }
                    if !force {
                        if let Some(cfg) = cfg::read_active_config() {
                            if cfg.active_env.as_deref() == Some(name.as_str()) {
                                return Err(ManagerError::EnvError(format!(
                                    "active environment (use --force)"
                                )));
                            }
                        }
                    }
                    let ec = cfg::read_env_config(scope, name)
                        .map_err(|_| ManagerError::EnvironmentNotFound(name.to_string()))?;
                    environment::remove_environment(ec.engine()?, scope, name)?;
                    Ok(())
                })();
                match result {
                    Ok(()) => {
                        // Check if removed env was active and report new state
                        if was_active.as_deref() == Some(name.as_str()) {
                            match environment::resolve_active_environment() {
                                Ok((new_active, _, _)) => {
                                    // Persist the fallback as the new active environment
                                    let _ = environment::select_environment(&new_active, Scope::Local);
                                    eprintln!("Removed environment: {name}. Active environment is now: {new_active}");
                                }
                                Err(_) => {
                                    eprintln!("Removed environment: {name}. No active environment. Use: morloc-manager select <name>");
                                }
                            }
                        } else {
                            eprintln!("Removed environment: {name}");
                        }
                    }
                    Err(e) => failures.push(format!("{name}: {e}")),
                }
            }
            if !failures.is_empty() {
                eprintln!();
                eprintln!("Failed to remove {} environment(s):", failures.len());
                for f in &failures {
                    eprintln!("  {f}");
                }
                return Err(ManagerError::EnvError(format!(
                    "{} of {} removals failed",
                    failures.len(),
                    names.len()
                )));
            }
            Ok(())
        }

        // ---- nuke ----
        Cmd::Nuke { system, images, yes } => {
            let scope = if system { Scope::System } else { Scope::Local };
            let scope_label = if system { "system" } else { "local" };

            if system {
                check_system_write_access()?;
            }

            // Confirm before removing all environments
            let env_names = cfg::list_env_names(scope);
            if env_names.is_empty() {
                eprintln!("No {scope_label} environments found.");
                return Ok(());
            }

            if !yes {
                eprintln!("This will remove {} {scope_label} environment(s):", env_names.len());
                for n in &env_names {
                    eprintln!("  {n}");
                }
                if io::stdin().is_terminal() {
                    eprint!("Continue? [y/N] ");
                    io::stderr().flush().ok();
                    let mut answer = String::new();
                    io::stdin().read_line(&mut answer).ok();
                    if !matches!(answer.trim(), "y" | "yes" | "Y" | "YES") {
                        eprintln!("Aborted.");
                        return Ok(());
                    }
                } else {
                    return Err(ManagerError::EnvError(
                        "nuke requires --yes for non-interactive use".to_string(),
                    ));
                }
            }

            eprintln!("Removing all {scope_label} morloc environments...");

            // Collect env info before removal (configs are deleted during removal)
            let mut env_list: Vec<(String, ContainerEngine)> = Vec::new();
            let mut base_images: HashSet<String> = HashSet::new();

            for name in cfg::list_env_names(scope) {
                if let Ok(ec) = cfg::read_env_config(scope, &name) {
                    if images {
                        base_images.insert(ec.base_image.clone());
                        if let Some(ref orig) = ec.original_image {
                            base_images.insert(orig.clone());
                        }
                    }
                    env_list.push((name, ec.engine()?));
                }
            }

            if env_list.is_empty() {
                eprintln!("No {scope_label} environments found.");
            } else {
                let mut removed = 0usize;
                let mut failures: Vec<String> = Vec::new();

                for (name, engine) in &env_list {
                    eprintln!("Removing environment: {name}...");
                    match environment::remove_environment(*engine, scope, name) {
                        Ok(()) => {
                            eprintln!("  Removed: {name}");
                            removed += 1;
                        }
                        Err(e) => {
                            eprintln!("  Failed: {name}: {e}");
                            failures.push(format!("{name}: {e}"));
                        }
                    }
                }

                // Clear active_env in the targeted scope's config
                let cfg_path = cfg::config_path(scope);
                if let Ok(cfg_data) = cfg::read_config::<Config>(&cfg_path) {
                    if cfg_data.active_env.is_some() {
                        let new_cfg = Config { active_env: None, ..cfg_data };
                        let _ = cfg::write_config(&cfg_path, &new_cfg);
                        eprintln!("Cleared active environment.");
                    }
                }

                eprintln!("Removed {removed} environment(s).");

                if !failures.is_empty() {
                    eprintln!();
                    eprintln!("Failed to remove {} environment(s):", failures.len());
                    for f in &failures {
                        eprintln!("  {f}");
                    }
                    return Err(ManagerError::EnvError(format!(
                        "{} of {} removals failed",
                        failures.len(),
                        env_list.len()
                    )));
                }
            }

            // Remove base images if --images
            if images && !base_images.is_empty() {
                let engine = ensure_engine().unwrap_or(ContainerEngine::Docker);
                eprintln!("Removing base images...");
                for img in &base_images {
                    if container::image_exists_locally(engine, img) {
                        eprintln!("  Removing image: {img}...");
                        if container::remove_image(engine, img) {
                            eprintln!("  Removed: {img}");
                        } else {
                            eprintln!("  Failed to remove: {img}");
                        }
                    }
                }
            }

            // Hint about the other scope
            let other_scope = if system { Scope::Local } else { Scope::System };
            let other_envs = cfg::list_env_names(other_scope);
            if !other_envs.is_empty() {
                if system {
                    eprintln!(
                        "{} local environment(s) remain. Use: morloc-manager nuke",
                        other_envs.len()
                    );
                } else {
                    eprintln!(
                        "{} system environment(s) remain. Use: sudo morloc-manager nuke --system",
                        other_envs.len()
                    );
                }
            }

            Ok(())
        }

        // ---- ls ----
        Cmd::Ls { system, local } => {
            let active_env = cfg::read_active_config()
                .and_then(|c| c.active_env);
            let active_str = active_env.as_deref();

            // Determine which scope effectively owns the active environment.
            // Local takes priority (same resolution as run/select).
            let active_in_local = active_str
                .map(|name| cfg::env_config_path(Scope::Local, name).is_file())
                .unwrap_or(false);

            let show_local = !system || local;
            let show_system = !local || system;

            let local_envs = if show_local {
                let local_active = if active_in_local { active_str } else { None };
                environment::list_environments(Scope::Local, local_active)
            } else {
                Vec::new()
            };
            let system_envs = if show_system {
                let system_active = if active_in_local { None } else { active_str };
                environment::list_environments(Scope::System, system_active)
            } else {
                Vec::new()
            };

            if json {
                #[derive(serde::Serialize)]
                struct LsOutput {
                    local: Vec<environment::EnvInfo>,
                    system: Vec<environment::EnvInfo>,
                }
                let output = LsOutput { local: local_envs, system: system_envs };
                println!("{}", serde_json::to_string_pretty(&output).unwrap());
            } else {
                let total = local_envs.len() + system_envs.len();
                if !local_envs.is_empty() {
                    println!("Local environments:");
                    for e in &local_envs {
                        let active_mark = if e.active { " (active)" } else { "" };
                        let ver_mark = e.morloc_version.as_ref()
                            .map(|v| format!(" [{}]", v.show()))
                            .unwrap_or_default();
                        println!("  {}{}{}", e.name, ver_mark, active_mark);
                    }
                }
                if !system_envs.is_empty() {
                    if !local_envs.is_empty() {
                        println!();
                    }
                    println!("System environments:");
                    for e in &system_envs {
                        let active_mark = if e.active { " (active)" } else { "" };
                        let ver_mark = e.morloc_version.as_ref()
                            .map(|v| format!(" [{}]", v.show()))
                            .unwrap_or_default();
                        println!("  {}{}{}", e.name, ver_mark, active_mark);
                    }
                }
                if total == 0 {
                    println!("No environments found. Create one with: morloc-manager new");
                }
            }
            Ok(())
        }

        // ---- info ----
        Cmd::Info { name, system } => {
            if let Some(env_name) = name {
                // Detailed info for a specific environment
                let scope = if system {
                    if !cfg::env_config_path(Scope::System, &env_name).is_file() {
                        return Err(ManagerError::EnvironmentNotFound(format!(
                            "{env_name} (in system scope)"
                        )));
                    }
                    Scope::System
                } else {
                    cfg::find_env_scope(&env_name)?
                };
                let ec = cfg::read_env_config(scope, &env_name)?;
                let data_dir = cfg::env_data_dir(scope, &env_name);
                let active = cfg::read_active_config()
                    .and_then(|c| c.active_env)
                    .as_deref() == Some(env_name.as_str());

                if json {
                    #[derive(serde::Serialize)]
                    struct InfoDetail {
                        name: String,
                        scope: String,
                        active: bool,
                        base_image: String,
                        #[serde(skip_serializing_if = "Option::is_none")]
                        built_image: Option<String>,
                        morloc_version: Option<Version>,
                        engine: String,
                        #[serde(skip_serializing_if = "Option::is_none")]
                        shm_size: Option<String>,
                        #[serde(skip_serializing_if = "Option::is_none")]
                        dockerfile: Option<String>,
                        #[serde(skip_serializing_if = "Option::is_none")]
                        deffile: Option<String>,
                        #[serde(skip_serializing_if = "Option::is_none")]
                        base_sif: Option<String>,
                        #[serde(skip_serializing_if = "Option::is_none")]
                        layered_sif: Option<String>,
                        flag_config: FlagConfig,
                        flags_file: String,
                        data_dir: String,
                    }
                    let df_str = ec.dockerfile.as_ref().map(|_| {
                        let df_path = cfg::env_dockerfile_path(scope, &env_name);
                        df_path.display().to_string()
                    });
                    let def_str = ec.singularity_def.as_ref().map(|_| {
                        let def_path = cfg::env_deffile_path(scope, &env_name);
                        def_path.display().to_string()
                    });
                    let flags_path = cfg::env_flags_yaml_path(scope, &env_name);
                    let flag_config = cfg::read_flag_config(scope, &env_name)
                        .unwrap_or_default();
                    // SHM size is honored only under docker/podman; Apptainer
                    // shares host /dev/shm so the field is meaningless there
                    // and is omitted from `info` output for that engine.
                    let shm = match ec.backend.container_engine() {
                        Some(ContainerEngine::Docker) | Some(ContainerEngine::Podman) => {
                            Some(ec.shm_size.clone())
                        }
                        // Apptainer shares host /dev/shm; native has no container.
                        _ => None,
                    };
                    // .sif paths only apply under Apptainer. Built_image
                    // mirrors that asymmetry: it is the OCI fallback tag for
                    // Apptainer and the primary built layer for docker/podman.
                    let (base_sif, layered_sif) = match ec.backend.container_engine() {
                        Some(ContainerEngine::Apptainer) => (ec.base_sif.clone(), ec.layered_sif.clone()),
                        _ => (None, None),
                    };
                    let output = InfoDetail {
                        name: ec.name.clone(),
                        scope: match scope { Scope::Local => "local", Scope::System => "system" }.to_string(),
                        active,
                        base_image: ec.base_image.clone(),
                        built_image: ec.built_image.clone(),
                        morloc_version: ec.morloc_version.clone(),
                        engine: ec.backend.label().to_string(),
                        shm_size: shm,
                        dockerfile: df_str,
                        deffile: def_str,
                        base_sif,
                        layered_sif,
                        flag_config,
                        flags_file: flags_path.display().to_string(),
                        data_dir: data_dir.display().to_string(),
                    };
                    println!("{}", serde_json::to_string_pretty(&output).unwrap());
                } else {
                    println!("Name:           {}", ec.name);
                    println!("Scope:          {}", match scope { Scope::Local => "local", Scope::System => "system" });
                    println!("Active:         {}", if active { "yes" } else { "no" });
                    println!("Base image:     {}", ec.base_image);
                    if let Some(ref img) = ec.built_image {
                        println!("Built image:    {img}");
                    }
                    if let Some(ref ver) = ec.morloc_version {
                        println!("Morloc version: {}", ver.show());
                    }
                    println!("Engine:         {}", ec.backend.label());
                    // Engine-specific fields:
                    // * Docker/Podman: show SHM size and the Dockerfile path.
                    // * Apptainer:    show base .sif path and the .def path
                    //                 (Dockerfile, if present, is the OCI
                    //                 fallback recipe and is also surfaced).
                    match ec.backend.container_engine() {
                        Some(ContainerEngine::Docker) | Some(ContainerEngine::Podman) => {
                            println!("SHM size:       {}", ec.shm_size);
                            println!("Dockerfile:     {}", match ec.dockerfile {
                                Some(_) => {
                                    let df_path = cfg::env_dockerfile_path(scope, &env_name);
                                    if df_path.exists() {
                                        df_path.display().to_string()
                                    } else {
                                        format!("{} (MISSING)", df_path.display())
                                    }
                                }
                                None => "none".to_string(),
                            });
                        }
                        Some(ContainerEngine::Apptainer) => {
                            println!("Base SIF:       {}", match ec.base_sif {
                                Some(ref p) => {
                                    if std::path::Path::new(p).is_file() {
                                        p.clone()
                                    } else {
                                        format!("{p} (MISSING)")
                                    }
                                }
                                None => "none".to_string(),
                            });
                            if let Some(ref p) = ec.layered_sif {
                                println!("Layered SIF:    {}", if std::path::Path::new(p).is_file() {
                                    p.clone()
                                } else {
                                    format!("{p} (MISSING)")
                                });
                            }
                            println!("Def file:       {}", match ec.singularity_def {
                                Some(_) => {
                                    let def_path = cfg::env_deffile_path(scope, &env_name);
                                    if def_path.exists() {
                                        def_path.display().to_string()
                                    } else {
                                        format!("{} (MISSING)", def_path.display())
                                    }
                                }
                                None => "none".to_string(),
                            });
                            // Surface a Dockerfile too if one exists -- under
                            // Apptainer it is the OCI-fallback recipe.
                            if ec.dockerfile.is_some() {
                                let df_path = cfg::env_dockerfile_path(scope, &env_name);
                                println!("Dockerfile:     {} (OCI fallback)", if df_path.exists() {
                                    df_path.display().to_string()
                                } else {
                                    format!("{} (MISSING)", df_path.display())
                                });
                            }
                        }
                        // Native environments have no engine-specific fields.
                        None => {}
                    }
                    if !ec.system_packages.is_empty() {
                        println!("System pkgs:    {}", ec.system_packages.join(" "));
                    }
                    let flags_path = cfg::env_flags_yaml_path(scope, &env_name);
                    println!("Flags:          {}", flags_path.display());
                    let flag_config = cfg::read_flag_config(scope, &env_name)
                        .unwrap_or_default();
                    print_flag_section("build", &flag_config.build);
                    print_flag_section("run", &flag_config.run);
                    print_flag_section("start", &flag_config.start);
                    println!("Data dir:       {}", data_dir.display());
                }
            } else {
                // Overview
                let local_cfg = cfg::read_config::<Config>(&cfg::config_path(Scope::Local)).ok();
                let system_cfg = cfg::read_config::<Config>(&cfg::config_path(Scope::System)).ok();
                let se_mode = detect_selinux();

                let active_env = environment::resolve_active_environment()
                    .map(|(name, _, _)| name)
                    .unwrap_or_else(|_| "none".to_string());

                let se_str = match se_mode {
                    SELinuxMode::Enforcing => "enforcing",
                    SELinuxMode::Permissive => "permissive",
                    SELinuxMode::Disabled => "not detected",
                };

                if json {
                    #[derive(serde::Serialize)]
                    struct DirInfo { path: String, exists: bool }
                    #[derive(serde::Serialize)]
                    struct InfoOverview {
                        active: String,
                        local_engine: String,
                        system_engine: String,
                        selinux: String,
                        directories: std::collections::BTreeMap<String, DirInfo>,
                        local: Vec<environment::EnvInfo>,
                        system: Vec<environment::EnvInfo>,
                    }
                    let active_str = if active_env == "none" { None } else { Some(active_env.as_str()) };
                    let mut directories = std::collections::BTreeMap::new();
                    for (label, path) in [
                        ("config_local", cfg::config_dir(Scope::Local)),
                        ("data_local", cfg::data_dir(Scope::Local)),
                        ("config_system", cfg::config_dir(Scope::System)),
                        ("data_system", cfg::data_dir(Scope::System)),
                    ] {
                        directories.insert(label.to_string(), DirInfo {
                            path: path.display().to_string(),
                            exists: path.is_dir(),
                        });
                    }
                    let output = InfoOverview {
                        active: active_env.clone(),
                        local_engine: local_cfg.as_ref().map(|c| c.backend.label()).unwrap_or("unset").to_string(),
                        system_engine: system_cfg.as_ref().map(|c| c.backend.label()).unwrap_or("unset").to_string(),
                        selinux: se_str.to_string(),
                        directories,
                        local: environment::list_environments(Scope::Local, active_str),
                        system: environment::list_environments(Scope::System, active_str),
                    };
                    println!("{}", serde_json::to_string_pretty(&output).unwrap());
                } else {
                    println!("Active:         {active_env}");
                    println!("Local engine:   {}",
                        local_cfg.as_ref().map(|c| c.backend.label()).unwrap_or("unset"));
                    println!("System engine:  {}",
                        system_cfg.as_ref().map(|c| c.backend.label()).unwrap_or("unset"));
                    println!("SELinux:        {se_str}");

                    let dirs = [
                        ("Config (local)", cfg::config_dir(Scope::Local)),
                        ("Data (local)", cfg::data_dir(Scope::Local)),
                        ("Config (system)", cfg::config_dir(Scope::System)),
                        ("Data (system)", cfg::data_dir(Scope::System)),
                    ];
                    println!("\nDirectories:");
                    for (label, path) in &dirs {
                        let status = if path.is_dir() { "exists" } else { "not found" };
                        println!("  {:<20} {} ({})", label, path.display(), status);
                    }

                    let active_str = if active_env == "none" { None } else { Some(active_env.as_str()) };

                    // Check if active env lives in local scope (local takes priority)
                    let active_in_local = active_str
                        .map(|name| cfg::env_config_path(Scope::Local, name).is_file())
                        .unwrap_or(false);

                    let local_envs = environment::list_environments(Scope::Local, active_str);
                    println!("\nLocal environments:");
                    if local_envs.is_empty() {
                        println!("  (none)");
                    } else {
                        for e in &local_envs {
                            let active_mark = if e.active { " (active)" } else { "" };
                            let ver_mark = e.morloc_version.as_ref()
                                .map(|v| format!(" [{}]", v.show()))
                                .unwrap_or_default();
                            println!("  {}{}{}", e.name, ver_mark, active_mark);
                        }
                    }

                    let system_envs = environment::list_environments(Scope::System, active_str);
                    if !system_envs.is_empty() {
                        println!("\nSystem environments:");
                        for e in &system_envs {
                            let active_mark = if e.active && active_in_local {
                                " (active - shadowed)"
                            } else if e.active {
                                " (active)"
                            } else {
                                ""
                            };
                            let ver_mark = e.morloc_version.as_ref()
                                .map(|v| format!(" [{}]", v.show()))
                                .unwrap_or_default();
                            println!("  {}{}{}", e.name, ver_mark, active_mark);
                        }
                    }
                }
            }
            Ok(())
        }

        // ---- select ----
        Cmd::Select { name, system } => {
            if system { check_system_write_access()?; }
            let write_scope = resolve_scope(system);
            environment::select_environment(&name, write_scope)?;
            if system {
                eprintln!("Set system default environment: {name}");
            } else {
                eprintln!("Selected environment: {name}");
            }
            Ok(())
        }

        // ---- update ----
        Cmd::Update {
            name,
            lang,
            system_package,
            non_interactive: _,
        } => {
            let (env_name, env_scope) = match name {
                Some(n) => {
                    let scope = cfg::find_env_scope(&n)?;
                    (n, scope)
                }
                None => {
                    let (n, s, _) = environment::resolve_active_environment()?;
                    (n, s)
                }
            };
            if env_scope == Scope::System {
                check_system_write_access()?;
            }

            let mut ec = cfg::read_env_config(env_scope, &env_name)?;

            // Merge any newly requested OS packages into the persisted set
            // (additive + idempotent); rematerialize_env reads them back from
            // env.yaml. Container-only: native has no image layer to bake into.
            if !system_package.is_empty() {
                if ec.backend.is_native() {
                    return Err(ManagerError::EnvError(
                        "--system-package applies only to container backends; the \
                         native backend has no image to bake packages into"
                            .to_string(),
                    ));
                }
                for p in system_package {
                    if !ec.system_packages.contains(&p) {
                        ec.system_packages.push(p);
                    }
                }
                cfg::write_env_config(env_scope, &env_name, &ec)?;
            }

            // Re-pin: an explicit --lang overwrites the stored pins that
            // rematerialize_env reads back; otherwise the stored pins are reused.
            if !lang.is_empty() {
                let pins = parse_lang_pins(&lang);
                cfg::write_env_inputs(env_scope, &env_name, &EnvInputs { lang_pins: pins })?;
            }
            // No extra specs: re-solve from the installed programs' envspecs.
            rematerialize_env(env_scope, &env_name, &[], verbose)?;

            if ec.backend.is_native() {
                eprintln!("Native environment '{env_name}' re-materialized.");
            } else {
                anstream::eprintln!("\x1b[1;32mContainer environment '{env_name}' rebuilt.\x1b[0m");
            }
            Ok(())
        }
        // ---- freeze ----
        Cmd::Freeze { name, output, force } => {
            let output_dir = output.as_deref().unwrap_or("./morloc-freeze");
            // Protect against silently overwriting a previous freeze
            let existing_tar = std::path::Path::new(output_dir).join("state.tar.gz");
            if existing_tar.exists() && !force {
                return Err(ManagerError::FreezeError(format!(
                    "Output directory already contains a freeze: {}\n  \
                     Use --force to overwrite, or specify a different -o path.",
                    existing_tar.display()
                )));
            }
            let (env_name, env_scope, ec) = resolve_env_or_active(name)?;
            let engine = ec.engine()?;
            // Detect the version from the container binary for sanity check.
            // The morloc binary can't report prerelease tags (stack limitation),
            // so if major.minor.patch match, keep the recorded version which has
            // the full tag from the image.
            eprintln!("Detecting morloc version from image...");
            let detected = environment::detect_morloc_version(ec.engine()?, ec.active_image())?;
            let ver = if let Some(ref recorded) = ec.morloc_version {
                if recorded.major == detected.major
                    && recorded.minor == detected.minor
                    && recorded.patch == detected.patch
                {
                    recorded.clone()
                } else {
                    eprintln!(
                        "Warning: recorded morloc version ({}) does not match image ({}).",
                        recorded.show(), detected.show()
                    );
                    detected
                }
            } else {
                detected
            };
            let data_dir = cfg::env_data_dir(env_scope, &env_name);
            let image = ec.active_image().to_string();
            let result = freeze::freeze_from_dir(env_scope, ver.clone(), engine, &image, &data_dir.to_string_lossy(), output_dir, verbose);
            if result.is_ok() && ec.morloc_version.as_ref() != Some(&ver) {
                let mut updated = ec.clone();
                updated.morloc_version = Some(ver);
                let _ = cfg::write_env_config(env_scope, &env_name, &updated);
            }
            result
        }

        // ---- unfreeze ----
        Cmd::Unfreeze { from, tag, base, engine: engine_override, rebuild } => {
            let from = {
                let p = std::path::Path::new(&from);
                if p.is_dir() {
                    let tar = p.join("state.tar.gz");
                    if tar.is_file() {
                        tar.to_string_lossy().to_string()
                    } else {
                        return Err(ManagerError::UnfreezeError(format!(
                            "Directory '{}' does not contain state.tar.gz. \
                             Pass the path to state.tar.gz directly, or the directory containing it.",
                            from
                        )));
                    }
                } else if p.is_file() {
                    from
                } else {
                    return Err(ManagerError::UnfreezeError(format!(
                        "Input not found: {from}. \
                         Pass the path to state.tar.gz or the directory containing it."
                    )));
                }
            };
            // Read version and engine from the freeze manifest so unfreeze
            // works on deployment machines with no morloc environments.
            let tarball_dir = std::path::Path::new(&from)
                .parent()
                .unwrap_or(std::path::Path::new("."));
            let manifest_path = tarball_dir.join("freeze-manifest.json");
            let manifest = freeze::read_freeze_manifest(&manifest_path.to_string_lossy())
                .map_err(|_| ManagerError::UnfreezeError(format!(
                    "Cannot read freeze manifest at {}. Ensure state.tar.gz and freeze-manifest.json are in the same directory.",
                    manifest_path.display()
                )))?;
            if matches!(engine_override, Some(EngineArg::None)) {
                return Err(ManagerError::UnfreezeError(
                    "unfreezing to the native backend is not yet supported; unfreeze to a \
                     container engine (--engine podman) or omit --engine".to_string(),
                ));
            }
            let engine = match engine_override {
                Some(arg) => arg.into(),
                None => {
                    let e = ensure_engine()?;
                    eprintln!(
                        "Note: using {} engine from global config. Override with --engine if needed.",
                        e.name()
                    );
                    e
                }
            };
            serve::build_serve_image(engine, verbose, &from, &tag, manifest.morloc_version, base.as_deref(), rebuild, &manifest.programs)
        }

        // ---- start ----
        Cmd::Start { name, mcp, auth_token, expose, allow_plaintext, allow_no_auth, unsafe_serve, port, env_vars, env_file, engine_arg, force } => {
            let (env_name, env_scope, ec) = resolve_env_or_active(name)?;
            // Refuse to replace a live serve -- dispatch on the STORED handle so a
            // serve of this env by either backend is detected; --force tears the
            // old one down first so nothing is stranded. A record with no handle is
            // still probed via the env's backend, so a container serve is not
            // silently replaced.
            if let Some(rt) = cfg::read_serve_runtime(env_scope, &env_name) {
                let alive = match &rt.handle {
                    Some(handle) => serve_handle_alive(handle),
                    None => match ec.backend {
                        Backend::Container(engine) => container::container_exists(
                            engine,
                            &serve::serve_container_name(&env_name),
                        ),
                        Backend::Native => false,
                    },
                };
                if alive {
                    if !force {
                        return Err(ManagerError::EnvError(format!(
                            "'{env_name}' is already serving on {}:{}. Use --force to replace.",
                            rt.host, rt.port
                        )));
                    }
                    match &rt.handle {
                        Some(handle) => stop_by_handle(handle, verbose)?,
                        None => {
                            if let Backend::Container(engine) = ec.backend {
                                let _ = serve::stop_serve_container(
                                    engine,
                                    verbose,
                                    &serve::serve_container_name(&env_name),
                                );
                            }
                        }
                    }
                }
            }
            // Backend-neutral orchestration: WHAT to serve (a --mcp one-off or the
            // exposed set) + the port. One listener serves both adapters; MCP
            // defaults to 9000, API-only to 8080, auto-picking a free port.
            let spec = resolve_serve_spec(env_scope, &env_name, &mcp)?;
            let eval_on = spec.eval_allow.is_some();
            let serves_mcp = !spec.mcp.is_empty() || eval_on;
            let serves_api = !spec.api.is_empty() || eval_on;
            let default_port: u16 = if serves_mcp { 9000 } else { 8080 };
            let (host_port, container_port) = if port.is_empty() {
                let p = find_free_host_port(default_port, 100);
                if p != default_port {
                    eprintln!("Port {default_port} is in use; serving on {p} instead (override with -p).");
                }
                (p, p)
            } else {
                port.first().copied().unwrap_or((default_port, default_port))
            };
            let user_env = collect_env_vars(&env_vars, env_file.as_deref())?;
            let token = resolve_mcp_token(auth_token);

            // Backend-dispatched launch (container image vs detached host nexus);
            // the record + client config below are shared.
            let req = ServeRequest {
                spec, host_port, container_port, user_env,
                expose, allow_plaintext, allow_no_auth, unsafe_serve,
                engine_args: engine_arg, token, verbose,
            };
            let env = runner::ResolvedEnv { name: env_name.clone(), scope: env_scope, ec };
            let ServeOutcome { handle, url_host, token: eff_token } =
                runner::runner_for(&env.ec).serve(&env, &req)?;

            if serves_mcp {
                eprintln!("  MCP:  http://{url_host}:{host_port}/mcp");
            }
            if serves_api {
                eprintln!("  API:  http://{url_host}:{host_port}/call/<module>/<command>");
            }
            // Record what we launched + how (the handle) so stop/logs/status act
            // on the real target regardless of the env's current backend.
            let _ = cfg::write_serve_runtime(env_scope, &env_name, &ServeRuntime {
                mcp: req.spec.mcp.clone(),
                api: req.spec.api.clone(),
                eval: eval_on,
                host: url_host.clone(),
                port: host_port,
                token_required: eff_token.is_some(),
                handle: Some(handle),
            });
            if serves_mcp {
                // The mcpServers entry name: the one-off program, else the env.
                let cfg_name = mcp.unwrap_or_else(|| env_name.clone());
                print_http_mcp_config(&cfg_name, &url_host, host_port, eff_token.as_deref());
            }
            Ok(())
        }

        // ---- stop ----
        Cmd::Stop { name } => {
            let (env_name, env_scope, ec) = resolve_env_or_active(name)?;
            // Prefer the stored launch handle: it tears down the right target
            // (native process group or container) regardless of the env's current
            // backend, so migration / dual-backend serve never strands a server.
            if let Some(rt) = cfg::read_serve_runtime(env_scope, &env_name) {
                if let Some(handle) = &rt.handle {
                    stop_by_handle(handle, verbose)?;
                    cfg::remove_serve_runtime(env_scope, &env_name);
                    eprintln!("Stopped serving environment: {env_name}");
                    return Ok(());
                }
            }
            // Legacy record (no handle): fall back to the container-name probe.
            let container_name = serve::serve_container_name(&env_name);
            if crate::container::container_exists(ec.engine()?, &container_name) {
                serve::stop_serve_container(ec.engine()?, verbose, &container_name)?;
                cfg::remove_serve_runtime(env_scope, &env_name);
                eprintln!("Stopped serving environment: {env_name}");
            } else {
                return Err(ManagerError::EnvError(
                    format!("No serve running for environment '{env_name}'")
                ));
            }
            Ok(())
        }

        // ---- logs ----
        Cmd::Logs { name, follow } => {
            // Native serve: if the resolved/active env is serving natively, tail
            // its host logfile. Falls through to the container path otherwise.
            if let Ok((en, sc, _)) = resolve_env_or_active(name.clone()) {
                if let Some(rt) = cfg::read_serve_runtime(sc, &en) {
                    if matches!(rt.handle, Some(ServeHandle::Native { .. })) {
                        let log_path = cfg::env_data_dir(sc, &en).join("logs").join("serve.log");
                        return tail_file(&log_path, follow);
                    }
                }
            }
            let (container_name, engine, logs_dir) = if let Some(ref n) = name {
                let (env_name, scope, ec) = resolve_env_or_active(Some(n.clone()))?;
                let cname = serve::serve_container_name(n);
                if !container::container_exists(ec.engine()?, &cname) {
                    return Err(ManagerError::EnvError(
                        format!("No serve container running for environment '{n}'")
                    ));
                }
                (cname, ec.engine()?, cfg::env_data_dir(scope, &env_name).join("logs"))
            } else {
                let (cname, engine) = find_running_serve_container()?;
                // Resolve the env's data dir for its captured daemon logs; fall
                // back to the Local scope if the env can't be resolved (display
                // only -- worst case the per-daemon logs are simply omitted).
                let env_name = serve::env_name_from_container(&cname).to_string();
                let logs_dir = match resolve_env_or_active(Some(env_name.clone())) {
                    Ok((en, sc, _)) => cfg::env_data_dir(sc, &en).join("logs"),
                    Err(_) => cfg::env_data_dir(Scope::Local, &env_name).join("logs"),
                };
                (cname, engine, logs_dir)
            };
            // Surface the router-captured per-daemon stderr first (startup
            // crashes that the engine's own container logs may not show), then
            // the container's own logs. Snapshot regardless of --follow so it is
            // shown even when the engine `logs -f` below blocks.
            serve::dump_err_files(&logs_dir)?;
            // Apptainer has no `logs` subcommand: instances write to per-name
            // log files under ~/.apptainer/instances/logs/.... Hand this off
            // to serve.rs which knows the path layout.
            if matches!(engine, ContainerEngine::Apptainer) {
                return serve::apptainer_logs(&container_name, follow);
            }
            let exe = match engine {
                ContainerEngine::Podman => "podman",
                ContainerEngine::Docker => "docker",
                ContainerEngine::Apptainer => unreachable!(),
            };
            let mut cmd_args = vec!["logs"];
            if follow {
                cmd_args.push("-f");
            }
            cmd_args.push(&container_name);
            // Log content is the primary data of this command, so both the
            // container's original stdout and stderr should go to our stdout.
            // docker/podman logs preserves the original stream split; we merge
            // them so that `morloc-manager logs | grep ERROR` works.
            let stdout_handle = std::io::stdout();
            let status = std::process::Command::new(exe)
                .args(&cmd_args)
                .stdin(Stdio::null())
                .stdout(Stdio::inherit())
                .stderr(Stdio::from(stdout_handle))
                .status()
                .map_err(|e| ManagerError::EnvError(format!("Failed to run {exe} logs: {e}")))?;
            if !status.success() {
                return Err(ManagerError::EngineError {
                    engine,
                    code: status.code().unwrap_or(1),
                    stderr: String::new(),
                });
            }
            Ok(())
        }

        // ---- eval ----
        Cmd::Eval { first, second, port } => {
            let expr = if let Some(ref expr_arg) = second {
                // first is env name — validate it exists and its serve container is running
                let (env_name, _, ec) = resolve_env_or_active(Some(first))?;
                let container_name = serve::serve_container_name(&env_name);
                if !container::container_exists(ec.engine()?, &container_name) {
                    return Err(ManagerError::EnvError(format!(
                        "No serve container running for '{env_name}'. Start with: morloc-manager start {env_name}"
                    )));
                }
                expr_arg.clone()
            } else {
                first
            };
            use std::io::{Read as IoRead, Write as IoWrite};
            let body = format!("{{\"expr\":{}}}", serde_json::to_string(&expr).unwrap_or_default());
            let request = format!(
                "POST /eval HTTP/1.1\r\nHost: localhost\r\nContent-Type: application/json\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{}",
                body.len(), body
            );
            let addr = format!("127.0.0.1:{port}");
            let mut stream = std::net::TcpStream::connect(&addr).map_err(|e| {
                ManagerError::EnvError(format!(
                    "Cannot connect to serve container on {addr}: {e}\n  Is a serve container running? Start with: morloc-manager start"
                ))
            })?;
            stream.write_all(request.as_bytes()).map_err(|e| {
                ManagerError::EnvError(format!("Failed to send request: {e}"))
            })?;
            let mut response = String::new();
            stream.read_to_string(&mut response).map_err(|e| {
                ManagerError::EnvError(format!("Failed to read response: {e}"))
            })?;
            // Extract body from HTTP response (after \r\n\r\n)
            if let Some(pos) = response.find("\r\n\r\n") {
                let body = &response[pos + 4..];
                println!("{body}");
            } else {
                println!("{response}");
            }
            Ok(())
        }

        // ---- install ----
        Cmd::Install { src, engine_arg } => {
            // Resolve the target environment up front so the program's declared
            // dependencies can be provisioned into it BEFORE it is built.
            let (env_name, scope, ec) = resolve_env_or_active(None)?;

            // 1. Resolve the program's env requirements without building it
            //    (frontend only). A directory is a package whose entry is its
            //    main.loc; a bare file is the entry itself.
            let is_dir = std::path::Path::new(&src).is_dir();
            let envspec_target = if is_dir {
                std::path::Path::new(&src)
                    .join("main.loc")
                    .to_string_lossy()
                    .to_string()
            } else {
                src.clone()
            };
            let dry = capture_envspec(
                (env_name.clone(), scope, ec.clone()),
                &envspec_target,
                engine_arg.clone(),
                verbose,
            )?;

            // 2. Re-materialize so the module closure's package.yaml deps are in
            //    pixi before the build. The dry spec is ephemeral -- once the
            //    program builds, `morloc make --install` writes its real
            //    envspec.json, which gather_env_specs picks up from then on.
            rematerialize_env(scope, &env_name, &[dry], verbose)?;

            // 3. Build + install into the now-complete environment. Step 2 may
            //    have rebuilt the image, so re-read the config.
            //    A directory routes through `morloc install --build <dir>` (reads
            //    package metadata, installs by module name); a bare file through
            //    `morloc make --install <src>`. Either way the installed program
            //    is named after its module, not the source file.
            let ec = cfg::read_env_config(scope, &env_name)?;
            let args = if is_dir {
                vec![
                    "morloc".to_string(), "install".to_string(),
                    "--build".to_string(), src,
                ]
            } else {
                vec![
                    "morloc".to_string(), "make".to_string(),
                    "--install".to_string(), src,
                ]
            };
            runner::run_in_env(
                Some((env_name, scope, ec)),
                runner::RunRequest {
                    verbose,
                    shell: false,
                    args,
                    user_env: Vec::new(),
                    engine_args: engine_arg,
                    phase: Phase::Run,
                    slurm_bridge: false,
                },
            )
        }

        // ---- expose ----
        Cmd::Expose { action } => match action {
            ExposeAction::Add { module, protocols, env } => {
                let (env_name, scope, _ec) = resolve_env_or_active(env)?;
                // Exposure is a view of an INSTALLED program; catch typos early.
                let launcher = cfg::env_data_dir(scope, &env_name).join("bin").join(&module);
                if !launcher.exists() {
                    return Err(ManagerError::EnvError(format!(
                        "Module '{module}' is not installed in environment '{env_name}' \
                         (no bin/{module}).\n  Install it first: morloc-manager install <src>.loc"
                    )));
                }
                let mut ex = cfg::read_exposure(scope, &env_name)?;
                ex.add(&module, &protocols);
                cfg::write_exposure(scope, &env_name, &ex)?;
                let protos: Vec<&str> = protocols.iter().map(|p| p.as_str()).collect();
                eprintln!(
                    "Exposed '{module}' over {} in '{env_name}'. Run 'morloc-manager start' to serve.",
                    protos.join(", ")
                );
                Ok(())
            }
            ExposeAction::Rm { module, env } => {
                let (env_name, scope, _ec) = resolve_env_or_active(env)?;
                let mut ex = cfg::read_exposure(scope, &env_name)?;
                if ex.remove(&module) {
                    cfg::write_exposure(scope, &env_name, &ex)?;
                    eprintln!("Unexposed '{module}' in '{env_name}'. Run 'morloc-manager start' to apply.");
                } else {
                    eprintln!("'{module}' was not exposed in '{env_name}'.");
                }
                Ok(())
            }
            ExposeAction::List { env } => {
                let (env_name, scope, _ec) = resolve_env_or_active(env)?;
                let ex = cfg::read_exposure(scope, &env_name)?;
                print_exposure(&env_name, &ex, json);
                Ok(())
            }
            ExposeAction::Eval { allow, off, env } => {
                let (env_name, scope, _ec) = resolve_env_or_active(env)?;
                let mut ex = cfg::read_exposure(scope, &env_name)?;
                if off {
                    ex.eval = None;
                    eprintln!("Disabled eval in '{env_name}'.");
                } else if allow.is_empty() {
                    ex.eval = Some(EvalExposure { allow: Vec::new() });
                    eprintln!(
                        "Enabled eval in '{env_name}' with an EMPTY allow-list \
                         (eval can import nothing; add modules with --allow)."
                    );
                } else {
                    ex.eval = Some(EvalExposure { allow: allow.clone() });
                    eprintln!("Enabled eval in '{env_name}', allow-list: {}.", allow.join(", "));
                }
                cfg::write_exposure(scope, &env_name, &ex)?;
                Ok(())
            }
        },

        // ---- status ----
        Cmd::Status => {
            let mut all_containers: Vec<serve::ServeContainerInfo> = Vec::new();
            for engine in [
                ContainerEngine::Podman,
                ContainerEngine::Docker,
                ContainerEngine::Apptainer,
            ] {
                let exe = match engine {
                    ContainerEngine::Podman => "podman",
                    ContainerEngine::Docker => "docker",
                    // Either apptainer or singularity counts; both serve
                    // instances live under the same Apptainer engine.
                    ContainerEngine::Apptainer => {
                        if which("apptainer") {
                            "apptainer"
                        } else if which("singularity") {
                            "singularity"
                        } else {
                            ""
                        }
                    }
                };
                if !exe.is_empty() && which(exe) {
                    if let Ok(containers) = serve::query_serve_containers(engine, verbose) {
                        all_containers.extend(containers);
                    }
                }
            }
            // Enrich each running container with its runtime serve-record
            // (mode / modules / url) -- authoritative even under host-networking
            // where `docker ps` shows no port.
            for c in all_containers.iter_mut() {
                let rt = cfg::find_env_scope(&c.env)
                    .ok()
                    .and_then(|scope| cfg::read_serve_runtime(scope, &c.env));
                if let Some(rt) = rt {
                    c.mode = rt.mode();
                    c.modules = rt.modules_summary();
                    c.url = if rt.token_required { format!("{} (token)", rt.url()) } else { rt.url() };
                }
            }
            // Native serves (host processes) tracked via per-env serve records.
            all_containers.extend(native_running_serves());
            if json {
                #[derive(serde::Serialize)]
                struct StatusOutput { containers: Vec<serve::ServeContainerInfo> }
                let output = StatusOutput { containers: all_containers };
                println!("{}", serde_json::to_string_pretty(&output).unwrap());
            } else if all_containers.is_empty() {
                println!("No servers running.");
            } else {
                println!("Running servers:");
                println!("  {:<16} {:<12} {:<20} {:<32} STATUS", "ENV", "MODE", "MODULES", "URL");
                for c in &all_containers {
                    println!(
                        "  {:<16} {:<12} {:<20} {:<32} {}",
                        c.env, c.mode, c.modules, c.url, c.status
                    );
                }
            }
            Ok(())
        }

        // ---- doctor ----
        Cmd::Doctor { name, system, deep, strict, slurm } => {
            let (env_name, env_scope, ec) = if let Some(ref n) = name {
                let s = if system { Scope::System } else { cfg::find_env_scope(n)? };
                let c = cfg::read_env_config(s, n)?;
                (n.clone(), s, c)
            } else {
                resolve_env_or_active(None)?
            };
            if ec.backend.is_native() {
                if slurm {
                    eprintln!("Note: --slurm applies to the container backend; ignoring for a native env.");
                }
                return doctor::native_doctor(verbose, &env_name, env_scope, &ec, deep, strict, json);
            }
            doctor::doctor(ec.engine()?, verbose, &env_name, env_scope, &ec, deep, strict, slurm, json)
        }

    }
}

// ======================================================================
// Serve container discovery
// ======================================================================

/// Find exactly one running morloc-serve-* container across all engines.
/// Returns (container_name, engine). Errors if zero or multiple found.
fn find_running_serve_container() -> Result<(String, ContainerEngine)> {
    let mut found: Vec<(String, ContainerEngine)> = Vec::new();
    for engine in [
        ContainerEngine::Podman,
        ContainerEngine::Docker,
        ContainerEngine::Apptainer,
    ] {
        let exe = match engine {
            ContainerEngine::Podman => "podman",
            ContainerEngine::Docker => "docker",
            ContainerEngine::Apptainer => {
                if which("apptainer") {
                    "apptainer"
                } else if which("singularity") {
                    "singularity"
                } else {
                    ""
                }
            }
        };
        if !exe.is_empty() && which(exe) {
            for name in serve::find_running_serve_containers(engine) {
                found.push((name, engine));
            }
        }
    }
    match found.len() {
        0 => Err(ManagerError::EnvError(
            "No morloc serve containers running".to_string(),
        )),
        1 => Ok(found.into_iter().next().unwrap()),
        _ => {
            let names: Vec<String> = found.iter().map(|(n, _)| n.clone()).collect();
            Err(ManagerError::EnvError(format!(
                "Multiple serve containers running. Specify one explicitly:\n  {}",
                names.join("\n  ")
            )))
        }
    }
}

// ======================================================================
// Container run
// ======================================================================

/// Native-backend `run`: execute a command directly on the host against the
/// environment's own MORLOC_HOME, reconstructing the provisioned toolchain
/// environment from the materialization record. Invoked through the `Runner`
/// seam (`NativeRunner`).
pub(crate) fn native_run_env(
    env: &runner::ResolvedEnv,
    req: &runner::RunRequest,
) -> Result<()> {
    // Container-only inputs have no meaning on the host; reject rather than
    // silently drop them.
    if !req.engine_args.is_empty() {
        return Err(ManagerError::EnvError(
            "--engine-arg / -x is a container-only option; the native backend has no \
             container engine to pass flags to".to_string(),
        ));
    }
    if req.slurm_bridge {
        return Err(ManagerError::EnvError(
            "--slurm-bridge is only supported on the container backend".to_string(),
        ));
    }

    // The toolchain env-map is captured when the environment is materialized.
    // Its absence means the environment was never provisioned.
    let runtime = cfg::read_native_runtime(env.scope, &env.name).map_err(|_| {
        ManagerError::EnvError(format!(
            "native environment '{}' has not been materialized. \
             Run 'morloc-manager update {}' to provision its toolchain.",
            env.name, env.name
        ))
    })?;

    let data_dir = cfg::env_data_dir(env.scope, &env.name);
    let mh = data_dir.to_string_lossy().to_string();

    let mut cmd = if req.shell {
        if !io::stdin().is_terminal() || !io::stdout().is_terminal() {
            eprintln!("Error: --shell requires an interactive terminal (TTY).");
            std::process::exit(1);
        }
        let shell_exe = std::env::var("SHELL").unwrap_or_else(|_| "/bin/bash".to_string());
        Command::new(shell_exe)
    } else {
        let (program, rest) = req
            .args
            .split_first()
            .ok_or(ManagerError::NoCommand)?;
        let mut c = Command::new(program);
        c.args(rest);
        c
    };

    // Inherited environ + captured toolchain activation + MORLOC_HOME, then the
    // caller's `--env` overrides last so `-e KEY=VAL` always wins.
    for (k, v) in &runtime.activation_env {
        cmd.env(k, v);
    }
    cmd.env("MORLOC_HOME", &mh);
    // Managed-env marker: the boolean signal the compiler's dependency callback
    // gates on. Distinct from MORLOC_HOME (a general config-home override a user
    // may export anywhere) -- MORLOC_ENV is set ONLY when running inside a
    // managed environment, so a bare `morloc make` on the host never triggers a
    // dependency sync. Its value is the environment name (informative).
    cmd.env("MORLOC_ENV", &env.name);
    // The pixi binary, so the in-env `morloc-env` agent re-solves without
    // rediscovering it. Best-effort: pixi was already provisioned at materialize.
    if let Ok(pixi) = provision::provision_pixi(env.scope) {
        cmd.env("MORLOC_PIXI", pixi);
    }
    for (k, v) in &req.user_env {
        cmd.env(k, v);
    }

    let status = cmd.status().map_err(|e| {
        ManagerError::EnvError(format!("failed to launch command on host: {e}"))
    })?;
    let code = status.code().unwrap_or(1);
    if status.success() {
        Ok(())
    } else {
        std::process::exit(code);
    }
}

// ======================================================================
// Native backend: materialize + new
// ======================================================================

/// The release tag to provision the morloc runtime from: `$MORLOC_RELEASE_TAG`
/// if set (e.g. "dev" or "v0.98.3"), else "latest" (resolved via the releases
/// API). morloc-manager is self-bootstrapping -- it downloads the compiler +
/// Rust source (init builds the runtime) rather than requiring a host morloc install.
fn resolve_release_tag() -> String {
    std::env::var("MORLOC_RELEASE_TAG")
        .ok()
        .filter(|s| !s.is_empty())
        .unwrap_or_else(|| "latest".to_string())
}

/// morloc's language-support table, in preference order: `$MORLOC_LANG_SUPPORT`
/// (a JSON file, for pinning); a table already present in the provisioned runtime
/// (release download, or a dev cache from a prior run -- the dev override drops a
/// stale one so a hit is always current); else, emit it from the compiler and
/// cache it. `engine` is the container engine when building an image, so the
/// table can be generated inside a container on hosts that cannot run the glibc
/// compiler natively (NixOS/musl) -- no manual file shuttling required.
fn load_lang_support(
    runtime_dir: &std::path::Path,
    engine: Option<ContainerEngine>,
) -> Result<langsupport::LangSupport> {
    if let Ok(path) = std::env::var("MORLOC_LANG_SUPPORT") {
        if !path.is_empty() {
            let text = std::fs::read_to_string(&path).map_err(|e| {
                ManagerError::EnvError(format!("cannot read MORLOC_LANG_SUPPORT {path}: {e}"))
            })?;
            return langsupport::LangSupport::from_json(&text).map_err(Into::into);
        }
    }
    let table = runtime_dir.join(provision::LANG_SUPPORT_FILE);
    if table.is_file() {
        let text = std::fs::read_to_string(&table).map_err(|e| {
            ManagerError::EnvError(format!("cannot read {}: {e}", table.display()))
        })?;
        return langsupport::LangSupport::from_json(&text).map_err(Into::into);
    }
    // No table on disk: emit it from the compiler (the table is baked into the
    // compiler at build time) and cache it next to the runtime so later runs skip
    // regeneration.
    let morloc_bin = provision::runtime_morloc_bin(runtime_dir);
    let json = emit_lang_support(&morloc_bin, engine)?;
    let _ = std::fs::write(&table, &json);
    langsupport::LangSupport::from_json(&json).map_err(Into::into)
}

/// Emit morloc's language-support JSON by running the compiler. Tries the host
/// first (always right for native + glibc hosts); if the compiler cannot execute
/// there and a container engine is available, runs the SAME compiler inside the
/// engine's base image -- where it is guaranteed to run, being the binary baked
/// into the image being built. This is what makes `--engine podman` dev builds
/// work on hosts that cannot run the glibc compiler, with no manual step.
fn emit_lang_support(
    morloc_bin: &std::path::Path,
    engine: Option<ContainerEngine>,
) -> Result<String> {
    if let Ok(out) = Command::new(morloc_bin).arg("lang-support").output() {
        if out.status.success() {
            return Ok(String::from_utf8_lossy(&out.stdout).into_owned());
        }
    }
    if let Some(engine) = engine {
        return emit_lang_support_in_container(morloc_bin, engine);
    }
    Err(ManagerError::EnvError(
        "could not obtain the language-support table: the provisioned compiler could \
         not run on this host and no container engine is available to run it. Set \
         MORLOC_LANG_SUPPORT to a lang-support JSON file, or use a container backend \
         (--engine podman/docker) so the table is generated in a container."
            .to_string(),
    ))
}

/// Run the staged compiler inside the engine's base image to emit the language-
/// support table, for hosts that cannot execute it directly. The compiler is a
/// dynamic glibc binary, so its two runtime libs (libgmp, libz) are installed
/// into the slim base first; its directory is mounted read-only.
fn emit_lang_support_in_container(
    morloc_bin: &std::path::Path,
    engine: ContainerEngine,
) -> Result<String> {
    if matches!(engine, ContainerEngine::Apptainer) {
        return Err(ManagerError::EnvError(
            "cannot generate the language-support table under apptainer; set \
             MORLOC_LANG_SUPPORT to a lang-support JSON file"
                .to_string(),
        ));
    }
    // Resolve the real compiler file (dev symlinks runtimes/dev/morloc -> the
    // local build) and mount its directory read-only into the container.
    let real = std::fs::canonicalize(morloc_bin).map_err(|e| {
        ManagerError::EnvError(format!("cannot resolve {}: {e}", morloc_bin.display()))
    })?;
    let dir = real
        .parent()
        .ok_or_else(|| ManagerError::EnvError(format!("{} has no parent", real.display())))?;
    let file = real
        .file_name()
        .and_then(|s| s.to_str())
        .ok_or_else(|| ManagerError::EnvError(format!("{} has no file name", real.display())))?;
    let mount = format!("{}:/morloc-src:ro", dir.display());
    // Best-effort apt (still works if the base already has the libs), then exec
    // the compiler so ONLY its lang-support JSON reaches stdout.
    // The GHC-linked compiler needs libgmp + libz, and commonly libffi, at
    // runtime; install them into the slim base before invoking it.
    let script = format!(
        "if command -v apt-get >/dev/null 2>&1; then \
           apt-get update -qq >/dev/null 2>&1 && \
           apt-get install -y -qq --no-install-recommends libgmp10 zlib1g libffi8 >/dev/null 2>&1; \
         fi; exec /morloc-src/{file} lang-support"
    );
    eprintln!(
        "Generating the language-support table in a {} container...",
        engine.name()
    );
    let out = Command::new(crate::container::engine_executable(engine))
        .args([
            "run", "--rm", "-v", mount.as_str(), CONTAINER_BASE_IMAGE, "sh", "-c",
            script.as_str(),
        ])
        .output()
        .map_err(|e| ManagerError::EnvError(format!("failed to run {}: {e}", engine.name())))?;
    if !out.status.success() {
        return Err(ManagerError::EnvError(format!(
            "generating the language-support table in a container failed:\n{}",
            String::from_utf8_lossy(&out.stderr)
        )));
    }
    Ok(String::from_utf8_lossy(&out.stdout).into_owned())
}

/// Run `morloc init` on the host to build the environment's language shims into
/// its own MORLOC_HOME, against the pixi toolchain (on PATH via the activation
/// env-map) and the provisioned compiler + Rust source.
fn run_native_morloc_init(
    morloc_bin: &std::path::Path,
    env_dir: &std::path::Path,
    runtime_dir: &std::path::Path,
    activation: &[(String, String)],
    verbose: bool,
) -> Result<()> {
    let mut cmd = Command::new(morloc_bin);
    cmd.arg("init").arg("-f");
    if !verbose {
        cmd.arg("-q");
    }
    for (k, v) in activation {
        cmd.env(k, v);
    }
    cmd.env("MORLOC_HOME", env_dir);
    // init builds libmorloc.so + morloc-nexus from source with the env toolchain;
    // point it at the Rust workspace bundled in the provisioned runtime.
    cmd.env("MORLOC_RUST_DIR", provision::runtime_rust_src(runtime_dir));
    let status = cmd
        .status()
        .map_err(|e| ManagerError::EnvError(format!("could not run morloc init: {e}")))?;
    if !status.success() {
        return Err(ManagerError::EnvError(
            "morloc init failed while provisioning the native environment".to_string(),
        ));
    }
    Ok(())
}

/// Materialize a native environment: solve its pixi toolchain, capture the
/// activation env-map, provision the morloc compiler + Rust source, run `morloc init` on
/// the host, and persist the native runtime record. `specs` is the union of the
/// installed programs' requirements (empty for a bare env).
/// Parse `--lang` values into (language, optional version pin) pairs. Accepts
/// repeated flags and comma-separated lists; each atom is `lang` or `lang@pin`
/// (e.g. `py`, `py@3.12`, `r@4.3`).
fn parse_lang_pins(lang: &[String]) -> Vec<(String, Option<String>)> {
    let mut out = Vec::new();
    for entry in lang {
        for atom in entry.split(',') {
            let atom = atom.trim();
            if atom.is_empty() {
                continue;
            }
            match atom.split_once('@') {
                Some((l, p)) => out.push((l.trim().to_string(), Some(p.trim().to_string()))),
                None => out.push((atom.to_string(), None)),
            }
        }
    }
    out
}

/// Resolve one `--lang` pin into a language requirement, intersecting the pin
/// with morloc's supported range for that language and erroring legibly (in
/// morloc's vocabulary) when they conflict -- before pixi is ever invoked.
fn resolve_lang_pin(
    lang: &str,
    pin: Option<&str>,
    morloc_version: &str,
    support: &langsupport::LangSupport,
) -> Result<envspec::LangReq> {
    let supported = support
        .languages
        .get(lang)
        .and_then(|e| e.runtime.as_ref())
        .map(|r| r.version.as_str())
        .unwrap_or("*");
    let spec = constraint::resolve_lang_version(lang, morloc_version, None, pin, supported)?;
    let constraint = (spec != "*").then_some(spec);
    Ok(envspec::LangReq { lang: lang.to_string(), constraint, std: None })
}

/// A provisioned runtime plus the structured requirement set for an
/// environment. Both backends share this "gather -> RequirementSet" step; they
/// differ only in what they do with it (native: render + solve on host;
/// container: render + lock + build an image). The requirement set is the
/// backend-neutral IR; the pixi manifest is rendered from it at the point of use.
struct ResolvedRequirements {
    /// The downloaded runtime store (`runtimes/<version>/`); its `rust/` subdir
    /// is `morloc init`'s MORLOC_RUST_DIR.
    runtime_dir: std::path::PathBuf,
    /// The concrete morloc version provisioned.
    version: String,
    /// The provisioned morloc compiler (`runtime_dir/morloc`).
    morloc_bin: std::path::PathBuf,
    /// The union of `program_specs` and the `--lang` pins.
    specs: Vec<envspec::EnvSpec>,
    /// The `--lang` pin spec (a languages-only EnvSpec), when any pins were
    /// given. Persisted into the requirements store as the env's initialization
    /// row so the in-env `morloc-env` agent re-solves with the same pins the
    /// manager used (otherwise it would drop the pin and drift the toolchain).
    lang_spec: Option<envspec::EnvSpec>,
    /// The structured requirement set (backend-neutral IR).
    requirements: pixi::RequirementSet,
}

/// Provision the morloc runtime (download the compiler + Rust source; no host install),
/// then lower the environment's requirements to a pixi manifest. `--lang` pins
/// are validated against morloc's supported range here (a legible pin-time error
/// before pixi runs).
fn resolve_env_requirements(
    scope: Scope,
    name: &str,
    program_specs: &[envspec::EnvSpec],
    lang_pins: &[(String, Option<String>)],
    engine: Option<ContainerEngine>,
) -> Result<ResolvedRequirements> {
    // provision_runtime announces which path it took (local dev override vs
    // release download), so the message is accurate rather than always "latest".
    let tag = resolve_release_tag();
    let (runtime_dir, version) = provision::provision_runtime(scope, &tag)?;
    let morloc_bin = provision::runtime_morloc_bin(&runtime_dir);
    // `engine` lets the table be generated in a container when the host cannot
    // run the compiler (native passes None; it runs on the host by definition).
    let support = load_lang_support(&runtime_dir, engine)?;

    let mut specs: Vec<envspec::EnvSpec> = program_specs.to_vec();
    let mut lang_spec = None;
    if !lang_pins.is_empty() {
        let mut langreqs = Vec::new();
        for (lang, pin) in lang_pins {
            langreqs.push(resolve_lang_pin(lang, pin.as_deref(), &version, &support)?);
        }
        let ls = envspec::EnvSpec::from_languages(&version, langreqs);
        specs.push(ls.clone());
        lang_spec = Some(ls);
    }

    let profile = hostprobe::probe_host();
    let channels = pixi::default_channels();
    let requirements = pixi::resolve_requirements(&pixi::PixiManifestInput {
        env_name: name,
        platform: &profile.platform,
        channels: &channels,
        specs: &specs,
        lang_support: &support,
    })?;

    Ok(ResolvedRequirements { runtime_dir, version, morloc_bin, specs, lang_spec, requirements })
}

/// Materialize a native environment: solve its pixi toolchain, capture the
/// activation env-map, provision the morloc compiler + Rust source, run `morloc init` on
/// the host, and persist the native runtime record. The toolchain is the union
/// of the installed programs' requirements (`program_specs`) and the user's
/// `--lang` pins. Re-solving is skipped when the rendered manifest is unchanged
/// from a prior materialization (avoids a redundant solve on every `update`).
fn materialize_native_env(
    scope: Scope,
    name: &str,
    program_specs: &[envspec::EnvSpec],
    lang_pins: &[(String, Option<String>)],
    verbose: bool,
) -> Result<String> {
    // Native runs on the host, so the compiler executes there -- no engine needed.
    let req = resolve_env_requirements(scope, name, program_specs, lang_pins, None)?;

    // Phase 1 of the impurity gate: a fast reject on host/vcpkg system deps that
    // conda cannot provide (the pixi solve below is the accurate phase 2).
    let blockers: Vec<String> = req.specs.iter().flat_map(|s| s.native_blockers()).collect();
    if !blockers.is_empty() {
        return Err(ManagerError::EnvError(format!(
            "this environment cannot be built on the native backend:\n  {}",
            blockers.join("\n  ")
        )));
    }

    let env_dir = cfg::env_data_dir(scope, name);
    prime_requirements_store(&env_dir, &req);
    let pixi_dir = env_dir.join("pixi");
    let manifest = pixi::render_manifest(&req.requirements);

    // Solve cache: compare against the last SUCCESSFULLY materialized manifest (a
    // marker written only after init completes), NOT the working pixi.toml. Using
    // a success marker avoids a poisoned skip -- a failed solve leaves a new
    // pixi.toml on disk, but the marker still reflects the last good build, so the
    // rebuild is not falsely skipped on retry.
    let key = cache_key(&manifest, &req.morloc_bin, &[]);
    let marker = pixi_dir.join("materialized.toml");
    let unchanged = std::fs::read_to_string(&marker)
        .map(|prev| prev == key)
        .unwrap_or(false);
    if unchanged && cfg::read_native_runtime(scope, name).is_ok() {
        eprintln!("Native toolchain is up to date (requirements unchanged).");
        return Ok(req.version);
    }

    // Requirements changed -> `morloc init` below rebuilds and cp/links over the
    // env's morloc-nexus + libmorloc.so. Refuse if a serve is live: overwriting a
    // running executable fails with ETXTBSY and corrupts the live runtime.
    if let Some(rt) = cfg::read_serve_runtime(scope, name) {
        if let Some(handle) = &rt.handle {
            if serve_handle_alive(handle) {
                return Err(ManagerError::EnvError(format!(
                    "Cannot re-materialize native environment '{name}' while it is being \
                     served: `morloc init` would overwrite the running runtime. \
                     Run 'morloc-manager stop {name}' first."
                )));
            }
        }
    }

    pixi::write_manifest(&pixi_dir, &manifest)?;
    let pixi_bin = provision::provision_pixi(scope)?;
    eprintln!("Solving native toolchain with pixi (this may take a few minutes)...");
    pixi::solve(&pixi_dir, &pixi_bin)?;
    let mut activation = pixi::capture_activation(&pixi_dir, &pixi_bin)?;
    // Expose the env's own bin (nexus + installed program launchers) and the
    // provisioned runtime (the morloc compiler) on PATH, so `run -- <program>`
    // and `run -- morloc ...` resolve inside the env alongside the conda toolchain.
    prepend_to_path(&mut activation, &[env_dir.join("bin"), req.runtime_dir.clone()]);

    run_native_morloc_init(&req.morloc_bin, &env_dir, &req.runtime_dir, &activation, verbose)?;

    // Pin the interpreter minors the shims were just built against (see
    // record_abi_lock_or_warn). The env's pixi dir is the EnvContext default.
    record_abi_lock_or_warn(&env_dir, name, &req.version);

    cfg::write_native_runtime(scope, name, &NativeRuntime { activation_env: activation })?;
    // Record the cache key (manifest + compiler identity) as successfully
    // materialized, written only now that the solve + init have succeeded.
    let _ = std::fs::write(&marker, &key);
    Ok(req.version)
}

/// Prepend `dirs` (in order) to the `PATH` entry of an activation env-map,
/// inserting a `PATH` entry if none exists.
fn prepend_to_path(activation: &mut Vec<(String, String)>, dirs: &[std::path::PathBuf]) {
    let prefix = dirs
        .iter()
        .map(|d| d.to_string_lossy().to_string())
        .collect::<Vec<_>>()
        .join(":");
    if prefix.is_empty() {
        return;
    }
    for (k, v) in activation.iter_mut() {
        if k == "PATH" {
            *v = format!("{prefix}:{v}");
            return;
        }
    }
    activation.push(("PATH".to_string(), prefix));
}

/// Collect the requirements of every program installed into a native env by
/// reading the `envspec.json` files morloc writes into each program's build dir
/// under the env's MORLOC_HOME. A malformed or unreadable spec is skipped (with
/// a warning) rather than aborting the whole re-solve.
/// The build-dir program key for an `envspec.json` path: the parent dir is
/// `<key>-build`, so strip the suffix.
fn build_dir_key(envspec_path: &std::path::Path) -> Option<String> {
    envspec_path
        .parent()
        .and_then(|d| d.file_name())
        .and_then(|n| n.to_str())
        .map(|b| b.strip_suffix("-build").unwrap_or(b).to_string())
}

/// Mirror each installed program's build-dir `envspec.json` into the env's
/// requirements store under `installed/<key>.json`. The in-env `morloc-env`
/// agent reads only the store, so this keeps its view of the installed baseline
/// complete -- otherwise a scratch sync would solve a world missing the
/// installed programs' deps and uninstall them. Best-effort.
fn seed_installed_store(env_dir: &std::path::Path) {
    let ctx = envstore::EnvContext::new(env_dir);
    for path in find_envspec_files(env_dir) {
        if let (Some(key), Ok(json)) = (build_dir_key(&path), std::fs::read_to_string(&path)) {
            let _ = ctx.write_spec(&key, envstore::Provenance::Installed, &json);
        }
    }
}

/// Prime the requirements store BEFORE the solve, identically for both backends:
/// seed the installed baseline from each program's build-dir envspec, and persist
/// the env's --lang pins as its toolchain spec. Without this the in-env agent
/// re-solves a world missing the installed programs' deps or the interpreter pins.
fn prime_requirements_store(env_dir: &std::path::Path, req: &ResolvedRequirements) {
    seed_installed_store(env_dir);
    if let Some(json) = req.lang_spec.as_ref().and_then(|ls| serde_json::to_string(ls).ok()) {
        let _ = envstore::EnvContext::new(env_dir).write_toolchain(&json);
    }
}

/// Record the shim-ABI pin AFTER the solve (see `envstore::record_abi_lock`),
/// identically for both backends; a failure is a warning, not fatal. The env's
/// pixi dir is the `EnvContext` default (`<env_dir>/pixi`) for both.
fn record_abi_lock_or_warn(env_dir: &std::path::Path, name: &str, version: &str) {
    if let Err(e) = envstore::EnvContext::new(env_dir).record_abi_lock(version) {
        eprintln!("Warning: could not record the ABI lock for '{name}': {e}");
    }
}

fn gather_env_specs(scope: Scope, name: &str) -> Vec<envspec::EnvSpec> {
    let env_dir = cfg::env_data_dir(scope, name);
    let ctx = envstore::EnvContext::new(&env_dir);
    // The requirements store is authoritative; a program it describes is NOT
    // re-read from its legacy build-dir `envspec.json` (that would double-count
    // it, and `aggregate` intersects constraints across all specs, so a divergent
    // stale build-dir copy would yield a phantom empty set). Prefer-store also
    // ignores a stale build-dir spec left by a failed rebuild.
    let store_programs: std::collections::HashSet<String> =
        ctx.program_names().unwrap_or_default().into_iter().collect();

    let mut specs = Vec::new();
    // Build-dir scan, only for programs the store does not yet describe (an env
    // whose specs have not been seeded into the store).
    for path in find_envspec_files(&env_dir) {
        if build_dir_key(&path).map_or(false, |k| store_programs.contains(&k)) {
            continue;
        }
        let build_dir = path.parent().unwrap_or(&env_dir);
        match envspec::EnvSpec::read_from_build_dir(build_dir) {
            Ok(spec) => specs.push(spec),
            Err(e) => eprintln!("warning: skipping {}: {e}", path.display()),
        }
    }
    // The store: installed baseline + scratch builds.
    match ctx.gather() {
        Ok(store_specs) => specs.extend(store_specs),
        Err(e) => eprintln!("warning: reading requirements store: {e}"),
    }
    specs
}

/// Find `envspec.json` files under `root`, skipping large non-source subtrees
/// (the pixi env, cargo/target dirs) so the scan stays cheap.
fn find_envspec_files(root: &std::path::Path) -> Vec<std::path::PathBuf> {
    let mut found = Vec::new();
    let mut stack = vec![root.to_path_buf()];
    while let Some(dir) = stack.pop() {
        let entries = match std::fs::read_dir(&dir) {
            Ok(e) => e,
            Err(_) => continue,
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                let skip = matches!(
                    path.file_name().and_then(|n| n.to_str()),
                    Some("pixi") | Some(".pixi") | Some("target") | Some("rust") | Some(".cargo")
                );
                if !skip {
                    stack.push(path);
                }
            } else if path.file_name().and_then(|n| n.to_str()) == Some("envspec.json") {
                found.push(path);
            }
        }
    }
    found
}

/// Create a native (no-container) environment: record it, then materialize its
/// pixi toolchain + morloc runtime on the host (unless `no_init`).
/// Resolve, validate, and dedup-check a new environment's name (shared by the
/// native and container `new` paths). Prompts interactively when no name given.
fn resolve_new_env_name(scope: Scope, name: Option<String>, interactive: bool) -> Result<String> {
    let env_name = match name {
        Some(n) => n,
        None if interactive => {
            eprint!("Environment name: ");
            io::stderr().flush().ok();
            let mut buf = String::new();
            io::stdin().read_line(&mut buf).ok();
            buf.trim().to_string()
        }
        None => "morloc-env".to_string(),
    };
    if env_name.is_empty() {
        return Err(ManagerError::EnvError("Environment name cannot be empty".to_string()));
    }
    environment::validate_env_name(&env_name)?;
    if cfg::env_config_path(scope, &env_name).is_file() {
        return Err(ManagerError::EnvError(format!(
            "Environment '{env_name}' already exists"
        )));
    }
    Ok(env_name)
}

/// Persist a freshly created environment: its `--lang` inputs, its config, a
/// default active-config when none exists, and the "ready" banner. Shared by the
/// native and container `new` paths.
fn finalize_new_env(
    scope: Scope,
    ec: &EnvironmentConfig,
    lang_pins: Vec<(String, Option<String>)>,
) -> Result<()> {
    cfg::write_env_inputs(scope, &ec.name, &EnvInputs { lang_pins })?;
    cfg::write_env_config(scope, &ec.name, ec)?;
    if cfg::read_active_config().is_none() {
        cfg::write_config(
            &cfg::config_path(scope),
            &Config { active_env: None, backend: ec.backend },
        )?;
    }
    let kind = if ec.backend.is_native() { "Native" } else { "Container" };
    anstream::eprintln!("\x1b[1;32m{kind} environment '{}' is ready.\x1b[0m", ec.name);
    eprintln!("Activate it with: morloc-manager select {}", ec.name);
    Ok(())
}

fn native_new(
    scope: Scope,
    name: Option<String>,
    lang: Vec<String>,
    no_init: bool,
    interactive: bool,
    verbose: bool,
) -> Result<()> {
    let env_name = resolve_new_env_name(scope, name, interactive)?;

    // A new env has no installed programs yet; its toolchain is morloc's core
    // language-support table plus any `--lang` pins. Provisioning (inside
    // materialize) yields the concrete morloc version.
    let lang_pins = parse_lang_pins(&lang);
    let morloc_version = if no_init {
        None
    } else {
        materialize_native_env(scope, &env_name, &[], &lang_pins, verbose)?
            .parse::<Version>()
            .ok()
    };

    let ec = EnvironmentConfig::new_backend(
        env_name,
        Backend::Native,
        String::new(),
        None,
        morloc_version,
        Vec::new(),
    );
    finalize_new_env(scope, &ec, lang_pins)
}


/// Slim base image for requirement-derived container builds. Fully qualified
/// (registry + `library/` namespace) so podman resolves it without a
/// containers-registries.conf: podman refuses to expand a bare short name like
/// `debian:bookworm-slim` when no unqualified-search-registries config is present
/// (common on NixOS), whereas docker silently assumes Docker Hub. The explicit
/// form works identically on both.
const CONTAINER_BASE_IMAGE: &str = "docker.io/library/debian:bookworm-slim";

/// Resolve a to-be-installed program's declared env requirements WITHOUT building
/// it, by running the frontend-only `morloc envspec` in the environment (host for
/// native, in a fresh container of the env image for the container backend) and
/// capturing its stdout. Running this BEFORE materialize is what lets a module's
/// package.yaml deps reach pixi ahead of the build.
fn capture_envspec(
    target: (String, Scope, EnvironmentConfig),
    envspec_target: &str,
    engine_args: Vec<String>,
    verbose: bool,
) -> Result<envspec::EnvSpec> {
    let json = capture_in_env(
        target,
        vec![
            "morloc".to_string(),
            "envspec".to_string(),
            envspec_target.to_string(),
        ],
        engine_args,
        verbose,
    )?;
    envspec::EnvSpec::from_json(&json).map_err(Into::into)
}

/// Run a command in the environment and CAPTURE its stdout, returning it as a
/// String. Unlike `run_in_env` (which streams stdio and, on a non-zero child
/// exit, calls `std::process::exit`), this returns a normal `Result`: a non-zero
/// exit becomes an `Err` carrying the child's stderr, so callers can recover and
/// the process is never killed from inside. The child's stderr is passed through
/// (native) / surfaced on failure (container), so diagnostics stay visible.
fn capture_in_env(
    target: (String, Scope, EnvironmentConfig),
    args: Vec<String>,
    engine_args: Vec<String>,
    verbose: bool,
) -> Result<String> {
    let (name, scope, ec) = target;
    if ec.backend.is_native() {
        native_capture_env(scope, &name, &args)
    } else {
        container_capture_env(scope, &name, &ec, &args, &engine_args, verbose)
    }
}

/// Native capture: run the command on the host under the env's captured toolchain
/// activation, capturing stdout (the result) and stderr (surfaced on failure so
/// diagnostics stay visible and are inspectable by the caller).
fn native_capture_env(scope: Scope, name: &str, args: &[String]) -> Result<String> {
    let runtime = cfg::read_native_runtime(scope, name).map_err(|_| {
        ManagerError::EnvError(format!(
            "native environment '{name}' has not been materialized. \
             Run 'morloc-manager update {name}' to provision its toolchain."
        ))
    })?;
    let data_dir = cfg::env_data_dir(scope, name);
    let (program, rest) = args.split_first().ok_or(ManagerError::NoCommand)?;
    let mut cmd = Command::new(program);
    cmd.args(rest);
    for (k, v) in &runtime.activation_env {
        cmd.env(k, v);
    }
    cmd.env("MORLOC_HOME", data_dir.to_string_lossy().to_string());
    let out = cmd
        .output()
        .map_err(|e| ManagerError::EnvError(format!("failed to run command on host: {e}")))?;
    if !out.status.success() {
        return Err(ManagerError::EnvError(format!(
            "resolving requirements failed in native environment '{name}':\n{}",
            String::from_utf8_lossy(&out.stderr).trim()
        )));
    }
    Ok(String::from_utf8_lossy(&out.stdout).into_owned())
}

/// Container capture: run the command in a fresh, removed-after container of the
/// env image (state + cwd mounted, env's engine flags applied), capturing stdout.
fn container_capture_env(
    scope: Scope,
    name: &str,
    ec: &EnvironmentConfig,
    args: &[String],
    engine_args: &[String],
    verbose: bool,
) -> Result<String> {
    let engine = ec.engine()?;
    let image = ec.active_image().to_string();
    require_docker_socket(engine)?;
    if !container::image_exists_locally(engine, &image) {
        return Err(ManagerError::EnvError(format!(
            "Image '{image}' not found. Run 'morloc-manager update {name}' to build it."
        )));
    }
    let data_dir = cfg::env_data_dir(scope, name);
    let v_data_dir = data_dir.to_string_lossy().to_string();
    let cwd = std::env::current_dir()
        .map_err(|e| ManagerError::EnvError(format!("cannot resolve current directory: {e}")))?
        .to_string_lossy()
        .to_string();
    // HOME lives under the mounted state (oci_base_env); make it on the host side
    // so writes do not hit ENOENT.
    let _ = std::fs::create_dir_all(format!("{v_data_dir}/home"));

    let mut cfg = container::RunConfig::new(&image);
    cfg.remove_after = true;
    cfg.bind_mounts = vec![
        (v_data_dir, serve::CONTAINER_MORLOC_STATE.to_string()),
        (cwd.clone(), cwd.clone()),
    ];
    cfg.env = serve::oci_base_env(serve::CONTAINER_MORLOC_HOME);
    cfg.command = Some(args.to_vec());
    cfg.work_dir = Some(cwd);
    cfg.selinux_suffix = volume_suffix(detect_selinux()).to_string();
    cfg.shm_size = Some(ec.shm_size.clone());
    cfg.extra_flags = engine_args.to_vec();

    if verbose {
        eprintln!("[morloc-manager] capturing in {}: {}", engine.name(), args.join(" "));
    }
    let (status, stdout, stderr) = container::container_run_quiet(engine, &cfg);
    if !status.success() {
        return Err(ManagerError::EnvError(format!(
            "resolving requirements in the container failed:\n{}",
            stderr.trim()
        )));
    }
    Ok(stdout)
}

/// A cheap identity for the provisioned compiler: its size + mtime, after
/// following the dev symlink to the real build. A `stack install` rebuild changes
/// the mtime, so this invalidates the solve/image cache when the compiler changes
/// even if the conda requirements did not. For a release the version-keyed store
/// keeps it stable across runs of the same version.
fn compiler_fingerprint(morloc_bin: &std::path::Path) -> String {
    match std::fs::canonicalize(morloc_bin).and_then(std::fs::metadata) {
        Ok(m) => {
            let mtime = m
                .modified()
                .ok()
                .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
                .map(|d| d.as_secs())
                .unwrap_or(0);
            format!("{}:{}", m.len(), mtime)
        }
        Err(_) => "unknown".to_string(),
    }
}

/// The solve-cache key: the rendered pixi manifest plus the compiler identity.
/// Stored in a `materialized.toml` marker written only after a SUCCESSFUL
/// materialize, and compared on the next run; a change in either the conda
/// requirements OR the compiler forces a rebuild (so a dev compiler rebuild
/// refreshes the env without a manual image removal).
fn cache_key(manifest: &str, morloc_bin: &std::path::Path, system_packages: &[String]) -> String {
    // System packages live outside the pixi manifest, so fold them in or an
    // edit would not invalidate the marker. Omitted when empty to keep the key
    // byte-identical for envs that declare none (no spurious rebuild).
    let extras = if system_packages.is_empty() {
        String::new()
    } else {
        format!("# system-packages: {}\n", system_packages.join(" "))
    };
    format!(
        "{manifest}\n# morloc-compiler: {}\n{extras}",
        compiler_fingerprint(morloc_bin)
    )
}

/// Re-solve and re-materialize an environment from the union of every installed
/// program's envspec (`gather_env_specs`) plus any `extra_specs` not yet on disk
/// -- e.g. the dry envspec of a program about to be installed. Shared by
/// `update` (empty `extra_specs`) and `install`. The extra specs are EPHEMERAL:
/// they are never persisted; once the program builds, `morloc make --install`
/// writes its real `envspec.json`, which `gather_env_specs` then picks up. The
/// container/native solve caches make this a no-op when requirements are
/// unchanged.
fn rematerialize_env(
    scope: Scope,
    name: &str,
    extra_specs: &[envspec::EnvSpec],
    verbose: bool,
) -> Result<()> {
    let ec = cfg::read_env_config(scope, name)?;
    let lang_pins = cfg::read_env_inputs(scope, name).lang_pins;
    let mut specs = gather_env_specs(scope, name);
    specs.extend_from_slice(extra_specs);

    if ec.backend.is_native() {
        materialize_native_env(scope, name, &specs, &lang_pins, verbose)?;
        return Ok(());
    }

    let ce = ec.engine()?;
    let (image_tag, mver) = build_requirement_derived_image(
        scope, name, ce, &specs, &lang_pins, &ec.system_packages,
    )?;
    let mut ec = ec;
    ec.built_image = Some(image_tag);
    ec.morloc_version = mver.parse::<Version>().ok();
    cfg::write_env_config(scope, name, &ec)?;
    Ok(())
}

/// Build a requirement-derived container image: render the env's pixi.toml
/// (shared with the native backend), lock it, stage the morloc runtime + a
/// generated Dockerfile into a build context, and build the image. Returns the
/// built image tag + the provisioned morloc version.
fn build_requirement_derived_image(
    scope: Scope,
    name: &str,
    engine: ContainerEngine,
    program_specs: &[envspec::EnvSpec],
    lang_pins: &[(String, Option<String>)],
    system_packages: &[String],
) -> Result<(String, String)> {
    // Unlike native, a container HAS a build layer, so host/vcpkg system deps are
    // not a hard blocker here -- they become build-extras (a later --system-package
    // flag). native_blockers therefore does not apply to the container path.
    // Pass the engine so the language-support table can be generated in a
    // container when the host cannot run the compiler (NixOS/musl).
    let req = resolve_env_requirements(scope, name, program_specs, lang_pins, Some(engine))?;

    let context = cfg::env_data_dir(scope, name).join("container-build");
    let manifest = pixi::render_manifest(&req.requirements);
    let image_tag = format!("localhost/morloc-env:{name}");

    // Solve cache (mirrors the native path): skip the multi-minute solve + image
    // rebuild when the requirements AND the compiler are unchanged and the image
    // still exists. Compare against a success marker (the cache key = manifest +
    // compiler identity) written only after a good build (NOT the working
    // pixi.toml), so a failed build cannot poison the cache, and a rebuilt dev
    // compiler forces a fresh image without a manual `podman rmi`.
    let key = cache_key(&manifest, &req.morloc_bin, system_packages);
    let marker = context.join("materialized.toml");
    let unchanged = std::fs::read_to_string(&marker)
        .map(|prev| prev == key)
        .unwrap_or(false);
    if unchanged && container::image_exists_locally(engine, &image_tag) {
        eprintln!("Environment image is up to date (requirements unchanged).");
        return Ok((image_tag, req.version));
    }

    // Pliable container: the pixi env and the morloc shims are NOT baked into the
    // image; they are materialized into host-mounted dirs (`<env_dir>/pixi` ->
    // /env, `<env_dir>/runtime` -> MORLOC_HOME) so an in-container `morloc make`
    // can mutate them in place. Render + lock the pixi env directly in the host
    // pixi dir (mounted at /env during materialize + every run).
    let env_dir = cfg::env_data_dir(scope, name);
    let pixi_host = env_dir.join("pixi");
    let home_host = env_dir.join("runtime");
    for d in [&pixi_host, &home_host] {
        std::fs::create_dir_all(d)
            .map_err(|e| ManagerError::EnvError(format!("cannot create {}: {e}", d.display())))?;
    }
    // Prime the requirements store for the in-container morloc-env agent, exactly
    // as the native path does.
    prime_requirements_store(&env_dir, &req);
    pixi::write_manifest(&pixi_host, &manifest)?;
    let pixi_bin = provision::provision_pixi(scope)?;
    eprintln!("Solving + locking the environment with pixi...");
    pixi::lock(&pixi_host, &pixi_bin)?;

    // Build the requirement-INDEPENDENT base image (base tools + pixi + the
    // compiler/rust source). The env-specific pixi solve + `morloc init` run in a
    // container step below, writing into the host mounts.
    eprintln!("Staging the morloc runtime into the build context...");
    provision::stage_runtime(&req.runtime_dir, &context.join("runtime"))?;
    let extras = dockerfile::BuildExtras {
        system_packages: system_packages.to_vec(),
    };
    let df_text = dockerfile::generate_dockerfile(&dockerfile::DockerfileInput {
        base_image: CONTAINER_BASE_IMAGE,
        pixi_version: provision::PIXI_VERSION,
        morloc_home: serve::CONTAINER_MORLOC_HOME,
        extras: &extras,
    });
    let df_path = context.join("Dockerfile");
    std::fs::write(&df_path, df_text)
        .map_err(|e| ManagerError::EnvError(format!("cannot write {}: {e}", df_path.display())))?;

    eprintln!("Building the environment base image ({image_tag}) with {}...", engine.name());
    let cfg = crate::container::BuildConfig {
        dockerfile: df_path.to_string_lossy().to_string(),
        context: context.to_string_lossy().to_string(),
        tag: image_tag.clone(),
        build_args: Vec::new(),
        extra_flags: Vec::new(),
    };
    let status = crate::container::container_build_visible(engine, &cfg);
    if !status.success() {
        return Err(ManagerError::EngineError {
            engine,
            code: crate::container::exit_code_to_int(status),
            stderr: "requirement-derived base image build failed".to_string(),
        });
    }

    // Materialize: solve the pixi env + build the morloc shims INSIDE a container
    // (as the host UID via keep-id), writing into the host-mounted /env and
    // MORLOC_HOME -- prefix-correct because solved at their final runtime paths.
    eprintln!("Materializing the environment (pixi install + morloc init) in a container...");
    materialize_container_env(engine, &image_tag, &env_dir)?;

    // Pin the interpreter minors the shims were just built against, read from the
    // now-solved conda prefix under the env's (host-mounted) pixi dir.
    record_abi_lock_or_warn(&env_dir, name, &req.version);

    // Record the cache key (manifest + compiler identity) as successfully built +
    // materialized, written only now that both have succeeded.
    let _ = std::fs::write(&marker, &key);
    Ok((image_tag, req.version))
}

/// Solve the pixi env + build the morloc runtime shims INSIDE a container,
/// writing into the host-mounted `/env` and MORLOC_HOME. Runs as the host UID
/// (keep-id via `container_run`), so the results are host-owned and writable by
/// a later in-container `morloc make`.
fn materialize_container_env(
    engine: ContainerEngine,
    image: &str,
    env_dir: &std::path::Path,
) -> Result<()> {
    let v_data_dir = env_dir.to_string_lossy().to_string();
    // Solve /env from its mounted pixi.toml/lock, activate the now-solved
    // toolchain, then build libmorloc/nexus into MORLOC_HOME.
    let activate = serve::conda_activate_lines().join("\n");
    let script = format!(
        "set -e\n{pixi} install --locked\n{activate}\nmorloc init -f\n",
        pixi = serve::CONTAINER_PIXI_BIN,
    );
    let cfg = crate::container::RunConfig {
        image: image.to_string(),
        bind_mounts: vec![
            (v_data_dir.clone(), serve::CONTAINER_MORLOC_STATE.to_string()),
            (format!("{v_data_dir}/pixi"), serve::CONTAINER_PIXI_DIR.to_string()),
            (
                format!("{v_data_dir}/runtime"),
                serve::CONTAINER_MORLOC_HOME.to_string(),
            ),
        ],
        ports: Vec::new(),
        publish_host: None,
        network: None,
        env: serve::oci_base_env(serve::CONTAINER_MORLOC_HOME),
        read_only: false,
        interactive: false,
        remove_after: true,
        name: None,
        shm_size: None,
        command: Some(vec!["bash".to_string(), "-c".to_string(), script]),
        work_dir: Some(serve::CONTAINER_PIXI_DIR.to_string()),
        selinux_suffix: volume_suffix(detect_selinux()).to_string(),
        extra_flags: Vec::new(),
    };
    let (status, _out, stderr) = crate::container::container_run(engine, &cfg);
    if !status.success() {
        return Err(ManagerError::EnvError(format!(
            "environment materialization (pixi install + morloc init) failed:\n{}",
            stderr.trim()
        )));
    }
    Ok(())
}

/// Create a container environment whose image is derived from its requirements
/// (a generated Dockerfile running pixi inside), rather than a pulled/recipe
/// image. Mirrors `native_new`; the run substrate is unchanged.
fn container_new_derived(
    scope: Scope,
    engine: ContainerEngine,
    name: Option<String>,
    lang: Vec<String>,
    system_packages: Vec<String>,
    no_init: bool,
    interactive: bool,
) -> Result<()> {
    let env_name = resolve_new_env_name(scope, name, interactive)?;

    let lang_pins = parse_lang_pins(&lang);
    let (built_image, morloc_version) = if no_init {
        (None, None)
    } else {
        let (image, version) = build_requirement_derived_image(
            scope, &env_name, engine, &[], &lang_pins, &system_packages,
        )?;
        (Some(image), version.parse::<Version>().ok())
    };

    let ec = EnvironmentConfig::new_backend(
        env_name,
        Backend::Container(engine),
        CONTAINER_BASE_IMAGE.to_string(),
        built_image,
        morloc_version,
        system_packages,
    );
    finalize_new_env(scope, &ec, lang_pins)
}

// ======================================================================
// Native serve (detached host nexus)
// ======================================================================

/// Whether a native serve pid is still our nexus: alive, and (on Linux) its
/// cmdline still looks like morloc-nexus -- a guard against PID reuse.
fn native_serve_alive(pid: u32) -> bool {
    use nix::sys::signal::kill;
    use nix::unistd::Pid;
    if kill(Pid::from_raw(pid as i32), None).is_err() {
        return false;
    }
    match std::fs::read(format!("/proc/{pid}/cmdline")) {
        Ok(bytes) => String::from_utf8_lossy(&bytes).contains("morloc-nexus"),
        Err(_) => true, // no /proc (e.g. macOS): trust the liveness signal
    }
}

/// SIGTERM then, after a grace period, SIGKILL a native serve process group (the
/// nexus + its pool daemons). Best-effort. Killing the GROUP (negative pid) is
/// what reaps the child pool daemons; SIGTERM first lets the nexus clean its SHM.
fn kill_native_group(pgid: u32) {
    use nix::sys::signal::{kill, Signal};
    use nix::unistd::Pid;
    let group = Pid::from_raw(-(pgid as i32));
    let _ = kill(group, Signal::SIGTERM);
    for _ in 0..30 {
        std::thread::sleep(std::time::Duration::from_millis(100));
        // Poll the whole GROUP (negative pgid), not just the leader: a pool daemon
        // that ignores SIGTERM and outlives the nexus leader must still gate the
        // early return, or it would be left running (SHM/port leak).
        if kill(group, None).is_err() {
            return; // entire group gone
        }
    }
    let _ = kill(group, Signal::SIGKILL);
}

/// Is the serve behind this handle still live? (dispatches on the stored handle,
/// not the env's current backend).
fn serve_handle_alive(handle: &ServeHandle) -> bool {
    match handle {
        ServeHandle::Native { pid, .. } => native_serve_alive(*pid),
        ServeHandle::Container { engine, name } => container::container_exists(*engine, name),
    }
}

/// Tear down a running serve by its stored launch handle (backend-independent).
fn stop_by_handle(handle: &ServeHandle, verbose: bool) -> Result<()> {
    match handle {
        ServeHandle::Native { pgid, .. } => {
            kill_native_group(*pgid);
            Ok(())
        }
        ServeHandle::Container { engine, name } => {
            serve::stop_serve_container(*engine, verbose, name)
        }
    }
}

/// Native serves currently running: scanned from each env's serve record with a
/// live `Native` handle. The native half of `status`/`ls-running` (the container
/// half comes from `docker ps`); kept a free function so one query covers all envs.
fn native_running_serves() -> Vec<serve::ServeContainerInfo> {
    let mut out = Vec::new();
    for scope in [Scope::Local, Scope::System] {
        for env in cfg::list_env_names(scope) {
            let Some(rt) = cfg::read_serve_runtime(scope, &env) else { continue };
            let Some(ServeHandle::Native { pid, .. }) = &rt.handle else { continue };
            if !native_serve_alive(*pid) {
                continue;
            }
            out.push(serve::ServeContainerInfo {
                name: format!("native:{env}"),
                env: env.clone(),
                ports: rt.port.to_string(),
                status: format!("running (native, pid {pid})"),
                mode: rt.mode(),
                modules: rt.modules_summary(),
                url: if rt.token_required {
                    format!("{} (token)", rt.url())
                } else {
                    rt.url()
                },
            });
        }
    }
    out
}

/// Resolve WHAT to serve: a `--mcp <program>` one-off, or the environment's
/// exposed set (expose.yaml). Ensures each program is installed. Shared by the
/// container and native start paths.
fn resolve_serve_spec(scope: Scope, name: &str, mcp: &Option<String>) -> Result<ServeSpec> {
    if let Some(program) = mcp {
        ensure_program_installed(scope, name, program)?;
        Ok(ServeSpec { mcp: vec![program.clone()], api: Vec::new(), eval_allow: None })
    } else {
        let ex = cfg::read_exposure(scope, name)?;
        if ex.is_empty() {
            return Err(ManagerError::EnvError(format!(
                "Nothing is exposed in '{name}'. Expose a module first:\n    \
                 morloc-manager expose add <module> --as mcp\n  \
                 (or 'start --mcp <module>' for a one-off)."
            )));
        }
        for m in ex.exposed_modules() {
            ensure_program_installed(scope, name, &m)?;
        }
        let eval_allow = ex.eval.as_ref().map(|e| e.allow.join(","));
        Ok(ServeSpec { mcp: ex.mcp.clone(), api: ex.api.clone(), eval_allow })
    }
}

/// Spawn `morloc-nexus router ...` as a detached host process group serving the
/// native environment, logging to `<env>/logs/serve.log`. Returns the launch
/// handle and the host used in URLs.
fn native_serve(
    scope: Scope,
    env_name: &str,
    spec: &ServeSpec,
    host_port: u16,
    user_env: &[(String, String)],
    expose: bool,
    allow_plaintext: bool,
    allow_no_auth: bool,
    token: Option<String>,
) -> Result<(ServeHandle, String)> {
    // Exposing an UNSANDBOXED host process off-loopback reuses the container
    // path's gates (plaintext ack + a token) so it can't happen by accident.
    if expose {
        if !allow_plaintext {
            return Err(ManagerError::EnvError(format!(
                "Refusing to expose off-loopback over plaintext HTTP without --allow-plaintext.\n  \
                 Prefer the default (loopback) and reach it over an SSH tunnel:\n    \
                 ssh -N -L {host_port}:127.0.0.1:{host_port} <host>"
            )));
        }
        if token.is_none() && !allow_no_auth {
            return Err(ManagerError::EnvError(
                "An exposed endpoint requires a token: set MORLOC_MCP_TOKEN or --auth-token \
                 (or --allow-no-auth to serve it unauthenticated)."
                    .to_string(),
            ));
        }
    }
    let http_host = if expose { "0.0.0.0" } else { "127.0.0.1" };
    let need_allow_no_auth = http_host == "0.0.0.0" && token.is_none();

    let data_dir = cfg::env_data_dir(scope, env_name);
    let mh = data_dir.to_string_lossy().to_string();
    let command = build_router_command(&mh, host_port, http_host, spec, need_allow_no_auth);

    // The stored activation env-map puts the env's bin (morloc-nexus) + conda
    // toolchain on PATH; its absence means the env was never materialized.
    let runtime = cfg::read_native_runtime(scope, env_name).map_err(|_| {
        ManagerError::EnvError(format!(
            "native environment '{env_name}' is not materialized; \
             run 'morloc-manager update {env_name}' first"
        ))
    })?;

    // Persistent logfile so `logs` can read it after the manager exits.
    let logs_dir = data_dir.join("logs");
    std::fs::create_dir_all(&logs_dir)
        .map_err(|e| ManagerError::EnvError(format!("cannot create {}: {e}", logs_dir.display())))?;
    let log_path = logs_dir.join("serve.log");
    let log = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(&log_path)
        .map_err(|e| ManagerError::EnvError(format!("cannot open {}: {e}", log_path.display())))?;
    let log_err = log
        .try_clone()
        .map_err(|e| ManagerError::EnvError(format!("duplicate log fd: {e}")))?;

    // Resolve the nexus binary explicitly (env bin), else rely on the activation PATH.
    let nexus = data_dir.join("bin").join("morloc-nexus");
    let program: std::path::PathBuf = if nexus.is_file() {
        nexus
    } else {
        command[0].clone().into()
    };

    let mut cmd = Command::new(&program);
    cmd.args(&command[1..]);
    for (k, v) in &runtime.activation_env {
        cmd.env(k, v);
    }
    cmd.env("MORLOC_HOME", &mh);
    if let Some(t) = &token {
        cmd.env("MORLOC_MCP_TOKEN", t);
    }
    for (k, v) in user_env {
        cmd.env(k, v);
    }
    cmd.stdin(Stdio::null())
        .stdout(Stdio::from(log))
        .stderr(Stdio::from(log_err));
    // Detach into a new session + process group: survives the terminal, and lets
    // stop kill the whole group (nexus + pool daemons). setsid is async-signal-safe.
    unsafe {
        cmd.pre_exec(|| {
            nix::unistd::setsid()
                .map(|_| ())
                .map_err(|e| std::io::Error::from_raw_os_error(e as i32))
        });
    }
    let mut child = cmd
        .spawn()
        .map_err(|e| ManagerError::EnvError(format!("failed to launch morloc-nexus: {e}")))?;
    let pid = child.id();

    // A moment to catch an immediate failure (e.g. port already in use).
    std::thread::sleep(std::time::Duration::from_millis(700));
    if let Ok(Some(status)) = child.try_wait() {
        let logs = std::fs::read_to_string(&log_path).unwrap_or_default();
        let tail: Vec<&str> = logs.lines().rev().take(20).collect();
        let tail: String = tail.into_iter().rev().collect::<Vec<_>>().join("\n");
        return Err(ManagerError::EnvError(format!(
            "native serve exited immediately ({status}). Last log lines:\n{tail}"
        )));
    }
    // setsid made the child its own session + group leader, so pgid == pid.
    let url_host = if expose {
        serve::system_hostname()
    } else {
        "127.0.0.1".to_string()
    };
    Ok((ServeHandle::Native { pid, pgid: pid }, url_host))
}

/// Native `start`: serve the environment as a detached host nexus process.
#[allow(clippy::too_many_arguments)]
/// Native-backend serve launch (behind `NativeRunner::serve`). Spawns the
/// detached host nexus + prints native-specific status; the shared orchestration
/// (spec/port/token/record/MCP-config) runs in the `start` handler.
pub(crate) fn native_serve_launch(
    env: &runner::ResolvedEnv,
    req: &ServeRequest,
) -> Result<ServeOutcome> {
    let (handle, url_host) = native_serve(
        env.scope, &env.name, &req.spec, req.host_port, &req.user_env,
        req.expose, req.allow_plaintext, req.allow_no_auth, req.token.clone(),
    )?;
    if req.expose {
        eprintln!(
            "Serving '{}' on {url_host}:{} (EXPOSED, plaintext; native host process).",
            env.name, req.host_port
        );
    } else {
        eprintln!("Serving '{}' on 127.0.0.1:{} (native, host-local).", env.name, req.host_port);
    }
    eprintln!("  Logs:   morloc-manager logs");
    eprintln!("  Stop:   morloc-manager stop");
    eprintln!("  Note:   native serve is unsupervised (no restart; does not survive reboot).");
    Ok(ServeOutcome { handle, url_host, token: req.token.clone() })
}

/// Container-backend serve launch (behind `ContainerRunner::serve`). Builds the
/// serve plan (security tiers + netns), launches the container, and prints
/// container-specific status; shared orchestration runs in the `start` handler.
pub(crate) fn container_serve(
    env: &runner::ResolvedEnv,
    req: &ServeRequest,
) -> Result<ServeOutcome> {
    let ec = &env.ec;
    let env_name = &env.name;
    let engine = ec.engine()?;
    let image = ec.active_image().to_string();
    let data_dir = cfg::env_data_dir(env.scope, env_name);
    let container_name = serve::serve_container_name(env_name);
    if ec.dockerfile.is_some() && ec.built_image.is_none() {
        eprintln!("Warning: Dockerfile is configured but image has not been built. Using base image.");
        eprintln!("  Run 'morloc-manager update {env_name}' to build the Dockerfile layer.");
    }

    let plan = serve_plan(
        engine, &req.spec, req.container_port, req.host_port,
        req.expose, req.allow_plaintext, req.allow_no_auth, req.unsafe_serve,
        cfg!(target_os = "linux"), req.token.clone(),
    )?;
    let mut user_env = req.user_env.clone();
    let mut mcp_token: Option<String> = None;
    if let Some(t) = plan.token {
        mcp_token = Some(t.clone());
        user_env.push(("MORLOC_MCP_TOKEN".to_string(), t));
    }
    let mut extra_flags = cfg::read_flag_config(env.scope, env_name)?.materialize(Phase::Start, engine);
    extra_flags.extend(req.engine_args.iter().cloned());

    serve::serve_environment(
        engine, req.verbose, &image, &data_dir.to_string_lossy(), &container_name,
        &[(req.host_port, req.container_port)], plan.publish_host.as_deref(), plan.network.as_deref(),
        &extra_flags, &Some(ec.shm_size.clone()), &user_env, &plan.command,
    )?;

    let url_host = if req.expose {
        eprintln!("Serving '{env_name}' on 0.0.0.0:{} (EXPOSED, plaintext).", req.host_port);
        eprintln!("  A bearer token over plaintext stops scanners, not eavesdroppers.");
        eprintln!("  Do not commit the printed token into a project-scoped .mcp.json.");
        serve::system_hostname()
    } else if plan.unsafe_unconfined {
        eprintln!("DANGER: serving '{env_name}' on 127.0.0.1:{} UNAUTHENTICATED (--unsafe).", req.host_port);
        eprintln!("  Reachable by any container co-resident on the engine's network. Trusted hosts only.");
        "127.0.0.1".to_string()
    } else if mcp_token.is_some() {
        eprintln!("Serving '{env_name}' on 127.0.0.1:{} (loopback; token required on this engine).", req.host_port);
        "127.0.0.1".to_string()
    } else {
        eprintln!("Serving '{env_name}' on 127.0.0.1:{} (host-local; no token needed).", req.host_port);
        "127.0.0.1".to_string()
    };

    Ok(ServeOutcome {
        handle: ServeHandle::Container { engine, name: container_name },
        url_host,
        token: mcp_token,
    })
}

/// Print (and optionally follow) a native serve's logfile via `tail`.
fn tail_file(path: &std::path::Path, follow: bool) -> Result<()> {
    if !path.exists() {
        return Err(ManagerError::EnvError(format!(
            "no serve log at {} (is it serving?)",
            path.display()
        )));
    }
    let mut args = vec!["-n".to_string(), "200".to_string()];
    if follow {
        args.push("-f".to_string());
    }
    args.push(path.to_string_lossy().to_string());
    let status = Command::new("tail")
        .args(&args)
        .status()
        .map_err(|e| ManagerError::EnvError(format!("could not run tail: {e}")))?;
    if !status.success() {
        return Err(ManagerError::EnvError("tail failed".to_string()));
    }
    Ok(())
}

/// Validate the active env can host a SLURM bridge and spawn one. The
/// returned handle owns the listener thread + socket; dropping it
/// shuts the bridge down and unlinks the socket.
///
/// The bridge does NOT directly invoke `apptainer exec` on the compute
/// node; the wrap command is `morloc-manager run --slurm-bridge --
/// <nexus> --call-packet ...` so each compute-node nexus comes up
/// under the same env machinery the driver uses (and can recursively
/// dispatch further remote calls). The only thing the bridge needs to
/// carry is the absolute path to the morloc-manager binary itself,
/// which is the path of the currently-running process (we are
/// morloc-manager).
fn setup_slurm_bridge(ec: &EnvironmentConfig) -> Result<bridge::BridgeHandle> {
    // Apptainer is the recommended engine for HPC -- its `.sif` is a
    // single shared-FS file and `$HOME` auto-mount makes path mirroring
    // automatic. Other engines work as long as the user can get the
    // image onto every compute node (registry pull or pre-populated
    // store); warn but don't reject so users on Podman/Docker clusters
    // aren't forced to recreate envs.
    if ec.engine()? != ContainerEngine::Apptainer {
        eprintln!(
            "warning: SLURM bridge with engine {:?}: ensure the env's image is \
             reachable from every compute node (registry or pre-populated \
             store). Apptainer is the recommended engine on HPC clusters.",
            ec.engine()?,
        );
    }

    // Sanity: confirm the env was actually built so the compute-node
    // `morloc-manager run` won't fail at image lookup. Equivalent to
    // `active_image()` resolving to a real path/tag.
    let image = ec.active_image();
    if image.is_empty() {
        return Err(ManagerError::EnvError(
            "Environment has no resolvable image. Run `morloc-manager update` first.".into(),
        ));
    }

    // The wrap command on the compute node is
    //   <this morloc-manager> run --slurm-bridge -- <nexus> --call-packet ...
    // For path-mirroring to work, this binary must be at the same
    // absolute path on every compute node. The typical setup is
    // `~/.local/bin/morloc-manager` on NFS-mounted $HOME; users with a
    // non-mirrored layout need to install morloc-manager at a path
    // visible to every node.
    let morloc_manager_exe = std::env::current_exe().map_err(|e| {
        ManagerError::EnvError(format!("resolve morloc-manager path: {e}"))
    })?;

    let sock_path = bridge_socket_path();
    bridge::spawn_bridge(
        &sock_path,
        bridge::BridgeConfig { morloc_manager_exe },
    )
    .map_err(|e| ManagerError::EnvError(format!("spawn slurm bridge: {e}")))
}

/// Pick a per-process socket path under `$XDG_RUNTIME_DIR` (or `/tmp`
/// fallback). The pid suffix avoids collisions across concurrent
/// `morloc-manager run --slurm-bridge` invocations.
fn bridge_socket_path() -> std::path::PathBuf {
    let dir = std::env::var("XDG_RUNTIME_DIR")
        .ok()
        .filter(|s| !s.is_empty())
        .unwrap_or_else(|| "/tmp".to_string());
    std::path::PathBuf::from(dir).join(format!("morloc-bridge-{}.sock", std::process::id()))
}

/// Container-backend `run`: execute a command inside the environment's image.
/// Invoked through the `Runner` seam (`ContainerRunner`); the target is already
/// resolved by `runner::run_in_env`.
pub(crate) fn container_run_env(
    env: &runner::ResolvedEnv,
    req: &runner::RunRequest,
) -> Result<()> {
    let env_name = env.name.clone();
    let env_scope = env.scope;
    let ec = &env.ec;
    let verbose = req.verbose;
    let shell = req.shell;
    let args: &[String] = &req.args;
    let user_env: &[(String, String)] = &req.user_env;
    let cli_engine_args: &[String] = &req.engine_args;
    let phase = req.phase;
    let slurm_bridge = req.slurm_bridge;
    let engine = ec.engine()?;
    let image = ec.active_image().to_string();
    let data_dir = cfg::env_data_dir(env_scope, &env_name);
    let v_data_dir = data_dir.to_string_lossy().to_string();

    // Optional SLURM submission bridge. The handle's drop tears the
    // listener down and unlinks the UDS at function exit (which only
    // happens after the container has exited synchronously, so the
    // bridge stays alive for the lifetime of every nested remote
    // call).
    let _bridge_guard = if slurm_bridge {
        Some(setup_slurm_bridge(ec)?)
    } else {
        None
    };
    let bridge_mount = _bridge_guard.as_ref().map(|h| h.sock_path().to_path_buf());

    // Warn if a Dockerfile is configured but the layered image hasn't been built
    if ec.dockerfile.is_some() && ec.built_image.is_none() {
        eprintln!("Warning: Dockerfile is configured but image has not been built. Using base image.");
        eprintln!("  Run 'morloc-manager update {env_name}' to build the Dockerfile layer.");
    }

    // Fail fast with a clear message if docker socket is unreachable
    require_docker_socket(engine)?;

    // Verify the image is accessible before attempting to run
    if !container::image_exists_locally(engine, &image) {
        // Show the raw container engine error before our hint
        if let Some(raw_err) = container::image_inspect_stderr(engine, &image) {
            let trimmed = raw_err.trim();
            if !trimmed.is_empty() {
                eprintln!("{trimmed}");
            }
        }
        if env_scope == Scope::System && !check_podman_additional_stores(engine) {
            return Err(ManagerError::EnvError(format!(
                "Image '{image}' not found. The environment '{env_name}' is a system environment \
                 but Podman is not configured to see rootful images.\n\
                 Option 1 (recommended): Use Docker for system environments.\n\
                 Option 2: Add to [storage.options] in /etc/containers/storage.conf:\n\n  \
                 additionalimagestores = [\"/var/lib/containers/storage\"]\n\n\
                 Note: Option 2 may cause storage locking conflicts on Fedora and Debian.\n"
            )));
        }
        let hint = if env_scope == Scope::System {
            format!("Ask your administrator to run: sudo morloc-manager update {env_name}")
        } else {
            format!("Run 'morloc-manager update {env_name}' to build it.")
        };
        return Err(ManagerError::EnvError(format!(
            "Image '{image}' not found locally. {hint}"
        )));
    }

    let se_mode = detect_selinux();
    let suffix = volume_suffix(se_mode);
    let home = dirs::home_dir()
        .unwrap_or_default()
        .to_string_lossy()
        .to_string();
    let cwd = std::env::current_dir()
        .unwrap_or_default()
        .to_string_lossy()
        .to_string();

    // Refuse to run from the root directory — container engines cannot
    // bind-mount "/" and the resulting error is opaque.
    if !shell && cwd == "/" {
        return Err(ManagerError::EnvError(
            "Cannot run from the root directory (/). \
             Change to a subdirectory first (e.g., cd /tmp).".to_string()
        ));
    }

    // Materialize flags from env.flags.yaml for the active phase + engine,
    // then append CLI one-shot overrides. The flag-file errors out on the
    // legacy flat env.flags format with a migration hint.
    let mut extra_flags = cfg::read_flag_config(env_scope, &env_name)?
        .materialize(phase, engine);
    extra_flags.extend(cli_engine_args.iter().cloned());

    let is_init = matches!(args, [a, b, ..] if a == "morloc" && b == "init");
    let is_home_dir = normalize_trailing(&cwd) == normalize_trailing(&home);

    if !is_init && !suffix.is_empty() && !is_home_dir {
        selinux::validate_mount_path(&cwd)?;
        run_with_config(
            engine, verbose, &image, &v_data_dir, &home, &cwd, suffix,
            shell, args, false, &ec.shm_size, &extra_flags, user_env,
            bridge_mount.as_deref(),
        )
    } else {
        let (cwd_final, skip_work_mount) = if is_home_dir && !suffix.is_empty() && !is_init {
            eprintln!("Warning: running from home directory with SELinux; working directory mount skipped.");
            eprintln!("Workaround: create a project subdirectory and work from there:");
            eprintln!("  mkdir ~/myproject && cd ~/myproject");
            (home.clone(), true)
        } else {
            (cwd, false)
        };
        run_with_config(
            engine, verbose, &image, &v_data_dir, &home, &cwd_final, suffix,
            shell, args, is_init || skip_work_mount, &ec.shm_size, &extra_flags, user_env,
            bridge_mount.as_deref(),
        )
    }
}

fn run_with_config(
    engine: ContainerEngine,
    verbose: bool,
    image: &str,
    v_data_dir: &str,
    home: &str,
    cwd: &str,
    suffix: &str,
    shell: bool,
    args: &[String],
    is_init: bool,
    shm_size: &str,
    extra_flags: &[String],
    user_env: &[(String, String)],
    bridge_socket: Option<&std::path::Path>,
) -> Result<()> {
    if shell {
        if !io::stdin().is_terminal() || !io::stdout().is_terminal() {
            eprintln!("Error: --shell requires an interactive terminal (TTY).");
            eprintln!("If connecting over SSH, use: ssh -t <host> morloc-manager run --shell");
            std::process::exit(1);
        }
    }

    // Pliable container: the pixi env (/env) and the morloc runtime shims
    // (MORLOC_HOME) are host-mounted MUTABLE dirs -- materialized at env setup
    // into `<env_dir>/pixi` and `<env_dir>/runtime` -- so an in-container
    // `morloc make` can install package deps in place. Mutable state
    // (exe/fdb/modules) is the third mount at MORLOC_STATE. All three are
    // host-owned, hence writable under the keep-id-mapped host UID (no chmod).
    // A pliable container env must be MATERIALIZED before it can run: `/env` (the
    // conda toolchain) and MORLOC_HOME (the morloc runtime shims) are host-mounted
    // from `<env>/pixi` and `<env>/runtime`, populated by materialize at env setup.
    // If a mount source is absent (an env created with --no-init, or a materialize
    // that never completed), mounting it would shadow the image with an empty dir
    // and the runtime would break with a cryptic error; fail early instead. `/env`
    // is an INPUT to every process (including a manual `morloc init`), so it is
    // always required; MORLOC_HOME is the OUTPUT init writes, so it may be empty
    // during an is_init run.
    let pixi_src = std::path::Path::new(v_data_dir).join("pixi");
    let runtime_src = std::path::Path::new(v_data_dir).join("runtime");
    let mut required: Vec<(&std::path::Path, &str)> =
        vec![(pixi_src.as_path(), "conda toolchain (/env)")];
    if !is_init {
        required.push((runtime_src.as_path(), "morloc runtime (MORLOC_HOME)"));
    }
    for (src, what) in required {
        let populated = std::fs::read_dir(src).map(|mut d| d.next().is_some()).unwrap_or(false);
        if !populated {
            return Err(ManagerError::EnvError(format!(
                "environment at '{v_data_dir}' is not materialized: its {what} is missing \
                 at '{}'. Provision it first with 'morloc-manager update <env>', or recreate \
                 the environment without --no-init.",
                src.display()
            )));
        }
    }

    let mh = serve::CONTAINER_MORLOC_HOME;
    let base_mounts = vec![
        (v_data_dir.to_string(), serve::CONTAINER_MORLOC_STATE.to_string()),
        (format!("{v_data_dir}/pixi"), serve::CONTAINER_PIXI_DIR.to_string()),
        (format!("{v_data_dir}/runtime"), mh.to_string()),
    ];
    let work_mount = if is_init {
        Vec::new()
    } else {
        vec![(cwd.to_string(), cwd.to_string())]
    };
    // Bridge socket bind-mount goes into a fixed in-container path so
    // libmorloc.so finds it via MORLOC_BRIDGE_SOCKET (set below).
    let bridge_mount: Vec<(String, String)> = match bridge_socket {
        Some(host) => vec![(
            host.to_string_lossy().to_string(),
            BRIDGE_SOCK_IN_CONTAINER.to_string(),
        )],
        None => Vec::new(),
    };
    let all_mounts: Vec<(String, String)> = base_mounts
        .into_iter()
        .chain(work_mount)
        .chain(bridge_mount)
        .collect();
    let work_dir = if is_init {
        mh.to_string()
    } else {
        cwd.to_string()
    };
    // The container runs as the invoking host UID (engine_specific_run_flags_io:
    // docker `--user`, podman `--userns=keep-id`), so the in-container
    // environment must not assume root or a real, writable $HOME. The two
    // engine families differ in one decisive way: Apptainer mounts the host
    // $HOME by default, docker/podman do NOT.
    //
    // Apptainer: the host $HOME is present and writable, so `morloc init` can
    // symlink the freshly installed nexus/manager binaries into a morloc-owned
    // subdir of it (`~/.local/share/morloc/bin/`) -- morloc-owned space that
    // avoids clobbering the user's general-purpose `~/.local/bin/` -- and we add
    // that dir to PATH so the symlinks are findable.
    //
    // Docker/Podman: the host $HOME is NOT mounted, so anything HOME-relative
    // (cargo's default CARGO_HOME, `morloc init`'s bin-link mkdir) would target
    // an unwritable path and fail. Point HOME and CARGO_HOME at the mounted,
    // writable, host-owned data dir ($MORLOC_HOME) and skip the convenience
    // bin-link entirely (`MORLOC_BIN_LINK_DIR=""`); the binaries are already on
    // PATH via `$MORLOC_HOME/bin`. RUSTUP_HOME is intentionally NOT set here so
    // the image's read-only toolchain ENV (/opt/rust/rustup) stays authoritative.
    //
    // Either way `user_env` is appended last, so `-x --env ...` overrides win.
    // Under docker/podman HOME is $MORLOC_HOME/home (the host $HOME is not
    // mounted). Create it on the host bind-mount side so pool daemons/tools that
    // touch $HOME do not hit ENOENT -- the env-create loop does not make it, and
    // existing environments predate it. Best-effort: a failure here means the
    // data dir is unwritable, which fails loudly downstream.
    if matches!(engine, ContainerEngine::Docker | ContainerEngine::Podman) {
        let _ = std::fs::create_dir_all(format!("{v_data_dir}/home"));
    }
    let mut env_vars = match engine {
        ContainerEngine::Apptainer => {
            let link_dir = format!("{home}/{MORLOC_BIN_LINK_REL}");
            vec![
                ("HOME".to_string(), home.to_string()),
                ("MORLOC_HOME".to_string(), mh.to_string()),
                (
                    "MORLOC_STATE".to_string(),
                    serve::CONTAINER_MORLOC_STATE.to_string(),
                ),
                ("MORLOC_BIN_LINK_DIR".to_string(), link_dir.clone()),
                (
                    "PATH".to_string(),
                    format!("{link_dir}:{}", serve::container_path(mh)),
                ),
            ]
        }
        ContainerEngine::Docker | ContainerEngine::Podman => {
            // Shared docker/podman base (HOME/MORLOC_HOME/PATH); add the
            // build-phase-only vars this call site owns.
            let mut v = serve::oci_base_env(mh);
            // Explicit skip: SystemConfig.hs treats "" as "do not link".
            v.push(("MORLOC_BIN_LINK_DIR".to_string(), String::new()));
            // Writable, mounted, persisted cargo cache for Rust pool builds
            // (under the mounted state root, not the baked runtime).
            v.push((
                "CARGO_HOME".to_string(),
                format!("{}/.cargo", serve::CONTAINER_MORLOC_STATE),
            ));
            // Managed-env marker + pixi location, so an in-container `morloc make`
            // provisions its package.yaml deps via `morloc-env` (the compiler's
            // callback gates on MORLOC_ENV). The container's pixi env is baked at
            // /env, distinct from the state root, so it is passed explicitly; the
            // store itself lives under the mounted MORLOC_STATE.
            v.push(("MORLOC_ENV".to_string(), "container".to_string()));
            v.push(("MORLOC_PIXI".to_string(), serve::CONTAINER_PIXI_BIN.to_string()));
            v.push((
                "MORLOC_PIXI_DIR".to_string(),
                serve::CONTAINER_PIXI_DIR.to_string(),
            ));
            v
        }
    };
    if bridge_socket.is_some() {
        env_vars.push((
            "MORLOC_BRIDGE_SOCKET".to_string(),
            BRIDGE_SOCK_IN_CONTAINER.to_string(),
        ));
    }
    env_vars.extend(user_env.iter().cloned());
    let cmd = if shell {
        Some(vec!["/bin/bash".to_string()])
    } else if args.is_empty() {
        None
    } else {
        Some(args.to_vec())
    };

    let cfg = RunConfig {
        image: image.to_string(),
        bind_mounts: all_mounts,
        env: env_vars,
        interactive: shell,
        shm_size: Some(shm_size.to_string()),
        work_dir: Some(work_dir),
        selinux_suffix: suffix.to_string(),
        command: cmd,
        extra_flags: extra_flags.to_vec(),
        ..RunConfig::new(image)
    };

    let status = container_run_passthrough(engine, verbose, shell, &cfg);
    let code = status.code().unwrap_or(1);
    if status.success() {
        Ok(())
    } else if code >= 125 {
        // Exit 125+ = container engine error (not the user's program)
        Err(ManagerError::EngineError {
            engine,
            code,
            stderr: "Container engine error".to_string(),
        })
    } else {
        // Exit 1-124 = program exited with non-zero, pass through silently
        std::process::exit(code);
    }
}

/// Confirm an MCP-servable program is installed in the environment. `morloc
/// make -o <program> --install` installs the CLI launcher at `bin/<program>`
/// (the `-o` name); the launcher's exec line carries the real manifest path,
/// so the nexus resolves it. (The build dir under exe/ is keyed on the source
/// basename, not the program name, so it is not a reliable lookup key.)
fn ensure_program_installed(env_scope: Scope, env_name: &str, program: &str) -> Result<()> {
    let host_launcher = cfg::env_data_dir(env_scope, env_name)
        .join("bin")
        .join(program);
    if !host_launcher.exists() {
        return Err(ManagerError::EnvError(format!(
            "Program '{program}' is not installed in environment '{env_name}' \
             (looked for bin/{program}).\n  Install it with: \
             morloc-manager install <file>.loc  (the program is named after its \
             module, so serve it as '{program}' only if that is the module name)."
        )));
    }
    Ok(())
}

/// Resolve the MCP bearer token: an explicit `--auth-token` wins, else the
/// `MORLOC_MCP_TOKEN` environment variable (which keeps the token off argv).
fn resolve_mcp_token(explicit: Option<String>) -> Option<String> {
    explicit.or_else(|| std::env::var("MORLOC_MCP_TOKEN").ok().filter(|s| !s.is_empty()))
}

/// Pick a free host port for a serve: try `preferred`, then scan up to `range`
/// ports above it, returning the first that binds on 127.0.0.1. Falls back to
/// `preferred` if none is free (the engine then reports the real conflict). This
/// is a best-effort predictor -- both host-networking (nexus binds the host
/// port) and bridge publishing (`-p`) contend for the same host port.
fn find_free_host_port(preferred: u16, range: u16) -> u16 {
    for p in preferred..=preferred.saturating_add(range) {
        if std::net::TcpListener::bind(("127.0.0.1", p)).is_ok() {
            return p;
        }
    }
    preferred
}

/// A resolved plan for serving one program as MCP over HTTP: the container
/// command, the host publish binding (docker `-p <ip>:H:C`; `None` for
/// apptainer, which has no `-p`), and the token to inject (if any).
struct ServePlan {
    command: Vec<String>,
    /// `Some("host")` => run in the host netns (docker/podman `--network host`)
    /// so a loopback bind lands on the host's loopback. `None` => engine default.
    network: Option<String>,
    publish_host: Option<String>,
    token: Option<String>,
    /// True when serving unauthenticated on an endpoint that is NOT confined to
    /// the host's loopback (the `--unsafe` fallback), so the caller warns loudly.
    unsafe_unconfined: bool,
}

/// What to serve: per-adapter module membership + the eval capability.
pub(crate) struct ServeSpec {
    mcp: Vec<String>,
    api: Vec<String>,
    /// `Some(csv)` enables the sandboxed eval capability with this allow-list.
    eval_allow: Option<String>,
}

/// Everything a backend needs to launch a serve, after the neutral orchestration
/// (spec resolution, port pick, env, token) has run in the `start` handler. The
/// backend impl differs only in how it launches + tracks the process.
pub(crate) struct ServeRequest {
    pub spec: ServeSpec,
    pub host_port: u16,
    pub container_port: u16,
    pub user_env: Vec<(String, String)>,
    pub expose: bool,
    pub allow_plaintext: bool,
    pub allow_no_auth: bool,
    pub unsafe_serve: bool,
    pub engine_args: Vec<String>,
    pub token: Option<String>,
    pub verbose: bool,
}

/// What a backend's launch produced: the tracking handle, the host used in URLs,
/// and the effective token (a backend may add one, e.g. the container VM-fallback).
pub(crate) struct ServeOutcome {
    pub handle: ServeHandle,
    pub url_host: String,
    pub token: Option<String>,
}

/// Decide how to serve the exposure `spec` (MCP + API adapters) over HTTP via the
/// front-end, enforcing the security tiers.
///
/// The loopback default must be reachable only by host processes. How that is
/// achieved is engine- and platform-specific:
///
/// * apptainer already runs in the host netns, so binding `--http-host
///   127.0.0.1` IS the host loopback (no `-p`).
/// * docker/podman on Linux share the host kernel, so `--network host` +
///   `--http-host 127.0.0.1` binds the host's loopback directly -- sibling
///   containers have their own loopback and cannot reach it.
/// * docker/podman on a VM-backed engine (Docker Desktop / podman machine, i.e.
///   a non-Linux manager) cannot bind the host loopback through the VM. There we
///   fall back to a bridge published on `127.0.0.1` and REQUIRE a token, because
///   co-resident VM containers can reach the bridge IP.
///
/// `--allow-no-auth` is added exactly when the nexus binds a non-loopback
/// address with no token (an exposed endpoint the operator explicitly waived).
fn serve_plan(
    engine: ContainerEngine,
    spec: &ServeSpec,
    container_port: u16,
    host_port: u16,
    expose: bool,
    allow_plaintext: bool,
    allow_no_auth: bool,
    unsafe_serve: bool,
    // Whether docker/podman can bind the host's loopback via the shared netns
    // (true on a Linux manager; false on a VM-backed engine). The production
    // caller passes `cfg!(target_os = "linux")`; kept a parameter so both the
    // native and VM-backed paths are unit-testable on any CI.
    host_net_usable: bool,
    token: Option<String>,
) -> Result<ServePlan> {
    if expose {
        if !allow_plaintext {
            return Err(ManagerError::EnvError(format!(
                "Refusing to expose MCP off-loopback over plaintext HTTP without \
                 --allow-plaintext.\n  A bearer token over plaintext protects against \
                 scanners, not eavesdroppers. Prefer the default (loopback) and reach \
                 it over an SSH tunnel:\n    ssh -N -L {host_port}:127.0.0.1:{host_port} <host>"
            )));
        }
        if token.is_none() && !allow_no_auth {
            return Err(ManagerError::EnvError(
                "An exposed MCP endpoint requires a token: set MORLOC_MCP_TOKEN or \
                 --auth-token (or --allow-no-auth to serve it unauthenticated, \
                 strongly discouraged)."
                    .to_string(),
            ));
        }
    }

    let is_apptainer = matches!(engine, ContainerEngine::Apptainer);

    // (http_host bound by the nexus, publish_host for `-p`, network mode,
    //  whether the endpoint is served unauthenticated but NOT host-confined).
    let (http_host, publish_host, network, unsafe_unconfined) = if expose {
        let pub_ip = if is_apptainer { None } else { Some("0.0.0.0".to_string()) };
        ("0.0.0.0".to_string(), pub_ip, None, false)
    } else if is_apptainer {
        ("127.0.0.1".to_string(), None, None, false)
    } else if host_net_usable {
        // Bind the host's loopback directly via the shared netns.
        ("127.0.0.1".to_string(), None, Some("host".to_string()), false)
    } else {
        // Docker Desktop / podman machine fallback: a bridge on 127.0.0.1 still
        // exposes the bridge IP to co-resident VM containers, so require a token
        // unless the operator explicitly waives it with --unsafe.
        if token.is_none() && !unsafe_serve {
            return Err(ManagerError::EnvError(
                "On this container engine (Docker Desktop / podman machine) a loopback \
                 MCP server cannot bind the host's loopback directly, so it is published \
                 on a bridge that co-resident containers can reach.\n  Provide a token:\n    \
                 MORLOC_MCP_TOKEN=$(openssl rand -hex 16) morloc-manager start\n  \
                 (Or serve off-box with --expose, run on a Linux engine for tokenless loopback, \
                 or pass --unsafe to serve it unauthenticated anyway.)"
                    .to_string(),
            ));
        }
        // Unconfined-and-unauthenticated only when there is no token (the
        // --unsafe waiver); a token still confines access to holders of it.
        ("0.0.0.0".to_string(), Some("127.0.0.1".to_string()), None, token.is_none())
    };

    // In a shared host netns there is no port mapping; the nexus binds the port
    // the client reaches (host_port). On a bridge, it binds the in-container port
    // and `-p` maps host_port -> container_port.
    let host_netns = is_apptainer || network.as_deref() == Some("host");
    let bind_port = if host_netns { host_port } else { container_port };

    // The nexus refuses a non-loopback bind with no token unless --allow-no-auth.
    let need_allow_no_auth = http_host == "0.0.0.0" && token.is_none();
    // Container: programs are installed under the mounted MORLOC_STATE, not the
    // baked MORLOC_HOME (mh), so point the router's --fdb at the state root.
    let command = build_router_command(
        serve::CONTAINER_MORLOC_STATE,
        bind_port,
        &http_host,
        spec,
        need_allow_no_auth,
    );
    Ok(ServePlan { command, network, publish_host, token, unsafe_unconfined })
}

/// Build the `morloc-nexus router ...` argv shared by the container and native
/// serve paths. `state_dir` is the in-context MORLOC_STATE (its `exe/` holds the
/// installed programs); the nexus listens on `http_host:bind_port`.
/// `need_allow_no_auth` is set when a non-loopback bind has no token (the nexus
/// otherwise refuses it).
fn build_router_command(
    // The exe/fdb tree lives under MORLOC_STATE, not MORLOC_HOME. Native: state
    // == home == data_dir. Container: state is the mounted /opt/morloc-state,
    // NOT the baked /opt/morloc, so the router must scan the mounted dir or it
    // sees an empty program set.
    state_dir: &str,
    bind_port: u16,
    http_host: &str,
    spec: &ServeSpec,
    need_allow_no_auth: bool,
) -> Vec<String> {
    let mut command = vec![
        "morloc-nexus".to_string(),
        "router".to_string(),
        "--fdb".to_string(), format!("{state_dir}/exe"),
        "--http-port".to_string(), bind_port.to_string(),
        "--http-host".to_string(), http_host.to_string(),
    ];
    for m in &spec.mcp {
        command.push("--mcp".to_string());
        command.push(m.clone());
    }
    for m in &spec.api {
        command.push("--api".to_string());
        command.push(m.clone());
    }
    if let Some(allow) = &spec.eval_allow {
        command.push("--eval".to_string());
        command.push("--eval-allowed-modules".to_string());
        command.push(allow.clone());
    }
    if need_allow_no_auth {
        command.push("--allow-no-auth".to_string());
    }
    command
}

/// Print an environment's exposure set (modules with their protocols, and the
/// eval capability). Under --json, pure JSON on stdout.
fn print_exposure(env: &str, ex: &ExposureConfig, json: bool) {
    if json {
        let v = serde_json::json!({
            "environment": env,
            "mcp": ex.mcp,
            "api": ex.api,
            "eval": ex.eval.as_ref().map(|e| serde_json::json!({ "allow": e.allow })),
        });
        println!("{}", serde_json::to_string_pretty(&v).unwrap_or_default());
        return;
    }
    if ex.is_empty() {
        println!("Nothing exposed in '{env}'. Add with: morloc-manager expose add <module> --as mcp");
        return;
    }
    println!("Exposed in '{env}':");
    for m in ex.exposed_modules() {
        let protos: Vec<&str> = ex.protocols_of(&m).iter().map(|p| p.as_str()).collect();
        println!("  {m}  [{}]", protos.join(", "));
    }
    match &ex.eval {
        Some(e) if e.allow.is_empty() => println!("  eval  [enabled; empty allow-list]"),
        Some(e) => println!("  eval  [enabled; allow: {}]", e.allow.join(", ")),
        None => {}
    }
    println!("\nRun 'morloc-manager start' to serve this set.");
}

/// Print a client `mcpServers` config entry (HTTP transport) as PURE JSON on
/// stdout -- so `> file` / `| jq` capture clean config. A token, if present,
/// adds the `Authorization` header. Any human-facing text is the caller's
/// responsibility (stderr).
fn print_http_mcp_config(name: &str, url_host: &str, port: u16, token: Option<&str>) {
    let config = build_http_mcp_config(name, url_host, port, token);
    println!("{}", serde_json::to_string_pretty(&config).unwrap_or_default());
}

fn build_http_mcp_config(
    name: &str,
    url_host: &str,
    port: u16,
    token: Option<&str>,
) -> serde_json::Value {
    let mut entry = serde_json::json!({ "url": format!("http://{url_host}:{port}/mcp") });
    if let Some(t) = token {
        entry["headers"] = serde_json::json!({ "Authorization": format!("Bearer {t}") });
    }
    let mut servers = serde_json::Map::new();
    servers.insert(name.to_string(), entry);
    serde_json::json!({ "mcpServers": serde_json::Value::Object(servers) })
}


fn normalize_trailing(p: &str) -> String {
    let mut s = p.to_string();
    if !s.ends_with('/') {
        s.push('/');
    }
    s
}

// ======================================================================
// Tests
// ======================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::container::{build_build_args, build_run_args, engine_executable, engine_specific_run_flags, BuildConfig};

    // ---- MCP config emission ----

    #[test]
    fn http_mcp_config_loopback_has_no_headers() {
        let v = build_http_mcp_config("dna", "127.0.0.1", 9000, None);
        let e = &v["mcpServers"]["dna"];
        assert_eq!(e["url"], "http://127.0.0.1:9000/mcp");
        assert!(e.get("headers").is_none(), "loopback config must carry no token");
    }

    #[test]
    fn http_mcp_config_offbox_carries_bearer_header() {
        let v = build_http_mcp_config("dna", "host.example", 9000, Some("s3cret"));
        let e = &v["mcpServers"]["dna"];
        assert_eq!(e["url"], "http://host.example:9000/mcp");
        assert_eq!(e["headers"]["Authorization"], "Bearer s3cret");
    }

    #[allow(clippy::too_many_arguments)]
    fn plan(
        engine: ContainerEngine,
        expose: bool,
        plaintext: bool,
        noauth: bool,
        unsafe_serve: bool,
        host_net: bool,
        tok: Option<&str>,
    ) -> Result<ServePlan> {
        let spec = ServeSpec {
            mcp: vec!["dna".to_string()],
            api: Vec::new(),
            eval_allow: None,
        };
        serve_plan(
            engine, &spec, 9000, 9000,
            expose, plaintext, noauth, unsafe_serve, host_net, tok.map(str::to_string),
        )
    }

    #[test]
    fn mcp_plan_docker_loopback_linux_uses_host_net_no_token() {
        // Option A: on a Linux engine the loopback default shares the host netns
        // and binds the host's 127.0.0.1 directly -- no -p, no token, no
        // --allow-no-auth, and unreachable by sibling containers.
        let p = plan(ContainerEngine::Docker, false, false, false, false, true, None).unwrap();
        assert!(p.command.windows(2).any(|w| w == ["--http-host", "127.0.0.1"]));
        assert_eq!(p.network.as_deref(), Some("host"));
        assert!(p.publish_host.is_none());
        assert!(!p.command.iter().any(|a| a == "--allow-no-auth"));
        assert!(p.token.is_none());
        assert!(!p.unsafe_unconfined);
    }

    #[test]
    fn mcp_plan_apptainer_loopback_binds_loopback_without_allow_no_auth() {
        // On apptainer (no -p) a loopback default must bind 127.0.0.1 directly
        // and NOT pass --allow-no-auth (which would open an unauthenticated
        // 0.0.0.0 endpoint on the host). Its host netns is intrinsic, so no
        // --network flag is emitted.
        let p = plan(ContainerEngine::Apptainer, false, false, false, false, true, None).unwrap();
        assert!(p.command.windows(2).any(|w| w == ["--http-host", "127.0.0.1"]));
        assert!(p.network.is_none());
        assert!(p.publish_host.is_none());
        assert!(!p.command.iter().any(|a| a == "--allow-no-auth"));
    }

    #[test]
    fn mcp_plan_desktop_fallback_requires_token() {
        // VM-backed engine (host_net=false): loopback cannot be host-confined, so
        // no token is refused unless --unsafe.
        assert!(plan(ContainerEngine::Docker, false, false, false, false, false, None).is_err());
        // With a token it serves on a bridge published to loopback, token-guarded.
        let p = plan(ContainerEngine::Docker, false, false, false, false, false, Some("t")).unwrap();
        assert!(p.command.windows(2).any(|w| w == ["--http-host", "0.0.0.0"]));
        assert_eq!(p.publish_host.as_deref(), Some("127.0.0.1"));
        assert!(p.network.is_none());
        assert!(!p.command.iter().any(|a| a == "--allow-no-auth")); // token present
        assert_eq!(p.token.as_deref(), Some("t"));
        assert!(!p.unsafe_unconfined);
    }

    #[test]
    fn mcp_plan_desktop_fallback_unsafe_serves_unauthenticated() {
        // --unsafe waives the token on the VM-backed fallback: unauthenticated on
        // a bridge reachable by co-resident containers -> --allow-no-auth + the
        // unconfined warning flag.
        let p = plan(ContainerEngine::Docker, false, false, false, true, false, None).unwrap();
        assert!(p.command.windows(2).any(|w| w == ["--http-host", "0.0.0.0"]));
        assert_eq!(p.publish_host.as_deref(), Some("127.0.0.1"));
        assert!(p.command.iter().any(|a| a == "--allow-no-auth"));
        assert!(p.token.is_none());
        assert!(p.unsafe_unconfined);
    }

    #[test]
    fn mcp_plan_expose_requires_plaintext_and_token() {
        assert!(plan(ContainerEngine::Docker, true, false, false, false, true, Some("t")).is_err()); // no --allow-plaintext
        assert!(plan(ContainerEngine::Docker, true, true, false, false, true, None).is_err());       // no token, no --allow-no-auth
        let p = plan(ContainerEngine::Docker, true, true, false, false, true, Some("t")).unwrap();
        assert_eq!(p.publish_host.as_deref(), Some("0.0.0.0"));
        assert!(p.network.is_none());
        assert!(!p.command.iter().any(|a| a == "--allow-no-auth")); // token present
        assert_eq!(p.token.as_deref(), Some("t"));
    }

    #[test]
    fn serve_plan_builds_router_frontend_command() {
        let spec = ServeSpec {
            mcp: vec!["dna".to_string()],
            api: vec!["align".to_string()],
            eval_allow: Some("dna,stats".to_string()),
        };
        let p = serve_plan(
            ContainerEngine::Docker, &spec, 9000, 9000,
            false, false, false, false, true, None,
        ).unwrap();
        // The front-end is the `router` mode, not the single-program `mcp` mode.
        assert_eq!(p.command.first().map(String::as_str), Some("morloc-nexus"));
        assert_eq!(p.command.get(1).map(String::as_str), Some("router"));
        // Programs install under the mounted MORLOC_STATE, so the router scans
        // /opt/morloc-state/exe -- NOT the baked runtime /opt/morloc/exe.
        assert!(p.command.windows(2).any(|w| w == ["--fdb", "/opt/morloc-state/exe"]));
        assert!(p.command.windows(2).any(|w| w == ["--mcp", "dna"]));
        assert!(p.command.windows(2).any(|w| w == ["--api", "align"]));
        assert!(p.command.iter().any(|a| a == "--eval"));
        assert!(p.command.windows(2).any(|w| w == ["--eval-allowed-modules", "dna,stats"]));
        assert!(!p.command.iter().any(|a| a == "--all"));
    }

    #[test]
    fn find_free_host_port_skips_taken() {
        // Bind a port, then confirm the picker avoids it and returns a bindable one.
        let l = std::net::TcpListener::bind(("127.0.0.1", 0)).unwrap();
        let taken = l.local_addr().unwrap().port();
        let picked = find_free_host_port(taken, 50);
        assert_ne!(picked, taken, "should skip the bound port");
        assert!(std::net::TcpListener::bind(("127.0.0.1", picked)).is_ok());
    }

    // ---- Type tests ----

    #[test]
    fn show_version_formats_correctly() {
        assert_eq!(Version::new(0, 67, 0).show(), "0.67.0");
    }

    #[test]
    fn parse_version_round_trips() {
        assert_eq!("0.67.0".parse::<Version>().ok(), Some(Version::new(0, 67, 0)));
    }

    #[test]
    fn parse_version_rejects_invalid() {
        assert!("abc".parse::<Version>().is_err());
    }

    #[test]
    fn parse_version_rejects_incomplete() {
        assert!("0.67".parse::<Version>().is_err());
    }

    #[test]
    fn version_ordering_is_semantic() {
        assert!(Version::new(1, 0, 0) > Version::new(0, 99, 99));
    }

    #[test]
    fn version_ordering_minor() {
        assert!(Version::new(0, 2, 0) > Version::new(0, 1, 99));
    }

    #[test]
    fn version_equality() {
        assert_eq!(Version::new(0, 67, 0), Version::new(0, 67, 0));
    }

    #[test]
    fn parse_version_with_prerelease() {
        for (input, expected_pre) in [
            ("0.77.0-rc.1", "rc.1"),
            ("1.0.0-alpha", "alpha"),
            ("1.0.0-beta.2", "beta.2"),
            ("0.1.0-dev.20260414", "dev.20260414"),
        ] {
            let ver: Version = input.parse().unwrap();
            assert_eq!(ver.prerelease, Some(expected_pre.to_string()), "input: {input}");
            assert_eq!(ver.show(), input, "round-trip failed for: {input}");
        }
    }

    #[test]
    fn prerelease_sorts_before_release() {
        let rc: Version = "0.77.0-rc.1".parse().unwrap();
        let release = Version::new(0, 77, 0);
        assert!(rc < release);
    }

    // ---- Error message tests ----


    #[test]
    fn no_command_renders() {
        let err = ManagerError::NoCommand;
        assert!(err.to_string().contains("No command"));
    }

    #[test]
    fn no_active_environment_suggests_new() {
        let err = ManagerError::NoActiveEnvironment;
        assert!(err.to_string().contains("new"));
    }

    #[test]
    fn config_permission_denied_mentions_permissions() {
        let err = ManagerError::ConfigPermissionDenied("/etc/morloc/config.json".to_string());
        assert!(err.to_string().contains("Permission"));
    }

    #[test]
    fn freeze_error_renders() {
        let err = ManagerError::FreezeError("tar error".to_string());
        assert!(err.to_string().contains("Freeze failed"));
    }

    // ---- Config default tests ----

    #[test]
    fn default_config_has_no_active_env() {
        assert_eq!(Config::default().active_env, None);
    }

    #[test]
    fn default_config_uses_podman() {
        assert_eq!(Config::default().engine().unwrap(), ContainerEngine::Podman);
    }

    // ---- Config JSON round-trip tests ----

    #[test]
    fn config_json_round_trip() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("config.json");
        let cfg = Config {
            active_env: Some("ml".to_string()),
            backend: Backend::Container(ContainerEngine::Docker),
        };
        cfg::write_config(&path, &cfg).unwrap();
        let cfg2: Config = cfg::read_config(&path).unwrap();
        assert_eq!(cfg2.active_env.as_deref(), Some("ml"));
        assert_eq!(cfg2.engine().unwrap(), ContainerEngine::Docker);
    }

    // The Backend abstraction must be on-disk compatible with the historical
    // bare `engine:` field: existing config.json / env.yaml files (written
    // before Backend existed) must still deserialize, and new writes must keep
    // the same `engine` key so a downgrade or an older reader is unaffected.
    #[test]
    fn backend_reads_legacy_engine_field() {
        let legacy = r#"{"active_env":"ml","engine":"podman"}"#;
        let cfg: Config = serde_json::from_str(legacy).unwrap();
        assert_eq!(cfg.engine().unwrap(), ContainerEngine::Podman);
        assert!(matches!(
            cfg.backend,
            Backend::Container(ContainerEngine::Podman)
        ));
    }

    #[test]
    fn backend_serializes_under_engine_key() {
        let cfg = Config {
            active_env: None,
            backend: Backend::Container(ContainerEngine::Apptainer),
        };
        let json = serde_json::to_string(&cfg).unwrap();
        assert!(json.contains(r#""engine":"apptainer""#), "got: {json}");
        assert!(!json.contains("backend"), "backend must not leak on disk: {json}");
    }

    #[test]
    fn native_backend_round_trips_under_engine_key() {
        let cfg = Config {
            active_env: None,
            backend: Backend::Native,
        };
        let json = serde_json::to_string(&cfg).unwrap();
        assert!(json.contains(r#""engine":"native""#), "got: {json}");
        let back: Config = serde_json::from_str(&json).unwrap();
        assert!(back.backend.is_native());
        // A native backend has no container engine; engine() must be an error.
        assert!(back.engine().is_err());
    }

    #[test]
    fn config_read_missing_returns_not_found() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("nonexistent.json");
        let result = cfg::read_config::<Config>(&path);
        assert!(matches!(result, Err(ManagerError::ConfigNotFound(_))));
    }

    #[test]
    fn config_read_invalid_json_returns_parse_error() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("bad.json");
        fs::write(&path, "not json at all").unwrap();
        let result = cfg::read_config::<Config>(&path);
        assert!(matches!(result, Err(ManagerError::ConfigParseError { .. })));
    }

    #[test]
    fn env_config_json_round_trip() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("env.json");
        let ec = EnvironmentConfig {
            schema_version: crate::types::CURRENT_ENV_SCHEMA,
            name: "test".to_string(),
            base_image: "ghcr.io/morloc-project/morloc/morloc-full:0.67.0".to_string(),
            original_image: None,
            dockerfile: None,
            content_hash: None,
            built_image: None,
            singularity_def: None,
            def_content_hash: None,
            base_sif: None,
            layered_sif: None,
            backend: Backend::Container(ContainerEngine::Podman),
            shm_size: "1g".to_string(),
            morloc_version: Some(Version::new(0, 67, 0)),
            system_packages: Vec::new(),
        };
        cfg::write_config(&path, &ec).unwrap();
        let ec2: EnvironmentConfig = cfg::read_config(&path).unwrap();
        assert_eq!(ec2.name, "test");
        assert_eq!(ec2.shm_size, "1g");
        assert_eq!(ec2.morloc_version, Some(Version::new(0, 67, 0)));
    }

    #[test]
    fn freeze_manifest_json_round_trip() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("fm.json");
        let fm = FreezeManifest {
            morloc_version: Version::new(0, 67, 0),
            frozen_at: chrono::Utc::now(),
            modules: vec![ModuleEntry {
                name: "math".to_string(),
                version: Some("0.3.0".to_string()),
                sha256: "abc123".to_string(),
            }],
            programs: vec![ProgramEntry {
                name: "svc".to_string(),
                commands: vec!["hello".to_string(), "compute".to_string()],
            }],
            base_image: "morloc-full:0.67.0".to_string(),
            env_layer: Some(FrozenEnvLayer {
                name: "ml".to_string(),
                dockerfile: "FROM scratch".to_string(),
                content_hash: "abc".to_string(),
                image_tag: None,
            }),
            env_vars: Vec::new(),
        };
        cfg::write_config(&path, &fm).unwrap();
        let fm2: FreezeManifest = cfg::read_config(&path).unwrap();
        assert_eq!(fm2.morloc_version, Version::new(0, 67, 0));
        assert_eq!(fm2.modules.len(), 1);
        assert_eq!(fm2.programs.len(), 1);
        assert_eq!(fm2.programs[0].commands, vec!["hello", "compute"]);
        // env_vars is no longer written but can still be read from old manifests
        assert!(fm2.env_vars.is_empty());
    }

    #[test]
    fn freeze_manifest_reads_legacy_env_vars() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("legacy.json");
        // Version (de)serializes as a string via its Display/FromStr impls.
        // env_vars survives for backward-compat with old manifests that
        // wrote the field; new code skips it on write.
        let json = r#"{
            "morloc_version": "0.67.0",
            "frozen_at": "2025-01-01T00:00:00Z",
            "modules": [],
            "programs": [],
            "base_image": "morloc-full:0.67.0",
            "env_layer": null,
            "env_vars": ["API_KEY", "DB_URL"]
        }"#;
        std::fs::write(&path, json).unwrap();
        let fm: FreezeManifest = cfg::read_config(&path).unwrap();
        assert_eq!(fm.env_vars, vec!["API_KEY", "DB_URL"]);
    }

    // ---- FlagConfig tests ----

    #[test]
    fn flag_config_default_is_all_empty() {
        let fc = FlagConfig::default();
        for phase in [Phase::Run, Phase::Start] {
            for eng in [
                ContainerEngine::Docker,
                ContainerEngine::Podman,
                ContainerEngine::Apptainer,
            ] {
                assert!(fc.materialize(phase, eng).is_empty());
            }
        }
    }

    #[test]
    fn flag_config_materialize_concatenates_all_then_engine() {
        let yaml = r#"
run:
  all:
    - --shared
  apptainer:
    - --ignore-subuid
"#;
        let fc: FlagConfig = serde_yaml::from_str(yaml).unwrap();
        assert_eq!(
            fc.materialize(Phase::Run, ContainerEngine::Apptainer),
            vec!["--shared", "--ignore-subuid"]
        );
        assert_eq!(
            fc.materialize(Phase::Run, ContainerEngine::Docker),
            vec!["--shared"]
        );
        assert!(fc.materialize(Phase::Start, ContainerEngine::Apptainer).is_empty());
    }

    #[test]
    fn flag_config_rejects_unknown_section() {
        let yaml = "runn:\n  apptainer:\n    - --nv\n";
        let err = serde_yaml::from_str::<FlagConfig>(yaml).unwrap_err();
        assert!(err.to_string().contains("runn"), "got: {err}");
    }

    #[test]
    fn flag_config_rejects_unknown_engine() {
        let yaml = "run:\n  aptainer:\n    - --nv\n";
        let err = serde_yaml::from_str::<FlagConfig>(yaml).unwrap_err();
        assert!(err.to_string().contains("aptainer"), "got: {err}");
    }

    #[test]
    fn flag_config_rejects_scalar_as_list() {
        let yaml = "run:\n  apptainer: --nv\n";
        let err = serde_yaml::from_str::<FlagConfig>(yaml).unwrap_err();
        assert!(
            err.to_string().to_lowercase().contains("sequence")
                || err.to_string().to_lowercase().contains("list")
                || err.to_string().to_lowercase().contains("invalid"),
            "got: {err}"
        );
    }

    #[test]
    fn flag_config_singularity_alias_resolves_to_apptainer() {
        let yaml = "run:\n  singularity:\n    - --nv\n";
        let fc: FlagConfig = serde_yaml::from_str(yaml).unwrap();
        assert_eq!(
            fc.materialize(Phase::Run, ContainerEngine::Apptainer),
            vec!["--nv"]
        );
    }

    #[test]
    fn read_flag_config_absent_file_returns_default() {
        let dir = tempfile::tempdir().unwrap();
        // env.flags.yaml at default scope path won't exist for an env name
        // we just made up. Materialize should still work.
        let fc = FlagConfig::default();
        assert!(fc.materialize(Phase::Run, ContainerEngine::Apptainer).is_empty());
        // keep `dir` alive
        drop(dir);
    }

    // ---- Container CLI argument tests ----

    #[test]
    fn engine_executable_docker() {
        assert_eq!(engine_executable(ContainerEngine::Docker), "docker");
    }

    #[test]
    fn engine_executable_podman() {
        assert_eq!(engine_executable(ContainerEngine::Podman), "podman");
    }

    #[test]
    fn build_run_args_minimal() {
        let cfg = RunConfig::new("myimage:latest");
        let args = build_run_args(
            ContainerEngine::Docker,
            &engine_specific_run_flags(ContainerEngine::Docker),
            &cfg,
        );
        assert_eq!(args[0], "run");
        assert!(args.contains(&"--rm".to_string()));
        assert!(args.contains(&"myimage:latest".to_string()));
        assert!(!args.contains(&"-it".to_string()));
    }

    #[test]
    fn build_run_args_podman_userns() {
        let cfg = RunConfig::new("myimage:latest");
        let args = build_run_args(
            ContainerEngine::Podman,
            &engine_specific_run_flags(ContainerEngine::Podman),
            &cfg,
        );
        assert!(args.contains(&"--userns=keep-id".to_string()));
    }

    #[test]
    fn build_run_args_interactive() {
        let mut cfg = RunConfig::new("img");
        cfg.interactive = true;
        let args = build_run_args(
            ContainerEngine::Docker,
            &engine_specific_run_flags(ContainerEngine::Docker),
            &cfg,
        );
        // -i and -t are emitted as separate flags; -i is unconditional
        // so piped stdin works, -t is added for interactive sessions.
        assert!(args.contains(&"-i".to_string()));
        assert!(args.contains(&"-t".to_string()));
    }

    #[test]
    fn build_run_args_selinux_suffix() {
        let mut cfg = RunConfig::new("img");
        cfg.bind_mounts = vec![("/host".to_string(), "/container".to_string())];
        cfg.selinux_suffix = ":z".to_string();
        let args = build_run_args(
            ContainerEngine::Docker,
            &engine_specific_run_flags(ContainerEngine::Docker),
            &cfg,
        );
        assert!(args.contains(&"-v".to_string()));
        assert!(args.contains(&"/host:/container:z".to_string()));
    }

    #[test]
    fn build_run_args_workdir() {
        let mut cfg = RunConfig::new("img");
        cfg.work_dir = Some("/work".to_string());
        let args = build_run_args(
            ContainerEngine::Docker,
            &engine_specific_run_flags(ContainerEngine::Docker),
            &cfg,
        );
        assert!(args.contains(&"-w".to_string()));
        assert!(args.contains(&"/work".to_string()));
    }

    #[test]
    fn build_run_args_read_only() {
        let mut cfg = RunConfig::new("img");
        cfg.read_only = true;
        let args = build_run_args(
            ContainerEngine::Docker,
            &engine_specific_run_flags(ContainerEngine::Docker),
            &cfg,
        );
        assert!(args.contains(&"--read-only".to_string()));
    }

    #[test]
    fn build_run_args_command_at_end() {
        let mut cfg = RunConfig::new("img");
        cfg.command = Some(vec![
            "morloc".to_string(),
            "make".to_string(),
            "-o".to_string(),
            "svc".to_string(),
            "svc.loc".to_string(),
        ]);
        let args = build_run_args(
            ContainerEngine::Docker,
            &engine_specific_run_flags(ContainerEngine::Docker),
            &cfg,
        );
        let img_idx = args.iter().position(|a| a == "img").unwrap();
        let cmd_idx = args.iter().position(|a| a == "morloc").unwrap();
        assert!(img_idx < cmd_idx);
    }

    #[test]
    fn build_build_args_includes_tag_and_dockerfile() {
        let cfg = BuildConfig {
            dockerfile: "/tmp/Dockerfile".to_string(),
            context: "/tmp/ctx".to_string(),
            tag: "test:v1".to_string(),
            build_args: vec![("BASE".to_string(), "ubuntu:22.04".to_string())],
            extra_flags: Vec::new(),
        };
        let args = build_build_args(&cfg);
        assert_eq!(args[0], "build");
        assert!(args.contains(&"-f".to_string()));
        assert!(args.contains(&"-t".to_string()));
        assert!(args.contains(&"--build-arg".to_string()));
        assert_eq!(args.last().unwrap(), "/tmp/ctx");
    }

    #[test]
    fn build_build_args_includes_extra_flags_before_context() {
        let cfg = BuildConfig {
            dockerfile: "/tmp/Dockerfile".to_string(),
            context: "/tmp/ctx".to_string(),
            tag: "test:v1".to_string(),
            build_args: vec![("BASE".to_string(), "ubuntu:22.04".to_string())],
            extra_flags: vec!["--platform=linux/amd64".to_string()],
        };
        let args = build_build_args(&cfg);
        let flag_idx = args.iter().position(|a| a == "--platform=linux/amd64").unwrap();
        let ctx_idx = args.iter().position(|a| a == "/tmp/ctx").unwrap();
        assert!(flag_idx < ctx_idx);
    }

    // ---- SELinux tests ----

    #[test]
    fn root_is_unsafe() {
        assert!(!selinux::is_safe_to_relabel("/"));
    }

    #[test]
    fn tmp_is_unsafe() {
        assert!(!selinux::is_safe_to_relabel("/tmp"));
    }

    #[test]
    fn tmp_subdir_is_unsafe() {
        assert!(!selinux::is_safe_to_relabel("/tmp/foo"));
    }

    #[test]
    fn home_subdir_is_safe() {
        assert!(selinux::is_safe_to_relabel("/home/user/project"));
    }

    #[test]
    fn var_tmp_is_unsafe() {
        assert!(!selinux::is_safe_to_relabel("/var/tmp"));
    }

    // ---- Apptainer engine tests ----

    #[test]
    fn engine_executable_apptainer_returns_one_of_two() {
        // Runtime-detected; the result depends on what's on $PATH at test time.
        // Just assert the cached value is a known name.
        let exe = engine_executable(ContainerEngine::Apptainer);
        assert!(exe == "apptainer" || exe == "singularity");
    }

    #[test]
    fn container_engine_deserializes_apptainer() {
        let j: ContainerEngine = serde_json::from_str("\"apptainer\"").unwrap();
        assert_eq!(j, ContainerEngine::Apptainer);
    }

    #[test]
    fn container_engine_deserializes_singularity_as_apptainer() {
        let j: ContainerEngine = serde_json::from_str("\"singularity\"").unwrap();
        assert_eq!(j, ContainerEngine::Apptainer);
    }

    #[test]
    fn container_engine_apptainer_serializes_as_apptainer() {
        let s = serde_json::to_string(&ContainerEngine::Apptainer).unwrap();
        assert_eq!(s, "\"apptainer\"");
    }

    #[test]
    fn build_run_args_apptainer_with_command() {
        let mut cfg = RunConfig::new("/path/to/base.sif");
        cfg.command = Some(vec!["morloc".to_string(), "--version".to_string()]);
        cfg.bind_mounts = vec![("/host/data".to_string(), "/opt/morloc".to_string())];
        cfg.env = vec![("MORLOC_HOME".to_string(), "/opt/morloc".to_string())];
        cfg.work_dir = Some("/tmp".to_string());
        let args = build_run_args(ContainerEngine::Apptainer, &[], &cfg);
        // Subcommand should be `exec` (not `run`/`shell`).
        assert_eq!(args[0], "exec");
        // Bind translation.
        assert!(args.windows(2).any(|w| w == ["--bind", "/host/data:/opt/morloc"]));
        // Env translation.
        assert!(args.windows(2).any(|w| w == ["--env", "MORLOC_HOME=/opt/morloc"]));
        // Workdir translation: -w -> --pwd.
        assert!(args.windows(2).any(|w| w == ["--pwd", "/tmp"]));
        // No `-i`, `-t`, `--rm`, `--user`, `--name`, `--shm-size`, `-p`, `-v`, `-w`, `-e` in argv.
        for forbidden in ["-i", "-t", "--rm", "--user", "--name", "--shm-size", "-v", "-w", "-e"] {
            assert!(
                !args.iter().any(|a| a == forbidden),
                "argv leaked a Docker-style flag: {forbidden} in {:?}", args
            );
        }
        // Image and command come at the end.
        let img_idx = args.iter().position(|a| a == "/path/to/base.sif").unwrap();
        assert!(args[img_idx + 1..].contains(&"morloc".to_string()));
        assert!(args[img_idx + 1..].contains(&"--version".to_string()));
    }

    #[test]
    fn build_run_args_apptainer_shell() {
        let mut cfg = RunConfig::new("/path/to/base.sif");
        cfg.interactive = true;
        cfg.command = Some(vec!["/bin/bash".to_string()]);
        let args = build_run_args(ContainerEngine::Apptainer, &[], &cfg);
        assert_eq!(args[0], "shell");
        // After the image, `shell` does not append the command (it's the shell itself).
        let img_idx = args.iter().position(|a| a == "/path/to/base.sif").unwrap();
        assert!(args[img_idx + 1..].is_empty());
    }

    #[test]
    fn build_run_args_apptainer_no_command_uses_run() {
        let cfg = RunConfig::new("/path/to/base.sif");
        let args = build_run_args(ContainerEngine::Apptainer, &[], &cfg);
        // No command and not shell -> `apptainer run <sif>` invokes runscript.
        assert_eq!(args[0], "run");
    }

    #[test]
    fn build_run_args_apptainer_drops_shm_size() {
        let mut cfg = RunConfig::new("/path/to/base.sif");
        cfg.shm_size = Some("1g".to_string());
        cfg.command = Some(vec!["true".to_string()]);
        let args = build_run_args(ContainerEngine::Apptainer, &[], &cfg);
        assert!(!args.iter().any(|a| a == "--shm-size"));
        assert!(!args.iter().any(|a| a == "1g"));
    }





    #[test]
    fn env_config_yaml_round_trip_with_apptainer_fields() {
        let dir = tempfile::tempdir().unwrap();
        // Use a temp env dir layout. write_env_config takes (scope, name) and
        // pins to cfg::env_config_path; we use the lower-level write helper
        // to keep the test hermetic.
        let path = dir.path().join("env.yaml");
        let ec = EnvironmentConfig {
            schema_version: crate::types::CURRENT_ENV_SCHEMA,
            name: "dnd".to_string(),
            base_image: "ghcr.io/morloc-project/morloc/morloc-full:0.85.0".to_string(),
            original_image: None,
            dockerfile: None,
            content_hash: None,
            built_image: None,
            singularity_def: Some("recipe.def".to_string()),
            def_content_hash: Some("deadbeef".to_string()),
            base_sif: Some("/data/dnd/sif/base.sif".to_string()),
            layered_sif: Some("/data/dnd/sif/layered.sif".to_string()),
            backend: Backend::Container(ContainerEngine::Apptainer),
            shm_size: "512m".to_string(),
            morloc_version: Some(Version::new(0, 85, 0)),
            system_packages: Vec::new(),
        };
        let yaml = serde_yaml::to_string(&ec).unwrap();
        std::fs::write(&path, yaml).unwrap();
        let raw = std::fs::read_to_string(&path).unwrap();
        let ec2: EnvironmentConfig = serde_yaml::from_str(&raw).unwrap();
        assert_eq!(ec2.engine().unwrap(), ContainerEngine::Apptainer);
        assert_eq!(ec2.singularity_def.as_deref(), Some("recipe.def"));
        assert_eq!(ec2.base_sif.as_deref(), Some("/data/dnd/sif/base.sif"));
        assert_eq!(ec2.layered_sif.as_deref(), Some("/data/dnd/sif/layered.sif"));
        assert_eq!(ec2.def_content_hash.as_deref(), Some("deadbeef"));
    }

    #[test]
    fn env_config_json_back_compat_reads_without_apptainer_fields() {
        // Legacy env.json (created before the new fields were added) must
        // still deserialize. Use the existing JSON read path.
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("env.json");
        let legacy = r#"{
            "name": "legacy",
            "base_image": "ghcr.io/morloc-project/morloc/morloc-full:0.67.0",
            "engine": "podman",
            "shm_size": "512m"
        }"#;
        std::fs::write(&path, legacy).unwrap();
        let ec: EnvironmentConfig = cfg::read_config(&path).unwrap();
        assert_eq!(ec.engine().unwrap(), ContainerEngine::Podman);
        assert!(ec.singularity_def.is_none());
        assert!(ec.base_sif.is_none());
        assert!(ec.layered_sif.is_none());
    }

    #[test]
    fn active_image_apptainer_prefers_layered_sif() {
        let ec = EnvironmentConfig {
            schema_version: crate::types::CURRENT_ENV_SCHEMA,
            name: "test".to_string(),
            base_image: "ghcr.io/morloc-project/morloc/morloc-full:0.85.0".to_string(),
            original_image: None,
            dockerfile: None,
            content_hash: None,
            built_image: None,
            singularity_def: None,
            def_content_hash: None,
            base_sif: Some("/base.sif".to_string()),
            layered_sif: Some("/layered.sif".to_string()),
            backend: Backend::Container(ContainerEngine::Apptainer),
            shm_size: "512m".to_string(),
            morloc_version: None,
            system_packages: Vec::new(),
        };
        assert_eq!(ec.active_image(), "/layered.sif");
    }

    #[test]
    fn active_image_apptainer_falls_back_to_base_sif() {
        let ec = EnvironmentConfig {
            schema_version: crate::types::CURRENT_ENV_SCHEMA,
            name: "test".to_string(),
            base_image: "ghcr.io/morloc-project/morloc/morloc-full:0.85.0".to_string(),
            original_image: None,
            dockerfile: None,
            content_hash: None,
            built_image: None,
            singularity_def: None,
            def_content_hash: None,
            base_sif: Some("/base.sif".to_string()),
            layered_sif: None,
            backend: Backend::Container(ContainerEngine::Apptainer),
            shm_size: "512m".to_string(),
            morloc_version: None,
            system_packages: Vec::new(),
        };
        assert_eq!(ec.active_image(), "/base.sif");
    }

    #[test]
    fn cache_key_folds_system_packages() {
        let bin = std::path::Path::new("/nonexistent/morloc");
        let base = cache_key("manifest", bin, &[]);
        // No packages: no extra line, so envs declaring none keep a stable key.
        assert!(!base.contains("system-packages"));
        // Declaring one changes the key so the image is rebuilt.
        let with_pkg = cache_key("manifest", bin, &["jq".to_string()]);
        assert_ne!(base, with_pkg);
        assert!(with_pkg.contains("# system-packages: jq"));
    }

    // ---- native Runner seam ----

    fn native_test_env(name: &str) -> runner::ResolvedEnv {
        runner::ResolvedEnv {
            name: name.to_string(),
            scope: Scope::Local,
            ec: EnvironmentConfig {
                schema_version: crate::types::CURRENT_ENV_SCHEMA,
                name: name.to_string(),
                base_image: String::new(),
                original_image: None,
                dockerfile: None,
                content_hash: None,
                built_image: None,
                singularity_def: None,
                def_content_hash: None,
                base_sif: None,
                layered_sif: None,
                backend: Backend::Native,
                shm_size: "512m".to_string(),
                morloc_version: None,
                system_packages: Vec::new(),
            },
        }
    }

    fn native_req(engine_args: Vec<String>, slurm_bridge: bool) -> runner::RunRequest {
        runner::RunRequest {
            verbose: false,
            shell: false,
            args: vec!["morloc".to_string(), "--version".to_string()],
            user_env: Vec::new(),
            engine_args,
            phase: Phase::Run,
            slurm_bridge,
        }
    }

    #[test]
    fn native_run_rejects_engine_args() {
        let env = native_test_env("nat-engine-args");
        let err = native_run_env(&env, &native_req(vec!["--privileged".to_string()], false))
            .unwrap_err();
        assert!(err.to_string().contains("container-only"), "{err}");
    }

    #[test]
    fn native_run_rejects_slurm_bridge() {
        let env = native_test_env("nat-slurm");
        let err = native_run_env(&env, &native_req(Vec::new(), true)).unwrap_err();
        assert!(err.to_string().contains("container backend"), "{err}");
    }

    #[test]
    fn native_run_requires_materialization() {
        // A native env with no materialization record must fail loudly rather
        // than spawn against an unprovisioned toolchain.
        let env = native_test_env("nat-unmaterialized-zzqx");
        let err = native_run_env(&env, &native_req(Vec::new(), false)).unwrap_err();
        assert!(err.to_string().contains("not been materialized"), "{err}");
    }

    // ---- --lang pins ----

    #[test]
    fn parse_lang_pins_splits_atoms_and_versions() {
        let pins = parse_lang_pins(&["py@3.12".to_string(), "cpp, r@4.3".to_string()]);
        assert_eq!(
            pins,
            vec![
                ("py".to_string(), Some("3.12".to_string())),
                ("cpp".to_string(), None),
                ("r".to_string(), Some("4.3".to_string())),
            ]
        );
        // Blank atoms are dropped.
        assert!(parse_lang_pins(&[" , ".to_string()]).is_empty());
    }

    fn lang_support_fixture() -> langsupport::LangSupport {
        const S: &str = r#"{"morloc_version":"0.0.0","toolchain":[],
          "languages":{
            "py":{"runtime":{"package":"python","version":">=3.10,<3.14","default":"3.12"},"requires":[]},
            "cpp":{"runtime":null,"requires":[]}
          }}"#;
        langsupport::LangSupport::from_json(S).unwrap()
    }

    #[test]
    fn resolve_lang_pin_clamps_to_supported_range() {
        let s = lang_support_fixture();
        // A minor pin intersects with morloc's supported range.
        let req = resolve_lang_pin("py", Some("3.12"), "0.0.0", &s).unwrap();
        assert_eq!(req.constraint.as_deref(), Some(">=3.12,<3.13"));
        // No pin yields the full supported range (so conda python is used).
        let req = resolve_lang_pin("py", None, "0.0.0", &s).unwrap();
        assert_eq!(req.constraint.as_deref(), Some(">=3.10,<3.14"));
        // A language with no runtime (cpp) carries no version constraint.
        let req = resolve_lang_pin("cpp", None, "0.0.0", &s).unwrap();
        assert!(req.constraint.is_none());
    }

    #[test]
    fn resolve_lang_pin_rejects_out_of_range() {
        let s = lang_support_fixture();
        // 3.20 is outside morloc's >=3.10,<3.14 support window.
        let err = resolve_lang_pin("py", Some("3.20"), "0.0.0", &s).unwrap_err();
        assert!(err.to_string().contains("no py version"), "{err}");
    }

    #[test]
    fn find_envspec_files_scans_and_skips_heavy_dirs() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        // A program build dir carries an envspec.json to be collected.
        std::fs::create_dir_all(root.join("prog-build")).unwrap();
        std::fs::write(root.join("prog-build/envspec.json"), "{}").unwrap();
        // A decoy inside the skipped pixi subtree must NOT be collected.
        std::fs::create_dir_all(root.join("pixi/.pixi/envs")).unwrap();
        std::fs::write(root.join("pixi/.pixi/envs/envspec.json"), "{}").unwrap();
        let found = find_envspec_files(root);
        assert_eq!(found.len(), 1);
        assert!(found[0].ends_with("prog-build/envspec.json"));
    }

    #[test]
    fn kill_native_group_reaps_children() {
        use nix::sys::signal::kill;
        use nix::unistd::Pid;
        use std::time::Duration;
        // A leader (setsid session/group leader) that spawns a background child
        // sharing the group -- the shape of the nexus + its pool daemons.
        let dir = tempfile::tempdir().unwrap();
        let pidfile = dir.path().join("child.pid");
        let mut cmd = std::process::Command::new("sh");
        cmd.arg("-c")
            .arg(format!("sleep 60 & echo $! > {}; wait", pidfile.display()));
        unsafe {
            cmd.pre_exec(|| {
                nix::unistd::setsid()
                    .map(|_| ())
                    .map_err(|e| std::io::Error::from_raw_os_error(e as i32))
            });
        }
        let mut child = cmd.spawn().unwrap();
        let leader = child.id();

        // Wait for the child (grandchild of us) to report its pid.
        let mut grandchild = 0u32;
        for _ in 0..100 {
            std::thread::sleep(Duration::from_millis(20));
            if let Ok(s) = std::fs::read_to_string(&pidfile) {
                if let Ok(p) = s.trim().parse::<u32>() {
                    grandchild = p;
                    break;
                }
            }
        }
        assert!(grandchild != 0, "child pid was not captured");
        assert!(
            kill(Pid::from_raw(grandchild as i32), None).is_ok(),
            "child should be alive before the group kill"
        );

        kill_native_group(leader);
        // In production the nexus is reparented to init and reaped there; in the
        // test WE are the leader's parent, so reap its zombie before asserting.
        let _ = child.wait();

        // The child pool daemon received the group SIGTERM. It is either fully
        // reaped, or (under a subreaper that hasn't reaped it yet) a zombie --
        // both prove the kill reached the WHOLE group, not just the leader. The
        // failure mode we're guarding against is an orphan still sleeping.
        let dead_or_zombie = |pid: u32| -> bool {
            if kill(Pid::from_raw(pid as i32), None).is_err() {
                return true; // gone
            }
            match std::fs::read_to_string(format!("/proc/{pid}/stat")) {
                // State char follows the "(comm)"; 'Z' = zombie (terminated).
                Ok(stat) => stat
                    .rsplit(')')
                    .next()
                    .map(|rest| rest.trim_start().starts_with('Z'))
                    .unwrap_or(false),
                Err(_) => true, // no /proc: the kill(0)==err path above governs
            }
        };
        let mut ok = false;
        for _ in 0..100 {
            if dead_or_zombie(grandchild) {
                ok = true;
                break;
            }
            std::thread::sleep(Duration::from_millis(20));
        }
        assert!(ok, "child pool daemon must be terminated by the group kill (not left an orphan)");
    }
}
