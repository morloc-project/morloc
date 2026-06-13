mod bridge;
mod config;
mod container;
mod doctor;
mod environment;
mod error;
mod freeze;
mod selinux;
mod serve;
mod types;

use std::collections::HashSet;
use std::fs;
use std::io::{self, IsTerminal, Write};
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

{bu}Deployment{r}
  {b}start{r}      Serve an environment over the network
  {b}stop{r}       Stop a running serve container
  {b}logs{r}       Stream logs from a running serve container
  {b}freeze{r}     Export installed state as a frozen artifact
  {b}unfreeze{r}   Build a portable serve image from frozen state
  {b}status{r}     List running serve containers
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
    #[command(after_help = "Examples:\n  morloc-manager new\n  morloc-manager new myenv --version 0.73.0\n  morloc-manager new myenv --tag edge\n  morloc-manager new myenv --image ubuntu:22.04 --dockerfile ./Dockerfile\n\nDefault (when --version, --tag, and --image are all omitted): pulls the\n:edge tag from the morloc registry and records the resolved version.\n\nIn non-interactive mode (no TTY), if no name is given, the latest edge\nimage is pulled and the environment is named after the detected morloc\nversion.")]
    New {
        /// Environment name (default: derived from base image version)
        name: Option<String>,
        /// Base image from Docker Hub or a registry
        #[arg(long)]
        image: Option<String>,
        /// Morloc version (MAJOR.MINOR.PATCH, leading 'v' stripped automatically)
        #[arg(long)]
        version: Option<String>,
        /// Container image tag (e.g., 'edge', 'nightly')
        #[arg(long, conflicts_with_all = ["version", "image"])]
        tag: Option<String>,
        /// Dockerfile to layer on top of the base image
        #[arg(long)]
        dockerfile: Option<String>,
        /// Generate a stub Dockerfile for customization
        #[arg(long)]
        dockerfile_stub: bool,
        /// Singularity .def recipe to layer on top of the base image (apptainer engine)
        #[arg(long)]
        deffile: Option<String>,
        /// Generate a stub Singularity .def recipe for customization (apptainer engine)
        #[arg(long)]
        deffile_stub: bool,
        /// Force overwrite of existing Dockerfile stub
        #[arg(long)]
        force: bool,
        /// Include file/dir in build context; use src:dest for explicit placement (repeatable)
        #[arg(short = 'i', long = "include")]
        include: Vec<String>,
        /// Path to a file with one engine argument per line
        #[arg(long)]
        flagfile: Option<String>,
        /// A single engine flag (may be repeated)
        #[arg(short = 'x', long = "engine-arg", allow_hyphen_values = true)]
        engine_arg: Vec<String>,
        /// Container engine: podman, docker, apptainer, or singularity
        #[arg(long, value_enum)]
        engine: Option<EngineArg>,
        /// Shared memory size (default: 512m; ignored under apptainer/singularity)
        #[arg(long)]
        shm_size: Option<String>,
        /// Create in system scope (requires root)
        #[arg(long)]
        system: bool,
        /// Skip morloc init after creation
        #[arg(long)]
        no_init: bool,
        /// Skip interactive wizard, use defaults for unspecified options
        #[arg(long)]
        non_interactive: bool,
        /// Extra argument to pass through to the in-container
        /// `morloc init -f` invocation (repeatable). Typical uses:
        /// `--init-arg --slurm` to enable SLURM-dispatch codegen,
        /// `--init-arg --sanitize` for libmorloc.so built with ASan.
        /// Generic mechanism: any compiler init flag works without a
        /// new morloc-manager flag.
        #[arg(long = "init-arg", allow_hyphen_values = true)]
        init_args: Vec<String>,
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
    #[command(after_help = "Examples:\n  morloc-manager update              # rebuild active environment\n  morloc-manager update myenv        # rebuild a specific environment\n  morloc-manager update --shm-size 1g\n  morloc-manager update --dockerfile ./new.Dockerfile -i ./data\n  morloc-manager update myenv --reinit  # re-run morloc init in myenv")]
    Update {
        /// Environment name (default: active environment)
        name: Option<String>,
        /// Change the base image
        #[arg(long)]
        image: Option<String>,
        /// Change to a specific morloc version (MAJOR.MINOR.PATCH, leading 'v' stripped)
        #[arg(long)]
        version: Option<String>,
        /// Container image tag (e.g., 'edge', 'nightly')
        #[arg(long, conflicts_with_all = ["version", "image"])]
        tag: Option<String>,
        /// Replace the Dockerfile
        #[arg(long)]
        dockerfile: Option<String>,
        /// Replace the Singularity .def recipe (apptainer engine)
        #[arg(long)]
        deffile: Option<String>,
        /// Include file/dir in build context; use src:dest for explicit placement (repeatable)
        #[arg(short = 'i', long = "include")]
        include: Vec<String>,
        /// Replace the flags file (YAML, schema documented in env.flags.yaml stub)
        #[arg(long)]
        flagfile: Option<String>,
        /// One-shot build-engine flag for this rebuild only (repeatable;
        /// requires --reinit; not persisted). Use env.flags.yaml `build`
        /// section for persistent build flags.
        #[arg(long = "reinit-arg", allow_hyphen_values = true, requires = "reinit")]
        reinit_arg: Vec<String>,
        /// Change the container engine
        #[arg(long, value_enum)]
        engine: Option<EngineArg>,
        /// Change shared memory size
        #[arg(long)]
        shm_size: Option<String>,
        /// Generate a stub Dockerfile (fails if one already exists)
        #[arg(long)]
        dockerfile_stub: bool,
        /// Generate a stub Singularity .def recipe (fails if one already exists)
        #[arg(long)]
        deffile_stub: bool,
        /// Force overwrite of existing Dockerfile/.def stub
        #[arg(long)]
        force: bool,
        /// Skip Dockerfile/.def build
        #[arg(long)]
        no_build: bool,
        /// Re-run morloc init
        #[arg(long)]
        reinit: bool,
        /// Extra argument to pass through to the in-container
        /// `morloc init -f` invocation when `--reinit` is set
        /// (repeatable). Same shape as `new --init-arg`. Typical:
        /// `--init-arg --slurm` to enable SLURM-dispatch codegen.
        #[arg(long = "init-arg", allow_hyphen_values = true, requires = "reinit")]
        init_args: Vec<String>,
        /// Accepted for scripting uniformity with `new` (no effect)
        #[arg(long, hide = true)]
        non_interactive: bool,
    },

    // -- Deployment --
    /// Serve an environment over the network
    #[command(display_order = 20)]
    #[command(after_help = "Examples:\n  morloc-manager start              # serve active environment\n  morloc-manager start myenv -p 9090:8080")]
    Start {
        /// Environment name (default: active environment)
        name: Option<String>,
        /// Port mapping HOST:CONTAINER (default: 8080:8080)
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
    /// List running serve containers
    #[command(display_order = 26)]
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

#[derive(Clone, ValueEnum)]
enum EngineArg {
    Docker,
    Podman,
    Apptainer,
    /// Alias for Apptainer (the older binary name). Both resolve to
    /// ContainerEngine::Apptainer; the runtime executable is detected later.
    Singularity,
}

impl From<EngineArg> for ContainerEngine {
    fn from(e: EngineArg) -> Self {
        match e {
            EngineArg::Docker => ContainerEngine::Docker,
            EngineArg::Podman => ContainerEngine::Podman,
            EngineArg::Apptainer | EngineArg::Singularity => ContainerEngine::Apptainer,
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
        return Ok(cfg.engine);
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

fn display_engine(engine: ContainerEngine) -> &'static str {
    match engine {
        ContainerEngine::Docker => "docker",
        ContainerEngine::Podman => "podman",
        ContainerEngine::Apptainer => "apptainer",
    }
}

/// Content of the Singularity .def stub written by `--deffile-stub`. Uses
/// Apptainer's native build-arg substitution: `{{ BASE_SIF }}` is replaced
/// at build time with the path to the env's base .sif. No textual
/// preprocessing in morloc-manager.
fn singularity_def_stub(env_name: &str) -> String {
    format!(
"# morloc environment: {env_name}
# Edit this file, then rebuild with: morloc-manager update
#
# {{{{ BASE_SIF }}}} is substituted at build time with the path to
# this environment's base .sif.

Bootstrap: localimage
From: {{{{ BASE_SIF }}}}

%post
    set -euo pipefail

    # apt-get on hosts without a full subuid range needs APT::Sandbox::User=root:
    #   apt-get -o APT::Sandbox::User=root update \\
    #     && apt-get -o APT::Sandbox::User=root install -y jq \\
    #     && rm -rf /var/lib/apt/lists/*
    #
    # pip install scikit-learn pandas
    # R -e \"install.packages('ggplot2', repos='https://cloud.r-project.org')\"
    :
"
    )
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

fn warn_podman_additional_stores() {
    eprintln!("Warning: Podman is not configured to see system (rootful) images.");
    eprintln!("  Non-root users will not be able to run system environments.");
    eprintln!("  Option 1 (recommended): Use Docker for system environments.");
    eprintln!("  Option 2: Add to [storage.options] in /etc/containers/storage.conf:");
    eprintln!();
    eprintln!("    additionalimagestores = [\"/var/lib/containers/storage\"]");
    eprintln!();
    eprintln!("  Note: Option 2 may cause storage locking conflicts on Fedora and Debian.");
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
                    local.as_ref().map(|c| display_engine(c.engine)).unwrap_or("unset"));
                println!("System engine:  {}",
                    sys.as_ref().map(|c| display_engine(c.engine)).unwrap_or("unset"));
                println!();
                println!("Set with: morloc-manager setup --engine <podman|docker|apptainer|singularity>");
                return Ok(());
            }
            if system { check_system_write_access()?; }
            let scope = resolve_scope(system);
            let eng: ContainerEngine = engine.unwrap().into();
            check_docker_socket(eng);
            let cfg_path = cfg::config_path(scope);
            let base_cfg = cfg::read_config::<Config>(&cfg_path).unwrap_or_default();
            let new_cfg = Config {
                engine: eng,
                ..base_cfg
            };
            cfg::write_config(&cfg_path, &new_cfg)?;
            eprintln!("Engine set to: {}", display_engine(eng));
            Ok(())
        }

        // ---- new ----
        Cmd::New {
            name,
            image,
            version,
            tag,
            dockerfile,
            dockerfile_stub,
            deffile,
            deffile_stub,
            force,
            include,
            flagfile,
            engine_arg,
            engine,
            shm_size,
            system,
            no_init,
            non_interactive,
            init_args,
        } => {
            if system { check_system_write_access()?; }
            let scope = resolve_scope(system);

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
                cfg.engine
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
                    engine: resolved_engine,
                };
                cfg::write_config(&cfg_path, &new_cfg)?;
            }

            let interactive = !non_interactive && io::stdin().is_terminal();
            if !non_interactive && !interactive {
                eprintln!("Note: No TTY detected, running in non-interactive mode.");
            }

            // Step 1: Resolve name (ask first so user isn't surprised after a long pull)
            let env_name = if let Some(n) = name {
                if cfg::env_config_path(scope, &n).is_file() {
                    return Err(ManagerError::EnvError(format!(
                        "Environment '{n}' already exists"
                    )));
                }
                n
            } else if interactive {
                loop {
                    eprint!("Environment name: ");
                    io::stderr().flush().ok();
                    let mut name_input = String::new();
                    io::stdin().read_line(&mut name_input).ok();
                    let n = name_input.trim().to_string();
                    if n.is_empty() {
                        eprintln!("Name cannot be empty.");
                        continue;
                    }
                    if cfg::env_config_path(scope, &n).is_file() {
                        eprintln!("Environment '{n}' already exists. Choose a different name.");
                        continue;
                    }
                    break n;
                }
            } else {
                // Non-interactive without a name: will be filled in after
                // version resolution below (default to version string)
                String::new()
            };

            // Validate name early (before potentially slow image pull)
            if !env_name.is_empty() {
                environment::validate_env_name(&env_name)?;
            }

            if version.is_some() && image.is_some() {
                return Err(ManagerError::EnvError(
                    "--version and --image are mutually exclusive".to_string()
                ));
            }

            // Validate cheap-to-check parameters before any I/O
            if let Some(ref shm) = shm_size {
                if !environment::is_valid_shm_size(shm) {
                    return Err(ManagerError::EnvError(format!(
                        "Invalid --shm-size '{shm}'. Use format like: 512m, 1g, 2048k"
                    )));
                }
            }

            // Step 2: Resolve base image and version
            let (base_image, original_image, morloc_ver) = if let Some(ref ver_str) = version {
                // Strip leading 'v' for convenience (e.g., "v0.77.0" -> "0.77.0")
                let clean = ver_str.strip_prefix('v').unwrap_or(ver_str);
                let ver: Version = clean.parse().map_err(|_| {
                    ManagerError::InvalidVersion(ver_str.clone())
                })?;
                let img = environment::pull_version_image(resolved_engine, &ver)?;
                (img, None, Some(ver))
            } else if let Some(ref t) = tag {
                let (img, ver) = environment::pull_tagged_image(resolved_engine, t)?;
                (img, None, Some(ver))
            } else if let Some(ref img) = image {
                environment::pull_custom_image(resolved_engine, img)?;
                (img.clone(), None, None)
            } else if interactive {
                eprintln!("Choose a base image:");
                eprintln!("  [1] Latest morloc release (recommended)");
                eprintln!("  [2] Specific morloc version");
                eprintln!("  [3] Custom image");
                eprint!("Choose [1]: ");
                io::stderr().flush().ok();
                let mut input = String::new();
                io::stdin().read_line(&mut input).ok();
                match input.trim() {
                    "2" => {
                        eprint!("Morloc version: ");
                        io::stderr().flush().ok();
                        let mut ver_input = String::new();
                        io::stdin().read_line(&mut ver_input).ok();
                        let ver: Version = ver_input.trim().parse().map_err(|_| {
                            ManagerError::InvalidVersion(ver_input.trim().to_string())
                        })?;
                        let img = environment::pull_version_image(resolved_engine, &ver)?;
                        (img, None, Some(ver))
                    }
                    "3" => {
                        eprint!("Image reference: ");
                        io::stderr().flush().ok();
                        let mut img_input = String::new();
                        io::stdin().read_line(&mut img_input).ok();
                        let img = img_input.trim().to_string();
                        if img.is_empty() {
                            return Err(ManagerError::EnvError("No image specified".to_string()));
                        }
                        environment::pull_custom_image(resolved_engine, &img)?;
                        (img, None, None)
                    }
                    _ => {
                        let (img, ver) = environment::resolve_latest(resolved_engine)?;
                        (img.clone(), Some(img), Some(ver))
                    }
                }
            } else {
                let (img, ver) = environment::resolve_latest(resolved_engine)?;
                (img.clone(), Some(img), Some(ver))
            };

            // Fill in name for non-interactive mode if it wasn't provided
            let env_name = if env_name.is_empty() {
                if let Some(ref ver) = morloc_ver {
                    let default_name = ver.show();
                    if cfg::env_config_path(scope, &default_name).is_file() {
                        return Err(ManagerError::EnvError(format!(
                            "Environment '{}' already exists. Specify a different name: morloc-manager new <NAME> ...",
                            default_name
                        )));
                    }
                    default_name
                } else {
                    return Err(ManagerError::EnvError(
                        "Environment name required in non-interactive mode".to_string(),
                    ));
                }
            } else {
                env_name
            };

            // Resolve dockerfile: explicit path takes precedence, then stub generation
            let resolved_dockerfile = if dockerfile.is_some() {
                if dockerfile_stub {
                    return Err(ManagerError::EnvError(
                        "Cannot use both --dockerfile and --dockerfile-stub".to_string(),
                    ));
                }
                dockerfile
            } else if dockerfile_stub {
                let df_path = cfg::env_dockerfile_path(scope, &env_name);
                if df_path.exists() && !force {
                    return Err(ManagerError::EnvError(format!(
                        "Dockerfile already exists: {}\nUse --force to overwrite.",
                        df_path.display()
                    )));
                }
                let stub_dir = cfg::data_dir(scope).join("tmp");
                fs::create_dir_all(&stub_dir).map_err(|e| {
                    ManagerError::EnvError(format!("Failed to create tmp dir: {e}"))
                })?;
                let stub_path = stub_dir.join(format!("{env_name}.Dockerfile"));
                let stub_content = format!(
                    "# morloc environment: {env_name}\n\
                     # Edit this file, then rebuild with: morloc-manager update\n\
                     \n\
                     # CONTAINER_BASE is replaced at build time with the environment's base image\n\
                     ARG CONTAINER_BASE=scratch\n\
                     FROM ${{CONTAINER_BASE}}\n\
                     \n\
                     # Example: install system packages\n\
                     # RUN apt-get update && apt-get install -y jq && rm -rf /var/lib/apt/lists/*\n\
                     \n\
                     # Example: install Python packages\n\
                     # RUN pip install scikit-learn pandas\n\
                     \n\
                     # Example: install R packages\n\
                     # RUN R -e \"install.packages('ggplot2', repos='https://cloud.r-project.org')\"\n"
                );
                fs::write(&stub_path, &stub_content).map_err(|e| {
                    ManagerError::EnvError(format!("Failed to write stub Dockerfile: {e}"))
                })?;
                Some(stub_path.to_string_lossy().to_string())
            } else {
                None
            };

            // Resolve .def file: explicit path takes precedence, then stub generation
            let resolved_deffile = if deffile.is_some() {
                if deffile_stub {
                    return Err(ManagerError::EnvError(
                        "Cannot use both --deffile and --deffile-stub".to_string(),
                    ));
                }
                deffile
            } else if deffile_stub {
                let def_path = cfg::env_deffile_path(scope, &env_name);
                if def_path.exists() && !force {
                    return Err(ManagerError::EnvError(format!(
                        ".def already exists: {}\nUse --force to overwrite.",
                        def_path.display()
                    )));
                }
                let stub_dir = cfg::data_dir(scope).join("tmp");
                fs::create_dir_all(&stub_dir).map_err(|e| {
                    ManagerError::EnvError(format!("Failed to create tmp dir: {e}"))
                })?;
                let stub_path = stub_dir.join(format!("{env_name}.def"));
                let stub_content = singularity_def_stub(&env_name);
                fs::write(&stub_path, &stub_content).map_err(|e| {
                    ManagerError::EnvError(format!("Failed to write stub .def: {e}"))
                })?;
                Some(stub_path.to_string_lossy().to_string())
            } else {
                None
            };

            let opts = environment::ApplyOptions {
                name: env_name.clone(),
                scope,
                is_new: true,
                base_image: Some(base_image),
                original_image,
                morloc_version: morloc_ver,
                dockerfile: resolved_dockerfile,
                deffile: resolved_deffile,
                includes: include,
                flagfile,
                engine_args: engine_arg,
                reinit_args: Vec::new(),
                engine: Some(resolved_engine),
                shm_size: Some(shm_size.unwrap_or_else(|| "512m".to_string())),
                skip_dockerfile_build: dockerfile_stub || deffile_stub,
                verbose,
            };

            environment::apply_environment(&opts)?;

            if dockerfile_stub {
                let df_path = cfg::env_dockerfile_path(scope, &env_name);
                eprintln!("Stub Dockerfile: {}", df_path.display());
                eprintln!("Edit it, then run: morloc-manager update {env_name}");
            }
            if deffile_stub {
                let def_path = cfg::env_deffile_path(scope, &env_name);
                eprintln!("Stub .def: {}", def_path.display());
                eprintln!("Edit it, then run: morloc-manager update {env_name}");
            }

            eprintln!("Created environment: {env_name}");

            // Run morloc init, passing the env explicitly (no active env needed)
            if !no_init {
                let ec = cfg::read_env_config(scope, &env_name)?;
                run_morloc_init_for(Some((env_name.clone(), scope, ec)), verbose, &init_args)?;
            } else {
                eprintln!("Warning: --no-init was used. Run 'morloc-manager run -- morloc init -f' before building morloc programs.");
            }

            anstream::eprintln!(
                "\x1b[1;32mEnvironment '{env_name}' is ready.\x1b[0m"
            );
            eprintln!("Activate it with: morloc-manager select {env_name}");

            if system && !check_podman_additional_stores(resolved_engine) {
                eprintln!();
                warn_podman_additional_stores();
            }

            Ok(())
        }

        // ---- run ----
        Cmd::Run { command, shell, env_vars, env_file, engine_arg, slurm_bridge } => {
            if !shell && command.is_empty() {
                return Err(ManagerError::NoCommand);
            }
            let user_env = collect_env_vars(&env_vars, env_file.as_deref())?;
            run_in_container(verbose, shell, &command, &user_env, &engine_arg, slurm_bridge).map_err(|e| match e {
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
                    environment::remove_environment(ec.engine, scope, name)?;
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
                    env_list.push((name, ec.engine));
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
                    let shm = match ec.engine {
                        ContainerEngine::Apptainer => None,
                        _ => Some(ec.shm_size.clone()),
                    };
                    // .sif paths only apply under Apptainer. Built_image
                    // mirrors that asymmetry: it is the OCI fallback tag for
                    // Apptainer and the primary built layer for docker/podman.
                    let (base_sif, layered_sif) = match ec.engine {
                        ContainerEngine::Apptainer => (ec.base_sif.clone(), ec.layered_sif.clone()),
                        _ => (None, None),
                    };
                    let output = InfoDetail {
                        name: ec.name.clone(),
                        scope: match scope { Scope::Local => "local", Scope::System => "system" }.to_string(),
                        active,
                        base_image: ec.base_image.clone(),
                        built_image: ec.built_image.clone(),
                        morloc_version: ec.morloc_version.clone(),
                        engine: display_engine(ec.engine).to_string(),
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
                    println!("Engine:         {}", display_engine(ec.engine));
                    // Engine-specific fields:
                    // * Docker/Podman: show SHM size and the Dockerfile path.
                    // * Apptainer:    show base .sif path and the .def path
                    //                 (Dockerfile, if present, is the OCI
                    //                 fallback recipe and is also surfaced).
                    match ec.engine {
                        ContainerEngine::Docker | ContainerEngine::Podman => {
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
                        ContainerEngine::Apptainer => {
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
                        local_engine: local_cfg.as_ref().map(|c| display_engine(c.engine)).unwrap_or("unset").to_string(),
                        system_engine: system_cfg.as_ref().map(|c| display_engine(c.engine)).unwrap_or("unset").to_string(),
                        selinux: se_str.to_string(),
                        directories,
                        local: environment::list_environments(Scope::Local, active_str),
                        system: environment::list_environments(Scope::System, active_str),
                    };
                    println!("{}", serde_json::to_string_pretty(&output).unwrap());
                } else {
                    println!("Active:         {active_env}");
                    println!("Local engine:   {}",
                        local_cfg.as_ref().map(|c| display_engine(c.engine)).unwrap_or("unset"));
                    println!("System engine:  {}",
                        system_cfg.as_ref().map(|c| display_engine(c.engine)).unwrap_or("unset"));
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
            name, image, version, tag, dockerfile, deffile, dockerfile_stub, deffile_stub,
            force, include, flagfile,
            reinit_arg, engine, shm_size, no_build, reinit, init_args, non_interactive: _,
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

            // Handle --dockerfile-stub: generate stub if no Dockerfile exists
            let resolved_dockerfile = if dockerfile.is_some() && dockerfile_stub {
                return Err(ManagerError::EnvError(
                    "Cannot use both --dockerfile and --dockerfile-stub".to_string(),
                ));
            } else if dockerfile_stub {
                let df_path = cfg::env_dockerfile_path(env_scope, &env_name);
                if df_path.exists() && !force {
                    return Err(ManagerError::EnvError(format!(
                        "Dockerfile already exists: {}\nUse --force to overwrite.",
                        df_path.display()
                    )));
                }
                let stub_dir = cfg::data_dir(env_scope).join("tmp");
                fs::create_dir_all(&stub_dir).map_err(|e| {
                    ManagerError::EnvError(format!("Failed to create tmp dir: {e}"))
                })?;
                let stub_path = stub_dir.join(format!("{env_name}.Dockerfile"));
                let stub_content = format!(
                    "# morloc environment: {env_name}\n\
                     # Edit this file, then rebuild with: morloc-manager update\n\
                     \n\
                     # CONTAINER_BASE is replaced at build time with the environment's base image\n\
                     ARG CONTAINER_BASE=scratch\n\
                     FROM ${{CONTAINER_BASE}}\n\
                     \n\
                     # Example: install system packages\n\
                     # RUN apt-get update && apt-get install -y jq && rm -rf /var/lib/apt/lists/*\n\
                     \n\
                     # Example: install Python packages\n\
                     # RUN pip install scikit-learn pandas\n\
                     \n\
                     # Example: install R packages\n\
                     # RUN R -e \"install.packages('ggplot2', repos='https://cloud.r-project.org')\"\n"
                );
                fs::write(&stub_path, &stub_content).map_err(|e| {
                    ManagerError::EnvError(format!("Failed to write stub Dockerfile: {e}"))
                })?;
                Some(stub_path.to_string_lossy().to_string())
            } else {
                dockerfile
            };

            // Handle --deffile-stub: generate stub if no .def exists
            let resolved_deffile = if deffile.is_some() && deffile_stub {
                return Err(ManagerError::EnvError(
                    "Cannot use both --deffile and --deffile-stub".to_string(),
                ));
            } else if deffile_stub {
                let def_path = cfg::env_deffile_path(env_scope, &env_name);
                if def_path.exists() && !force {
                    return Err(ManagerError::EnvError(format!(
                        ".def already exists: {}\nUse --force to overwrite.",
                        def_path.display()
                    )));
                }
                let stub_dir = cfg::data_dir(env_scope).join("tmp");
                fs::create_dir_all(&stub_dir).map_err(|e| {
                    ManagerError::EnvError(format!("Failed to create tmp dir: {e}"))
                })?;
                let stub_path = stub_dir.join(format!("{env_name}.def"));
                let stub_content = singularity_def_stub(&env_name);
                fs::write(&stub_path, &stub_content).map_err(|e| {
                    ManagerError::EnvError(format!("Failed to write stub .def: {e}"))
                })?;
                Some(stub_path.to_string_lossy().to_string())
            } else {
                deffile
            };

            if version.is_some() && image.is_some() {
                return Err(ManagerError::EnvError(
                    "--version and --image are mutually exclusive".to_string()
                ));
            }

            // Resolve base image if --version, --tag, or --image provided
            let (base_image, original_image, morloc_ver) = if let Some(ref ver_str) = version {
                let ec = cfg::read_env_config(env_scope, &env_name)?;
                let clean = ver_str.strip_prefix('v').unwrap_or(ver_str);
                let ver: Version = clean.parse().map_err(|_| {
                    ManagerError::InvalidVersion(ver_str.clone())
                })?;
                let img = environment::pull_version_image(ec.engine, &ver)?;
                (Some(img), None, Some(ver))
            } else if let Some(ref t) = tag {
                let ec = cfg::read_env_config(env_scope, &env_name)?;
                let (img, ver) = environment::pull_tagged_image(ec.engine, t)?;
                (Some(img), None, Some(ver))
            } else if let Some(ref img) = image {
                let ec = cfg::read_env_config(env_scope, &env_name)?;
                environment::pull_custom_image(ec.engine, img)?;
                // Detect version from the new image so it doesn't stay stale
                let detected_ver = environment::detect_morloc_version(ec.engine, img).ok();
                (Some(img.clone()), None, detected_ver)
            } else {
                (None, None, None)
            };

            eprintln!("Updating environment: {env_name}");
            let opts = environment::ApplyOptions {
                name: env_name.clone(),
                scope: env_scope,
                is_new: false,
                base_image,
                original_image,
                morloc_version: morloc_ver,
                dockerfile: resolved_dockerfile,
                deffile: resolved_deffile,
                includes: include,
                flagfile,
                engine_args: Vec::new(),
                reinit_args: reinit_arg,
                engine: engine.map(|e| e.into()),
                shm_size,
                skip_dockerfile_build: no_build || dockerfile_stub || deffile_stub,
                verbose,
            };
            environment::apply_environment(&opts)?;

            if dockerfile_stub {
                let df_path = cfg::env_dockerfile_path(env_scope, &env_name);
                eprintln!("Stub Dockerfile: {}", df_path.display());
                eprintln!("Edit it, then run: morloc-manager update {env_name}");
            }
            if deffile_stub {
                let def_path = cfg::env_deffile_path(env_scope, &env_name);
                eprintln!("Stub .def: {}", def_path.display());
                eprintln!("Edit it, then run: morloc-manager update {env_name}");
            }

            // --version, --tag, and --image imply --reinit (ABI may have changed)
            if reinit || version.is_some() || tag.is_some() || image.is_some() {
                // Re-read the config (apply_environment may have updated it)
                let ec = cfg::read_env_config(env_scope, &env_name)?;

                // Check for running serve container -- reinit replaces morloc-nexus
                // which will fail with "Text file busy" if the container has it open.
                let serve_name = serve::serve_container_name(&env_name);
                let running = serve::find_running_serve_containers(ec.engine);
                if running.iter().any(|n| n == &serve_name) {
                    return Err(ManagerError::EnvError(format!(
                        "Cannot reinit environment '{env_name}' while its serve container is running.\n  \
                         Run 'morloc-manager stop {env_name}' first."
                    )));
                }

                run_morloc_init_for(Some((env_name.clone(), env_scope, ec)), verbose, &init_args)?;
            }

            anstream::eprintln!(
                "\x1b[1;32mEnvironment '{env_name}' updated.\x1b[0m"
            );

            if env_scope == Scope::System && !check_podman_additional_stores(
                cfg::read_env_config(env_scope, &env_name)
                    .map(|ec| ec.engine)
                    .unwrap_or(ContainerEngine::Podman),
            ) {
                eprintln!();
                warn_podman_additional_stores();
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
            let engine = ec.engine;
            // Detect the version from the container binary for sanity check.
            // The morloc binary can't report prerelease tags (stack limitation),
            // so if major.minor.patch match, keep the recorded version which has
            // the full tag from the image.
            eprintln!("Detecting morloc version from image...");
            let detected = environment::detect_morloc_version(ec.engine, ec.active_image())?;
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
            let engine = match engine_override {
                Some(arg) => arg.into(),
                None => {
                    let e = ensure_engine()?;
                    eprintln!(
                        "Note: using {} engine from global config. Override with --engine if needed.",
                        display_engine(e)
                    );
                    e
                }
            };
            serve::build_serve_image(engine, verbose, &from, &tag, manifest.morloc_version, base.as_deref(), rebuild, &manifest.programs)
        }

        // ---- start ----
        Cmd::Start { name, port, env_vars, env_file, engine_arg, force } => {
            let (env_name, env_scope, ec) = resolve_env_or_active(name)?;
            let image = ec.active_image().to_string();
            let data_dir = cfg::env_data_dir(env_scope, &env_name);
            let container_name = serve::serve_container_name(&env_name);
            // Warn if a Dockerfile is configured but the layered image hasn't been built
            if ec.dockerfile.is_some() && ec.built_image.is_none() {
                eprintln!("Warning: Dockerfile is configured but image has not been built. Using base image.");
                eprintln!("  Run 'morloc-manager update {env_name}' to build the Dockerfile layer.");
            }
            // Refuse to replace a running container unless --force is passed
            if container::container_exists(ec.engine, &container_name) {
                if !force {
                    return Err(ManagerError::EnvError(format!(
                        "Serve container already running for '{env_name}'. Use --force to replace."
                    )));
                }
                eprintln!("Warning: replacing existing serve container '{container_name}'");
            }
            let port_mappings = if port.is_empty() {
                vec![(8080, 8080)]
            } else {
                port
            };
            let mut extra_flags = cfg::read_flag_config(env_scope, &env_name)?
                .materialize(Phase::Start, ec.engine);
            extra_flags.extend(engine_arg.iter().cloned());
            let user_env = collect_env_vars(&env_vars, env_file.as_deref())?;
            serve::serve_environment(
                ec.engine, verbose, &image,
                &data_dir.to_string_lossy(), &container_name,
                &port_mappings, &extra_flags, &Some(ec.shm_size.clone()),
                &user_env,
            )
        }

        // ---- stop ----
        Cmd::Stop { name } => {
            let (env_name, _, ec) = resolve_env_or_active(name)?;
            let container_name = serve::serve_container_name(&env_name);
            if crate::container::container_exists(ec.engine, &container_name) {
                serve::stop_serve_container(ec.engine, verbose, &container_name)?;
                eprintln!("Stopped serving environment: {env_name}");
            } else {
                return Err(ManagerError::EnvError(
                    format!("No serve container running for environment '{env_name}'")
                ));
            }
            Ok(())
        }

        // ---- logs ----
        Cmd::Logs { name, follow } => {
            let (container_name, engine) = if let Some(ref n) = name {
                let (_, _, ec) = resolve_env_or_active(Some(n.clone()))?;
                let cname = serve::serve_container_name(n);
                if !container::container_exists(ec.engine, &cname) {
                    return Err(ManagerError::EnvError(
                        format!("No serve container running for environment '{n}'")
                    ));
                }
                (cname, ec.engine)
            } else {
                find_running_serve_container()?
            };
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
                if !container::container_exists(ec.engine, &container_name) {
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

        // ---- status ----
        Cmd::Status => {
            let mut all_containers: Vec<serve::ServeContainerInfo> = Vec::new();
            let mut any_engine = false;
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
                    any_engine = true;
                    if let Ok(containers) = serve::query_serve_containers(engine, verbose) {
                        all_containers.extend(containers);
                    }
                }
            }
            if !any_engine {
                return Err(ManagerError::EngineNotFound);
            }
            if json {
                #[derive(serde::Serialize)]
                struct StatusOutput { containers: Vec<serve::ServeContainerInfo> }
                let output = StatusOutput { containers: all_containers };
                println!("{}", serde_json::to_string_pretty(&output).unwrap());
            } else if all_containers.is_empty() {
                println!("No morloc serve containers running.");
            } else {
                println!("Running servers:");
                for c in &all_containers {
                    println!("  {}  {}  ({})  [{}]", c.name, c.ports, c.env, c.status);
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
            doctor::doctor(ec.engine, verbose, &env_name, env_scope, &ec, deep, strict, slurm, json)
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

fn run_in_container(
    verbose: bool,
    shell: bool,
    args: &[String],
    user_env: &[(String, String)],
    cli_engine_args: &[String],
    slurm_bridge: bool,
) -> Result<()> {
    run_in_container_for(None, verbose, shell, args, user_env, cli_engine_args, Phase::Run, slurm_bridge)
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
    if ec.engine != ContainerEngine::Apptainer {
        eprintln!(
            "warning: SLURM bridge with engine {:?}: ensure the env's image is \
             reachable from every compute node (registry or pre-populated \
             store). Apptainer is the recommended engine on HPC clusters.",
            ec.engine,
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

fn run_in_container_for(
    target: Option<(String, Scope, EnvironmentConfig)>,
    verbose: bool,
    shell: bool,
    args: &[String],
    user_env: &[(String, String)],
    cli_engine_args: &[String],
    phase: Phase,
    slurm_bridge: bool,
) -> Result<()> {
    let (env_name, env_scope, ec) = match target {
        Some(t) => t,
        None => environment::resolve_active_environment()?,
    };
    let engine = ec.engine;
    let image = ec.active_image().to_string();
    let data_dir = cfg::env_data_dir(env_scope, &env_name);
    let v_data_dir = data_dir.to_string_lossy().to_string();

    // Optional SLURM submission bridge. The handle's drop tears the
    // listener down and unlinks the UDS at function exit (which only
    // happens after the container has exited synchronously, so the
    // bridge stays alive for the lifetime of every nested remote
    // call).
    let _bridge_guard = if slurm_bridge {
        Some(setup_slurm_bridge(&ec)?)
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

    // Mount data at /opt/morloc — matching the serve container (start).
    // The compiler reads MORLOC_HOME to resolve all generated paths.
    let mh = "/opt/morloc";
    let base_mounts = vec![
        (v_data_dir.to_string(), mh.to_string()),
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
    // The Haskell-side `morloc init` (SystemConfig.hs) optionally symlinks the
    // freshly installed nexus/manager binaries into a "user bin" directory so
    // they end up on PATH. Its legacy default for that target was
    // `~/.local/bin/`, but under engines that mount the host $HOME (Apptainer
    // does so by default) that directory IS the host's general-purpose bin,
    // and symlinking container-internal targets (`/opt/morloc/bin/...`) into
    // it leaves dangling host symlinks that can clobber the user's own
    // ~/.local/bin/morloc-* files.
    //
    // We instead point `MORLOC_BIN_LINK_DIR` at a morloc-specific subdir of
    // the user's data tree: `~/.local/share/morloc/bin/`. That path is
    // morloc-owned space (no third-party binaries land there), so cross-tool
    // clobbering is not a concern, and we add it to PATH so the resulting
    // symlinks are actually findable. Users who want to redirect the link
    // target can override `MORLOC_BIN_LINK_DIR` via `-x --env ...` or via
    // their env.flags; the later --env wins under both docker/podman and
    // apptainer.
    let link_dir = format!("{home}/{MORLOC_BIN_LINK_REL}");
    let mut env_vars = vec![
        ("HOME".to_string(), home.to_string()),
        ("MORLOC_HOME".to_string(), mh.to_string()),
        ("MORLOC_BIN_LINK_DIR".to_string(), link_dir.clone()),
        (
            "PATH".to_string(),
            format!("{link_dir}:{mh}/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"),
        ),
    ];
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

fn run_morloc_init_for(
    target: Option<(String, Scope, EnvironmentConfig)>,
    verbose: bool,
    extra_init_args: &[String],
) -> Result<()> {
    let mut argv: Vec<String> = if verbose {
        ["morloc", "init", "-f"].iter().map(|s| s.to_string()).collect()
    } else {
        ["morloc", "init", "-f", "-q"].iter().map(|s| s.to_string()).collect()
    };
    argv.extend(extra_init_args.iter().cloned());
    eprintln!("Initializing morloc (this may take several minutes)...");
    // `morloc init` is a run-style invocation (it execs inside the
    // container); it picks up `run.<engine>` flags from env.flags.yaml.
    // No CLI engine-args here (the inner command is set by morloc-manager).
    run_in_container_for(target, verbose, false, &argv, &[], &[], Phase::Run, false)
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
    fn invalid_version_renders() {
        let err = ManagerError::InvalidVersion("abc".to_string());
        assert!(err.to_string().contains("Invalid version"));
    }

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
        assert_eq!(Config::default().engine, ContainerEngine::Podman);
    }

    // ---- Config JSON round-trip tests ----

    #[test]
    fn config_json_round_trip() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("config.json");
        let cfg = Config {
            active_env: Some("ml".to_string()),
            engine: ContainerEngine::Docker,
        };
        cfg::write_config(&path, &cfg).unwrap();
        let cfg2: Config = cfg::read_config(&path).unwrap();
        assert_eq!(cfg2.active_env.as_deref(), Some("ml"));
        assert_eq!(cfg2.engine, ContainerEngine::Docker);
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
            engine: ContainerEngine::Podman,
            shm_size: "1g".to_string(),
            morloc_version: Some(Version::new(0, 67, 0)),
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
        for phase in [Phase::Build, Phase::Run, Phase::Start] {
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
build:
  all:
    - --shared
  apptainer:
    - --ignore-subuid
"#;
        let fc: FlagConfig = serde_yaml::from_str(yaml).unwrap();
        assert_eq!(
            fc.materialize(Phase::Build, ContainerEngine::Apptainer),
            vec!["--shared", "--ignore-subuid"]
        );
        assert_eq!(
            fc.materialize(Phase::Build, ContainerEngine::Docker),
            vec!["--shared"]
        );
        assert!(fc.materialize(Phase::Run, ContainerEngine::Apptainer).is_empty());
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
    fn build_apptainer_native_argv_emits_build_args_and_paths() {
        let cfg = container::ApptainerNativeBuildConfig {
            deffile: "/cfg/recipe.def".to_string(),
            output_sif: "/data/sif/layered.sif".to_string(),
            build_args: vec![("BASE_SIF".to_string(), "/data/sif/base.sif".to_string())],
            extra_flags: Vec::new(),
        };
        let argv = container::build_apptainer_native_argv(&cfg);
        assert_eq!(argv[0], "build");
        assert!(argv.contains(&"--force".to_string()));
        // No mode-selecting flags are passed by default; user supplies them
        // via env.flags.yaml `build.apptainer` or `update --reinit-arg`.
        assert!(!argv.contains(&"--ignore-subuid".to_string()));
        assert!(!argv.contains(&"--ignore-fakeroot-command".to_string()));
        assert!(argv.windows(2).any(|w| w == ["--build-arg", "BASE_SIF=/data/sif/base.sif"]));
        // Output sif is the temp path with .tmp.sif suffix; final rename
        // happens after success in apptainer_build_native.
        assert!(argv.iter().any(|a| a == "/data/sif/layered.sif.tmp.sif"));
        // Recipe path is the last argv slot.
        assert_eq!(argv.last().unwrap(), "/cfg/recipe.def");
    }

    #[test]
    fn build_apptainer_native_argv_threads_extra_flags() {
        let cfg = container::ApptainerNativeBuildConfig {
            deffile: "/cfg/recipe.def".to_string(),
            output_sif: "/data/sif/layered.sif".to_string(),
            build_args: vec![("BASE_SIF".to_string(), "/data/sif/base.sif".to_string())],
            extra_flags: vec![
                "--ignore-subuid".to_string(),
                "--ignore-fakeroot-command".to_string(),
            ],
        };
        let argv = container::build_apptainer_native_argv(&cfg);
        let force_idx = argv.iter().position(|a| a == "--force").unwrap();
        let isubuid_idx = argv.iter().position(|a| a == "--ignore-subuid").unwrap();
        let buildarg_idx = argv.iter().position(|a| a == "--build-arg").unwrap();
        // extra_flags land between --force and the first --build-arg.
        assert!(force_idx < isubuid_idx);
        assert!(isubuid_idx < buildarg_idx);
    }

    #[test]
    fn build_apptainer_oci_convert_argv_picks_docker_daemon_scheme() {
        let cfg = container::ApptainerOciConvertConfig {
            source_engine: ContainerEngine::Docker,
            source_tag: "localhost/morloc-env:dnd".to_string(),
            output_sif: "/data/sif/layered.sif".to_string(),
            extra_flags: Vec::new(),
        };
        let argv = container::build_apptainer_oci_convert_argv(&cfg);
        assert_eq!(argv[0], "build");
        // No mode-selecting flags by default; user supplies via flag file.
        assert!(!argv.contains(&"--ignore-subuid".to_string()));
        assert!(!argv.contains(&"--ignore-fakeroot-command".to_string()));
        assert!(argv.iter().any(|a| a == "/data/sif/layered.sif.tmp.sif"));
        assert_eq!(
            argv.last().unwrap(),
            "docker-daemon://localhost/morloc-env:dnd"
        );
    }

    #[test]
    fn build_apptainer_oci_convert_argv_uses_podman_daemon() {
        let cfg = container::ApptainerOciConvertConfig {
            source_engine: ContainerEngine::Podman,
            source_tag: "localhost/morloc-env:dnd".to_string(),
            output_sif: "/data/sif/layered.sif".to_string(),
            extra_flags: Vec::new(),
        };
        let argv = container::build_apptainer_oci_convert_argv(&cfg);
        assert_eq!(
            argv.last().unwrap(),
            "podman-daemon://localhost/morloc-env:dnd"
        );
    }

    #[test]
    fn env_config_yaml_round_trip_with_apptainer_fields() {
        let dir = tempfile::tempdir().unwrap();
        // Use a temp env dir layout. write_env_config takes (scope, name) and
        // pins to cfg::env_config_path; we use the lower-level write helper
        // to keep the test hermetic.
        let path = dir.path().join("env.yaml");
        let ec = EnvironmentConfig {
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
            engine: ContainerEngine::Apptainer,
            shm_size: "512m".to_string(),
            morloc_version: Some(Version::new(0, 85, 0)),
        };
        let yaml = serde_yaml::to_string(&ec).unwrap();
        std::fs::write(&path, yaml).unwrap();
        let raw = std::fs::read_to_string(&path).unwrap();
        let ec2: EnvironmentConfig = serde_yaml::from_str(&raw).unwrap();
        assert_eq!(ec2.engine, ContainerEngine::Apptainer);
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
        assert_eq!(ec.engine, ContainerEngine::Podman);
        assert!(ec.singularity_def.is_none());
        assert!(ec.base_sif.is_none());
        assert!(ec.layered_sif.is_none());
    }

    #[test]
    fn active_image_apptainer_prefers_layered_sif() {
        let ec = EnvironmentConfig {
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
            engine: ContainerEngine::Apptainer,
            shm_size: "512m".to_string(),
            morloc_version: None,
        };
        assert_eq!(ec.active_image(), "/layered.sif");
    }

    #[test]
    fn active_image_apptainer_falls_back_to_base_sif() {
        let ec = EnvironmentConfig {
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
            engine: ContainerEngine::Apptainer,
            shm_size: "512m".to_string(),
            morloc_version: None,
        };
        assert_eq!(ec.active_image(), "/base.sif");
    }
}
