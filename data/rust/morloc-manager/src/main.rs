mod config;
mod container;
mod environment;
mod error;
mod freeze;
mod selinux;
mod serve;
mod types;
mod version;

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

// ======================================================================
// CLI types
// ======================================================================

fn build_help_template() -> String {
    let b = Style::new().bold().render();
    let bu = Style::new().bold().underline().render();
    let r = "\x1b[0m"; // full ANSI reset — Style::new().render() is a no-op

    format!(
        "\
{{name}} - {{about}}

{{usage-heading}} {{usage}}

{bu}Setup{r}
  {b}setup{r}      Configure engine for a scope
  {b}install{r}    Install a morloc version
  {b}uninstall{r}  Remove a version
  {b}select{r}     Set the active version or workspace
  {b}info{r}       Show configuration and installed versions

{bu}Development{r}
  {b}new{r}        Create a named workspace
  {b}run{r}        Run a command in the active container
  {b}env{r}        Manage dependency environments

{bu}Deployment{r}
  {b}start{r}      Start a serve container
  {b}stop{r}       Stop a running serve container
  {b}freeze{r}     Export installed state as a frozen artifact
  {b}unfreeze{r}   Build a serve image from frozen state
  {b}status{r}     List running serve containers
  {b}logs{r}       Show logs from a serve container

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
struct Cli {
    /// Print container commands to stderr before executing
    #[arg(short, long, global = true)]
    verbose: bool,

    /// Print version and exit
    #[arg(long)]
    version: bool,

    #[command(subcommand)]
    command: Option<Cmd>,
}

#[derive(Subcommand)]
enum Cmd {
    // -- Setup --
    /// Configure engine for a scope
    #[command(display_order = 1)]
    Setup {
        /// Apply to system scope instead of local (requires root)
        #[arg(long)]
        system: bool,
        /// Container engine: "podman" or "docker"
        #[arg(long, value_enum)]
        engine: Option<EngineArg>,
    },
    /// Install a morloc version
    #[command(display_order = 2)]
    Install {
        /// Version to install (default: latest)
        version: Option<String>,
        /// Install system-wide instead of locally (requires root)
        #[arg(long)]
        system: bool,
        /// Skip morloc init after install
        #[arg(long)]
        no_init: bool,
        /// Force re-install even if version is already installed
        #[arg(short, long)]
        force: bool,
    },
    /// Remove a version
    #[command(display_order = 3)]
    Uninstall {
        /// Version(s) to remove
        versions: Vec<String>,
        /// Uninstall from system scope instead of local (requires root)
        #[arg(long)]
        system: bool,
        /// Remove all installed versions
        #[arg(short, long)]
        all: bool,
    },
    /// Set the active version or workspace
    #[command(display_order = 4)]
    Select {
        /// Version or workspace name
        target: String,
        /// Set the system-wide default instead of the user selection
        #[arg(long)]
        system: bool,
    },
    /// Show configuration and installed versions
    #[command(display_order = 5)]
    Info,

    // -- Development --
    /// Create a named workspace
    #[command(display_order = 10)]
    New {
        /// Workspace name
        name: String,
        /// Create in system scope instead of local (requires root)
        #[arg(long)]
        system: bool,
        /// Base version (default: currently active version)
        #[arg(long)]
        from: Option<String>,
        /// Copy lib/fdb/bin from base version
        #[arg(long)]
        copy: bool,
    },
    /// Run a command in the active container
    #[command(display_order = 11)]
    Run {
        /// Command to run inside the container
        command: Vec<String>,
        /// Start an interactive shell
        #[arg(long)]
        shell: bool,
    },
    /// Manage dependency environments
    #[command(display_order = 12)]
    Env {
        #[command(subcommand)]
        action: Option<EnvCmd>,
        /// Activate (and build if needed) an environment
        name: Option<String>,
    },

    // -- Deployment --
    /// Start a serve container
    #[command(display_order = 20)]
    Start {
        /// Serve image tag
        image: String,
        /// Port mapping (e.g., 8080:8080)
        #[arg(short, long, value_parser = parse_port)]
        port: Vec<(u16, u16)>,
    },
    /// Stop a running serve container
    #[command(display_order = 21)]
    Stop {
        /// Container name
        name: String,
    },
    /// Export installed state as a frozen artifact
    #[command(display_order = 22)]
    Freeze {
        /// Output directory
        #[arg(short, long)]
        output: Option<String>,
    },
    /// Build a serve image from frozen state
    #[command(display_order = 23)]
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
    },
    /// List running serve containers
    #[command(display_order = 24)]
    Status,
    /// Show logs from a serve container
    #[command(display_order = 25)]
    Logs {
        /// Container name
        name: String,
        /// Follow log output
        #[arg(short, long)]
        follow: bool,
    },
}

#[derive(Subcommand)]
enum EnvCmd {
    /// Create a new environment Dockerfile
    Init { name: String },
    /// List available environments
    List,
    /// Reset to base environment
    Reset,
}

#[derive(Clone, ValueEnum)]
enum EngineArg {
    Docker,
    Podman,
}

impl From<EngineArg> for ContainerEngine {
    fn from(e: EngineArg) -> Self {
        match e {
            EngineArg::Docker => ContainerEngine::Docker,
            EngineArg::Podman => ContainerEngine::Podman,
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

// ======================================================================
// Main
// ======================================================================

fn main() -> ExitCode {
    // Restore default SIGPIPE handling so piped output (e.g. `info | head`)
    // exits cleanly instead of causing a Rust panic
    #[cfg(unix)]
    {
        use nix::sys::signal::{signal, SigHandler, Signal};
        unsafe { let _ = signal(Signal::SIGPIPE, SigHandler::SigDfl); }
    }

    let matches = Cli::command()
        .help_template(build_help_template())
        .get_matches();
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
    match dispatch(cli.verbose, cmd) {
        Ok(()) => ExitCode::SUCCESS,
        Err(err) => {
            eprintln!("{err}");
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

fn resolve_active_version_or_workspace() -> Result<(Version, Scope)> {
    match version::resolve_active_version() {
        Ok(v) => Ok(v),
        Err(ManagerError::NoActiveVersion) => {
            let target = version::resolve_active_target()?;
            if let ActiveTarget::Workspace(name) = target {
                let ws_scope = cfg::find_workspace_scope(&name)?;
                let wc = cfg::read_workspace_config(ws_scope, &name)?;
                let ver_scope = cfg::find_installed_scope(wc.base_version)?;
                Ok((wc.base_version, ver_scope))
            } else {
                Err(ManagerError::NoActiveVersion)
            }
        }
        Err(e) => Err(e),
    }
}

fn ensure_engine() -> Result<ContainerEngine> {
    if let Some(cfg) = cfg::read_active_config() {
        return Ok(cfg.engine);
    }
    if io::stdin().is_terminal() {
        eprintln!("No configuration found. Let's set up.\n");
        run_interactive_setup(Scope::Local)?;
        if let Some(cfg) = cfg::read_active_config() {
            return Ok(cfg.engine);
        }
    }
    Err(ManagerError::SetupNotComplete(Scope::Local))
}

fn ensure_engine_for_scope(scope: Scope) -> Result<ContainerEngine> {
    match cfg::require_scope_config(scope) {
        Ok(cfg) => Ok(cfg.engine),
        Err(ManagerError::SetupNotComplete(s)) => {
            if io::stdin().is_terminal() {
                let scope_str = match s {
                    Scope::Local => "local",
                    Scope::System => "system",
                };
                eprintln!("No {scope_str} configuration found. Let's set up.\n");
                run_interactive_setup(s)?;
                let cfg = cfg::require_scope_config(s)?;
                Ok(cfg.engine)
            } else {
                Err(ManagerError::SetupNotComplete(s))
            }
        }
        Err(e) => Err(e),
    }
}

fn run_interactive_setup(scope: Scope) -> Result<()> {
    let engine = interactive_engine_choice()?;
    let cfg_path = cfg::config_path(scope);
    let base_cfg = cfg::read_config::<Config>(&cfg_path).unwrap_or_default();
    let new_cfg = Config {
        engine,
        ..base_cfg
    };
    cfg::write_config(&cfg_path, &new_cfg)?;
    eprintln!("  Engine: {}", display_engine(engine));
    eprintln!("  Config: {}", cfg_path.display());
    eprintln!();
    Ok(())
}

fn run_non_interactive_setup(scope: Scope, engine: ContainerEngine) -> Result<()> {
    let cfg_path = cfg::config_path(scope);
    let base_cfg = cfg::read_config::<Config>(&cfg_path).unwrap_or_default();
    let new_cfg = Config {
        engine,
        ..base_cfg
    };
    cfg::write_config(&cfg_path, &new_cfg)?;
    eprintln!("  Engine: {}", display_engine(engine));
    eprintln!("  Config: {}", cfg_path.display());
    eprintln!();
    Ok(())
}

fn show_setup_status() -> Result<()> {
    // Show detected engines
    let has_podman = which("podman");
    let has_docker = which("docker");
    let detected: Vec<&str> = [
        if has_podman { Some("podman") } else { None },
        if has_docker { Some("docker") } else { None },
    ]
    .into_iter()
    .flatten()
    .collect();
    if detected.is_empty() {
        eprintln!("Detected engines: none");
    } else {
        eprintln!("Detected engines: {}", detected.join(", "));
    }
    eprintln!();

    // Show local scope
    let local_path = cfg::config_path(Scope::Local);
    eprintln!("(local)");
    match cfg::read_config::<Config>(&local_path) {
        Ok(c) => {
            eprintln!("    Set engine: {}", display_engine(c.engine));
            eprintln!("    Config: {}", local_path.display());
        }
        Err(_) => {
            eprintln!("    Set engine: unset");
        }
    }
    eprintln!();

    // Show system scope
    let system_path = cfg::config_path(Scope::System);
    eprintln!("(system)");
    match cfg::read_config::<Config>(&system_path) {
        Ok(c) => {
            eprintln!("    Set engine: {}", display_engine(c.engine));
            eprintln!("    Config: {}", system_path.display());
        }
        Err(_) => {
            eprintln!("    Set engine: unset");
        }
    }
    eprintln!();

    Ok(())
}

fn interactive_engine_choice() -> Result<ContainerEngine> {
    let has_podman = which("podman");
    let has_docker = which("docker");
    match (has_podman, has_docker) {
        (false, false) => Err(ManagerError::EngineNotFound),
        (true, false) => {
            eprintln!("Detected: podman");
            Ok(ContainerEngine::Podman)
        }
        (false, true) => {
            eprintln!("Detected: docker");
            Ok(ContainerEngine::Docker)
        }
        (true, true) => {
            eprintln!("Both podman and docker detected.");
            eprintln!("  [1] podman (recommended)");
            eprintln!("  [2] docker");
            eprint!("Choose [1]: ");
            io::stderr().flush().ok();
            let mut input = String::new();
            match io::stdin().read_line(&mut input) {
                Ok(_) => {
                    if input.trim() == "2" {
                        Ok(ContainerEngine::Docker)
                    } else {
                        Ok(ContainerEngine::Podman)
                    }
                }
                Err(_) => {
                    eprintln!("\nNon-interactive input detected, defaulting to podman.");
                    Ok(ContainerEngine::Podman)
                }
            }
        }
    }
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
    }
}

// ======================================================================
// Dispatch
// ======================================================================

fn dispatch(verbose: bool, cmd: Cmd) -> Result<()> {
    match cmd {
        // ---- setup ----
        Cmd::Setup { system, engine } => {
            let scope = resolve_scope(system);
            match engine {
                // No engine specified: show status (if no --system) or interactive choice
                None if !system => show_setup_status(),
                None => run_interactive_setup(scope),
                Some(e) => run_non_interactive_setup(scope, e.into()),
            }
        }

        // ---- install ----
        Cmd::Install {
            version: ver_str,
            system,
            no_init,
            force,
        } => {
            let scope = resolve_scope(system);
            let engine = ensure_engine_for_scope(scope)?;
            let (ver, was_fresh) = match ver_str.as_deref() {
                None | Some("latest") => version::install_latest(engine, scope)?,
                Some(s) => {
                    let ver: Version = s.parse().map_err(|_| ManagerError::InvalidVersion(s.to_string()))?;
                    let fresh = version::install_version_force(force, engine, scope, ver)?;
                    (ver, fresh)
                }
            };
            // Auto-select if none active (always writes to local config)
            if version::resolve_active_version().is_err() {
                let _ = version::select_version(ver);
                eprintln!("Auto-selected version {}", ver.show());
            }
            if no_init {
                Ok(())
            } else if was_fresh || force {
                run_morloc_init(engine, verbose)
            } else {
                Ok(())
            }
        }

        // ---- select ----
        Cmd::Select { target, system } => {
            if let Ok(ver) = target.parse::<Version>() {
                if system {
                    version::select_version_system(ver)?;
                    eprintln!("Set system default to version {}", ver.show());
                } else {
                    version::select_version(ver)?;
                    eprintln!("Selected version {}", ver.show());
                }
                Ok(())
            } else {
                // Treat as workspace name
                let ws_scope = cfg::find_workspace_scope(&target).map_err(|_| {
                    ManagerError::InvalidVersion(format!(
                        "Not found as version or workspace: {target}"
                    ))
                })?;
                cfg::read_workspace_config(ws_scope, &target).map_err(|_| {
                    ManagerError::InvalidVersion(format!(
                        "Not found as version or workspace: {target}"
                    ))
                })?;
                let write_scope = if system { Scope::System } else { Scope::Local };
                let cfg_path = cfg::config_path(write_scope);
                let base = cfg::read_config::<Config>(&cfg_path).unwrap_or_default();
                let new_cfg = Config {
                    active_target: Some(ActiveTarget::Workspace(target.clone())),
                    ..base
                };
                cfg::write_config(&cfg_path, &new_cfg)?;
                if system {
                    eprintln!("Set system default to workspace {target}");
                } else {
                    eprintln!("Selected workspace {target}");
                }
                Ok(())
            }
        }

        // ---- uninstall ----
        Cmd::Uninstall {
            versions,
            system,
            all,
        } => {
            if !all && versions.is_empty() {
                return Err(ManagerError::UninstallError(
                    "No versions specified. Use VERSION... or --all.".to_string(),
                ));
            }
            let scope = resolve_scope(system);
            let vers: Vec<Version> = if all {
                version::list_versions(scope)
            } else {
                versions
                    .iter()
                    .map(|s| s.parse::<Version>())
                    .collect::<std::result::Result<Vec<_>, _>>()
                    .map_err(|_| ManagerError::InvalidVersion(versions.join(" ")))?
            };
            for ver in &vers {
                eprintln!("Uninstalling {}...", ver.show());
                version::uninstall_version(scope, *ver)?;
            }
            eprintln!("Uninstalled {} version(s)", vers.len());
            Ok(())
        }

        // ---- run ----
        Cmd::Run { command, shell } => {
            let engine = ensure_engine()?;
            if !shell && command.is_empty() {
                return Err(ManagerError::NoCommand);
            }
            run_in_container(engine, verbose, shell, &command)
        }

        // ---- env ----
        Cmd::Env { action, name } => {
            // Env Dockerfiles and configs are always user-local, even when
            // the active version comes from a system install
            let scope = Scope::Local;
            let m_cfg = cfg::read_active_config();

            match (action, name) {
                (Some(EnvCmd::Init { name }), _) => {
                    let path = environment::init_environment(scope, &name)?;
                    eprintln!("Created environment Dockerfile: {path}");
                    eprintln!("Edit the Dockerfile, then run: morloc-manager env <name>");
                    Ok(())
                }
                (Some(EnvCmd::List), _) => {
                    let (ver, _) = resolve_active_version_or_workspace()?;
                    let envs = environment::list_environments(scope, ver);
                    let active_env = m_cfg.as_ref()
                        .map(|c| c.active_env.as_str())
                        .unwrap_or("base");
                    for e in envs {
                        if e == active_env {
                            println!("{e} (active)");
                        } else {
                            println!("{e}");
                        }
                    }
                    Ok(())
                }
                (Some(EnvCmd::Reset), _) => {
                    if let Some(cfg) = m_cfg {
                        let cfg_path = cfg::config_path(scope);
                        let new_cfg = Config {
                            active_env: "base".to_string(),
                            ..cfg
                        };
                        cfg::write_config(&cfg_path, &new_cfg)?;
                    }
                    eprintln!("Reset to base environment");
                    Ok(())
                }
                (None, Some(name)) => {
                    // Common mistake detection
                    match name.as_str() {
                        "list" | "ls" => {
                            return Err(ManagerError::EnvError(
                                "Unknown argument. Did you mean: morloc-manager env list".to_string(),
                            ));
                        }
                        "reset" => {
                            return Err(ManagerError::EnvError(
                                "Unknown argument. Did you mean: morloc-manager env reset".to_string(),
                            ));
                        }
                        "init" => {
                            return Err(ManagerError::EnvError(
                                "Unknown argument. Did you mean: morloc-manager env init <NAME>"
                                    .to_string(),
                            ));
                        }
                        _ => {}
                    }

                    let engine = ensure_engine()?;
                    let (ver, _) = resolve_active_version_or_workspace()?;
                    environment::build_environment(engine, scope, ver, &name)?;
                    environment::activate_environment(scope, ver, &name)?;
                    eprintln!("Activated environment: {name}");
                    Ok(())
                }
                (None, None) => {
                    // No subcommand and no name - show list
                    let (ver, _) = resolve_active_version_or_workspace()?;
                    let envs = environment::list_environments(scope, ver);
                    let active_env = m_cfg.as_ref()
                        .map(|c| c.active_env.as_str())
                        .unwrap_or("base");
                    for e in envs {
                        if e == active_env {
                            println!("{e} (active)");
                        } else {
                            println!("{e}");
                        }
                    }
                    Ok(())
                }
            }
        }

        // ---- new ----
        Cmd::New {
            name,
            system,
            from,
            copy,
        } => {
            let scope = resolve_scope(system);
            let engine = ensure_engine_for_scope(scope)?;
            let (base_ver, base_ver_scope) = match from {
                Some(ver_str) => {
                    let ver: Version = ver_str
                        .parse()
                        .map_err(|_| ManagerError::InvalidVersion(ver_str))?;
                    let found_scope = cfg::find_installed_scope(ver)?;
                    (ver, found_scope)
                }
                None => resolve_active_version_or_workspace()?,
            };
            let ws_data_dir = cfg::workspace_data_dir(scope, &name);
            if ws_data_dir.is_dir() {
                return Err(ManagerError::WorkspaceError(format!(
                    "Workspace already exists: {name}"
                )));
            }
            for sub in &["bin", "lib", "fdb", "include", "opt", "tmp"] {
                fs::create_dir_all(ws_data_dir.join(sub)).map_err(|e| {
                    ManagerError::InstallError(format!("Failed to create directory: {e}"))
                })?;
            }
            if copy {
                let src_dir = cfg::version_data_dir(base_ver_scope, base_ver);
                for sub in &["lib", "fdb", "bin"] {
                    let src = src_dir.join(sub);
                    let dst = ws_data_dir.join(sub);
                    if src.is_dir() {
                        let _ = Command::new("cp")
                            .args(["-a", &format!("{}/.",&src.display()), &dst.to_string_lossy()])
                            .output();
                    }
                }
                eprintln!("Copied state from {}", base_ver.show());
            }
            let wc = WorkspaceConfig {
                base_version: base_ver,
                engine,
            };
            cfg::write_workspace_config(scope, &name, &wc)?;
            // Always write selection to local config
            let cfg_path = cfg::config_path(Scope::Local);
            let base = cfg::read_config::<Config>(&cfg_path).unwrap_or_default();
            let new_cfg = Config {
                active_target: Some(ActiveTarget::Workspace(name.clone())),
                ..base
            };
            cfg::write_config(&cfg_path, &new_cfg)?;
            eprintln!("Created workspace: {name}");
            eprintln!("Based on version {}", base_ver.show());
            run_morloc_init(engine, verbose)?;
            eprintln!("Activated workspace: {name}");
            Ok(())
        }

        // ---- info ----
        Cmd::Info => {
            let local_cfg = cfg::read_config::<Config>(&cfg::config_path(Scope::Local)).ok();
            let system_cfg = cfg::read_config::<Config>(&cfg::config_path(Scope::System)).ok();
            let se_mode = detect_selinux();

            // Resolve active target (local overrides system)
            let active_target = version::resolve_active_target().ok();

            // Read active env from whichever config provides the target
            let active_env = local_cfg
                .as_ref()
                .filter(|c| c.active_target.is_some())
                .or(system_cfg.as_ref().filter(|c| c.active_target.is_some()))
                .map(|c| c.active_env.as_str())
                .unwrap_or("base");

            let active_str = match &active_target {
                None => "none".to_string(),
                Some(ActiveTarget::Version(v)) => v.show(),
                Some(ActiveTarget::Workspace(w)) => format!("{w} (workspace)"),
            };

            // Local selection vs system default
            let local_target = local_cfg.as_ref().and_then(|c| c.active_target.clone());
            let system_target = system_cfg.as_ref().and_then(|c| c.active_target.clone());

            let se_str = match se_mode {
                SELinuxMode::Enforcing => "enforcing",
                SELinuxMode::Permissive => "permissive",
                SELinuxMode::Disabled => "not detected",
            };

            println!("Active:         {active_str}");
            println!("Active env:     {active_env}");
            println!("Local engine:   {}",
                local_cfg.as_ref().map(|c| display_engine(c.engine)).unwrap_or("unset"));
            println!("System engine:  {}",
                system_cfg.as_ref().map(|c| display_engine(c.engine)).unwrap_or("unset"));
            println!("SELinux:        {se_str}");

            // Show all morloc directories with existence status
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

            // List versions
            let local_versions = version::list_versions(Scope::Local);
            println!("\nLocal versions:");
            if local_versions.is_empty() {
                println!("  (none)");
            } else {
                for v in &local_versions {
                    let mut markers = Vec::new();
                    if local_target == Some(ActiveTarget::Version(*v)) {
                        markers.push("active");
                    }
                    if system_target == Some(ActiveTarget::Version(*v)) {
                        markers.push("system default");
                    }
                    let marker = if markers.is_empty() {
                        String::new()
                    } else {
                        format!(" ({})", markers.join(", "))
                    };
                    println!("  {}{marker}", v.show());
                }
            }

            let system_versions = version::list_versions(Scope::System);
            println!("\nSystem versions:");
            if system_versions.is_empty() {
                println!("  (none)");
            } else {
                for v in &system_versions {
                    let mut markers = Vec::new();
                    if local_target == Some(ActiveTarget::Version(*v)) {
                        markers.push("active");
                    }
                    if system_target == Some(ActiveTarget::Version(*v)) {
                        markers.push("system default");
                    }
                    let marker = if markers.is_empty() {
                        String::new()
                    } else {
                        format!(" ({})", markers.join(", "))
                    };
                    println!("  {}{marker}", v.show());
                }
            }

            // List workspaces
            let local_workspaces = cfg::list_workspaces(Scope::Local);
            println!("\nLocal workspaces:");
            if local_workspaces.is_empty() {
                println!("  (none)");
            } else {
                for w in &local_workspaces {
                    let base_str = match cfg::read_workspace_config(Scope::Local, w) {
                        Ok(wc) => format!(" (based on {})", wc.base_version.show()),
                        Err(_) => String::new(),
                    };
                    let mut markers = Vec::new();
                    if local_target == Some(ActiveTarget::Workspace(w.clone())) {
                        markers.push("active");
                    }
                    if system_target == Some(ActiveTarget::Workspace(w.clone())) {
                        markers.push("system default");
                    }
                    let marker = if markers.is_empty() {
                        String::new()
                    } else {
                        format!(" ({})", markers.join(", "))
                    };
                    println!("  {w}{base_str}{marker}");
                }
            }

            let system_workspaces = cfg::list_workspaces(Scope::System);
            if !system_workspaces.is_empty() {
                println!("\nSystem workspaces:");
                for w in &system_workspaces {
                    let base_str = match cfg::read_workspace_config(Scope::System, w) {
                        Ok(wc) => format!(" (based on {})", wc.base_version.show()),
                        Err(_) => String::new(),
                    };
                    let mut markers = Vec::new();
                    if local_target == Some(ActiveTarget::Workspace(w.clone())) {
                        markers.push("active");
                    }
                    if system_target == Some(ActiveTarget::Workspace(w.clone())) {
                        markers.push("system default");
                    }
                    let marker = if markers.is_empty() {
                        String::new()
                    } else {
                        format!(" ({})", markers.join(", "))
                    };
                    println!("  {w}{base_str}{marker}");
                }
            }
            Ok(())
        }

        // ---- freeze ----
        Cmd::Freeze { output } => {
            let output_dir = output.as_deref().unwrap_or("./morloc-freeze");
            let target = version::resolve_active_target()?;
            match target {
                ActiveTarget::Version(ver) => {
                    let scope = cfg::find_installed_scope(ver)?;
                    freeze::freeze(scope, ver, output_dir)
                }
                ActiveTarget::Workspace(name) => {
                    let ws_scope = cfg::find_workspace_scope(&name)?;
                    let wc = cfg::read_workspace_config(ws_scope, &name)?;
                    let ws_dir = cfg::workspace_data_dir(ws_scope, &name);
                    let ver_scope = cfg::find_installed_scope(wc.base_version)?;
                    freeze::freeze_from_dir(
                        ver_scope,
                        wc.base_version,
                        &ws_dir.to_string_lossy(),
                        output_dir,
                    )
                }
            }
        }

        // ---- unfreeze ----
        Cmd::Unfreeze { from, tag, base } => {
            let engine = ensure_engine()?;
            let target = version::resolve_active_target()?;
            let ver = match target {
                ActiveTarget::Version(v) => v,
                ActiveTarget::Workspace(_) => Version::new(0, 0, 0),
            };
            serve::build_serve_image(engine, &from, &tag, ver, base.as_deref())
        }

        // ---- start ----
        Cmd::Start { image, port } => {
            let engine = ensure_engine()?;
            let container_name = format!("morloc-serve-{}", image.replace(':', "-"));
            let port_mappings = if port.is_empty() {
                vec![(8080, 8080)]
            } else {
                port
            };
            serve::run_serve_container(engine, verbose, &image, &container_name, &port_mappings)
        }

        // ---- stop ----
        Cmd::Stop { name } => {
            let engine = ensure_engine()?;
            serve::stop_serve_container(engine, &name)?;
            eprintln!("Stopped container {name}");
            Ok(())
        }

        // ---- status ----
        Cmd::Status => {
            let engine = ensure_engine()?;
            serve::list_serve_containers(engine)
        }

        // ---- logs ----
        Cmd::Logs { name, follow } => {
            let engine = ensure_engine()?;
            serve::stream_serve_logs(engine, &name, follow)
        }
    }
}

// ======================================================================
// Container run
// ======================================================================

fn run_in_container(
    engine: ContainerEngine,
    verbose: bool,
    shell: bool,
    args: &[String],
) -> Result<()> {
    let target = version::resolve_active_target()?;

    // Resolve version config and data dir
    let (vc, v_data_dir, ver_scope, ver) = match target {
        ActiveTarget::Version(ver) => {
            let scope = cfg::find_installed_scope(ver)?;
            let vc = cfg::read_version_config(scope, ver)?;
            let d = cfg::version_data_dir(scope, ver);
            (vc, d.to_string_lossy().to_string(), scope, ver)
        }
        ActiveTarget::Workspace(name) => {
            let ws_scope = cfg::find_workspace_scope(&name)?;
            let wc = cfg::read_workspace_config(ws_scope, &name)?;
            let base_scope = cfg::find_installed_scope(wc.base_version)?;
            let vc = cfg::read_version_config(base_scope, wc.base_version)?;
            let d = cfg::workspace_data_dir(ws_scope, &name);
            (
                vc,
                d.to_string_lossy().to_string(),
                base_scope,
                wc.base_version,
            )
        }
    };

    let m_cfg = cfg::read_active_config();
    let image = resolve_image(ver_scope, ver, &vc, m_cfg.as_ref());
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
    let env_name = m_cfg
        .as_ref()
        .map(|c| c.active_env.as_str())
        .unwrap_or("base");

    let global_fp = cfg::global_flags_path(ver_scope);
    let global_flags = cfg::read_flags_file(&global_fp);
    let env_flags = if env_name == "base" {
        Vec::new()
    } else {
        let efp = cfg::env_flags_path(ver_scope, ver, env_name);
        cfg::read_flags_file(&efp)
    };
    let extra_flags: Vec<String> = global_flags.into_iter().chain(env_flags).collect();

    let is_init = matches!(args, [a, b, ..] if a == "morloc" && b == "init");

    let is_home_dir = normalize_trailing(&cwd) == normalize_trailing(&home);

    if !is_init && !suffix.is_empty() && !is_home_dir {
        selinux::validate_mount_path(&cwd)?;
        run_with_config(
            engine,
            verbose,
            &image,
            &v_data_dir,
            &home,
            &cwd,
            suffix,
            shell,
            args,
            false,
            &vc.shm_size,
            &extra_flags,
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
            engine,
            verbose,
            &image,
            &v_data_dir,
            &home,
            &cwd_final,
            suffix,
            shell,
            args,
            is_init || skip_work_mount,
            &vc.shm_size,
            &extra_flags,
        )
    }
}

fn resolve_image(
    scope: Scope,
    ver: Version,
    vc: &VersionConfig,
    m_cfg: Option<&Config>,
) -> String {
    let base_image = &vc.image;
    match m_cfg {
        Some(cfg) if cfg.active_env != "base" => {
            match cfg::read_environment_config(scope, ver, &cfg.active_env) {
                Ok(ec) => ec.image,
                Err(_) => {
                    eprintln!(
                        "Warning: Active environment '{}' is not built for version {}. Using base image.",
                        cfg.active_env, ver.show()
                    );
                    eprintln!(
                        "  Run 'morloc-manager env {}' to rebuild for {}.",
                        cfg.active_env, ver.show()
                    );
                    base_image.clone()
                }
            }
        }
        _ => base_image.clone(),
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
) -> Result<()> {
    if shell {
        if !io::stdin().is_terminal() || !io::stdout().is_terminal() {
            eprintln!("Error: --shell requires an interactive terminal (TTY).");
            eprintln!("If connecting over SSH, use: ssh -t <host> morloc-manager run --shell");
            std::process::exit(1);
        }
    }

    let container_home = home;
    let base_mounts = vec![
        (
            v_data_dir.to_string(),
            format!("{container_home}/.local/share/morloc"),
        ),
        (
            format!("{v_data_dir}/bin"),
            format!("{container_home}/.local/bin"),
        ),
    ];
    let work_mount = if is_init {
        Vec::new()
    } else {
        vec![(cwd.to_string(), cwd.to_string())]
    };
    let all_mounts: Vec<(String, String)> = base_mounts.into_iter().chain(work_mount).collect();
    let work_dir = if is_init {
        container_home.to_string()
    } else {
        cwd.to_string()
    };
    let env_vars = vec![
        ("HOME".to_string(), container_home.to_string()),
        (
            "PATH".to_string(),
            format!("{container_home}/.local/bin:{container_home}/.local/share/morloc/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"),
        ),
    ];
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

    let status = container_run_passthrough(engine, verbose, &cfg);
    if status.success() {
        Ok(())
    } else {
        Err(ManagerError::EngineError {
            engine,
            code: status.code().unwrap_or(1),
            stderr: "Container exited with error".to_string(),
        })
    }
}

fn run_morloc_init(engine: ContainerEngine, verbose: bool) -> Result<()> {
    let init_args: Vec<String> = if verbose {
        ["morloc", "init", "-f"].iter().map(|s| s.to_string()).collect()
    } else {
        ["morloc", "init", "-f", "-q"].iter().map(|s| s.to_string()).collect()
    };
    eprintln!("Initializing morloc...");
    run_in_container(engine, verbose, false, &init_args)
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
    fn no_active_version_suggests_select() {
        let err = ManagerError::NoActiveVersion;
        assert!(err.to_string().contains("select"));
    }

    #[test]
    fn version_not_installed_suggests_info() {
        let err = ManagerError::VersionNotInstalled(Version::new(0, 99, 0));
        assert!(err.to_string().contains("info"));
    }

    #[test]
    fn config_permission_denied_mentions_permissions() {
        let err = ManagerError::ConfigPermissionDenied("/etc/morloc/config.json".to_string());
        assert!(err.to_string().contains("Permission"));
    }

    #[test]
    fn install_error_renders() {
        let err = ManagerError::InstallError("test error".to_string());
        assert!(err.to_string().contains("Install failed"));
    }

    #[test]
    fn freeze_error_renders() {
        let err = ManagerError::FreezeError("tar error".to_string());
        assert!(err.to_string().contains("Freeze failed"));
    }

    // ---- Config default tests ----

    #[test]
    fn default_config_has_no_active_version() {
        assert_eq!(Config::default().active_version(), None);
    }

    #[test]
    fn default_config_uses_podman() {
        assert_eq!(Config::default().engine, ContainerEngine::Podman);
    }

    #[test]
    fn default_config_uses_base_env() {
        assert_eq!(Config::default().active_env, "base");
    }

    // ---- Config JSON round-trip tests ----

    #[test]
    fn config_json_round_trip() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("config.json");
        let cfg = Config {
            active_target: Some(ActiveTarget::Version(Version::new(0, 67, 0))),
            active_env: "ml".to_string(),
            engine: ContainerEngine::Docker,
        };
        cfg::write_config(&path, &cfg).unwrap();
        let cfg2: Config = cfg::read_config(&path).unwrap();
        assert_eq!(cfg2.active_version(), Some(Version::new(0, 67, 0)));
        assert_eq!(cfg2.active_env, "ml");
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
    fn version_config_json_round_trip() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("vc.json");
        let vc = VersionConfig {
            image: "ghcr.io/morloc-project/morloc/morloc-full:0.67.0".to_string(),
            original_image: Some("ghcr.io/morloc-project/morloc/morloc-full:edge".to_string()),
            host_dir: "/home/user/.local/share/morloc/versions/0.67.0".to_string(),
            shm_size: "1g".to_string(),
            engine: ContainerEngine::Podman,
        };
        cfg::write_config(&path, &vc).unwrap();
        let vc2: VersionConfig = cfg::read_config(&path).unwrap();
        assert_eq!(
            vc2.image,
            "ghcr.io/morloc-project/morloc/morloc-full:0.67.0"
        );
        assert_eq!(vc2.shm_size, "1g");
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
                image_digest: None,
            }),
        };
        cfg::write_config(&path, &fm).unwrap();
        let fm2: FreezeManifest = cfg::read_config(&path).unwrap();
        assert_eq!(fm2.morloc_version, Version::new(0, 67, 0));
        assert_eq!(fm2.modules.len(), 1);
        assert_eq!(fm2.programs.len(), 1);
        assert_eq!(fm2.programs[0].commands, vec!["hello", "compute"]);
    }

    // ---- Config flags tests ----

    #[test]
    fn read_flags_file_parses() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("test.flags");
        fs::write(
            &path,
            "# This is a comment\n--gpus all\n\n  -v /data:/data  \n# another comment\n--network host\n",
        )
        .unwrap();
        let flags = cfg::read_flags_file(&path);
        assert_eq!(flags, vec!["--gpus all", "-v /data:/data", "--network host"]);
    }

    #[test]
    fn read_flags_file_missing() {
        let dir = tempfile::tempdir().unwrap();
        let flags = cfg::read_flags_file(&dir.path().join("nope.flags"));
        assert!(flags.is_empty());
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
        assert!(args.contains(&"-it".to_string()));
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
        };
        let args = build_build_args(&cfg);
        assert_eq!(args[0], "build");
        assert!(args.contains(&"-f".to_string()));
        assert!(args.contains(&"-t".to_string()));
        assert!(args.contains(&"--build-arg".to_string()));
        assert_eq!(args.last().unwrap(), "/tmp/ctx");
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
}
