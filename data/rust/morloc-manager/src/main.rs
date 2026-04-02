mod config;
mod container;
mod environment;
mod error;
mod freeze;
mod selinux;
mod serve;
mod types;

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
    let r = "\x1b[0m"; // full ANSI reset

    format!(
        "\
{{name}} - {{about}}

{{usage-heading}} {{usage}}

{bu}Development{r}
  {b}new{r}        Build a new morloc environment
  {b}run{r}        Run a command in the active environment
  {b}rm{r}         Remove a morloc environment
  {b}ls{r}         List morloc environments
  {b}info{r}       Show configuration and installed environments
  {b}select{r}     Select an environment
  {b}update{r}     Rebuild an environment

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
    // -- Development --
    /// Build a new morloc environment
    #[command(display_order = 1)]
    #[command(after_help = "Examples:\n  morloc-manager new\n  morloc-manager new myenv --version 0.69.0\n  morloc-manager new myenv --image ubuntu:22.04 --dockerfile ./Dockerfile")]
    New {
        /// Environment name (default: derived from base image version)
        name: Option<String>,
        /// Base image from Docker Hub or a registry
        #[arg(long)]
        image: Option<String>,
        /// Morloc version (shorthand for --image ghcr.io/.../morloc-full:VERSION)
        #[arg(long)]
        version: Option<String>,
        /// Dockerfile to layer on top of the base image
        #[arg(long)]
        dockerfile: Option<String>,
        /// Generate a stub Dockerfile for customization
        #[arg(long)]
        dockerfile_stub: bool,
        /// Path to a file with one engine argument per line
        #[arg(long)]
        flagfile: Option<String>,
        /// A single engine flag (may be repeated)
        #[arg(short = 'x', long = "engine-arg")]
        engine_arg: Vec<String>,
        /// Container engine: podman or docker
        #[arg(long, value_enum)]
        engine: Option<EngineArg>,
        /// Shared memory size (default: 512m)
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
    #[command(after_help = "Examples:\n  morloc-manager update              # rebuild active environment\n  morloc-manager update myenv        # rebuild a specific environment")]
    Update {
        /// Environment name (default: active environment)
        name: Option<String>,
    },

    // -- Deployment --
    /// Start a serve container
    #[command(display_order = 20)]
    #[command(after_help = "Examples:\n  morloc-manager start myservice:v1\n  morloc-manager start myservice:v1 -p 9090:8080")]
    Start {
        /// Serve image tag
        image: String,
        /// Port mapping HOST:CONTAINER (default: 8080:8080)
        #[arg(short, long, value_parser = parse_port)]
        port: Vec<(u16, u16)>,
    },
    /// Stop a running serve container
    #[command(display_order = 21)]
    #[command(after_help = "Examples:\n  morloc-manager stop myservice:v1")]
    Stop {
        /// Container name
        name: String,
    },
    /// Export installed state as a frozen artifact
    #[command(display_order = 22)]
    #[command(after_help = "Examples:\n  morloc-manager freeze\n  morloc-manager freeze -o ./my-freeze")]
    Freeze {
        /// Output directory
        #[arg(short, long)]
        output: Option<String>,
    },
    /// Build a serve image from frozen state
    #[command(display_order = 23)]
    #[command(after_help = "Examples:\n  morloc-manager unfreeze --from ./morloc-freeze/state.tar.gz -t myservice:v1")]
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
    #[command(after_help = "Examples:\n  morloc-manager status")]
    Status,
    /// Show logs from a serve container
    #[command(display_order = 25)]
    #[command(after_help = "Examples:\n  morloc-manager logs myservice:v1\n  morloc-manager logs myservice:v1 -f")]
    Logs {
        /// Container name
        name: String,
        /// Follow log output
        #[arg(short, long)]
        follow: bool,
    },
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

fn check_system_write_access() -> Result<()> {
    let sys_dir = cfg::config_dir(Scope::System);
    if sys_dir.exists() {
        let test_path = sys_dir.join(".write-check");
        match fs::write(&test_path, b"") {
            Ok(_) => { let _ = fs::remove_file(&test_path); Ok(()) }
            Err(_) => Err(ManagerError::ConfigPermissionDenied(format!(
                "{}. --system requires root. Run with: sudo morloc-manager <command> --system",
                sys_dir.display()
            )))
        }
    } else {
        match fs::create_dir_all(&sys_dir) {
            Ok(_) => Ok(()),
            Err(_) => Err(ManagerError::ConfigPermissionDenied(format!(
                "{}. --system requires root. Run with: sudo morloc-manager <command> --system",
                sys_dir.display()
            )))
        }
    }
}

fn ensure_engine() -> Result<ContainerEngine> {
    if let Some(cfg) = cfg::read_active_config() {
        return Ok(cfg.engine);
    }
    Err(ManagerError::SetupNotComplete(Scope::Local))
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
                    eprintln!("\nNon-interactive, defaulting to podman.");
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

fn bold_green(msg: &str) -> String {
    if io::stderr().is_terminal() {
        format!("\x1b[1;32m{msg}\x1b[0m")
    } else {
        msg.to_string()
    }
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

// ======================================================================
// Dispatch
// ======================================================================

fn dispatch(verbose: bool, cmd: Cmd) -> Result<()> {
    match cmd {
        // ---- new ----
        Cmd::New {
            name,
            image,
            version,
            dockerfile,
            dockerfile_stub,
            flagfile,
            engine_arg,
            engine,
            shm_size,
            system,
            no_init,
            non_interactive,
        } => {
            if system { check_system_write_access()?; }
            let scope = resolve_scope(system);

            // Resolve engine
            let resolved_engine = if let Some(e) = engine {
                let eng: ContainerEngine = e.into();
                check_docker_socket(eng);
                eng
            } else if let Some(cfg) = cfg::read_active_config() {
                cfg.engine
            } else if !non_interactive && io::stdin().is_terminal() {
                let eng = interactive_engine_choice()?;
                check_docker_socket(eng);
                eng
            } else {
                return Err(ManagerError::EngineNotFound);
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

            // Step 2: Resolve base image and version
            let (base_image, original_image, morloc_ver) = if let Some(ref ver_str) = version {
                let ver: Version = ver_str.parse().map_err(|_| {
                    ManagerError::InvalidVersion(ver_str.clone())
                })?;
                let img = environment::pull_version_image(resolved_engine, &ver)?;
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
                if let Some(ver) = morloc_ver {
                    let default_name = ver.show();
                    if cfg::env_config_path(scope, &default_name).is_file() {
                        return Err(ManagerError::EnvError(format!(
                            "Environment '{}' already exists. Use --name to specify a different name.",
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
                     # RUN apt-get update && apt-get install -y cowsay && rm -rf /var/lib/apt/lists/*\n\
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
                eprintln!("Generated stub Dockerfile: {}", stub_path.display());
                Some(stub_path.to_string_lossy().to_string())
            } else {
                None
            };

            let opts = environment::CreateOptions {
                name: env_name.clone(),
                base_image,
                original_image,
                morloc_version: morloc_ver,
                dockerfile: resolved_dockerfile,
                flagfile,
                engine_args: engine_arg,
                engine: resolved_engine,
                shm_size: shm_size.unwrap_or_else(|| "512m".to_string()),
                scope,
                skip_dockerfile_build: dockerfile_stub,
            };

            environment::create_environment(&opts)?;

            // Auto-select
            environment::select_environment(&env_name, scope)?;
            eprintln!("Created and activated environment: {env_name}");

            // Run morloc init
            if !no_init {
                run_morloc_init(resolved_engine, verbose)?;
            }

            eprintln!("{}", bold_green(&format!("Environment '{env_name}' is ready.")));
            Ok(())
        }

        // ---- run ----
        Cmd::Run { command, shell } => {
            let engine = ensure_engine()?;
            if !shell && command.is_empty() {
                return Err(ManagerError::NoCommand);
            }
            run_in_container(engine, verbose, shell, &command).map_err(|e| match e {
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
            let engine = ensure_engine()?;
            for name in &names {
                let scope = if system {
                    Scope::System
                } else {
                    cfg::find_env_scope(name)?
                };
                if scope == Scope::System && !system {
                    check_system_write_access()?;
                }

                // Check if active
                if !force {
                    if let Some(cfg) = cfg::read_active_config() {
                        if cfg.active_env.as_deref() == Some(name.as_str()) {
                            return Err(ManagerError::EnvError(format!(
                                "Cannot remove active environment '{name}'. Use --force or select a different environment first."
                            )));
                        }
                    }
                }

                environment::remove_environment(engine, scope, name)?;
                eprintln!("Removed environment: {name}");
            }
            Ok(())
        }

        // ---- ls ----
        Cmd::Ls { system, local } => {
            let active_env = cfg::read_active_config()
                .and_then(|c| c.active_env);
            let active_str = active_env.as_deref();

            let show_local = !system || local;
            let show_system = !local || system;

            if show_local {
                let envs = environment::list_environments(Scope::Local, active_str);
                for e in envs {
                    let active_mark = if e.active { " (active)" } else { "" };
                    let ver_mark = e.morloc_version
                        .map(|v| format!(" [{}]", v.show()))
                        .unwrap_or_default();
                    println!("{}{}{}", e.name, ver_mark, active_mark);
                }
            }
            if show_system {
                let envs = environment::list_environments(Scope::System, active_str);
                if !envs.is_empty() {
                    if show_local {
                        println!();
                        println!("System:");
                    }
                    for e in envs {
                        let active_mark = if e.active { " (active)" } else { "" };
                        let ver_mark = e.morloc_version
                            .map(|v| format!(" [{}]", v.show()))
                            .unwrap_or_default();
                        println!("  {}{}{}", e.name, ver_mark, active_mark);
                    }
                }
            }
            Ok(())
        }

        // ---- info ----
        Cmd::Info { name } => {
            if let Some(env_name) = name {
                // Detailed info for a specific environment
                let scope = cfg::find_env_scope(&env_name)?;
                let ec = cfg::read_env_config(scope, &env_name)?;
                let data_dir = cfg::env_data_dir(scope, &env_name);
                let active = cfg::read_active_config()
                    .and_then(|c| c.active_env)
                    .as_deref() == Some(env_name.as_str());

                println!("Name:           {}", ec.name);
                println!("Scope:          {}", match scope { Scope::Local => "local", Scope::System => "system" });
                println!("Active:         {}", if active { "yes" } else { "no" });
                println!("Base image:     {}", ec.base_image);
                if let Some(ref img) = ec.built_image {
                    println!("Built image:    {img}");
                }
                if let Some(ver) = ec.morloc_version {
                    println!("Morloc version: {}", ver.show());
                }
                println!("Engine:         {}", display_engine(ec.engine));
                println!("SHM size:       {}", ec.shm_size);
                println!("Dockerfile:     {}", match ec.dockerfile {
                    Some(_) => cfg::env_dockerfile_path(scope, &env_name).display().to_string(),
                    None => "none".to_string(),
                });
                let flags_path = cfg::env_flags_path(scope, &env_name);
                println!("Flags:          {}", flags_path.display());
                let flags = cfg::read_flags_file(&flags_path);
                for flag in &flags {
                    println!("  {flag}");
                }
                println!("Data dir:       {}", data_dir.display());
            } else {
                // Overview
                let local_cfg = cfg::read_config::<Config>(&cfg::config_path(Scope::Local)).ok();
                let system_cfg = cfg::read_config::<Config>(&cfg::config_path(Scope::System)).ok();
                let se_mode = detect_selinux();

                let active_env = local_cfg.as_ref()
                    .and_then(|c| c.active_env.as_deref())
                    .or_else(|| system_cfg.as_ref().and_then(|c| c.active_env.as_deref()))
                    .unwrap_or("none");

                let se_str = match se_mode {
                    SELinuxMode::Enforcing => "enforcing",
                    SELinuxMode::Permissive => "permissive",
                    SELinuxMode::Disabled => "not detected",
                };

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

                let active_str = cfg::read_active_config().and_then(|c| c.active_env);

                let local_envs = environment::list_environments(Scope::Local, active_str.as_deref());
                println!("\nLocal environments:");
                if local_envs.is_empty() {
                    println!("  (none)");
                } else {
                    for e in &local_envs {
                        let active_mark = if e.active { " (active)" } else { "" };
                        let ver_mark = e.morloc_version
                            .map(|v| format!(" [{}]", v.show()))
                            .unwrap_or_default();
                        println!("  {}{}{}", e.name, ver_mark, active_mark);
                    }
                }

                let system_envs = environment::list_environments(Scope::System, active_str.as_deref());
                if !system_envs.is_empty() {
                    println!("\nSystem environments:");
                    for e in &system_envs {
                        let active_mark = if e.active { " (active)" } else { "" };
                        let ver_mark = e.morloc_version
                            .map(|v| format!(" [{}]", v.show()))
                            .unwrap_or_default();
                        println!("  {}{}{}", e.name, ver_mark, active_mark);
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
        Cmd::Update { name } => {
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
            eprintln!("Rebuilding environment: {env_name}");
            environment::rebuild_environment(env_scope, &env_name)?;
            eprintln!("{}", bold_green(&format!("Environment '{env_name}' rebuilt.")));
            Ok(())
        }

        // ---- freeze ----
        Cmd::Freeze { output } => {
            let engine = ensure_engine()?;
            let output_dir = output.as_deref().unwrap_or("./morloc-freeze");
            let (env_name, env_scope, ec) = environment::resolve_active_environment()?;
            let ver = ec.morloc_version.ok_or_else(|| {
                ManagerError::FreezeError("Active environment has no morloc version set".to_string())
            })?;
            let data_dir = cfg::env_data_dir(env_scope, &env_name);
            freeze::freeze_from_dir(env_scope, ver, engine, &data_dir.to_string_lossy(), output_dir)
        }

        // ---- unfreeze ----
        Cmd::Unfreeze { from, tag, base } => {
            let engine = ensure_engine()?;
            let (_, _, ec) = environment::resolve_active_environment()?;
            let ver = ec.morloc_version.ok_or_else(|| {
                ManagerError::UnfreezeError("Active environment has no morloc version set".to_string())
            })?;
            serve::build_serve_image(engine, verbose, &from, &tag, ver, base.as_deref())
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
            let container_name = if name.starts_with("morloc-serve-") {
                name.clone()
            } else {
                format!("morloc-serve-{}", name.replace(':', "-"))
            };
            serve::stop_serve_container(engine, &container_name)?;
            eprintln!("Stopped container {container_name}");
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
            let container_name = if name.starts_with("morloc-serve-") {
                name.clone()
            } else {
                format!("morloc-serve-{}", name.replace(':', "-"))
            };
            serve::stream_serve_logs(engine, &container_name, follow)
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
    let (env_name, env_scope, ec) = environment::resolve_active_environment()?;
    let image = ec.active_image().to_string();
    let data_dir = cfg::env_data_dir(env_scope, &env_name);
    let v_data_dir = data_dir.to_string_lossy().to_string();

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

    // Read flags from the environment's flags file
    let flags_path = cfg::env_flags_path(env_scope, &env_name);
    let extra_flags = cfg::read_flags_file(&flags_path);

    let is_init = matches!(args, [a, b, ..] if a == "morloc" && b == "init");
    let is_home_dir = normalize_trailing(&cwd) == normalize_trailing(&home);

    if !is_init && !suffix.is_empty() && !is_home_dir {
        selinux::validate_mount_path(&cwd)?;
        run_with_config(
            engine, verbose, &image, &v_data_dir, &home, &cwd, suffix,
            shell, args, false, &ec.shm_size, &extra_flags,
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
            shell, args, is_init || skip_work_mount, &ec.shm_size, &extra_flags,
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
