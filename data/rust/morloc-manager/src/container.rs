use std::process::{Command, ExitStatus, Stdio};

use crate::types::ContainerEngine;

// ======================================================================
// Configuration records
// ======================================================================

#[derive(Debug, Clone)]
pub struct RunConfig {
    pub image: String,
    pub bind_mounts: Vec<(String, String)>,
    pub ports: Vec<(u16, u16)>,
    pub env: Vec<(String, String)>,
    pub read_only: bool,
    pub interactive: bool,
    pub remove_after: bool,
    pub name: Option<String>,
    pub shm_size: Option<String>,
    pub command: Option<Vec<String>>,
    pub work_dir: Option<String>,
    pub selinux_suffix: String,
    pub extra_flags: Vec<String>,
}

impl RunConfig {
    pub fn new(image: &str) -> Self {
        Self {
            image: image.to_string(),
            bind_mounts: Vec::new(),
            ports: Vec::new(),
            env: Vec::new(),
            read_only: false,
            interactive: false,
            remove_after: true,
            name: None,
            shm_size: None,
            command: None,
            work_dir: None,
            selinux_suffix: String::new(),
            extra_flags: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BuildConfig {
    pub dockerfile: String,
    pub context: String,
    pub tag: String,
    pub build_args: Vec<(String, String)>,
}

// ======================================================================
// Engine detection
// ======================================================================

pub fn engine_executable(engine: ContainerEngine) -> &'static str {
    match engine {
        ContainerEngine::Docker => "docker",
        ContainerEngine::Podman => "podman",
    }
}

// ======================================================================
// Operations
// ======================================================================

pub fn container_run(engine: ContainerEngine, cfg: &RunConfig) -> (ExitStatus, String, String) {
    let exe = engine_executable(engine);
    let extra = engine_specific_run_flags_io(engine);
    let args = build_run_args(engine, &extra, cfg);
    run_process(exe, &args)
}

pub fn container_run_passthrough(
    engine: ContainerEngine,
    verbose: bool,
    cfg: &RunConfig,
) -> ExitStatus {
    let exe = engine_executable(engine);
    let extra = engine_specific_run_flags_io(engine);
    let args = build_run_args(engine, &extra, cfg);

    if verbose {
        let quoted: Vec<String> = args
            .iter()
            .map(|a| {
                if a.contains(' ') {
                    format!("'{a}'")
                } else {
                    a.clone()
                }
            })
            .collect();
        eprintln!("[morloc-manager] {exe} {}", quoted.join(" "));
    }

    Command::new(exe)
        .args(&args)
        .stdin(Stdio::inherit())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .status()
        .unwrap_or_else(|_| std::process::exit(1))
}

pub fn container_build(engine: ContainerEngine, cfg: &BuildConfig) -> (ExitStatus, String, String) {
    let exe = engine_executable(engine);
    let args = build_build_args(cfg);
    run_process_pass_stderr(exe, &args)
}

pub fn container_pull(engine: ContainerEngine, image: &str) -> (ExitStatus, String, String) {
    let exe = engine_executable(engine);
    run_process_pass_stderr(exe, &["pull".to_string(), image.to_string()])
}

pub fn image_exists_locally(engine: ContainerEngine, image: &str) -> bool {
    let exe = engine_executable(engine);
    Command::new(exe)
        .args(["image", "inspect", image])
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .map(|s| s.success())
        .unwrap_or(false)
}

pub fn remote_image_exists(engine: ContainerEngine, image: &str) -> bool {
    let exe = engine_executable(engine);
    Command::new(exe)
        .args(["manifest", "inspect", image])
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .map(|s| s.success())
        .unwrap_or(false)
}

pub fn container_stop(engine: ContainerEngine, name_or_id: &str) -> (ExitStatus, String) {
    let exe = engine_executable(engine);
    let (code, _, err) = run_process(exe, &["stop".to_string(), name_or_id.to_string()]);
    (code, err)
}

pub fn container_remove(engine: ContainerEngine, name_or_id: &str) -> ExitStatus {
    let exe = engine_executable(engine);
    let (code, _, _) = run_process(exe, &["rm".to_string(), "-f".to_string(), name_or_id.to_string()]);
    code
}

// ======================================================================
// CLI argument construction
// ======================================================================

pub fn build_run_args(
    _engine: ContainerEngine,
    extra_engine_flags: &[String],
    cfg: &RunConfig,
) -> Vec<String> {
    let mut args = vec!["run".to_string()];
    args.extend(extra_engine_flags.iter().cloned());

    if cfg.remove_after {
        args.push("--rm".to_string());
    }
    if cfg.read_only {
        args.push("--read-only".to_string());
    }
    if cfg.interactive {
        args.push("-it".to_string());
    }
    if let Some(ref n) = cfg.name {
        args.push("--name".to_string());
        args.push(n.clone());
    }
    if let Some(ref s) = cfg.shm_size {
        args.push("--shm-size".to_string());
        args.push(s.clone());
    }
    if let Some(ref w) = cfg.work_dir {
        args.push("-w".to_string());
        args.push(w.clone());
    }
    for (host, container) in &cfg.bind_mounts {
        args.push("-v".to_string());
        args.push(format!("{host}:{container}{}", cfg.selinux_suffix));
    }
    for (host_port, container_port) in &cfg.ports {
        args.push("-p".to_string());
        args.push(format!("{host_port}:{container_port}"));
    }
    for (key, val) in &cfg.env {
        args.push("-e".to_string());
        args.push(format!("{key}={val}"));
    }
    args.extend(cfg.extra_flags.iter().cloned());
    args.push(cfg.image.clone());
    if let Some(ref cmd) = cfg.command {
        args.extend(cmd.iter().cloned());
    }
    args
}

pub fn engine_specific_run_flags_io(engine: ContainerEngine) -> Vec<String> {
    let uid = nix::unistd::getuid();
    match engine {
        ContainerEngine::Podman => {
            if uid.is_root() {
                Vec::new()
            } else {
                vec!["--userns=keep-id".to_string()]
            }
        }
        ContainerEngine::Docker => {
            if uid.is_root() {
                Vec::new()
            } else {
                let gid = nix::unistd::getgid();
                vec!["--user".to_string(), format!("{}:{}", uid, gid)]
            }
        }
    }
}

/// Pure version for testing.
#[cfg(test)]
pub fn engine_specific_run_flags(engine: ContainerEngine) -> Vec<String> {
    match engine {
        ContainerEngine::Podman => vec!["--userns=keep-id".to_string()],
        ContainerEngine::Docker => Vec::new(),
    }
}

pub fn build_build_args(cfg: &BuildConfig) -> Vec<String> {
    let mut args = vec![
        "build".to_string(),
        "-f".to_string(),
        cfg.dockerfile.clone(),
        "-t".to_string(),
        cfg.tag.clone(),
    ];
    for (key, val) in &cfg.build_args {
        args.push("--build-arg".to_string());
        args.push(format!("{key}={val}"));
    }
    args.push(cfg.context.clone());
    args
}

// ======================================================================
// Process execution
// ======================================================================

fn run_process(exe: &str, args: &[String]) -> (ExitStatus, String, String) {
    let output = Command::new(exe)
        .args(args)
        .stdin(Stdio::null())
        .output()
        .unwrap_or_else(|e| {
            eprintln!("Failed to execute {exe}: {e}");
            std::process::exit(1);
        });
    (
        output.status,
        String::from_utf8_lossy(&output.stdout).to_string(),
        String::from_utf8_lossy(&output.stderr).to_string(),
    )
}

fn run_process_pass_stderr(exe: &str, args: &[String]) -> (ExitStatus, String, String) {
    let child = Command::new(exe)
        .args(args)
        .stdin(Stdio::inherit())
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .output()
        .unwrap_or_else(|e| {
            eprintln!("Failed to execute {exe}: {e}");
            std::process::exit(1);
        });
    (
        child.status,
        String::from_utf8_lossy(&child.stdout).to_string(),
        String::new(),
    )
}

// ======================================================================
// Helpers
// ======================================================================

pub fn exit_code_to_int(status: ExitStatus) -> i32 {
    status.code().unwrap_or(1)
}
