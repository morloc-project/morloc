use std::io;
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

/// Like `container_run` but captures both stdout and stderr (no streaming).
pub fn container_run_quiet(engine: ContainerEngine, cfg: &RunConfig) -> (ExitStatus, String, String) {
    let exe = engine_executable(engine);
    let extra = engine_specific_run_flags_io(engine);
    let args = build_run_args(engine, &extra, cfg);
    run_process_quiet(exe, &args)
}

pub fn container_run_passthrough(
    engine: ContainerEngine,
    verbose: bool,
    shell: bool,
    cfg: &RunConfig,
) -> ExitStatus {
    let exe = engine_executable(engine);
    let extra = engine_specific_run_flags_io(engine);
    let args = build_run_args(engine, &extra, cfg);

    if verbose || shell {
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
    run_process(exe, &args)
}

pub fn container_pull(engine: ContainerEngine, image: &str) -> (ExitStatus, String, String) {
    let exe = engine_executable(engine);
    run_process(exe, &["pull".to_string(), image.to_string()])
}

/// Build a container image with all output (stdout+stderr) redirected to stderr.
/// Use for IO () commands where stdout must stay clean.
pub fn container_build_visible(engine: ContainerEngine, cfg: &BuildConfig) -> ExitStatus {
    let exe = engine_executable(engine);
    let args = build_build_args(cfg);
    run_process_to_stderr(exe, &args)
}

/// Pull a container image with all output (stdout+stderr) redirected to stderr.
/// Use for IO () commands where stdout must stay clean.
pub fn container_pull_visible(engine: ContainerEngine, image: &str) -> ExitStatus {
    let exe = engine_executable(engine);
    run_process_to_stderr(exe, &["pull".to_string(), image.to_string()])
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

/// Run `image inspect` and return the stderr if it fails.
/// Returns None on success, Some(stderr) on failure.
pub fn image_inspect_stderr(engine: ContainerEngine, image: &str) -> Option<String> {
    let exe = engine_executable(engine);
    let output = Command::new(exe)
        .args(["image", "inspect", image])
        .stdout(Stdio::null())
        .output()
        .ok()?;
    if output.status.success() {
        None
    } else {
        Some(String::from_utf8_lossy(&output.stderr).to_string())
    }
}

/// Result of checking whether a remote image exists.
pub enum RemoteImageStatus {
    /// The image exists on the registry.
    Exists,
    /// The registry was reached but the image/tag was not found.
    NotFound,
    /// The check failed for an unknown reason (network, auth, etc).
    /// Contains the stderr output from the container engine.
    Unknown(String),
}

pub fn check_remote_image(engine: ContainerEngine, image: &str) -> RemoteImageStatus {
    let exe = engine_executable(engine);
    let output = Command::new(exe)
        .args(["manifest", "inspect", image])
        .stdout(Stdio::null())
        .output();

    match output {
        Ok(o) if o.status.success() => RemoteImageStatus::Exists,
        Ok(o) => {
            let stderr = String::from_utf8_lossy(&o.stderr).to_string();
            let lower = stderr.to_lowercase();
            // "manifest unknown" / "not found" / "name unknown" indicate
            // the registry was reachable but the image doesn't exist.
            if lower.contains("manifest unknown")
                || lower.contains("not found")
                || lower.contains("name unknown")
            {
                RemoteImageStatus::NotFound
            } else {
                RemoteImageStatus::Unknown(stderr)
            }
        }
        Err(e) => RemoteImageStatus::Unknown(format!("Failed to execute {exe}: {e}")),
    }
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

/// Quiet container removal: suppresses stderr (for pre-emptive cleanup).
pub fn container_remove_quiet(engine: ContainerEngine, name_or_id: &str) -> ExitStatus {
    let exe = engine_executable(engine);
    let (code, _, _) = run_process_quiet(exe, &["rm".to_string(), "-f".to_string(), name_or_id.to_string()]);
    code
}

/// Check whether a container with this name exists (running or stopped).
pub fn container_exists(engine: ContainerEngine, name: &str) -> bool {
    let exe = engine_executable(engine);
    Command::new(exe)
        .args(["container", "inspect", name])
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .map(|s| s.success())
        .unwrap_or(false)
}

pub fn remove_image(engine: ContainerEngine, tag: &str) -> bool {
    let exe = engine_executable(engine);
    let (status, _, _) = run_process(exe, &["rmi".to_string(), tag.to_string()]);
    status.success()
}

// ======================================================================
// CLI argument construction
// ======================================================================

pub fn build_run_args(
    engine: ContainerEngine,
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
        // Docker does not auto-mount a tmpfs at /tmp when --read-only is used
        // (podman does). Pool daemons need a writable /tmp for temp files.
        if engine == ContainerEngine::Docker {
            args.push("--tmpfs".to_string());
            args.push("/tmp".to_string());
        }
    }
    // Always attach stdin so piped input works; only allocate a TTY for
    // interactive (shell) sessions.
    args.push("-i".to_string());
    if cfg.interactive {
        args.push("-t".to_string());
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

/// Run a process with both stdout and stderr redirected to our stderr.
/// Returns only the exit status. Use for IO () commands where morloc-manager's
/// stdout must stay clean but the user should see all container output.
fn run_process_to_stderr(exe: &str, args: &[String]) -> ExitStatus {
    let mut child = Command::new(exe)
        .args(args)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .spawn()
        .unwrap_or_else(|e| {
            eprintln!("Failed to execute {exe}: {e}");
            std::process::exit(1);
        });
    // Pump child stdout -> our stderr
    if let Some(mut child_stdout) = child.stdout.take() {
        let stderr = io::stderr();
        let _ = io::copy(&mut child_stdout, &mut stderr.lock());
    }
    child.wait().unwrap_or_else(|e| {
        eprintln!("Failed to wait for {exe}: {e}");
        std::process::exit(1);
    })
}

/// Run a process with stderr streamed live to the terminal.
/// Returns (exit_status, captured_stdout, "").
fn run_process(exe: &str, args: &[String]) -> (ExitStatus, String, String) {
    let output = Command::new(exe)
        .args(args)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .output()
        .unwrap_or_else(|e| {
            eprintln!("Failed to execute {exe}: {e}");
            std::process::exit(1);
        });
    (
        output.status,
        String::from_utf8_lossy(&output.stdout).to_string(),
        String::new(),
    )
}

/// Run a process with all output captured (no streaming).
/// Used when stderr must be parsed (e.g., for error classification).
fn run_process_quiet(exe: &str, args: &[String]) -> (ExitStatus, String, String) {
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

// ======================================================================
// Helpers
// ======================================================================

pub fn exit_code_to_int(status: ExitStatus) -> i32 {
    status.code().unwrap_or(1)
}
