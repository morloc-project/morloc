use std::io;
use std::os::unix::fs::PermissionsExt;
use std::path::Path;
use std::process::{Command, ExitStatus, Stdio};
use std::sync::OnceLock;

use crate::types::ContainerEngine;

// ======================================================================
// Configuration records
// ======================================================================

#[derive(Debug, Clone)]
pub struct RunConfig {
    pub image: String,
    pub bind_mounts: Vec<(String, String)>,
    pub ports: Vec<(u16, u16)>,
    /// Host interface to publish ports on (`docker -p <host>:H:C`). `None`
    /// binds all interfaces (0.0.0.0); `Some("127.0.0.1")` restricts to
    /// loopback so the service is not reachable off the host.
    pub publish_host: Option<String>,
    /// Container network mode (`docker --network <net>`). `Some("host")` shares
    /// the host network namespace so a `--http-host 127.0.0.1` bind lands on the
    /// host's loopback, unreachable by sibling containers; `-p` is then invalid
    /// and suppressed. `None` uses the engine default (bridge).
    pub network: Option<String>,
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
            publish_host: None,
            network: None,
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
    /// User-supplied build-engine flags (from env.flags.yaml `build` section
    /// plus any one-shot CLI overrides). Forwarded verbatim into the build
    /// argv after `--build-arg` pairs, before the context.
    pub extra_flags: Vec<String>,
}



// ======================================================================
// Engine detection
// ======================================================================

/// Returns the executable name for the given engine. For Apptainer, runtime-
/// detects `apptainer` (preferred) then `singularity`; result is cached for
/// the lifetime of the process.
pub fn engine_executable(engine: ContainerEngine) -> &'static str {
    match engine {
        ContainerEngine::Docker => "docker",
        ContainerEngine::Podman => "podman",
        ContainerEngine::Apptainer => apptainer_executable(),
    }
}

/// Cached selection of the Apptainer binary. Resolved once per process.
fn apptainer_executable() -> &'static str {
    static EXE: OnceLock<&'static str> = OnceLock::new();
    EXE.get_or_init(|| {
        if has_on_path("apptainer") {
            "apptainer"
        } else if has_on_path("singularity") {
            "singularity"
        } else {
            // Fall through to "apptainer"; the actual exec will fail loudly
            // with a "command not found" the user can act on. Better than a
            // silent compile-time decision.
            "apptainer"
        }
    })
}


/// Internal: argv-shape used by the dispatch in build_run_args.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ArgStyle {
    /// Docker/Podman `run` semantics.
    Oci,
    /// Apptainer `exec`/`shell` semantics.
    Apptainer,
}

fn argstyle(engine: ContainerEngine) -> ArgStyle {
    match engine {
        ContainerEngine::Docker | ContainerEngine::Podman => ArgStyle::Oci,
        ContainerEngine::Apptainer => ArgStyle::Apptainer,
    }
}

/// Check $PATH for an executable named `exe`. Reads PATH directly rather than
/// shelling out: avoids the cost of a process spawn and makes the check
/// hermetic against shell aliases.
fn has_on_path(exe: &str) -> bool {
    let Ok(path) = std::env::var("PATH") else { return false };
    for dir in path.split(':') {
        let candidate = Path::new(dir).join(exe);
        if let Ok(meta) = candidate.metadata() {
            if meta.is_file() && meta.permissions().mode() & 0o111 != 0 {
                return true;
            }
        }
    }
    false
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
    let args = pull_argv(engine, image, None);
    run_process(exe, &args)
}

/// Build a container image with all output (stdout+stderr) redirected to stderr.
/// Use for IO () commands where stdout must stay clean.
pub fn container_build_visible(engine: ContainerEngine, cfg: &BuildConfig) -> ExitStatus {
    let exe = engine_executable(engine);
    let args = build_build_args(cfg);
    run_process_to_stderr(exe, &args)
}



/// Build the argv for `pull`. For OCI engines this is `pull <image>`. For
/// Apptainer it is `pull <output.sif> docker://<image>` (the `docker://`
/// scheme triggers OCI conversion).
fn pull_argv(engine: ContainerEngine, image: &str, target_path: Option<&str>) -> Vec<String> {
    match argstyle(engine) {
        ArgStyle::Oci => vec!["pull".to_string(), image.to_string()],
        ArgStyle::Apptainer => {
            let mut args = vec!["pull".to_string()];
            if let Some(path) = target_path {
                args.push(path.to_string());
            }
            // Treat any caller-supplied scheme (docker://, oras://, library://,
            // docker-daemon://, oci-archive://) as-is. Otherwise default to
            // docker:// so a bare OCI ref like ghcr.io/foo/bar:tag works.
            let normalized = if image.contains("://") {
                image.to_string()
            } else {
                format!("docker://{image}")
            };
            args.push(normalized);
            args
        }
    }
}

pub fn image_exists_locally(engine: ContainerEngine, image: &str) -> bool {
    match argstyle(engine) {
        ArgStyle::Oci => {
            let exe = engine_executable(engine);
            Command::new(exe)
                .args(["image", "inspect", image])
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .status()
                .map(|s| s.success())
                .unwrap_or(false)
        }
        // For Apptainer, "image" is a .sif file path on disk. A file's
        // existence is the same as the image being available.
        ArgStyle::Apptainer => Path::new(image).is_file(),
    }
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



pub fn container_stop(engine: ContainerEngine, name_or_id: &str) -> (ExitStatus, String) {
    match argstyle(engine) {
        ArgStyle::Oci => {
            let exe = engine_executable(engine);
            let (code, _, err) = run_process(exe, &["stop".to_string(), name_or_id.to_string()]);
            (code, err)
        }
        // For Apptainer, `morloc-manager run` is one-shot; there is nothing
        // to stop. Long-running instances are managed in serve.rs via
        // `apptainer instance stop`. Return a successful no-op so callers
        // doing pre-emptive cleanup (e.g. remove_environment) succeed.
        ArgStyle::Apptainer => (no_op_exit_status(), String::new()),
    }
}

pub fn container_remove(engine: ContainerEngine, name_or_id: &str) -> ExitStatus {
    match argstyle(engine) {
        ArgStyle::Oci => {
            let exe = engine_executable(engine);
            let (code, _, _) = run_process(
                exe,
                &["rm".to_string(), "-f".to_string(), name_or_id.to_string()],
            );
            code
        }
        ArgStyle::Apptainer => no_op_exit_status(),
    }
}

/// Quiet container removal: suppresses stderr (for pre-emptive cleanup).
pub fn container_remove_quiet(engine: ContainerEngine, name_or_id: &str) -> ExitStatus {
    match argstyle(engine) {
        ArgStyle::Oci => {
            let exe = engine_executable(engine);
            let (code, _, _) = run_process_quiet(
                exe,
                &["rm".to_string(), "-f".to_string(), name_or_id.to_string()],
            );
            code
        }
        ArgStyle::Apptainer => no_op_exit_status(),
    }
}

/// Check whether a container with this name exists (running or stopped).
pub fn container_exists(engine: ContainerEngine, name: &str) -> bool {
    match argstyle(engine) {
        ArgStyle::Oci => {
            let exe = engine_executable(engine);
            Command::new(exe)
                .args(["container", "inspect", name])
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .status()
                .map(|s| s.success())
                .unwrap_or(false)
        }
        // For Apptainer, persistent containers live as instances and are
        // queried via serve.rs. The generic container_exists check on the
        // OCI side is only meaningful in the OCI world, so report false
        // here -- callers should go through serve::query_serve_containers
        // for the Apptainer path.
        ArgStyle::Apptainer => false,
    }
}

pub fn remove_image(engine: ContainerEngine, tag: &str) -> bool {
    match argstyle(engine) {
        ArgStyle::Oci => {
            let exe = engine_executable(engine);
            let (status, _, _) = run_process(exe, &["rmi".to_string(), tag.to_string()]);
            status.success()
        }
        // For Apptainer the "image" is a .sif file on disk. Removing the
        // image is just deleting the file. Treat tag as a path here -- the
        // env-cleanup caller in environment.rs already has the right path
        // because it comes from EnvironmentConfig::layered_sif.
        ArgStyle::Apptainer => {
            std::fs::remove_file(Path::new(tag)).is_ok()
        }
    }
}

/// Produce a synthetic ExitStatus(0) for engine no-ops. The standard library
/// does not expose a constructor for ExitStatus, so we run `true` -- a
/// guaranteed-fast successful no-op available on every POSIX system.
fn no_op_exit_status() -> ExitStatus {
    Command::new("true")
        .status()
        .unwrap_or_else(|_| std::process::exit(1))
}

// ======================================================================
// CLI argument construction
// ======================================================================

pub fn build_run_args(
    engine: ContainerEngine,
    extra_engine_flags: &[String],
    cfg: &RunConfig,
) -> Vec<String> {
    match argstyle(engine) {
        ArgStyle::Oci => build_oci_run_args(engine, extra_engine_flags, cfg),
        ArgStyle::Apptainer => build_apptainer_args(extra_engine_flags, cfg),
    }
}

/// Today's `docker run`-style argv builder. Behavior is unchanged from before
/// the Apptainer addition.
fn build_oci_run_args(
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
    if let Some(ref net) = cfg.network {
        args.push("--network".to_string());
        args.push(net.clone());
    }
    // Under host networking the container shares the host netns and the nexus
    // binds host ports directly, so `-p` is invalid (docker rejects it) and
    // unnecessary. Skip publishing in that mode.
    if cfg.network.as_deref() != Some("host") {
        for (host_port, container_port) in &cfg.ports {
            args.push("-p".to_string());
            match &cfg.publish_host {
                Some(ip) => args.push(format!("{ip}:{host_port}:{container_port}")),
                None => args.push(format!("{host_port}:{container_port}")),
            }
        }
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

/// Apptainer/Singularity argv builder. Translates RunConfig to `apptainer
/// exec`, `apptainer shell`, or `apptainer run` semantics. Image is the path
/// to a local .sif file.
///
/// Subcommand selection:
/// * `interactive=true` + cmd=Some(["/bin/bash"]) => `shell` (matches the
///   `--shell` UX in run_with_config).
/// * cmd=Some(other) => `exec` (run the command directly, bypassing any
///   runscript).
/// * cmd=None => `run` (invoke the image's runscript).
///
/// Flag translation (see plan):
/// * `-v` => `--bind` (selinux suffixes preserved)
/// * `-e` => `--env`
/// * `-w` => `--pwd`
/// * `--rm`, `-i`/`-t`, `--name`, `--read-only`, `--tmpfs`, `--user`,
///   `--userns=keep-id` => dropped (Apptainer semantics make them
///   redundant or nonsensical).
/// * `--shm-size` => dropped silently. Apptainer shares host /dev/shm.
/// * `-p H:C` with H==C => dropped silently. Apptainer uses host network.
/// * `-p H:C` with H!=C => dropped here with a warning; callers that care
///   about exact port mapping (e.g. serve) should validate before reaching
///   this function.
fn build_apptainer_args(extra_engine_flags: &[String], cfg: &RunConfig) -> Vec<String> {
    let is_shell = cfg.interactive
        && cfg
            .command
            .as_ref()
            .map(|c| c.as_slice() == ["/bin/bash"])
            .unwrap_or(false);

    let subcommand = if is_shell {
        "shell"
    } else if cfg.command.is_some() {
        "exec"
    } else {
        "run"
    };
    let mut args = vec![subcommand.to_string()];
    args.extend(extra_engine_flags.iter().cloned());

    if let Some(ref w) = cfg.work_dir {
        args.push("--pwd".to_string());
        args.push(w.clone());
    }
    for (host, container) in &cfg.bind_mounts {
        // Selinux suffix passes through identically (Apptainer ignores it on
        // non-selinux systems; on selinux systems the kernel honors it).
        args.push("--bind".to_string());
        args.push(format!("{host}:{container}{}", cfg.selinux_suffix));
    }
    for (key, val) in &cfg.env {
        args.push("--env".to_string());
        args.push(format!("{key}={val}"));
    }

    // Port mapping: Apptainer shares the host network namespace. H==C is a
    // no-op (the inside-container port is the host port). H!=C cannot be
    // expressed; we surface the impossibility once per invocation and drop
    // the flag rather than silently rewrite.
    for (host_port, container_port) in &cfg.ports {
        if host_port != container_port {
            warn_dropped(&format!(
                "-p {host_port}:{container_port}: apptainer uses host networking; \
                 H!=C port mapping is not supported. Bind the container service \
                 directly to host port {host_port} or change --port to a matching pair."
            ));
        }
        // No flag emitted either way.
    }

    if cfg.shm_size.is_some() {
        // Drop silently per plan: Apptainer shares host /dev/shm so
        // --shm-size has no analog and the user's intent (large SHM) is
        // already satisfied.
    }

    args.extend(cfg.extra_flags.iter().cloned());
    args.push(cfg.image.clone());
    if !is_shell {
        if let Some(ref cmd) = cfg.command {
            args.extend(cmd.iter().cloned());
        }
    }
    args
}

/// Print a warning to stderr once per (process, message). Used by the
/// Apptainer translation layer to surface dropped flags without spamming
/// repeated invocations.
fn warn_dropped(message: &str) {
    use std::sync::Mutex;
    static SEEN: OnceLock<Mutex<std::collections::HashSet<String>>> = OnceLock::new();
    let seen = SEEN.get_or_init(|| Mutex::new(std::collections::HashSet::new()));
    let mut guard = seen.lock().unwrap();
    if guard.insert(message.to_string()) {
        eprintln!("[morloc-manager] note: {message}");
    }
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
        // Apptainer runs as the calling user and ignores --user/--userns.
        ContainerEngine::Apptainer => Vec::new(),
    }
}

/// Pure version for testing.
#[cfg(test)]
pub fn engine_specific_run_flags(engine: ContainerEngine) -> Vec<String> {
    match engine {
        ContainerEngine::Podman => vec!["--userns=keep-id".to_string()],
        ContainerEngine::Docker => Vec::new(),
        ContainerEngine::Apptainer => Vec::new(),
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
    args.extend(cfg.extra_flags.iter().cloned());
    args.push(cfg.context.clone());
    args
}

// ======================================================================
// Apptainer build operations
// ======================================================================







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
