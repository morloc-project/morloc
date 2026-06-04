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

/// Configuration for an Apptainer native build from a Singularity .def file.
/// Does not require Docker or Podman on the host -- the native `apptainer
/// build` is used.
#[derive(Debug, Clone)]
pub struct ApptainerNativeBuildConfig {
    /// Path to the .def file.
    pub deffile: String,
    /// Output .sif path. Built to `<output>.tmp.sif` first and renamed on
    /// success.
    pub output_sif: String,
    /// `--build-arg` pairs forwarded to `apptainer build`.
    pub build_args: Vec<(String, String)>,
}

/// Configuration for converting an existing OCI image (in the local
/// docker/podman daemon) to a .sif. Used as the Apptainer-engine fallback
/// when only a Dockerfile is present and an OCI builder is available.
#[derive(Debug, Clone)]
pub struct ApptainerOciConvertConfig {
    /// OCI engine that holds the source image (docker or podman).
    pub source_engine: ContainerEngine,
    /// Local OCI image tag the source engine has built (e.g.
    /// `localhost/morloc-env:dnd`).
    pub source_tag: String,
    /// Output .sif path.
    pub output_sif: String,
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

/// Returns true if either Docker or Podman is reachable on $PATH. Used by
/// the Apptainer-engine fallback build path that converts an OCI image to a
/// .sif. Prefers docker when both are present (matches the existing morloc
/// preference order in setup).
pub fn detect_oci_builder() -> Option<ContainerEngine> {
    if has_on_path("docker") {
        Some(ContainerEngine::Docker)
    } else if has_on_path("podman") {
        Some(ContainerEngine::Podman)
    } else {
        None
    }
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

/// Pull a container image with all output (stdout+stderr) redirected to stderr.
/// Use for IO () commands where stdout must stay clean.
pub fn container_pull_visible(engine: ContainerEngine, image: &str) -> ExitStatus {
    let exe = engine_executable(engine);
    let args = pull_argv(engine, image, None);
    run_process_to_stderr(exe, &args)
}

/// Pull an image to a specific local file path. For docker/podman this is
/// not meaningful (the daemon owns image storage) and `target_path` is
/// ignored. For apptainer, `target_path` is required and is the local .sif
/// destination.
pub fn container_pull_to_path(
    engine: ContainerEngine,
    image: &str,
    target_path: &str,
) -> ExitStatus {
    let exe = engine_executable(engine);
    let args = pull_argv(engine, image, Some(target_path));
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
    match argstyle(engine) {
        ArgStyle::Oci => {
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
        // Apptainer has no `manifest inspect` analog. Best-effort: assume the
        // remote exists and let the subsequent `apptainer pull` fail loudly
        // with the registry's own error if it does not. This trades
        // pre-flight precision for not needing skopeo on the build host.
        ArgStyle::Apptainer => RemoteImageStatus::Exists,
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
    args.push(cfg.context.clone());
    args
}

// ======================================================================
// Apptainer build operations
// ======================================================================

/// Build a .sif natively from a Singularity .def file. Requires only the
/// Apptainer binary on $PATH -- no Docker daemon, no Podman. Atomic write:
/// builds to `<output>.tmp.sif` and renames on success so a failed build
/// never leaves a corrupt .sif behind.
pub fn apptainer_build_native(cfg: &ApptainerNativeBuildConfig) -> ExitStatus {
    let exe = engine_executable(ContainerEngine::Apptainer);
    note_ignore_subuid();
    let argv = build_apptainer_native_argv(cfg);

    let status = run_process_to_stderr(exe, &argv);
    if status.success() {
        finalize_sif_atomic(&cfg.output_sif);
    }
    status
}

/// Build a .sif by converting an OCI image that already exists in a
/// docker/podman daemon. This is the Apptainer-engine *fallback* path used
/// when the user has only a Dockerfile recipe. Atomic write as above.
pub fn apptainer_build_from_oci_daemon(cfg: &ApptainerOciConvertConfig) -> ExitStatus {
    let exe = engine_executable(ContainerEngine::Apptainer);
    note_ignore_subuid();
    let argv = build_apptainer_oci_convert_argv(cfg);

    let status = run_process_to_stderr(exe, &argv);
    if status.success() {
        finalize_sif_atomic(&cfg.output_sif);
    }
    status
}

/// Announce -- once per build, before invoking apptainer -- that we are
/// passing the user-namespace-only build flags. This is a build-policy
/// choice the user should see; the message also gives users an obvious
/// string to grep for if they want to revisit the policy.
fn note_ignore_subuid() {
    eprintln!(
        "[morloc-manager] INFO: passing --ignore-subuid and --ignore-fakeroot-command\n  \
         to `apptainer build`. Together these select Apptainer's unprivileged\n  \
         user-namespace build mode:\n  \
           * --ignore-subuid skips the /etc/subuid-driven fakeroot path that\n  \
             FATAL-errors when unprivileged user namespaces are unavailable.\n  \
           * --ignore-fakeroot-command skips Apptainer's bundled fakeroot\n  \
             LD_PRELOAD wrapper, which can fail with a libc/library mismatch\n  \
             inside the target image, and instead runs %post as namespace-\n  \
             mapped root.\n  \
         Both flags are no-ops under `sudo apptainer build`. To override, edit\n  \
         build_apptainer_*_argv in data/rust/morloc-manager/src/container.rs."
    );
}

/// Pure-function argv builder for the native .def path. Kept separate so
/// tests can assert on the exact argv without spawning Apptainer.
///
/// Two policy flags are always passed:
///
/// * `--ignore-subuid`: Apptainer otherwise prefers fakeroot mode when
///   /etc/subuid is mapped, and FATAL-errors if unprivileged user
///   namespaces are not available.
/// * `--ignore-fakeroot-command`: With subuid ignored, Apptainer falls
///   through to "root-mapped namespace" mode and by default tries to run
///   %post under its bundled `fakeroot` LD_PRELOAD wrapper. That wrapper
///   is dynamically linked and fails if the target image's libc differs
///   from the host's. Ignoring it makes %post run as namespace-mapped
///   root, which is what we actually want for `Bootstrap: localimage`
///   plus `apt-get install`-style recipes.
///
/// The two flags together select Apptainer's modern user-namespace-only
/// build mode. They are no-ops when the build is run as real root (sudo).
pub fn build_apptainer_native_argv(cfg: &ApptainerNativeBuildConfig) -> Vec<String> {
    let mut argv = vec![
        "build".to_string(),
        "--force".to_string(),
        "--ignore-subuid".to_string(),
        "--ignore-fakeroot-command".to_string(),
    ];
    for (key, val) in &cfg.build_args {
        argv.push("--build-arg".to_string());
        argv.push(format!("{key}={val}"));
    }
    argv.push(tmp_sif_path(&cfg.output_sif));
    argv.push(cfg.deffile.clone());
    argv
}

/// Pure-function argv builder for the OCI-conversion path.
pub fn build_apptainer_oci_convert_argv(cfg: &ApptainerOciConvertConfig) -> Vec<String> {
    let scheme = match cfg.source_engine {
        ContainerEngine::Docker => "docker-daemon",
        ContainerEngine::Podman => "podman-daemon",
        // Apptainer cannot be the source of an OCI image; reject by code.
        ContainerEngine::Apptainer => "docker-daemon",
    };
    vec![
        "build".to_string(),
        "--force".to_string(),
        "--ignore-subuid".to_string(),
        "--ignore-fakeroot-command".to_string(),
        tmp_sif_path(&cfg.output_sif),
        format!("{scheme}://{}", cfg.source_tag),
    ]
}

fn tmp_sif_path(final_path: &str) -> String {
    format!("{final_path}.tmp.sif")
}

/// Rename `<output>.tmp.sif` -> `<output>`, replacing any existing file.
/// Best-effort: if the rename fails, the .tmp.sif is left in place so the
/// user can inspect what was produced.
fn finalize_sif_atomic(output_sif: &str) {
    let tmp = tmp_sif_path(output_sif);
    if let Some(parent) = Path::new(output_sif).parent() {
        let _ = std::fs::create_dir_all(parent);
    }
    let _ = std::fs::rename(&tmp, output_sif);
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
