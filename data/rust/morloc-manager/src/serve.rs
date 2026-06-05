use std::fs;
use std::path::Path;
use std::process::{Command, Stdio};
use std::thread;
use std::time::Duration;

use crate::container::{
    container_build, container_pull, container_run, container_run_quiet, container_stop,
    container_remove, engine_executable, exit_code_to_int, image_exists_locally,
    BuildConfig, RunConfig,
};
use crate::error::{ManagerError, Result};
use crate::types::*;

pub fn build_serve_image(
    engine: ContainerEngine,
    verbose: bool,
    state_tarball: &str,
    tag: &str,
    ver: Version,
    base_override: Option<&str>,
    rebuild: bool,
    programs: &[ProgramEntry],
) -> Result<()> {
    if matches!(engine, ContainerEngine::Apptainer) {
        // The Apptainer unfreeze path is not yet implemented end-to-end --
        // the OCI builder available on the freeze host may not be available
        // on a deployment host. Produce a clear error here instead of
        // silently falling into a docker/podman code path that will fail
        // later with a confusing message.
        return Err(ManagerError::UnfreezeError(format!(
            "Apptainer unfreeze is not yet implemented in this build. The frozen \
             state at '{state_tarball}' is engine-agnostic and can be unfrozen \
             under --engine docker or --engine podman; or rebuild from the env's \
             .def recipe with `apptainer build {tag}.sif ...`. Track support in \
             the SLURM-prep roadmap."
        )));
    }
    if !Path::new(state_tarball).exists() {
        return Err(ManagerError::UnfreezeError(format!(
            "Tarball not found: {state_tarball}"
        )));
    }

    if !rebuild && image_exists_locally(engine, tag) {
        eprintln!("Image '{tag}' already exists locally; skipping build (use --rebuild to force)");
        return Ok(());
    }

    let tarball_dir = Path::new(state_tarball)
        .parent()
        .unwrap_or(Path::new("."));
    let manifest_path = tarball_dir.join("freeze-manifest.json");
    let m_manifest = if manifest_path.exists() {
        crate::freeze::read_freeze_manifest(&manifest_path.to_string_lossy()).ok()
    } else {
        None
    };

    let base_image = match base_override {
        Some(b) => b.to_string(),
        None => resolve_base_from_manifest(engine, m_manifest.as_ref(), ver),
    };

    eprintln!("Using base image: {base_image}");
    if !image_exists_locally(engine, &base_image) {
        let exe = engine_executable(engine);
        if verbose {
            eprintln!("[morloc-manager] {exe} pull {base_image}");
        }
        let (pull_status, _, pull_err) = container_pull(engine, &base_image);
        if !pull_status.success() {
            return Err(ManagerError::EngineError {
                engine,
                code: exit_code_to_int(pull_status),
                stderr: pull_err,
            });
        }
    }

    let context_dir = tarball_dir.join("serve-build");
    fs::create_dir_all(&context_dir)
        .map_err(|e| ManagerError::UnfreezeError(format!("mkdir failed: {e}")))?;

    eprintln!("Extracting frozen state...");
    let tar_status = Command::new("tar")
        .args(["-xzf", state_tarball, "-C", &context_dir.to_string_lossy()])
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::inherit())
        .status()
        .map_err(|e| ManagerError::UnfreezeError(format!("tar extract failed: {e}")))?;
    if !tar_status.success() {
        return Err(ManagerError::UnfreezeError(
            "tar extract failed (see error output above)".to_string()
        ));
    }

    // Rewrite build.path in each manifest so the nexus chdirs to the
    // container-internal path instead of the original host path.
    rewrite_manifest_paths(&context_dir)?;

    let dockerfile_path = context_dir.join("Dockerfile");
    let has_exe = context_dir.join("exe").is_dir()
        && fs::read_dir(context_dir.join("exe"))
            .map(|mut d| d.next().is_some())
            .unwrap_or(false);
    let has_opt = context_dir.join("opt").is_dir()
        && fs::read_dir(context_dir.join("opt"))
            .map(|mut d| d.next().is_some())
            .unwrap_or(false);
    let has_src = context_dir.join("src").is_dir()
        && fs::read_dir(context_dir.join("src"))
            .map(|mut d| d.next().is_some())
            .unwrap_or(false);
    let mh = CONTAINER_MORLOC_HOME;
    let exe_line = if has_exe {
        format!("COPY exe/ {mh}/exe/\n")
    } else {
        String::new()
    };
    let opt_line = if has_opt {
        format!("COPY opt/ {mh}/opt/\n")
    } else {
        String::new()
    };
    let src_line = if has_src {
        format!("COPY src/ {mh}/src/\n")
    } else {
        String::new()
    };
    // Podman's OCI format drops HEALTHCHECK; omit it to avoid warnings.
    let healthcheck = if engine == ContainerEngine::Docker {
        "# Health check for container orchestrators\n\
         HEALTHCHECK --interval=30s --timeout=5s --retries=3 \\\n\
           CMD curl -sf http://localhost:8080/health || exit 1\n\
         \n"
            .to_string()
    } else {
        String::new()
    };
    let dockerfile_content = format!(
        "# Auto-generated by morloc-manager serve-image\n\
         FROM {base_image}\n\
         \n\
         # Ensure morloc binaries are on PATH\n\
         ENV PATH=\"{mh}/bin:${{PATH}}\"\n\
         \n\
         # Morloc home for pool path resolution\n\
         ENV MORLOC_HOME=\"{mh}\"\n\
         \n\
         # Copy frozen morloc state (modules, manifests, binaries, pools)\n\
         COPY lib/ {mh}/lib/\n\
         COPY fdb/ {mh}/fdb/\n\
         COPY bin/ {mh}/bin/\n\
         {exe_line}\
         {opt_line}\
         {src_line}\
         RUN chmod -R a+rX {mh}\n\
         \n\
         {healthcheck}\
         # Entrypoint: nexus router aggregates all installed programs\n\
         ENTRYPOINT [\"morloc-nexus\", \"--router\", \\\n\
                     \"--fdb\", \"{mh}/fdb\", \\\n\
                     \"--http-port\", \"8080\"]\n"
    );
    fs::write(&dockerfile_path, &dockerfile_content)
        .map_err(|e| ManagerError::UnfreezeError(format!("Write Dockerfile failed: {e}")))?;

    eprintln!("Building serve image {tag} (base: {base_image})...");
    let build_cfg = BuildConfig {
        dockerfile: dockerfile_path.to_string_lossy().to_string(),
        context: context_dir.to_string_lossy().to_string(),
        tag: tag.to_string(),
        build_args: Vec::new(),
        // Serve-image builds are short-lived bootstrap recipes generated
        // by morloc-manager itself; no user-supplied build flags apply.
        extra_flags: Vec::new(),
    };
    if verbose {
        let exe = engine_executable(engine);
        eprintln!(
            "[morloc-manager] {exe} build -f {} -t {tag} {}",
            build_cfg.dockerfile, build_cfg.context
        );
    }
    let (status, _, build_err) = container_build(engine, &build_cfg);
    if !status.success() {
        return Err(ManagerError::EngineError {
            engine,
            code: exit_code_to_int(status),
            stderr: build_err,
        });
    }
    eprintln!("Built serve image: {tag}");

    // Validate programs work inside the built image
    validate_programs(engine, tag, programs, Vec::new(), verbose)?;

    // Clean up the temporary build context
    if let Err(e) = fs::remove_dir_all(&context_dir) {
        eprintln!("Warning: failed to clean up {}: {e}", context_dir.display());
    }

    Ok(())
}

#[allow(dead_code)]
pub fn run_serve_container(
    engine: ContainerEngine,
    verbose: bool,
    image: &str,
    name: &str,
    ports: &[(u16, u16)],
) -> Result<()> {
    // Clean up any existing dead container with this name (silently)
    let _ = crate::container::container_remove_quiet(engine, name);

    let port_str: Vec<String> = ports
        .iter()
        .map(|(h, c)| format!("{h}:{c}"))
        .collect();
    eprintln!(
        "Starting serve container {name} on ports {}...",
        port_str.join(", ")
    );

    let mut cfg = RunConfig::new(image);
    cfg.read_only = true;
    cfg.remove_after = false;
    cfg.name = Some(name.to_string());
    cfg.ports = ports.to_vec();
    cfg.extra_flags = vec!["-d".to_string()];

    if verbose {
        let exe = engine_executable(engine);
        let extra = crate::container::engine_specific_run_flags_io(engine);
        let args = crate::container::build_run_args(engine, &extra, &cfg);
        let quoted: Vec<String> = args.iter().map(|a| {
            if a.contains(' ') { format!("'{a}'") } else { a.clone() }
        }).collect();
        eprintln!("[morloc-manager] {exe} {}", quoted.join(" "));
    }

    let (status, _stdout, run_err) = container_run(engine, &cfg);
    if !status.success() {
        let _ = crate::container::container_remove_quiet(engine, name);
        return Err(ManagerError::EngineError {
            engine,
            code: exit_code_to_int(status),
            stderr: run_err,
        });
    }

    // Verify container reached running state
    thread::sleep(Duration::from_secs(1));
    let exe = engine_executable(engine);
    let insp_output = Command::new(exe)
        .args(["inspect", "--format", "{{.State.Status}}", name])
        .output();
    match insp_output {
        Ok(o) if o.status.success() => {
            let state = String::from_utf8_lossy(&o.stdout).trim().to_string();
            if state == "running" {
                eprintln!("Container {name} started");
                eprintln!("  Logs:   morloc-manager logs");
                eprintln!("  Stop:   morloc-manager stop {name}");
                eprintln!("  Status: morloc-manager status");
                Ok(())
            } else {
                let log_output = Command::new(exe).args(["logs", name]).output();
                let logs = log_output
                    .map(|o| {
                        let stdout = String::from_utf8_lossy(&o.stdout);
                        let stderr = String::from_utf8_lossy(&o.stderr);
                        format!("{stdout}{stderr}")
                    })
                    .unwrap_or_default();
                // Clean up the dead container to prevent name conflicts on retry
                let _ = container_remove(engine, name);
                Err(ManagerError::EngineError {
                    engine,
                    code: 1,
                    stderr: format!("Container failed to start (state: {state}):\n{logs}"),
                })
            }
        }
        _ => Err(ManagerError::EngineError {
            engine,
            code: 1,
            stderr: "Failed to inspect container state".to_string(),
        }),
    }
}

/// Serve an environment by bind-mounting its data directory into the container.
pub fn serve_environment(
    engine: ContainerEngine,
    verbose: bool,
    image: &str,
    data_dir: &str,
    container_name: &str,
    ports: &[(u16, u16)],
    extra_flags: &[String],
    shm_size: &Option<String>,
    user_env: &[(String, String)],
) -> Result<()> {
    if matches!(engine, ContainerEngine::Apptainer) {
        return serve_apptainer_instance(
            verbose,
            image,
            data_dir,
            container_name,
            ports,
            extra_flags,
            user_env,
        );
    }

    // Clean up any existing dead container with this name (silently)
    let _ = crate::container::container_remove_quiet(engine, container_name);

    let port_str: Vec<String> = ports
        .iter()
        .map(|(h, c)| format!("{h}:{c}"))
        .collect();
    eprintln!(
        "Starting serve container {container_name} on ports {}...",
        port_str.join(", ")
    );

    let mut cfg = RunConfig::new(image);
    cfg.read_only = true;
    cfg.remove_after = false;
    cfg.name = Some(container_name.to_string());
    cfg.ports = ports.to_vec();
    let mh = CONTAINER_MORLOC_HOME;
    cfg.bind_mounts = vec![(data_dir.to_string(), mh.to_string())];
    cfg.env = vec![
        ("PATH".to_string(), format!("{mh}/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin")),
        ("MORLOC_HOME".to_string(), mh.to_string()),
    ];
    cfg.env.extend(user_env.iter().cloned());
    cfg.command = Some(vec![
        "morloc-nexus".to_string(),
        "--router".to_string(),
        "--fdb".to_string(), format!("{mh}/fdb"),
        "--http-port".to_string(), "8080".to_string(),
    ]);
    cfg.shm_size = shm_size.clone();
    cfg.extra_flags = vec!["-d".to_string()];
    cfg.extra_flags.extend(extra_flags.iter().cloned());

    if verbose {
        let exe = engine_executable(engine);
        let extra = crate::container::engine_specific_run_flags_io(engine);
        let args = crate::container::build_run_args(engine, &extra, &cfg);
        let quoted: Vec<String> = args.iter().map(|a| {
            if a.contains(' ') { format!("'{a}'") } else { a.clone() }
        }).collect();
        eprintln!("[morloc-manager] {exe} {}", quoted.join(" "));
    }

    let (status, _stdout, run_err) = container_run(engine, &cfg);
    if !status.success() {
        // `container_run` may have left a partially-created container behind
        // (e.g., port conflict after container creation). Clean it up so the
        // next `start` doesn't fail on a name collision.
        let _ = crate::container::container_remove_quiet(engine, container_name);

        // Detect port conflict and provide a friendlier error message
        let lower = run_err.to_lowercase();
        if lower.contains("address already in use") || lower.contains("port is already allocated")
            || lower.contains("pasta failed")
        {
            // Try to extract the port number from the error
            let port_hint = ports.first()
                .map(|(h, _)| format!(" Port {h} is already in use."))
                .unwrap_or_default();
            return Err(ManagerError::EnvError(format!(
                "{port_hint}\n  \
                 Another container or process is using this port.\n  \
                 Use '-p <other-port>:8080' to choose a different host port, or\n  \
                 check running containers with 'morloc-manager status'."
            )));
        }

        return Err(ManagerError::EngineError {
            engine,
            code: exit_code_to_int(status),
            stderr: run_err,
        });
    }

    // Verify container reached running state
    thread::sleep(Duration::from_secs(1));
    let exe = engine_executable(engine);
    let insp_output = Command::new(exe)
        .args(["inspect", "--format", "{{.State.Status}}", container_name])
        .output();
    match insp_output {
        Ok(o) if o.status.success() => {
            let state = String::from_utf8_lossy(&o.stdout).trim().to_string();
            if state == "running" {
                eprintln!("Container {container_name} started");
                eprintln!("  Logs:   morloc-manager logs");
                eprintln!("  Stop:   morloc-manager stop");
                eprintln!("  Status: morloc-manager status");
                Ok(())
            } else {
                let log_output = Command::new(exe).args(["logs", container_name]).output();
                let logs = log_output
                    .map(|o| {
                        let stdout = String::from_utf8_lossy(&o.stdout);
                        let stderr = String::from_utf8_lossy(&o.stderr);
                        format!("{stdout}{stderr}")
                    })
                    .unwrap_or_default();
                let _ = container_remove(engine, container_name);
                Err(ManagerError::EngineError {
                    engine,
                    code: 1,
                    stderr: format!("Container failed to start (state: {state}):\n{logs}"),
                })
            }
        }
        _ => Err(ManagerError::EngineError {
            engine,
            code: 1,
            stderr: "Failed to inspect container state".to_string(),
        }),
    }
}

pub fn stop_serve_container(engine: ContainerEngine, verbose: bool, name: &str) -> Result<()> {
    if matches!(engine, ContainerEngine::Apptainer) {
        // Existence check is more useful as an error than as a precondition
        // here: the instance might already be gone from a SIGKILL etc.
        let running = apptainer_list_serve_instances();
        if !running.iter().any(|c| c.name == name) {
            return Err(ManagerError::EnvError(format!(
                "No serve instance running for '{name}'"
            )));
        }
        if verbose {
            let exe = engine_executable(engine);
            eprintln!("[morloc-manager] {exe} instance stop {name}");
        }
        return apptainer_instance_stop(name);
    }
    if !crate::container::container_exists(engine, name) {
        return Err(ManagerError::EnvError(format!(
            "No serve container running for '{name}'"
        )));
    }
    if verbose {
        let exe = engine_executable(engine);
        eprintln!("[morloc-manager] {exe} stop {name}");
    }
    let (status, err) = container_stop(engine, name);
    let _ = crate::container::container_remove_quiet(engine, name);
    if !status.success() {
        return Err(ManagerError::EngineError {
            engine,
            code: exit_code_to_int(status),
            stderr: err,
        });
    }
    Ok(())
}

/// Build the serve container name for an environment.
/// Format: morloc-serve-<username>-<envname>
pub fn serve_container_name(env_name: &str) -> String {
    let user = std::env::var("USER")
        .or_else(|_| std::env::var("LOGNAME"))
        .unwrap_or_else(|_| "unknown".to_string());
    format!("morloc-serve-{user}-{env_name}")
}

/// The prefix used to filter all serve containers for the current user.
pub fn serve_container_prefix() -> String {
    let user = std::env::var("USER")
        .or_else(|_| std::env::var("LOGNAME"))
        .unwrap_or_else(|_| "unknown".to_string());
    format!("morloc-serve-{user}-")
}

/// Extract the environment name from a serve container name.
pub fn env_name_from_container(container_name: &str) -> &str {
    let prefix = serve_container_prefix();
    container_name.strip_prefix(&prefix).unwrap_or(container_name)
}

#[derive(serde::Serialize)]
pub struct ServeContainerInfo {
    pub name: String,
    pub env: String,
    pub ports: String,
    pub status: String,
}

/// Query running serve containers and return structured info.
pub fn query_serve_containers(engine: ContainerEngine, verbose: bool) -> Result<Vec<ServeContainerInfo>> {
    if matches!(engine, ContainerEngine::Apptainer) {
        if verbose {
            let exe = engine_executable(engine);
            eprintln!("[morloc-manager] {exe} instance list --json");
        }
        return Ok(apptainer_list_serve_instances());
    }
    let exe = engine_executable(engine);
    let fmt = "{{.Names}}\t{{.Status}}\t{{.Ports}}";
    let prefix = serve_container_prefix();
    let filter = format!("name={prefix}");
    if verbose {
        eprintln!("[morloc-manager] {exe} ps -a --filter {filter} --format '{fmt}'");
    }
    let output = Command::new(exe)
        .args([
            "ps", "-a", "--filter", &filter, "--format", fmt,
        ])
        // Use /tmp as cwd to avoid podman "cannot chdir" failures when the
        // current directory is inaccessible (e.g. another user's home).
        .current_dir("/tmp")
        .output()
        .map_err(|e| ManagerError::EngineError {
            engine,
            code: 1,
            stderr: format!("Failed to list containers: {e}"),
        })?;
    if !output.status.success() {
        return Err(ManagerError::EngineError {
            engine,
            code: exit_code_to_int(output.status),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
        });
    }
    let text = String::from_utf8_lossy(&output.stdout).trim().to_string();
    let mut result = Vec::new();
    for line in text.lines() {
        let parts: Vec<&str> = line.split('\t').collect();
        if parts.len() >= 3 {
            let name = parts[0];
            let status = parts[1];
            let ports = parts[2];
            let env = env_name_from_container(name);
            result.push(ServeContainerInfo {
                name: name.to_string(),
                env: env.to_string(),
                ports: if ports.is_empty() { "-".to_string() } else { ports.to_string() },
                status: status.to_string(),
            });
        }
    }
    Ok(result)
}

/// Find running serve container names for the current user.
pub fn find_running_serve_containers(engine: ContainerEngine) -> Vec<String> {
    if matches!(engine, ContainerEngine::Apptainer) {
        return apptainer_list_serve_instances()
            .into_iter()
            .map(|c| c.name)
            .collect();
    }
    let exe = engine_executable(engine);
    let filter = format!("name={}", serve_container_prefix());
    let output = Command::new(exe)
        .args(["ps", "--filter", &filter, "--format", "{{.Names}}"])
        .current_dir("/tmp")
        .output();
    match output {
        Ok(o) if o.status.success() => {
            String::from_utf8_lossy(&o.stdout)
                .lines()
                .filter(|l| !l.is_empty())
                .map(|l| l.to_string())
                .collect()
        }
        _ => Vec::new(),
    }
}

// ======================================================================
// Program validation
// ======================================================================

/// Run `--help` for each installed program inside a container image to
/// verify that pool processes start correctly (e.g. all imports resolve).
///
/// `bind_mounts` should be non-empty for pre-freeze validation (where the
/// data dir is on the host) and empty for post-unfreeze validation (where
/// everything is baked into the image).
pub fn validate_programs(
    engine: ContainerEngine,
    image: &str,
    programs: &[ProgramEntry],
    bind_mounts: Vec<(String, String)>,
    verbose: bool,
) -> Result<()> {
    if programs.is_empty() {
        return Ok(());
    }
    eprintln!("Validating installed programs...");
    let mut any_failed = false;
    for prog in programs {
        let exe_path = format!("{}/bin/{}", CONTAINER_MORLOC_HOME, prog.name);
        if verbose {
            let exe = engine_executable(engine);
            eprintln!("[morloc-manager] {exe} run --rm --entrypoint '' {image} {exe_path} --help");
        }
        let cfg = RunConfig {
            bind_mounts: bind_mounts.clone(),
            command: Some(vec![exe_path, "--help".to_string()]),
            env: vec![
                ("MORLOC_HOME".to_string(), CONTAINER_MORLOC_HOME.to_string()),
            ],
            // Override the image ENTRYPOINT so the command runs directly
            // instead of being appended to the router entrypoint.
            extra_flags: vec!["--entrypoint".to_string(), "".to_string()],
            ..RunConfig::new(image)
        };
        let (status, _stdout, stderr) = container_run_quiet(engine, &cfg);
        if status.success() {
            let n = prog.commands.len();
            eprintln!("  [ok] {} ({} commands)", prog.name, n);
        } else {
            let snippet: String = stderr.lines().take(5).collect::<Vec<_>>().join("\n    ");
            eprintln!("  [FAIL] {}: {}", prog.name, snippet);
            any_failed = true;
        }
    }
    if any_failed {
        return Err(ManagerError::FreezeError(
            "Some programs failed validation (see errors above)".to_string(),
        ));
    }
    Ok(())
}

// ======================================================================
// Manifest path rewriting for frozen images
// ======================================================================

const CONTAINER_MORLOC_HOME: &str = "/opt/morloc";
const MANIFEST_MARKER: &str = "### MANIFEST ###";

/// Rewrite `build.path` in every `.manifest` file under `fdb/` so the
/// nexus inside the container chdirs to the correct location instead of
/// the original host path.
fn rewrite_manifest_paths(context_dir: &Path) -> Result<()> {
    let fdb_dir = context_dir.join("fdb");
    if !fdb_dir.is_dir() {
        return Ok(());
    }
    let entries = fs::read_dir(&fdb_dir)
        .map_err(|e| ManagerError::UnfreezeError(format!("read fdb/: {e}")))?;
    for entry in entries {
        let entry = entry
            .map_err(|e| ManagerError::UnfreezeError(format!("read fdb/ entry: {e}")))?;
        let path = entry.path();
        let name = entry.file_name();
        let name_str = name.to_string_lossy();
        if !name_str.ends_with(".manifest") {
            continue;
        }
        let prog_name = &name_str[..name_str.len() - ".manifest".len()];
        let container_build_path = format!("{}/exe/{}", CONTAINER_MORLOC_HOME, prog_name);
        rewrite_one_manifest(&path, &container_build_path)?;
    }
    Ok(())
}

/// Rewrite the `build.path` field in a single manifest wrapper script.
fn rewrite_one_manifest(path: &Path, new_build_path: &str) -> Result<()> {
    let content = fs::read_to_string(path)
        .map_err(|e| ManagerError::UnfreezeError(format!("read {}: {e}", path.display())))?;

    let (prefix, json_str) = if content.starts_with("#!") {
        if let Some(marker_pos) = content.find(MANIFEST_MARKER) {
            let after_marker = &content[marker_pos..];
            let json_start = after_marker
                .find('\n')
                .map(|i| marker_pos + i + 1)
                .unwrap_or(content.len());
            (&content[..json_start], &content[json_start..])
        } else {
            return Ok(()); // no marker, skip
        }
    } else {
        ("", content.as_str())
    };

    let mut manifest: serde_json::Value = serde_json::from_str(json_str)
        .map_err(|e| ManagerError::UnfreezeError(format!("parse {}: {e}", path.display())))?;

    if let Some(build) = manifest.get_mut("build") {
        if let Some(p) = build.get_mut("path") {
            *p = serde_json::Value::String(new_build_path.to_string());
        }
    }

    let new_json = serde_json::to_string(&manifest)
        .map_err(|e| ManagerError::UnfreezeError(format!("serialize {}: {e}", path.display())))?;

    let new_content = format!("{}{}\n", prefix, new_json);
    fs::write(path, new_content)
        .map_err(|e| ManagerError::UnfreezeError(format!("write {}: {e}", path.display())))?;
    Ok(())
}

// ======================================================================
// Manifest and image resolution
// ======================================================================

fn resolve_base_from_manifest(
    engine: ContainerEngine,
    m_manifest: Option<&FreezeManifest>,
    ver: Version,
) -> String {
    let ghcr_fallback = format!(
        "ghcr.io/morloc-project/morloc/morloc-full:{}",
        ver.show()
    );
    let Some(fm) = m_manifest else {
        return ghcr_fallback;
    };

    // Resolve the effective base image: use manifest's base_image if it exists
    // locally, otherwise fall back to the GHCR image. The manifest may record a
    // locally-retagged image (e.g. localhost/morloc:0.69.0) that won't exist on
    // other machines.
    let effective_base = if image_exists_locally(engine, &fm.base_image) {
        fm.base_image.clone()
    } else {
        eprintln!(
            "Base image '{}' not found locally, trying GHCR fallback...",
            fm.base_image
        );
        ghcr_fallback
    };

    match &fm.env_layer {
        None => effective_base,
        Some(fel) => {
            // Fast path: env image tag exists locally
            if let Some(ref tag) = fel.image_tag {
                let exe = engine_executable(engine);
                let check = Command::new(exe)
                    .args(["image", "inspect", tag])
                    .stdout(Stdio::null())
                    .stderr(Stdio::null())
                    .status();
                if check.map(|s| s.success()).unwrap_or(false) {
                    return tag.clone();
                }
            }
            // Rebuild env layer from stored Dockerfile using effective base
            rebuild_env_image(engine, &effective_base, fm, fel)
        }
    }
}

fn rebuild_env_image(
    engine: ContainerEngine,
    effective_base: &str,
    fm: &FreezeManifest,
    fel: &FrozenEnvLayer,
) -> String {
    let env_tag = format!(
        "localhost/morloc-env:{}-{}",
        fm.morloc_version.show(),
        fel.name
    );
    let exe = engine_executable(engine);
    // Check if tagged image exists locally
    let check = Command::new(exe)
        .args(["image", "inspect", &env_tag])
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status();
    if check.map(|s| s.success()).unwrap_or(false) {
        return env_tag;
    }

    eprintln!("Building deployment image (environment layer: {})", fel.name);
    let build_dir = "/tmp/morloc-env-rebuild";
    let _ = fs::create_dir_all(build_dir);
    let df_path = format!("{build_dir}/Dockerfile");
    let _ = fs::write(&df_path, &fel.dockerfile);
    let build_cfg = BuildConfig {
        dockerfile: df_path,
        context: build_dir.to_string(),
        tag: env_tag.clone(),
        build_args: vec![("CONTAINER_BASE".to_string(), effective_base.to_string())],
        // Deployment-image rebuild during unfreeze is morloc-manager's own
        // bootstrap step; no user flag-file applies here.
        extra_flags: Vec::new(),
    };
    let (status, _, build_err) = container_build(engine, &build_cfg);
    if status.success() {
        env_tag
    } else {
        eprintln!(
            "Warning: env rebuild failed, falling back to base image: {build_err}"
        );
        effective_base.to_string()
    }
}

// ======================================================================
// Apptainer instance backend
// ======================================================================

/// Start a long-running morloc serve instance under Apptainer.
///
/// Apptainer has no NAT; host_port and container_port must match. If they
/// differ, return a clear error rather than silently rewriting either side.
/// All other RunConfig flags translate per the table in build_apptainer_args.
fn serve_apptainer_instance(
    verbose: bool,
    image: &str,
    data_dir: &str,
    instance_name: &str,
    ports: &[(u16, u16)],
    extra_flags: &[String],
    user_env: &[(String, String)],
) -> Result<()> {
    // Stop any leftover instance with this name from a previous run.
    let _ = apptainer_instance_stop(instance_name);

    for (h, c) in ports {
        if h != c {
            return Err(ManagerError::EnvError(format!(
                "Apptainer uses host networking; port mapping -p {h}:{c} cannot be \
                 rewritten. Either configure the nexus to bind to port {h} directly, \
                 or use -p {c}:{c} (matching host:container)."
            )));
        }
    }

    let port_str: Vec<String> = ports
        .iter()
        .map(|(h, c)| format!("{h}:{c}"))
        .collect();
    eprintln!(
        "Starting serve instance {instance_name} on ports {}...",
        port_str.join(", ")
    );
    eprintln!(
        "[morloc-manager] note: apptainer uses host networking; nexus binds directly \
         to host:{}",
        ports.first().map(|(h, _)| *h).unwrap_or(8080)
    );

    let mh = CONTAINER_MORLOC_HOME;

    let exe = engine_executable(ContainerEngine::Apptainer);
    let mut argv: Vec<String> = vec!["instance".to_string(), "start".to_string()];
    argv.push("--bind".to_string());
    argv.push(format!("{data_dir}:{mh}"));
    argv.push("--env".to_string());
    argv.push(format!(
        "PATH={mh}/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
    ));
    argv.push("--env".to_string());
    argv.push(format!("MORLOC_HOME={mh}"));
    for (k, v) in user_env {
        argv.push("--env".to_string());
        argv.push(format!("{k}={v}"));
    }
    for f in extra_flags {
        argv.push(f.clone());
    }
    argv.push(image.to_string());
    argv.push(instance_name.to_string());
    // After the instance name, args are forwarded to the image's startscript.
    // The morloc image's startscript can dispatch to `morloc-nexus --router`
    // by reading these args, or the user supplies a custom %startscript in
    // their .def. For the MVP we pass the standard router args.
    argv.push("morloc-nexus".to_string());
    argv.push("--router".to_string());
    argv.push("--fdb".to_string());
    argv.push(format!("{mh}/fdb"));
    argv.push("--http-port".to_string());
    argv.push(ports.first().map(|(_, c)| c.to_string()).unwrap_or_else(|| "8080".to_string()));

    if verbose {
        let quoted: Vec<String> = argv
            .iter()
            .map(|a| if a.contains(' ') { format!("'{a}'") } else { a.clone() })
            .collect();
        eprintln!("[morloc-manager] {exe} {}", quoted.join(" "));
    }

    let status = Command::new(exe)
        .args(&argv)
        .stdin(Stdio::null())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .status()
        .map_err(|e| ManagerError::EnvError(format!("Failed to spawn apptainer: {e}")))?;
    if !status.success() {
        return Err(ManagerError::EngineError {
            engine: ContainerEngine::Apptainer,
            code: exit_code_to_int(status),
            stderr: "apptainer instance start failed (see output above)".to_string(),
        });
    }

    eprintln!("Instance {instance_name} started");
    eprintln!("  Logs:   morloc-manager logs");
    eprintln!("  Stop:   morloc-manager stop");
    eprintln!("  Status: morloc-manager status");
    Ok(())
}

/// Stop a running Apptainer instance by name. Returns Ok(()) when no such
/// instance exists -- mirrors the pre-emptive cleanup model used by the OCI
/// path.
pub fn apptainer_instance_stop(name: &str) -> Result<()> {
    let exe = engine_executable(ContainerEngine::Apptainer);
    let _ = Command::new(exe)
        .args(["instance", "stop", name])
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status();
    Ok(())
}

/// List Apptainer instances whose names begin with the morloc serve prefix.
/// Parses `apptainer instance list --json`.
pub fn apptainer_list_serve_instances() -> Vec<ServeContainerInfo> {
    let exe = engine_executable(ContainerEngine::Apptainer);
    let output = Command::new(exe)
        .args(["instance", "list", "--json"])
        .current_dir("/tmp")
        .output();
    let Ok(o) = output else {
        return Vec::new();
    };
    if !o.status.success() {
        return Vec::new();
    }
    // Apptainer's JSON is `{"instances": [{"instance": "name", ...}, ...]}`.
    let text = String::from_utf8_lossy(&o.stdout);
    let parsed: serde_json::Value = match serde_json::from_str(&text) {
        Ok(v) => v,
        Err(_) => return Vec::new(),
    };
    let prefix = serve_container_prefix();
    let mut result = Vec::new();
    if let Some(arr) = parsed.get("instances").and_then(|v| v.as_array()) {
        for entry in arr {
            let name = entry
                .get("instance")
                .and_then(|v| v.as_str())
                .unwrap_or("")
                .to_string();
            if !name.starts_with(&prefix) {
                continue;
            }
            let env = env_name_from_container(&name).to_string();
            result.push(ServeContainerInfo {
                name,
                env,
                ports: "-".to_string(),
                status: "Up".to_string(),
            });
        }
    }
    result
}

/// Read the Apptainer per-instance log file. Apptainer has no `logs`
/// subcommand; logs live at
/// `~/.apptainer/instances/logs/<host>/<user>/<instance>/<instance>.{out,err}`.
///
/// `follow` is honored on a best-effort basis: if true, we tail -f the .out
/// file (mirroring the OCI path which interleaves stdout/stderr to stdout).
pub fn apptainer_logs(instance_name: &str, follow: bool) -> Result<()> {
    // Read the kernel-exported hostname directly. Cheaper than spawning
    // /bin/hostname and avoids dragging in a new nix feature.
    let host = fs::read_to_string("/proc/sys/kernel/hostname")
        .map(|s| s.trim().to_string())
        .unwrap_or_else(|_| "localhost".to_string());
    let user = std::env::var("USER")
        .or_else(|_| std::env::var("LOGNAME"))
        .unwrap_or_else(|_| "unknown".to_string());
    let home = dirs::home_dir().ok_or_else(|| {
        ManagerError::EnvError("Cannot determine home directory".to_string())
    })?;
    let log_dir = home
        .join(".apptainer/instances/logs")
        .join(host)
        .join(&user)
        .join(instance_name);
    let out_path = log_dir.join(format!("{instance_name}.out"));
    let err_path = log_dir.join(format!("{instance_name}.err"));

    if !out_path.exists() && !err_path.exists() {
        return Err(ManagerError::EnvError(format!(
            "No log files found for instance '{instance_name}'. \
             Expected under {}",
            log_dir.display()
        )));
    }

    if follow {
        // Use `tail -F` for follow mode; works regardless of file rotation.
        let mut argv = vec!["-F".to_string()];
        if out_path.exists() {
            argv.push(out_path.to_string_lossy().to_string());
        }
        if err_path.exists() {
            argv.push(err_path.to_string_lossy().to_string());
        }
        let status = Command::new("tail")
            .args(&argv)
            .stdin(Stdio::null())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .status()
            .map_err(|e| ManagerError::EnvError(format!("Failed to spawn tail: {e}")))?;
        if !status.success() {
            return Err(ManagerError::EngineError {
                engine: ContainerEngine::Apptainer,
                code: exit_code_to_int(status),
                stderr: "tail failed".to_string(),
            });
        }
        return Ok(());
    }

    // Non-follow: dump out then err to stdout (stderr-as-stdout merge mirrors
    // what the OCI path does via stderr-as-stdout redirection).
    use std::io::Write;
    let stdout = std::io::stdout();
    let mut lock = stdout.lock();
    if let Ok(buf) = fs::read(&out_path) {
        let _ = lock.write_all(&buf);
    }
    if let Ok(buf) = fs::read(&err_path) {
        let _ = lock.write_all(&buf);
    }
    Ok(())
}
