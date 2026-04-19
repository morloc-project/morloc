use std::fs;
use std::path::Path;
use std::process::Command;

use sha2::{Digest, Sha256};

use crate::config;
use crate::container::{
    self, check_remote_image, container_pull, engine_executable,
    exit_code_to_int, image_exists_locally, BuildConfig, RemoteImageStatus,
};
use crate::error::{ManagerError, Result};
use crate::types::*;

// ======================================================================
// Public types
// ======================================================================

/// Options for creating or updating an environment.
/// For `new` (is_new=true): all Option fields that are None use defaults.
/// For `update` (is_new=false): None means keep the existing value.
pub struct ApplyOptions {
    pub name: String,
    pub scope: Scope,
    pub is_new: bool,
    pub base_image: Option<String>,
    pub original_image: Option<String>,
    pub morloc_version: Option<Version>,
    pub dockerfile: Option<String>,
    pub includes: Vec<String>,
    pub flagfile: Option<String>,
    pub engine_args: Vec<String>,
    pub engine: Option<ContainerEngine>,
    pub shm_size: Option<String>,
    pub skip_dockerfile_build: bool,
    pub verbose: bool,
}

/// Info returned by list_environments.
pub struct EnvInfo {
    pub name: String,
    pub morloc_version: Option<Version>,
    pub active: bool,
}

// ======================================================================
// Image resolution
// ======================================================================

const MORLOC_IMAGE_PREFIX: &str = "ghcr.io/morloc-project/morloc/morloc-full";

/// Recognize engine errors that mean "cannot chdir into the current working
/// directory" and rewrite them into a clearer message. This commonly happens
/// when running `sudo -u <other-user> morloc-manager ...` from a directory
/// that <other-user> cannot access (e.g., /root or another user's $HOME).
/// Without this hint, the error bubbles up as "Failed to check registry..."
/// which misleads users toward debugging network/auth problems.
fn cwd_access_hint(stderr: &str) -> Option<String> {
    let lower = stderr.to_lowercase();
    let looks_like_cwd_denied = (lower.contains("chdir") || lower.contains("getwd")
        || lower.contains("current working directory"))
        && (lower.contains("permission denied") || lower.contains("no such file"));
    if looks_like_cwd_denied {
        Some(format!(
            "Cannot change into the current working directory as the target user. \
             Run morloc-manager from a directory the target user can access \
             (for example /tmp or the user's home directory).\nOriginal error: {}",
            stderr.trim()
        ))
    } else {
        None
    }
}

/// Resolve a morloc version string to a registry image reference.
pub fn version_to_image(ver: &Version) -> String {
    format!("{MORLOC_IMAGE_PREFIX}:{}", ver.show())
}

/// Pull an image by tag from the morloc registry, detect its version, and
/// return (image_ref, version). The tag can be a semver string ("0.77.0"),
/// a named tag ("edge", "nightly"), or any other valid container tag.
pub fn pull_tagged_image(engine: ContainerEngine, tag: &str) -> Result<(String, Version)> {
    let image_ref = format!("{MORLOC_IMAGE_PREFIX}:{tag}");

    if !image_exists_locally(engine, &image_ref) {
        match check_remote_image(engine, &image_ref) {
            RemoteImageStatus::Exists => {}
            RemoteImageStatus::NotFound => {
                return Err(ManagerError::EnvError(format!(
                    "No container image found for tag '{tag}'"
                )));
            }
            RemoteImageStatus::Unknown(stderr) => {
                if let Some(hint) = cwd_access_hint(&stderr) {
                    return Err(ManagerError::EnvError(hint));
                }
                return Err(ManagerError::EnvError(format!(
                    "Failed to check registry for tag '{tag}': {}",
                    stderr.trim()
                )));
            }
        }

        eprintln!("Pulling {image_ref}...");
        let (status, _, stderr) = container_pull(engine, &image_ref);
        if !status.success() {
            return Err(ManagerError::EngineError {
                engine,
                code: exit_code_to_int(status),
                stderr,
            });
        }
    } else {
        eprintln!("Using local copy of {image_ref}");
    }

    let ver = detect_morloc_version(engine, &image_ref)?;

    // Also tag with the detected version so future --version lookups find it
    let versioned_image = version_to_image(&ver);
    if versioned_image != image_ref {
        let exe = engine_executable(engine);
        let _ = Command::new(exe)
            .args(["tag", &image_ref, &versioned_image])
            .output();
    }

    Ok((versioned_image, ver))
}

/// Pull the :edge image. Convenience wrapper around pull_tagged_image.
pub fn resolve_latest(engine: ContainerEngine) -> Result<(String, Version)> {
    pull_tagged_image(engine, "edge")
}

/// Pull a specific version image from the morloc registry.
pub fn pull_version_image(engine: ContainerEngine, ver: &Version) -> Result<String> {
    let (img, _) = pull_tagged_image(engine, &ver.show())?;
    Ok(img)
}

/// Detect the morloc version by running `morloc --version` inside the image.
pub fn detect_morloc_version(engine: ContainerEngine, image: &str) -> Result<Version> {
    let exe = engine_executable(engine);
    let output = Command::new(exe)
        .args(["run", "--rm", image, "morloc", "--version"])
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .output()
        .map_err(|e| ManagerError::EnvError(format!("Failed to run container: {e}")))?;

    if !output.status.success() {
        return Err(ManagerError::EnvError(format!(
            "Image '{image}' does not have a working morloc binary: {}",
            String::from_utf8_lossy(&output.stderr).trim()
        )));
    }

    let ver_out = String::from_utf8_lossy(&output.stdout).trim().to_string();
    let ver_str = ver_out.split_whitespace().last().unwrap_or(&ver_out);
    ver_str.parse().map_err(|_| {
        ManagerError::EnvError(format!(
            "Could not parse morloc version from image '{image}' output: {ver_out}"
        ))
    })
}

/// Pull a custom image (not from morloc registry).
pub fn pull_custom_image(engine: ContainerEngine, image: &str) -> Result<()> {
    if image_exists_locally(engine, image) {
        eprintln!("Using local copy of {image}");
        return Ok(());
    }

    eprintln!("Pulling {image}...");
    let (status, _, stderr) = container_pull(engine, image);
    if !status.success() {
        return Err(ManagerError::EngineError {
            engine,
            code: exit_code_to_int(status),
            stderr,
        });
    }
    Ok(())
}

// ======================================================================
// Core operations
// ======================================================================

/// Create or update an environment.
///
/// When `is_new` is true: validates name uniqueness, creates data directories.
/// Validate that an environment name contains only allowed characters.
pub fn validate_env_name(name: &str) -> Result<()> {
    if name.is_empty()
        || !name
            .chars()
            .all(|c| c.is_alphanumeric() || c == '-' || c == '_' || c == '.')
    {
        return Err(ManagerError::EnvError(format!(
            "Invalid environment name '{name}': must contain only alphanumeric characters, hyphens, underscores, or dots"
        )));
    }
    Ok(())
}

/// When `is_new` is false: loads existing config, applies overrides.
pub fn apply_environment(opts: &ApplyOptions) -> Result<()> {
    let scope = opts.scope;
    let name = &opts.name;

    validate_env_name(name)?;

    // Load existing config or start fresh
    let mut ec = if opts.is_new {
        let cfg_path = config::env_config_path(scope, name);
        if cfg_path.is_file() {
            return Err(ManagerError::EnvError(format!(
                "Environment '{name}' already exists"
            )));
        }
        // Create data directories
        let data_dir = config::env_data_dir(scope, name);
        for sub in &["bin", "lib", "fdb", "include", "opt", "tmp"] {
            fs::create_dir_all(data_dir.join(sub)).map_err(|e| {
                ManagerError::EnvError(format!("Failed to create directory: {e}"))
            })?;
        }
        if scope == Scope::System {
            use std::os::unix::fs::PermissionsExt;
            let dirs: Vec<_> = std::iter::once(data_dir.clone())
                .chain(
                    ["bin", "lib", "fdb", "include", "opt", "tmp"]
                        .iter()
                        .map(|d| data_dir.join(d)),
                )
                .collect();
            for d in dirs {
                let _ = fs::set_permissions(&d, fs::Permissions::from_mode(0o2775));
            }
        }
        // Start with required fields from opts; the rest will be applied below
        EnvironmentConfig {
            name: name.clone(),
            base_image: opts.base_image.clone().unwrap_or_default(),
            original_image: None,
            dockerfile: None,
            content_hash: None,
            built_image: None,
            engine: opts.engine.unwrap_or(ContainerEngine::Podman),
            shm_size: "512m".to_string(),
            morloc_version: None,
        }
    } else {
        config::read_env_config(scope, name)
            .map_err(|_| ManagerError::EnvironmentNotFound(name.to_string()))?
    };

    // Apply overrides
    if let Some(ref img) = opts.base_image {
        ec.base_image = img.clone();
    }
    if let Some(ref img) = opts.original_image {
        ec.original_image = Some(img.clone());
    }
    if let Some(ref ver) = opts.morloc_version {
        ec.morloc_version = Some(ver.clone());
    }
    if let Some(engine) = opts.engine {
        ec.engine = engine;
    }
    if let Some(ref shm) = opts.shm_size {
        if !is_valid_shm_size(shm) {
            return Err(ManagerError::EnvError(format!(
                "Invalid --shm-size '{shm}'. Use format like: 512m, 1g, 2048k"
            )));
        }
        ec.shm_size = shm.clone();
    }

    // Copy Dockerfile if a new one was provided
    let dockerfile_changed = if let Some(ref src) = opts.dockerfile {
        let dest = config::env_dockerfile_path(scope, name);
        let dest_dir = dest.parent().unwrap();
        fs::create_dir_all(dest_dir).map_err(|e| {
            ManagerError::EnvError(format!("Failed to create config dir: {e}"))
        })?;
        fs::copy(src, &dest).map_err(|e| {
            ManagerError::EnvError(format!("Failed to copy Dockerfile '{}': {e}", src))
        })?;
        ec.dockerfile = Some("Dockerfile".to_string());
        true
    } else {
        false
    };

    // Copy included files/directories into build context
    let cfg_dir = config::env_config_dir(scope, name);
    fs::create_dir_all(&cfg_dir).map_err(|e| {
        ManagerError::EnvError(format!("Failed to create config dir: {e}"))
    })?;
    for src in &opts.includes {
        let src_path = Path::new(src);
        let file_name = src_path.file_name().ok_or_else(|| {
            ManagerError::EnvError(format!("Invalid include path: {src}"))
        })?;
        let dest = cfg_dir.join(file_name);
        if src_path.is_dir() {
            let status = Command::new("cp")
                .args(["-a", src, &dest.to_string_lossy()])
                .stdin(std::process::Stdio::null())
                .stdout(std::process::Stdio::null())
                .stderr(std::process::Stdio::inherit())
                .status()
                .map_err(|e| ManagerError::EnvError(format!("Failed to copy '{src}': {e}")))?;
            if !status.success() {
                return Err(ManagerError::EnvError(format!(
                    "Failed to copy directory '{src}'"
                )));
            }
        } else {
            fs::copy(src_path, &dest).map_err(|e| {
                ManagerError::EnvError(format!("Failed to copy '{src}': {e}"))
            })?;
        }
    }

    // Write flags file: for new envs or when flagfile is provided, write fresh.
    // For updates with only engine_args, append to existing.
    let flags_path = config::env_flags_path(scope, name);
    if opts.is_new || opts.flagfile.is_some() {
        let mut flag_lines: Vec<String> = Vec::new();
        if let Some(ref src) = opts.flagfile {
            let content = fs::read_to_string(src).map_err(|e| {
                ManagerError::EnvError(format!("Failed to read flagfile '{}': {e}", src))
            })?;
            flag_lines.extend(
                content
                    .lines()
                    .map(|l| l.trim().to_string())
                    .filter(|l| !l.is_empty() && !l.starts_with('#')),
            );
        }
        flag_lines.extend(opts.engine_args.iter().cloned());
        let flags_content = if flag_lines.is_empty() {
            String::new()
        } else {
            flag_lines.join("\n") + "\n"
        };
        fs::write(&flags_path, &flags_content).map_err(|e| {
            ManagerError::EnvError(format!("Failed to write flags file: {e}"))
        })?;
    } else if !opts.engine_args.is_empty() {
        // Append engine_args to existing flags file
        let mut existing = config::read_flags_file_lines(&flags_path);
        existing.extend(opts.engine_args.iter().cloned());
        let flags_content = existing.join("\n") + "\n";
        fs::write(&flags_path, &flags_content).map_err(|e| {
            ManagerError::EnvError(format!("Failed to write flags file: {e}"))
        })?;
    }

    // Build Dockerfile layer if present and not skipped
    let has_dockerfile = ec.dockerfile.is_some();
    let should_build = has_dockerfile
        && !opts.skip_dockerfile_build
        && (opts.is_new || dockerfile_changed || !opts.includes.is_empty()
            || opts.base_image.is_some() || opts.engine.is_some()
            // For update with no specific changes, rebuild if Dockerfile exists
            || (!opts.is_new && opts.dockerfile.is_none() && opts.includes.is_empty()));

    if should_build {
        let tag = format!("localhost/morloc-env:{name}");
        let df_path = config::env_dockerfile_path(scope, name);
        if df_path.exists() {
            let hash = hash_file(&df_path)?;
            // Skip rebuild when nothing has actually changed: same Dockerfile
            // hash, no new includes, no base-image change, tagged image still
            // present. Without this, `update` with no arguments silently
            // re-runs the full build every time.
            let unchanged = !opts.is_new
                && !dockerfile_changed
                && opts.includes.is_empty()
                && opts.base_image.is_none()
                && ec.content_hash.as_deref() == Some(hash.as_str())
                && ec.built_image.as_ref()
                    .map(|img| image_exists_locally(ec.engine, img))
                    .unwrap_or(false);
            if unchanged {
                eprintln!("Dockerfile unchanged; skipping rebuild.");
            } else {
                let build_cfg = BuildConfig {
                    dockerfile: df_path.to_string_lossy().to_string(),
                    context: cfg_dir.to_string_lossy().to_string(),
                    tag: tag.clone(),
                    build_args: vec![("CONTAINER_BASE".to_string(), ec.base_image.clone())],
                };
                if opts.verbose {
                    let exe = engine_executable(ec.engine);
                    eprintln!(
                        "[morloc-manager] {exe} build -f {} -t {} {}",
                        build_cfg.dockerfile, build_cfg.tag, build_cfg.context
                    );
                }
                let (status, _, stderr) = container::container_build(ec.engine, &build_cfg);
                if !status.success() {
                    return Err(ManagerError::EngineError {
                        engine: ec.engine,
                        code: exit_code_to_int(status),
                        stderr,
                    });
                }
                ec.built_image = Some(tag);
                ec.content_hash = Some(hash);
            }
        }
    }

    // Always reconcile the stored morloc version against the actual image.
    // - For `new --version 0.77.0-rc.6`, the binary reports "0.77.0" (stack
    //   does not expose prerelease tags), so keep the recorded value when
    //   major.minor.patch match — the recorded tag is more informative.
    // - For `new --image <custom>` or `update --image ...`, nothing was
    //   recorded yet, so store the detected version.
    // - If the image has no morloc binary (e.g., a bare base image staged
    //   for a Dockerfile layer not yet built), silently leave the field
    //   unchanged rather than failing the whole operation.
    let detect_target = ec.built_image.clone().unwrap_or_else(|| ec.base_image.clone());
    if !detect_target.is_empty() {
        if let Ok(detected) = detect_morloc_version(ec.engine, &detect_target) {
            ec.morloc_version = Some(match ec.morloc_version.take() {
                Some(recorded) if recorded.major == detected.major
                    && recorded.minor == detected.minor
                    && recorded.patch == detected.patch => recorded,
                _ => detected,
            });
        }
    }

    // Write environment config
    config::write_env_config(scope, name, &ec)?;

    Ok(())
}

/// Remove an environment and all its data.
pub fn remove_environment(engine: ContainerEngine, scope: Scope, name: &str) -> Result<()> {
    let ec = config::read_env_config(scope, name)
        .map_err(|_| ManagerError::EnvironmentNotFound(name.to_string()))?;

    // Stop and remove any running serve container for this environment before
    // removing its image. If we skipped this, the serve container would keep
    // running and be unreachable through morloc-manager.
    let serve_name = format!("morloc-serve-{name}");
    if container::container_exists(engine, &serve_name) {
        let _ = container::container_stop(engine, &serve_name);
        let _ = container::container_remove_quiet(engine, &serve_name);
    }

    // Remove built Dockerfile layer image
    if let Some(ref img) = ec.built_image {
        if image_exists_locally(engine, img) {
            container::remove_image(engine, img);
        }
    }

    // Remove config directory
    let cfg_dir = config::env_config_dir(scope, name);
    if cfg_dir.is_dir() {
        let _ = fs::remove_dir_all(&cfg_dir);
    }

    // Remove data directory
    let data_dir = config::env_data_dir(scope, name);
    if data_dir.is_dir() {
        let _ = fs::remove_dir_all(&data_dir);
    }

    // If the active env was this one, clear it in both local and system configs
    for cfg_scope in [Scope::Local, Scope::System] {
        let cfg_path = config::config_path(cfg_scope);
        if let Ok(cfg) = config::read_config::<Config>(&cfg_path) {
            if cfg.active_env.as_deref() == Some(name) {
                let new_cfg = Config {
                    active_env: None,
                    ..cfg
                };
                let _ = config::write_config(&cfg_path, &new_cfg);
            }
        }
    }

    Ok(())
}

/// List environments in the given scope.
pub fn list_environments(scope: Scope, active_env: Option<&str>) -> Vec<EnvInfo> {
    let names = config::list_env_names(scope);
    let mut result = Vec::new();
    for name in names {
        if let Ok(ec) = config::read_env_config(scope, &name) {
            result.push(EnvInfo {
                name: name.clone(),
                morloc_version: ec.morloc_version,
                active: active_env == Some(name.as_str()),
            });
        }
    }
    result
}

/// Select an environment by writing active_env to the given write_scope config.
pub fn select_environment(name: &str, write_scope: Scope) -> Result<()> {
    // Verify the environment exists somewhere
    config::find_env_scope(name)?;

    let cfg_path = config::config_path(write_scope);
    let base_cfg = config::read_config::<Config>(&cfg_path)
        .or_else(|_| config::read_config::<Config>(&config::config_path(Scope::System)))
        .unwrap_or_default();
    let new_cfg = Config {
        active_env: Some(name.to_string()),
        ..base_cfg
    };
    config::write_config(&cfg_path, &new_cfg)
}

/// Resolve the active environment. Checks local config first, then system.
/// Returns (name, scope where env config lives, EnvironmentConfig).
pub fn resolve_active_environment() -> Result<(String, Scope, EnvironmentConfig)> {
    // Find active_env name from config (local first, then system)
    let name = resolve_active_env_name()?;

    // Find which scope has the environment config
    let scope = config::find_env_scope(&name)?;
    let ec = config::read_env_config(scope, &name)?;
    Ok((name, scope, ec))
}

/// Resolve just the active environment name from config.
/// Skips names that don't resolve to an actual environment (e.g., stale
/// entries from old config formats).
fn resolve_active_env_name() -> Result<String> {
    if let Ok(cfg) = config::read_config::<Config>(&config::config_path(Scope::Local)) {
        if let Some(ref name) = cfg.active_env {
            if config::find_env_scope(name).is_ok() {
                return Ok(name.clone());
            }
        }
    }
    if let Ok(cfg) = config::read_config::<Config>(&config::config_path(Scope::System)) {
        if let Some(ref name) = cfg.active_env {
            if config::find_env_scope(name).is_ok() {
                return Ok(name.clone());
            }
        }
    }
    // Check if any environments exist to give a better suggestion
    let local_envs = config::list_env_names(Scope::Local);
    let system_envs = config::list_env_names(Scope::System);
    if local_envs.is_empty() && system_envs.is_empty() {
        Err(ManagerError::NoActiveEnvironment)
    } else {
        // Label each entry with its scope so same-named envs are distinguishable.
        // System envs are flagged with --system to disambiguate in select.
        let mut available: Vec<String> = local_envs
            .iter()
            .map(|n| format!("{n} (local)"))
            .collect();
        available.extend(system_envs.iter().map(|n| format!("{n} (system)")));
        Err(ManagerError::EnvError(format!(
            "No active environment. Select one with: morloc-manager select <name>\n\
             Available: {}",
            available.join(", ")
        )))
    }
}

// ======================================================================
// Internal
// ======================================================================

pub fn is_valid_shm_size(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }
    let (digits, suffix) = if s.ends_with(|c: char| "bkmgBKMG".contains(c)) {
        (&s[..s.len() - 1], true)
    } else {
        (s, false)
    };
    !digits.is_empty() && digits.chars().all(|c| c.is_ascii_digit()) && (suffix || !digits.is_empty())
}

fn hash_file(path: &Path) -> Result<String> {
    let contents = fs::read(path).map_err(|e| {
        ManagerError::EnvError(format!("Failed to read file: {e}"))
    })?;
    let digest = Sha256::digest(&contents);
    Ok(hex_encode(&digest))
}

fn hex_encode(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{b:02x}")).collect()
}
