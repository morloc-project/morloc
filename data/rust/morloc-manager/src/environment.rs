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

/// Options for creating a new environment.
pub struct CreateOptions {
    pub name: String,
    pub base_image: String,
    pub original_image: Option<String>,
    pub morloc_version: Option<Version>,
    pub dockerfile: Option<String>,
    pub flagfile: Option<String>,
    pub engine_args: Vec<String>,
    pub engine: ContainerEngine,
    pub shm_size: String,
    pub scope: Scope,
    /// If true, copy the Dockerfile but don't build it yet.
    pub skip_dockerfile_build: bool,
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

/// Resolve a morloc version string to a registry image reference.
pub fn version_to_image(ver: &Version) -> String {
    format!("{MORLOC_IMAGE_PREFIX}:{}", ver.show())
}

/// Pull the :edge image, detect the version, tag it, and return (image_ref, version).
pub fn resolve_latest(engine: ContainerEngine) -> Result<(String, Version)> {
    let edge_image = format!("{MORLOC_IMAGE_PREFIX}:edge");

    match check_remote_image(engine, &edge_image) {
        RemoteImageStatus::Exists => {}
        RemoteImageStatus::NotFound => {
            return Err(ManagerError::EnvError(
                "No container for latest morloc version exists".to_string(),
            ));
        }
        RemoteImageStatus::Unknown(stderr) => {
            return Err(ManagerError::EnvError(format!(
                "Failed to check registry for latest morloc version: {}",
                stderr.trim()
            )));
        }
    }

    eprintln!("Pulling {edge_image}...");
    let (status, _, stderr) = container_pull(engine, &edge_image);
    if !status.success() {
        return Err(ManagerError::EngineError {
            engine,
            code: exit_code_to_int(status),
            stderr,
        });
    }

    let exe = engine_executable(engine);
    let output = Command::new(exe)
        .args(["run", "--rm", &edge_image, "morloc", "--version"])
        .output()
        .map_err(|e| ManagerError::EnvError(format!("Failed to run edge container: {e}")))?;

    if !output.status.success() {
        return Err(ManagerError::EnvError(format!(
            "Failed to detect morloc version from edge image: {}",
            String::from_utf8_lossy(&output.stderr)
        )));
    }

    let ver_out = String::from_utf8_lossy(&output.stdout).trim().to_string();
    let ver_str = ver_out.split_whitespace().last().unwrap_or(&ver_out);
    let ver: Version = ver_str.parse().map_err(|_| {
        ManagerError::EnvError(format!(
            "Could not parse version from edge image output: {ver_out}"
        ))
    })?;

    // Tag edge image with actual version
    let versioned_image = version_to_image(&ver);
    let _ = Command::new(exe)
        .args(["tag", &edge_image, &versioned_image])
        .output();

    Ok((versioned_image, ver))
}

/// Pull a specific version image from the morloc registry.
pub fn pull_version_image(engine: ContainerEngine, ver: &Version) -> Result<String> {
    let image_ref = version_to_image(ver);

    if image_exists_locally(engine, &image_ref) {
        eprintln!("Using local copy of {image_ref}");
        return Ok(image_ref);
    }

    match check_remote_image(engine, &image_ref) {
        RemoteImageStatus::Exists => {}
        RemoteImageStatus::NotFound => {
            return Err(ManagerError::EnvError(format!(
                "No container for morloc v{} exists",
                ver.show()
            )));
        }
        RemoteImageStatus::Unknown(stderr) => {
            return Err(ManagerError::EnvError(format!(
                "Failed to check registry for morloc v{}: {}",
                ver.show(),
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

    Ok(image_ref)
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

/// Create a new environment.
pub fn create_environment(opts: &CreateOptions) -> Result<()> {
    let scope = opts.scope;
    let name = &opts.name;

    // Validate name
    if name.is_empty()
        || !name
            .chars()
            .all(|c| c.is_alphanumeric() || c == '-' || c == '_' || c == '.')
    {
        return Err(ManagerError::EnvError(format!(
            "Invalid environment name '{name}': must contain only alphanumeric characters, hyphens, underscores, or dots"
        )));
    }

    // Check if already exists
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

    // Copy Dockerfile if provided
    let dockerfile_name = if let Some(src) = &opts.dockerfile {
        let dest = config::env_dockerfile_path(scope, name);
        let dest_dir = dest.parent().unwrap();
        fs::create_dir_all(dest_dir).map_err(|e| {
            ManagerError::EnvError(format!("Failed to create config dir: {e}"))
        })?;
        fs::copy(src, &dest).map_err(|e| {
            ManagerError::EnvError(format!("Failed to copy Dockerfile '{}': {e}", src))
        })?;
        Some("Dockerfile".to_string())
    } else {
        None
    };

    // Write flags file
    let flags_path = config::env_flags_path(scope, name);
    let flags_dir = flags_path.parent().unwrap();
    fs::create_dir_all(flags_dir).map_err(|e| {
        ManagerError::EnvError(format!("Failed to create config dir: {e}"))
    })?;
    let mut flag_lines: Vec<String> = Vec::new();
    if let Some(src) = &opts.flagfile {
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

    // Build Dockerfile layer if present (skip if stub-only)
    let (built_image, content_hash) = if dockerfile_name.is_some() && !opts.skip_dockerfile_build {
        let tag = format!("localhost/morloc-env:{name}");
        let df_path = config::env_dockerfile_path(scope, name);
        let hash = hash_file(&df_path)?;
        let cfg_dir = config::env_config_dir(scope, name);
        let build_cfg = BuildConfig {
            dockerfile: df_path.to_string_lossy().to_string(),
            context: cfg_dir.to_string_lossy().to_string(),
            tag: tag.clone(),
            build_args: vec![("CONTAINER_BASE".to_string(), opts.base_image.clone())],
        };
        let (status, _, stderr) = container::container_build(opts.engine, &build_cfg);
        if !status.success() {
            return Err(ManagerError::EngineError {
                engine: opts.engine,
                code: exit_code_to_int(status),
                stderr,
            });
        }
        (Some(tag), Some(hash))
    } else {
        (None, None)
    };

    // Write environment config
    let ec = EnvironmentConfig {
        name: name.clone(),
        base_image: opts.base_image.clone(),
        original_image: opts.original_image.clone(),
        dockerfile: dockerfile_name,
        content_hash,
        built_image,
        engine: opts.engine,
        shm_size: opts.shm_size.clone(),
        morloc_version: opts.morloc_version,
    };
    config::write_env_config(scope, name, &ec)?;

    Ok(())
}

/// Remove an environment and all its data.
pub fn remove_environment(engine: ContainerEngine, scope: Scope, name: &str) -> Result<()> {
    let ec = config::read_env_config(scope, name)
        .map_err(|_| ManagerError::EnvironmentNotFound(name.to_string()))?;

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

    // If the active env was this one, clear it
    let local_cfg_path = config::config_path(Scope::Local);
    if let Ok(cfg) = config::read_config::<Config>(&local_cfg_path) {
        if cfg.active_env.as_deref() == Some(name) {
            let new_cfg = Config {
                active_env: None,
                ..cfg
            };
            let _ = config::write_config(&local_cfg_path, &new_cfg);
        }
    }

    Ok(())
}

/// Rebuild an environment's Dockerfile layer from its current Dockerfile.
pub fn rebuild_environment(scope: Scope, name: &str) -> Result<()> {
    let mut ec = config::read_env_config(scope, name)
        .map_err(|_| ManagerError::EnvironmentNotFound(name.to_string()))?;

    let df_path = config::env_dockerfile_path(scope, name);
    if !df_path.exists() {
        return Err(ManagerError::EnvError(format!(
            "Environment '{name}' has no Dockerfile to build"
        )));
    }

    let tag = format!("localhost/morloc-env:{name}");
    let hash = hash_file(&df_path)?;
    let cfg_dir = config::env_config_dir(scope, name);
    let build_cfg = BuildConfig {
        dockerfile: df_path.to_string_lossy().to_string(),
        context: cfg_dir.to_string_lossy().to_string(),
        tag: tag.clone(),
        build_args: vec![("CONTAINER_BASE".to_string(), ec.base_image.clone())],
    };
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
    ec.dockerfile = Some("Dockerfile".to_string());
    config::write_env_config(scope, name, &ec)
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
    Err(ManagerError::NoActiveEnvironment)
}

// ======================================================================
// Internal
// ======================================================================

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
