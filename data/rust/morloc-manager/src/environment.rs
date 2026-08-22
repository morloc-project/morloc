use std::fs;
use std::process::Command;

use serde::Serialize;

use crate::config;
use crate::container::{self, engine_executable, image_exists_locally};
use crate::error::{ManagerError, Result};
use crate::serve;
use crate::types::*;

// ======================================================================
// Public types
// ======================================================================


/// Info returned by list_environments.
#[derive(Serialize)]
pub struct EnvInfo {
    pub name: String,
    pub morloc_version: Option<Version>,
    pub is_default: bool,
}

// ======================================================================
// Image resolution
// ======================================================================










/// Detect the morloc version by running `morloc --version` inside the image.
/// For docker/podman this uses `<engine> run --rm <ref>`; for apptainer it
/// uses `apptainer exec <sif-path>`.
pub fn detect_morloc_version(engine: ContainerEngine, image: &str) -> Result<Version> {
    let exe = engine_executable(engine);
    let argv: Vec<&str> = match engine {
        ContainerEngine::Docker | ContainerEngine::Podman => {
            vec!["run", "--rm", image, "morloc", "--version"]
        }
        ContainerEngine::Apptainer => vec!["exec", image, "morloc", "--version"],
    };
    let output = Command::new(exe)
        .args(&argv)
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




/// Remove an environment and all its data.
pub fn remove_environment(engine: ContainerEngine, scope: Scope, name: &str) -> Result<()> {
    let ec = config::read_env_config(scope, name)
        .map_err(|_| ManagerError::EnvironmentNotFound(name.to_string()))?;

    // Stop and remove any running serve container for this environment before
    // removing its image. If we skipped this, the serve container would keep
    // running and be unreachable through morloc-manager.
    let serve_name = serve::serve_container_name(name);
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

    // If a config's default tag pointed at this name AND no env of that name
    // survives in any scope, clear the tag. The name-still-resolves guard avoids
    // over-clearing when a same-named env remains in the other scope (e.g. a
    // local `default` is removed while a system `default` — the common case now
    // that no-name `new` uses the constant name `default` — still exists).
    let name_orphaned = config::find_env_scope(name).is_err();
    if name_orphaned {
        for cfg_scope in [Scope::Local, Scope::System] {
            let cfg_path = config::config_path(cfg_scope);
            if let Ok(cfg) = config::read_config::<Config>(&cfg_path) {
                if cfg.default_env.as_deref() == Some(name) {
                    let new_cfg = Config {
                        default_env: None,
                        ..cfg
                    };
                    let _ = config::write_config(&cfg_path, &new_cfg);
                }
            }
        }
    }

    Ok(())
}

/// List environments in the given scope.
pub fn list_environments(scope: Scope, default_env: Option<&str>) -> Vec<EnvInfo> {
    let names = config::list_env_names(scope);
    let mut result = Vec::new();
    for name in names {
        if let Ok(ec) = config::read_env_config(scope, &name) {
            result.push(EnvInfo {
                name: name.clone(),
                morloc_version: ec.morloc_version,
                is_default: default_env == Some(name.as_str()),
            });
        }
    }
    result
}

/// Tag an environment as the default by writing default_env to the given
/// write_scope config.
pub fn set_default_environment(name: &str, write_scope: Scope) -> Result<()> {
    // Verify the environment exists somewhere
    config::find_env_scope(name)?;

    let cfg_path = config::config_path(write_scope);
    let base_cfg = config::read_config::<Config>(&cfg_path)
        .or_else(|_| config::read_config::<Config>(&config::config_path(Scope::System)))
        .unwrap_or_default();
    let new_cfg = Config {
        default_env: Some(name.to_string()),
        ..base_cfg
    };
    config::write_config(&cfg_path, &new_cfg)
}

/// Resolve the default environment. Checks local config first, then system.
/// Returns (name, scope where env config lives, EnvironmentConfig).
pub fn resolve_default_environment() -> Result<(String, Scope, EnvironmentConfig)> {
    // Find default_env name from config (local first, then system)
    let name = resolve_default_env_name()?;

    // Find which scope has the environment config
    let scope = config::find_env_scope(&name)?;
    let ec = config::read_env_config(scope, &name)?;
    Ok((name, scope, ec))
}

/// Resolve just the default environment name from config.
/// Skips names that don't resolve to an actual environment (e.g., stale
/// entries from old config formats).
fn resolve_default_env_name() -> Result<String> {
    if let Ok(cfg) = config::read_config::<Config>(&config::config_path(Scope::Local)) {
        if let Some(ref name) = cfg.default_env {
            if config::find_env_scope(name).is_ok() {
                return Ok(name.clone());
            }
        }
    }
    if let Ok(cfg) = config::read_config::<Config>(&config::config_path(Scope::System)) {
        if let Some(ref name) = cfg.default_env {
            if config::find_env_scope(name).is_ok() {
                return Ok(name.clone());
            }
        }
    }
    // Check if any environments exist to give a better suggestion
    let local_envs = config::list_env_names(Scope::Local);
    let system_envs = config::list_env_names(Scope::System);
    if local_envs.is_empty() && system_envs.is_empty() {
        Err(ManagerError::NoDefaultEnvironment)
    } else {
        // Label each entry with its scope so same-named envs are distinguishable.
        let mut available: Vec<String> = local_envs
            .iter()
            .map(|n| format!("{n} (local)"))
            .collect();
        available.extend(system_envs.iter().map(|n| format!("{n} (system)")));
        Err(ManagerError::EnvError(format!(
            "No default environment set. Pass --env <name>, or set a default with: \
             morloc-manager update --env <name> --set-default\n\
             Available: {}",
            available.join(", ")
        )))
    }
}

// ======================================================================
// Internal
// ======================================================================



