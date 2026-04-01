use std::fs;
use std::path::Path;

use sha2::{Digest, Sha256};

use crate::config;
use crate::container::{self, BuildConfig};
use crate::error::{ManagerError, Result};
use crate::types::*;

/// Options for initializing a new environment.
pub struct InitOptions {
    /// If Some, copy this Dockerfile instead of generating a stub.
    pub dockerfile: Option<String>,
    /// If Some, copy this file as the env flags file.
    pub flagfile: Option<String>,
    /// Extra engine arguments (each may contain spaces, e.g. "--port 8080:8080").
    pub engine_args: Vec<String>,
    /// Inherited flags from the parent version/environment (empty when --no-inherit).
    pub parent_flags: Vec<String>,
}

pub fn init_environment(scope: Scope, env_name: &str, opts: &InitOptions) -> Result<Vec<String>> {
    // Reject reserved names
    match env_name {
        "base" | "reset" => {
            return Err(ManagerError::EnvError(format!(
                "'{env_name}' is a reserved name and cannot be used as an environment name"
            )));
        }
        _ => {}
    }

    // Validate name against Docker tag rules
    if env_name.is_empty()
        || !env_name
            .chars()
            .all(|c| c.is_alphanumeric() || c == '-' || c == '_' || c == '.')
    {
        return Err(ManagerError::EnvError(format!(
            "Invalid environment name '{env_name}': must contain only alphanumeric characters, hyphens, underscores, or dots"
        )));
    }

    let deps = config::deps_dir(scope);
    fs::create_dir_all(&deps).map_err(|e| {
        ManagerError::EnvError(format!("Failed to create deps dir: {e}"))
    })?;
    let _ = best_effort_chmod(&deps, 0o755);

    let dockerfile_path = deps.join(format!("{env_name}.Dockerfile"));
    if dockerfile_path.exists() {
        return Err(ManagerError::EnvError(format!(
            "Environment already exists: {}",
            dockerfile_path.display()
        )));
    }

    // Write the Dockerfile: copy user-provided or generate a stub
    let content = match &opts.dockerfile {
        Some(src) => {
            fs::read_to_string(src).map_err(|e| {
                ManagerError::EnvError(format!("Failed to read Dockerfile '{}': {e}", src))
            })?
        }
        None => {
            format!(
                "# morloc-manager environment: {env_name}\n\
                 # Edit this file to add your dependencies, then run:\n\
                 #   morloc-manager env select {env_name}\n\
                 \n\
                 ARG CONTAINER_BASE=scratch\n\
                 FROM ${{CONTAINER_BASE}}\n\
                 \n\
                 # Example: install Python packages\n\
                 # RUN pip install scikit-learn pandas\n\
                 \n\
                 # Example: install R packages\n\
                 # RUN R -e \"install.packages('ggplot2', repos='https://cloud.r-project.org')\"\n\
                 \n\
                 # Example: install system packages\n\
                 # RUN apt-get update && apt-get install -y libfoo-dev\n"
            )
        }
    };
    fs::write(&dockerfile_path, &content).map_err(|e| {
        ManagerError::EnvError(format!("Failed to write Dockerfile: {e}"))
    })?;
    let _ = best_effort_chmod(&dockerfile_path, 0o644);

    let mut created_files = vec![dockerfile_path.to_string_lossy().to_string()];

    // Build the combined flags: inherited parent flags + flagfile + engine args
    let mut lines: Vec<String> = opts.parent_flags.clone();
    if let Some(src) = &opts.flagfile {
        let content = fs::read_to_string(src).map_err(|e| {
            ManagerError::EnvError(format!("Failed to read flagfile '{}': {e}", src))
        })?;
        lines.extend(
            content.lines()
                .map(|l| l.trim().to_string())
                .filter(|l| !l.is_empty() && !l.starts_with('#')),
        );
    }
    lines.extend(opts.engine_args.iter().cloned());

    // Always write the flags file (it captures the full resolved set)
    let flags_path = deps.join(format!("{env_name}.flags"));
    let final_content = if lines.is_empty() {
        String::new()
    } else {
        lines.join("\n") + "\n"
    };
    fs::write(&flags_path, &final_content).map_err(|e| {
        ManagerError::EnvError(format!("Failed to write flags file: {e}"))
    })?;
    let _ = best_effort_chmod(&flags_path, 0o644);
    created_files.push(flags_path.to_string_lossy().to_string());

    Ok(created_files)
}

pub fn build_environment(
    engine: ContainerEngine,
    scope: Scope,
    ver: Version,
    env_name: &str,
) -> Result<()> {
    let deps = config::deps_dir(scope);
    let dockerfile_path = deps.join(format!("{env_name}.Dockerfile"));
    if !dockerfile_path.exists() {
        return Err(ManagerError::EnvironmentNotFound(env_name.to_string()));
    }

    let current_hash = hash_file(&dockerfile_path)?;
    let ec_result = config::read_environment_config(scope, ver, env_name);
    let needs_rebuild = match ec_result {
        Ok(ec) => ec.content_hash.as_deref() != Some(current_hash.as_str()),
        Err(_) => true,
    };
    if !needs_rebuild {
        // Verify the image actually exists in the current engine store
        let tag = format!("localhost/morloc-env:{}-{env_name}", ver.show());
        if container::image_exists_locally(engine, &tag) {
            return Ok(());
        }
        // Image missing (e.g. engine switch or manual deletion) - fall through to rebuild
    }

    let vc = config::read_version_config(scope, ver)?;
    let tag = format!("localhost/morloc-env:{}-{env_name}", ver.show());
    let build_cfg = BuildConfig {
        dockerfile: dockerfile_path.to_string_lossy().to_string(),
        context: deps.to_string_lossy().to_string(),
        tag: tag.clone(),
        build_args: vec![("CONTAINER_BASE".to_string(), vc.image)],
    };
    let (status, _stdout, stderr) = container::container_build(engine, &build_cfg);
    if !status.success() {
        return Err(ManagerError::EngineError {
            engine,
            code: container::exit_code_to_int(status),
            stderr,
        });
    }

    let ec = EnvironmentConfig {
        image: tag,
        dockerfile: dockerfile_path.to_string_lossy().to_string(),
        content_hash: Some(current_hash),
    };
    config::write_environment_config(scope, ver, env_name, &ec)
}

pub fn activate_environment(scope: Scope, ver: Version, env_name: &str, write_scope: Scope) -> Result<()> {
    config::read_environment_config(scope, ver, env_name)
        .map_err(|_| ManagerError::EnvironmentNotFound(env_name.to_string()))?;

    let cfg_path = config::config_path(write_scope);
    let cfg: Config = config::read_config(&cfg_path)?;
    let new_cfg = Config {
        active_env: env_name.to_string(),
        ..cfg
    };
    config::write_config(&cfg_path, &new_cfg)
}

/// Remove an environment: delete Dockerfile, flags file, env config, and container image.
pub fn remove_environment(
    engine: ContainerEngine,
    scope: Scope,
    ver: Version,
    env_name: &str,
) -> Result<()> {
    if env_name == "base" {
        return Err(ManagerError::EnvError(
            "Cannot remove the base environment".to_string(),
        ));
    }

    let deps = config::deps_dir(scope);
    let dockerfile_path = deps.join(format!("{env_name}.Dockerfile"));
    let flags_path = deps.join(format!("{env_name}.flags"));
    let env_config_path = config::environment_config_path(scope, ver, env_name);
    let env_flags_config = config::env_flags_path(scope, ver, env_name);

    // Check that something exists to remove
    if !dockerfile_path.exists() && !env_config_path.exists() {
        return Err(ManagerError::EnvironmentNotFound(env_name.to_string()));
    }

    // Remove the container image if it was built
    let tag = format!("localhost/morloc-env:{}-{env_name}", ver.show());
    if container::image_exists_locally(engine, &tag) {
        container::remove_image(engine, &tag);
    }

    // Remove files (best-effort for each)
    let _ = fs::remove_file(&dockerfile_path);
    let _ = fs::remove_file(&flags_path);
    let _ = fs::remove_file(&env_config_path);
    let _ = fs::remove_file(&env_flags_config);

    // If the active env was this one, reset to base
    let cfg_path = config::config_path(Scope::Local);
    if let Ok(cfg) = config::read_config::<Config>(&cfg_path) {
        if cfg.active_env == env_name {
            let new_cfg = Config {
                active_env: "base".to_string(),
                ..cfg
            };
            config::write_config(&cfg_path, &new_cfg)?;
        }
    }

    Ok(())
}

/// Information about a listed environment.
pub struct EnvInfo {
    pub name: String,
    pub scope: Scope,
    pub built: bool,
    pub active: bool,
}

/// List all environments across both scopes, with build status.
pub fn list_environments(active_env: &str, ver: Version) -> Vec<EnvInfo> {
    let mut result = Vec::new();

    // Always include base
    result.push(EnvInfo {
        name: "base".to_string(),
        scope: Scope::Local,
        built: true,
        active: active_env == "base",
    });

    // Collect from both scopes
    for scope in [Scope::System, Scope::Local] {
        let mut seen = std::collections::HashSet::new();

        // Built environments (have .json config)
        let env_dir = config::version_config_dir(scope, ver).join("environments");
        if let Ok(entries) = fs::read_dir(&env_dir) {
            for entry in entries.flatten() {
                let name = entry.file_name().to_string_lossy().to_string();
                if let Some(base) = name.strip_suffix(".json") {
                    if base != "base" {
                        seen.insert(base.to_string());
                        // Skip if we already have this name from system scope
                        if result.iter().any(|e| e.name == base) {
                            continue;
                        }
                        result.push(EnvInfo {
                            name: base.to_string(),
                            scope,
                            built: true,
                            active: active_env == base,
                        });
                    }
                }
            }
        }

        // Initialized but not-yet-built environments (have .Dockerfile in deps dir)
        let deps = config::deps_dir(scope);
        if let Ok(entries) = fs::read_dir(&deps) {
            for entry in entries.flatten() {
                let name = entry.file_name().to_string_lossy().to_string();
                if let Some(base) = name.strip_suffix(".Dockerfile") {
                    if base != "base" && !seen.contains(base) {
                        // Skip if we already have this name from system scope
                        if result.iter().any(|e| e.name == base) {
                            continue;
                        }
                        result.push(EnvInfo {
                            name: base.to_string(),
                            scope,
                            built: false,
                            active: active_env == base,
                        });
                    }
                }
            }
        }
    }

    result
}

/// Find which scope an environment lives in. Prefers explicit --system flag,
/// otherwise checks local first then system.
pub fn find_env_scope(env_name: &str, system: bool) -> Result<Scope> {
    if system {
        // Check system scope only
        let deps = config::deps_dir(Scope::System);
        let dockerfile = deps.join(format!("{env_name}.Dockerfile"));
        if dockerfile.exists() {
            return Ok(Scope::System);
        }
        return Err(ManagerError::EnvironmentNotFound(format!(
            "No system environment named '{env_name}'"
        )));
    }

    // Check local first, then system
    let local_deps = config::deps_dir(Scope::Local);
    if local_deps.join(format!("{env_name}.Dockerfile")).exists() {
        return Ok(Scope::Local);
    }
    let sys_deps = config::deps_dir(Scope::System);
    if sys_deps.join(format!("{env_name}.Dockerfile")).exists() {
        return Ok(Scope::System);
    }

    // Also check env configs (for built envs whose Dockerfile might not exist locally)
    // This shouldn't normally happen but be thorough
    Err(ManagerError::EnvironmentNotFound(env_name.to_string()))
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

fn best_effort_chmod(path: &Path, mode: u32) -> std::io::Result<()> {
    use std::os::unix::fs::PermissionsExt;
    fs::set_permissions(path, fs::Permissions::from_mode(mode))
}
