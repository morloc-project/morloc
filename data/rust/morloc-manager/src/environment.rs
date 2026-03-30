use std::fs;
use std::path::Path;

use sha2::{Digest, Sha256};

use crate::config;
use crate::container::{self, BuildConfig};
use crate::error::{ManagerError, Result};
use crate::types::*;

pub fn init_environment(scope: Scope, env_name: &str) -> Result<String> {
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
        ManagerError::InstallError(format!("Failed to create deps dir: {e}"))
    })?;
    let _ = best_effort_chmod(&deps, 0o755);

    let dockerfile_path = deps.join(format!("{env_name}.Dockerfile"));
    if dockerfile_path.exists() {
        return Err(ManagerError::InstallError(format!(
            "Environment already exists: {}",
            dockerfile_path.display()
        )));
    }

    let content = format!(
        "# morloc-manager environment: {env_name}\n\
         # Edit this file to add your dependencies, then run:\n\
         #   morloc-manager env {env_name}\n\
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
    );
    fs::write(&dockerfile_path, &content).map_err(|e| {
        ManagerError::InstallError(format!("Failed to write Dockerfile: {e}"))
    })?;
    let _ = best_effort_chmod(&dockerfile_path, 0o644);

    Ok(dockerfile_path.to_string_lossy().to_string())
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

pub fn activate_environment(scope: Scope, ver: Version, env_name: &str) -> Result<()> {
    config::read_environment_config(scope, ver, env_name)
        .map_err(|_| ManagerError::EnvironmentNotFound(env_name.to_string()))?;

    let cfg_path = config::config_path(scope);
    let cfg: Config = config::read_config(&cfg_path)?;
    let new_cfg = Config {
        active_env: env_name.to_string(),
        ..cfg
    };
    config::write_config(&cfg_path, &new_cfg)
}

pub fn list_environments(scope: Scope, ver: Version) -> Vec<String> {
    let mut envs = Vec::new();

    // Built environments (have .json config)
    let env_dir = config::version_config_dir(scope, ver).join("environments");
    if let Ok(entries) = fs::read_dir(&env_dir) {
        for entry in entries.flatten() {
            let name = entry.file_name().to_string_lossy().to_string();
            if let Some(base) = name.strip_suffix(".json") {
                envs.push(base.to_string());
            }
        }
    }

    // Initialized environments (have .Dockerfile in deps dir)
    let deps = config::deps_dir(scope);
    if let Ok(entries) = fs::read_dir(&deps) {
        for entry in entries.flatten() {
            let name = entry.file_name().to_string_lossy().to_string();
            if let Some(base) = name.strip_suffix(".Dockerfile") {
                if !envs.contains(&base.to_string()) {
                    envs.push(base.to_string());
                }
            }
        }
    }

    let mut result = vec!["base".to_string()];
    result.extend(envs.into_iter().filter(|e| e != "base"));
    result
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
