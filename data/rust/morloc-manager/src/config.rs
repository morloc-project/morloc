use std::fs;
use std::os::unix::fs::{OpenOptionsExt, PermissionsExt};
use std::path::{Path, PathBuf};
use std::process::Command as StdCommand;

use crate::error::{ManagerError, Result};
use crate::types::*;

// ======================================================================
// Path utilities
// ======================================================================

pub fn config_dir(scope: Scope) -> PathBuf {
    match scope {
        Scope::Local => dirs::config_dir()
            .unwrap_or_else(|| PathBuf::from("~/.config"))
            .join("morloc"),
        Scope::System => PathBuf::from("/etc/morloc"),
    }
}

pub fn config_path(scope: Scope) -> PathBuf {
    config_dir(scope).join("config.json")
}

pub fn data_dir(scope: Scope) -> PathBuf {
    match scope {
        Scope::Local => dirs::data_dir()
            .unwrap_or_else(|| PathBuf::from("~/.local/share"))
            .join("morloc"),
        Scope::System => PathBuf::from("/usr/local/share/morloc"),
    }
}

// Environment paths

pub fn env_config_dir(scope: Scope, name: &str) -> PathBuf {
    config_dir(scope).join("environments").join(name)
}

/// Canonical path for the env config file (YAML).
pub fn env_config_path(scope: Scope, name: &str) -> PathBuf {
    env_config_dir(scope, name).join("env.yaml")
}

/// Legacy env config path. Read-only fallback for environments created before
/// the YAML switch; new writes always go to env.yaml.
pub fn env_config_path_legacy_json(scope: Scope, name: &str) -> PathBuf {
    env_config_dir(scope, name).join("env.json")
}

pub fn env_dockerfile_path(scope: Scope, name: &str) -> PathBuf {
    env_config_dir(scope, name).join("Dockerfile")
}

/// Path to the Singularity definition file in an env's config directory
/// (Apptainer-engine equivalent of the Dockerfile path).
pub fn env_deffile_path(scope: Scope, name: &str) -> PathBuf {
    env_config_dir(scope, name).join("recipe.def")
}

pub fn env_flags_path(scope: Scope, name: &str) -> PathBuf {
    env_config_dir(scope, name).join("env.flags")
}

pub fn env_data_dir(scope: Scope, name: &str) -> PathBuf {
    data_dir(scope).join("environments").join(name)
}

/// Directory holding the .sif files for an Apptainer environment.
pub fn env_sif_dir(scope: Scope, name: &str) -> PathBuf {
    env_data_dir(scope, name).join("sif")
}

/// Path to the cached base .sif (Apptainer engine only).
pub fn env_base_sif_path(scope: Scope, name: &str) -> PathBuf {
    env_sif_dir(scope, name).join("base.sif")
}

/// Path to the built layered .sif from `update` (Apptainer engine only).
pub fn env_layered_sif_path(scope: Scope, name: &str) -> PathBuf {
    env_sif_dir(scope, name).join("layered.sif")
}

// ======================================================================
// Reading configuration
// ======================================================================

pub fn read_config<T: serde::de::DeserializeOwned>(path: &Path) -> Result<T> {
    let bytes = fs::read(path).map_err(|e| {
        if e.kind() == std::io::ErrorKind::PermissionDenied {
            ManagerError::ConfigPermissionDenied(path.display().to_string())
        } else {
            ManagerError::ConfigNotFound(path.display().to_string())
        }
    })?;
    serde_json::from_slice(&bytes).map_err(|e| ManagerError::ConfigParseError {
        path: path.display().to_string(),
        msg: e.to_string(),
    })
}

/// Read a YAML config file. Falls back through YAML parse, then JSON parse for
/// the same path. Used by env configs to ease migration from env.json.
fn read_yaml_config<T: serde::de::DeserializeOwned>(path: &Path) -> Result<T> {
    let bytes = fs::read(path).map_err(|e| {
        if e.kind() == std::io::ErrorKind::PermissionDenied {
            ManagerError::ConfigPermissionDenied(path.display().to_string())
        } else {
            ManagerError::ConfigNotFound(path.display().to_string())
        }
    })?;
    serde_yaml::from_slice(&bytes).map_err(|e| ManagerError::ConfigParseError {
        path: path.display().to_string(),
        msg: e.to_string(),
    })
}

pub fn read_active_config() -> Option<Config> {
    let local_path = config_path(Scope::Local);
    if let Ok(cfg) = read_config::<Config>(&local_path) {
        return Some(cfg);
    }
    let system_path = config_path(Scope::System);
    read_config::<Config>(&system_path).ok()
}

/// Read an environment's config. Prefers env.yaml; falls back to legacy
/// env.json for envs created before the YAML switch.
pub fn read_env_config(scope: Scope, name: &str) -> Result<EnvironmentConfig> {
    let yaml_path = env_config_path(scope, name);
    if yaml_path.is_file() {
        return read_yaml_config(&yaml_path);
    }
    let json_path = env_config_path_legacy_json(scope, name);
    if json_path.is_file() {
        return read_config(&json_path);
    }
    Err(ManagerError::ConfigNotFound(yaml_path.display().to_string()))
}

// ======================================================================
// Writing configuration
// ======================================================================

pub fn write_config<T: serde::Serialize>(path: &Path, val: &T) -> Result<()> {
    let dir = path.parent().unwrap();
    fs::create_dir_all(dir).map_err(|e| ManagerError::ConfigParseError {
        path: path.display().to_string(),
        msg: e.to_string(),
    })?;
    best_effort_chmod(dir, 0o755);

    let lock_path = format!("{}.lock", path.display());
    with_file_lock(&lock_path, || {
        // Atomic write: temp file then rename
        let tmp_path = path.with_extension("tmp");
        let json = serde_json::to_vec(val).map_err(|e| ManagerError::ConfigParseError {
            path: path.display().to_string(),
            msg: e.to_string(),
        })?;
        fs::write(&tmp_path, &json).map_err(|e| ManagerError::ConfigParseError {
            path: path.display().to_string(),
            msg: e.to_string(),
        })?;
        fs::rename(&tmp_path, path).map_err(|e| ManagerError::ConfigParseError {
            path: path.display().to_string(),
            msg: e.to_string(),
        })?;
        best_effort_chmod(path, 0o644);
        Ok(())
    })
}

/// Atomically write a YAML config file with locking, mirroring `write_config`.
fn write_yaml_config<T: serde::Serialize>(path: &Path, val: &T) -> Result<()> {
    let dir = path.parent().unwrap();
    fs::create_dir_all(dir).map_err(|e| ManagerError::ConfigParseError {
        path: path.display().to_string(),
        msg: e.to_string(),
    })?;
    best_effort_chmod(dir, 0o755);

    let lock_path = format!("{}.lock", path.display());
    with_file_lock(&lock_path, || {
        let tmp_path = path.with_extension("tmp");
        let yaml = serde_yaml::to_string(val).map_err(|e| ManagerError::ConfigParseError {
            path: path.display().to_string(),
            msg: e.to_string(),
        })?;
        fs::write(&tmp_path, yaml.as_bytes()).map_err(|e| ManagerError::ConfigParseError {
            path: path.display().to_string(),
            msg: e.to_string(),
        })?;
        fs::rename(&tmp_path, path).map_err(|e| ManagerError::ConfigParseError {
            path: path.display().to_string(),
            msg: e.to_string(),
        })?;
        best_effort_chmod(path, 0o644);
        Ok(())
    })
}

/// Write an environment config to env.yaml. If a legacy env.json exists from
/// before the YAML switch, leave it in place as a safety net; future reads
/// prefer the YAML.
pub fn write_env_config(scope: Scope, name: &str, ec: &EnvironmentConfig) -> Result<()> {
    write_yaml_config(&env_config_path(scope, name), ec)
}

// ======================================================================
// Scope utilities
// ======================================================================

/// Find which scope an environment lives in. Checks local first, then system.
pub fn find_env_scope(name: &str) -> Result<Scope> {
    let local_path = env_config_path(Scope::Local, name);
    if local_path.is_file() {
        return Ok(Scope::Local);
    }
    let sys_path = env_config_path(Scope::System, name);
    if sys_path.is_file() {
        return Ok(Scope::System);
    }
    Err(ManagerError::EnvironmentNotFound(name.to_string()))
}

/// List environment names in a given scope. Recognizes both env.yaml (new) and
/// env.json (legacy) so envs created before the YAML switch still appear.
pub fn list_env_names(scope: Scope) -> Vec<String> {
    let env_dir = config_dir(scope).join("environments");
    if !env_dir.is_dir() {
        return Vec::new();
    }
    let Ok(entries) = fs::read_dir(&env_dir) else {
        return Vec::new();
    };
    entries
        .filter_map(|e| e.ok())
        .filter(|e| {
            let p = e.path();
            p.join("env.yaml").is_file() || p.join("env.json").is_file()
        })
        .filter_map(|e| e.file_name().into_string().ok())
        .collect()
}


// ======================================================================
// Flags files
// ======================================================================

pub fn read_flags_file(path: &Path) -> Vec<String> {
    let Ok(contents) = fs::read_to_string(path) else {
        return Vec::new();
    };
    contents
        .lines()
        .map(|line| line.trim())
        .filter(|line| !line.is_empty() && !line.starts_with('#'))
        .flat_map(shell_expand_line)
        .collect()
}

/// Expand a single flagfile line through the shell, getting glob expansion,
/// environment variable expansion, tilde expansion, and quote handling.
/// Falls back to simple whitespace splitting if the shell invocation fails.
fn shell_expand_line(line: &str) -> Vec<String> {
    let output = StdCommand::new("sh")
        .args(["-c", &format!("printf '%s\\0' {}", line)])
        .output();
    match output {
        Ok(out) if out.status.success() => {
            let stdout = String::from_utf8_lossy(&out.stdout);
            let tokens: Vec<String> = stdout
                .split('\0')
                .filter(|s| !s.is_empty())
                .map(|s| s.to_string())
                .collect();
            if tokens.is_empty() {
                line.split_whitespace().map(|s| s.to_string()).collect()
            } else {
                tokens
            }
        }
        _ => line.split_whitespace().map(|s| s.to_string()).collect(),
    }
}

/// Read flags file preserving one line per entry (for display).
pub fn read_flags_file_lines(path: &Path) -> Vec<String> {
    let Ok(contents) = fs::read_to_string(path) else {
        return Vec::new();
    };
    contents
        .lines()
        .map(|line| line.trim())
        .filter(|line| !line.is_empty() && !line.starts_with('#'))
        .map(|s| s.to_string())
        .collect()
}

// ======================================================================
// File locking
// ======================================================================

fn with_file_lock<F, T>(lock_path: &str, action: F) -> Result<T>
where
    F: FnOnce() -> Result<T>,
{
    if let Some(parent) = Path::new(lock_path).parent() {
        let _ = fs::create_dir_all(parent);
    }
    let file = std::fs::OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(false)
        .mode(0o644)
        .open(lock_path)
        .map_err(|e| {
            if e.kind() == std::io::ErrorKind::PermissionDenied {
                ManagerError::ConfigPermissionDenied(format!(
                    "{}. Use sudo for system-scope operations", lock_path
                ))
            } else {
                ManagerError::ConfigParseError {
                    path: lock_path.to_string(),
                    msg: format!("Failed to open lock file: {e}"),
                }
            }
        })?;

    use nix::fcntl::FlockArg;
    let locked = nix::fcntl::Flock::lock(file, FlockArg::LockExclusive).map_err(
        |(_file, errno)| ManagerError::ConfigParseError {
            path: lock_path.to_string(),
            msg: format!("Failed to acquire lock: {errno}"),
        },
    )?;

    let result = action();

    // Lock is released when Flock is dropped
    drop(locked);
    result
}

// ======================================================================
// Internal
// ======================================================================

fn best_effort_chmod(path: &Path, mode: u32) {
    let _ = fs::set_permissions(path, fs::Permissions::from_mode(mode));
}
