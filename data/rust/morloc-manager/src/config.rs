use std::fs;
use std::os::unix::fs::{OpenOptionsExt, PermissionsExt};
use std::path::{Path, PathBuf};

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

pub fn version_config_dir(scope: Scope, ver: Version) -> PathBuf {
    config_dir(scope).join("versions").join(ver.show())
}

pub fn version_config_path(scope: Scope, ver: Version) -> PathBuf {
    version_config_dir(scope, ver).join("config.json")
}

pub fn environment_config_path(scope: Scope, ver: Version, env_name: &str) -> PathBuf {
    version_config_dir(scope, ver)
        .join("environments")
        .join(format!("{env_name}.json"))
}

pub fn data_dir(scope: Scope) -> PathBuf {
    match scope {
        Scope::Local => dirs::data_dir()
            .unwrap_or_else(|| PathBuf::from("~/.local/share"))
            .join("morloc"),
        Scope::System => PathBuf::from("/usr/local/share/morloc"),
    }
}

pub fn version_data_dir(scope: Scope, ver: Version) -> PathBuf {
    data_dir(scope).join("versions").join(ver.show())
}

pub fn deps_dir(scope: Scope) -> PathBuf {
    data_dir(scope).join("deps")
}

// Workspace paths

pub fn workspace_config_dir(scope: Scope, name: &str) -> PathBuf {
    config_dir(scope).join("workspaces").join(name)
}

pub fn workspace_config_path(scope: Scope, name: &str) -> PathBuf {
    workspace_config_dir(scope, name).join("config.json")
}

pub fn workspace_data_dir(scope: Scope, name: &str) -> PathBuf {
    data_dir(scope).join("workspaces").join(name)
}

pub fn list_workspaces(scope: Scope) -> Vec<String> {
    let ws_dir = config_dir(scope).join("workspaces");
    if !ws_dir.is_dir() {
        return Vec::new();
    }
    let Ok(entries) = fs::read_dir(&ws_dir) else {
        return Vec::new();
    };
    entries
        .filter_map(|e| e.ok())
        .filter(|e| e.path().join("config.json").is_file())
        .filter_map(|e| e.file_name().into_string().ok())
        .collect()
}

// Flags paths

pub fn global_flags_path(scope: Scope) -> PathBuf {
    data_dir(scope).join("morloc.flags")
}

pub fn env_flags_path(scope: Scope, ver: Version, env_name: &str) -> PathBuf {
    version_config_dir(scope, ver)
        .join("environments")
        .join(format!("{env_name}.flags"))
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

pub fn read_active_config() -> Option<Config> {
    let local_path = config_path(Scope::Local);
    if let Ok(cfg) = read_config::<Config>(&local_path) {
        return Some(cfg);
    }
    let system_path = config_path(Scope::System);
    read_config::<Config>(&system_path).ok()
}

pub fn read_version_config(scope: Scope, ver: Version) -> Result<VersionConfig> {
    read_config(&version_config_path(scope, ver))
}

pub fn read_environment_config(
    scope: Scope,
    ver: Version,
    env_name: &str,
) -> Result<EnvironmentConfig> {
    read_config(&environment_config_path(scope, ver, env_name))
}

pub fn read_workspace_config(scope: Scope, name: &str) -> Result<WorkspaceConfig> {
    read_config(&workspace_config_path(scope, name))
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

pub fn write_version_config(scope: Scope, ver: Version, vc: &VersionConfig) -> Result<()> {
    write_config(&version_config_path(scope, ver), vc)
}

pub fn write_environment_config(
    scope: Scope,
    ver: Version,
    env_name: &str,
    ec: &EnvironmentConfig,
) -> Result<()> {
    write_config(&environment_config_path(scope, ver, env_name), ec)
}

pub fn write_workspace_config(scope: Scope, name: &str, wc: &WorkspaceConfig) -> Result<()> {
    write_config(&workspace_config_path(scope, name), wc)
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
        .map_err(|e| ManagerError::ConfigParseError {
            path: lock_path.to_string(),
            msg: format!("Failed to open lock file: {e}"),
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
        .map(|s| s.to_string())
        .collect()
}

// ======================================================================
// Scope utilities
// ======================================================================

pub fn find_installed_scope(ver: Version) -> Option<Scope> {
    let local_path = version_config_path(Scope::Local, ver);
    if local_path.is_file() {
        return Some(Scope::Local);
    }
    let sys_path = version_config_path(Scope::System, ver);
    if sys_path.is_file() {
        return Some(Scope::System);
    }
    None
}

pub fn find_workspace_scope(name: &str) -> Option<Scope> {
    let local_path = workspace_config_path(Scope::Local, name);
    if local_path.is_file() {
        return Some(Scope::Local);
    }
    let sys_path = workspace_config_path(Scope::System, name);
    if sys_path.is_file() {
        return Some(Scope::System);
    }
    None
}

pub fn require_scope_config(scope: Scope) -> Result<Config> {
    let path = config_path(scope);
    match read_config::<Config>(&path) {
        Ok(cfg) => Ok(cfg),
        Err(ManagerError::ConfigNotFound(_)) => Err(ManagerError::SetupNotComplete(scope)),
        Err(err) => Err(err),
    }
}

// ======================================================================
// Internal
// ======================================================================

fn best_effort_chmod(path: &Path, mode: u32) {
    let _ = fs::set_permissions(path, fs::Permissions::from_mode(mode));
}

