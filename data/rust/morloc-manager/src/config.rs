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

/// Path to an environment's custom Dockerfile (read by freeze/doctor; not
/// produced by the requirement-derived build).
pub fn env_dockerfile_path(scope: Scope, name: &str) -> PathBuf {
    env_config_dir(scope, name).join("Dockerfile")
}

/// Path to an environment's Singularity .def recipe (apptainer; read by freeze).
pub fn env_deffile_path(scope: Scope, name: &str) -> PathBuf {
    env_config_dir(scope, name).join("recipe.def")
}



/// Path to the YAML-format container flag config file (canonical).
pub fn env_flags_yaml_path(scope: Scope, name: &str) -> PathBuf {
    env_config_dir(scope, name).join("env.flags.yaml")
}

pub fn env_data_dir(scope: Scope, name: &str) -> PathBuf {
    data_dir(scope).join("environments").join(name)
}

/// Path to an environment's exposure config (which modules are served, and how).
/// Lives beside env.yaml; declarative intent, edited by `expose`, realized by
/// `start`. Not mounted into the container.
pub fn env_expose_path(scope: Scope, name: &str) -> PathBuf {
    env_config_dir(scope, name).join("expose.yaml")
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
    let ec = if yaml_path.is_file() {
        read_yaml_config(&yaml_path)?
    } else {
        let json_path = env_config_path_legacy_json(scope, name);
        if json_path.is_file() {
            read_config(&json_path)?
        } else {
            return Err(ManagerError::ConfigNotFound(yaml_path.display().to_string()));
        }
    };
    migrate_env_config(ec)
}

/// Bring an environment record up to `CURRENT_ENV_SCHEMA`, or reject it loudly.
/// A record from a newer morloc-manager (unknown fields already dropped by
/// serde) is refused rather than half-read; an older record is migrated forward
/// one version at a time.
fn migrate_env_config(ec: EnvironmentConfig) -> Result<EnvironmentConfig> {
    if ec.schema_version > CURRENT_ENV_SCHEMA {
        return Err(ManagerError::EnvError(format!(
            "environment '{}' was created by a newer morloc-manager (env schema v{}, this build \
             understands up to v{}); upgrade morloc-manager to use it",
            ec.name, ec.schema_version, CURRENT_ENV_SCHEMA
        )));
    }
    while ec.schema_version < CURRENT_ENV_SCHEMA {
        match ec.schema_version {
            // Add an arm here per version bump: transform in place, then set
            // `ec.schema_version` to the next version.
            v => {
                return Err(ManagerError::EnvError(format!(
                    "environment '{}' uses env schema v{v} with no migration path to v{}; \
                     recreate it with `morloc-manager new`",
                    ec.name, CURRENT_ENV_SCHEMA
                )));
            }
        }
    }
    Ok(ec)
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
    // Every write produces a current-schema record; stamp it here so no caller
    // can persist a stale `schema_version`.
    let mut ec = ec.clone();
    ec.schema_version = CURRENT_ENV_SCHEMA;
    write_yaml_config(&env_config_path(scope, name), &ec)
}

/// Path to an environment's runtime serve-record (what `start` actually
/// launched). Host-side, beside the config; read by `status`, removed by `stop`.
pub fn env_serve_runtime_path(scope: Scope, name: &str) -> PathBuf {
    env_config_dir(scope, name).join("serve-runtime.json")
}

/// Read the runtime serve-record, or `None` if absent/unreadable.
pub fn read_serve_runtime(scope: Scope, name: &str) -> Option<ServeRuntime> {
    let p = env_serve_runtime_path(scope, name);
    if p.is_file() {
        read_config(&p).ok()
    } else {
        None
    }
}

/// Write the runtime serve-record (JSON).
pub fn write_serve_runtime(scope: Scope, name: &str, r: &ServeRuntime) -> Result<()> {
    write_config(&env_serve_runtime_path(scope, name), r)
}

/// Remove the runtime serve-record (best effort; a missing file is fine).
pub fn remove_serve_runtime(scope: Scope, name: &str) {
    let _ = fs::remove_file(env_serve_runtime_path(scope, name));
}

/// Path to a native environment's materialization record (the captured toolchain
/// activation). Host-side, beside the config; written when the native toolchain
/// is provisioned, read by the native Runner.
pub fn env_native_runtime_path(scope: Scope, name: &str) -> PathBuf {
    env_config_dir(scope, name).join("native-runtime.json")
}

/// Read the native materialization record. An absent record means the native
/// environment has not been materialized yet.
pub fn read_native_runtime(scope: Scope, name: &str) -> Result<NativeRuntime> {
    let p = env_native_runtime_path(scope, name);
    if p.is_file() {
        read_config(&p)
    } else {
        Err(ManagerError::ConfigNotFound(p.display().to_string()))
    }
}

/// Write the native materialization record (JSON).
pub fn write_native_runtime(scope: Scope, name: &str, r: &NativeRuntime) -> Result<()> {
    write_config(&env_native_runtime_path(scope, name), r)
}

/// Path to a native environment's persisted requirement inputs.
pub fn env_inputs_path(scope: Scope, name: &str) -> PathBuf {
    env_config_dir(scope, name).join("env-inputs.json")
}

/// Read the persisted native inputs; an absent file means no `--lang` pins.
pub fn read_env_inputs(scope: Scope, name: &str) -> EnvInputs {
    let p = env_inputs_path(scope, name);
    if p.is_file() {
        read_config(&p).unwrap_or_default()
    } else {
        EnvInputs::default()
    }
}

/// Write the persisted native inputs (JSON).
pub fn write_env_inputs(scope: Scope, name: &str, inputs: &EnvInputs) -> Result<()> {
    write_config(&env_inputs_path(scope, name), inputs)
}

/// Read an environment's exposure config. An absent file means nothing is
/// exposed yet (the default), not an error.
pub fn read_exposure(scope: Scope, name: &str) -> Result<ExposureConfig> {
    let path = env_expose_path(scope, name);
    if path.is_file() {
        read_yaml_config(&path)
    } else {
        Ok(ExposureConfig::default())
    }
}

/// Write an environment's exposure config to expose.yaml (atomic, locked).
pub fn write_exposure(scope: Scope, name: &str, ex: &ExposureConfig) -> Result<()> {
    write_yaml_config(&env_expose_path(scope, name), ex)
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
// Flag configuration (env.flags.yaml)
// ======================================================================

/// Read an env's container flag configuration from `env.flags.yaml`.
/// Strict schema: unknown sections or engine names are parse errors.
/// Absent file -> `FlagConfig::default()` (all empty).
pub fn read_flag_config(scope: Scope, name: &str) -> Result<FlagConfig> {
    let yaml_path = env_flags_yaml_path(scope, name);
    if !yaml_path.is_file() {
        return Ok(FlagConfig::default());
    }
    let mut cfg: FlagConfig = read_yaml_config(&yaml_path)?;
    expand_flag_config(&mut cfg);
    Ok(cfg)
}


fn expand_flag_config(cfg: &mut FlagConfig) {
    expand_engine_flags(&mut cfg.build);
    expand_engine_flags(&mut cfg.run);
    expand_engine_flags(&mut cfg.start);
}

fn expand_engine_flags(ef: &mut EngineFlags) {
    ef.all = expand_list(&ef.all);
    ef.docker = expand_list(&ef.docker);
    ef.podman = expand_list(&ef.podman);
    ef.apptainer = expand_list(&ef.apptainer);
}

fn expand_list(items: &[String]) -> Vec<String> {
    items.iter().flat_map(|s| shell_expand_line(s)).collect()
}

/// Expand a string through the shell, getting glob expansion, environment
/// variable expansion, tilde expansion, and quote handling. Falls back
/// to simple whitespace splitting if the shell invocation fails.
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

#[cfg(test)]
mod tests {
    use super::*;

    // A minimal env.yaml body omitting `schema_version` (a pre-versioning record).
    const V1_YAML: &str = "name: test\nbase_image: \"img:1\"\nengine: podman\n";

    fn parse(yaml: &str) -> EnvironmentConfig {
        serde_yaml::from_str(yaml).expect("valid env config")
    }

    #[test]
    fn missing_schema_version_defaults_to_v1() {
        let ec = parse(V1_YAML);
        assert_eq!(ec.schema_version, 1);
    }

    #[test]
    fn v1_record_migrates_forward() {
        let ec = parse(V1_YAML);
        let migrated = migrate_env_config(ec).expect("v1 migrates");
        assert_eq!(migrated.schema_version, CURRENT_ENV_SCHEMA);
    }

    #[test]
    fn future_schema_is_rejected() {
        let mut ec = parse(V1_YAML);
        ec.schema_version = CURRENT_ENV_SCHEMA + 1;
        let err = migrate_env_config(ec).expect_err("future schema must fail");
        let msg = err.to_string();
        assert!(
            msg.contains("newer morloc-manager"),
            "unexpected error: {msg}"
        );
    }

    #[test]
    fn unknown_older_schema_without_migration_is_rejected() {
        // Schema 0 is below the v1 baseline and has no migration arm.
        let mut ec = parse(V1_YAML);
        ec.schema_version = 0;
        let err = migrate_env_config(ec).expect_err("schema 0 has no migration path");
        assert!(err.to_string().contains("no migration path"));
    }
}
