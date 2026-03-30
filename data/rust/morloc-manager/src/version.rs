use std::fs;
use std::process::Command;

use crate::config;
use crate::container::{container_pull, engine_executable, exit_code_to_int};
use crate::error::{ManagerError, Result};
use crate::types::*;

pub fn install_version(
    engine: ContainerEngine,
    scope: Scope,
    ver: Version,
) -> Result<bool> {
    install_version_inner(false, engine, scope, ver)
}

pub fn install_version_force(
    force: bool,
    engine: ContainerEngine,
    scope: Scope,
    ver: Version,
) -> Result<bool> {
    install_version_inner(force, engine, scope, ver)
}

fn install_version_inner(
    force: bool,
    engine: ContainerEngine,
    scope: Scope,
    ver: Version,
) -> Result<bool> {
    let image_ref = format!(
        "ghcr.io/morloc-project/morloc/morloc-full:{}",
        ver.show()
    );
    let already = if force {
        false
    } else {
        is_version_installed(engine, scope, ver, &image_ref)
    };
    if already {
        eprintln!("Version {} is already installed.", ver.show());
        return Ok(false);
    }

    let (status, _stdout, stderr) = container_pull(engine, &image_ref);
    if !status.success() {
        return Err(ManagerError::EngineError {
            engine,
            code: exit_code_to_int(status),
            stderr,
        });
    }

    let v_data_dir = config::version_data_dir(scope, ver);
    for sub in &["bin", "lib", "fdb", "include", "opt", "tmp"] {
        fs::create_dir_all(v_data_dir.join(sub)).map_err(|e| {
            ManagerError::InstallError(format!("Failed to create directory: {e}"))
        })?;
    }

    if scope == Scope::System {
        use std::os::unix::fs::PermissionsExt;
        let dirs: Vec<_> = std::iter::once(v_data_dir.clone())
            .chain(["bin", "lib", "fdb", "include", "opt", "tmp"].iter().map(|d| v_data_dir.join(d)))
            .collect();
        for d in dirs {
            let _ = fs::set_permissions(&d, fs::Permissions::from_mode(0o2775));
        }
    }

    let vc = VersionConfig {
        image: image_ref,
        host_dir: v_data_dir.to_string_lossy().to_string(),
        shm_size: "512m".to_string(),
        engine,
    };
    config::write_version_config(scope, ver, &vc)?;
    Ok(true)
}

pub fn install_latest(
    engine: ContainerEngine,
    scope: Scope,
) -> Result<(Version, bool)> {
    let edge_image = "ghcr.io/morloc-project/morloc/morloc-full:edge";
    let (pull_status, _, pull_err) = container_pull(engine, edge_image);
    if !pull_status.success() {
        return Err(ManagerError::EngineError {
            engine,
            code: exit_code_to_int(pull_status),
            stderr: pull_err,
        });
    }

    let exe = engine_executable(engine);
    let output = Command::new(exe)
        .args(["run", "--rm", edge_image, "morloc", "--version"])
        .output()
        .map_err(|e| ManagerError::InstallError(format!("Failed to run edge container: {e}")))?;

    if !output.status.success() {
        return Err(ManagerError::InstallError(format!(
            "Failed to detect morloc version from edge image: {}",
            String::from_utf8_lossy(&output.stderr)
        )));
    }

    let ver_out = String::from_utf8_lossy(&output.stdout).trim().to_string();
    let ver_str = ver_out.split_whitespace().last().unwrap_or(&ver_out);
    let ver: Version = ver_str
        .parse()
        .map_err(|_| ManagerError::InstallError(format!(
            "Could not parse version from edge image output: {ver_out}"
        )))?;

    // Tag edge image with actual version
    let versioned_image = format!(
        "ghcr.io/morloc-project/morloc/morloc-full:{}",
        ver.show()
    );
    let _ = Command::new(exe)
        .args(["tag", edge_image, &versioned_image])
        .output();

    let was_fresh = install_version(engine, scope, ver)?;
    Ok((ver, was_fresh))
}

pub fn select_version(scope: Scope, ver: Version) -> Result<()> {
    let vc_path = config::version_config_path(scope, ver);
    config::read_config::<VersionConfig>(&vc_path)
        .map_err(|_| ManagerError::VersionNotInstalled(ver))?;

    let cfg_path = config::config_path(scope);
    let base_cfg = config::read_config::<Config>(&cfg_path).unwrap_or_default();
    let new_cfg = Config {
        active_target: Some(ActiveTarget::Version(ver)),
        active_scope: scope,
        ..base_cfg
    };
    config::write_config(&cfg_path, &new_cfg)
}

pub fn list_versions(scope: Scope) -> Vec<Version> {
    let versions_dir = config::config_dir(scope).join("versions");
    if !versions_dir.is_dir() {
        return Vec::new();
    }
    let Ok(entries) = fs::read_dir(&versions_dir) else {
        return Vec::new();
    };
    let mut versions: Vec<Version> = entries
        .filter_map(|e| e.ok())
        .filter_map(|e| {
            let name = e.file_name().into_string().ok()?;
            let ver: Version = name.parse().ok()?;
            if e.path().join("config.json").is_file() {
                Some(ver)
            } else {
                None
            }
        })
        .collect();
    versions.sort();
    versions
}

pub fn resolve_active_version() -> Result<(Version, Scope)> {
    let cfg = config::read_active_config().ok_or(ManagerError::NoActiveVersion)?;
    let ver = cfg.active_version().ok_or(ManagerError::NoActiveVersion)?;
    Ok((ver, cfg.active_scope))
}

pub fn resolve_active_target() -> Result<(ActiveTarget, Scope)> {
    let cfg = config::read_active_config().ok_or(ManagerError::NoActiveVersion)?;
    let target = cfg.active_target.ok_or(ManagerError::NoActiveVersion)?;
    Ok((target, cfg.active_scope))
}

pub fn uninstall_version(scope: Scope, ver: Version) -> Result<()> {
    let vc_path = config::version_config_path(scope, ver);
    let vc: VersionConfig = config::read_config(&vc_path)
        .map_err(|_| ManagerError::VersionNotInstalled(ver))?;

    let exe = engine_executable(vc.engine);
    let rmi_output = Command::new(exe).args(["rmi", &vc.image]).output();
    match rmi_output {
        Ok(o) if o.status.success() => {
            eprintln!("  Removed image: {}", vc.image);
        }
        _ => {
            eprintln!(
                "  Warning: could not remove image {} (may have been removed manually)",
                vc.image
            );
        }
    }

    // Clear active version if this was selected
    if let Some(cfg) = config::read_active_config() {
        if cfg.active_target == Some(ActiveTarget::Version(ver)) {
            let cfg_path = config::config_path(scope);
            let new_cfg = Config {
                active_target: None,
                ..cfg
            };
            let _ = config::write_config(&cfg_path, &new_cfg);
            eprintln!("  Cleared active version");
        }
    }

    // Remove config directory
    let vc_dir = config::version_config_dir(scope, ver);
    if vc_dir.is_dir() {
        let _ = fs::remove_dir_all(&vc_dir);
        eprintln!("  Removed config: {}", vc_dir.display());
    }

    // Remove data directory
    let vd_dir = config::version_data_dir(scope, ver);
    if vd_dir.is_dir() {
        let _ = fs::remove_dir_all(&vd_dir);
        eprintln!("  Removed data: {}", vd_dir.display());
    }

    Ok(())
}

pub fn is_version_installed(
    engine: ContainerEngine,
    scope: Scope,
    ver: Version,
    image_ref: &str,
) -> bool {
    let path = config::version_config_path(scope, ver);
    if !path.is_file() {
        return false;
    }
    let exe = engine_executable(engine);
    Command::new(exe)
        .args(["image", "inspect", image_ref])
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .map(|s| s.success())
        .unwrap_or(false)
}
