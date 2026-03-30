use std::fs;
use std::process::Command;

use crate::config;
use crate::container::{container_pull, engine_executable, exit_code_to_int, image_exists_locally, remote_image_exists};
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
        println!("Using local copy");
        return Ok(false);
    }

    // Check if the image exists locally in the container engine (e.g. pulled
    // but config was removed, or force-reinstall scenario)
    let have_local_image = image_exists_locally(engine, &image_ref);

    if !have_local_image {
        // Verify the remote image exists before attempting to pull
        if !remote_image_exists(engine, &image_ref) {
            return Err(ManagerError::InstallError(format!(
                "No container for morloc v{} exists",
                ver.show()
            )));
        }

        eprintln!("Pulling {}...", image_ref);
        let (status, _stdout, stderr) = container_pull(engine, &image_ref);
        if !status.success() {
            return Err(ManagerError::EngineError {
                engine,
                code: exit_code_to_int(status),
                stderr,
            });
        }
    } else {
        println!("Using local copy");
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
        original_image: None,
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

    if !remote_image_exists(engine, edge_image) {
        return Err(ManagerError::InstallError(
            "No container for latest morloc version exists".to_string(),
        ));
    }

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

    // Record the original pullable image reference (edge) so freeze/unfreeze
    // can use a tag that actually exists on the registry
    if let Ok(mut vc) = config::read_version_config(scope, ver) {
        vc.original_image = Some(edge_image.to_string());
        let _ = config::write_version_config(scope, ver, &vc);
    }

    Ok((ver, was_fresh))
}

pub fn select_version(ver: Version) -> Result<()> {
    // Verify version exists in some scope
    config::find_installed_scope(ver)?;
    // Always write to local config
    let cfg_path = config::config_path(Scope::Local);
    // Inherit engine from system config if no local config exists, to avoid
    // overriding the system engine with the hardcoded default (podman)
    let base_cfg = config::read_config::<Config>(&cfg_path)
        .or_else(|_| config::read_config::<Config>(&config::config_path(Scope::System)))
        .unwrap_or_default();
    let new_cfg = Config {
        active_target: Some(ActiveTarget::Version(ver)),
        ..base_cfg
    };
    config::write_config(&cfg_path, &new_cfg)
}

pub fn select_version_system(ver: Version) -> Result<()> {
    // Verify version exists in some scope
    config::find_installed_scope(ver)?;
    // Write to system config
    let cfg_path = config::config_path(Scope::System);
    let base_cfg = config::read_config::<Config>(&cfg_path).unwrap_or_default();
    let new_cfg = Config {
        active_target: Some(ActiveTarget::Version(ver)),
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
    let target = resolve_active_target()?;
    match target {
        ActiveTarget::Version(ver) => {
            let scope = config::find_installed_scope(ver)?;
            Ok((ver, scope))
        }
        _ => Err(ManagerError::NoActiveVersion),
    }
}

/// Resolve the active target by checking local config first, then system.
pub fn resolve_active_target() -> Result<ActiveTarget> {
    // Try local config first
    if let Ok(cfg) = config::read_config::<Config>(&config::config_path(Scope::Local)) {
        if let Some(target) = cfg.active_target {
            return Ok(target);
        }
    }
    // Fall back to system config
    if let Ok(cfg) = config::read_config::<Config>(&config::config_path(Scope::System)) {
        if let Some(target) = cfg.active_target {
            return Ok(target);
        }
    }
    Err(ManagerError::NoActiveVersion)
}

pub fn uninstall_version(scope: Scope, ver: Version) -> Result<()> {
    // Pre-check: system-scope uninstall requires root
    if scope == Scope::System && !nix::unistd::getuid().is_root() {
        return Err(ManagerError::UninstallError(
            "Permission denied: system-scope uninstall requires root. Use: sudo morloc-manager uninstall --scope system".to_string(),
        ));
    }

    let vc_path = config::version_config_path(scope, ver);
    let vc: VersionConfig = config::read_config(&vc_path)
        .map_err(|_| ManagerError::VersionNotInstalled(ver))?;

    // Only remove the container image if the other scope doesn't also have
    // this version installed (prevents cross-scope breakage on shared Docker daemon)
    let other_scope = match scope {
        Scope::Local => Scope::System,
        Scope::System => Scope::Local,
    };
    let other_has_version = config::version_config_path(other_scope, ver).is_file();
    if other_has_version {
        eprintln!("  Skipping image removal (also installed in {} scope)", match other_scope {
            Scope::Local => "local",
            Scope::System => "system",
        });
    } else {
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
    }

    // Clear local active version if it pointed to this version
    {
        let cfg_path = config::config_path(Scope::Local);
        if let Ok(cfg) = config::read_config::<Config>(&cfg_path) {
            if cfg.active_target == Some(ActiveTarget::Version(ver)) {
                let new_cfg = Config {
                    active_target: None,
                    ..cfg
                };
                let _ = config::write_config(&cfg_path, &new_cfg);
                eprintln!("  Cleared local active version");
            }
        }
    }
    // Warn if system default still references the uninstalled version
    {
        let cfg_path = config::config_path(Scope::System);
        if let Ok(cfg) = config::read_config::<Config>(&cfg_path) {
            if cfg.active_target == Some(ActiveTarget::Version(ver)) {
                eprintln!(
                    "  Warning: system default still references {}",
                    ver.show()
                );
            }
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
