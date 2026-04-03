use std::fs;
use std::path::Path;
use std::process::{Command, Stdio};

use chrono::Utc;
use crate::config;
use crate::container;
use crate::error::{ManagerError, Result};
use crate::types::*;

pub fn freeze_from_dir(
    scope: Scope,
    ver: Version,
    engine: ContainerEngine,
    v_data_dir: &str,
    output_dir: &str,
) -> Result<()> {
    fs::create_dir_all(output_dir)
        .map_err(|e| ManagerError::FreezeError(format!("Failed to create output dir: {e}")))?;

    if !Path::new(v_data_dir).is_dir() {
        return Err(ManagerError::FreezeError(format!(
            "Data directory does not exist: {v_data_dir}"
        )));
    }

    // Validate programs exist before writing any files
    let modules = scan_modules(&format!("{v_data_dir}/fdb"));
    let programs = scan_programs(&format!("{v_data_dir}/fdb"));
    if programs.is_empty() {
        return Err(ManagerError::FreezeError(
            "No morloc programs are installed. Install programs with 'morloc install' or compile with 'morloc make --install' before freezing.".to_string()
        ));
    }

    eprintln!("Freezing installed state from {v_data_dir}...");
    let tar_path = Path::new(output_dir).join("state.tar.gz");
    let tar_path = tar_path.to_string_lossy();
    let mut tar_dirs: Vec<&str> = Vec::new();
    for dir in &["lib", "fdb", "bin", "exe"] {
        if Path::new(&format!("{v_data_dir}/{dir}")).is_dir() {
            tar_dirs.push(dir);
        }
    }
    let tar_status = Command::new("tar")
        .args(["-czf", &tar_path, "-C", v_data_dir])
        .args(&tar_dirs)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::inherit())
        .status()
        .map_err(|e| ManagerError::FreezeError(format!("tar failed: {e}")))?;

    if !tar_status.success() {
        return Err(ManagerError::FreezeError(
            "tar failed (see error output above)".to_string()
        ));
    }
    eprintln!("Created {tar_path}");
    let now = Utc::now();

    // Get base image from the active environment config
    let m_cfg = config::read_active_config();
    let active_env_name = m_cfg.as_ref().and_then(|c| c.active_env.as_deref());

    let (base_img, env_layer) = if let Some(env_name) = active_env_name {
        match config::read_env_config(scope, env_name) {
            Ok(ec) => {
                let base = ec.base_image.clone();
                // Capture env layer info if there's a Dockerfile
                let layer = if ec.dockerfile.is_some() {
                    let df_path = config::env_dockerfile_path(scope, env_name);
                    if df_path.exists() {
                        let df_contents = fs::read_to_string(&df_path).unwrap_or_default();
                        let content_hash = ec.content_hash.unwrap_or_default();
                        // Query image digest
                        let env_image_tag = ec.built_image.as_deref()
                            .unwrap_or(&ec.base_image);
                        let exe = container::engine_executable(engine);
                        let digest_output = Command::new(exe)
                            .args([
                                "inspect", "--format",
                                "{{index .RepoDigests 0}}",
                                env_image_tag,
                            ])
                            .output();
                        let image_digest = match digest_output {
                            Ok(o) if o.status.success() => {
                                let s = String::from_utf8_lossy(&o.stdout).trim().to_string();
                                if s.is_empty() { None } else { Some(s) }
                            }
                            _ => None,
                        };
                        Some(FrozenEnvLayer {
                            name: env_name.to_string(),
                            dockerfile: df_contents,
                            content_hash,
                            image_digest,
                        })
                    } else {
                        None
                    }
                } else {
                    None
                };
                (base, layer)
            }
            Err(_) => ("unknown".to_string(), None),
        }
    } else {
        ("unknown".to_string(), None)
    };

    let manifest = FreezeManifest {
        morloc_version: ver,
        frozen_at: now,
        modules,
        programs,
        base_image: base_img,
        env_layer,
    };
    let manifest_path = Path::new(output_dir).join("freeze-manifest.json");
    let manifest_path = manifest_path.to_string_lossy();
    write_freeze_manifest(&manifest_path, &manifest)?;
    eprintln!("Wrote {manifest_path}");
    eprintln!("Frozen state written to {output_dir}");
    Ok(())
}

pub fn write_freeze_manifest(path: &str, manifest: &FreezeManifest) -> Result<()> {
    let json = serde_json::to_vec(manifest)
        .map_err(|e| ManagerError::FreezeError(format!("JSON encode failed: {e}")))?;
    fs::write(path, json)
        .map_err(|e| ManagerError::FreezeError(format!("Write failed: {e}")))?;
    Ok(())
}

pub fn read_freeze_manifest(path: &str) -> Result<FreezeManifest> {
    let bytes =
        fs::read(path).map_err(|e| ManagerError::FreezeError(format!("Read failed: {e}")))?;
    serde_json::from_slice(&bytes)
        .map_err(|e| ManagerError::FreezeError(format!("Invalid manifest: {e}")))
}

// ======================================================================
// Internal: scanning installed state
// ======================================================================

fn scan_modules(fdb_dir: &str) -> Vec<ModuleEntry> {
    let fdb_path = Path::new(fdb_dir);
    if !fdb_path.is_dir() {
        return Vec::new();
    }
    let Ok(entries) = fs::read_dir(fdb_path) else {
        return Vec::new();
    };

    #[derive(serde::Deserialize)]
    struct ModuleStub {
        name: String,
        #[serde(default)]
        version: Option<String>,
    }

    entries
        .flatten()
        .filter(|e| {
            e.file_name()
                .to_string_lossy()
                .ends_with(".module")
        })
        .filter_map(|e| {
            let bytes = fs::read(e.path()).ok()?;
            let stub: ModuleStub = serde_json::from_slice(&bytes).ok()?;
            Some(ModuleEntry {
                name: stub.name,
                version: stub.version,
                sha256: String::new(),
            })
        })
        .collect()
}

fn scan_programs(fdb_dir: &str) -> Vec<ProgramEntry> {
    let fdb_path = Path::new(fdb_dir);
    if !fdb_path.is_dir() {
        return Vec::new();
    }
    let Ok(entries) = fs::read_dir(fdb_path) else {
        return Vec::new();
    };
    entries
        .flatten()
        .filter(|e| {
            e.file_name()
                .to_string_lossy()
                .ends_with(".manifest")
        })
        .map(|e| {
            let filename = e.file_name().to_string_lossy().to_string();
            let prog_name = filename.strip_suffix(".manifest").unwrap_or(&filename);
            let commands = parse_manifest_commands(&e.path());
            ProgramEntry {
                name: prog_name.to_string(),
                commands,
            }
        })
        .collect()
}

fn parse_manifest_commands(path: &Path) -> Vec<String> {
    let Ok(bytes) = fs::read(path) else {
        return Vec::new();
    };
    #[derive(serde::Deserialize)]
    struct ManifestStub {
        #[serde(default)]
        commands: Vec<ManifestStubCmd>,
    }
    #[derive(serde::Deserialize)]
    struct ManifestStubCmd {
        name: String,
    }
    match serde_json::from_slice::<ManifestStub>(&bytes) {
        Ok(stub) => stub.commands.into_iter().map(|c| c.name).collect(),
        Err(_) => Vec::new(),
    }
}
