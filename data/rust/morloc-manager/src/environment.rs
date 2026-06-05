use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use serde::Serialize;
use sha2::{Digest, Sha256};

use crate::config;
use crate::container::{
    self, apptainer_build_from_oci_daemon, apptainer_build_native,
    check_remote_image, container_build_visible, container_pull_to_path,
    container_pull_visible, detect_oci_builder, engine_executable,
    exit_code_to_int, image_exists_locally, ApptainerNativeBuildConfig,
    ApptainerOciConvertConfig, BuildConfig, RemoteImageStatus,
};
use crate::error::{ManagerError, Result};
use crate::serve;
use crate::types::*;

// ======================================================================
// Public types
// ======================================================================

/// Options for creating or updating an environment.
/// For `new` (is_new=true): all Option fields that are None use defaults.
/// For `update` (is_new=false): None means keep the existing value.
pub struct ApplyOptions {
    pub name: String,
    pub scope: Scope,
    pub is_new: bool,
    pub base_image: Option<String>,
    pub original_image: Option<String>,
    pub morloc_version: Option<Version>,
    pub dockerfile: Option<String>,
    /// Path to a Singularity .def file. Mutually exclusive with a Dockerfile
    /// stub only for stub generation (which lives in main.rs); the env config
    /// itself may hold both a Dockerfile and a .def. On Apptainer the .def
    /// takes precedence; on docker/podman the Dockerfile takes precedence.
    pub deffile: Option<String>,
    pub includes: Vec<String>,
    /// Path to a user-supplied YAML flag file. When set, its contents replace
    /// env.flags.yaml in the env config dir (declarative bulk write).
    pub flagfile: Option<String>,
    /// `-x/--engine-arg` flags on `new`. Written to `run.<engine>` of the
    /// freshly created env.flags.yaml as the user's initial configuration.
    /// Unused on `update` (where `-x` has been removed).
    pub engine_args: Vec<String>,
    /// `--reinit-arg` flags on `update`. One-shot build-engine flags
    /// appended to the materialized build flag list for this invocation.
    /// Never persisted. Requires `--reinit`; CLI layer enforces that.
    pub reinit_args: Vec<String>,
    pub engine: Option<ContainerEngine>,
    pub shm_size: Option<String>,
    /// Skip the recipe-build step (Dockerfile under docker/podman, .def under
    /// apptainer, or the OCI fallback). Retains the legacy name despite now
    /// covering both recipe kinds because of how callers reference it.
    pub skip_dockerfile_build: bool,
    pub verbose: bool,
}

/// Info returned by list_environments.
#[derive(Serialize)]
pub struct EnvInfo {
    pub name: String,
    pub morloc_version: Option<Version>,
    pub active: bool,
}

// ======================================================================
// Image resolution
// ======================================================================

const MORLOC_IMAGE_PREFIX: &str = "ghcr.io/morloc-project/morloc/morloc-full";

/// Recognize engine errors that mean "cannot chdir into the current working
/// directory" and rewrite them into a clearer message. This commonly happens
/// when running `sudo -u <other-user> morloc-manager ...` from a directory
/// that <other-user> cannot access (e.g., /root or another user's $HOME).
/// Without this hint, the error bubbles up as "Failed to check registry..."
/// which misleads users toward debugging network/auth problems.
fn cwd_access_hint(stderr: &str) -> Option<String> {
    let lower = stderr.to_lowercase();
    let looks_like_cwd_denied = (lower.contains("chdir") || lower.contains("getwd")
        || lower.contains("current working directory"))
        && (lower.contains("permission denied") || lower.contains("no such file"));
    if looks_like_cwd_denied {
        Some(format!(
            "Cannot change into the current working directory as the target user. \
             Run morloc-manager from a directory the target user can access \
             (for example /tmp or the user's home directory).\nOriginal error: {}",
            stderr.trim()
        ))
    } else {
        None
    }
}

/// Resolve a morloc version string to a registry image reference.
pub fn version_to_image(ver: &Version) -> String {
    format!("{MORLOC_IMAGE_PREFIX}:{}", ver.show())
}

/// Pull an image by tag from the morloc registry, detect its version, and
/// return (image_ref, version). The tag can be a semver string ("0.77.0"),
/// a named tag ("edge", "nightly"), or any other valid container tag.
///
/// For docker/podman this writes to the engine's image store and returns the
/// versioned OCI ref. For apptainer the .sif is written to a per-tag cache
/// path (`apptainer_pull_cache_path`); the returned string is the OCI ref
/// (the .sif path is recoverable from the same helper) and the cached .sif
/// is later moved into the env's data dir by apply_environment.
pub fn pull_tagged_image(engine: ContainerEngine, tag: &str) -> Result<(String, Version)> {
    let image_ref = format!("{MORLOC_IMAGE_PREFIX}:{tag}");

    if matches!(engine, ContainerEngine::Apptainer) {
        return pull_tagged_image_apptainer(&image_ref);
    }

    if !image_exists_locally(engine, &image_ref) {
        match check_remote_image(engine, &image_ref) {
            RemoteImageStatus::Exists => {}
            RemoteImageStatus::NotFound => {
                return Err(ManagerError::EnvError(format!(
                    "No container image found for tag '{tag}'"
                )));
            }
            RemoteImageStatus::Unknown(stderr) => {
                if let Some(hint) = cwd_access_hint(&stderr) {
                    return Err(ManagerError::EnvError(hint));
                }
                return Err(ManagerError::EnvError(format!(
                    "Failed to check registry for tag '{tag}': {}",
                    stderr.trim()
                )));
            }
        }

        eprintln!("Pulling {image_ref}...");
        let status = container_pull_visible(engine, &image_ref);
        if !status.success() {
            return Err(ManagerError::EngineError {
                engine,
                code: exit_code_to_int(status),
                stderr: "Pull failed (see output above)".to_string(),
            });
        }
    } else {
        eprintln!("Using local copy of {image_ref}");
    }

    let ver = detect_morloc_version(engine, &image_ref)?;

    // Also tag with the detected version so future --version lookups find it
    let versioned_image = version_to_image(&ver);
    if versioned_image != image_ref {
        let exe = engine_executable(engine);
        let _ = Command::new(exe)
            .args(["tag", &image_ref, &versioned_image])
            .output();
    }

    Ok((versioned_image, ver))
}

/// Apptainer-specific pull. Writes the .sif to a per-OCI-ref cache path so
/// that the env-name resolution that happens after the pull (driven by
/// `detect_morloc_version`) does not need to know the env name yet. The
/// caller later calls `claim_apptainer_pull_into_env` to move the cached
/// .sif into the env's data dir.
fn pull_tagged_image_apptainer(image_ref: &str) -> Result<(String, Version)> {
    let cache_path = apptainer_pull_cache_path(image_ref);

    if cache_path.is_file() {
        eprintln!("Using local copy of {image_ref} ({})", cache_path.display());
    } else {
        if let Some(parent) = cache_path.parent() {
            fs::create_dir_all(parent).map_err(|e| {
                ManagerError::EnvError(format!(
                    "Failed to create apptainer pull cache dir {}: {e}",
                    parent.display()
                ))
            })?;
        }
        eprintln!("Pulling {image_ref} -> {}...", cache_path.display());
        let status = container_pull_to_path(
            ContainerEngine::Apptainer,
            image_ref,
            &cache_path.to_string_lossy(),
        );
        if !status.success() {
            return Err(ManagerError::EngineError {
                engine: ContainerEngine::Apptainer,
                code: exit_code_to_int(status),
                stderr: "Apptainer pull failed (see output above)".to_string(),
            });
        }
    }

    let ver = detect_morloc_version(
        ContainerEngine::Apptainer,
        &cache_path.to_string_lossy(),
    )?;

    // Return the OCI ref (NOT the cache path). Storing the OCI URI in
    // ec.base_image keeps it human-readable and re-pullable; the .sif path
    // lives in ec.base_sif and is filled in by apply_environment after the
    // move.
    Ok((image_ref.to_string(), ver))
}

/// Cache path for an Apptainer pull, keyed by the OCI image reference. The
/// .sif lives here until claimed into an env's data dir.
pub fn apptainer_pull_cache_path(oci_ref: &str) -> PathBuf {
    let mut hasher = Sha256::new();
    hasher.update(oci_ref.as_bytes());
    let h = hex_encode(&hasher.finalize());
    let base = dirs::cache_dir()
        .unwrap_or_else(|| PathBuf::from("/tmp"))
        .join("morloc")
        .join("apptainer-pull");
    base.join(format!("{}.sif", &h[..16]))
}

/// Move a cached .sif (produced by `pull_tagged_image_apptainer` or
/// `pull_custom_image`) into the env's data dir at the canonical
/// `<env-data-dir>/sif/base.sif` location. Returns the final path. If the
/// cache file is missing (e.g. caller did not pull through this function),
/// returns Err.
pub fn claim_apptainer_pull_into_env(
    scope: Scope,
    name: &str,
    oci_ref: &str,
) -> Result<PathBuf> {
    let cache = apptainer_pull_cache_path(oci_ref);
    if !cache.is_file() {
        return Err(ManagerError::EnvError(format!(
            "Apptainer pull cache missing for {oci_ref} (expected at {}). Re-run with --image or --tag to repull.",
            cache.display()
        )));
    }
    let dst = config::env_base_sif_path(scope, name);
    if let Some(parent) = dst.parent() {
        fs::create_dir_all(parent).map_err(|e| {
            ManagerError::EnvError(format!(
                "Failed to create sif dir {}: {e}",
                parent.display()
            ))
        })?;
    }
    // rename within the same filesystem is atomic; if cache and dst are on
    // different filesystems, fall back to copy+remove.
    if fs::rename(&cache, &dst).is_err() {
        fs::copy(&cache, &dst).map_err(|e| {
            ManagerError::EnvError(format!(
                "Failed to move pulled .sif from {} to {}: {e}",
                cache.display(),
                dst.display()
            ))
        })?;
        let _ = fs::remove_file(&cache);
    }
    Ok(dst)
}

/// Pull the :edge image. Convenience wrapper around pull_tagged_image.
pub fn resolve_latest(engine: ContainerEngine) -> Result<(String, Version)> {
    pull_tagged_image(engine, "edge")
}

/// Pull a specific version image from the morloc registry.
pub fn pull_version_image(engine: ContainerEngine, ver: &Version) -> Result<String> {
    let (img, _) = pull_tagged_image(engine, &ver.show())?;
    Ok(img)
}

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

/// Pull a custom image (not from morloc registry). For Apptainer this writes
/// to the per-OCI-ref cache path (see `apptainer_pull_cache_path`); the
/// caller is expected to claim the .sif into an env via
/// `claim_apptainer_pull_into_env` after the env name is known.
pub fn pull_custom_image(engine: ContainerEngine, image: &str) -> Result<()> {
    if matches!(engine, ContainerEngine::Apptainer) {
        let cache_path = apptainer_pull_cache_path(image);
        if cache_path.is_file() {
            eprintln!("Using local copy of {image} ({})", cache_path.display());
            return Ok(());
        }
        if let Some(parent) = cache_path.parent() {
            fs::create_dir_all(parent).map_err(|e| {
                ManagerError::EnvError(format!(
                    "Failed to create apptainer pull cache dir {}: {e}",
                    parent.display()
                ))
            })?;
        }
        eprintln!("Pulling {image} -> {}...", cache_path.display());
        let status = container_pull_to_path(engine, image, &cache_path.to_string_lossy());
        if !status.success() {
            return Err(ManagerError::EngineError {
                engine,
                code: exit_code_to_int(status),
                stderr: "Apptainer pull failed (see output above)".to_string(),
            });
        }
        return Ok(());
    }

    if image_exists_locally(engine, image) {
        eprintln!("Using local copy of {image}");
        return Ok(());
    }

    eprintln!("Pulling {image}...");
    let status = container_pull_visible(engine, image);
    if !status.success() {
        return Err(ManagerError::EngineError {
            engine,
            code: exit_code_to_int(status),
            stderr: "Pull failed (see output above)".to_string(),
        });
    }
    Ok(())
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

/// Parse an include spec into (resolved_source, destination).
///
/// Supports two forms:
/// - `path`        — copies to cfg_dir/basename(path)
/// - `src:dest`    — copies src to cfg_dir/dest
///
/// Rules for dest:
/// - Must be relative (no leading `/`)
/// - Cannot contain `..`
/// - If dest ends with `/`, src's basename is appended
///
/// Source symlinks are resolved via canonicalize().
fn parse_include_spec(spec: &str, cfg_dir: &Path) -> Result<(PathBuf, PathBuf)> {
    let (src_str, dest_rel) = if let Some(idx) = spec.find(':') {
        let s = &spec[..idx];
        let d = &spec[idx + 1..];
        if s.is_empty() || d.is_empty() {
            return Err(ManagerError::EnvError(format!(
                "Invalid include spec: '{spec}'"
            )));
        }
        (s, d.to_string())
    } else {
        let src_path = Path::new(spec);
        let fname = src_path.file_name().ok_or_else(|| {
            ManagerError::EnvError(format!("Invalid include path: {spec}"))
        })?;
        (spec.as_ref(), fname.to_string_lossy().to_string())
    };

    // Validate dest constraints
    if dest_rel.starts_with('/') {
        return Err(ManagerError::EnvError(format!(
            "Include destination must be relative, not absolute: '{dest_rel}'"
        )));
    }
    if dest_rel.contains("..") {
        return Err(ManagerError::EnvError(format!(
            "Include destination cannot contain '..': '{dest_rel}'"
        )));
    }

    // Resolve src (canonicalize follows symlinks, errors if path doesn't exist)
    let real_src = Path::new(src_str).canonicalize().map_err(|e| {
        ManagerError::EnvError(format!("Cannot resolve include path '{src_str}': {e}"))
    })?;

    // Compute final dest
    let dest = cfg_dir.join(&dest_rel);
    let final_dest = if dest_rel.ends_with('/') {
        dest.join(real_src.file_name().unwrap_or_default())
    } else {
        dest
    };

    Ok((real_src, final_dest))
}

/// When `is_new` is false: loads existing config, applies overrides.
pub fn apply_environment(opts: &ApplyOptions) -> Result<()> {
    let scope = opts.scope;
    let name = &opts.name;

    validate_env_name(name)?;

    // Load existing config or start fresh
    let mut ec = if opts.is_new {
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
        // Start with required fields from opts; the rest will be applied below
        EnvironmentConfig {
            name: name.clone(),
            base_image: opts.base_image.clone().unwrap_or_default(),
            original_image: None,
            dockerfile: None,
            content_hash: None,
            built_image: None,
            singularity_def: None,
            def_content_hash: None,
            base_sif: None,
            layered_sif: None,
            engine: opts.engine.unwrap_or(ContainerEngine::Podman),
            shm_size: "512m".to_string(),
            morloc_version: None,
        }
    } else {
        config::read_env_config(scope, name)
            .map_err(|_| ManagerError::EnvironmentNotFound(name.to_string()))?
    };

    // Apply overrides
    if let Some(ref img) = opts.base_image {
        ec.base_image = img.clone();
    }
    if let Some(ref img) = opts.original_image {
        ec.original_image = Some(img.clone());
    }
    if let Some(ref ver) = opts.morloc_version {
        ec.morloc_version = Some(ver.clone());
    }
    if let Some(engine) = opts.engine {
        ec.engine = engine;
    }
    if let Some(ref shm) = opts.shm_size {
        if !is_valid_shm_size(shm) {
            return Err(ManagerError::EnvError(format!(
                "Invalid --shm-size '{shm}'. Use format like: 512m, 1g, 2048k"
            )));
        }
        ec.shm_size = shm.clone();
    }

    // Copy Dockerfile if a new one was provided
    let dockerfile_changed = if let Some(ref src) = opts.dockerfile {
        let dest = config::env_dockerfile_path(scope, name);
        let dest_dir = dest.parent().unwrap();
        fs::create_dir_all(dest_dir).map_err(|e| {
            ManagerError::EnvError(format!("Failed to create config dir: {e}"))
        })?;
        fs::copy(src, &dest).map_err(|e| {
            ManagerError::EnvError(format!("Failed to copy Dockerfile '{}': {e}", src))
        })?;
        ec.dockerfile = Some("Dockerfile".to_string());
        true
    } else {
        false
    };

    // Copy Singularity .def if a new one was provided
    let deffile_changed = if let Some(ref src) = opts.deffile {
        let dest = config::env_deffile_path(scope, name);
        let dest_dir = dest.parent().unwrap();
        fs::create_dir_all(dest_dir).map_err(|e| {
            ManagerError::EnvError(format!("Failed to create config dir: {e}"))
        })?;
        fs::copy(src, &dest).map_err(|e| {
            ManagerError::EnvError(format!("Failed to copy .def file '{}': {e}", src))
        })?;
        ec.singularity_def = Some("recipe.def".to_string());
        true
    } else {
        false
    };

    // For Apptainer: claim the cached pull .sif (from pull_tagged_image /
    // pull_custom_image) into the env's data dir. This runs once at `new`
    // and again whenever the base image changes via `update --image`.
    let needs_base_sif_claim = matches!(ec.engine, ContainerEngine::Apptainer)
        && (opts.is_new || opts.base_image.is_some())
        && !ec.base_image.is_empty();
    if needs_base_sif_claim {
        let dst = claim_apptainer_pull_into_env(scope, name, &ec.base_image)?;
        ec.base_sif = Some(dst.to_string_lossy().to_string());
    }

    // Copy included files/directories into build context.
    // Supports src:dest syntax (like Docker volume mounts) for explicit placement.
    let cfg_dir = config::env_config_dir(scope, name);
    fs::create_dir_all(&cfg_dir).map_err(|e| {
        ManagerError::EnvError(format!("Failed to create config dir: {e}"))
    })?;
    for spec in &opts.includes {
        let (real_src, final_dest) = parse_include_spec(spec, &cfg_dir)?;
        if let Some(parent) = final_dest.parent() {
            fs::create_dir_all(parent).map_err(|e| {
                ManagerError::EnvError(format!("Failed to create directory: {e}"))
            })?;
        }
        if real_src.is_dir() {
            let status = Command::new("cp")
                .args(["-a", &real_src.to_string_lossy(), &final_dest.to_string_lossy()])
                .stdin(std::process::Stdio::null())
                .stdout(std::process::Stdio::null())
                .stderr(std::process::Stdio::inherit())
                .status()
                .map_err(|e| ManagerError::EnvError(format!("Failed to copy '{spec}': {e}")))?;
            if !status.success() {
                return Err(ManagerError::EnvError(format!(
                    "Failed to copy directory '{spec}'"
                )));
            }
        } else {
            fs::copy(&real_src, &final_dest).map_err(|e| {
                ManagerError::EnvError(format!("Failed to copy '{spec}': {e}"))
            })?;
        }
    }

    // Flag-config write policy:
    //   * --flagfile: parse the user file as a FlagConfig (strict schema),
    //     replace env.flags.yaml. Works on both `new` and `update`.
    //   * is_new + engine_args: seed env.flags.yaml with run.<engine> set
    //     to engine_args. This is the only path where `-x` writes to the
    //     file; on `update` `-x` no longer exists.
    //   * Otherwise: do not touch env.flags.yaml. Users edit it themselves.
    if let Some(ref src) = opts.flagfile {
        let bytes = fs::read(src).map_err(|e| {
            ManagerError::EnvError(format!("Failed to read flagfile '{}': {e}", src))
        })?;
        let parsed: FlagConfig = serde_yaml::from_slice(&bytes).map_err(|e| {
            ManagerError::EnvError(format!("Invalid flagfile '{}': {e}", src))
        })?;
        config::write_flag_config(scope, name, &parsed)?;
    } else if opts.is_new && !opts.engine_args.is_empty() {
        let mut fc = FlagConfig::default();
        let target = match ec.engine {
            ContainerEngine::Docker => &mut fc.run.docker,
            ContainerEngine::Podman => &mut fc.run.podman,
            ContainerEngine::Apptainer => &mut fc.run.apptainer,
        };
        target.extend(opts.engine_args.iter().cloned());
        config::write_flag_config(scope, name, &fc)?;
    }

    // Build the recipe layer if present and not skipped. The dispatch matrix
    // covers all engine x recipe combinations:
    //
    //   docker/podman + Dockerfile   -> today's OCI build
    //   docker/podman + only .def    -> error (Singularity-only env)
    //   apptainer + .def             -> native apptainer build (no Docker needed)
    //   apptainer + only Dockerfile  -> fallback: OCI build + .sif convert
    //                                   (requires docker/podman on the host)
    //   apptainer + both             -> prefer .def
    let has_recipe = ec.dockerfile.is_some() || ec.singularity_def.is_some();
    let recipe_changed = dockerfile_changed || deffile_changed;
    let want_build = has_recipe
        && !opts.skip_dockerfile_build
        && (opts.is_new
            || recipe_changed
            || !opts.includes.is_empty()
            || opts.base_image.is_some()
            || opts.engine.is_some()
            // For update with no specific changes, rebuild if a recipe exists.
            || (!opts.is_new
                && opts.dockerfile.is_none()
                && opts.deffile.is_none()
                && opts.includes.is_empty()));

    if want_build {
        run_recipe_build(scope, name, &mut ec, &cfg_dir, opts, dockerfile_changed, deffile_changed)?;
    }

    // Always reconcile the stored morloc version against the actual image.
    // - For `new --version 0.77.0-rc.6`, the binary reports "0.77.0" (stack
    //   does not expose prerelease tags), so keep the recorded value when
    //   major.minor.patch match — the recorded tag is more informative.
    // - For `new --image <custom>` or `update --image ...`, nothing was
    //   recorded yet, so store the detected version.
    // - If the image has no morloc binary (e.g., a bare base image staged
    //   for a Dockerfile layer not yet built), silently leave the field
    //   unchanged rather than failing the whole operation.
    let detect_target = ec.active_image().to_string();
    if !detect_target.is_empty() {
        if let Ok(detected) = detect_morloc_version(ec.engine, &detect_target) {
            ec.morloc_version = Some(match ec.morloc_version.take() {
                Some(recorded) if recorded.major == detected.major
                    && recorded.minor == detected.minor
                    && recorded.patch == detected.patch => recorded,
                _ => detected,
            });
        }
    }

    // Write environment config
    config::write_env_config(scope, name, &ec)?;

    Ok(())
}

/// Dispatch the recipe build per the engine x recipe matrix documented in the
/// caller. Mutates `ec` with the resulting image identifier (OCI tag or .sif
/// path) and content hash.
fn run_recipe_build(
    scope: Scope,
    name: &str,
    ec: &mut EnvironmentConfig,
    cfg_dir: &Path,
    opts: &ApplyOptions,
    dockerfile_changed: bool,
    deffile_changed: bool,
) -> Result<()> {
    let df_path = config::env_dockerfile_path(scope, name);
    let def_path = config::env_deffile_path(scope, name);

    // Materialize build-phase flags: env.flags.yaml `build.<engine>` prepended
    // to `build.all`, then this invocation's --reinit-arg flags appended.
    // The engine resolves duplicates last-wins.
    let mut build_extra_flags = config::read_flag_config(scope, name)?
        .materialize(Phase::Build, ec.engine);
    build_extra_flags.extend(opts.reinit_args.iter().cloned());

    match ec.engine {
        ContainerEngine::Docker | ContainerEngine::Podman => {
            if ec.dockerfile.is_none() {
                // Only the .def is present; docker/podman cannot consume it.
                return Err(ManagerError::EnvError(format!(
                    "Environment '{name}' has only a Singularity .def recipe. \
                     Either switch to `--engine apptainer` or add a Dockerfile \
                     via `update --dockerfile-stub`."
                )));
            }
            if !df_path.is_file() {
                return Ok(());
            }
            let hash = hash_file(&df_path)?;
            let tag = format!("localhost/morloc-env:{name}");
            let unchanged = !opts.is_new
                && !dockerfile_changed
                && opts.includes.is_empty()
                && opts.base_image.is_none()
                && ec.content_hash.as_deref() == Some(hash.as_str())
                && ec
                    .built_image
                    .as_ref()
                    .map(|img| image_exists_locally(ec.engine, img))
                    .unwrap_or(false);
            if unchanged {
                eprintln!("Dockerfile unchanged; skipping rebuild.");
                return Ok(());
            }
            let build_cfg = BuildConfig {
                dockerfile: df_path.to_string_lossy().to_string(),
                context: cfg_dir.to_string_lossy().to_string(),
                tag: tag.clone(),
                build_args: vec![("CONTAINER_BASE".to_string(), ec.base_image.clone())],
                extra_flags: build_extra_flags.clone(),
            };
            if opts.verbose {
                let exe = engine_executable(ec.engine);
                eprintln!(
                    "[morloc-manager] {exe} build -f {} -t {} {} {}",
                    build_cfg.dockerfile, build_cfg.tag,
                    build_extra_flags.join(" "), build_cfg.context
                );
            }
            let status = container_build_visible(ec.engine, &build_cfg);
            if !status.success() {
                return Err(ManagerError::EngineError {
                    engine: ec.engine,
                    code: exit_code_to_int(status),
                    stderr: "Build failed (see output above)".to_string(),
                });
            }
            ec.built_image = Some(tag);
            ec.content_hash = Some(hash);
            Ok(())
        }
        ContainerEngine::Apptainer => {
            // Native path: prefer the .def when present (Docker-free build).
            if def_path.is_file() {
                let def_hash = hash_file(&def_path)?;
                let out_sif = config::env_layered_sif_path(scope, name);
                let unchanged = !opts.is_new
                    && !deffile_changed
                    && opts.includes.is_empty()
                    && opts.base_image.is_none()
                    && ec.def_content_hash.as_deref() == Some(def_hash.as_str())
                    && out_sif.is_file();
                if unchanged {
                    eprintln!(".def recipe unchanged; skipping rebuild.");
                    return Ok(());
                }
                let base_sif = ec.base_sif.as_deref().ok_or_else(|| {
                    ManagerError::EnvError(format!(
                        "Environment '{name}' has no base .sif recorded. \
                         Re-run `morloc-manager new` or `update --image <ref>` \
                         to pull a base image first."
                    ))
                })?;
                let cfg = ApptainerNativeBuildConfig {
                    deffile: def_path.to_string_lossy().to_string(),
                    output_sif: out_sif.to_string_lossy().to_string(),
                    build_args: vec![("BASE_SIF".to_string(), base_sif.to_string())],
                    extra_flags: build_extra_flags.clone(),
                };
                if opts.verbose {
                    let exe = engine_executable(ec.engine);
                    eprintln!(
                        "[morloc-manager] {exe} build {} --build-arg BASE_SIF={} {} {}",
                        build_extra_flags.join(" "),
                        base_sif,
                        out_sif.display(),
                        def_path.display()
                    );
                }
                let status = apptainer_build_native(&cfg);
                if !status.success() {
                    return Err(ManagerError::EngineError {
                        engine: ec.engine,
                        code: exit_code_to_int(status),
                        stderr: "Apptainer build failed (see output above)".to_string(),
                    });
                }
                ec.layered_sif = Some(out_sif.to_string_lossy().to_string());
                ec.def_content_hash = Some(def_hash);
                return Ok(());
            }

            // Fallback path: Dockerfile-only on apptainer. Requires a local
            // OCI builder (docker or podman).
            if !df_path.is_file() {
                return Ok(());
            }
            let oci_engine = detect_oci_builder().ok_or_else(|| {
                ManagerError::EnvError(format!(
                    "Environment '{name}' has a Dockerfile but Apptainer cannot read \
                     Dockerfiles directly, and no Docker or Podman was found on PATH \
                     to convert it. Either install one, or replace the Dockerfile \
                     with a Singularity .def via `update --deffile-stub`."
                ))
            })?;
            let df_hash = hash_file(&df_path)?;
            let out_sif = config::env_layered_sif_path(scope, name);
            let unchanged = !opts.is_new
                && !dockerfile_changed
                && opts.includes.is_empty()
                && opts.base_image.is_none()
                && ec.content_hash.as_deref() == Some(df_hash.as_str())
                && out_sif.is_file();
            if unchanged {
                eprintln!("Dockerfile unchanged; skipping rebuild.");
                return Ok(());
            }
            let oci_tag = format!("localhost/morloc-env:{name}");
            // The OCI build helper step gets no user build-flags. Those are
            // intended for `apptainer build`, not the upstream `docker
            // build` that produces the source image. Forwarding them
            // would risk mixing flag dialects.
            let build_cfg = BuildConfig {
                dockerfile: df_path.to_string_lossy().to_string(),
                context: cfg_dir.to_string_lossy().to_string(),
                tag: oci_tag.clone(),
                build_args: vec![("CONTAINER_BASE".to_string(), ec.base_image.clone())],
                extra_flags: Vec::new(),
            };
            if opts.verbose {
                let oci_exe = engine_executable(oci_engine);
                eprintln!(
                    "[morloc-manager] (apptainer fallback) {oci_exe} build -f {} -t {} {}",
                    build_cfg.dockerfile, build_cfg.tag, build_cfg.context
                );
            }
            let oci_status = container_build_visible(oci_engine, &build_cfg);
            if !oci_status.success() {
                return Err(ManagerError::EngineError {
                    engine: oci_engine,
                    code: exit_code_to_int(oci_status),
                    stderr: "OCI build (for apptainer fallback) failed (see output above)".to_string(),
                });
            }
            let convert_cfg = ApptainerOciConvertConfig {
                source_engine: oci_engine,
                source_tag: oci_tag.clone(),
                output_sif: out_sif.to_string_lossy().to_string(),
                extra_flags: build_extra_flags.clone(),
            };
            let convert_status = apptainer_build_from_oci_daemon(&convert_cfg);
            if !convert_status.success() {
                return Err(ManagerError::EngineError {
                    engine: ec.engine,
                    code: exit_code_to_int(convert_status),
                    stderr: "OCI -> .sif conversion failed (see output above)".to_string(),
                });
            }
            ec.built_image = Some(oci_tag);
            ec.content_hash = Some(df_hash);
            ec.layered_sif = Some(out_sif.to_string_lossy().to_string());
            eprintln!(
                "[morloc-manager] note: using {} as a build helper. For a Docker-free \
                 build, add a .def recipe via `update --deffile-stub`.",
                engine_executable(oci_engine)
            );
            Ok(())
        }
    }
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

    // If the active env was this one, clear it in both local and system configs
    for cfg_scope in [Scope::Local, Scope::System] {
        let cfg_path = config::config_path(cfg_scope);
        if let Ok(cfg) = config::read_config::<Config>(&cfg_path) {
            if cfg.active_env.as_deref() == Some(name) {
                let new_cfg = Config {
                    active_env: None,
                    ..cfg
                };
                let _ = config::write_config(&cfg_path, &new_cfg);
            }
        }
    }

    Ok(())
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
    // Check if any environments exist to give a better suggestion
    let local_envs = config::list_env_names(Scope::Local);
    let system_envs = config::list_env_names(Scope::System);
    if local_envs.is_empty() && system_envs.is_empty() {
        Err(ManagerError::NoActiveEnvironment)
    } else {
        // Label each entry with its scope so same-named envs are distinguishable.
        // System envs are flagged with --system to disambiguate in select.
        let mut available: Vec<String> = local_envs
            .iter()
            .map(|n| format!("{n} (local)"))
            .collect();
        available.extend(system_envs.iter().map(|n| format!("{n} (system)")));
        Err(ManagerError::EnvError(format!(
            "No active environment. Select one with: morloc-manager select <name>\n\
             Available: {}",
            available.join(", ")
        )))
    }
}

// ======================================================================
// Internal
// ======================================================================

pub fn is_valid_shm_size(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }
    let (digits, suffix) = if s.ends_with(|c: char| "bkmgBKMG".contains(c)) {
        (&s[..s.len() - 1], true)
    } else {
        (s, false)
    };
    !digits.is_empty() && digits.chars().all(|c| c.is_ascii_digit()) && (suffix || !digits.is_empty())
}

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
