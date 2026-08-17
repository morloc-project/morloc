//! Native-environment provisioning: fetch the version-matched morloc runtime
//! trio (compiler + libmorloc + nexus) and the rust sources from a GitHub
//! release into `runtimes/<version>/`, so `morloc init` (pointed there via
//! MORLOC_RUST_BIN) can set up a native environment with no container.
//!
//! Coherence: every piece is pulled from a SINGLE release tag, so they share a
//! version by construction; the tag's manifest declares that version. This
//! single-tag fetch is the interim coherence guarantee while the ABI-version
//! gate (`morloc_abi_version()`) stays deferred.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use serde::Deserialize;

use crate::config;
use crate::error::{ManagerError, Result};
use crate::types::Scope;

/// GitHub release download base for the morloc project.
const RELEASE_BASE: &str = "https://github.com/morloc-project/morloc/releases/download";
/// The manifest asset attached to every release (see .github/workflows/release.yml).
const MANIFEST_ASSET: &str = "morloc-release-manifest.json";
/// The language-support table asset (compiler-owned static data). Downloaded so
/// the manager can render pixi manifests without executing the compiler on the
/// host. Filename in the runtime store once downloaded.
const LANG_SUPPORT_ASSET: &str = "morloc-lang-support.json";
/// Filename of the downloaded lang-support table inside `runtimes/<version>/`.
pub const LANG_SUPPORT_FILE: &str = "lang-support.json";
/// Highest release-manifest schema this build understands.
const SUPPORTED_MANIFEST_SCHEMA: u32 = 1;

/// The loose artifacts that make up a native runtime trio for one platform.
#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub struct TripleAssets {
    pub morloc: String,
    pub libmorloc: String,
    pub nexus: String,
    pub manager: String,
}

/// The native-install manifest attached to a release.
#[derive(Debug, Clone, Deserialize)]
pub struct ReleaseManifest {
    pub schema: u32,
    pub version: String,
    pub rust_src: String,
    /// Release-triple ("linux-x86_64", ...) -> its runtime trio. Only triples
    /// with a complete, tested native trio are present.
    pub triples: BTreeMap<String, TripleAssets>,
}

impl ReleaseManifest {
    pub fn from_json(text: &str) -> Result<Self> {
        let m: ReleaseManifest = serde_json::from_str(text)
            .map_err(|e| ManagerError::EnvError(format!("Failed to parse {MANIFEST_ASSET}: {e}")))?;
        if m.schema > SUPPORTED_MANIFEST_SCHEMA {
            return Err(ManagerError::EnvError(format!(
                "{MANIFEST_ASSET} schema {} is newer than this morloc-manager supports \
                 (up to {SUPPORTED_MANIFEST_SCHEMA}); upgrade morloc-manager.",
                m.schema
            )));
        }
        Ok(m)
    }

    /// The runtime trio for a release triple, if this release provides one.
    pub fn assets_for(&self, triple: &str) -> Option<&TripleAssets> {
        self.triples.get(triple)
    }
}

/// The release triple string (matching release.yml asset naming) for an
/// (os, arch) pair, or None where no native artifacts are published.
pub fn release_triple(os: &str, arch: &str) -> Option<&'static str> {
    match (os, arch) {
        ("linux", "x86_64") => Some("linux-x86_64"),
        ("linux", "aarch64") => Some("linux-arm64"),
        ("macos", "aarch64") => Some("macos-arm64"),
        // Intel macOS, Windows, etc.: no native artifacts published.
        _ => None,
    }
}

/// The release triple for the current host.
pub fn host_release_triple() -> Option<&'static str> {
    release_triple(std::env::consts::OS, std::env::consts::ARCH)
}

/// GitHub download URL for `asset` of a release `tag` (e.g. "v0.98.3" or "dev").
pub fn asset_url(tag: &str, asset: &str) -> String {
    format!("{RELEASE_BASE}/{tag}/{asset}")
}

/// A curl command hardened with the same TLS/proto flags and stdio for every
/// fetch. Callers add `-o <dest>` (download) or just the URL (capture).
fn curl_base() -> Command {
    let mut c = Command::new("curl");
    c.args(["--proto", "=https", "--tlsv1.2", "-fsSL"])
        .stdin(Stdio::null())
        .stderr(Stdio::inherit());
    c
}

fn curl_spawn_err(e: std::io::Error) -> ManagerError {
    ManagerError::EnvError(format!("could not run curl (is it installed?): {e}"))
}

/// Capture a URL's body to a string via curl (follows redirects; fails on HTTP
/// error). Used for the release manifest + the releases API.
fn curl_capture(url: &str) -> Result<String> {
    let out = curl_base().arg(url).output().map_err(curl_spawn_err)?;
    if !out.status.success() {
        return Err(ManagerError::EnvError(format!("download failed: {url}")));
    }
    Ok(String::from_utf8_lossy(&out.stdout).to_string())
}

/// Resolve the GitHub tag of the latest published (non-prerelease) morloc
/// release via the releases API.
pub fn fetch_latest_tag() -> Result<String> {
    let json = curl_capture("https://api.github.com/repos/morloc-project/morloc/releases/latest")?;
    let v: serde_json::Value = serde_json::from_str(&json)
        .map_err(|e| ManagerError::EnvError(format!("could not parse releases API response: {e}")))?;
    v.get("tag_name")
        .and_then(|t| t.as_str())
        .map(|s| s.to_string())
        .ok_or_else(|| ManagerError::EnvError("no tag_name in releases API response".to_string()))
}

/// Resolve a requested release specifier to a concrete GitHub tag. "latest"
/// queries the API; a bare version like "0.98.3" becomes "v0.98.3"; anything
/// else (an explicit tag such as "dev" or "v0.98.3") is used verbatim.
pub fn resolve_tag(requested: &str) -> Result<String> {
    if requested == "latest" {
        fetch_latest_tag()
    } else if requested.chars().next().is_some_and(|c| c.is_ascii_digit()) {
        Ok(format!("v{requested}"))
    } else {
        Ok(requested.to_string())
    }
}

/// The per-version native runtime store directory.
pub fn runtimes_dir(scope: Scope, version: &str) -> PathBuf {
    config::data_dir(scope).join("runtimes").join(version)
}

/// Best-effort download (quiet): returns whether it succeeded. Used for optional
/// assets (e.g. the lang-support table, absent from older releases).
fn curl_download_optional(url: &str, dest: &Path) -> bool {
    curl_base()
        .stderr(Stdio::null())
        .arg("-o")
        .arg(dest)
        .arg(url)
        .status()
        .map(|s| s.success())
        .unwrap_or(false)
}

/// Download a URL to a path via curl (follows redirects; fails on HTTP error).
fn curl_download(url: &str, dest: &Path) -> Result<()> {
    let status = curl_base()
        .arg("-o")
        .arg(dest)
        .arg(url)
        .status()
        .map_err(curl_spawn_err)?;
    if !status.success() {
        return Err(ManagerError::EnvError(format!("download failed: {url}")));
    }
    Ok(())
}

/// chmod 0755 (downloaded binaries arrive without the executable bit).
fn make_executable(path: &Path) -> Result<()> {
    use std::os::unix::fs::PermissionsExt;
    let mut perms = std::fs::metadata(path)
        .map_err(|e| ManagerError::EnvError(format!("cannot stat {}: {e}", path.display())))?
        .permissions();
    perms.set_mode(0o755);
    std::fs::set_permissions(path, perms)
        .map_err(|e| ManagerError::EnvError(format!("cannot chmod {}: {e}", path.display())))
}

/// Extract a .tar.gz into `dest` (the rust-src tarball unpacks to `rust/`).
/// `--no-same-owner`: extracted files belong to the current user, not the uids
/// recorded in the archive (restoring those fails for a non-root user, and for
/// root inside a user namespace where the uid is unmapped).
fn extract_tar_gz(tar: &Path, dest: &Path) -> Result<()> {
    let status = Command::new("tar")
        .arg("--no-same-owner")
        .arg("-xzf")
        .arg(tar)
        .arg("-C")
        .arg(dest)
        .stdin(Stdio::null())
        .stderr(Stdio::inherit())
        .status()
        .map_err(|e| ManagerError::EnvError(format!("could not run tar: {e}")))?;
    if !status.success() {
        return Err(ManagerError::EnvError(format!(
            "tar extract failed: {}",
            tar.display()
        )));
    }
    Ok(())
}

/// Fetch and parse the release manifest attached to a GitHub `tag`. The
/// manifest declares the concrete version and the per-triple asset names.
pub fn fetch_manifest(tag: &str) -> Result<ReleaseManifest> {
    let text = curl_capture(&asset_url(tag, MANIFEST_ASSET))?;
    ReleaseManifest::from_json(&text)
}

/// Provision the native runtime into `runtimes/<version>/` and return that
/// directory (suitable as MORLOC_RUST_BIN). Downloads the version-coherent trio
/// (compiler + libmorloc + nexus) under the fixed names `morloc init` expects,
/// plus the rust sources (rustmorloc is compiled locally by init). The
/// already-installed `morloc-manager` bootstrap is not re-fetched.
pub fn provision_runtime(scope: Scope, requested: &str) -> Result<(PathBuf, String)> {
    let triple = host_release_triple().ok_or_else(|| {
        ManagerError::BackendUnsupported(format!(
            "no prebuilt native runtime is published for this platform ({}/{}); \
             use a container backend",
            std::env::consts::OS,
            std::env::consts::ARCH
        ))
    })?;

    // Resolve "latest"/a bare version to a concrete download tag (asset URLs need
    // the real tag, not the "latest" alias).
    let tag = resolve_tag(requested)?;
    let manifest = fetch_manifest(&tag)?;
    let version = manifest.version.clone();
    let assets = manifest.assets_for(triple).ok_or_else(|| {
        ManagerError::BackendUnsupported(format!(
            "release {tag} publishes no native runtime for {triple}; use a container backend"
        ))
    })?;

    let dir = runtimes_dir(scope, &version);
    // The trio + extracted rust/ are the expensive download; skip it when already
    // present (idempotent re-materialize; a mid-extract interruption re-does it).
    let trio_present = dir.join("morloc").exists()
        && dir.join("libmorloc.so").exists()
        && dir.join("morloc-nexus").exists()
        && dir.join("rust").is_dir();
    if !trio_present {
        std::fs::create_dir_all(&dir)
            .map_err(|e| ManagerError::EnvError(format!("cannot create {}: {e}", dir.display())))?;
        // MORLOC_RUST_BIN expects these exact filenames; download to them directly.
        let nexus_dest = dir.join("morloc-nexus");
        let morloc_dest = dir.join("morloc");
        curl_download(&asset_url(&tag, &assets.libmorloc), &dir.join("libmorloc.so"))?;
        curl_download(&asset_url(&tag, &assets.nexus), &nexus_dest)?;
        curl_download(&asset_url(&tag, &assets.morloc), &morloc_dest)?;
        make_executable(&nexus_dest)?;
        make_executable(&morloc_dest)?;

        // rustmorloc is an rlib compiled locally by `morloc init`; ship its source.
        let src_tar = dir.join(&manifest.rust_src);
        curl_download(&asset_url(&tag, &manifest.rust_src), &src_tar)?;
        extract_tar_gz(&src_tar, &dir)?;
    }

    // The lang-support table lets the manager render pixi manifests without
    // running the compiler on the host (essential where the glibc compiler can't
    // execute, e.g. NixOS/musl). Best-effort: older releases lack this asset.
    let ls = dir.join(LANG_SUPPORT_FILE);
    if !ls.exists() {
        let _ = curl_download_optional(&asset_url(&tag, LANG_SUPPORT_ASSET), &ls);
    }

    Ok((dir, version))
}

// ======================================================================
// Staging a provisioned runtime into a container build context
// ======================================================================

/// Copy a file, mapping IO errors to a legible provisioning error.
fn copy_file(src: &Path, dst: &Path) -> Result<()> {
    std::fs::copy(src, dst).map(|_| ()).map_err(|e| {
        ManagerError::EnvError(format!(
            "cannot copy {} -> {}: {e}",
            src.display(),
            dst.display()
        ))
    })
}

/// Recursively copy `src` into `dst`, skipping any directory whose name is in
/// `skip` (e.g. `target` build artifacts). Symlinks are followed as files.
fn copy_dir_excluding(src: &Path, dst: &Path, skip: &[&str]) -> Result<()> {
    std::fs::create_dir_all(dst)
        .map_err(|e| ManagerError::EnvError(format!("cannot create {}: {e}", dst.display())))?;
    let entries = std::fs::read_dir(src)
        .map_err(|e| ManagerError::EnvError(format!("cannot read {}: {e}", src.display())))?;
    for entry in entries.flatten() {
        let from = entry.path();
        let name = entry.file_name();
        let to = dst.join(&name);
        if from.is_dir() {
            if skip.iter().any(|s| name.to_str() == Some(s)) {
                continue;
            }
            copy_dir_excluding(&from, &to, skip)?;
        } else {
            copy_file(&from, &to)?;
        }
    }
    Ok(())
}

/// Stage a provisioned runtime (`runtimes/<version>/`, from `provision_runtime`)
/// into a container build context's `runtime/` directory: the morloc trio
/// (compiler + libmorloc.so + morloc-nexus) plus the Rust workspace source
/// (rustmorloc is compiled inside the image). Everything comes from the
/// downloaded runtime -- no host morloc install is required.
pub fn stage_runtime(src: &Path, dest: &Path) -> Result<()> {
    std::fs::create_dir_all(dest)
        .map_err(|e| ManagerError::EnvError(format!("cannot create {}: {e}", dest.display())))?;
    for f in ["libmorloc.so", "morloc-nexus", "morloc"] {
        let from = src.join(f);
        if !from.exists() {
            return Err(ManagerError::EnvError(format!(
                "provisioned runtime is missing {f} at {}",
                from.display()
            )));
        }
        copy_file(&from, &dest.join(f))?;
    }
    make_executable(&dest.join("morloc-nexus"))?;
    make_executable(&dest.join("morloc"))?;
    let rust = src.join("rust");
    if rust.is_dir() {
        copy_dir_excluding(&rust, &dest.join("rust"), &["target"])?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = r#"{
      "schema": 1,
      "version": "0.98.3",
      "rust_src": "morloc-rust-src.tar.gz",
      "triples": {
        "linux-x86_64": {
          "morloc": "morloc-linux-x86_64",
          "libmorloc": "libmorloc-linux-x86_64.so",
          "nexus": "morloc-nexus-linux-x86_64",
          "manager": "morloc-manager-linux-x86_64"
        }
      }
    }"#;

    #[test]
    fn parses_manifest() {
        let m = ReleaseManifest::from_json(SAMPLE).unwrap();
        assert_eq!(m.version, "0.98.3");
        assert_eq!(m.rust_src, "morloc-rust-src.tar.gz");
        let a = m.assets_for("linux-x86_64").unwrap();
        assert_eq!(a.libmorloc, "libmorloc-linux-x86_64.so");
        assert_eq!(a.morloc, "morloc-linux-x86_64");
        // A triple with no published native runtime is absent.
        assert!(m.assets_for("macos-arm64").is_none());
    }

    #[test]
    fn rejects_future_schema() {
        let j = r#"{"schema":999,"version":"9.9.9","rust_src":"x","triples":{}}"#;
        assert!(ReleaseManifest::from_json(j).is_err());
    }

    #[test]
    fn triple_mapping() {
        assert_eq!(release_triple("linux", "x86_64"), Some("linux-x86_64"));
        assert_eq!(release_triple("linux", "aarch64"), Some("linux-arm64"));
        assert_eq!(release_triple("macos", "aarch64"), Some("macos-arm64"));
        // Intel macOS and Windows have no native artifacts.
        assert_eq!(release_triple("macos", "x86_64"), None);
        assert_eq!(release_triple("windows", "x86_64"), None);
    }

    #[test]
    fn asset_urls_use_the_tag() {
        assert_eq!(
            asset_url("v0.98.3", "morloc-linux-x86_64"),
            "https://github.com/morloc-project/morloc/releases/download/v0.98.3/morloc-linux-x86_64"
        );
        // A channel tag (e.g. "dev") is used verbatim.
        assert_eq!(
            asset_url("dev", "morloc-release-manifest.json"),
            "https://github.com/morloc-project/morloc/releases/download/dev/morloc-release-manifest.json"
        );
    }

    #[test]
    fn resolve_tag_normalizes_versions_and_passes_channels() {
        assert_eq!(resolve_tag("0.98.3").unwrap(), "v0.98.3");
        assert_eq!(resolve_tag("v0.98.3").unwrap(), "v0.98.3");
        assert_eq!(resolve_tag("dev").unwrap(), "dev");
    }

    #[test]
    fn runtimes_dir_is_versioned() {
        let d = runtimes_dir(Scope::Local, "0.98.3");
        assert!(d.ends_with("morloc/runtimes/0.98.3"));
    }
}
