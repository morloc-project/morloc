//! Native-environment provisioning: fetch the version-matched morloc runtime
//! trio (compiler + libmorloc + nexus) and the rust sources from a GitHub
//! release into `runtimes/<version>/`, so `morloc init` (pointed there via
//! MORLOC_RUST_BIN) can set up a native environment with no container.
//!
//! Coherence: every piece is pulled from a SINGLE release tag, so they share a
//! version by construction, and the parsed manifest's `version` is checked
//! against the requested one. This single-tag fetch is the interim coherence
//! guarantee while the ABI-version gate (`morloc_abi_version()`) stays deferred.

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

/// GitHub download URL for `asset` of release tag `v<version>`.
pub fn asset_url(version: &str, asset: &str) -> String {
    format!("{RELEASE_BASE}/v{version}/{asset}")
}

/// The per-version native runtime store directory.
pub fn runtimes_dir(scope: Scope, version: &str) -> PathBuf {
    config::data_dir(scope).join("runtimes").join(version)
}

/// Download a URL to a path via curl (follows redirects; fails on HTTP error).
fn curl_download(url: &str, dest: &Path) -> Result<()> {
    let status = Command::new("curl")
        .args(["--proto", "=https", "--tlsv1.2", "-fsSL", "-o"])
        .arg(dest)
        .arg(url)
        .stdin(Stdio::null())
        .stderr(Stdio::inherit())
        .status()
        .map_err(|e| {
            ManagerError::EnvError(format!("could not run curl (is it installed?): {e}"))
        })?;
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
fn extract_tar_gz(tar: &Path, dest: &Path) -> Result<()> {
    let status = Command::new("tar")
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

/// Fetch and parse the release manifest for `version`, verifying it describes
/// the release we asked for.
pub fn fetch_manifest(scope: Scope, version: &str) -> Result<ReleaseManifest> {
    let dir = runtimes_dir(scope, version);
    std::fs::create_dir_all(&dir)
        .map_err(|e| ManagerError::EnvError(format!("cannot create {}: {e}", dir.display())))?;
    let manifest_path = dir.join(MANIFEST_ASSET);
    curl_download(&asset_url(version, MANIFEST_ASSET), &manifest_path)?;
    let text = std::fs::read_to_string(&manifest_path)
        .map_err(|e| ManagerError::EnvError(format!("cannot read {}: {e}", manifest_path.display())))?;
    let manifest = ReleaseManifest::from_json(&text)?;
    if manifest.version != version {
        return Err(ManagerError::EnvError(format!(
            "release manifest version '{}' does not match requested '{version}'",
            manifest.version
        )));
    }
    Ok(manifest)
}

/// Provision the native runtime into `runtimes/<version>/` and return that
/// directory (suitable as MORLOC_RUST_BIN). Downloads the version-coherent trio
/// (compiler + libmorloc + nexus) under the fixed names `morloc init` expects,
/// plus the rust sources (rustmorloc is compiled locally by init). The
/// already-installed `morloc-manager` bootstrap is not re-fetched.
pub fn provision_runtime(scope: Scope, version: &str) -> Result<PathBuf> {
    let triple = host_release_triple().ok_or_else(|| {
        ManagerError::BackendUnsupported(format!(
            "no prebuilt native runtime is published for this platform ({}/{}); \
             use a container backend",
            std::env::consts::OS,
            std::env::consts::ARCH
        ))
    })?;

    let manifest = fetch_manifest(scope, version)?;
    let assets = manifest.assets_for(triple).ok_or_else(|| {
        ManagerError::BackendUnsupported(format!(
            "release {version} publishes no native runtime for {triple}; use a container backend"
        ))
    })?;

    let dir = runtimes_dir(scope, version);
    // MORLOC_RUST_BIN expects these exact filenames; download to them directly.
    let libmorloc_dest = dir.join("libmorloc.so");
    let nexus_dest = dir.join("morloc-nexus");
    let morloc_dest = dir.join("morloc");
    curl_download(&asset_url(version, &assets.libmorloc), &libmorloc_dest)?;
    curl_download(&asset_url(version, &assets.nexus), &nexus_dest)?;
    curl_download(&asset_url(version, &assets.morloc), &morloc_dest)?;
    make_executable(&nexus_dest)?;
    make_executable(&morloc_dest)?;

    // rustmorloc is an rlib compiled locally by `morloc init`; ship its source.
    let src_tar = dir.join(&manifest.rust_src);
    curl_download(&asset_url(version, &manifest.rust_src), &src_tar)?;
    extract_tar_gz(&src_tar, &dir)?;

    Ok(dir)
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
            asset_url("0.98.3", "morloc-linux-x86_64"),
            "https://github.com/morloc-project/morloc/releases/download/v0.98.3/morloc-linux-x86_64"
        );
    }

    #[test]
    fn runtimes_dir_is_versioned() {
        let d = runtimes_dir(Scope::Local, "0.98.3");
        assert!(d.ends_with("morloc/runtimes/0.98.3"));
    }
}
