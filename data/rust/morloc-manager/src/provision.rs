//! Native-environment provisioning: fetch the version-matched morloc compiler
//! and the rust sources from a GitHub release into `runtimes/<version>/`, so
//! `morloc init` (pointed at `<dir>/rust` via MORLOC_RUST_DIR) can build the
//! runtime from source and set up a native environment with no container.
//!
//! Coherence: every piece is pulled from a SINGLE release tag, so they share a
//! version by construction; the tag's manifest declares that version. This
//! single-tag fetch is the interim coherence guarantee while the ABI-version
//! gate (`morloc_abi_version()`) stays deferred.

use std::collections::BTreeMap;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use serde::Deserialize;
use sha2::{Digest, Sha256};

use crate::config;
use crate::error::{ManagerError, Result};
use crate::types::Scope;

/// GitHub release download base for the morloc project.
const RELEASE_BASE: &str = "https://github.com/morloc-project/morloc/releases/download";
/// GitHub release *page* base (used to check that a tag exists, independent of
/// which assets it publishes).
const RELEASE_TAG_BASE: &str = "https://github.com/morloc-project/morloc/releases/tag";
/// The manifest asset attached to every release (see .github/workflows/release.yml).
const MANIFEST_ASSET: &str = "morloc-release-manifest.json";
/// Pinned pixi version morloc-manager provisions + generates container images
/// against (single source of truth for both). Bundling pixi keeps the native
/// solve self-contained; linking rattler is the eventual replacement.
pub const PIXI_VERSION: &str = "0.76.2";
/// The language-support table asset (compiler-owned static data). Downloaded so
/// the manager can render pixi manifests without executing the compiler on the
/// host. Filename in the runtime store once downloaded.
const LANG_SUPPORT_ASSET: &str = "morloc-lang-support.json";
/// Filename of the downloaded lang-support table inside `runtimes/<version>/`.
pub const LANG_SUPPORT_FILE: &str = "lang-support.json";
/// Highest release-manifest schema this build understands. v2 dropped the
/// prebuilt `libmorloc`/`nexus` assets: the runtime is now built from source at
/// `morloc init` with the env's toolchain (ABI coherence), so a release ships
/// only the compiler + static manager per platform.
const SUPPORTED_MANIFEST_SCHEMA: u32 = 2;

/// The per-platform prebuilt binaries in a release: the morloc compiler and the
/// static morloc-manager. libmorloc.so + morloc-nexus are NOT here -- they are
/// built from the (platform-independent) Rust source at `morloc init`.
#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub struct TripleAssets {
    pub morloc: String,
    pub manager: String,
}

/// The native-install manifest attached to a release.
#[derive(Debug, Clone, Deserialize)]
pub struct ReleaseManifest {
    pub schema: u32,
    pub version: String,
    pub rust_src: String,
    /// Release-triple ("linux-x86_64", ...) -> its prebuilt assets. Only triples
    /// with a complete, tested asset set are present.
    pub triples: BTreeMap<String, TripleAssets>,
    /// Asset filename -> lowercase hex SHA-256. Every downloaded artifact whose
    /// name appears here is verified after fetch; artifacts absent from the map
    /// (older releases that predate digest publishing) are fetched atomically but
    /// not verified. Defaulted so pre-digest manifests still parse.
    #[serde(default)]
    pub sha256: BTreeMap<String, String>,
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

    /// The prebuilt assets for a release triple, if this release provides one.
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

/// The public release page for `tag` (HTTP 200 if the release exists, 404
/// otherwise). Used as an asset-independent existence check and in diagnostics.
pub fn release_page_url(tag: &str) -> String {
    format!("{RELEASE_TAG_BASE}/{tag}")
}

/// Verify a requested version resolves to a published release, returning the
/// resolved tag. Checks the release PAGE (which 404s for a nonexistent tag)
/// rather than a specific asset, so it does not depend on which assets a given
/// release publishes.
pub fn verify_release(requested: &str) -> Result<String> {
    let tag = resolve_tag(requested)?;
    let url = release_page_url(&tag);
    // -I/-o /dev/null: a HEAD that discards the body; -f (from curl_base) makes a
    // 404 a non-success exit. Silence curl's own stderr so the caller controls
    // the message.
    let ok = curl_base()
        .args(["-I", "-o", "/dev/null"])
        .arg(&url)
        .stderr(Stdio::null())
        .status()
        .map_err(curl_spawn_err)?
        .success();
    if ok {
        Ok(tag)
    } else {
        Err(ManagerError::EnvError(format!("no published release at {url}")))
    }
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
/// error). Used to fetch the release manifest.
fn curl_capture(url: &str) -> Result<String> {
    let out = curl_base().arg(url).output().map_err(curl_spawn_err)?;
    if !out.status.success() {
        return Err(ManagerError::EnvError(format!("download failed: {url}")));
    }
    Ok(String::from_utf8_lossy(&out.stdout).to_string())
}

/// The final URL after following redirects for `url` (an HTTP HEAD; the body is
/// discarded). Used to resolve the `releases/latest` redirect to its tag page.
fn curl_effective_url(url: &str) -> Result<String> {
    let out = curl_base()
        .args(["-I", "-o", "/dev/null", "-w", "%{url_effective}"])
        .arg(url)
        .output()
        .map_err(curl_spawn_err)?;
    if !out.status.success() {
        return Err(ManagerError::EnvError(format!(
            "could not resolve the latest release (is there a published release?): {url}"
        )));
    }
    Ok(String::from_utf8_lossy(&out.stdout).trim().to_string())
}

/// Extract the release tag from a `.../releases/tag/<tag>` URL (the target of the
/// `releases/latest` redirect).
fn parse_tag_from_release_url(url: &str) -> Option<String> {
    url.rsplit_once("/tag/")
        .map(|(_, tag)| tag.trim_end_matches('/').to_string())
        .filter(|t| !t.is_empty())
}

/// Resolve the GitHub tag of the latest published morloc release.
///
/// This follows the unauthenticated `.../releases/latest` redirect (which lands
/// on `.../releases/tag/<tag>`) rather than the REST API. The REST API's
/// unauthenticated limit is 60 requests/hour PER IP, which silently breaks
/// provisioning behind shared IPs / CI / NAT; the web redirect carries no such
/// per-hour cap and needs no token. Set `MORLOC_RELEASE_TAG` to pin an explicit
/// tag and skip this lookup entirely.
pub fn fetch_latest_tag() -> Result<String> {
    let latest = "https://github.com/morloc-project/morloc/releases/latest";
    let final_url = curl_effective_url(latest)?;
    parse_tag_from_release_url(&final_url).ok_or_else(|| {
        ManagerError::EnvError(format!(
            "could not determine the latest release tag from {final_url}"
        ))
    })
}

/// Resolve a requested release specifier to a concrete GitHub tag. "latest"
/// queries the API; a bare version like "0.98.3" becomes "v0.98.3"; anything
/// else (an explicit tag such as "dev" or "v0.98.3") is used verbatim.
pub fn resolve_tag(requested: &str) -> Result<String> {
    let requested = requested.trim();
    if requested.eq_ignore_ascii_case("latest") {
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

/// Lowercase hex of a byte slice.
pub(crate) fn hex_lower(bytes: &[u8]) -> String {
    use std::fmt::Write;
    let mut s = String::with_capacity(bytes.len() * 2);
    for b in bytes {
        let _ = write!(s, "{b:02x}");
    }
    s
}

/// Streaming SHA-256 of a file, as lowercase hex.
fn file_sha256(path: &Path) -> Result<String> {
    let mut f = std::fs::File::open(path)
        .map_err(|e| ManagerError::EnvError(format!("cannot open {}: {e}", path.display())))?;
    let mut hasher = Sha256::new();
    let mut buf = [0u8; 65536];
    loop {
        let n = f
            .read(&mut buf)
            .map_err(|e| ManagerError::EnvError(format!("cannot read {}: {e}", path.display())))?;
        if n == 0 {
            break;
        }
        hasher.update(&buf[..n]);
    }
    Ok(hex_lower(&hasher.finalize()))
}

/// The sibling `<name>.part` staging path for a download destination.
fn part_path(dest: &Path) -> PathBuf {
    let name = dest
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_default();
    dest.with_file_name(format!("{name}.part"))
}

/// Download `asset` of release `tag` to `dest` atomically, verifying its SHA-256
/// against `digests` when a digest is published for that asset name. The body
/// lands on a temporary `.part` path and is renamed into place only after the
/// download (and any verification) succeeds, so an interrupted or corrupt fetch
/// never leaves a file that a presence check would bless as valid.
fn download_asset(
    tag: &str,
    asset: &str,
    dest: &Path,
    digests: &BTreeMap<String, String>,
) -> Result<()> {
    let part = part_path(dest);
    let result = (|| {
        curl_download(&asset_url(tag, asset), &part)?;
        if let Some(expected) = digests.get(asset) {
            let actual = file_sha256(&part)?;
            if !actual.eq_ignore_ascii_case(expected) {
                return Err(ManagerError::EnvError(format!(
                    "checksum mismatch for {asset}: expected {expected}, got {actual} \
                     (corrupt or tampered download)"
                )));
            }
        }
        std::fs::rename(&part, dest).map_err(|e| {
            ManagerError::EnvError(format!("cannot finalize {}: {e}", dest.display()))
        })
    })();
    // On any failure the staged body is never promoted; drop it so it can't be
    // mistaken for a finished download (a no-op when rename already consumed it).
    if result.is_err() {
        let _ = std::fs::remove_file(&part);
    }
    result
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

// Layout of a provisioned runtime store (`runtimes/<version>/`), shared by the
// code that writes it (provision_runtime), copies it (stage_runtime), and
// consumes it (main.rs). One place to change if the store layout ever moves.

/// The prebuilt morloc compiler within a provisioned runtime store.
pub fn runtime_morloc_bin(dir: &Path) -> PathBuf {
    dir.join("morloc")
}

/// The in-environment dependency agent within a provisioned runtime store. It
/// sits next to the compiler so the compiler finds it as a sibling (and on
/// PATH) for the dependency callback.
pub fn runtime_morloc_env_bin(dir: &Path) -> PathBuf {
    dir.join("morloc-env")
}

/// The Rust workspace source within a provisioned runtime store; this is
/// `morloc init`'s MORLOC_RUST_DIR.
pub fn runtime_rust_src(dir: &Path) -> PathBuf {
    dir.join("rust")
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
/// directory (its `rust/` subdir is `morloc init`'s MORLOC_RUST_DIR). Downloads
/// the version-matched morloc compiler plus the rust sources; `morloc init`
/// builds libmorloc.so, morloc-nexus, and rustmorloc locally from that source.
/// The already-installed `morloc-manager` bootstrap is not re-fetched.
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
    eprintln!("Provisioning morloc runtime from release {tag}...");
    let manifest = fetch_manifest(&tag)?;
    let version = manifest.version.clone();
    let assets = manifest.assets_for(triple).ok_or_else(|| {
        ManagerError::BackendUnsupported(format!(
            "release {tag} publishes no native runtime for {triple}; use a container backend"
        ))
    })?;

    let dir = runtimes_dir(scope, &version);
    // A completion stamp -- written only after every artifact is downloaded,
    // verified, and extracted -- is the idempotency gate. Gating on mere file
    // presence would bless a truncated or half-extracted runtime from an
    // interrupted prior run; the stamp is absent until provisioning fully
    // succeeds, so a crash always re-provisions.
    let stamp = dir.join(".provisioned");
    if !stamp.exists() {
        std::fs::create_dir_all(&dir)
            .map_err(|e| ManagerError::EnvError(format!("cannot create {}: {e}", dir.display())))?;
        // Ship only the prebuilt morloc COMPILER (it never links into a pool) and
        // the Rust SOURCE. libmorloc.so + morloc-nexus are NOT prebuilt: they are
        // built from this source by `morloc init` with the env's own toolchain,
        // so the runtime that pools link/load is ABI-coherent with the toolchain
        // that compiles those pools.
        let morloc_dest = runtime_morloc_bin(&dir);
        download_asset(&tag, &assets.morloc, &morloc_dest, &manifest.sha256)?;
        make_executable(&morloc_dest)?;

        // Wipe any stale rust/ from an interrupted prior extraction so a
        // re-provision never mixes old and new files.
        let src_tar = dir.join(&manifest.rust_src);
        download_asset(&tag, &manifest.rust_src, &src_tar, &manifest.sha256)?;
        let _ = std::fs::remove_dir_all(runtime_rust_src(&dir));
        extract_tar_gz(&src_tar, &dir)?;
        let _ = std::fs::remove_file(&src_tar);

        std::fs::write(&stamp, &version).map_err(|e| {
            ManagerError::EnvError(format!("cannot write {}: {e}", stamp.display()))
        })?;
    }

    // The lang-support table lets the manager render pixi manifests without
    // running the compiler on the host (essential where the glibc compiler can't
    // execute, e.g. NixOS/musl). Best-effort: older releases lack this asset.
    let ls = dir.join(LANG_SUPPORT_FILE);
    if !ls.exists() {
        // Best-effort (older releases lack this asset), but still atomic +
        // checksum-verified when the manifest publishes a digest for it.
        let _ = download_asset(&tag, LANG_SUPPORT_ASSET, &ls, &manifest.sha256);
    }

    Ok((dir, version))
}

// ======================================================================
// pixi provisioning (self-contained conda solver; no host install)
// ======================================================================

/// pixi's release target-triple for an (os, arch) pair. pixi ships static musl
/// on Linux, so one binary runs on any glibc/musl/NixOS host.
fn pixi_triple(os: &str, arch: &str) -> Option<&'static str> {
    match (os, arch) {
        ("linux", "x86_64") => Some("x86_64-unknown-linux-musl"),
        ("linux", "aarch64") => Some("aarch64-unknown-linux-musl"),
        ("macos", "aarch64") => Some("aarch64-apple-darwin"),
        ("macos", "x86_64") => Some("x86_64-apple-darwin"),
        _ => None,
    }
}

/// Ensure a pinned pixi binary is available and return its path. `$MORLOC_PIXI`
/// overrides (dev/testing); otherwise a per-user copy is downloaded once into
/// `<data_dir>/bin/pixi` and reused. This makes the conda solve self-contained --
/// the host does not need pixi installed.
pub fn provision_pixi(scope: Scope) -> Result<PathBuf> {
    if let Ok(p) = std::env::var("MORLOC_PIXI") {
        if !p.is_empty() {
            return Ok(PathBuf::from(p));
        }
    }
    let bin_dir = config::data_dir(scope).join("bin");
    let dest = bin_dir.join("pixi");
    if dest.is_file() {
        return Ok(dest);
    }
    let triple = pixi_triple(std::env::consts::OS, std::env::consts::ARCH).ok_or_else(|| {
        ManagerError::BackendUnsupported(format!(
            "no pinned pixi is available for this platform ({}/{})",
            std::env::consts::OS,
            std::env::consts::ARCH
        ))
    })?;
    std::fs::create_dir_all(&bin_dir)
        .map_err(|e| ManagerError::EnvError(format!("cannot create {}: {e}", bin_dir.display())))?;
    eprintln!("Provisioning pixi {PIXI_VERSION}...");
    // pixi ships as a .tar.gz that unpacks to a bare `pixi` binary. Download and
    // unpack in a staging dir, then atomically rename the finished binary into
    // place, so an interrupted fetch/extract never leaves a truncated `pixi` that
    // the `dest.is_file()` gate above would treat as ready.
    let url = format!(
        "https://github.com/prefix-dev/pixi/releases/download/v{PIXI_VERSION}/pixi-{triple}.tar.gz"
    );
    let stage = bin_dir.join(".pixi-stage");
    let _ = std::fs::remove_dir_all(&stage);
    let result = (|| {
        std::fs::create_dir_all(&stage).map_err(|e| {
            ManagerError::EnvError(format!("cannot create {}: {e}", stage.display()))
        })?;
        let tar = stage.join("pixi.tar.gz");
        curl_download(&url, &tar)?;
        extract_tar_gz(&tar, &stage)?;
        let extracted = stage.join("pixi");
        if !extracted.is_file() {
            return Err(ManagerError::EnvError(format!(
                "pixi binary not found at {} after extraction",
                extracted.display()
            )));
        }
        make_executable(&extracted)?;
        std::fs::rename(&extracted, &dest).map_err(|e| {
            ManagerError::EnvError(format!("cannot finalize {}: {e}", dest.display()))
        })
    })();
    // Drop the staging dir on every path (success or any early `?` failure).
    let _ = std::fs::remove_dir_all(&stage);
    result.map(|()| dest)
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
/// `skip` (e.g. `target` build artifacts). Real directories are recursed; a
/// symlink to a file is followed and copied as content, but a symlink to a
/// directory is NOT recursed into -- following it could form a cycle (infinite
/// recursion) or escape the source tree (e.g. a dotfiles dir that symlinks to
/// `$HOME`). Existing files are overwritten (`fs::copy` semantics).
pub fn copy_dir_excluding(src: &Path, dst: &Path, skip: &[&str]) -> Result<()> {
    std::fs::create_dir_all(dst)
        .map_err(|e| ManagerError::EnvError(format!("cannot create {}: {e}", dst.display())))?;
    let entries = std::fs::read_dir(src)
        .map_err(|e| ManagerError::EnvError(format!("cannot read {}: {e}", src.display())))?;
    for entry in entries.flatten() {
        let from = entry.path();
        let name = entry.file_name();
        let to = dst.join(&name);
        // file_type() does NOT follow the link, so we can tell a symlink apart
        // from a real directory before deciding whether to recurse.
        let is_symlink = entry
            .file_type()
            .map(|t| t.is_symlink())
            .unwrap_or(false);
        if is_symlink {
            // Skip a symlinked directory (cycle/escape hazard); copy a symlinked
            // file as its target content (fs::copy follows the link).
            if from.is_dir() {
                continue;
            }
            copy_file(&from, &to)?;
        } else if from.is_dir() {
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
/// into a container build context's `runtime/` directory: the morloc compiler
/// plus the Rust workspace source. libmorloc.so + morloc-nexus are built from
/// that source in-image by `morloc init`, so they are not staged. Everything
/// comes from the downloaded runtime -- no host morloc install is required.
pub fn stage_runtime(src: &Path, dest: &Path) -> Result<()> {
    std::fs::create_dir_all(dest)
        .map_err(|e| ManagerError::EnvError(format!("cannot create {}: {e}", dest.display())))?;
    let from = runtime_morloc_bin(src);
    if !from.exists() {
        return Err(ManagerError::EnvError(format!(
            "provisioned runtime is missing morloc at {}",
            from.display()
        )));
    }
    let morloc_dest = runtime_morloc_bin(dest);
    copy_file(&from, &morloc_dest)?;
    make_executable(&morloc_dest)?;
    // Stage the in-env dependency agent alongside the compiler when present, so
    // the in-container compiler finds it (sibling on PATH) for the dep callback.
    // (`copy_file` follows a symlink, so a dev-staged symlink copies the real
    // binary into the image.)
    let agent = runtime_morloc_env_bin(src);
    if agent.is_file() {
        let agent_dest = runtime_morloc_env_bin(dest);
        copy_file(&agent, &agent_dest)?;
        make_executable(&agent_dest)?;
    }
    let rust = runtime_rust_src(src);
    if !rust.is_dir() {
        return Err(ManagerError::EnvError(format!(
            "provisioned runtime is missing the rust source at {}",
            rust.display()
        )));
    }
    copy_dir_excluding(&rust, &runtime_rust_src(dest), &["target"])?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = r#"{
      "schema": 2,
      "version": "0.98.3",
      "rust_src": "morloc-rust-src.tar.gz",
      "triples": {
        "linux-x86_64": {
          "morloc": "morloc-linux-x86_64",
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
        assert_eq!(a.morloc, "morloc-linux-x86_64");
        assert_eq!(a.manager, "morloc-manager-linux-x86_64");
        // A triple with no published runtime is absent.
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

    #[test]
    fn sha256_matches_known_vectors() {
        let dir = std::env::temp_dir().join(format!("morloc-sha-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let empty = dir.join("empty");
        std::fs::write(&empty, b"").unwrap();
        assert_eq!(
            file_sha256(&empty).unwrap(),
            "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
        );
        let abc = dir.join("abc");
        std::fs::write(&abc, b"abc").unwrap();
        assert_eq!(
            file_sha256(&abc).unwrap(),
            "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
        );
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn parses_tag_from_latest_redirect() {
        assert_eq!(
            parse_tag_from_release_url(
                "https://github.com/morloc-project/morloc/releases/tag/v0.99.0"
            )
            .as_deref(),
            Some("v0.99.0")
        );
        // A trailing slash on the redirect target is tolerated.
        assert_eq!(
            parse_tag_from_release_url(
                "https://github.com/morloc-project/morloc/releases/tag/dev/"
            )
            .as_deref(),
            Some("dev")
        );
        // A URL that never reached a /tag/ page (e.g. no releases) yields None.
        assert_eq!(
            parse_tag_from_release_url("https://github.com/morloc-project/morloc/releases"),
            None
        );
    }

    #[test]
    fn manifest_parses_sha256_and_defaults_empty() {
        let j = r#"{"schema":1,"version":"1.0.0","rust_src":"r","triples":{},
          "sha256":{"morloc-linux-x86_64":"abc123"}}"#;
        let m = ReleaseManifest::from_json(j).unwrap();
        assert_eq!(
            m.sha256.get("morloc-linux-x86_64").map(|s| s.as_str()),
            Some("abc123")
        );
        // A manifest predating digest publishing parses with an empty map.
        assert!(ReleaseManifest::from_json(SAMPLE).unwrap().sha256.is_empty());
    }
}
