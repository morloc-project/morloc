use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::cmp::Ordering;
use std::str::FromStr;

// ======================================================================
// Core enumerations
// ======================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Scope {
    Local,
    System,
}

impl Serialize for Scope {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            Scope::Local => serializer.serialize_str("local"),
            Scope::System => serializer.serialize_str("system"),
        }
    }
}

impl<'de> Deserialize<'de> for Scope {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let s = String::deserialize(deserializer)?;
        match s.as_str() {
            "local" => Ok(Scope::Local),
            "system" => Ok(Scope::System),
            _ => Err(serde::de::Error::custom(format!("Unknown scope: {s}"))),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ContainerEngine {
    Docker,
    Podman,
    /// Apptainer or Singularity. The two are treated as a single engine; the
    /// runtime executable is detected at dispatch time.
    Apptainer,
}

impl ContainerEngine {
    /// Canonical lowercase name; also the serialized form and the display name.
    pub fn name(&self) -> &'static str {
        match self {
            ContainerEngine::Docker => "docker",
            ContainerEngine::Podman => "podman",
            ContainerEngine::Apptainer => "apptainer",
        }
    }

    /// Parse an engine token. "singularity" is an accepted alias for apptainer.
    pub fn from_token(s: &str) -> Option<Self> {
        match s {
            "docker" => Some(ContainerEngine::Docker),
            "podman" => Some(ContainerEngine::Podman),
            "apptainer" | "singularity" => Some(ContainerEngine::Apptainer),
            _ => None,
        }
    }
}

impl Serialize for ContainerEngine {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(self.name())
    }
}

impl<'de> Deserialize<'de> for ContainerEngine {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let s = String::deserialize(deserializer)?;
        ContainerEngine::from_token(&s)
            .ok_or_else(|| serde::de::Error::custom(format!("Unknown container engine: {s}")))
    }
}

/// Execution backend for an environment: how morloc processes are run.
///
/// `Container(engine)` runs pools inside a container under the given engine;
/// `Native` runs them directly on the host against a pixi-provided toolchain.
/// `Backend` is serialized as a bare string compatible with the historical
/// `engine:` field (a container engine name, or "native"), so existing
/// env.yaml / config.json files load unchanged.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Backend {
    Container(ContainerEngine),
    Native,
}

impl Backend {
    /// The container engine, if this is a container backend; `None` for native.
    pub fn container_engine(&self) -> Option<ContainerEngine> {
        match self {
            Backend::Container(e) => Some(*e),
            Backend::Native => None,
        }
    }

    /// True for the native (no-container) backend.
    // Used by the command-dispatch native branch, wired in the next P2b step.
    #[allow(dead_code)]
    pub fn is_native(&self) -> bool {
        matches!(self, Backend::Native)
    }

    /// Short human-readable name for display.
    pub fn label(&self) -> &'static str {
        match self.container_engine() {
            Some(e) => e.name(),
            None => "native",
        }
    }
}

impl Serialize for Backend {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            Backend::Container(e) => e.serialize(serializer),
            Backend::Native => serializer.serialize_str("native"),
        }
    }
}

impl<'de> Deserialize<'de> for Backend {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        // Accept "native" or a historical bare engine string ("docker"/"podman"/
        // "apptainer"/"singularity"); the engine tokens delegate to ContainerEngine.
        let s = String::deserialize(deserializer)?;
        if s == "native" {
            return Ok(Backend::Native);
        }
        ContainerEngine::from_token(&s)
            .map(Backend::Container)
            .ok_or_else(|| serde::de::Error::custom(format!("Unknown backend/engine: {s}")))
    }
}

// ======================================================================
// Version
// ======================================================================

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Version {
    pub major: u32,
    pub minor: u32,
    pub patch: u32,
    pub prerelease: Option<String>,
}

impl Version {
    #[cfg(test)]
    pub fn new(major: u32, minor: u32, patch: u32) -> Self {
        Self {
            major,
            minor,
            patch,
            prerelease: None,
        }
    }

    pub fn show(&self) -> String {
        match &self.prerelease {
            Some(pre) => format!("{}.{}.{}-{}", self.major, self.minor, self.patch, pre),
            None => format!("{}.{}.{}", self.major, self.minor, self.patch),
        }
    }
}

impl Ord for Version {
    fn cmp(&self, other: &Self) -> Ordering {
        self.major
            .cmp(&other.major)
            .then(self.minor.cmp(&other.minor))
            .then(self.patch.cmp(&other.patch))
            .then(match (&self.prerelease, &other.prerelease) {
                (None, None) => Ordering::Equal,
                (Some(_), None) => Ordering::Less,    // pre-release < release
                (None, Some(_)) => Ordering::Greater,  // release > pre-release
                (Some(a), Some(b)) => a.cmp(b),
            })
    }
}

impl PartialOrd for Version {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl FromStr for Version {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Split off pre-release suffix on first '-': "0.77.0-rc.1" -> ("0.77.0", Some("rc.1"))
        let (version_part, prerelease) = match s.find('-') {
            Some(idx) => (&s[..idx], Some(s[idx + 1..].to_string())),
            None => (s, None),
        };
        let parts: Vec<&str> = version_part.split('.').collect();
        if parts.len() != 3 {
            return Err(format!("Invalid version: {s}. Expected format: MAJOR.MINOR.PATCH[-PRERELEASE]"));
        }
        let major = parts[0]
            .parse()
            .map_err(|_| format!("Invalid major version: {}", parts[0]))?;
        let minor = parts[1]
            .parse()
            .map_err(|_| format!("Invalid minor version: {}", parts[1]))?;
        let patch = parts[2]
            .parse()
            .map_err(|_| format!("Invalid patch version: {}", parts[2]))?;
        Ok(Version {
            major,
            minor,
            patch,
            prerelease,
        })
    }
}

impl Serialize for Version {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(&self.show())
    }
}

impl<'de> Deserialize<'de> for Version {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let s = String::deserialize(deserializer)?;
        s.parse().map_err(serde::de::Error::custom)
    }
}

// ======================================================================
// Configuration
// ======================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    /// Name of the active environment.
    pub active_env: Option<String>,
    /// Default execution backend (serialized under the historical `engine` key).
    #[serde(rename = "engine", default = "default_backend")]
    pub backend: Backend,
}

fn default_backend() -> Backend {
    Backend::Container(ContainerEngine::Podman)
}

impl Config {
    /// The container engine of the default backend, or an error if it is native.
    pub fn engine(&self) -> crate::error::Result<ContainerEngine> {
        self.backend.container_engine().ok_or_else(|| {
            crate::error::ManagerError::BackendUnsupported(
                "the default backend is native; this operation requires a container engine"
                    .to_string(),
            )
        })
    }
}

impl Default for Config {
    fn default() -> Self {
        Self {
            active_env: None,
            backend: Backend::Container(ContainerEngine::Podman),
        }
    }
}

// ======================================================================
// Environment configuration
// ======================================================================

/// Current on-disk schema version for `env.yaml`. Bump when the field set
/// changes incompatibly and add a matching arm to `migrate_env_config`.
pub const CURRENT_ENV_SCHEMA: u32 = 1;

/// Records written before schema versioning existed carry no `schema_version`
/// field; they are the v1 baseline.
fn default_env_schema() -> u32 {
    1
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnvironmentConfig {
    /// On-disk schema version. Read-time validation rejects a version newer
    /// than `CURRENT_ENV_SCHEMA` and migrates anything older forward.
    #[serde(default = "default_env_schema")]
    pub schema_version: u32,
    /// Human-readable name (also the directory name).
    pub name: String,
    /// Base container image reference (OCI URI; same field for all engines).
    pub base_image: String,
    /// Original pullable image reference (e.g., :edge tag) before local re-tagging.
    #[serde(default)]
    pub original_image: Option<String>,
    /// Filename of the custom Dockerfile layer (within the env config dir).
    #[serde(default)]
    pub dockerfile: Option<String>,
    /// SHA256 hash of the Dockerfile content (for rebuild detection).
    #[serde(default)]
    pub content_hash: Option<String>,
    /// Built image tag after applying the Dockerfile layer.
    /// None when only the base image is used.
    #[serde(default)]
    pub built_image: Option<String>,
    /// Filename of the Singularity definition file (within the env config dir).
    /// Apptainer-engine equivalent of `dockerfile`. Both may be present in the
    /// same env; on Apptainer the .def is preferred.
    #[serde(default)]
    pub singularity_def: Option<String>,
    /// SHA256 hash of the .def content (for rebuild detection under Apptainer).
    #[serde(default)]
    pub def_content_hash: Option<String>,
    /// Path to the cached base .sif (Apptainer engine only).
    #[serde(default)]
    pub base_sif: Option<String>,
    /// Path to the built layered .sif from `update` (Apptainer engine only).
    /// Apptainer-engine equivalent of `built_image`.
    #[serde(default)]
    pub layered_sif: Option<String>,
    /// Execution backend for this environment (serialized under `engine`).
    #[serde(rename = "engine")]
    pub backend: Backend,
    /// Shared memory size for container runs. Honored under docker/podman;
    /// ignored under apptainer (host /dev/shm is shared transparently).
    #[serde(default = "default_shm_size")]
    pub shm_size: String,
    /// Morloc version this environment was created from.
    #[serde(default)]
    pub morloc_version: Option<Version>,
    /// Extra OS packages baked into the image via the base package manager
    /// (apt), for tools conda cannot provide (e.g. jq). Container-only; the
    /// native backend rejects them at creation.
    #[serde(default)]
    pub system_packages: Vec<String>,
}

fn default_shm_size() -> String {
    "512m".to_string()
}

impl EnvironmentConfig {
    /// Construct a fresh env record for a requirement-derived backend (native or
    /// container), leaving the legacy container-recipe fields (dockerfile, .sif,
    /// content hashes) at their inert defaults.
    pub fn new_backend(
        name: String,
        backend: Backend,
        base_image: String,
        built_image: Option<String>,
        morloc_version: Option<Version>,
        system_packages: Vec<String>,
    ) -> Self {
        EnvironmentConfig {
            schema_version: CURRENT_ENV_SCHEMA,
            name,
            base_image,
            original_image: None,
            dockerfile: None,
            content_hash: None,
            built_image,
            singularity_def: None,
            def_content_hash: None,
            base_sif: None,
            layered_sif: None,
            backend,
            shm_size: default_shm_size(),
            morloc_version,
            system_packages,
        }
    }

    /// The container engine of this environment's backend, or an error if the
    /// environment is native (a caller reaching here is on a container-only
    /// code path that does not apply to native environments).
    pub fn engine(&self) -> crate::error::Result<ContainerEngine> {
        self.backend.container_engine().ok_or_else(|| {
            crate::error::ManagerError::BackendUnsupported(format!(
                "environment '{}' uses the native backend; this operation requires a container",
                self.name
            ))
        })
    }

    /// Returns the image reference to use for running containers.
    ///
    /// For docker/podman this is the built layer tag (preferred) or the base
    /// OCI URI. For apptainer this is the local .sif path (layered preferred,
    /// base as fallback). Native environments have no image; the base image is
    /// returned as an inert default (container run paths are not taken there).
    pub fn active_image(&self) -> &str {
        match self.backend.container_engine() {
            Some(ContainerEngine::Docker) | Some(ContainerEngine::Podman) => self
                .built_image
                .as_deref()
                .unwrap_or(&self.base_image),
            Some(ContainerEngine::Apptainer) => self
                .layered_sif
                .as_deref()
                .or(self.base_sif.as_deref())
                .unwrap_or(&self.base_image),
            None => &self.base_image,
        }
    }
}

// ======================================================================
// Container flag configuration
// ======================================================================

/// Phase of container invocation a flag list applies to. Names match the
/// CLI subcommand that triggers the phase, so users can grep the same
/// word in their notes and find both the section in env.flags.yaml and
/// the subcommand that consumes it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Phase {
    Run,
    Start,
}

/// Per-engine flag lists for a single phase. The `all` slot applies to
/// every engine; engine-specific slots apply only to that engine. At
/// materialize time the active engine's list is appended to `all`,
/// in that order. `deny_unknown_fields` catches typos like `aptainer`.
#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub struct EngineFlags {
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub all: Vec<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub docker: Vec<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub podman: Vec<String>,
    #[serde(default, alias = "singularity", skip_serializing_if = "Vec::is_empty")]
    pub apptainer: Vec<String>,
}

impl EngineFlags {
    pub fn for_engine(&self, engine: ContainerEngine) -> Vec<String> {
        let engine_slice = match engine {
            ContainerEngine::Docker => &self.docker,
            ContainerEngine::Podman => &self.podman,
            ContainerEngine::Apptainer => &self.apptainer,
        };
        let mut out = self.all.clone();
        out.extend(engine_slice.iter().cloned());
        out
    }
}

fn is_empty_engine_flags(ef: &EngineFlags) -> bool {
    ef.all.is_empty()
        && ef.docker.is_empty()
        && ef.podman.is_empty()
        && ef.apptainer.is_empty()
}

/// The full env.flags.yaml schema. All sections optional; missing or
/// empty sections materialize to empty lists. Strict schema: unknown
/// top-level keys are a parse error so a typo like `runn:` is caught
/// instead of silently ignored.
#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub struct FlagConfig {
    #[serde(default, skip_serializing_if = "is_empty_engine_flags")]
    pub build: EngineFlags,
    #[serde(default, skip_serializing_if = "is_empty_engine_flags")]
    pub run: EngineFlags,
    #[serde(default, skip_serializing_if = "is_empty_engine_flags")]
    pub start: EngineFlags,
}

impl FlagConfig {
    /// Returns the materialized flag list for a (phase, engine) pair:
    /// `phase.all ++ phase.engine`. Pure operation; CLI one-shot
    /// overrides are appended by the caller, not here.
    pub fn materialize(&self, phase: Phase, engine: ContainerEngine) -> Vec<String> {
        let section = match phase {
            Phase::Run => &self.run,
            Phase::Start => &self.start,
        };
        section.for_engine(engine)
    }
}

// ======================================================================
// Freeze manifest
// ======================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FreezeManifest {
    pub morloc_version: Version,
    pub frozen_at: chrono::DateTime<chrono::Utc>,
    pub modules: Vec<ModuleEntry>,
    pub programs: Vec<ProgramEntry>,
    pub base_image: String,
    pub env_layer: Option<FrozenEnvLayer>,
    /// Deprecated: previously held expected env var names. Retained for backward
    /// compatibility when reading older freeze manifests.
    #[serde(default, skip_serializing)]
    #[allow(dead_code)]
    pub env_vars: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FrozenEnvLayer {
    pub name: String,
    pub dockerfile: String,
    pub content_hash: String,
    /// Container image tag (e.g. localhost/morloc-env:0.79.2-dnd).
    /// Named image_tag because it stores a mutable tag, not a content-addressed digest.
    #[serde(alias = "image_digest")]
    pub image_tag: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModuleEntry {
    pub name: String,
    pub version: Option<String>,
    pub sha256: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProgramEntry {
    pub name: String,
    pub commands: Vec<String>,
}

// ======================================================================
// Module exposure (which installed modules are served, and how)
// ======================================================================

/// A served view of a module. CLI is the LOCAL view (`morloc run`), not a
/// network protocol, so it is not part of exposure.
#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
pub enum Protocol {
    /// Model Context Protocol (tools for an AI assistant).
    Mcp,
    /// HTTP JSON API (`/call/<module>/<command>`).
    Api,
}

impl Protocol {
    pub fn as_str(self) -> &'static str {
        match self {
            Protocol::Mcp => "mcp",
            Protocol::Api => "api",
        }
    }
}

/// The eval capability on a served environment. Present only when eval is
/// exposed; always sandboxed. Its allow-list is INDEPENDENT of the exposed
/// module sets (eval composes generic code a fixed tool surface cannot express,
/// and may need to import modules that are not themselves exposed as tools).
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct EvalExposure {
    /// Modules eval may import (sandbox allow-list). Empty = eval can import
    /// nothing (harmless but useless).
    #[serde(default)]
    pub allow: Vec<String>,
}

/// Declarative record of what an environment exposes and how. Lives at
/// `<config>/environments/<name>/expose.yaml`, edited by `expose` and realized
/// by `start`. Separate from `EnvironmentConfig` (build/image config): install
/// makes a module importable; expose declares which modules are served, over
/// which protocol.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExposureConfig {
    /// Modules exposed over MCP (each contributes `<module>__<command>` tools).
    #[serde(default)]
    pub mcp: Vec<String>,
    /// Modules exposed over the HTTP JSON API.
    #[serde(default)]
    pub api: Vec<String>,
    /// The eval capability. `None` = off (the default).
    #[serde(default)]
    pub eval: Option<EvalExposure>,
}

impl ExposureConfig {
    pub fn is_empty(&self) -> bool {
        self.mcp.is_empty() && self.api.is_empty() && self.eval.is_none()
    }

    fn set_mut(&mut self, p: Protocol) -> &mut Vec<String> {
        match p {
            Protocol::Mcp => &mut self.mcp,
            Protocol::Api => &mut self.api,
        }
    }

    /// Add `module` to each given protocol set (idempotent; kept sorted-unique).
    pub fn add(&mut self, module: &str, protocols: &[Protocol]) {
        for &p in protocols {
            let set = self.set_mut(p);
            if !set.iter().any(|m| m == module) {
                set.push(module.to_string());
                set.sort();
            }
        }
    }

    /// Remove `module` from all protocol sets. Returns true if anything changed.
    pub fn remove(&mut self, module: &str) -> bool {
        let before = self.mcp.len() + self.api.len();
        self.mcp.retain(|m| m != module);
        self.api.retain(|m| m != module);
        before != self.mcp.len() + self.api.len()
    }

    /// The protocols a given module is exposed over (for display).
    pub fn protocols_of(&self, module: &str) -> Vec<Protocol> {
        let mut ps = Vec::new();
        if self.mcp.iter().any(|m| m == module) {
            ps.push(Protocol::Mcp);
        }
        if self.api.iter().any(|m| m == module) {
            ps.push(Protocol::Api);
        }
        ps
    }

    /// Every module exposed over any protocol (deduped, sorted).
    pub fn exposed_modules(&self) -> Vec<String> {
        sorted_union(&self.mcp, &self.api)
    }
}

/// Sorted, deduped union of two module lists. Shared by `ExposureConfig` and
/// `ServeRuntime` so "the set of modules across both adapters" has one spelling.
fn sorted_union(a: &[String], b: &[String]) -> Vec<String> {
    let mut all: Vec<String> = a.iter().chain(b.iter()).cloned().collect();
    all.sort();
    all.dedup();
    all
}

/// Runtime record of what a serve container is ACTUALLY serving, written by
/// `start` and read by `status`. Distinct from `ExposureConfig` (declared
/// intent): this reflects the live invocation, including the resolved host/port
/// and whether a token is required.
/// Host-side materialization record for a native (no-container) environment.
/// Written when the environment's toolchain is provisioned; read by the native
/// Runner to reconstruct the toolchain environment before spawning a command.
/// `activation_env` is the captured env-map (PATH, compiler vars, etc.) that
/// the provisioned toolchain requires. Additional fields may accrue as the
/// native backend gains features; readers must tolerate their absence.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct NativeRuntime {
    #[serde(default)]
    pub activation_env: Vec<(String, String)>,
}

/// Persisted requirement inputs for an environment (native or container), kept
/// so that a later `update` re-materializes the same toolchain request. Program
/// requirements are re-gathered from installed `envspec.json` files; only the
/// user's `--lang` pins (which have no other on-disk home) are stored here.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct EnvInputs {
    /// `--lang` pins as (language, optional version pin), e.g. ("py", Some("3.12")).
    #[serde(default)]
    pub lang_pins: Vec<(String, Option<String>)>,
}

/// How a running serve was launched -- the authority for stop/status/logs, which
/// dispatch on this stored handle rather than the environment's current backend
/// (so migrating an env or serving it two ways never strands a server).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "backend", rename_all = "lowercase")]
pub enum ServeHandle {
    /// A container/apptainer instance, torn down by engine + name.
    Container { engine: ContainerEngine, name: String },
    /// A detached host process group (native backend), torn down by killing pgid.
    Native { pid: u32, pgid: u32 },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ServeRuntime {
    #[serde(default)]
    pub mcp: Vec<String>,
    #[serde(default)]
    pub api: Vec<String>,
    #[serde(default)]
    pub eval: bool,
    pub host: String,
    pub port: u16,
    #[serde(default)]
    pub token_required: bool,
    /// The launch handle. `Option` for backward compatibility with serve records
    /// written before this field existed (an in-flight serve during upgrade);
    /// when absent, stop/status/logs fall back to deriving the target from the
    /// environment's backend.
    #[serde(default)]
    pub handle: Option<ServeHandle>,
}

impl ServeRuntime {
    /// Short adapter/mode label for display, e.g. "mcp+api", "mcp", "mcp+eval".
    pub fn mode(&self) -> String {
        let base = match (!self.mcp.is_empty(), !self.api.is_empty()) {
            (true, true) => "mcp+api",
            (true, false) => "mcp",
            (false, true) => "api",
            (false, false) => "-",
        };
        if self.eval { format!("{base}+eval") } else { base.to_string() }
    }

    /// Short module summary for display (sorted, deduped).
    pub fn modules_summary(&self) -> String {
        let mods = sorted_union(&self.mcp, &self.api);
        if mods.is_empty() { "-".to_string() } else { mods.join(",") }
    }

    pub fn url(&self) -> String {
        format!("http://{}:{}", self.host, self.port)
    }
}

#[cfg(test)]
mod exposure_tests {
    use super::*;

    #[test]
    fn add_is_idempotent_and_per_protocol() {
        let mut ex = ExposureConfig::default();
        ex.add("dna", &[Protocol::Mcp]);
        ex.add("dna", &[Protocol::Mcp]); // idempotent
        ex.add("align", &[Protocol::Api]);
        ex.add("util", &[Protocol::Mcp, Protocol::Api]);
        assert_eq!(ex.mcp, vec!["dna".to_string(), "util".to_string()]);
        assert_eq!(ex.api, vec!["align".to_string(), "util".to_string()]);
    }

    #[test]
    fn remove_clears_all_sets() {
        let mut ex = ExposureConfig::default();
        ex.add("util", &[Protocol::Mcp, Protocol::Api]);
        assert!(ex.remove("util"));
        assert!(ex.mcp.is_empty() && ex.api.is_empty());
        assert!(!ex.remove("util")); // no-op second time
    }

    #[test]
    fn protocols_of_reports_both() {
        let mut ex = ExposureConfig::default();
        ex.add("util", &[Protocol::Mcp, Protocol::Api]);
        ex.add("dna", &[Protocol::Mcp]);
        assert_eq!(ex.protocols_of("util"), vec![Protocol::Mcp, Protocol::Api]);
        assert_eq!(ex.protocols_of("dna"), vec![Protocol::Mcp]);
        assert!(ex.protocols_of("missing").is_empty());
    }

    #[test]
    fn eval_is_independent_of_exposed_sets() {
        let mut ex = ExposureConfig::default();
        ex.add("dna", &[Protocol::Mcp]);
        ex.eval = Some(EvalExposure { allow: vec!["dna".into(), "stats".into()] });
        // eval may allow a module (stats) that is NOT in any exposed set.
        assert!(!ex.exposed_modules().contains(&"stats".to_string()));
        assert_eq!(ex.eval.as_ref().unwrap().allow, vec!["dna".to_string(), "stats".to_string()]);
    }

    #[test]
    fn serve_runtime_mode_and_modules() {
        let r = ServeRuntime {
            mcp: vec!["dna".into()],
            api: vec!["align".into(), "dna".into()],
            eval: true,
            host: "127.0.0.1".into(),
            port: 9000,
            token_required: false,
            handle: None,
        };
        assert_eq!(r.mode(), "mcp+api+eval");
        assert_eq!(r.modules_summary(), "align,dna");
        assert_eq!(r.url(), "http://127.0.0.1:9000");
        let empty = ServeRuntime {
            mcp: vec![], api: vec![], eval: false,
            host: "h".into(), port: 8080, token_required: true,
            handle: None,
        };
        assert_eq!(empty.mode(), "-");
        assert_eq!(empty.modules_summary(), "-");
    }

    #[test]
    fn yaml_round_trip() {
        let mut ex = ExposureConfig::default();
        ex.add("dna", &[Protocol::Mcp]);
        ex.add("align", &[Protocol::Api]);
        ex.eval = Some(EvalExposure { allow: vec!["dna".into()] });
        let yaml = serde_yaml::to_string(&ex).unwrap();
        let back: ExposureConfig = serde_yaml::from_str(&yaml).unwrap();
        assert_eq!(ex, back);
    }
}
