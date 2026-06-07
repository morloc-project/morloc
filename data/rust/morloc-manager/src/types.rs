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

impl Serialize for ContainerEngine {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            ContainerEngine::Docker => serializer.serialize_str("docker"),
            ContainerEngine::Podman => serializer.serialize_str("podman"),
            ContainerEngine::Apptainer => serializer.serialize_str("apptainer"),
        }
    }
}

impl<'de> Deserialize<'de> for ContainerEngine {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let s = String::deserialize(deserializer)?;
        match s.as_str() {
            "docker" => Ok(ContainerEngine::Docker),
            "podman" => Ok(ContainerEngine::Podman),
            "apptainer" | "singularity" => Ok(ContainerEngine::Apptainer),
            _ => Err(serde::de::Error::custom(format!(
                "Unknown container engine: {s}"
            ))),
        }
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
    /// Default container engine.
    #[serde(default = "default_engine")]
    pub engine: ContainerEngine,
}

fn default_engine() -> ContainerEngine {
    ContainerEngine::Podman
}

impl Default for Config {
    fn default() -> Self {
        Self {
            active_env: None,
            engine: ContainerEngine::Podman,
        }
    }
}

// ======================================================================
// Environment configuration
// ======================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnvironmentConfig {
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
    /// Container engine for this environment.
    pub engine: ContainerEngine,
    /// Shared memory size for container runs. Honored under docker/podman;
    /// ignored under apptainer (host /dev/shm is shared transparently).
    #[serde(default = "default_shm_size")]
    pub shm_size: String,
    /// Morloc version this environment was created from.
    #[serde(default)]
    pub morloc_version: Option<Version>,
}

fn default_shm_size() -> String {
    "512m".to_string()
}

impl EnvironmentConfig {
    /// Returns the image reference to use for running containers.
    ///
    /// For docker/podman this is the built layer tag (preferred) or the base
    /// OCI URI. For apptainer this is the local .sif path (layered preferred,
    /// base as fallback). The returned string is opaque to the caller; pass
    /// it as `RunConfig::image` and the engine dispatch will route correctly.
    pub fn active_image(&self) -> &str {
        match self.engine {
            ContainerEngine::Docker | ContainerEngine::Podman => self
                .built_image
                .as_deref()
                .unwrap_or(&self.base_image),
            ContainerEngine::Apptainer => self
                .layered_sif
                .as_deref()
                .or(self.base_sif.as_deref())
                .unwrap_or(&self.base_image),
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
    Build,
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
            Phase::Build => &self.build,
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
