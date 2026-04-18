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
}

impl Serialize for ContainerEngine {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            ContainerEngine::Docker => serializer.serialize_str("docker"),
            ContainerEngine::Podman => serializer.serialize_str("podman"),
        }
    }
}

impl<'de> Deserialize<'de> for ContainerEngine {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let s = String::deserialize(deserializer)?;
        match s.as_str() {
            "docker" => Ok(ContainerEngine::Docker),
            "podman" => Ok(ContainerEngine::Podman),
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
    /// Base container image reference.
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
    /// Container engine for this environment.
    pub engine: ContainerEngine,
    /// Shared memory size for container runs.
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
    /// Returns the image to use for running containers.
    /// Prefers the built Dockerfile layer image, falls back to base_image.
    pub fn active_image(&self) -> &str {
        self.built_image.as_deref().unwrap_or(&self.base_image)
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
    /// Expected environment variable names (no values — injected at start/run time).
    #[serde(default)]
    pub env_vars: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FrozenEnvLayer {
    pub name: String,
    pub dockerfile: String,
    pub content_hash: String,
    pub image_digest: Option<String>,
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
