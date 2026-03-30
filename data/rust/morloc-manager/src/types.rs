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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Version {
    pub major: u32,
    pub minor: u32,
    pub patch: u32,
}

impl Version {
    pub fn new(major: u32, minor: u32, patch: u32) -> Self {
        Self {
            major,
            minor,
            patch,
        }
    }

    pub fn show(&self) -> String {
        format!("{}.{}.{}", self.major, self.minor, self.patch)
    }
}

impl Ord for Version {
    fn cmp(&self, other: &Self) -> Ordering {
        self.major
            .cmp(&other.major)
            .then(self.minor.cmp(&other.minor))
            .then(self.patch.cmp(&other.patch))
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
        let parts: Vec<&str> = s.split('.').collect();
        if parts.len() != 3 {
            return Err(format!("Invalid version: {s}"));
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
        Ok(Version::new(major, minor, patch))
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
// Active target
// ======================================================================

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type", content = "value")]
pub enum ActiveTarget {
    #[serde(rename = "version")]
    Version(Version),
    #[serde(rename = "workspace")]
    Workspace(String),
}

// ======================================================================
// Configuration
// ======================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    pub active_target: Option<ActiveTarget>,
    #[serde(default = "default_scope")]
    pub active_scope: Scope,
    #[serde(default = "default_env")]
    pub active_env: String,
    #[serde(default = "default_engine")]
    pub engine: ContainerEngine,
}

fn default_scope() -> Scope {
    Scope::Local
}
fn default_env() -> String {
    "base".to_string()
}
fn default_engine() -> ContainerEngine {
    ContainerEngine::Podman
}

impl Default for Config {
    fn default() -> Self {
        Self {
            active_target: None,
            active_scope: Scope::Local,
            active_env: "base".to_string(),
            engine: ContainerEngine::Podman,
        }
    }
}

impl Config {
    pub fn active_version(&self) -> Option<Version> {
        match &self.active_target {
            Some(ActiveTarget::Version(v)) => Some(*v),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VersionConfig {
    pub image: String,
    pub host_dir: String,
    #[serde(default = "default_shm_size")]
    pub shm_size: String,
    #[serde(default = "default_engine")]
    pub engine: ContainerEngine,
}

fn default_shm_size() -> String {
    "512m".to_string()
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkspaceConfig {
    pub base_version: Version,
    #[serde(default = "default_scope")]
    pub base_scope: Scope,
    #[serde(default = "default_engine")]
    pub engine: ContainerEngine,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnvironmentConfig {
    pub image: String,
    pub dockerfile: String,
    pub content_hash: Option<String>,
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
