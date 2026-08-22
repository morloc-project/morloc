use std::fmt;
use thiserror::Error;

use crate::types::{ContainerEngine, Scope, Version};

#[derive(Debug, Error, PartialEq, Eq)]
pub enum ManagerError {
    #[error("Configuration not found: {0}")]
    ConfigNotFound(String),

    #[error("Permission denied: {0}")]
    ConfigPermissionDenied(String),

    #[error("Invalid configuration in {path}: {msg}")]
    ConfigParseError { path: String, msg: String },

    #[error("No default environment set. Run: morloc-manager new")]
    NoDefaultEnvironment,

    #[error("Environment not found: {0}")]
    EnvironmentNotFound(String),

    #[error("Environment error: {0}")]
    EnvError(String),

    #[error("{0}")]
    BackendUnsupported(String),


    #[error("No command specified. Provide a command after --, or use 'morloc-manager shell' for an interactive shell.")]
    NoCommand,

    #[error("No container engine found. Install podman or docker.")]
    EngineNotFound,

    #[error("Container engine ({engine}) failed with exit code {code}:\n{stderr}")]
    EngineError {
        engine: ContainerEngine,
        code: i32,
        stderr: String,
    },

    #[error("Freeze failed: {0}")]
    FreezeError(String),

    #[error("Unfreeze failed: {0}")]
    UnfreezeError(String),

    #[error("SELinux error: {0}")]
    SELinuxError(String),

    #[error("Doctor found {0} error(s)")]
    DoctorFailed(u32),

    #[error("{}", match .0 {
        Scope::Local => "No local configuration found. Run: morloc-manager new",
        Scope::System => "No system configuration found. Run: sudo morloc-manager new --system",
    })]
    SetupNotComplete(Scope),
}

impl fmt::Display for Version {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)
    }
}

impl fmt::Display for ContainerEngine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ContainerEngine::Docker => write!(f, "Docker"),
            ContainerEngine::Podman => write!(f, "Podman"),
            ContainerEngine::Apptainer => write!(f, "Apptainer"),
        }
    }
}

pub type Result<T> = std::result::Result<T, ManagerError>;

// The dependency kernel (`morloc-deps`) reports failures as `DepsError`; collapse
// them into the existing `EnvError` variant so `?` at kernel call sites converts
// transparently and existing `EnvError` match arms still catch them.
impl From<morloc_deps::error::DepsError> for ManagerError {
    fn from(e: morloc_deps::error::DepsError) -> Self {
        ManagerError::EnvError(e.to_string())
    }
}
