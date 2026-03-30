use std::fmt;
use thiserror::Error;

use crate::types::{ContainerEngine, Scope, Version};

#[derive(Debug, Error, PartialEq, Eq)]
pub enum ManagerError {
    #[error("Configuration not found: {0}")]
    ConfigNotFound(String),

    #[error("Permission denied: {0}. Check file ownership and permissions.")]
    ConfigPermissionDenied(String),

    #[error("Invalid configuration in {path}: {msg}")]
    ConfigParseError { path: String, msg: String },

    #[error("No morloc version selected. Run: morloc-manager select <version>")]
    NoActiveVersion,

    #[error("Version {0} is not installed in any scope. Run: morloc-manager info")]
    VersionNotInstalled(Version),

    #[error("Invalid version: {0}. Expected format: MAJOR.MINOR.PATCH")]
    InvalidVersion(String),

    #[error("No command specified. Use --shell or provide a command after --.")]
    NoCommand,

    #[error("No container engine found. Install podman or docker.")]
    EngineNotFound,

    #[error("Container engine ({engine}) failed with exit code {code}:\n{stderr}")]
    EngineError {
        engine: ContainerEngine,
        code: i32,
        stderr: String,
    },

    #[error("Environment not found: {0}")]
    EnvironmentNotFound(String),

    #[error("Install failed: {0}")]
    InstallError(String),

    #[error("Uninstall failed: {0}")]
    UninstallError(String),

    #[error("{0}")]
    WorkspaceError(String),

    #[error("Environment error: {0}")]
    EnvError(String),

    #[error("Freeze failed: {0}")]
    FreezeError(String),

    #[error("SELinux error: {0}")]
    SELinuxError(String),

    #[error("{}", match .0 {
        Scope::Local => "No local configuration found. Run: morloc-manager setup",
        Scope::System => "No system configuration found. Run: sudo morloc-manager setup --scope system",
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
        }
    }
}

pub type Result<T> = std::result::Result<T, ManagerError>;
