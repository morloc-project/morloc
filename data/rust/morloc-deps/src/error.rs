//! Error type for the dependency kernel. Kept deliberately small: every failure
//! carries a message. Consumers convert `DepsError` into their own error at the
//! crate boundary (e.g. `morloc-manager`'s `ManagerError::EnvError`).

use std::fmt;

#[derive(Debug, PartialEq, Eq)]
pub enum DepsError {
    /// An environment / dependency-resolution failure, carrying a message.
    Env(String),
    /// The declared world does not solve: the requirements of the listed
    /// programs cannot be satisfied together. `detail` carries the solver's
    /// explanation (which distinguishes true unsatisfiability from e.g. a
    /// network failure). Kept structured so the surfacing layer can name the
    /// contributing programs rather than dumping an opaque solver blob.
    Conflict {
        programs: Vec<String>,
        detail: String,
    },
}

impl DepsError {
    pub fn conflict(programs: Vec<String>, detail: impl Into<String>) -> Self {
        DepsError::Conflict { programs, detail: detail.into() }
    }
}

impl fmt::Display for DepsError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DepsError::Env(msg) => write!(f, "{msg}"),
            DepsError::Conflict { programs, detail } => {
                write!(
                    f,
                    "dependency conflict: the requirements of the environment's programs \
                     cannot be satisfied together"
                )?;
                if !programs.is_empty() {
                    write!(f, "\n  programs: {}", programs.join(", "))?;
                }
                write!(f, "\n  {detail}")
            }
        }
    }
}

impl std::error::Error for DepsError {}

pub type Result<T> = std::result::Result<T, DepsError>;
