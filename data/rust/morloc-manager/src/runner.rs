//! Execution seam between the manager and an environment's execution substrate.
//!
//! A `Runner` executes commands against a materialized environment. The two
//! implementors differ only in isolation + substrate: `ContainerRunner` runs
//! inside an OCI/Apptainer container; `NativeRunner` runs directly on the host
//! against a provisioned toolchain. Both consume the same backend-neutral
//! `RunRequest`, which is what keeps the seam honest -- container-only inputs
//! (engine flags, images, shm-size, SELinux relabeling) live behind the
//! container implementation, not in the shared request.

use crate::environment;
use crate::error::Result;
use crate::types::{Backend, EnvironmentConfig, Phase, Scope};

/// An environment resolved to its name, scope, and on-disk config.
pub struct ResolvedEnv {
    pub name: String,
    pub scope: Scope,
    pub ec: EnvironmentConfig,
}

/// A backend-neutral request to execute a command against an environment.
/// `engine_args` and `slurm_bridge` are container-only; the native backend
/// rejects them rather than silently dropping them.
pub struct RunRequest {
    pub verbose: bool,
    pub shell: bool,
    pub args: Vec<String>,
    pub user_env: Vec<(String, String)>,
    pub engine_args: Vec<String>,
    pub phase: Phase,
    pub slurm_bridge: bool,
}

/// The execution substrate for an environment.
///
/// Note: `stop`/`logs`/`status` are deliberately NOT on this trait -- they must
/// dispatch on the *stored launch handle* (which knows its own engine/pid), not
/// on the environment's current backend, so a migrated or dual-launched serve is
/// never stranded. Only `run` and `serve` (fresh launches, keyed on the env's
/// backend) belong here.
pub trait Runner {
    /// Execute a command against the environment, streaming stdio through and
    /// propagating the child's exit code.
    fn run(&self, env: &ResolvedEnv, req: &RunRequest) -> Result<()>;

    /// Launch a serve (the shared orchestration -- spec, port, token, record --
    /// lives in the `start` handler; this only launches + tracks the process).
    fn serve(&self, env: &ResolvedEnv, req: &crate::ServeRequest) -> Result<crate::ServeOutcome>;
}

pub struct ContainerRunner;
pub struct NativeRunner;

impl Runner for ContainerRunner {
    fn run(&self, env: &ResolvedEnv, req: &RunRequest) -> Result<()> {
        crate::container_run_env(env, req)
    }
    fn serve(&self, env: &ResolvedEnv, req: &crate::ServeRequest) -> Result<crate::ServeOutcome> {
        crate::container_serve(env, req)
    }
}

impl Runner for NativeRunner {
    fn run(&self, env: &ResolvedEnv, req: &RunRequest) -> Result<()> {
        crate::native_run_env(env, req)
    }
    fn serve(&self, env: &ResolvedEnv, req: &crate::ServeRequest) -> Result<crate::ServeOutcome> {
        crate::native_serve_launch(env, req)
    }
}

/// Select the execution substrate for an environment from its backend.
pub fn runner_for(ec: &EnvironmentConfig) -> Box<dyn Runner> {
    match ec.backend {
        Backend::Container(_) => Box::new(ContainerRunner),
        Backend::Native => Box::new(NativeRunner),
    }
}

/// Resolve the target environment (the active one when `target` is `None`) and
/// execute the request through its backend's Runner.
pub fn run_in_env(
    target: Option<(String, Scope, EnvironmentConfig)>,
    req: RunRequest,
) -> Result<()> {
    let (name, scope, ec) = match target {
        Some(t) => t,
        None => environment::resolve_active_environment()?,
    };
    let env = ResolvedEnv { name, scope, ec };
    runner_for(&env.ec).run(&env, &req)
}
