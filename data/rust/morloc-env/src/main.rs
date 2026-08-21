//! `morloc-env` -- the in-environment dependency agent.
//!
//! It runs *inside* a morloc environment (the compiler invokes it as a sibling
//! of its own executable; a user may run it directly in an entered shell) and
//! operates on that env's dependency world via the shared `morloc-deps` kernel:
//!
//!   * `sync`  -- merge a program's declared deps, re-solve, install
//!   * `clean` -- reset the world to the installed baseline
//!   * `spec`  -- print the merged pixi manifest (no solve)
//!
//! It carries none of the manager's orchestration surface (env lifecycle,
//! containers, serve). Its inputs come from the activated environment:
//!   * `MORLOC_HOME` -- the env data dir (holds `requirements/` and `pixi/`)
//!   * `morloc lang-support` -- the language-support table, from the on-PATH
//!      compiler (coherent with whatever compiler is calling us)
//!   * `MORLOC_PIXI` (or `pixi` on PATH) -- the pixi solver
//!   * the host's conda platform tag, computed in-process

use std::path::PathBuf;
use std::process::Command;

use clap::{Parser, Subcommand};
use morloc_deps::envstore::{EnvContext, Provenance, SolveInputs};
use morloc_deps::error::DepsError;
use morloc_deps::langsupport::LangSupport;
use morloc_deps::platform::conda_platform;

#[derive(Parser)]
#[command(name = "morloc-env", about = "In-environment dependency agent for Morloc")]
struct Cli {
    #[command(subcommand)]
    cmd: Cmd,
}

#[derive(Subcommand)]
enum Cmd {
    /// Merge a program's dependency spec into the environment and re-solve.
    Sync {
        /// Program name; the key under which its spec is stored.
        #[arg(long)]
        program: String,
        /// Path to the program's `envspec.json` (as emitted by `morloc make` /
        /// `morloc envspec`).
        #[arg(long)]
        spec: PathBuf,
        /// Record as an installed program (default: a transient scratch build).
        #[arg(long)]
        installed: bool,
    },
    /// Reset the environment's dependency world to its installed baseline
    /// (drop all scratch builds and re-solve).
    Clean,
    /// Print the merged pixi manifest for the current declared world.
    Spec,
}

fn main() {
    if let Err(e) = run(Cli::parse()) {
        eprintln!("morloc-env: {e}");
        std::process::exit(1);
    }
}

fn run(cli: Cli) -> Result<(), DepsError> {
    let ctx = env_context()?;
    let pixi = pixi_bin();
    let platform = conda_platform();
    let channels = morloc_deps::pixi::default_channels();
    let support = load_lang_support()?;
    let inputs = SolveInputs {
        lang_support: &support,
        pixi_bin: pixi.as_path(),
        platform: platform.as_str(),
        channels: channels.as_slice(),
    };

    match cli.cmd {
        Cmd::Sync { program, spec, installed } => {
            let json = std::fs::read_to_string(&spec)
                .map_err(|e| DepsError::Env(format!("cannot read {}: {e}", spec.display())))?;
            let provenance = if installed {
                Provenance::Installed
            } else {
                Provenance::Scratch
            };
            ctx.sync(&program, provenance, &json, &inputs)?;
            eprintln!("Environment dependencies synced for '{program}'.");
        }
        Cmd::Clean => {
            ctx.clean(&inputs)?;
            eprintln!("Environment reset to its installed baseline.");
        }
        Cmd::Spec => {
            let specs = ctx.gather()?;
            print!("{}", ctx.rendered_manifest(&specs, &inputs)?);
        }
    }
    Ok(())
}

/// The environment context. The requirements store lives under the mutable
/// STATE root (`MORLOC_STATE`); natively that equals `MORLOC_HOME`, but a
/// container mounts state separately, so prefer `MORLOC_STATE` and fall back to
/// `MORLOC_HOME`. The pixi project dir defaults to `<state>/pixi`; a container
/// bakes its env elsewhere and passes `MORLOC_PIXI_DIR`.
fn env_context() -> Result<EnvContext, DepsError> {
    let home = std::env::var("MORLOC_STATE")
        .or_else(|_| std::env::var("MORLOC_HOME"))
        .map_err(|_| {
            DepsError::Env(
                "neither MORLOC_STATE nor MORLOC_HOME is set; run morloc-env inside a \
                 morloc environment"
                    .to_string(),
            )
        })?;
    let mut ctx = EnvContext::new(home);
    if let Ok(pixi_dir) = std::env::var("MORLOC_PIXI_DIR") {
        ctx = ctx.with_pixi_dir(pixi_dir);
    }
    Ok(ctx)
}

/// The pixi binary: `MORLOC_PIXI` if the environment names it, else `pixi` on
/// PATH (resolved by the OS at spawn).
fn pixi_bin() -> PathBuf {
    std::env::var("MORLOC_PIXI")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("pixi"))
}

/// The language-support table, obtained from the on-PATH compiler so it is
/// coherent with whichever `morloc` is driving this env.
fn load_lang_support() -> Result<LangSupport, DepsError> {
    let out = Command::new("morloc")
        .arg("lang-support")
        .output()
        .map_err(|e| DepsError::Env(format!("could not run `morloc lang-support`: {e}")))?;
    if !out.status.success() {
        return Err(DepsError::Env(format!(
            "`morloc lang-support` failed: {}",
            String::from_utf8_lossy(&out.stderr).trim()
        )));
    }
    LangSupport::from_json(&String::from_utf8_lossy(&out.stdout))
}
