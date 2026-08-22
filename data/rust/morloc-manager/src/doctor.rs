use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::config as cfg;
use crate::container::{container_run_quiet, engine_executable, image_exists_locally, RunConfig};
use crate::environment;
use crate::error::Result;
use crate::types::*;

#[derive(serde::Serialize)]
pub struct CheckResult {
    pub category: String,
    pub result: String,
    pub message: String,
}

#[derive(serde::Serialize)]
pub struct DoctorSummary {
    pub ok: u32,
    pub warnings: u32,
    pub errors: u32,
}

struct Counts {
    ok: u32,
    warn: u32,
    fail: u32,
    json_mode: bool,
    current_category: String,
    checks: Vec<CheckResult>,
}

impl Counts {
    fn new(json_mode: bool) -> Self {
        Self { ok: 0, warn: 0, fail: 0, json_mode, current_category: String::new(), checks: Vec::new() }
    }

    fn set_category(&mut self, cat: &str) {
        self.current_category = cat.to_string();
    }

    fn pass(&mut self, msg: &str) {
        self.ok += 1;
        if self.json_mode {
            self.checks.push(CheckResult {
                category: self.current_category.clone(),
                result: "ok".to_string(),
                message: msg.to_string(),
            });
        } else {
            // anstream strips the ANSI when stdout is not a TTY (and honors
            // NO_COLOR), matching the rest of morloc-manager's coloured output.
            anstream::println!("  \x1b[1;32m[ok]\x1b[0m {msg}");
        }
    }

    fn warn(&mut self, msg: &str) {
        self.warn += 1;
        if self.json_mode {
            self.checks.push(CheckResult {
                category: self.current_category.clone(),
                result: "warning".to_string(),
                message: msg.to_string(),
            });
        } else {
            anstream::println!("  \x1b[1;33m[!!]\x1b[0m {msg}");
        }
    }

    fn fail(&mut self, msg: &str) {
        self.fail += 1;
        if self.json_mode {
            self.checks.push(CheckResult {
                category: self.current_category.clone(),
                result: "error".to_string(),
                message: msg.to_string(),
            });
        } else {
            anstream::println!("  \x1b[1;31m[EE]\x1b[0m {msg}");
        }
    }

    fn skip(&mut self, msg: &str) {
        if self.json_mode {
            self.checks.push(CheckResult {
                category: self.current_category.clone(),
                result: "skipped".to_string(),
                message: msg.to_string(),
            });
        } else {
            // Skips are low-signal; dim the whole line.
            anstream::println!("  \x1b[2m[--] {msg}\x1b[0m");
        }
    }
}

/// A "<n> <label>" summary segment, coloured with `ansi` only when non-zero (so
/// "0 errors" stays neutral rather than glowing red). anstream strips the codes
/// off a non-TTY.
fn color_count(n: u32, label: &str, ansi: &str) -> String {
    if n > 0 {
        format!("\x1b[{ansi}m{n} {label}\x1b[0m")
    } else {
        format!("{n} {label}")
    }
}

/// Print the final `N passed, M warnings, K errors` line (green pass count;
/// warnings/errors coloured only when non-zero).
fn print_summary_line(ok: u32, warn: u32, fail: u32) {
    anstream::println!(
        "\x1b[1;32m{ok} passed\x1b[0m, {}, {}",
        color_count(warn, "warnings", "1;33"),
        color_count(fail, "errors", "1;31"),
    );
}

pub fn doctor(
    engine: ContainerEngine,
    verbose: bool,
    env_name: &str,
    scope: Scope,
    ec: &EnvironmentConfig,
    deep: bool,
    strict: bool,
    slurm: bool,
    json_mode: bool,
) -> Result<()> {
    let scope_str = match scope {
        Scope::Local => "local",
        Scope::System => "system",
    };
    let engine_str = match engine {
        ContainerEngine::Docker => "docker",
        ContainerEngine::Podman => "podman",
        ContainerEngine::Apptainer => "apptainer",
    };

    if !json_mode {
        println!("Environment: {env_name} ({scope_str})");
        println!("Engine:      {engine_str}");
        println!();
    }

    let mut c = Counts::new(json_mode);
    let data_dir = cfg::env_data_dir(scope, env_name);

    // ==== Prerequisites ====
    if !json_mode { println!("Prerequisites"); }
    c.set_category("prerequisites");
    check_engine(&mut c, engine);
    check_base_image(&mut c, engine, &ec.base_image);
    check_built_image(&mut c, engine, ec, scope, env_name);
    check_data_dirs(&mut c, &data_dir);
    check_file_readability(&mut c, &data_dir);

    // ==== Manifests ====
    if !json_mode { println!("\nManifests"); }
    c.set_category("manifests");
    check_manifests(&mut c, &data_dir, ec.morloc_version.as_ref());

    // ==== Deep checks ====
    c.set_category("deep");
    if deep {
        if !json_mode { println!("\nDeep checks"); }
        check_morloc_version(&mut c, engine, ec);
        check_programs_deep(&mut c, engine, verbose, ec, &data_dir);
    } else {
        if !json_mode { println!("\nDeep checks"); }
        c.skip("Use --deep to run container-side checks");
    }

    // ==== SLURM-bridge prerequisites (opt-in via --slurm) ====
    if slurm {
        if !json_mode { println!("\nSLURM bridge"); }
        c.set_category("slurm");
        check_slurm_prereqs(&mut c, engine, ec, scope, env_name);
    }

    let fail_count = c.fail;
    let warn_count = c.warn;

    if json_mode {
        #[derive(serde::Serialize)]
        struct DoctorOutput {
            environment: String,
            scope: String,
            engine: String,
            checks: Vec<CheckResult>,
            summary: DoctorSummary,
        }
        let output = DoctorOutput {
            environment: env_name.to_string(),
            scope: scope_str.to_string(),
            engine: engine_str.to_string(),
            checks: c.checks,
            summary: DoctorSummary { ok: c.ok, warnings: warn_count, errors: fail_count },
        };
        println!("{}", serde_json::to_string_pretty(&output).unwrap());
    } else {
        // ==== Summary ====
        println!();
        print_summary_line(c.ok, warn_count, fail_count);
    }

    if fail_count > 0 {
        return Err(crate::error::ManagerError::DoctorFailed(fail_count));
    }
    if strict && warn_count > 0 {
        return Err(crate::error::ManagerError::DoctorFailed(warn_count));
    }
    Ok(())
}

/// Native-backend doctor: host-side health checks (no container engine). Reuses
/// the backend-neutral data-dir / readability / manifest checks and adds native
/// toolchain + runtime probes.
pub fn native_doctor(
    _verbose: bool,
    env_name: &str,
    scope: Scope,
    ec: &EnvironmentConfig,
    deep: bool,
    strict: bool,
    json_mode: bool,
) -> Result<()> {
    let scope_str = match scope {
        Scope::Local => "local",
        Scope::System => "system",
    };
    if !json_mode {
        println!("Environment: {env_name} ({scope_str})");
        println!("Backend:     native");
        println!();
        println!("Prerequisites");
    }

    let mut c = Counts::new(json_mode);
    let data_dir = cfg::env_data_dir(scope, env_name);
    let pixi_dir = data_dir.join("pixi");
    // Read the native runtime record once; the toolchain/version/linkage checks
    // all key off its captured activation env-map.
    let rt = cfg::read_native_runtime(scope, env_name).ok();

    c.set_category("prerequisites");
    check_data_dirs(&mut c, &data_dir);
    check_file_readability(&mut c, &data_dir);
    check_native_runtime(&mut c, env_name, rt.as_ref());
    check_state(&mut c, &data_dir);
    if let Some(ref rt) = rt {
        check_activation_toolchain(&mut c, rt);
    }
    check_pixi_present(&mut c, scope);

    if !json_mode { println!("\nRuntime"); }
    c.set_category("runtime");
    check_core_artifacts(&mut c, &data_dir);
    let (langs, lock_present) = detect_languages(&pixi_dir);
    if lock_present {
        check_runtime_artifacts(&mut c, &data_dir, &langs);
    } else {
        c.warn("cannot inventory language stacks: no pixi.lock (env not solved; re-run: morloc-manager update)");
    }
    if let Some(ref rt) = rt {
        check_linkage(&mut c, &data_dir, rt);
    }

    if !json_mode { println!("\nVersions"); }
    c.set_category("versions");
    match rt.as_ref() {
        Some(rt) => check_staleness(&mut c, ec, rt),
        None => c.skip("native env not materialized -- no version info"),
    }

    if !json_mode { println!("\nDependencies"); }
    c.set_category("dependencies");
    check_dep_world(&mut c, &pixi_dir);

    if !json_mode { println!("\nManifests"); }
    c.set_category("manifests");
    check_manifests(&mut c, &data_dir, ec.morloc_version.as_ref());

    if !json_mode { println!("\nDeep checks"); }
    c.set_category("deep");
    if deep {
        check_native_programs(&mut c, &data_dir);
    } else {
        c.skip("Use --deep to check installed program launchers");
    }

    let fail_count = c.fail;
    let warn_count = c.warn;
    if json_mode {
        #[derive(serde::Serialize)]
        struct DoctorOutput {
            environment: String,
            scope: String,
            backend: String,
            checks: Vec<CheckResult>,
            summary: DoctorSummary,
        }
        let output = DoctorOutput {
            environment: env_name.to_string(),
            scope: scope_str.to_string(),
            backend: "native".to_string(),
            checks: c.checks,
            summary: DoctorSummary { ok: c.ok, warnings: warn_count, errors: fail_count },
        };
        println!("{}", serde_json::to_string_pretty(&output).unwrap());
    } else {
        println!();
        print_summary_line(c.ok, warn_count, fail_count);
    }

    if fail_count > 0 {
        return Err(crate::error::ManagerError::DoctorFailed(fail_count));
    }
    if strict && warn_count > 0 {
        return Err(crate::error::ManagerError::DoctorFailed(warn_count));
    }
    Ok(())
}

/// Native materialization health: the runtime record + captured toolchain
/// activation, the conda prefix it points at, and the libmorloc + nexus
/// `morloc init` builds into the env.
fn check_native_runtime(c: &mut Counts, env_name: &str, rt: Option<&NativeRuntime>) {
    match rt {
        Some(rt) => match rt.activation_env.iter().find(|(k, _)| k == "CONDA_PREFIX") {
            Some((_, prefix)) => {
                c.pass("native runtime materialized (toolchain activation captured)");
                if Path::new(prefix).is_dir() {
                    c.pass(&format!("conda toolchain present ({prefix})"));
                } else {
                    c.fail(&format!(
                        "conda prefix missing at {prefix} -- re-run: morloc-manager update --env {env_name}"
                    ));
                }
            }
            None => c.warn("runtime record present but no CONDA_PREFIX in the activation env-map"),
        },
        None => c.fail(&format!(
            "native environment not materialized -- run: morloc-manager update --env {env_name}"
        )),
    }
}

// ======================================================================
// Native health: artifact inventory, toolchain, versions, deps, state
// ======================================================================

/// Timeout for doctor's activation subprocesses. A hung env binary must not hang
/// doctor; on timeout the call reports "could not run" (None) and doctor moves on.
const CMD_TIMEOUT: std::time::Duration = std::time::Duration::from_secs(15);

/// Run `program args` with the captured activation env-map applied, bounded by
/// `CMD_TIMEOUT`. `None` on spawn error or timeout. On timeout the short-lived
/// child (a `--version`/`ldd`) is left to exit on its own; doctor returns
/// promptly rather than blocking on it.
fn run_with_activation(
    activation: &[(String, String)],
    program: impl AsRef<std::ffi::OsStr>,
    args: &[&str],
) -> Option<std::process::Output> {
    let program = program.as_ref().to_os_string();
    let args: Vec<String> = args.iter().map(|s| s.to_string()).collect();
    let activation = activation.to_vec();
    let (tx, rx) = std::sync::mpsc::channel();
    std::thread::spawn(move || {
        let mut cmd = Command::new(program);
        cmd.args(&args).stdin(std::process::Stdio::null());
        crate::apply_activation(&mut cmd, &activation);
        let _ = tx.send(cmd.output());
    });
    match rx.recv_timeout(CMD_TIMEOUT) {
        Ok(Ok(out)) => Some(out),
        _ => None,
    }
}

/// The runtime-store dir this env is bound to, parsed from the activation PATH
/// (materialize prepends `runtimes/<ver>/`).
fn runtime_dir_from_activation(activation: &[(String, String)]) -> Option<PathBuf> {
    let path = activation.iter().find(|(k, _)| k == "PATH").map(|(_, v)| v.as_str())?;
    path.split(':')
        .map(Path::new)
        .find(|p| p.components().any(|c| c.as_os_str() == "runtimes"))
        .map(|p| p.to_path_buf())
}

/// The provisioned runtime version: the `.provisioned` stamp, else the dir name.
fn runtime_store_version(runtime_dir: &Path) -> Option<String> {
    fs::read_to_string(runtime_dir.join(".provisioned"))
        .ok()
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .or_else(|| {
            runtime_dir
                .file_name()
                .and_then(|n| n.to_str())
                .map(str::to_string)
        })
}

/// The last whitespace token of a `--version` line, parsed as a `Version`.
fn parse_version_output(out: &[u8]) -> Option<Version> {
    let s = String::from_utf8_lossy(out);
    s.split_whitespace().last().and_then(|t| t.trim().parse::<Version>().ok())
}

/// Activation completeness: the captured activation must export the compiler
/// tools cargo/cc-rs need. Without `$AR`, native C/Rust builds fail with
/// `cc-rs: failed to find tool "ar"`.
fn check_activation_toolchain(c: &mut Counts, rt: &NativeRuntime) {
    let get = |k: &str| {
        rt.activation_env
            .iter()
            .find(|(kk, _)| kk == k)
            .map(|(_, v)| v.as_str())
    };
    let required = ["CC", "CXX", "AR", "RANLIB"];
    let missing: Vec<&str> = required
        .iter()
        .copied()
        .filter(|k| get(k).map(|v| v.is_empty()).unwrap_or(true))
        .collect();
    if missing.is_empty() {
        c.pass("toolchain compiler vars exported (CC, CXX, AR, RANLIB)");
    } else {
        c.fail(&format!(
            "activation missing {} -- native C/Rust builds will fail (\"failed to find tool ar\"); re-run: morloc-manager update",
            missing.join(", ")
        ));
    }
    // The named tools should exist (bare name -> conda prefix bin; else a path).
    if let Some(prefix) = get("CONDA_PREFIX") {
        let bin = Path::new(prefix).join("bin");
        for var in ["CC", "AR"] {
            if let Some(tool) = get(var) {
                let name = tool.split_whitespace().next().unwrap_or(tool);
                if name.is_empty() {
                    continue;
                }
                let candidate = if name.contains('/') {
                    PathBuf::from(name)
                } else {
                    bin.join(name)
                };
                if !candidate.is_file() {
                    c.warn(&format!(
                        "${var}={name} not found under {} -- toolchain may be incomplete",
                        bin.display()
                    ));
                }
            }
        }
    }
}

/// Pixi presence: locatable without provisioning (never triggers a download).
fn check_pixi_present(c: &mut Counts, scope: Scope) {
    match crate::locate_pixi(scope) {
        Some(p) => c.pass(&format!("pixi present ({})", p.display())),
        None => c.warn(
            "pixi not found (MORLOC_PIXI unset and none provisioned) -- dependency solves must provision it",
        ),
    }
}

/// The runtime artifacts `morloc init` builds for `lang`, relative to the env
/// runtime root; `(relpath, is_dir)`.
fn language_artifacts(lang: &str) -> Vec<(&'static str, bool)> {
    // Only the C ABI (core), the C++ static lib + headers, the R binding lib, and
    // the Rust workspace SOURCE land under `<env>`. Notably absent by design: the
    // rust rlibs (built on demand
    // into `cache/rust-build/`), a `lib/R` dir, and the python binding (it lives
    // in the pixi env's site-packages, not `<env>/lib`, so python is not
    // inventoried here).
    match lang {
        "cpp" => vec![
            ("lib/libcppmorloc.a", false),
            ("include/cppmorloc.hpp", false),
            ("include/mlccpptypes", true),
        ],
        // The R binding is a single shared lib (no `lib/R` dir).
        "r" => vec![("lib/librmorloc.so", false)],
        // Rust readiness = the workspace source (MORLOC_RUST_DIR); the rlibs are
        // compiled on demand, not prebuilt.
        "rust" => vec![("rust", true)],
        _ => vec![],
    }
}

fn dir_non_empty(p: &Path) -> bool {
    fs::read_dir(p).map(|mut d| d.next().is_some()).unwrap_or(false)
}

/// The core runtime artifacts (built for every env).
fn check_core_artifacts(c: &mut Counts, data_dir: &Path) {
    let mut missing = Vec::new();
    let so = data_dir.join("lib/libmorloc.so");
    let dylib = data_dir.join("lib/libmorloc.dylib");
    if !(so.is_file() || dylib.is_file()) {
        missing.push("libmorloc");
    }
    if !data_dir.join("include/morloc.h").is_file() {
        missing.push("morloc.h");
    }
    if !data_dir.join("bin/morloc-nexus").is_file() {
        missing.push("morloc-nexus");
    }
    if missing.is_empty() {
        c.pass("core runtime present (libmorloc, morloc.h, morloc-nexus)");
    } else {
        c.fail(&format!(
            "core runtime incomplete: missing {} -- re-run: morloc-manager update",
            missing.join(", ")
        ));
    }
}

/// Per-language stack inventory. C++ is core (cxx-compiler in the core
/// toolchain); the active languages are inventoried too, but a language with no
/// verified per-env layout (empty `language_artifacts`) is skipped rather than
/// asserted. One line per stack; a miss names the exact absent artifacts.
fn check_runtime_artifacts(c: &mut Counts, data_dir: &Path, langs: &[String]) {
    let mut to_check: Vec<&str> = vec!["cpp"];
    for l in langs {
        to_check.push(l.as_str());
    }
    for lang in to_check {
        let artifacts = language_artifacts(lang);
        // No verified per-env layout for this language yet -> don't assert.
        if artifacts.is_empty() {
            continue;
        }
        let missing: Vec<&str> = artifacts
            .into_iter()
            .filter(|(rel, is_dir)| {
                let p = data_dir.join(rel);
                if *is_dir {
                    !dir_non_empty(&p)
                } else {
                    !p.is_file()
                }
            })
            .map(|(rel, _)| rel)
            .collect();
        if missing.is_empty() {
            c.pass(&format!("{lang} runtime stack present"));
        } else {
            c.fail(&format!(
                "{lang} runtime stack incomplete: missing {} -- re-run: morloc-manager update",
                missing.join(", ")
            ));
        }
    }
}

/// The languages this env provisions, read from the solved world (pixi.lock).
/// Returns `(langs, lock_present)`; the lock reflects what `morloc init` actually
/// built, which is exactly what the inventory should be checked against, so its
/// mtime relative to pixi.toml is irrelevant (pixi does not rewrite the lock on an
/// unchanged solve, so that comparison false-positives). The runtime-package ->
/// language mapping is shared with `info` via `pixi::runtime_languages`.
fn detect_languages(pixi_dir: &Path) -> (Vec<String>, bool) {
    if !pixi_dir.join("pixi.lock").is_file() {
        return (Vec::new(), false);
    }
    let platform = morloc_deps::platform::conda_platform();
    let pkgs = morloc_deps::pixi::locked_packages(pixi_dir, &platform).unwrap_or_default();
    let langs = morloc_deps::pixi::runtime_languages(&pkgs)
        .into_iter()
        .map(|(lang, _)| lang.to_string())
        .collect();
    (langs, true)
}

/// Loadability companion to the inventory: presence is not loadability. `ldd`
/// (under the activation, so conda's libs are on the loader path) must resolve
/// every dependency -- an unresolved GLIBC symbol or a broken rpath passes the
/// inventory but crashes at runtime as the cryptic "daemon exited" failures.
fn check_linkage(c: &mut Counts, data_dir: &Path, rt: &NativeRuntime) {
    // Only libmorloc.so (the core C ABI lib) is self-contained enough for a bare
    // ldd: it resolves against libc + the conda toolchain libs via its rpath. The
    // language bindings (librmorloc.so -> libR.so, pymorloc -> libpython) resolve
    // their runtime's libs only inside that runtime's own load context, so ldd on
    // them in isolation reports false "not found"s. This check targets the one
    // artifact whose unresolved symbol IS a real toolchain/GLIBC break.
    let lib = data_dir.join("lib/libmorloc.so");
    if !lib.is_file() {
        return; // core check already reports a missing libmorloc
    }
    let ts = lib.to_string_lossy();
    let out = match run_with_activation(&rt.activation_env, "ldd", &[ts.as_ref()]) {
        Some(out) => out,
        None => return, // ldd unavailable (e.g. macOS) or timed out; skip
    };
    let text = String::from_utf8_lossy(&out.stdout);
    let unresolved: Vec<&str> = text
        .lines()
        .filter(|l| l.contains("not found"))
        .map(|l| l.trim())
        .collect();
    if unresolved.is_empty() {
        c.pass("libmorloc resolves its dynamic dependencies");
    } else {
        c.fail(&format!(
            "libmorloc has unresolved dependencies ({}) -- ABI/toolchain mismatch; re-run: morloc-manager update",
            unresolved.join("; ")
        ));
    }
}

/// Version staleness: the morloc/manager/agent this env resolves vs what it was
/// built with. Catches a stale compiler/manager/agent after an update, or a
/// shadowing binary on PATH.
fn check_staleness(c: &mut Counts, ec: &EnvironmentConfig, rt: &NativeRuntime) {
    let running = env!("CARGO_PKG_VERSION");
    let runtime_dir = runtime_dir_from_activation(&rt.activation_env);
    let stamp_raw = runtime_dir.as_deref().and_then(runtime_store_version);
    let stamp_ver = stamp_raw.as_deref().and_then(|s| s.parse::<Version>().ok());
    // A stamp that isn't a semver ("dev") is a local runtime bound via
    // MORLOC_COMPILER_BIN: there is no released version to track, so the
    // compiler-version checks do not apply. The tooling checks still do -- the
    // manager/agent are real builds even in dev mode.
    let dev_runtime = stamp_raw.is_some() && stamp_ver.is_none();

    if dev_runtime {
        c.skip("local dev runtime (MORLOC_COMPILER_BIN) -- compiler-version checks skipped");
    } else {
        // The morloc resolved under activation vs the store stamp.
        if let Some(want) = &stamp_ver {
            match run_with_activation(&rt.activation_env, "morloc", &["--version"]) {
                Some(out) if out.status.success() => match parse_version_output(&out.stdout) {
                    Some(got) if &got == want => {
                        c.pass(&format!("morloc compiler matches the env runtime ({})", got.show()))
                    }
                    Some(got) => c.fail(&format!(
                        "on-PATH morloc is {} but this env's runtime is {} -- a different morloc is shadowing it; re-run: morloc-manager update",
                        got.show(),
                        want.show()
                    )),
                    None => c.warn("could not parse `morloc --version` output"),
                },
                _ => c.warn("could not run `morloc --version` under the env activation"),
            }
        }

        // Recorded env version vs the store stamp.
        match (&ec.morloc_version, &stamp_ver) {
            (Some(rec), Some(store)) if rec == store => {
                c.pass(&format!("recorded env version matches the runtime store ({})", rec.show()))
            }
            (Some(rec), Some(store)) => c.fail(&format!(
                "env records morloc {} but the runtime store is {} -- re-run: morloc-manager update",
                rec.show(),
                store.show()
            )),
            (None, _) => c.warn(
                "materialized env has no recorded morloc version -- re-run: morloc-manager update",
            ),
            (Some(_), None) => {}
        }
    }

    // The manager that materialized the env vs the running manager.
    match &rt.manager_version {
        Some(v) if v == running => {
            c.pass(&format!("environment materialized by this morloc-manager ({running})"))
        }
        Some(v) => c.warn(&format!(
            "env materialized by morloc-manager {v}, running {running} -- re-run: morloc-manager update if tooling misbehaves"
        )),
        None => {}
    }

    // The in-env dependency agent vs the running manager (same tooling lane).
    if let Some(dir) = &runtime_dir {
        let agent = crate::provision::runtime_morloc_env_bin(dir);
        if agent.is_file() {
            match run_with_activation(&rt.activation_env, &agent, &["--version"]) {
                Some(out) if out.status.success() => {
                    let s = String::from_utf8_lossy(&out.stdout);
                    let tok = s.split_whitespace().last().unwrap_or("").trim();
                    if tok == running {
                        c.pass(&format!("morloc-env agent matches the manager ({running})"));
                    } else {
                        c.warn(&format!(
                            "morloc-env agent is {tok} but the manager is {running} -- dep-sync may misbehave; re-run: morloc-manager update"
                        ));
                    }
                }
                _ => c.skip("morloc-env agent version not reported (predates --version)"),
            }
        } else {
            c.warn(
                "morloc-env agent missing from the runtime store -- `morloc make` cannot provision deps",
            );
        }
    }
}

/// Dependency-world consistency: is the solved world in step with the declared
/// requirements? A stale lock means pools import packages that are not installed
/// -- a runtime ImportError with no build error.
fn check_dep_world(c: &mut Counts, pixi_dir: &Path) {
    // Presence only -- deliberately NO mtime comparison. pixi leaves pixi.lock
    // untouched on an unchanged solve while `update` still rewrites pixi.toml, so
    // lock-vs-toml false-positives; and a hand-driven `pixi add` makes pixi.toml
    // newer than the marker without anything being wrong. The robust, quiet
    // signals are just: is there a solved world, and did a materialize complete.
    if pixi_dir.join("pixi.lock").is_file() {
        c.pass("environment is solved (pixi.lock present)");
    } else {
        c.warn("no pixi.lock -- environment not solved (re-run: morloc-manager update)");
    }
    if !pixi_dir.join("materialized.toml").is_file() {
        c.warn(
            "no materialization marker -- the last solve may not have completed; re-run: morloc-manager update",
        );
    }
}

/// State hygiene: the data dir is writable (build/serve/freeze all write), and
/// no shared-memory segments leaked from a prior crash.
fn check_state(c: &mut Counts, data_dir: &Path) {
    // Only probe writability when the dir exists (its absence is reported by
    // check_data_dirs); otherwise the ENOENT would read as a permissions issue.
    if data_dir.is_dir() {
        let probe = data_dir.join(".doctor-write-probe");
        match fs::write(&probe, b"") {
            Ok(_) => {
                let _ = fs::remove_file(&probe);
                c.pass("environment data dir is writable");
            }
            Err(e) => c.fail(&format!(
                "environment data dir is not writable ({e}) -- serving/building will fail"
            )),
        }
    }
    // Leaked shared-memory segments. Segments are named `morloc-<pid>-...`; a
    // segment whose owner pid is still alive belongs to a RUNNING program or serve
    // and must NOT be flagged. Only segments whose owner is gone are true leaks
    // from a crash. (/dev/shm and /proc are both Linux; on other platforms the
    // read_dir simply fails and the scan is skipped.)
    if let Ok(entries) = fs::read_dir("/dev/shm") {
        let leaked = entries
            .flatten()
            .filter_map(|e| {
                let n = e.file_name().to_string_lossy().to_string();
                let rest = n.strip_prefix("morloc-").or_else(|| n.strip_prefix("morloc_"))?;
                // The owner pid is the leading numeric field of the suffix.
                let pid: i32 = rest
                    .split(|ch: char| !ch.is_ascii_digit())
                    .next()?
                    .parse()
                    .ok()?;
                Some(pid)
            })
            .filter(|&pid| !pid_alive(pid))
            .count();
        if leaked > 0 {
            c.warn(&format!(
                "{leaked} orphaned /dev/shm/morloc-* segment(s) from a dead process -- remove to reclaim memory"
            ));
        }
    }
}

/// Whether a pid is still running. `/dev/shm` and `/proc` are both Linux, and
/// this is only reached from the `/dev/shm` scan, so a `/proc/<pid>` probe is
/// sufficient and needs no extra dependency.
fn pid_alive(pid: i32) -> bool {
    pid > 0 && Path::new("/proc").join(pid.to_string()).exists()
}

/// Deep native check: each installed program launcher exists and is executable.
fn check_native_programs(c: &mut Counts, data_dir: &Path) {
    let bin = data_dir.join("bin");
    let reserved = ["morloc-nexus", "morloc", "morloc-manager"];
    let mut found = 0;
    if let Ok(entries) = fs::read_dir(&bin) {
        for entry in entries.flatten() {
            let name = entry.file_name().to_string_lossy().to_string();
            if reserved.contains(&name.as_str()) {
                continue;
            }
            found += 1;
            #[cfg(unix)]
            {
                use std::os::unix::fs::PermissionsExt;
                let exec = fs::metadata(entry.path())
                    .map(|m| m.permissions().mode() & 0o111 != 0)
                    .unwrap_or(false);
                if exec {
                    c.pass(&format!("program '{name}' launcher is executable"));
                } else {
                    c.warn(&format!("program '{name}' launcher is not executable"));
                }
            }
        }
    }
    if found == 0 {
        c.skip("no programs installed yet (morloc-manager install <file>.loc)");
    }
}

// ======================================================================
// Individual checks
// ======================================================================

fn check_engine(c: &mut Counts, engine: ContainerEngine) {
    let exe = engine_executable(engine);
    // For Apptainer/Singularity there is no `info --format ...`; use
    // `--version` directly which prints something like
    // "apptainer version 1.3.4".
    let argv: Vec<&str> = match engine {
        ContainerEngine::Podman => vec!["info", "--format", "{{.Version.Version}}"],
        ContainerEngine::Docker => vec!["info", "--format", "{{.ServerVersion}}"],
        ContainerEngine::Apptainer => vec!["--version"],
    };
    let output = Command::new(exe).args(&argv).output();
    match output {
        Ok(o) if o.status.success() => {
            let ver = String::from_utf8_lossy(&o.stdout).trim().to_string();
            if ver.is_empty() {
                c.pass(&format!("{exe} engine reachable"));
            } else {
                c.pass(&format!("{exe} engine reachable ({ver})"));
            }
        }
        Ok(o) => {
            let err = String::from_utf8_lossy(&o.stderr).trim().to_string();
            if err.contains("permission denied") || err.contains("Permission denied") {
                c.fail(&format!("{exe} permission denied -- add user to {exe} group?"));
            } else {
                c.fail(&format!("{exe} not reachable: {err}"));
            }
        }
        Err(e) => {
            c.fail(&format!("{exe} not found: {e}"));
        }
    }
}

fn check_base_image(c: &mut Counts, engine: ContainerEngine, base_image: &str) {
    // For Apptainer the engine-side `image_exists_locally` call below is
    // skipped: ec.base_image is the human-readable OCI URI, while presence
    // is determined by the .sif file on disk (checked separately when
    // base_sif is recorded). check_built_image handles the .sif-path case.
    if matches!(engine, ContainerEngine::Apptainer) {
        c.pass(&format!("Base image {base_image} (apptainer; presence checked via .sif)"));
        return;
    }
    if image_exists_locally(engine, base_image) {
        c.pass(&format!("Base image {base_image}"));
    } else {
        c.fail(&format!(
            "Base image {base_image} not found locally\n       \
             Run: morloc-manager run -- morloc --version  (triggers pull)"
        ));
    }
}

fn check_built_image(c: &mut Counts, engine: ContainerEngine, ec: &EnvironmentConfig, scope: Scope, env_name: &str) {
    // Apptainer: check .sif files on disk rather than OCI image tags.
    if matches!(engine, ContainerEngine::Apptainer) {
        if let Some(ref sif) = ec.base_sif {
            if std::path::Path::new(sif).is_file() {
                c.pass(&format!("Base .sif {sif}"));
            } else {
                c.fail(&format!(
                    "Base .sif {sif} not found on disk\n       \
                     Run: morloc-manager update --image <ref> to repull"
                ));
            }
        }
        if ec.singularity_def.is_some() || ec.dockerfile.is_some() {
            let recipe_kind = if ec.singularity_def.is_some() { ".def" } else { "Dockerfile" };
            match &ec.layered_sif {
                Some(sif) => {
                    if std::path::Path::new(sif).is_file() {
                        c.pass(&format!("Layered .sif {sif}"));
                    } else {
                        c.fail(&format!(
                            "Layered .sif {sif} not found on disk\n       \
                             Run: morloc-manager update"
                        ));
                    }
                }
                None => {
                    c.warn(&format!(
                        "{recipe_kind} configured but no layered .sif built yet\n       \
                         Run: morloc-manager update"
                    ));
                }
            }
        }
        return;
    }
    if ec.dockerfile.is_none() {
        return;
    }
    // Check if the Dockerfile file itself still exists
    let df_path = cfg::env_dockerfile_path(scope, env_name);
    if !df_path.exists() {
        c.warn(&format!(
            "Dockerfile configured but file is missing: {}\n       \
             Remove stale config or recreate the file, then run: morloc-manager update",
            df_path.display()
        ));
        return;
    }
    match &ec.built_image {
        Some(img) => {
            if image_exists_locally(engine, img) {
                c.pass(&format!("Built image {img}"));
            } else {
                c.fail(&format!(
                    "Built image {img} not found locally\n       \
                     Run: morloc-manager update"
                ));
            }
        }
        None => {
            c.warn("Dockerfile configured but no image built yet\n       \
                    Run: morloc-manager update");
        }
    }
}

fn check_data_dirs(c: &mut Counts, data_dir: &Path) {
    let expected = ["lib", "bin", "opt", "fdb", "src/morloc/plane", "exe"];

    let mut missing: Vec<&str> = Vec::new();
    for dir in &expected {
        if !data_dir.join(dir).is_dir() {
            missing.push(dir);
        }
    }
    if missing.is_empty() {
        c.pass("Data directories intact");
    } else {
        c.fail(&format!(
            "Missing directories: {}\n       \
             Run: morloc-manager run -- morloc init -f",
            missing.join(", ")
        ));
    }
}

/// Walk exe/ and other data subdirectories, warning about files unreadable
/// by the current user (which would cause freeze to fail).
fn check_file_readability(c: &mut Counts, data_dir: &Path) {
    let dirs_to_check = ["exe", "bin", "lib"];
    let mut unreadable: Vec<String> = Vec::new();
    for dir in &dirs_to_check {
        let dir_path = data_dir.join(dir);
        if dir_path.is_dir() {
            collect_unreadable(&dir_path, &mut unreadable);
        }
    }
    if unreadable.is_empty() {
        c.pass("All data files readable");
    } else {
        let shown: Vec<&str> = unreadable.iter().take(5).map(|s| s.as_str()).collect();
        let suffix = if unreadable.len() > 5 {
            format!(" (and {} more)", unreadable.len() - 5)
        } else {
            String::new()
        };
        c.fail(&format!(
            "Unreadable files (freeze will fail): {}{suffix}\n       \
             Fix with: chmod -R a+rX <data-dir>",
            shown.join(", ")
        ));
    }
}

fn collect_unreadable(dir: &Path, out: &mut Vec<String>) {
    let Ok(entries) = fs::read_dir(dir) else {
        out.push(dir.display().to_string());
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect_unreadable(&path, out);
        } else if fs::File::open(&path).is_err() {
            out.push(path.display().to_string());
        }
    }
}

fn check_manifests(
    c: &mut Counts,
    data_dir: &Path,
    expected_version: Option<&Version>,
) {
    let exe_dir = data_dir.join("exe");
    if !exe_dir.is_dir() {
        c.warn("No exe/ directory found");
        return;
    }

    let entries = match fs::read_dir(&exe_dir) {
        Ok(e) => e,
        Err(e) => {
            c.fail(&format!("Cannot read exe/: {e}"));
            return;
        }
    };

    let mut found_any = false;
    for entry in entries.flatten() {
        let dir = entry.path();
        if !dir.is_dir() {
            continue;
        }
        let prog_name = entry.file_name().to_string_lossy().into_owned();
        // Installed layout: exe/<name>/<name>-build/manifest.json.
        let manifest_path = dir.join(format!("{}-build", prog_name)).join("manifest.json");
        if !manifest_path.is_file() {
            continue;
        }
        found_any = true;
        check_one_manifest(c, &manifest_path, &prog_name, expected_version);
    }

    if !found_any {
        c.warn("No program manifests found in exe/");
    }
}

fn check_one_manifest(
    c: &mut Counts,
    path: &Path,
    prog_name: &str,
    expected_version: Option<&Version>,
) {
    let content = match fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            c.fail(&format!("{prog_name} -- cannot read manifest: {e}"));
            return;
        }
    };

    let manifest: serde_json::Value = match serde_json::from_str(&content) {
        Ok(v) => v,
        Err(e) => {
            c.fail(&format!("{prog_name} -- invalid manifest JSON: {e}"));
            return;
        }
    };

    let mut issues: Vec<String> = Vec::new();

    // Check morloc_version
    let build_ver = manifest
        .get("build")
        .and_then(|b| b.get("morloc_version"))
        .and_then(|v| v.as_str());

    if let (Some(build_ver_str), Some(expected)) = (build_ver, expected_version) {
        let expected_str = expected.show();
        if build_ver_str != expected_str {
            issues.push(format!(
                "built with {build_ver_str}, expected {expected_str}"
            ));
        }
    }

    // The manifest is self-describing: its pools live in a sibling pools/
    // directory (pool exec paths are relative to the manifest's own dir).
    let manifest_dir = path.parent().unwrap_or_else(|| Path::new("."));
    if !manifest_dir.join("pools").is_dir() {
        issues.push("pools/ directory missing".to_string());
    }

    // Check pool files exist
    let pool_count = manifest
        .get("pools")
        .and_then(|p| p.as_array())
        .map(|a| a.len())
        .unwrap_or(0);

    if pool_count == 0 {
        issues.push("no pools defined".to_string());
    }

    if issues.is_empty() {
        let ver_str = build_ver.unwrap_or("unknown");
        c.pass(&format!(
            "{prog_name} -- built with {ver_str}, {pool_count} pools"
        ));
    } else {
        for issue in &issues {
            c.warn(&format!(
                "{prog_name} -- {issue}\n       \
                 Recompile: morloc-manager run -- morloc make --install"
            ));
        }
    }
}

fn check_morloc_version(c: &mut Counts, engine: ContainerEngine, ec: &EnvironmentConfig) {
    let image = ec.active_image();
    match environment::detect_morloc_version(engine, image) {
        Ok(detected) => {
            if let Some(ref expected) = ec.morloc_version {
                if detected == *expected {
                    c.pass(&format!("morloc {} (matches config)", detected.show()));
                } else {
                    c.warn(&format!(
                        "morloc {} in container, config says {}",
                        detected.show(),
                        expected.show()
                    ));
                }
            } else {
                c.pass(&format!("morloc {} (no version in config to compare)", detected.show()));
            }
        }
        Err(e) => {
            c.fail(&format!("Cannot run morloc in container: {e}"));
        }
    }
}

fn check_programs_deep(
    c: &mut Counts,
    engine: ContainerEngine,
    verbose: bool,
    ec: &EnvironmentConfig,
    data_dir: &Path,
) {
    let image = ec.active_image();
    let mh = crate::serve::CONTAINER_MORLOC_HOME;
    let state = crate::serve::CONTAINER_MORLOC_STATE;
    // Mount the host env dir at MORLOC_STATE (mutable), not over the baked runtime.
    let bind_mounts = vec![(data_dir.to_string_lossy().to_string(), state.to_string())];
    let env = vec![
        ("MORLOC_HOME".to_string(), mh.to_string()),
        ("MORLOC_STATE".to_string(), state.to_string()),
    ];

    // Scan programs from exe/ (one exe/<name>/ subdir per program) under state.
    let exe_dir = format!("{state}/exe");
    let cfg = RunConfig {
        command: Some(vec!["ls".to_string(), exe_dir.clone()]),
        bind_mounts: bind_mounts.clone(),
        env: env.clone(),
        ..RunConfig::new(image)
    };
    let (status, stdout, _) = container_run_quiet(engine, &cfg);
    if !status.success() {
        c.fail("Cannot list programs in container");
        return;
    }

    let programs: Vec<ProgramEntry> = stdout
        .lines()
        .map(|l| l.trim())
        .filter(|l| !l.is_empty())
        .map(|l| ProgramEntry {
            name: l.to_string(),
            commands: Vec::new(),
        })
        .collect();

    if programs.is_empty() {
        c.warn("No programs found in container");
        return;
    }

    if !c.json_mode {
        println!("Running smoke tests for {} programs...", programs.len());
    }
    for prog in &programs {
        let exe_path = format!("{mh}/bin/{}", prog.name);
        let cfg = RunConfig {
            command: Some(vec![exe_path.clone(), "--help".to_string()]),
            bind_mounts: bind_mounts.clone(),
            env: env.clone(),
            ..RunConfig::new(image)
        };
        if verbose {
            let exe = engine_executable(engine);
            eprintln!("[morloc-manager] {exe} run --rm {image} {exe_path} --help");
        }
        let (status, _, stderr) = container_run_quiet(engine, &cfg);
        if status.success() {
            c.pass(&format!("{} -- smoke test passed", prog.name));
        } else {
            let snippet: String = stderr.lines().take(3).collect::<Vec<_>>().join("\n       ");
            c.fail(&format!("{} -- smoke test failed: {snippet}", prog.name));
        }
    }
}

// ======================================================================
// SLURM-bridge prerequisites
// ======================================================================

/// Checks for a containerized SLURM dispatch workflow. Verifies the
/// host-side prerequisites that `morloc-manager run --slurm-bridge`
/// will rely on: cluster client tools, codegen toggle in the build
/// config, env image resolvability, NFS-shared morloc-manager binary
/// path, and a writable runtime dir for the bridge UDS.
fn check_slurm_prereqs(c: &mut Counts, engine: ContainerEngine, ec: &EnvironmentConfig, scope: Scope, env_name: &str) {
    fn which(name: &str) -> bool {
        Command::new(name).arg("--version").output().is_ok()
    }

    // 1. sbatch on PATH.
    if which("sbatch") {
        c.pass("sbatch on host PATH");
    } else {
        c.fail("sbatch not on host PATH (install slurm-client or load the slurm module)");
    }

    // 2. sacct on PATH.
    if which("sacct") {
        c.pass("sacct on host PATH");
    } else {
        c.fail("sacct not on host PATH (install slurm-client or load the slurm module)");
    }

    // 3. build.yaml has slurm-support: true.
    let build_yaml = cfg::config_dir(Scope::Local).join("build.yaml");
    match fs::read_to_string(&build_yaml) {
        Ok(content) => {
            // Lightweight grep; a YAML parse here would duplicate
            // Config.hs's logic for a one-line key.
            if content.lines().any(|l| {
                let t = l.trim();
                t == "slurm-support: true" || t == "slurm-support:true"
            }) {
                c.pass(&format!("{} has slurm-support: true", build_yaml.display()));
            } else {
                c.fail(&format!(
                    "{} missing `slurm-support: true` (run `morloc-manager update --reinit --init-arg --slurm`, or `morloc-manager run -- morloc init -f --slurm`)",
                    build_yaml.display()
                ));
            }
        }
        Err(_) => {
            c.fail(&format!(
                "{} does not exist; run `morloc-manager update --reinit --init-arg --slurm`",
                build_yaml.display()
            ));
        }
    }

    // 4. Env image is resolvable.
    let image = ec.active_image();
    if image.is_empty() {
        c.fail("env has no resolvable image (run `morloc-manager update`)");
    } else {
        c.pass(&format!("env image: {}", image));
    }

    // 5. Engine softly recommended to be Apptainer.
    if engine != ContainerEngine::Apptainer {
        c.warn(&format!(
            "engine is {:?}; non-Apptainer engines require image distribution to every compute node (registry pull or pre-populated store)",
            engine
        ));
    } else {
        c.pass("engine is Apptainer (image is a shared-FS file)");
    }

    // 6. morloc-manager binary path looks NFS-mirrorable. Heuristic:
    //    must live under $HOME (the typical NFS-shared root).
    match (std::env::current_exe(), dirs::home_dir()) {
        (Ok(exe), Some(home)) => {
            if exe.starts_with(&home) {
                c.pass(&format!(
                    "morloc-manager binary is under $HOME ({})",
                    exe.display()
                ));
            } else {
                c.warn(&format!(
                    "morloc-manager binary at {} is not under $HOME; compute nodes may not be able to exec the same absolute path. Install under ~/.local/bin or another NFS-shared location.",
                    exe.display()
                ));
            }
        }
        _ => {
            c.warn("could not resolve morloc-manager's own absolute path; --slurm-bridge may fail to compose a working sbatch wrap");
        }
    }

    // 6b. The env config must be resolvable at the SAME path on every compute
    //     node. The wrap pins `run --env <name>`, so each worker re-reads this
    //     env's config; if it lives outside the NFS-shared $HOME (e.g. a
    //     system-scope env under /etc/morloc), workers may not find it.
    let env_cfg = cfg::env_config_path(scope, env_name);
    match dirs::home_dir() {
        Some(home) if env_cfg.starts_with(&home) => {
            c.pass(&format!("env config is under $HOME ({})", env_cfg.display()));
        }
        Some(_) => {
            c.warn(&format!(
                "env '{env_name}' config at {} is not under $HOME; compute nodes may not resolve `--env {env_name}`. Use a local (per-user) env on NFS-shared $HOME for the SLURM bridge.",
                env_cfg.display()
            ));
        }
        None => {
            c.warn("could not resolve $HOME to check the env config is NFS-mirrorable for compute nodes");
        }
    }

    // 7. Bridge UDS dir is writable.
    let runtime_dir = std::env::var("XDG_RUNTIME_DIR")
        .ok()
        .filter(|s| !s.is_empty())
        .unwrap_or_else(|| "/tmp".to_string());
    let probe = std::path::PathBuf::from(&runtime_dir).join(".morloc-doctor-probe");
    match fs::write(&probe, b"") {
        Ok(_) => {
            let _ = fs::remove_file(&probe);
            c.pass(&format!("bridge UDS dir writable: {}", runtime_dir));
        }
        Err(e) => {
            c.fail(&format!(
                "bridge UDS dir {} not writable ({}); set XDG_RUNTIME_DIR or fix /tmp",
                runtime_dir, e
            ));
        }
    }
}

#[cfg(test)]
mod native_health_tests {
    use super::*;

    fn rt_with(env: &[(&str, &str)]) -> NativeRuntime {
        NativeRuntime {
            activation_env: env.iter().map(|(k, v)| (k.to_string(), v.to_string())).collect(),
            manager_version: None,
        }
    }

    #[test]
    fn activation_completeness_flags_missing_ar() {
        // All compiler vars present (no CONDA_PREFIX -> tool-existence sub-check
        // is skipped) -> no failure.
        let full = rt_with(&[
            ("CC", "x-gcc"),
            ("CXX", "x-g++"),
            ("AR", "x-ar"),
            ("RANLIB", "x-ranlib"),
        ]);
        let mut c = Counts::new(true);
        check_activation_toolchain(&mut c, &full);
        assert_eq!(c.fail, 0, "all compiler vars present -> no failure");

        // A captured activation that dropped $AR must FAIL.
        let no_ar = rt_with(&[("CC", "x-gcc"), ("CXX", "x-g++"), ("RANLIB", "x-ranlib")]);
        let mut c2 = Counts::new(true);
        check_activation_toolchain(&mut c2, &no_ar);
        assert!(c2.fail >= 1, "missing $AR must fail");
    }

    #[test]
    fn runtime_artifacts_inventory_names_missing() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        std::fs::create_dir_all(root.join("include/mlccpptypes")).unwrap();
        std::fs::create_dir_all(root.join("lib")).unwrap();
        std::fs::write(root.join("include/mlccpptypes/x.hpp"), "").unwrap();
        std::fs::write(root.join("lib/libcppmorloc.a"), "").unwrap();
        std::fs::write(root.join("include/cppmorloc.hpp"), "").unwrap();

        // Complete cpp stack (cpp is always checked) -> pass.
        let mut c = Counts::new(true);
        check_runtime_artifacts(&mut c, root, &[]);
        assert_eq!(c.fail, 0, "complete cpp stack passes");

        // Remove one artifact -> the stack fails and names it.
        std::fs::remove_file(root.join("lib/libcppmorloc.a")).unwrap();
        let mut c2 = Counts::new(true);
        check_runtime_artifacts(&mut c2, root, &[]);
        assert_eq!(c2.fail, 1, "missing cpp artifact fails");
    }

    #[test]
    fn detect_languages_reads_fresh_lock() {
        let dir = tempfile::tempdir().unwrap();
        let pixi = dir.path();
        // No pixi.toml -> fresh = lock present. Single platform -> the
        // sole-platform fallback resolves regardless of the host arch.
        let lock = "environments:\n  default:\n    packages:\n      linux-64:\n      \
                    - conda: https://conda.anaconda.org/conda-forge/linux-64/python-3.12.4-h0_0.conda\n      \
                    - conda: https://conda.anaconda.org/conda-forge/linux-64/rust-1.83.0-h0_0.conda\n";
        std::fs::write(pixi.join("pixi.lock"), lock).unwrap();
        let (langs, fresh) = detect_languages(pixi);
        assert!(fresh);
        assert!(langs.contains(&"python".to_string()));
        assert!(langs.contains(&"rust".to_string()));
        assert!(!langs.contains(&"r".to_string()));
    }
}
