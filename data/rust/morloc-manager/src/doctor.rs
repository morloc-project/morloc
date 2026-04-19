use std::fs;
use std::path::Path;
use std::process::Command;

use crate::config as cfg;
use crate::container::{container_run_quiet, engine_executable, image_exists_locally, RunConfig};
use crate::environment;
use crate::error::Result;
use crate::types::*;

const MANIFEST_MARKER: &str = "### MANIFEST ###";

struct Counts {
    ok: u32,
    warn: u32,
    fail: u32,
}

impl Counts {
    fn new() -> Self {
        Self { ok: 0, warn: 0, fail: 0 }
    }

    fn pass(&mut self, msg: &str) {
        self.ok += 1;
        println!("  [ok] {msg}");
    }

    fn warn(&mut self, msg: &str) {
        self.warn += 1;
        println!("  [!!] {msg}");
    }

    fn fail(&mut self, msg: &str) {
        self.fail += 1;
        println!("  [EE] {msg}");
    }

    fn skip(&self, msg: &str) {
        println!("  [--] {msg}");
    }
}

pub fn doctor(
    engine: ContainerEngine,
    verbose: bool,
    env_name: &str,
    scope: Scope,
    ec: &EnvironmentConfig,
    deep: bool,
    strict: bool,
) -> Result<()> {
    let scope_str = match scope {
        Scope::Local => "local",
        Scope::System => "system",
    };
    let engine_str = match engine {
        ContainerEngine::Docker => "docker",
        ContainerEngine::Podman => "podman",
    };

    println!("Environment: {env_name} ({scope_str})");
    println!("Engine:      {engine_str}");
    println!();

    let mut c = Counts::new();
    let data_dir = cfg::env_data_dir(scope, env_name);

    // ==== Prerequisites ====
    println!("Prerequisites");
    check_engine(&mut c, engine);
    check_base_image(&mut c, engine, &ec.base_image);
    check_built_image(&mut c, engine, ec, scope, env_name);
    check_data_dirs(&mut c, &data_dir);

    // ==== Manifests ====
    println!("\nManifests");
    check_manifests(&mut c, &data_dir, ec.morloc_version.as_ref());

    // ==== Deep checks ====
    if deep {
        println!("\nDeep checks");
        check_morloc_version(&mut c, engine, ec);
        check_programs_deep(&mut c, engine, verbose, ec, &data_dir);
    } else {
        println!("\nDeep checks");
        c.skip("Use --deep to run container-side checks");
    }

    // ==== Summary ====
    println!();
    println!(
        "{} passed, {} warnings, {} errors",
        c.ok, c.warn, c.fail
    );

    if c.fail > 0 {
        return Err(crate::error::ManagerError::DoctorFailed(c.fail));
    }
    if strict && c.warn > 0 {
        return Err(crate::error::ManagerError::DoctorFailed(c.warn));
    }
    Ok(())
}

// ======================================================================
// Individual checks
// ======================================================================

fn check_engine(c: &mut Counts, engine: ContainerEngine) {
    let exe = engine_executable(engine);
    let fmt = match engine {
        ContainerEngine::Podman => "{{.Version.Version}}",
        ContainerEngine::Docker => "{{.ServerVersion}}",
    };
    let output = Command::new(exe)
        .args(["info", "--format", fmt])
        .output();
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

fn check_manifests(
    c: &mut Counts,
    data_dir: &Path,
    expected_version: Option<&Version>,
) {
    let fdb_dir = data_dir.join("fdb");
    if !fdb_dir.is_dir() {
        c.warn("No fdb/ directory found");
        return;
    }

    let entries = match fs::read_dir(&fdb_dir) {
        Ok(e) => e,
        Err(e) => {
            c.fail(&format!("Cannot read fdb/: {e}"));
            return;
        }
    };

    let mut found_any = false;
    for entry in entries.flatten() {
        let name = entry.file_name();
        let name_str = name.to_string_lossy();
        if !name_str.ends_with(".manifest") {
            continue;
        }
        found_any = true;
        let prog_name = &name_str[..name_str.len() - ".manifest".len()];
        check_one_manifest(c, &entry.path(), prog_name, data_dir, expected_version);
    }

    if !found_any {
        c.warn("No program manifests found in fdb/");
    }
}

fn check_one_manifest(
    c: &mut Counts,
    path: &Path,
    prog_name: &str,
    data_dir: &Path,
    expected_version: Option<&Version>,
) {
    let content = match fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            c.fail(&format!("{prog_name} -- cannot read manifest: {e}"));
            return;
        }
    };

    let json_str = if content.starts_with("#!") {
        if let Some(marker_pos) = content.find(MANIFEST_MARKER) {
            let after_marker = &content[marker_pos..];
            let json_start = after_marker
                .find('\n')
                .map(|i| marker_pos + i + 1)
                .unwrap_or(content.len());
            &content[json_start..]
        } else {
            c.fail(&format!("{prog_name} -- manifest missing ### MANIFEST ### marker"));
            return;
        }
    } else {
        content.as_str()
    };

    let manifest: serde_json::Value = match serde_json::from_str(json_str) {
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

    // Check build.path exists
    let build_path = manifest
        .get("build")
        .and_then(|b| b.get("path"))
        .and_then(|v| v.as_str());

    if let Some(bp) = build_path {
        // Build paths inside containers are /opt/morloc/exe/..., on host they're
        // under data_dir/exe/... Try the host path first.
        let host_path = data_dir.join("exe").join(prog_name);
        if !host_path.is_dir() && !Path::new(bp).is_dir() {
            issues.push("build directory missing".to_string());
        }
    } else {
        issues.push("no build.path in manifest".to_string());
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
    let mh = "/opt/morloc";
    let bind_mounts = vec![(data_dir.to_string_lossy().to_string(), mh.to_string())];
    let env = vec![
        ("MORLOC_HOME".to_string(), mh.to_string()),
    ];

    // Scan programs from fdb/ to get program names
    let fdb_dir = format!("{mh}/fdb");
    let cfg = RunConfig {
        command: Some(vec!["ls".to_string(), fdb_dir.clone()]),
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
        .filter(|l| l.ends_with(".manifest"))
        .map(|l| {
            let name = l.strip_suffix(".manifest").unwrap_or(l);
            ProgramEntry {
                name: name.to_string(),
                commands: Vec::new(),
            }
        })
        .collect();

    if programs.is_empty() {
        c.warn("No programs found in container");
        return;
    }

    eprintln!("Running smoke tests for {} programs...", programs.len());
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
