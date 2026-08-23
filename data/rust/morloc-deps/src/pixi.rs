//! Lower a set of `EnvSpec`s to a pixi (conda) project manifest (`pixi.toml`).
//!
//! An environment's pixi manifest is the union of the language toolchains and
//! native packages required by every program installed into it. This module
//! only *generates* the manifest; the pixi solve (run elsewhere) is what decides
//! purity -- whether every compiled dependency resolves from one coherent world.
//!
//! Mapping decisions:
//!   * Language toolchains -> conda `[dependencies]` (py->python, cpp->cxx-compiler,
//!     rust->rust, r->r-base, julia->julia, futhark->futhark).
//!   * Python packages: `source` (pure wheels) -> `[pypi-dependencies]` (uv);
//!     `abi`/`unknown` -> conda `[dependencies]`.
//!   * R packages -> conda `r-<name>` (conda-forge R feedstocks).
//!   * C++ packages and system libs -> conda `[dependencies]`.
//!   * Rust and Julia packages are EXCLUDED: cargo (crates.io) and Pkg.jl own
//!     those, resolved in-world when the pool is built. Only their toolchains
//!     appear here.
//!   * Package names are used as-is (no conda-name-mapping table yet); an
//!     unresolvable name surfaces at solve time as an impurity/escalation.

use std::collections::BTreeMap;
use std::path::Path;
use std::process::{Command, Stdio};

use crate::envspec::{DepSource, EnvSpec};
use crate::error::{DepsError, Result};
use crate::langsupport::LangSupport;

/// The universal default conda channel and highest strict-priority base. An
/// omitted per-dep channel means this; it is the sole entry in `default_channels`.
const CONDA_FORGE: &str = "conda-forge";

/// Inputs for rendering an environment's pixi manifest.
pub struct PixiManifestInput<'a> {
    pub env_name: &'a str,
    /// conda platform: "linux-64" | "osx-64" | "osx-arm64".
    pub platform: &'a str,
    /// conda channels, in priority order (typically just ["conda-forge"]).
    pub channels: &'a [String],
    /// The union of requirements of every program installed into this env.
    pub specs: &'a [EnvSpec],
    /// morloc's own language-support table (`morloc lang-support`): the core
    /// toolchain, per-language binder requirements, and supported runtime version
    /// ranges. Injected into, and clamps, the manifest.
    pub lang_support: &'a LangSupport,
}

/// Toolchain package for a language morloc has NO requirements.yaml entry for
/// (julia) -- the escape hatch until it gains a table entry. Languages in the
/// support table get their toolchain from `runtime.package` and `requires`
/// instead; those are NOT duplicated here (a second copy would silently diverge).
/// Script-provisioned languages (futhark) are NOT here: they carry an empty
/// support entry (with an `install_script`) and are installed outside conda.
fn lang_toolchain(lang: &str) -> Option<&'static str> {
    match lang {
        "julia" => Some("julia"),
        _ => None,
    }
}

/// The conda package name for a dependency. On conda-forge (the default channel)
/// an R package is a `r-<lowercase>` feedstock; on any other channel the author
/// writes the exact conda name (e.g. `bioconductor-deseq2`), so it passes through
/// literally. Non-R packages always use the name as written.
fn conda_name(channel: Option<&str>, lang: &str, pkg: &str) -> String {
    let is_conda_forge = matches!(channel, None | Some(CONDA_FORGE));
    if lang == "r" && is_conda_forge {
        format!("r-{}", pkg.to_lowercase())
    } else {
        pkg.to_string()
    }
}

/// Merge two channels for the same conda package. A channel is a provenance
/// choice and is NOT intersectable: an explicit channel beats `None` (an unstated
/// conda-forge default), equal channels coalesce, but two DIFFERENT explicit
/// channels are a hard conflict (naming the package).
fn merge_channel(name: &str, a: Option<&str>, b: Option<&str>) -> Result<Option<String>> {
    match (a, b) {
        (None, x) | (x, None) => Ok(x.map(str::to_string)),
        (Some(x), Some(y)) if x == y => Ok(Some(x.to_string())),
        (Some(x), Some(y)) => Err(DepsError::Env(format!(
            "conflicting conda channels for dependency '{name}': '{x}' vs '{y}'. \
             A package can be drawn from only one channel across all programs in an \
             environment."
        ))),
    }
}

/// Combine two version constraints for the same package. conda match-specs use
/// `,` as AND, so we simply intersect. `*` (any) yields to a real constraint.
fn merge_constraint(existing: &str, incoming: &str) -> String {
    if existing == incoming || incoming == "*" {
        existing.to_string()
    } else if existing == "*" {
        incoming.to_string()
    } else {
        // Deduplicate the comma-separated atoms, preserving order.
        let mut atoms: Vec<&str> = Vec::new();
        for atom in existing.split(',').chain(incoming.split(',')) {
            let a = atom.trim();
            if !a.is_empty() && !atoms.contains(&a) {
                atoms.push(a);
            }
        }
        atoms.join(",")
    }
}

fn insert_merged(map: &mut BTreeMap<String, String>, name: &str, constraint: &str) {
    map.entry(name.to_string())
        .and_modify(|c| *c = merge_constraint(c, constraint))
        .or_insert_with(|| constraint.to_string());
}

/// Insert or merge a conda dependency (constraint + channel). Constraints
/// intersect (`merge_constraint`); channels merge under `merge_channel`, which
/// errors on two different explicit channels for one package.
fn insert_merged_conda(
    map: &mut BTreeMap<String, (String, Option<String>)>,
    name: &str,
    constraint: &str,
    channel: Option<&str>,
) -> Result<()> {
    use std::collections::btree_map::Entry;
    match map.entry(name.to_string()) {
        Entry::Vacant(v) => {
            v.insert((constraint.to_string(), channel.map(str::to_string)));
        }
        Entry::Occupied(mut o) => {
            let (c, existing) = o.get_mut();
            *c = merge_constraint(c, constraint);
            *existing = merge_channel(name, existing.as_deref(), channel)?;
        }
    }
    Ok(())
}

/// Aggregate all specs into (conda dependencies, pypi dependencies) maps,
/// clamped and injected against morloc's language-support table. Every injected
/// dependency (toolchain, runtime, binder, system lib) rides on conda-forge
/// (channel `None`); only a program's own conda package may carry an explicit
/// channel. Fails on a cross-program channel conflict for one package.
fn aggregate(
    specs: &[EnvSpec],
    support: &LangSupport,
) -> Result<(BTreeMap<String, (String, Option<String>)>, BTreeMap<String, String>)> {
    let mut conda: BTreeMap<String, (String, Option<String>)> = BTreeMap::new();
    let mut pypi: BTreeMap<String, String> = BTreeMap::new();

    // Core toolchain: always required (libmorloc + any shim). Non-optional only.
    for p in &support.toolchain {
        if !p.optional {
            insert_merged_conda(&mut conda, &p.package, &p.constraint, None)?;
        }
    }

    // Merge each language's author version constraint across all programs.
    let mut lang_author: BTreeMap<String, String> = BTreeMap::new();
    for spec in specs {
        for lang in &spec.languages {
            let c = lang.constraint.as_deref().unwrap_or("*");
            lang_author
                .entry(lang.lang.clone())
                .and_modify(|e| *e = merge_constraint(e, c))
                .or_insert_with(|| c.to_string());
        }
    }

    // For each language a program uses: clamp its runtime version against
    // morloc's supported range and inject morloc's binder requirements. A
    // language morloc has no support entry for (e.g. futhark) falls back to the
    // toolchain map with only the author's constraint.
    for (lang, author_c) in &lang_author {
        match support.languages.get(lang) {
            Some(entry) => {
                if let Some(rt) = &entry.runtime {
                    // clamp: morloc's supported range intersected with the author's
                    insert_merged_conda(&mut conda, &rt.package, &merge_constraint(&rt.version, author_c), None)?;
                }
                for p in &entry.requires {
                    if !p.optional {
                        insert_merged_conda(&mut conda, &p.package, &p.constraint, None)?;
                    }
                }
            }
            None => {
                if let Some(tc) = lang_toolchain(lang) {
                    insert_merged_conda(&mut conda, tc, author_c, None)?;
                }
            }
        }
    }

    for spec in specs {
        for (lang, reqs) in &spec.packages {
            for r in reqs {
                match r.source {
                    // R conda feedstocks are named r-<lowercase> ONLY on
                    // conda-forge; on another channel the author's exact name is
                    // used. Other conda packages use the name as written.
                    DepSource::Conda => {
                        let channel = r.channel.as_deref();
                        let name = conda_name(channel, lang.as_str(), &r.name);
                        insert_merged_conda(&mut conda, &name, &r.constraint, channel)?;
                    }
                    DepSource::Pypi => insert_merged(&mut pypi, &r.name, &r.constraint),
                    // cargo and Pkg.jl own these; resolved in-world at pool build.
                    DepSource::Crates | DepSource::Pkg => {}
                    // Not yet honored natively; the compiler rejects these
                    // sources at build time, so reaching here means the
                    // compiler/manager contract was violated. Fail loud rather
                    // than silently dropping the dependency.
                    DepSource::Cran | DepSource::Bioconductor => unreachable!(
                        "dependency '{}' has source {:?}, which the compiler must reject \
                         until the R pool supports it",
                        r.name, r.source
                    ),
                }
            }
        }

        // System libs: host-provided ones are intentionally left out (they mark
        // impurity, which the solve/escalation handles); everything else is a
        // conda dependency.
        for s in &spec.system {
            if s.provider != "host" {
                insert_merged_conda(&mut conda, &s.name, "*", None)?;
            }
        }
    }

    Ok((conda, pypi))
}

/// A TOML key, quoted (package names may contain `.`, which is illegal bare).
fn key(name: &str) -> String {
    format!("\"{name}\"")
}

/// The backend-neutral, structured requirement set for an environment: the union
/// of every gathered `EnvSpec` intersected with the language-support table and
/// user pins, resolved to concrete package requirements. This is the IR between
/// requirement gathering (main.rs) and backend materialization (today: pixi). It
/// replaces passing rendered `pixi.toml` *text* around, so the coherence key and
/// any future backend consume structured data rather than re-parsing a manifest.
#[derive(Debug, Clone)]
pub struct RequirementSet {
    pub env_name: String,
    /// conda platform string ("linux-64" | "osx-arm64" | ...).
    pub platform: String,
    pub channels: Vec<String>,
    /// conda `[dependencies]`: package -> (version match-spec, optional channel),
    /// sorted. A `Some` channel is a non-conda-forge subordinate channel and
    /// lowers to a pixi inline table; `None` is conda-forge (flat form).
    pub conda: BTreeMap<String, (String, Option<String>)>,
    /// pypi `[pypi-dependencies]`: package -> version (sorted).
    pub pypi: BTreeMap<String, String>,
}

/// The default conda channel list for morloc environments (one policy home for
/// both the manager and the in-env agent).
pub fn default_channels() -> Vec<String> {
    vec![CONDA_FORGE.to_string()]
}

/// Resolve gathered specs + morloc's language-support table into a structured
/// `RequirementSet` (aggregate/clamp/inject, bucketed into conda and pypi). The
/// morloc version is not carried here -- it lives on `ResolvedRequirements`
/// alongside this IR, where the coherence key reads it.
pub fn resolve_requirements(input: &PixiManifestInput) -> Result<RequirementSet> {
    let (conda, pypi) = aggregate(input.specs, input.lang_support)?;
    Ok(RequirementSet {
        env_name: input.env_name.to_string(),
        platform: input.platform.to_string(),
        channels: input.channels.to_vec(),
        conda,
        pypi,
    })
}

/// Render the pixi manifest text from a resolved `RequirementSet`. Deterministic
/// (the dependency maps are sorted). This is the pixi *lowering* of the IR.
pub fn render_manifest(req: &RequirementSet) -> String {
    // Workspace channels: the base channels (conda-forge, always first) followed
    // by every new per-dep channel. pixi/rattler honor a per-dep channel ONLY if
    // it is also a declared workspace channel, so this union is required, not
    // cosmetic. Per-dep channels are never conda-forge (`resolveChannel` strips
    // it), so conda-forge stays at the front from `req.channels`.
    let mut channel_list: Vec<&str> = req.channels.iter().map(String::as_str).collect();
    for ch in req.conda.values().filter_map(|(_, c)| c.as_deref()) {
        if !channel_list.contains(&ch) {
            channel_list.push(ch);
        }
    }
    let channels = channel_list
        .iter()
        .map(|c| format!("\"{c}\""))
        .collect::<Vec<_>>()
        .join(", ");

    let mut out = String::new();
    out.push_str("[workspace]\n");
    out.push_str(&format!("name = \"{}\"\n", req.env_name));
    out.push_str(&format!("channels = [{channels}]\n"));
    out.push_str(&format!("platforms = [\"{}\"]\n", req.platform));

    out.push_str("\n[dependencies]\n");
    for (name, (constraint, channel)) in &req.conda {
        match channel {
            // A non-conda-forge channel lowers to a pixi inline table so the dep
            // is pinned to that channel; conda-forge (None) keeps the flat form.
            Some(ch) => out.push_str(&format!(
                "{} = {{ version = \"{}\", channel = \"{}\" }}\n",
                key(name),
                constraint,
                ch
            )),
            None => out.push_str(&format!("{} = \"{}\"\n", key(name), constraint)),
        }
    }

    if !req.pypi.is_empty() {
        out.push_str("\n[pypi-dependencies]\n");
        for (name, constraint) in &req.pypi {
            out.push_str(&format!("{} = \"{}\"\n", key(name), constraint));
        }
    }

    out
}

// ======================================================================
// Solve driver (shells out to a pinned pixi binary)
// ======================================================================

/// Write the rendered manifest to `<env_dir>/pixi.toml`.
pub fn write_manifest(env_dir: &Path, manifest: &str) -> Result<()> {
    std::fs::create_dir_all(env_dir)
        .map_err(|e| DepsError::Env(format!("cannot create {}: {e}", env_dir.display())))?;
    let path = env_dir.join("pixi.toml");
    std::fs::write(&path, manifest)
        .map_err(|e| DepsError::Env(format!("cannot write {}: {e}", path.display())))
}

/// Solve + install the manifest in `env_dir` using `pixi_bin`. This is phase 2
/// of the impurity gate: an unresolvable package (one conda-forge cannot
/// provide for this platform) fails here, which is the accurate verdict that a
/// native build is impossible -- the caller escalates to a container.
pub fn solve(env_dir: &Path, pixi_bin: &Path) -> Result<()> {
    let manifest = env_dir.join("pixi.toml");
    let status = Command::new(pixi_bin)
        .arg("install")
        .arg("--manifest-path")
        .arg(&manifest)
        .stdin(Stdio::null())
        .status()
        .map_err(|e| DepsError::Env(format!("could not run pixi ({}): {e}", pixi_bin.display())))?;
    if !status.success() {
        return Err(DepsError::Env(
            "pixi could not solve this environment: a required package is unavailable on \
             conda-forge for this platform. The native backend can only provide what conda \
             offers; rebuild with a container backend (--engine podman) for host or system \
             dependencies."
                .to_string(),
        ));
    }
    Ok(())
}

/// Produce the `pixi.lock` for the manifest in `env_dir` without installing on
/// the host. The lock is what a container image reproduces with `pixi install
/// --locked`, so the container's conda world is pinned to the same solve. A
/// failure here is phase 2 of the impurity gate (a package conda cannot provide).
pub fn lock(env_dir: &Path, pixi_bin: &Path) -> Result<()> {
    let manifest = env_dir.join("pixi.toml");
    let status = Command::new(pixi_bin)
        .arg("lock")
        .arg("--manifest-path")
        .arg(&manifest)
        .stdin(Stdio::null())
        .status()
        .map_err(|e| DepsError::Env(format!("could not run pixi ({}): {e}", pixi_bin.display())))?;
    if !status.success() {
        return Err(DepsError::Env(
            "pixi could not lock this environment: a required package is unavailable on \
             conda-forge for this platform (see the solver output above)."
                .to_string(),
        ));
    }
    Ok(())
}

/// A package installed in the environment's solved world, read from `pixi.lock`.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize)]
pub struct LockedPackage {
    pub name: String,
    pub version: String,
    /// Provider: "conda" or "pypi".
    pub kind: String,
}

/// The packages recorded in `<pixi_dir>/pixi.lock` for `platform` -- the env's
/// actual solved world, sorted by name. Returns an empty vec if the lock is
/// absent (the environment has not been solved yet).
///
/// The lock references each package only by artifact URL under
/// `environments.<env>.packages.<platform>`, with no separate name/version
/// fields, so both are parsed from the artifact filename.
pub fn locked_packages(pixi_dir: &Path, platform: &str) -> Result<Vec<LockedPackage>> {
    let lock_path = pixi_dir.join("pixi.lock");
    let text = match std::fs::read_to_string(&lock_path) {
        Ok(t) => t,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(Vec::new()),
        Err(e) => {
            return Err(DepsError::Env(format!(
                "cannot read {}: {e}",
                lock_path.display()
            )))
        }
    };
    parse_locked_packages(&text, platform)
}

#[derive(serde::Deserialize)]
struct Lock {
    #[serde(default)]
    environments: std::collections::BTreeMap<String, LockEnv>,
}

#[derive(serde::Deserialize)]
struct LockEnv {
    #[serde(default)]
    packages: std::collections::BTreeMap<String, Vec<PackageRef>>,
}

#[derive(serde::Deserialize)]
struct PackageRef {
    #[serde(default)]
    conda: Option<String>,
    #[serde(default)]
    pypi: Option<String>,
}

/// Parse pixi.lock text into the sorted package list for `platform`. Prefers the
/// `default` environment and the requested platform, falling back to the sole
/// entry when there is exactly one (so a single-env/single-platform lock always
/// resolves even if names drift).
fn parse_locked_packages(text: &str, platform: &str) -> Result<Vec<LockedPackage>> {
    let lock: Lock = serde_yaml::from_str(text)
        .map_err(|e| DepsError::Env(format!("cannot parse pixi.lock: {e}")))?;
    let env = lock.environments.get("default").or_else(|| {
        (lock.environments.len() == 1).then(|| lock.environments.values().next().unwrap())
    });
    let Some(env) = env else {
        return Ok(Vec::new());
    };
    let refs = env.packages.get(platform).or_else(|| {
        (env.packages.len() == 1).then(|| env.packages.values().next().unwrap())
    });
    let Some(refs) = refs else {
        return Ok(Vec::new());
    };
    let mut pkgs: Vec<LockedPackage> = refs
        .iter()
        .filter_map(|r| {
            if let Some(url) = &r.conda {
                parse_conda_artifact(url)
                    .map(|(name, version)| LockedPackage { name, version, kind: "conda".to_string() })
            } else if let Some(url) = &r.pypi {
                parse_pypi_artifact(url)
                    .map(|(name, version)| LockedPackage { name, version, kind: "pypi".to_string() })
            } else {
                None
            }
        })
        .collect();
    pkgs.sort_by(|a, b| a.name.cmp(&b.name).then_with(|| a.version.cmp(&b.version)));
    pkgs.dedup();
    Ok(pkgs)
}

/// Parse `name` and `version` from a conda artifact URL. The filename is
/// `<name>-<version>-<build>.conda` (or `.tar.bz2`); split from the RIGHT so a
/// name containing hyphens (e.g. `binutils_impl_linux-64`) stays intact.
fn parse_conda_artifact(url: &str) -> Option<(String, String)> {
    let file = url.rsplit('/').next()?;
    let stem = file
        .strip_suffix(".conda")
        .or_else(|| file.strip_suffix(".tar.bz2"))?;
    let mut it = stem.rsplitn(3, '-');
    let _build = it.next()?;
    let version = it.next()?;
    let name = it.next()?;
    if name.is_empty() || version.is_empty() {
        None
    } else {
        Some((name.to_string(), version.to_string()))
    }
}

/// Parse `name` and `version` from a pypi artifact URL. A wheel filename is
/// `<name>-<version>-...whl` (the distribution name is normalized to contain no
/// hyphen), so split from the LEFT.
fn parse_pypi_artifact(url: &str) -> Option<(String, String)> {
    let file = url.rsplit('/').next()?;
    let stem = file
        .strip_suffix(".whl")
        .or_else(|| file.strip_suffix(".tar.gz"))
        .or_else(|| file.strip_suffix(".zip"))
        .unwrap_or(file);
    let mut it = stem.split('-');
    let name = it.next()?;
    let version = it.next()?;
    if name.is_empty() || version.is_empty() {
        None
    } else {
        Some((name.to_string(), version.to_string()))
    }
}

/// The morloc language whose runtime each recognized conda package provides,
/// paired with that package's resolved version, for the packages present in
/// `locked`. The single source of truth for the runtime-package -> language
/// mapping, so `info` (which formats "<lang> <version>") and `doctor` (which
/// wants just the language set) cannot drift apart.
pub fn runtime_languages(locked: &[LockedPackage]) -> Vec<(&'static str, String)> {
    // (conda package name, morloc language)
    const RUNTIMES: &[(&str, &str)] = &[
        ("python", "python"),
        ("rust", "rust"),
        ("r-base", "r"),
        ("julia", "julia"),
    ];
    RUNTIMES
        .iter()
        .filter_map(|(pkg, lang)| {
            locked
                .iter()
                .find(|p| p.name == *pkg)
                .map(|p| (*lang, p.version.clone()))
        })
        .collect()
}

/// Single-quote a string for safe interpolation into a shell command: wrap in
/// single quotes and replace each embedded `'` with `'\''`. Prevents a path
/// containing a quote/space/metachar from breaking or injecting into the script.
fn sh_quote(s: &str) -> String {
    format!("'{}'", s.replace('\'', "'\\''"))
}

/// Capture the toolchain activation env-map from a solved pixi env. The map is
/// what the native Runner injects before spawning a command, and it must carry
/// EVERY variable the conda activation exports -- PATH + CONDA_PREFIX, and
/// (crucially) `$CC`/`$CXX`/`$AR`/`$RANLIB`/`$STRIP`/... set by conda-forge's
/// `c-compiler`/`cxx-compiler` packages. conda ships the binutils/compilers under
/// target-prefixed names (e.g. `x86_64-conda-linux-gnu-ar`), NOT as a bare
/// `ar`/`gcc` on PATH, so those `$AR`/`$CC` entries are what let cargo/cc-rs find
/// the archiver; dropping them breaks C-dependency builds.
///
/// `pixi shell-hook` alone does NOT set those compiler vars -- they are exported
/// by the `$CONDA_PREFIX/etc/conda/activate.d/*.sh` scripts, which the hook does
/// not inline. So the capture activates exactly as the container's
/// `morloc-activate` does (shell-hook THEN source activate.d) and diffs the
/// resulting environment against the base, keeping only what activation added or
/// changed. Statically parsing the hook text would silently drop `$AR` and leave
/// native Rust builds failing with `cc-rs: failed to find tool "ar"` on any host
/// without a bare `ar` on PATH.
pub fn capture_activation(env_dir: &Path, pixi_bin: &Path) -> Result<Vec<(String, String)>> {
    let manifest = env_dir.join("pixi.toml");
    const SPLIT: &str = "__MORLOC_ACTIVATION_SPLIT__";
    // NUL-delimited dump of the exported environment via POSIX awk. `env -0` is a
    // GNU extension (absent on macOS/BSD/busybox); awk's ENVIRON is portable and
    // preserves values that span newlines. Running it before and after activation
    // captures the delta.
    const ENV_DUMP: &str = "awk 'BEGIN{for(k in ENVIRON) printf \"%s=%s%c\", k, ENVIRON[k], 0}'";
    // Interpolated paths are shell-quoted: a single quote or space in the env
    // path must not break the script or inject commands. shell-hook's stdout is
    // captured into `hook` (not eval'd inline) so it cannot pollute the env dump;
    // its failure aborts with the real error surfaced (no `2>/dev/null`/`|| true`),
    // and CONDA_PREFIX is asserted in-shell rather than assumed. Sourced activate.d
    // stdout is sent to stderr so a chatty script cannot corrupt the dump.
    let script = format!(
        "{dump}\n\
         printf '%s\\0' {split}\n\
         hook=$({pixi} shell-hook --manifest-path {manifest} --shell bash) || exit 3\n\
         eval \"$hook\"\n\
         [ -n \"$CONDA_PREFIX\" ] || exit 4\n\
         for f in \"$CONDA_PREFIX/etc/conda/activate.d/\"*.sh; do [ -r \"$f\" ] && . \"$f\" >&2; done\n\
         {dump}",
        dump = ENV_DUMP,
        split = sh_quote(SPLIT),
        pixi = sh_quote(&pixi_bin.display().to_string()),
        manifest = sh_quote(&manifest.display().to_string()),
    );
    let out = Command::new("bash")
        .arg("-c")
        .arg(&script)
        .stdin(Stdio::null())
        .output()
        .map_err(|e| DepsError::Env(format!("could not run activation capture: {e}")))?;
    if !out.status.success() {
        return Err(DepsError::Env(format!(
            "activation capture failed: {}",
            String::from_utf8_lossy(&out.stderr).trim()
        )));
    }
    let stdout = String::from_utf8_lossy(&out.stdout);
    let (base_text, activated_text) = stdout
        .split_once(&format!("{SPLIT}\0"))
        .ok_or_else(|| {
            DepsError::Env("activation capture produced no split marker".to_string())
        })?;
    let activated = parse_env0(activated_text);
    if !activated.iter().any(|(k, _)| k == "CONDA_PREFIX") {
        return Err(DepsError::Env(
            "activation produced no CONDA_PREFIX; cannot activate the native toolchain"
                .to_string(),
        ));
    }
    Ok(activation_delta(&parse_env0(base_text), activated))
}

/// The activation delta: entries the activated environment ADDED or CHANGED
/// relative to the base, so it never pins the caller's unrelated host vars when
/// applied on top of the inherited environ at run time. Pure shell bookkeeping
/// (`_`, `SHLVL`, `PWD`, `OLDPWD`) is dropped. Kept even when identical to the
/// base: the loader/toolchain essentials (`is_forced_keep`) and any var whose
/// value references the conda prefix -- these are conda-set and must survive so a
/// later `run` from a clean shell still has the compiler toolchain.
fn activation_delta(
    base: &[(String, String)],
    activated: Vec<(String, String)>,
) -> Vec<(String, String)> {
    let base_map: BTreeMap<&str, &str> =
        base.iter().map(|(k, v)| (k.as_str(), v.as_str())).collect();
    // The activated conda prefix. Vars whose value references it are conda-set
    // (e.g. CFLAGS=-I$PREFIX/include, CC=$PREFIX/bin/...) and must survive even
    // when identical to the base, otherwise running `update` from an already-
    // activated shell would drop them from the record; `native_run_env` replays
    // that record, so a later `run` from a clean shell would build with no
    // $CC/$AR/$CFLAGS and fail with "failed to find tool ar".
    let prefix = activated
        .iter()
        .find(|(k, _)| k == "CONDA_PREFIX")
        .map(|(_, v)| v.clone())
        .filter(|p| !p.is_empty());
    activated
        .into_iter()
        .filter(|(k, _)| !matches!(k.as_str(), "_" | "SHLVL" | "PWD" | "OLDPWD"))
        .filter(|(k, v)| {
            is_forced_keep(k)
                || prefix.as_deref().map(|p| v.contains(p)).unwrap_or(false)
                || base_map.get(k.as_str()) != Some(&v.as_str())
        })
        .collect()
}

/// Env vars that must survive the activation delta even when identical to the
/// base: the loader essentials (`PATH`/`CONDA_PREFIX`, which downstream steps
/// like `prepend_to_path` require) and the conda compiler/binutils vars that
/// cargo/cc-rs and the C++ pools need (dropping any of these reintroduces the
/// "failed to find tool ar" class of build failure). Conda vars whose VALUE
/// points into the prefix are kept separately (see `activation_delta`), so this
/// list only needs the ones that can be bare names (e.g. `AR`).
fn is_forced_keep(key: &str) -> bool {
    matches!(
        key,
        "PATH" | "CONDA_PREFIX"
            | "CC" | "CXX" | "CPP" | "AR" | "AS" | "LD" | "NM" | "RANLIB"
            | "STRIP" | "OBJCOPY" | "OBJDUMP" | "READELF" | "ADDR2LINE"
            | "CFLAGS" | "CXXFLAGS" | "CPPFLAGS" | "LDFLAGS"
            | "DEBUG_CFLAGS" | "DEBUG_CXXFLAGS" | "CMAKE_ARGS"
            | "CMAKE_PREFIX_PATH" | "PKG_CONFIG_PATH" | "CONDA_BUILD_SYSROOT"
            | "HOST" | "BUILD"
    )
}

/// Parse a NUL-terminated environment dump into a `KEY=VALUE` map. Entries
/// without a `=` or with an empty key are ignored; values may contain `=` and
/// newlines (they are preserved by the NUL delimiter).
pub fn parse_env0(dump: &str) -> Vec<(String, String)> {
    dump.split('\0')
        .filter(|e| !e.is_empty())
        .filter_map(|entry| {
            let (k, v) = entry.split_once('=')?;
            if k.is_empty() {
                None
            } else {
                Some((k.to_string(), v.to_string()))
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_spec() -> EnvSpec {
        const SAMPLE: &str = r#"{"envspec_version":2,"morloc_version":"0.98.2","languages":[{"lang":"py","constraint":">=3.10"},{"lang":"cpp","std":"c++20"},{"lang":"rust"}],"packages":{"cpp":[{"name":"opencv","constraint":">=4.8","source":"conda"}],"py":[{"name":"numpy","constraint":">=2,<3","source":"conda"},{"name":"requests","constraint":"*","source":"pypi"}],"rust":[{"name":"ndarray","constraint":"0.16","source":"crates"}]},"system":[{"name":"blas","provider":"unspecified"}],"modules":[]}"#;
        EnvSpec::from_json(SAMPLE).unwrap()
    }

    fn sample_support() -> crate::langsupport::LangSupport {
        const SUPPORT: &str = r#"{"morloc_version":"0.98.2",
          "toolchain":[{"package":"c-compiler","constraint":"*","phase":"build","optional":false},
                       {"package":"rust","constraint":"*","phase":"build","optional":false}],
          "languages":{
            "py":{"runtime":{"package":"python","version":">=3.10,<3.14","default":"3.12"},
                  "requires":[{"package":"numpy","constraint":">=1.22,<3","phase":"both","optional":false},
                              {"package":"setuptools","constraint":"*","phase":"build","optional":false},
                              {"package":"pyarrow","constraint":"*","phase":"runtime","optional":true}]},
            "cpp":{"runtime":null,"requires":[{"package":"cxx-compiler","constraint":"*","phase":"build","optional":false}]},
            "r":{"runtime":{"package":"r-base","version":">=4.2,<4.6","default":"4.4"},
                 "requires":[{"package":"r-bit64","constraint":"*","phase":"runtime","optional":false}]},
            "rust":{"runtime":{"package":"rust","version":"*"},"requires":[]}
          }}"#;
        crate::langsupport::LangSupport::from_json(SUPPORT).unwrap()
    }

    #[test]
    fn renders_expected_manifest() {
        let spec = sample_spec();
        let support = sample_support();
        let channels = vec!["conda-forge".to_string()];
        let input = PixiManifestInput {
            env_name: "morloc-env-demo",
            platform: "linux-64",
            channels: &channels,
            specs: std::slice::from_ref(&spec),
            lang_support: &support,
        };
        let req = resolve_requirements(&input).unwrap();
        let got = render_manifest(&req);
        // The table injects the core toolchain (c-compiler, rust); clamps python
        // to morloc's supported range intersected with the author's >=3.10; and
        // injects the non-optional py binder deps numpy + setuptools (pyarrow is
        // optional -> omitted). The program's own numpy>=2,<3 merges with the
        // injected numpy>=1.22,<3. cxx-compiler comes from the cpp entry.
        let expected = "\
[workspace]
name = \"morloc-env-demo\"
channels = [\"conda-forge\"]
platforms = [\"linux-64\"]

[dependencies]
\"blas\" = \"*\"
\"c-compiler\" = \"*\"
\"cxx-compiler\" = \"*\"
\"numpy\" = \">=1.22,<3,>=2\"
\"opencv\" = \">=4.8\"
\"python\" = \">=3.10,<3.14\"
\"rust\" = \"*\"
\"setuptools\" = \"*\"

[pypi-dependencies]
\"requests\" = \"*\"
";
        assert_eq!(got, expected);
    }

    #[test]
    fn rust_and_julia_packages_are_excluded_but_toolchains_kept() {
        let spec = sample_spec();
        let support = sample_support();
        let (conda, pypi) = aggregate(std::slice::from_ref(&spec), &support).unwrap();
        // ndarray (a rust crate) must not leak into the conda/pypi manifest...
        assert!(!conda.contains_key("ndarray"));
        assert!(!pypi.contains_key("ndarray"));
        // ...but the rust toolchain is present.
        assert!(conda.contains_key("rust"));
    }

    #[test]
    fn r_packages_get_conda_prefix() {
        const R: &str = r#"{"envspec_version":2,"morloc_version":"0","languages":[{"lang":"r"}],"packages":{"r":[{"name":"data.table","constraint":"*","source":"conda"}]}}"#;
        let spec = EnvSpec::from_json(R).unwrap();
        let support = sample_support();
        let (conda, _) = aggregate(std::slice::from_ref(&spec), &support).unwrap();
        assert!(conda.contains_key("r-data.table"));
        assert!(conda.contains_key("r-base"));
    }

    fn render_with(spec: EnvSpec) -> String {
        let support = sample_support();
        let channels = default_channels();
        let input = PixiManifestInput {
            env_name: "e",
            platform: "linux-64",
            channels: &channels,
            specs: std::slice::from_ref(&spec),
            lang_support: &support,
        };
        render_manifest(&resolve_requirements(&input).unwrap())
    }

    #[test]
    fn bioconda_channel_renders_inline_table_and_workspace_union() {
        // A conda dep on bioconda lowers to a pixi inline table AND adds bioconda
        // to the workspace channels (conda-forge stays first).
        const S: &str = r#"{"envspec_version":3,"morloc_version":"0","languages":[{"lang":"py"}],"packages":{"py":[{"name":"samtools","constraint":"*","source":"conda","channel":"bioconda"}]}}"#;
        let manifest = render_with(EnvSpec::from_json(S).unwrap());
        assert!(manifest.contains("channels = [\"conda-forge\", \"bioconda\"]"), "{manifest}");
        assert!(
            manifest.contains("\"samtools\" = { version = \"*\", channel = \"bioconda\" }"),
            "{manifest}"
        );
    }

    #[test]
    fn conda_forge_dep_stays_flat_and_channel_list_unchanged() {
        // A conda-forge dep (no channel on the wire) keeps the flat form and does
        // not perturb the workspace channel list -- byte-identical to pre-channel.
        const S: &str = r#"{"envspec_version":3,"morloc_version":"0","languages":[{"lang":"py"}],"packages":{"py":[{"name":"numpy","constraint":">=2","source":"conda"}]}}"#;
        let manifest = render_with(EnvSpec::from_json(S).unwrap());
        assert!(manifest.contains("channels = [\"conda-forge\"]"), "{manifest}");
        // flat form (`= "`), not an inline table (`= {`), and no channel key.
        assert!(manifest.contains("\"numpy\" = \""), "{manifest}");
        assert!(!manifest.contains("channel ="), "{manifest}");
    }

    #[test]
    fn r_bioconda_name_passes_literally() {
        // An R dep on a non-conda-forge channel is NOT r-prefixed; the author's
        // exact conda name is used and the channel rides through.
        const S: &str = r#"{"envspec_version":3,"morloc_version":"0","languages":[{"lang":"r"}],"packages":{"r":[{"name":"bioconductor-deseq2","constraint":"*","source":"conda","channel":"bioconda"}]}}"#;
        let manifest = render_with(EnvSpec::from_json(S).unwrap());
        assert!(
            manifest.contains("\"bioconductor-deseq2\" = { version = \"*\", channel = \"bioconda\" }"),
            "{manifest}"
        );
        assert!(!manifest.contains("r-bioconductor"), "{manifest}");
    }

    #[test]
    fn conflicting_channels_across_specs_error() {
        // Two programs drawing the same conda package from different channels is a
        // hard conflict (a channel is a provenance choice, not intersectable).
        const A: &str = r#"{"envspec_version":3,"morloc_version":"0","languages":[{"lang":"py"}],"packages":{"py":[{"name":"pkg","constraint":"*","source":"conda","channel":"bioconda"}]}}"#;
        const B: &str = r#"{"envspec_version":3,"morloc_version":"0","languages":[{"lang":"py"}],"packages":{"py":[{"name":"pkg","constraint":"*","source":"conda","channel":"custom"}]}}"#;
        let specs = vec![EnvSpec::from_json(A).unwrap(), EnvSpec::from_json(B).unwrap()];
        let r = aggregate(&specs, &sample_support());
        assert!(r.is_err());
    }

    #[test]
    fn explicit_channel_beats_omitted_default() {
        // The same package with an explicit channel in one spec and no channel in
        // another coalesces to the explicit channel (no conflict).
        const A: &str = r#"{"envspec_version":3,"morloc_version":"0","languages":[{"lang":"py"}],"packages":{"py":[{"name":"pkg","constraint":"*","source":"conda","channel":"bioconda"}]}}"#;
        const B: &str = r#"{"envspec_version":3,"morloc_version":"0","languages":[{"lang":"py"}],"packages":{"py":[{"name":"pkg","constraint":">=1","source":"conda"}]}}"#;
        let specs = vec![EnvSpec::from_json(A).unwrap(), EnvSpec::from_json(B).unwrap()];
        let (conda, _) = aggregate(&specs, &sample_support()).unwrap();
        // constraint `*` yields to the real `>=1`; the explicit bioconda channel
        // survives the omitted (conda-forge) default.
        assert_eq!(conda.get("pkg"), Some(&(">=1".to_string(), Some("bioconda".to_string()))));
    }

    #[test]
    fn table_clamps_runtime_and_injects_binder_deps() {
        // A python program with a permissive author floor gets morloc's supported
        // range added (clamp) plus the non-optional binder deps; optional deps
        // (pyarrow) are omitted; the core toolchain is always present.
        const PY: &str = r#"{"envspec_version":2,"morloc_version":"0","languages":[{"lang":"py","constraint":">=3.11"}]}"#;
        let spec = EnvSpec::from_json(PY).unwrap();
        let (conda, _) = aggregate(std::slice::from_ref(&spec), &sample_support()).unwrap();
        // clamp: author >=3.11 intersected with morloc's >=3.10,<3.14
        assert_eq!(conda.get("python").map(|(c, _)| c.as_str()), Some(">=3.10,<3.14,>=3.11"));
        // injected non-optional binder deps
        assert_eq!(conda.get("numpy").map(|(c, _)| c.as_str()), Some(">=1.22,<3"));
        assert!(conda.contains_key("setuptools"));
        // optional pyarrow is NOT injected
        assert!(!conda.contains_key("pyarrow"));
        // core toolchain is always present
        assert!(conda.contains_key("c-compiler"));
        assert!(conda.contains_key("rust"));
    }

    #[test]
    fn constraints_merge_across_specs() {
        assert_eq!(merge_constraint(">=2", "<3"), ">=2,<3");
        assert_eq!(merge_constraint("*", ">=1"), ">=1");
        assert_eq!(merge_constraint(">=1", "*"), ">=1");
        assert_eq!(merge_constraint(">=2,<3", ">=2"), ">=2,<3");
    }

    #[test]
    fn conda_artifact_name_version_split_from_right() {
        // A name with an internal hyphen must survive the split.
        assert_eq!(
            parse_conda_artifact("https://conda.anaconda.org/conda-forge/linux-64/binutils_impl_linux-64-2.46.1-default_hfdba357_102.conda"),
            Some(("binutils_impl_linux-64".to_string(), "2.46.1".to_string()))
        );
        assert_eq!(
            parse_conda_artifact("https://conda.anaconda.org/conda-forge/linux-64/python-3.12.4-hab00c5b_0_cpython.conda"),
            Some(("python".to_string(), "3.12.4".to_string()))
        );
        // Legacy .tar.bz2 artifacts parse the same way.
        assert_eq!(
            parse_conda_artifact("https://conda.anaconda.org/conda-forge/noarch/wheel-0.48.0-pyhd8ed1ab_0.tar.bz2"),
            Some(("wheel".to_string(), "0.48.0".to_string()))
        );
    }

    #[test]
    fn pypi_artifact_name_version_split_from_left() {
        assert_eq!(
            parse_pypi_artifact("https://files.pythonhosted.org/packages/aa/numpy-2.1.3-cp312-cp312-manylinux_2_17_x86_64.whl"),
            Some(("numpy".to_string(), "2.1.3".to_string()))
        );
        assert_eq!(
            parse_pypi_artifact("https://files.pythonhosted.org/packages/bb/some_pkg-1.4.2.tar.gz"),
            Some(("some_pkg".to_string(), "1.4.2".to_string()))
        );
    }

    #[test]
    fn locked_packages_reads_env_platform_section() {
        let lock = "\
version: 7
environments:
  default:
    channels:
    - url: https://conda.anaconda.org/conda-forge/
    packages:
      linux-64:
      - conda: https://conda.anaconda.org/conda-forge/linux-64/python-3.12.4-hab00c5b_0_cpython.conda
      - conda: https://conda.anaconda.org/conda-forge/linux-64/binutils_impl_linux-64-2.46.1-default_hfdba357_102.conda
      - pypi: https://files.pythonhosted.org/packages/aa/numpy-2.1.3-cp312-cp312-manylinux_2_17_x86_64.whl
packages:
- conda: https://conda.anaconda.org/conda-forge/linux-64/python-3.12.4-hab00c5b_0_cpython.conda
  size: 1
";
        let pkgs = parse_locked_packages(lock, "linux-64").unwrap();
        // sorted by name: binutils_impl_linux-64, numpy, python
        assert_eq!(pkgs.len(), 3);
        assert_eq!(pkgs[0].name, "binutils_impl_linux-64");
        assert_eq!(pkgs[0].version, "2.46.1");
        assert_eq!(pkgs[0].kind, "conda");
        assert_eq!(pkgs[1].name, "numpy");
        assert_eq!(pkgs[1].kind, "pypi");
        assert_eq!(pkgs[2].name, "python");
        assert_eq!(pkgs[2].version, "3.12.4");
    }

    #[test]
    fn locked_packages_falls_back_to_sole_platform() {
        // Requested platform absent, but exactly one platform present -> use it.
        let lock = "\
environments:
  default:
    packages:
      osx-arm64:
      - conda: https://conda.anaconda.org/conda-forge/osx-arm64/python-3.11.9-h0_0.conda
";
        let pkgs = parse_locked_packages(lock, "linux-64").unwrap();
        assert_eq!(pkgs.len(), 1);
        assert_eq!(pkgs[0].name, "python");
        assert_eq!(pkgs[0].version, "3.11.9");
    }

    #[test]
    fn sh_quote_escapes_single_quotes() {
        assert_eq!(sh_quote("plain"), "'plain'");
        assert_eq!(sh_quote("a b"), "'a b'");
        // The o'brien case: the embedded quote must be escaped, not close the quote.
        assert_eq!(sh_quote("/home/o'brien/x"), "'/home/o'\\''brien/x'");
    }

    #[test]
    fn runtime_languages_maps_present_packages() {
        let pkgs = vec![
            LockedPackage { name: "python".into(), version: "3.12.4".into(), kind: "conda".into() },
            LockedPackage { name: "rust".into(), version: "1.83.0".into(), kind: "conda".into() },
        ];
        assert_eq!(
            runtime_languages(&pkgs),
            vec![("python", "3.12.4".to_string()), ("rust", "1.83.0".to_string())]
        );
        // r-base -> the "r" language label; absent runtimes are omitted.
        let r = vec![LockedPackage { name: "r-base".into(), version: "4.3.1".into(), kind: "conda".into() }];
        assert_eq!(runtime_languages(&r), vec![("r", "4.3.1".to_string())]);
    }

    fn find_pixi() -> std::path::PathBuf {
        let home = std::path::PathBuf::from(std::env::var("HOME").unwrap());
        let local = home.join(".pixi/bin/pixi");
        if local.exists() {
            local
        } else {
            std::path::PathBuf::from("pixi")
        }
    }

    #[test]
    fn parse_env0_handles_multiline_and_equals() {
        let dump = "PATH=/a:/b\0AR=x86_64-conda-linux-gnu-ar\0MULTI=line1\nline2\0KV=a=b\0\0";
        let map = parse_env0(dump);
        let get = |k: &str| map.iter().find(|(kk, _)| kk == k).map(|(_, v)| v.as_str());
        assert_eq!(get("PATH"), Some("/a:/b"));
        assert_eq!(get("AR"), Some("x86_64-conda-linux-gnu-ar"));
        // a value may span newlines and may itself contain '='
        assert_eq!(get("MULTI"), Some("line1\nline2"));
        assert_eq!(get("KV"), Some("a=b"));
        // empty entries (trailing NUL) contribute nothing
        assert_eq!(map.len(), 4);
    }

    #[test]
    fn activation_delta_keeps_added_and_changed_drops_noise() {
        let base = vec![
            ("PATH".to_string(), "/usr/bin".to_string()),
            ("HOME".to_string(), "/home/u".to_string()),
            ("SHLVL".to_string(), "1".to_string()),
        ];
        let activated = vec![
            // changed: conda prepends its bin
            ("PATH".to_string(), "/env/bin:/usr/bin".to_string()),
            // unchanged: dropped
            ("HOME".to_string(), "/home/u".to_string()),
            // added by activate.d: the whole point
            ("AR".to_string(), "x86_64-conda-linux-gnu-ar".to_string()),
            ("CONDA_PREFIX".to_string(), "/env".to_string()),
            // shell bookkeeping that changed: dropped
            ("SHLVL".to_string(), "2".to_string()),
        ];
        let delta = activation_delta(&base, activated);
        let has = |k: &str| delta.iter().any(|(kk, _)| kk == k);
        assert!(has("PATH"), "changed PATH kept");
        assert!(has("AR"), "added AR kept");
        assert!(has("CONDA_PREFIX"), "added CONDA_PREFIX kept");
        assert!(!has("HOME"), "unchanged var dropped");
        assert!(!has("SHLVL"), "shell bookkeeping dropped");
    }

    #[test]
    fn activation_delta_keeps_toolchain_vars_equal_to_base() {
        // `update` run from an already-activated shell: the toolchain vars are
        // IDENTICAL in base and activated. They must not be dropped, else a later
        // `run` from a clean shell builds with no compiler and fails.
        let prefix = "/home/u/.local/share/morloc/env/pixi/.pixi/envs/default";
        let base = vec![
            ("CC".to_string(), "x86_64-conda-linux-gnu-cc".to_string()),
            ("AR".to_string(), "x86_64-conda-linux-gnu-ar".to_string()),
            ("CFLAGS".to_string(), format!("-I{prefix}/include")),
            ("PYTHONHASHSEED".to_string(), "0".to_string()),
            ("CONDA_PREFIX".to_string(), prefix.to_string()),
        ];
        let activated = base.clone(); // fully-activated shell: nothing changed
        let delta = activation_delta(&base, activated);
        let has = |k: &str| delta.iter().any(|(kk, _)| kk == k);
        assert!(has("CC"), "CC kept via forced-keep");
        assert!(has("AR"), "AR kept via forced-keep");
        assert!(has("CFLAGS"), "CFLAGS kept (value references the prefix)");
        assert!(has("CONDA_PREFIX"), "CONDA_PREFIX always kept");
        // an unrelated var equal to base and unrelated to the prefix is dropped
        assert!(!has("PYTHONHASHSEED"), "unrelated unchanged var dropped");
    }

    #[test]
    #[ignore = "requires pixi + network; run with `cargo test -- --ignored`"]
    fn live_solve_and_capture_activation() {
        let pixi = find_pixi();
        let dir = tempfile::tempdir().unwrap();
        // cxx-compiler (not c-compiler alone) pulls the binutils activation that
        // exports $AR, which the capture must preserve.
        write_manifest(
            dir.path(),
            "[workspace]\nname = \"morloc-live-test\"\nchannels = [\"conda-forge\"]\n\
             platforms = [\"linux-64\"]\n\n[dependencies]\n\"cxx-compiler\" = \"*\"\n",
        )
        .unwrap();
        solve(dir.path(), &pixi).expect("pixi solve");
        let act = capture_activation(dir.path(), &pixi).expect("capture activation");
        let get = |k: &str| act.iter().find(|(kk, _)| kk == k).map(|(_, v)| v.clone());
        assert!(get("CONDA_PREFIX").is_some(), "no CONDA_PREFIX captured");
        let path = get("PATH").expect("PATH captured");
        assert!(path.contains(".pixi/envs"), "conda bin not first on PATH: {path}");
        // activate.d exports (not just shell-hook lines) must be captured;
        // without them cc-rs cannot find the archiver.
        assert!(get("AR").is_some(), "no $AR captured (activate.d not sourced)");
        assert!(get("CC").is_some(), "no $CC captured (activate.d not sourced)");
    }
}
