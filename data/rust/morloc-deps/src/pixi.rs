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
/// (julia, futhark) -- the escape hatch until they gain table entries. Languages
/// in the support table get their toolchain from `runtime.package` and `requires`
/// instead; those are NOT duplicated here (a second copy would silently diverge).
fn lang_toolchain(lang: &str) -> Option<&'static str> {
    match lang {
        "julia" => Some("julia"),
        "futhark" => Some("futhark"),
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

/// Capture the toolchain activation env-map from a solved pixi env by parsing
/// `pixi shell-hook`. The map is what the native Runner injects before spawning a
/// command, and it carries EVERY variable the conda activation scripts export --
/// PATH + CONDA_PREFIX, and (crucially) `$CC`/`$CXX`/`$AR`/`$RANLIB`/`$STRIP`/...
/// set by conda-forge's `c-compiler`/`cxx-compiler` packages. conda ships the
/// binutils/compilers under target-prefixed names (e.g. `x86_64-conda-linux-gnu-
/// ar`), NOT as a bare `ar`/`gcc` on PATH, so those `$AR`/`$CC` entries are what
/// let cargo/cc-rs find the archiver; dropping them breaks C-dependency builds.
pub fn capture_activation(env_dir: &Path, pixi_bin: &Path) -> Result<Vec<(String, String)>> {
    let manifest = env_dir.join("pixi.toml");
    let out = Command::new(pixi_bin)
        .arg("shell-hook")
        .arg("--manifest-path")
        .arg(&manifest)
        .stdin(Stdio::null())
        .output()
        .map_err(|e| DepsError::Env(format!("could not run pixi shell-hook: {e}")))?;
    if !out.status.success() {
        return Err(DepsError::Env(format!(
            "pixi shell-hook failed: {}",
            String::from_utf8_lossy(&out.stderr).trim()
        )));
    }
    let map = parse_shell_hook(&String::from_utf8_lossy(&out.stdout));
    if !map.iter().any(|(k, _)| k == "CONDA_PREFIX") {
        return Err(DepsError::Env(
            "pixi shell-hook produced no CONDA_PREFIX; cannot activate the native toolchain"
                .to_string(),
        ));
    }
    Ok(map)
}

/// Parse `pixi shell-hook` output into an activation env-map. Recognizes
/// `export KEY=VALUE` lines with an identifier key; ignores comments, blank
/// lines, and any trailing shell-function body. Values are shell-unquoted.
pub fn parse_shell_hook(output: &str) -> Vec<(String, String)> {
    let mut map = Vec::new();
    for line in output.lines() {
        let rest = match line.trim().strip_prefix("export ") {
            Some(r) => r,
            None => continue,
        };
        let (key, val) = match rest.split_once('=') {
            Some(kv) => kv,
            None => continue,
        };
        if key.is_empty()
            || !key.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
        {
            continue;
        }
        map.push((key.to_string(), shell_unquote(val)));
    }
    map
}

/// Strip a single layer of matching single or double quotes.
fn shell_unquote(s: &str) -> String {
    let s = s.trim();
    let bytes = s.as_bytes();
    if bytes.len() >= 2
        && (bytes[0] == b'"' || bytes[0] == b'\'')
        && bytes[bytes.len() - 1] == bytes[0]
    {
        s[1..s.len() - 1].to_string()
    } else {
        s.to_string()
    }
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

    // The exact shape `pixi shell-hook` emits: export lines, a blank line, and a
    // trailing shellcheck comment (which must be ignored).
    const SHELL_HOOK: &str = "\
export PATH=\"/env/.pixi/envs/default/bin:/usr/bin:/bin\"
export CONDA_SHLVL=1
export CONDA_PREFIX=/env/.pixi/envs/default
export PIXI_PROMPT='(default) '

# shellcheck shell=bash
";

    #[test]
    fn parse_shell_hook_extracts_exports() {
        let map = parse_shell_hook(SHELL_HOOK);
        let get = |k: &str| map.iter().find(|(kk, _)| kk == k).map(|(_, v)| v.as_str());
        assert_eq!(get("CONDA_PREFIX"), Some("/env/.pixi/envs/default"));
        assert_eq!(get("PATH"), Some("/env/.pixi/envs/default/bin:/usr/bin:/bin"));
        assert_eq!(get("CONDA_SHLVL"), Some("1"));
        // single-quoted value is unquoted
        assert_eq!(get("PIXI_PROMPT"), Some("(default) "));
        // comment / blank lines contribute nothing
        assert!(!map.iter().any(|(k, _)| k.starts_with('#')));
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
    #[ignore = "requires pixi + network; run with `cargo test -- --ignored`"]
    fn live_solve_and_capture_activation() {
        let pixi = find_pixi();
        let dir = tempfile::tempdir().unwrap();
        write_manifest(
            dir.path(),
            "[workspace]\nname = \"morloc-live-test\"\nchannels = [\"conda-forge\"]\n\
             platforms = [\"linux-64\"]\n\n[dependencies]\n\"c-compiler\" = \"*\"\n",
        )
        .unwrap();
        solve(dir.path(), &pixi).expect("pixi solve");
        let act = capture_activation(dir.path(), &pixi).expect("capture activation");
        let get = |k: &str| act.iter().find(|(kk, _)| kk == k).map(|(_, v)| v.clone());
        assert!(get("CONDA_PREFIX").is_some(), "no CONDA_PREFIX captured");
        let path = get("PATH").expect("PATH captured");
        assert!(path.contains(".pixi/envs"), "conda bin not first on PATH: {path}");
    }
}
