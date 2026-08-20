//! Parser for `envspec.json`, the backend-agnostic environment-requirement
//! record emitted by the morloc compiler (`morloc make`) beside `manifest.json`.
//!
//! This is the Rust half of a cross-language contract: the schema is produced by
//! `Morloc.CodeGenerator.EnvSpec.renderEnvSpec` in the compiler. Every package
//! carries an explicit `source` (the package database it is drawn from); the
//! backend routes each package by its stated source rather than guessing from
//! the name.

use std::path::Path;

use serde::{Deserialize, Serialize};

use crate::error::{DepsError, Result};

/// The lowest and highest `envspec_version` this build understands. Both bounds
/// are enforced: an older schema lacks fields this build needs (the program must
/// be rebuilt), a newer one carries fields this build cannot honor (upgrade the
/// manager).
pub const MIN_ENVSPEC_VERSION: u32 = 2;
pub const SUPPORTED_ENVSPEC_VERSION: u32 = 2;

/// The package database a dependency is drawn from. Names WHERE a package comes
/// from, never the tool that fetches it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum DepSource {
    /// The conda package database (conda-forge and friends).
    Conda,
    /// The Python Package Index.
    Pypi,
    /// The R CRAN registry (not yet honored natively; the compiler rejects it).
    Cran,
    /// The R Bioconductor registry (not yet honored natively).
    Bioconductor,
    /// crates.io (owned by cargo; resolved at pool build).
    Crates,
    /// Julia's General registry (owned by Pkg.jl; resolved at pool build).
    Pkg,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LangReq {
    pub lang: String,
    #[serde(default)]
    pub constraint: Option<String>,
    /// C++ standard, e.g. "c++20" (cpp only).
    #[serde(default)]
    pub std: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageReq {
    pub name: String,
    pub constraint: String,
    pub source: DepSource,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SystemReq {
    pub name: String,
    /// Provider hint: "conda-forge" | "host" | "vcpkg" | "unspecified".
    pub provider: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModuleReq {
    pub name: String,
    #[serde(default)]
    pub git_hash: Option<String>,
}

/// A program's declared environment requirements. Keys of `packages` are
/// canonical morloc language names ("py", "r", "cpp", "rust", "julia").
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnvSpec {
    pub envspec_version: u32,
    pub morloc_version: String,
    #[serde(default)]
    pub languages: Vec<LangReq>,
    #[serde(default)]
    pub packages: std::collections::BTreeMap<String, Vec<PackageReq>>,
    #[serde(default)]
    pub system: Vec<SystemReq>,
    #[serde(default)]
    pub modules: Vec<ModuleReq>,
}

impl EnvSpec {
    /// Parse an EnvSpec from JSON text.
    pub fn from_json(text: &str) -> Result<Self> {
        // Peek the schema version before the full parse. A stale (v1) envspec.json
        // lacks the `source` field and would otherwise fail with an opaque serde
        // error deep in an env-wide operation; peeking lets us name the real fix.
        #[derive(Deserialize)]
        struct VersionPeek {
            envspec_version: u32,
        }
        let peek: VersionPeek = serde_json::from_str(text).map_err(|e| {
            DepsError::Env(format!("Failed to parse envspec.json: {e}"))
        })?;
        if peek.envspec_version < MIN_ENVSPEC_VERSION {
            return Err(DepsError::Env(format!(
                "envspec.json is version {} but this morloc-manager requires version {}. \
                 Rebuild the program with a current morloc (morloc make).",
                peek.envspec_version, MIN_ENVSPEC_VERSION
            )));
        }
        if peek.envspec_version > SUPPORTED_ENVSPEC_VERSION {
            return Err(DepsError::Env(format!(
                "envspec.json is version {} but this morloc-manager understands only \
                 up to version {}. Upgrade morloc-manager.",
                peek.envspec_version, SUPPORTED_ENVSPEC_VERSION
            )));
        }
        serde_json::from_str(text).map_err(|e| {
            DepsError::Env(format!("Failed to parse envspec.json: {e}"))
        })
    }

    /// Read and parse the `envspec.json` sitting in a program's build directory.
    pub fn read_from_build_dir(build_dir: &Path) -> Result<Self> {
        let path = build_dir.join("envspec.json");
        let text = std::fs::read_to_string(&path).map_err(|e| {
            DepsError::Env(format!("Cannot read {}: {e}", path.display()))
        })?;
        Self::from_json(&text)
    }

    /// Build a synthetic spec carrying only language requirements. `--lang` pins
    /// enter the solve exactly like a program's declared language deps, so they
    /// are modeled as a spec with no packages/system/modules.
    pub fn from_languages(morloc_version: &str, languages: Vec<LangReq>) -> EnvSpec {
        EnvSpec {
            envspec_version: SUPPORTED_ENVSPEC_VERSION,
            morloc_version: morloc_version.to_string(),
            languages,
            packages: std::collections::BTreeMap::new(),
            system: Vec::new(),
            modules: Vec::new(),
        }
    }

    /// Fast, pre-solve reasons this program cannot build on the native backend.
    /// The native backend provides only conda-forge packages and has no build
    /// layer, so a system dependency that must come from the host or another
    /// non-conda provider is a hard blocker. A `conda-forge` provider is fine;
    /// an `unspecified` provider is left to the solve (not a fast blocker). An
    /// empty result means "no fast blocker" -- the pixi solve remains the final
    /// authority on whether the requirements resolve natively.
    pub fn native_blockers(&self) -> Vec<String> {
        self.system
            .iter()
            .filter(|s| {
                let p = s.provider.to_ascii_lowercase();
                p != "conda-forge" && p != "unspecified"
            })
            .map(|s| {
                format!(
                    "system dependency '{}' (provider: {}) cannot be provided by the \
                     native backend; use a container backend (--engine podman)",
                    s.name, s.provider
                )
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Exactly the shape emitted by the compiler's renderEnvSpec (sorted package
    // map keys; a bare language entry with neither constraint nor std). Each
    // package carries an explicit source.
    const SAMPLE: &str = r#"{"envspec_version":2,"morloc_version":"0.98.2","languages":[{"lang":"py","constraint":">=3.10"},{"lang":"cpp","std":"c++20"},{"lang":"rust"}],"packages":{"cpp":[{"name":"opencv","constraint":">=4.8","source":"conda"}],"py":[{"name":"numpy","constraint":">=2,<3","source":"conda"},{"name":"requests","constraint":"*","source":"pypi"}],"rust":[{"name":"ndarray","constraint":"0.16","source":"crates"}]},"system":[{"name":"blas","provider":"unspecified"}],"modules":[{"name":"tensor-cpp","git_hash":"abc123"}]}"#;

    #[test]
    fn parses_compiler_output() {
        let s = EnvSpec::from_json(SAMPLE).unwrap();
        assert_eq!(s.envspec_version, 2);
        assert_eq!(s.morloc_version, "0.98.2");

        assert_eq!(s.languages.len(), 3);
        assert_eq!(s.languages[0].lang, "py");
        assert_eq!(s.languages[0].constraint.as_deref(), Some(">=3.10"));
        assert_eq!(s.languages[1].std.as_deref(), Some("c++20"));
        // A bare language entry has neither constraint nor std.
        assert_eq!(s.languages[2].lang, "rust");
        assert!(s.languages[2].constraint.is_none() && s.languages[2].std.is_none());

        let py = &s.packages["py"];
        assert_eq!(py[0].name, "numpy");
        assert_eq!(py[0].source, DepSource::Conda);
        assert_eq!(py[1].name, "requests");
        assert_eq!(py[1].source, DepSource::Pypi);
        assert_eq!(s.packages["cpp"][0].source, DepSource::Conda);
        assert_eq!(s.packages["rust"][0].source, DepSource::Crates);

        assert_eq!(s.system[0].name, "blas");
        assert_eq!(s.system[0].provider, "unspecified");
        assert_eq!(s.modules[0].name, "tensor-cpp");
        assert_eq!(s.modules[0].git_hash.as_deref(), Some("abc123"));
    }

    #[test]
    fn empty_collections_default() {
        let s = EnvSpec::from_json(r#"{"envspec_version":2,"morloc_version":"0.0.0"}"#).unwrap();
        assert!(s.languages.is_empty());
        assert!(s.packages.is_empty());
        assert!(s.system.is_empty());
        assert!(s.modules.is_empty());
    }

    #[test]
    fn rejects_future_version() {
        let r = EnvSpec::from_json(r#"{"envspec_version":999,"morloc_version":"9.9.9"}"#);
        assert!(r.is_err());
    }

    #[test]
    fn rejects_stale_version() {
        // A v1 envspec.json (pre-source schema) must fail with a clear message,
        // not an opaque serde error over the missing `source` field.
        let r = EnvSpec::from_json(r#"{"envspec_version":1,"morloc_version":"0.0.0"}"#);
        assert!(r.is_err());
    }

    fn spec_with_system(system_json: &str) -> EnvSpec {
        EnvSpec::from_json(&format!(
            r#"{{"envspec_version":2,"morloc_version":"0.0.0","system":{system_json}}}"#
        ))
        .unwrap()
    }

    #[test]
    fn native_blockers_ignores_conda_and_unspecified() {
        // conda-forge is provided natively; unspecified is left to the solve.
        let s = spec_with_system(
            r#"[{"name":"blas","provider":"conda-forge"},{"name":"lapack","provider":"unspecified"}]"#,
        );
        assert!(s.native_blockers().is_empty());
    }

    #[test]
    fn native_blockers_flags_host_and_vcpkg_providers() {
        let s = spec_with_system(
            r#"[{"name":"cuda","provider":"host"},{"name":"zlib","provider":"conda-forge"},{"name":"boost","provider":"vcpkg"}]"#,
        );
        let blockers = s.native_blockers();
        assert_eq!(blockers.len(), 2);
        assert!(blockers.iter().any(|b| b.contains("cuda")));
        assert!(blockers.iter().any(|b| b.contains("boost")));
        assert!(blockers.iter().all(|b| !b.contains("zlib")));
    }

    #[test]
    fn native_blockers_empty_when_no_system_deps() {
        let s = EnvSpec::from_json(r#"{"envspec_version":2,"morloc_version":"0.0.0"}"#).unwrap();
        assert!(s.native_blockers().is_empty());
    }
}
