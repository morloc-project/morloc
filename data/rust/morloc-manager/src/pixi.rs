//! Lower a set of `EnvSpec`s to a pixi (conda) project manifest (`pixi.toml`).
//!
//! An environment's pixi manifest is the union of the language toolchains and
//! native packages required by every program installed into it. This module
//! only *generates* the manifest; the pixi solve (run elsewhere) is what decides
//! purity -- whether every compiled dependency resolves from one coherent world.
//!
//! Mapping decisions (documented, first cut):
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

use crate::envspec::{DepClass, EnvSpec};

/// Inputs for rendering an environment's pixi manifest.
pub struct PixiManifestInput<'a> {
    pub env_name: &'a str,
    /// conda platform: "linux-64" | "osx-64" | "osx-arm64".
    pub platform: &'a str,
    /// conda channels, in priority order (typically just ["conda-forge"]).
    pub channels: &'a [String],
    /// The union of requirements of every program installed into this env.
    pub specs: &'a [EnvSpec],
}

/// The conda package that provides a language's toolchain, if morloc knows one.
fn lang_toolchain(lang: &str) -> Option<&'static str> {
    match lang {
        "py" => Some("python"),
        "cpp" => Some("cxx-compiler"),
        "rust" => Some("rust"),
        "r" => Some("r-base"),
        "julia" => Some("julia"),
        "futhark" => Some("futhark"),
        _ => None,
    }
}

/// The conda package name for an R package (conda-forge `r-` feedstock, lowercased).
fn conda_r_name(pkg: &str) -> String {
    format!("r-{}", pkg.to_lowercase())
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

/// Aggregate all specs into (conda dependencies, pypi dependencies) maps.
fn aggregate(specs: &[EnvSpec]) -> (BTreeMap<String, String>, BTreeMap<String, String>) {
    let mut conda: BTreeMap<String, String> = BTreeMap::new();
    let mut pypi: BTreeMap<String, String> = BTreeMap::new();

    for spec in specs {
        // Toolchains, carrying any language version constraint.
        for lang in &spec.languages {
            if let Some(tc) = lang_toolchain(&lang.lang) {
                let c = lang.constraint.as_deref().unwrap_or("*");
                insert_merged(&mut conda, tc, c);
            }
        }

        for (lang, reqs) in &spec.packages {
            match lang.as_str() {
                // cargo and Pkg.jl own these; resolved in-world at pool build.
                "rust" | "julia" => {}
                "py" => {
                    for r in reqs {
                        match r.class {
                            DepClass::Source => insert_merged(&mut pypi, &r.name, &r.constraint),
                            _ => insert_merged(&mut conda, &r.name, &r.constraint),
                        }
                    }
                }
                "r" => {
                    for r in reqs {
                        insert_merged(&mut conda, &conda_r_name(&r.name), &r.constraint);
                    }
                }
                // cpp and any unknown language: best-effort conda.
                _ => {
                    for r in reqs {
                        insert_merged(&mut conda, &r.name, &r.constraint);
                    }
                }
            }
        }

        // System libs: host-provided ones are intentionally left out (they mark
        // impurity, which the solve/escalation handles); everything else is a
        // conda dependency.
        for s in &spec.system {
            if s.provider != "host" {
                insert_merged(&mut conda, &s.name, "*");
            }
        }
    }

    (conda, pypi)
}

/// A TOML key, quoted (package names may contain `.`, which is illegal bare).
fn key(name: &str) -> String {
    format!("\"{name}\"")
}

/// Render the pixi manifest text. Deterministic (sorted dependency maps).
pub fn render_pixi_manifest(input: &PixiManifestInput) -> String {
    let (conda, pypi) = aggregate(input.specs);

    let channels = input
        .channels
        .iter()
        .map(|c| format!("\"{c}\""))
        .collect::<Vec<_>>()
        .join(", ");

    let mut out = String::new();
    out.push_str("[project]\n");
    out.push_str(&format!("name = \"{}\"\n", input.env_name));
    out.push_str(&format!("channels = [{channels}]\n"));
    out.push_str(&format!("platforms = [\"{}\"]\n", input.platform));

    out.push_str("\n[dependencies]\n");
    for (name, constraint) in &conda {
        out.push_str(&format!("{} = \"{}\"\n", key(name), constraint));
    }

    if !pypi.is_empty() {
        out.push_str("\n[pypi-dependencies]\n");
        for (name, constraint) in &pypi {
            out.push_str(&format!("{} = \"{}\"\n", key(name), constraint));
        }
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_spec() -> EnvSpec {
        const SAMPLE: &str = r#"{"envspec_version":1,"morloc_version":"0.98.2","languages":[{"lang":"py","constraint":">=3.10"},{"lang":"cpp","std":"c++20"},{"lang":"rust"}],"packages":{"cpp":[{"name":"opencv","constraint":">=4.8","class":"abi"}],"py":[{"name":"numpy","constraint":">=2,<3","class":"abi"},{"name":"requests","constraint":"*","class":"source"}],"rust":[{"name":"ndarray","constraint":"0.16","class":"source"}]},"system":[{"name":"blas","provider":"unspecified"}],"modules":[]}"#;
        EnvSpec::from_json(SAMPLE).unwrap()
    }

    #[test]
    fn renders_expected_manifest() {
        let spec = sample_spec();
        let channels = vec!["conda-forge".to_string()];
        let input = PixiManifestInput {
            env_name: "morloc-env-demo",
            platform: "linux-64",
            channels: &channels,
            specs: std::slice::from_ref(&spec),
        };
        let got = render_pixi_manifest(&input);
        let expected = "\
[project]
name = \"morloc-env-demo\"
channels = [\"conda-forge\"]
platforms = [\"linux-64\"]

[dependencies]
\"blas\" = \"*\"
\"cxx-compiler\" = \"*\"
\"numpy\" = \">=2,<3\"
\"opencv\" = \">=4.8\"
\"python\" = \">=3.10\"
\"rust\" = \"*\"

[pypi-dependencies]
\"requests\" = \"*\"
";
        assert_eq!(got, expected);
    }

    #[test]
    fn rust_and_julia_packages_are_excluded_but_toolchains_kept() {
        let spec = sample_spec();
        let (conda, pypi) = aggregate(std::slice::from_ref(&spec));
        // ndarray (a rust crate) must not leak into the conda/pypi manifest...
        assert!(!conda.contains_key("ndarray"));
        assert!(!pypi.contains_key("ndarray"));
        // ...but the rust toolchain is present.
        assert!(conda.contains_key("rust"));
    }

    #[test]
    fn r_packages_get_conda_prefix() {
        const R: &str = r#"{"envspec_version":1,"morloc_version":"0","languages":[{"lang":"r"}],"packages":{"r":[{"name":"data.table","constraint":"*","class":"abi"}]}}"#;
        let spec = EnvSpec::from_json(R).unwrap();
        let (conda, _) = aggregate(std::slice::from_ref(&spec));
        assert!(conda.contains_key("r-data.table"));
        assert!(conda.contains_key("r-base"));
    }

    #[test]
    fn constraints_merge_across_specs() {
        assert_eq!(merge_constraint(">=2", "<3"), ">=2,<3");
        assert_eq!(merge_constraint("*", ">=1"), ">=1");
        assert_eq!(merge_constraint(">=1", "*"), ">=1");
        assert_eq!(merge_constraint(">=2,<3", ">=2"), ">=2,<3");
    }
}
