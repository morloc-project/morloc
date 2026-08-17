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
use crate::langsupport::LangSupport;

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

/// Aggregate all specs into (conda dependencies, pypi dependencies) maps,
/// clamped and injected against morloc's language-support table.
fn aggregate(
    specs: &[EnvSpec],
    support: &LangSupport,
) -> (BTreeMap<String, String>, BTreeMap<String, String>) {
    let mut conda: BTreeMap<String, String> = BTreeMap::new();
    let mut pypi: BTreeMap<String, String> = BTreeMap::new();

    // Core toolchain: always required (libmorloc + any shim). Non-optional only.
    for p in &support.toolchain {
        if !p.optional {
            insert_merged(&mut conda, &p.package, &p.constraint);
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
                    insert_merged(&mut conda, &rt.package, &merge_constraint(&rt.version, author_c));
                }
                for p in &entry.requires {
                    if !p.optional {
                        insert_merged(&mut conda, &p.package, &p.constraint);
                    }
                }
            }
            None => {
                if let Some(tc) = lang_toolchain(lang) {
                    insert_merged(&mut conda, tc, author_c);
                }
            }
        }
    }

    for spec in specs {
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
    let (conda, pypi) = aggregate(input.specs, input.lang_support);

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
        let got = render_pixi_manifest(&input);
        // The table injects the core toolchain (c-compiler, rust); clamps python
        // to morloc's supported range intersected with the author's >=3.10; and
        // injects the non-optional py binder deps numpy + setuptools (pyarrow is
        // optional -> omitted). The program's own numpy>=2,<3 merges with the
        // injected numpy>=1.22,<3. cxx-compiler comes from the cpp entry.
        let expected = "\
[project]
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
        let (conda, pypi) = aggregate(std::slice::from_ref(&spec), &support);
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
        let support = sample_support();
        let (conda, _) = aggregate(std::slice::from_ref(&spec), &support);
        assert!(conda.contains_key("r-data.table"));
        assert!(conda.contains_key("r-base"));
    }

    #[test]
    fn table_clamps_runtime_and_injects_binder_deps() {
        // A python program with a permissive author floor gets morloc's supported
        // range added (clamp) plus the non-optional binder deps; optional deps
        // (pyarrow) are omitted; the core toolchain is always present.
        const PY: &str = r#"{"envspec_version":1,"morloc_version":"0","languages":[{"lang":"py","constraint":">=3.11"}]}"#;
        let spec = EnvSpec::from_json(PY).unwrap();
        let (conda, _) = aggregate(std::slice::from_ref(&spec), &sample_support());
        // clamp: author >=3.11 intersected with morloc's >=3.10,<3.14
        assert_eq!(conda.get("python").map(String::as_str), Some(">=3.10,<3.14,>=3.11"));
        // injected non-optional binder deps
        assert_eq!(conda.get("numpy").map(String::as_str), Some(">=1.22,<3"));
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
}
