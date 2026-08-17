//! Parser for the language-support table emitted by `morloc lang-support`.
//!
//! Cross-language contract: the schema is produced by
//! `Morloc.LangSupport.renderLangSupport` in the compiler. It records, per morloc
//! release, the conda packages morloc's own binders require and the language
//! runtime versions morloc supports -- morloc's contribution to a native
//! environment's constraints, independent of any user program's dependencies.
//! The pixi lowering (`pixi.rs`) intersects it with each program's EnvSpec.

use std::collections::BTreeMap;

use serde::Deserialize;

use crate::error::{ManagerError, Result};

fn any_constraint() -> String {
    "*".to_string()
}

/// One conda package morloc requires, with a version match-spec.
#[derive(Debug, Clone, Deserialize)]
pub struct PkgReq {
    pub package: String,
    #[serde(default = "any_constraint")]
    pub constraint: String,
    /// Optional: a binder can use it if present but does not need it for basic
    /// operation (feature/type-gated, e.g. pyarrow). Included in a full env,
    /// omitted in a minimal one.
    #[serde(default)]
    pub optional: bool,
    // `phase` (build|runtime|both) is in the JSON but unused here: the manifest
    // carries all deps; phase matters only for serve-image trimming later.
}

/// A language's versioned runtime (absent for C++, whose "version" is the
/// standard, not a package).
#[derive(Debug, Clone, Deserialize)]
pub struct RuntimeSpec {
    pub package: String,
    #[serde(default = "any_constraint")]
    pub version: String,
    #[serde(default)]
    pub default: Option<String>,
}

/// Support for one language: its runtime (if versioned) plus morloc's extra
/// binder packages.
#[derive(Debug, Clone, Deserialize)]
pub struct LangEntry {
    #[serde(default)]
    pub runtime: Option<RuntimeSpec>,
    #[serde(default)]
    pub requires: Vec<PkgReq>,
}

/// The whole language-support table for a morloc release.
#[derive(Debug, Clone, Default, Deserialize)]
pub struct LangSupport {
    #[serde(default)]
    pub morloc_version: String,
    /// Core toolchain, always required (libmorloc + any shim).
    #[serde(default)]
    pub toolchain: Vec<PkgReq>,
    /// Per-language entries, keyed by canonical short name (py/r/cpp/rust).
    #[serde(default)]
    pub languages: BTreeMap<String, LangEntry>,
}

impl LangSupport {
    /// Parse the table from `morloc lang-support` JSON output.
    pub fn from_json(text: &str) -> Result<Self> {
        serde_json::from_str(text).map_err(|e| {
            ManagerError::EnvError(format!("Failed to parse the language-support table: {e}"))
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = r#"{"morloc_version":"0.99.0",
      "toolchain":[{"package":"c-compiler","constraint":"*","phase":"build","optional":false},
                   {"package":"rust","constraint":"*","phase":"build","optional":false}],
      "languages":{
        "py":{"runtime":{"package":"python","version":">=3.10,<3.14","default":"3.12"},
              "requires":[{"package":"numpy","constraint":">=1.22,<3","phase":"both","optional":false},
                          {"package":"setuptools","constraint":"*","phase":"build","optional":false},
                          {"package":"pyarrow","constraint":"*","phase":"runtime","optional":true}]},
        "cpp":{"runtime":null,"requires":[{"package":"cxx-compiler","constraint":"*","phase":"build","optional":false}]}
      }}"#;

    #[test]
    fn parses_the_table() {
        let t = LangSupport::from_json(SAMPLE).unwrap();
        assert_eq!(t.morloc_version, "0.99.0");
        assert_eq!(t.toolchain.len(), 2);
        let py = t.languages.get("py").unwrap();
        assert_eq!(py.runtime.as_ref().unwrap().package, "python");
        assert_eq!(py.runtime.as_ref().unwrap().default.as_deref(), Some("3.12"));
        // pyarrow is optional; numpy/setuptools are not
        let opt = |n: &str| py.requires.iter().find(|p| p.package == n).map(|p| p.optional);
        assert_eq!(opt("pyarrow"), Some(true));
        assert_eq!(opt("numpy"), Some(false));
        // cpp has no runtime, just a requirement
        assert!(t.languages.get("cpp").unwrap().runtime.is_none());
    }

    #[test]
    fn missing_fields_default() {
        let t = LangSupport::from_json(r#"{"morloc_version":"0"}"#).unwrap();
        assert!(t.toolchain.is_empty());
        assert!(t.languages.is_empty());
    }
}
