//! Shim-ABI coherence lock: pin the environment's interpreter minor versions to
//! the ones morloc's language shims were built against.
//!
//! `morloc init` produces version-embedded artifacts -- the Python binding is
//! tagged to a CPython ABI (`pymorloc.cpython-3XY-*.so`), the R binding loads a
//! specific `libR`. If a later dependency solve bumped an interpreter's MINOR
//! version those shims would no longer load, so this module derives a pin (from
//! the solved conda prefix) folded back into the requirement set: a dependency
//! demanding a different interpreter minor then fails the solve as a legible
//! conflict instead of silently breaking the shims. (Which packages are pinned,
//! and why libstdc++/glibc are not, is documented on `ABI_PACKAGES`.)

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use serde::Deserialize;

use crate::envspec::{EnvSpec, LangReq};

/// conda packages whose MINOR version is baked into a morloc language shim,
/// paired with the morloc language name the pin must be expressed under (so it
/// flows through the same language-runtime clamp as the rest of the toolchain).
///
/// This is the complete set of ABI-minor-sensitive shims today (python + R). It
/// is a SAFETY list: a shim language missing from it is silently unprotected -- a
/// dependency could bump its interpreter minor and break the shim with no error.
/// Any future shim that bakes an interpreter minor into its artifact (e.g. a
/// Julia binder, whose embedding C-API is minor-sensitive) MUST be added here. Do
/// not derive this from "has a versioned runtime": Rust has a runtime entry but is
/// C-ABI to libmorloc and NOT minor-sensitive, so that would over-pin it.
const ABI_PACKAGES: &[(&str, &str)] = &[("python", "py"), ("r-base", "r")];

/// The solved conda prefix under a pixi project dir. Both backends solve into the
/// `default` environment, and the container bind-mounts its `/env` from this same
/// host dir, so one derivation serves native and container alike.
pub fn conda_prefix(pixi_dir: &Path) -> PathBuf {
    pixi_dir.join(".pixi").join("envs").join("default")
}

/// Read the installed versions of the ABI-relevant packages from a solved conda
/// prefix's `conda-meta/`. Each installed package leaves a
/// `<name>-<version>-<build>.json` record carrying its name and version. Absent
/// packages (a language the env does not use) are simply not returned; an
/// unreadable prefix yields an empty map (best-effort, never fatal).
fn abi_versions(conda_prefix: &Path) -> BTreeMap<String, String> {
    #[derive(Deserialize)]
    struct Meta {
        name: String,
        version: String,
    }
    let mut found = BTreeMap::new();
    let entries = match std::fs::read_dir(conda_prefix.join("conda-meta")) {
        Ok(e) => e,
        Err(_) => return found,
    };
    for path in entries.flatten().map(|e| e.path()) {
        if path.extension().and_then(|x| x.to_str()) != Some("json") {
            continue;
        }
        if let Ok(text) = std::fs::read_to_string(&path) {
            if let Ok(m) = serde_json::from_str::<Meta>(&text) {
                if ABI_PACKAGES.iter().any(|(pkg, _)| *pkg == m.name) {
                    found.insert(m.name, m.version);
                }
            }
        }
    }
    found
}

/// A `>=MAJOR.MINOR,<MAJOR.(MINOR+1)` match-spec holding the minor while leaving
/// the patch free, or None if the version lacks two leading numeric components.
/// The interval form (rather than a fuzzy `MAJOR.MINOR.*` atom) matches the shape
/// `constraint::range.to_spec` already emits for the toolchain, so this merges
/// with the language's existing `>=..,<..` range as plain comma-separated
/// intervals -- no mixed fuzzy/interval spec for the conda parser to choke on.
fn minor_pin(version: &str) -> Option<String> {
    let mut parts = version.split('.');
    let major: u64 = parts.next()?.parse().ok()?;
    let minor: u64 = parts.next()?.parse().ok()?;
    Some(format!(">={major}.{minor},<{major}.{}", minor + 1))
}

/// Build the ABI-lock spec -- a languages-only `EnvSpec` pinning each shim
/// interpreter to its solved minor -- from a solved conda prefix. Returns None
/// when no ABI-relevant interpreter is present (e.g. a C++/Rust-only env), so the
/// caller can clear rather than write a spurious lock.
pub fn abi_lock_spec(conda_prefix: &Path, morloc_version: &str) -> Option<EnvSpec> {
    let versions = abi_versions(conda_prefix);
    let langs: Vec<LangReq> = ABI_PACKAGES
        .iter()
        .filter_map(|(pkg, lang)| {
            let pin = minor_pin(versions.get(*pkg)?)?;
            Some(LangReq { lang: lang.to_string(), constraint: Some(pin), std: None })
        })
        .collect();
    if langs.is_empty() {
        None
    } else {
        Some(EnvSpec::from_languages(morloc_version, langs))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn minor_pin_keeps_major_minor_frees_patch() {
        assert_eq!(minor_pin("3.12.5"), Some(">=3.12,<3.13".to_string()));
        assert_eq!(minor_pin("4.3.3"), Some(">=4.3,<4.4".to_string()));
        // A bare major (no minor) cannot express an ABI minor -> no pin.
        assert_eq!(minor_pin("3"), None);
        assert_eq!(minor_pin("dev"), None);
        // A non-numeric minor is rejected (not silently pinned).
        assert_eq!(minor_pin("3.x.1"), None);
    }

    /// Write a minimal `conda-meta/<name>-<ver>-<build>.json` record.
    fn write_meta(prefix: &Path, name: &str, version: &str) {
        let dir = prefix.join("conda-meta");
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(
            dir.join(format!("{name}-{version}-0.json")),
            format!(r#"{{"name":"{name}","version":"{version}"}}"#),
        )
        .unwrap();
    }

    #[test]
    fn abi_lock_pins_present_interpreters_only() {
        let tmp = tempfile::tempdir().unwrap();
        let prefix = tmp.path();
        write_meta(prefix, "python", "3.12.5");
        write_meta(prefix, "r-base", "4.3.3");
        // A non-ABI package must not enter the lock.
        write_meta(prefix, "numpy", "2.1.0");

        let spec = abi_lock_spec(prefix, "0.99.0").expect("some interpreter present");
        let mut pins: Vec<(String, String)> = spec
            .languages
            .iter()
            .map(|l| (l.lang.clone(), l.constraint.clone().unwrap()))
            .collect();
        pins.sort();
        assert_eq!(
            pins,
            vec![
                ("py".to_string(), ">=3.12,<3.13".to_string()),
                ("r".to_string(), ">=4.3,<4.4".to_string()),
            ]
        );
    }

    #[test]
    fn abi_lock_absent_when_no_interpreter() {
        let tmp = tempfile::tempdir().unwrap();
        // Only a non-interpreter package: nothing to protect.
        write_meta(tmp.path(), "libstdcxx-ng", "14.1.0");
        assert!(abi_lock_spec(tmp.path(), "0.99.0").is_none());
        // A missing prefix is also just "no lock", never a panic.
        assert!(abi_lock_spec(&tmp.path().join("nope"), "0.99.0").is_none());
    }
}
