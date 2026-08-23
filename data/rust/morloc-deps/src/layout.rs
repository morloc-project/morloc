//! Well-known paths within a morloc SOURCE repo, in ONE place.
//!
//! A dev environment mounts a morloc source tree (the developer builds the
//! compiler + runtime from it), so the manager must locate a few files/dirs
//! inside it (`stack.yaml`, the Rust workspace, the language-requirement YAMLs).
//! That is unavoidable coupling to the repo layout; centralizing it here keeps a
//! future repo reorganization a one-line change rather than a scattered break,
//! and lets `new --dev` validate the source with a clear fail-fast error.

use std::path::{Path, PathBuf};

/// Haskell compiler build config (presence marks a plausible morloc source).
pub const STACK_YAML: &str = "stack.yaml";
/// The Rust workspace, `morloc init`'s `MORLOC_RUST_DIR`.
pub const RUST_DIR: &str = "data/rust";
/// The core toolchain requirements (pixi `toolchain` + the dev-only `dev-apt`).
pub const CORE_REQUIREMENTS: &str = "data/lang/requirements.yaml";
/// Per-language requirements live at `data/lang/<lang>/requirements.yaml`.
pub const LANG_DIR: &str = "data/lang";

/// The languages provisioned via conda (a per-language `requirements.yaml`),
/// matching the compiler's `DataFiles.requirementsFiles`. Keep in lockstep.
pub const LANGUAGES: [&str; 4] = ["py", "r", "cpp", "rust"];

/// Languages provisioned by a container `install.sh` instead of conda (their
/// upstream binary is not on conda-forge, e.g. futhark). No `requirements.yaml`;
/// the install script IS the provisioning, and it is OCI-container-only. Keep in
/// lockstep with the compiler's `DataFiles.installScriptFiles`.
pub const SCRIPT_LANGUAGES: [&str; 1] = ["futhark"];

/// The per-language requirements file for `lang` under `source`.
pub fn lang_requirements(source: &Path, lang: &str) -> PathBuf {
    source.join(LANG_DIR).join(lang).join("requirements.yaml")
}

/// The per-language container install script for `lang` under `source`.
pub fn lang_install_script(source: &Path, lang: &str) -> PathBuf {
    source.join(LANG_DIR).join(lang).join("install.sh")
}

/// Validate that `source` looks like a morloc source repo the dev path can build
/// from, returning a clear error naming the first missing piece.
pub fn validate_source(source: &Path) -> Result<(), String> {
    for rel in [STACK_YAML, CORE_REQUIREMENTS] {
        if !source.join(rel).is_file() {
            return Err(format!(
                "{} is not a morloc source tree (missing {rel})",
                source.display()
            ));
        }
    }
    if !source.join(RUST_DIR).join("Cargo.toml").is_file() {
        return Err(format!(
            "{} is not a morloc source tree (missing {RUST_DIR}/Cargo.toml)",
            source.display()
        ));
    }
    Ok(())
}
