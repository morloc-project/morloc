//! The environment's persisted dependency-spec store.
//!
//! Each program contributes its compiler-produced `envspec.json` to
//! `<env_home>/requirements/{installed,scratch}/<program>.json`. The union of
//! these is the environment's declared dependency world; `installed/` alone is
//! the canonical baseline that `clean` resets to. Scratch holds the specs of
//! programs that were built (`morloc make`) but not installed.
//!
//! Specs are stored as the raw JSON the compiler emitted -- storing verbatim
//! avoids a second (Rust) serializer for a schema the compiler already owns.

use std::path::{Path, PathBuf};

use crate::envspec::EnvSpec;
use crate::error::{DepsError, Result};
use crate::langsupport::LangSupport;

/// The runtime ingredients a solve needs that the environment context cannot
/// supply on its own: morloc's language-support table, the pixi binary, the
/// conda platform tag, and the channel priority list. Callers assemble these
/// (the manager from its config/provisioning, the in-env agent from its
/// environment) and hand them to `sync`/`clean`.
pub struct SolveInputs<'a> {
    pub lang_support: &'a LangSupport,
    pub pixi_bin: &'a Path,
    pub platform: &'a str,
    pub channels: &'a [String],
}

/// Locates an environment's on-disk layout. Constructed from the env's home
/// directory: the manager derives that from its config, the in-env agent from
/// its environment variables. Deliberately agnostic to how the env was located.
pub struct EnvContext {
    env_home: PathBuf,
    /// Explicit pixi project dir. `None` = `<env_home>/pixi` (the native
    /// layout). A container bakes its pixi env at a fixed path distinct from the
    /// state root, so it supplies the location explicitly.
    pixi_dir: Option<PathBuf>,
}

/// Whether a program's spec belongs to the canonical installed baseline or a
/// transient scratch build. `clean` keeps only `Installed`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Provenance {
    Installed,
    Scratch,
}

impl Provenance {
    fn dir_name(self) -> &'static str {
        match self {
            Provenance::Installed => "installed",
            Provenance::Scratch => "scratch",
        }
    }
}

impl EnvContext {
    pub fn new(env_home: impl AsRef<Path>) -> Self {
        EnvContext { env_home: env_home.as_ref().to_path_buf(), pixi_dir: None }
    }

    /// Override the pixi project dir (default `<env_home>/pixi`).
    pub fn with_pixi_dir(mut self, dir: impl AsRef<Path>) -> Self {
        self.pixi_dir = Some(dir.as_ref().to_path_buf());
        self
    }

    pub fn env_home(&self) -> &Path {
        &self.env_home
    }

    /// `<env_home>/requirements`
    pub fn requirements_dir(&self) -> PathBuf {
        self.env_home.join("requirements")
    }

    fn provenance_dir(&self, p: Provenance) -> PathBuf {
        self.requirements_dir().join(p.dir_name())
    }

    /// Persist a program's compiler-produced envspec JSON under the given
    /// provenance. The spec is validated before it is written so a malformed
    /// spec never enters the store.
    pub fn write_spec(&self, program: &str, provenance: Provenance, json: &str) -> Result<()> {
        validate_program_name(program)?;
        EnvSpec::from_json(json)?;
        let dir = self.provenance_dir(provenance);
        std::fs::create_dir_all(&dir)
            .map_err(|e| DepsError::Env(format!("cannot create {}: {e}", dir.display())))?;
        let path = dir.join(format!("{program}.json"));
        std::fs::write(&path, json)
            .map_err(|e| DepsError::Env(format!("cannot write {}: {e}", path.display())))
    }

    /// Remove a program's spec from a provenance tier (uninstall, or promoting a
    /// scratch build to installed). A missing file is not an error.
    pub fn remove_spec(&self, program: &str, provenance: Provenance) -> Result<()> {
        validate_program_name(program)?;
        remove_if_present(&self.provenance_dir(provenance).join(format!("{program}.json")))
    }

    /// Delete every scratch spec: the `clean` reset to the installed baseline.
    pub fn clear_scratch(&self) -> Result<()> {
        let dir = self.provenance_dir(Provenance::Scratch);
        match std::fs::remove_dir_all(&dir) {
            Ok(()) => Ok(()),
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(()),
            Err(e) => Err(DepsError::Env(format!("cannot clear {}: {e}", dir.display()))),
        }
    }

    /// The installed baseline: the env's toolchain pins plus every spec under
    /// `installed/`, ordered by program name for deterministic downstream
    /// rendering. `clean` re-solves exactly this set, so the pins survive it.
    pub fn gather_installed(&self) -> Result<Vec<EnvSpec>> {
        let mut specs = read_specs(&self.provenance_dir(Provenance::Installed))?;
        if let Some(toolchain) = self.read_toolchain()? {
            specs.push(toolchain);
        }
        Ok(specs)
    }

    /// The full declared world: the installed baseline then `scratch`, each
    /// ordered by program name. The total order matters: the union feeds a
    /// solve-cache marker compared as text, so a non-deterministic order churns.
    pub fn gather(&self) -> Result<Vec<EnvSpec>> {
        let mut specs = self.gather_installed()?;
        specs.extend(read_specs(&self.provenance_dir(Provenance::Scratch))?);
        Ok(specs)
    }

    /// `<env_home>/requirements/toolchain.json` -- the env's initialization pins
    /// (from `--lang`). Env-level toolchain configuration, deliberately kept
    /// OUTSIDE the per-program `installed/`/`scratch/` dirs so it is not mistaken
    /// for a program (e.g. it never appears in `program_names`).
    fn toolchain_path(&self) -> PathBuf {
        self.requirements_dir().join("toolchain.json")
    }

    /// Persist the env's toolchain-pin spec (a languages-only EnvSpec), validated
    /// before writing. Folded into every gather so the in-env agent re-solves
    /// with the same pins the manager used.
    pub fn write_toolchain(&self, json: &str) -> Result<()> {
        EnvSpec::from_json(json)?;
        self.write_requirements_file(&self.toolchain_path(), json)
    }

    /// The env's toolchain-pin spec, if one has been persisted.
    fn read_toolchain(&self) -> Result<Option<EnvSpec>> {
        read_optional_spec(&self.toolchain_path())
    }

    /// Create the requirements dir and write `json` to a file directly under it
    /// (the toolchain pin, the abi lock). Callers own any pre-write validation.
    fn write_requirements_file(&self, path: &Path, json: &str) -> Result<()> {
        let dir = self.requirements_dir();
        std::fs::create_dir_all(&dir)
            .map_err(|e| DepsError::Env(format!("cannot create {}: {e}", dir.display())))?;
        std::fs::write(path, json)
            .map_err(|e| DepsError::Env(format!("cannot write {}: {e}", path.display())))
    }

    /// `<env_home>/requirements/abi-lock.json` -- the interpreter minor-version
    /// pins that protect the shim ABI. Kept OUTSIDE `installed/`/`scratch/` (like
    /// `toolchain.json`) so it is never mistaken for a program.
    fn abi_lock_path(&self) -> PathBuf {
        self.requirements_dir().join("abi-lock.json")
    }

    /// Append the shim-ABI pin to a solve set, if one is recorded. Called only by
    /// the in-env solve entry points (`sync`/`clean`), so a make-time or clean
    /// re-solve holds the interpreter minor the shims were built against. It is
    /// deliberately NOT folded into `gather`/`gather_installed`: the manager's
    /// provision solve reads those, and provision is allowed to move the
    /// interpreter (it rebuilds the shims), so the pin must not gate it.
    fn append_abi_lock(&self, specs: &mut Vec<EnvSpec>) -> Result<()> {
        if let Some(abi_lock) = self.read_abi_lock()? {
            specs.push(abi_lock);
        }
        Ok(())
    }

    /// The env's shim-ABI pin spec, if one has been recorded.
    fn read_abi_lock(&self) -> Result<Option<EnvSpec>> {
        read_optional_spec(&self.abi_lock_path())
    }

    /// Record the shim-ABI pin from this env's freshly solved conda prefix: pin
    /// each interpreter (python, r-base) to the MINOR version `morloc init` just
    /// built its shims against. Folded into every later solve (see
    /// `gather_installed`), so a dependency that would bump an interpreter minor
    /// fails the solve legibly rather than silently breaking the shim ABI.
    ///
    /// The manager calls this at provision time ONLY -- after the solve that
    /// rebuilds the shims -- so a re-provision is free to move the interpreter;
    /// the in-env agent's make-time solves are the ones the recorded pin gates (see
    /// `append_abi_lock`). If the env has no ABI-relevant interpreter, any stale
    /// lock is removed.
    pub fn record_abi_lock(&self, morloc_version: &str) -> Result<()> {
        let prefix = crate::abi::conda_prefix(&self.pixi_dir());
        match crate::abi::abi_lock_spec(&prefix, morloc_version) {
            Some(spec) => {
                let json = serde_json::to_string(&spec)
                    .map_err(|e| DepsError::Env(format!("cannot serialize abi lock: {e}")))?;
                self.write_requirements_file(&self.abi_lock_path(), &json)
            }
            None => remove_if_present(&self.abi_lock_path()),
        }
    }

    /// The pixi project directory (holds `pixi.toml`/`pixi.lock` and the solved
    /// prefix): the explicit override if set, else `<env_home>/pixi`.
    pub fn pixi_dir(&self) -> PathBuf {
        self.pixi_dir
            .clone()
            .unwrap_or_else(|| self.env_home.join("pixi"))
    }

    /// The environment's name, taken from its home directory's final component
    /// (env homes live at `.../envs/<name>`). Used only as the pixi workspace
    /// name, so a fallback is harmless.
    fn env_name(&self) -> String {
        self.env_home
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("morloc-env")
            .to_string()
    }

    /// Render the given specs to a pixi manifest (the merged, backend-neutral
    /// requirement set lowered to `pixi.toml` text). Pure: no solve, no I/O.
    pub fn rendered_manifest(&self, specs: &[EnvSpec], inputs: &SolveInputs) -> String {
        let name = self.env_name();
        let requirements = crate::pixi::resolve_requirements(&crate::pixi::PixiManifestInput {
            env_name: name.as_str(),
            platform: inputs.platform,
            channels: inputs.channels,
            specs,
            lang_support: inputs.lang_support,
        });
        crate::pixi::render_manifest(&requirements)
    }

    /// Render the given specs to a pixi manifest and solve it into this env's
    /// pixi prefix, returning the captured activation env-map. Does NOT persist
    /// specs or rebuild the morloc runtime -- callers own those steps.
    pub fn solve_world(
        &self,
        specs: &[EnvSpec],
        inputs: &SolveInputs,
    ) -> Result<Vec<(String, String)>> {
        let manifest = self.rendered_manifest(specs, inputs);
        let pixi_dir = self.pixi_dir();
        crate::pixi::write_manifest(&pixi_dir, &manifest)?;
        crate::pixi::solve(&pixi_dir, inputs.pixi_bin)?;
        crate::pixi::capture_activation(&pixi_dir, inputs.pixi_bin)
    }

    /// Merge a program's spec into the declared world under `provenance`,
    /// re-solve, and install. On failure the tentative spec is rolled back so the
    /// STORE is never left half-applied (the no-mutation rule), and the error is
    /// returned as-is -- `solve_world` failures include setup errors (an
    /// unwritable prefix, a missing pixi, etc.), not only unsatisfiability, so
    /// they must NOT be blanket-relabeled as a dependency conflict. (A true UNSAT
    /// is surfaced as `DepsError::Conflict` by the solve layer once it parses the
    /// solver's output.)
    ///
    /// The store rollback does NOT undo a partial pixi prefix install (pixi links
    /// into the shared prefix as it solves). A stale `pixi.toml`/`pixi.lock` or a
    /// partially-linked prefix is reconciled on the next successful re-solve
    /// (`sync` or `clean`), which re-renders from the rolled-back store.
    pub fn sync(
        &self,
        program: &str,
        provenance: Provenance,
        spec_json: &str,
        inputs: &SolveInputs,
    ) -> Result<Vec<(String, String)>> {
        self.write_spec(program, provenance, spec_json)?;
        let mut specs = self.gather()?;
        self.append_abi_lock(&mut specs)?;
        match self.solve_world(&specs, inputs) {
            Ok(activation) => Ok(activation),
            Err(e) => {
                let _ = self.remove_spec(program, provenance);
                Err(e)
            }
        }
    }

    /// The names (file stems) of every program with a stored spec, sorted and
    /// deduplicated. Used to attribute a conflict.
    pub fn program_names(&self) -> Result<Vec<String>> {
        let mut names = program_stems(&self.provenance_dir(Provenance::Installed))?;
        names.extend(program_stems(&self.provenance_dir(Provenance::Scratch))?);
        names.sort();
        names.dedup();
        Ok(names)
    }

    /// Reset the declared world to the installed baseline: drop all scratch
    /// specs, then re-solve so the prefix reflects only installed programs.
    pub fn clean(&self, inputs: &SolveInputs) -> Result<Vec<(String, String)>> {
        self.clear_scratch()?;
        let mut specs = self.gather_installed()?;
        self.append_abi_lock(&mut specs)?;
        self.solve_world(&specs, inputs)
    }
}

/// Read + parse an optional on-disk spec file (the toolchain pin, the abi lock):
/// a missing file is `None`, a present one is validated.
fn read_optional_spec(path: &Path) -> Result<Option<EnvSpec>> {
    match std::fs::read_to_string(path) {
        Ok(text) => Ok(Some(EnvSpec::from_json(&text)?)),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(None),
        Err(e) => Err(DepsError::Env(format!("cannot read {}: {e}", path.display()))),
    }
}

/// Remove a file, tolerating its absence (an uninstall, or clearing a stale lock).
fn remove_if_present(path: &Path) -> Result<()> {
    match std::fs::remove_file(path) {
        Ok(()) => Ok(()),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(()),
        Err(e) => Err(DepsError::Env(format!("cannot remove {}: {e}", path.display()))),
    }
}

/// The `*.json` files directly under `dir`, sorted by name for a stable order.
/// A missing directory yields an empty list.
fn json_paths(dir: &Path) -> Result<Vec<PathBuf>> {
    let entries = match std::fs::read_dir(dir) {
        Ok(e) => e,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(Vec::new()),
        Err(e) => return Err(DepsError::Env(format!("cannot read {}: {e}", dir.display()))),
    };
    let mut files: Vec<PathBuf> = entries
        .flatten()
        .map(|e| e.path())
        .filter(|p| p.extension().and_then(|x| x.to_str()) == Some("json"))
        .collect();
    files.sort();
    Ok(files)
}

/// Read and parse every `*.json` in `dir`, sorted for a stable order.
fn read_specs(dir: &Path) -> Result<Vec<EnvSpec>> {
    let files = json_paths(dir)?;
    let mut specs = Vec::with_capacity(files.len());
    for path in files {
        let text = std::fs::read_to_string(&path)
            .map_err(|e| DepsError::Env(format!("cannot read {}: {e}", path.display())))?;
        specs.push(EnvSpec::from_json(&text)?);
    }
    Ok(specs)
}

/// The `.json` file stems (program names) directly under `dir`.
fn program_stems(dir: &Path) -> Result<Vec<String>> {
    Ok(json_paths(dir)?
        .into_iter()
        .filter_map(|p| p.file_stem().and_then(|s| s.to_str()).map(str::to_string))
        .collect())
}

/// A program name is used as a bare file stem in the store, so it must not
/// contain path separators or traversal segments -- otherwise a crafted name
/// could write outside the store.
fn validate_program_name(program: &str) -> Result<()> {
    let bad = program.is_empty()
        || program == "."
        || program == ".."
        || program.contains('/')
        || program.contains('\\')
        || program.contains(std::path::MAIN_SEPARATOR);
    if bad {
        return Err(DepsError::Env(format!(
            "invalid program name '{program}': must be non-empty and contain no path separators"
        )));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    // Minimal valid v2 specs (the version guard requires >= 2); the program name
    // is carried by the file name, not the spec body.
    fn spec(langs: &str) -> String {
        format!(
            r#"{{"envspec_version":2,"morloc_version":"0.0.0","languages":[{langs}]}}"#
        )
    }

    fn ctx() -> (tempfile::TempDir, EnvContext) {
        let dir = tempfile::tempdir().unwrap();
        let ctx = EnvContext::new(dir.path());
        (dir, ctx)
    }

    #[test]
    fn gather_unions_installed_and_scratch_in_stable_order() {
        let (_d, ctx) = ctx();
        ctx.write_spec("bravo", Provenance::Installed, &spec(r#"{"lang":"py"}"#)).unwrap();
        ctx.write_spec("alpha", Provenance::Installed, &spec(r#"{"lang":"cpp"}"#)).unwrap();
        ctx.write_spec("zeta", Provenance::Scratch, &spec(r#"{"lang":"r"}"#)).unwrap();

        let all = ctx.gather().unwrap();
        assert_eq!(all.len(), 3);
        // installed (alpha, bravo by file-name order) then scratch (zeta)
        assert_eq!(all[0].languages[0].lang, "cpp"); // alpha
        assert_eq!(all[1].languages[0].lang, "py"); // bravo
        assert_eq!(all[2].languages[0].lang, "r"); // zeta

        let installed = ctx.gather_installed().unwrap();
        assert_eq!(installed.len(), 2);
    }

    #[test]
    fn clear_scratch_resets_to_installed_baseline() {
        let (_d, ctx) = ctx();
        ctx.write_spec("keep", Provenance::Installed, &spec(r#"{"lang":"py"}"#)).unwrap();
        ctx.write_spec("temp", Provenance::Scratch, &spec(r#"{"lang":"r"}"#)).unwrap();
        assert_eq!(ctx.gather().unwrap().len(), 2);

        ctx.clear_scratch().unwrap();
        let after = ctx.gather().unwrap();
        assert_eq!(after.len(), 1);
        assert_eq!(after[0].languages[0].lang, "py");
    }

    #[test]
    fn gather_is_empty_when_no_store_exists() {
        let (_d, ctx) = ctx();
        assert!(ctx.gather().unwrap().is_empty());
        assert!(ctx.gather_installed().unwrap().is_empty());
    }

    #[test]
    fn write_spec_rejects_malformed_json() {
        let (_d, ctx) = ctx();
        let r = ctx.write_spec("bad", Provenance::Scratch, "{ not json");
        assert!(r.is_err());
    }

    #[test]
    fn toolchain_folds_into_baseline_but_is_not_a_program() {
        let (_d, ctx) = ctx();
        ctx.write_spec("prog", Provenance::Installed, &spec(r#"{"lang":"py"}"#)).unwrap();
        ctx.write_toolchain(&spec(r#"{"lang":"py","constraint":"==3.12"}"#)).unwrap();

        // the pins are part of the installed baseline (so `clean` keeps them)...
        assert_eq!(ctx.gather_installed().unwrap().len(), 2);
        assert_eq!(ctx.gather().unwrap().len(), 2);
        // ...but they are NOT a program: never surfaced in conflict attribution.
        assert_eq!(ctx.program_names().unwrap(), vec!["prog".to_string()]);
    }

    #[test]
    fn abi_lock_gates_only_the_in_env_solve_not_the_gather() {
        let (_d, ctx) = ctx();
        ctx.write_spec("prog", Provenance::Installed, &spec(r#"{"lang":"py"}"#)).unwrap();
        std::fs::create_dir_all(ctx.requirements_dir()).unwrap();
        std::fs::write(
            ctx.abi_lock_path(),
            spec(r#"{"lang":"py","constraint":"3.12.*"}"#),
        )
        .unwrap();

        // The gather the MANAGER's provision reads must NOT include the pin (so a
        // re-provision is free to move the interpreter and rebuild the shims).
        assert_eq!(ctx.gather_installed().unwrap().len(), 1);
        assert_eq!(ctx.gather().unwrap().len(), 1);
        // The in-env solve set (sync/clean) DOES include it.
        let mut solve_set = ctx.gather_installed().unwrap();
        ctx.append_abi_lock(&mut solve_set).unwrap();
        assert_eq!(solve_set.len(), 2);
        // ...but the pin is NOT a program: never surfaced in conflict attribution.
        assert_eq!(ctx.program_names().unwrap(), vec!["prog".to_string()]);
    }

    #[test]
    fn record_abi_lock_pins_solved_interpreter_and_clears_when_absent() {
        let (_d, ctx) = ctx();
        let prefix = crate::abi::conda_prefix(&ctx.pixi_dir());
        let meta = prefix.join("conda-meta");
        std::fs::create_dir_all(&meta).unwrap();
        std::fs::write(meta.join("python-3.12.5-0.json"), r#"{"name":"python","version":"3.12.5"}"#).unwrap();

        ctx.record_abi_lock("0.99.0").unwrap();
        let mut solve_set = Vec::new();
        ctx.append_abi_lock(&mut solve_set).unwrap();
        assert_eq!(solve_set.len(), 1);
        assert_eq!(solve_set[0].languages[0].lang, "py");
        assert_eq!(solve_set[0].languages[0].constraint.as_deref(), Some(">=3.12,<3.13"));

        // No interpreter in the prefix -> a subsequent record clears the stale lock.
        std::fs::remove_dir_all(&meta).unwrap();
        std::fs::create_dir_all(&meta).unwrap();
        ctx.record_abi_lock("0.99.0").unwrap();
        let mut after = Vec::new();
        ctx.append_abi_lock(&mut after).unwrap();
        assert!(after.is_empty());
    }

    #[test]
    fn write_spec_rejects_path_traversal_names() {
        let (_d, ctx) = ctx();
        let good = spec(r#"{"lang":"py"}"#);
        for name in ["../escape", "sub/child", "..", ".", ""] {
            assert!(
                ctx.write_spec(name, Provenance::Scratch, &good).is_err(),
                "expected '{name}' to be rejected"
            );
        }
        // a plain name is accepted
        assert!(ctx.write_spec("ok-name", Provenance::Scratch, &good).is_ok());
    }

    #[test]
    fn write_spec_rejects_stale_schema_version() {
        let (_d, ctx) = ctx();
        // v1 predates the `source` field; the store must not accept it.
        let r = ctx.write_spec(
            "old",
            Provenance::Scratch,
            r#"{"envspec_version":1,"morloc_version":"0.0.0"}"#,
        );
        assert!(r.is_err());
    }

    #[test]
    fn remove_and_promote_semantics() {
        let (_d, ctx) = ctx();
        ctx.write_spec("p", Provenance::Scratch, &spec(r#"{"lang":"py"}"#)).unwrap();
        // promote: write to installed, drop from scratch
        ctx.write_spec("p", Provenance::Installed, &spec(r#"{"lang":"py"}"#)).unwrap();
        ctx.remove_spec("p", Provenance::Scratch).unwrap();
        assert_eq!(ctx.gather().unwrap().len(), 1);
        assert_eq!(ctx.gather_installed().unwrap().len(), 1);
        // removing a missing spec is a no-op
        ctx.remove_spec("p", Provenance::Scratch).unwrap();
    }
}
