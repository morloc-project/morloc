# Install

The gap these share: no spec says that installing a commit from its remote
and installing a clone of it at that commit give the same result.

- **A local git repo installed without a selector copies the working
  tree.** [read, 2026-11-13] library/Morloc/Module.hs:1700
  `installLocalGitRepoIO` with `LatestDefaultBranch` runs
  `copyAllFiltered` on the source, so uncommitted and untracked files are
  installed and the recorded state names no commit.
- **installed_hash records the requested hash, not the fetched commit.**
  [read, 2026-11-13] library/Morloc/Module.hs:1111 `installedSelfHash` is
  the snapshot or pin entry; an unpinned install records null, and nothing
  reads `git rev-parse HEAD` of the clone.
- **The two install filters differ.** [read, 2026-11-13]
  library/Morloc/ProgramBuilder/Install.hs: `copyAllFiltered` (local
  sources) drops morloc build directories, `cleanIgnoredFiles` (after a
  clone) does not, so a committed `*-build/` directory is installed from a
  remote but not from a local copy.
- **A dependency installed during `morloc make` is always fetched over
  HTTPS.** [read, 2026-11-15] library/Morloc/Module.hs:550 `autoInstallDep`
  passes `HttpsProtocol`, so a private dependency reachable only over ssh
  fails there although `morloc install --ssh` succeeds.
- **Pins are passed only down the dependency walk.** [read, 2026-11-15]
  library/Morloc/Module.hs:1042 passes `pinMap'` to each child, so sibling
  subtrees never see each other's pins: two pins of one module to
  different hashes meet only at the coherence check, and under `--force`
  the last one visited is kept silently (speculative, not reproduced).
- **A build can read a module while `--force` replaces it.** [read,
  2026-11-19] PKG-11. Imports check the module's record and then read its
  files without a lock, so a `morloc make` running during a forced
  reinstall may read part of the old or new tree. A shared lock taken by
  the frontend for the parse would close it.
- **A setup script's effects survive a failed install.** [gap, 2026-11-19]
  PKG-11. Rollback restores the module tree, exposures and record, but not
  what a `setup` script did (pip installs, built libraries).
- **A file's lack of a header is decided by a text scan.** [read,
  2026-11-19] MOD-1. `keyFileModules` (Frontend/API.hs) treats a lone
  `module main` as anonymous only when no line starts with `module`, so a
  file that parses header-less but has such a line in a comment block is
  read as named `main`.
- **C++ library headers are searched at a path the plane never has.**
  [read, 2026-11-15] CodeGenerator/Pools/CAbi/Members/Cpp.hs
  `getHeaderPaths` tries `<state>/src/<base>/<base>`; installed modules
  live at `<state>/src/morloc/plane/<library>/<name>`.
- **Misspelled package.yaml keys are ignored.** [read, 2026-11-15] PKG-1.
  The PackageMeta parser (Namespace/State.hs) ignores unknown keys, so
  `libary: weena` installs as if no library were given and fails with
  "gives no library" rather than naming the typo; `name` is not validated
  where it is read.
