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
