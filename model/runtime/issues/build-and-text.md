# Build tooling and stale text

- **Build swap not atomic.** [read] Violates ART-3. `swapIn`
  (library/Morloc/ProgramBuilder/Build.hs) deletes the old build, then
  renames staging in; a kill between leaves no build.
- Stale text: `morloc-nexus --help` says launchers pass the wrapper (they
  pass the manifest); ProgramBuilder/Install.hs names
  `exe/<name>/manifest.json` (real: `exe/<name>/<name>-build/manifest.json`);
  schema.rs:39 and data/morloc/morloc.h:164 call Int an array of limbs (one
  limb is inline); schema.rs:981 promises 64-byte alignment for [Enum];
  stream.rs:159 says the salt is per process (per registry); morloc.h:497-504
  lacks metadata kind 0x08; kind 0x02 is defined and unused.
- [read] morloc-runtime/src/ipc.rs `send_and_receive`: public, uncalled,
  underflows `offset - 32`. Delete.
