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
- **morloc.h expression structs drift from Rust.** [read] The header's
  `morloc_tag_test_expression_s` holds `subject, constructor` (two
  expression pointers); `MorlocTagTestExpression` in manifest_ffi.rs holds
  `subject, tag: u8`. The header has no ctor-field or ctor-make struct and
  its expression union lacks those members. `c_abi_layout.rs` checks neither
  struct, so nothing caught it. Fix: mirror both and add them to
  `expression_structs_match_the_header`.
- **ABI version never gated.** [read] Spec gap: no item covers the ABI
  version. library/Morloc/Abi.hs:18-22 says provisioning compares
  `morloc_abi_version()` and `morloc-nexus --abi-version` against
  `abiVersion` and refuses a mismatch; nothing does. `abiVersion` is only
  printed (executable/Subcommands.hs:734) and unit-tested
  (test-suite/AbiTests.hs). tooling/morloc-manager mim/src/provision.rs:8
  calls the gate deferred and relies on one release tag. A runtime built
  from another release's source loads silently.
- **Crate versions claim a lockstep they lack.** [read]
  morloc-runtime-types (0.87.0) and rustmorloc (0.95.0) say "Version
  locked with morloc-runtime" (0.109.0); scripts/bump-version.sh:36 bumps
  only nexus, manifest and runtime.
- **`mlcKind*` agreement with morloc.h untested on the Haskell side.**
  [read] library/Morloc/BaseTypes.hs:196 requires `mlcKind*` to equal
  morloc.h `MLC_KIND_*`; no test in test-suite/ compares them.
