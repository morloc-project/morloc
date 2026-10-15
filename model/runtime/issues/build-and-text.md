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
- **Rust crates are resolved per pool, outside the environment's solve.**
  [read] Rust pools resolve rust-deps one pool at a time against an
  environment lock the compiler maintains (Build/CargoLock.hs), while
  Python, R and C++ packages are solved by mim into one solved set per
  environment (mim model/dependencies.md DEP-3, DEP-7; DEP-8 leaves crates to
  cargo). Consequences: a version cargo had to bump for one pool (lib/biolib
  salmon-rust: anyhow ^1.0.104 against the runtime's 1.0.102) is dropped
  when the merge re-bases on the runtime's lock, so each rebuild re-resolves
  it online (violates the intent of DEP-7: versions move only when forced,
  and stay); and the environment lock keeps a table for every program ever
  built (each `pool_<hash>` crate and its local path crates). Ruled
  2026-10-14: crates join the single solved environment. One lock per
  environment, resolved by cargo over a workspace of the runtime crates and
  every program's rust-deps; every pool builds against it, with no
  per-pool merge. For crates, "one package, one version" means one version
  per crate per semver-compatible line: cargo links several major lines of
  one crate into one binary, and the ecosystem relies on that during major
  migrations, so one version per crate name would make ordinary crate
  combinations unsatisfiable. Conflicting exact pins fail the build. Needs
  its own session: DEP-3/DEP-8 wording in mim's model, the solve in mim or
  the build hook, removal of the compiler's lock merge.
- **A native type form does not bring in the file that defines the type.**
  [reproduced, 0.109.0; C++ in lib/biolib FINDINGS F7, Rust in rnaseq R26]
  Gap in NATIVE-3: it says the user defines the struct and links it with
  `record Rust => R = "R"`, not how its definition reaches the pool. A pool
  includes a module's sources in its language only when it runs one of that
  module's manifolds (`stateSources`, Emit.hs `findSources`); lib/biolib
  bio-rust maps `record Rust => FragHits = "BioFragHits"` (defined in
  bio.rs), and a program calling only bioquant-rust functions fails in
  cargo "cannot find type `BioFragHits`". Workaround in lib/biolib: an extra
  `bioRustAnchor` argument that pulls bio.rs in. Needs a ruling. Rejected
  after DA review: including the declaring module's sources for every type
  form the pool mentions. Root forms (`type Rust => Int = "i64"`) would
  include core.rs in every Rust pool, whose flat namespace then clashes with
  user files (E0252/E0428); R pools `source()` into one environment, so an
  extra file can silently shadow a function; and a user who defined the
  struct themselves gets a duplicate definition. Direction ruled
  2026-10-14: language-specific type forms move into `source` blocks, so a
  form travels with the file it is declared from; `type L =>` keeps working
  with a deprecation warning. Proposal and open questions:
  /work/plans/source-type-forms.md.

- **`cargo clippy` fails on morloc-runtime.** [reproduced 2026-10-15]
  `clippy::not_unsafe_ptr_arg_deref` (deny by default) at
  morloc-runtime/src/stream.rs:3990: a safe public function passes a raw
  pointer it may dereference to `voidstar::flatten_portable_in_place`.
  The crate's own lint denials (json.rs casts) go unchecked while this
  stops the run.
