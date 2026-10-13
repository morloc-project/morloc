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
- **A pool's own newer crate pins never reach the environment lock.**
  [read] Gap: none stated. Build/CargoLock.hs `mergeLockFiles` re-bases the
  environment lock on the runtime's Cargo.lock and `mergeCargoLocks` keeps
  the base's table for every crate name the base has, so a version cargo had
  to bump for one pool (lib/biolib salmon-rust: cf1-rs needs anyhow
  ^1.0.104, the runtime pins 1.0.102) is dropped. Since pool builds are no
  longer forced offline this no longer fails; each rebuild of that program
  re-resolves the bumped crate online instead, and may pick a newer version
  than the last build did. Options (DA review 2026-10-13): start each pool
  from its own previous pools/rust/Cargo.lock and merge only new names from
  the environment lock (keeps the runtime's authority); or keep the newer
  pin per semver-compatible line environment-wide, which needs dependency
  references rewritten on replacement, numeric semver comparison and a
  same-`source` check, and ratchets versions for every program.
- **The environment lock keeps a table for every pool ever built.** [read]
  `mergeCargoLocks` adds any crate name the base lacks, including each
  program's `pool_<hash>` crate and its local path crates, so
  $MORLOC_STATE/cache/rust-env.lock grows by one table per program built
  and never shrinks.
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
  struct themselves gets a duplicate definition. Candidates: (a) the form
  names its file (`record Rust => FragHits = "BioFragHits" from "bio.rs"`,
  a syntax change); (b) the rule above restricted to C++ and Rust `record`
  forms of user modules, accepting the duplicate-definition break; (c) leave
  it to the library (current workaround).

- **`choose_workers` is dead and its doc is stale.** [read] Gap: none;
  stale text. morloc-runtime-types/src/compression.rs:596 `choose_workers`
  (one worker per 4 MiB) has no caller outside its own unit test, and the
  doc of `compress_payload_zstd` (~627) says threading is decided "via
  `choose_workers`"; every path uses `frame_workers()`
  (`MORLOC_FRAME_WORKERS`, else `available_parallelism` capped at 16).
