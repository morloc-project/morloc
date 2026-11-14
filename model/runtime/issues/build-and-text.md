# Build tooling and stale text

- **Build swap not atomic.** [read] Violates ART-3. `swapIn`
  (library/Morloc/ProgramBuilder/Build.hs) deletes the old build, then
  renames staging in; a kill between leaves no build.
- **A killed build leaves its staging tree forever.** [reproduced,
  2026-10-17] Spec gap beside ART-3: nothing says what a failed build may
  leave behind. `morloc make` stages into `<target>.tmp.<pid>`
  (`ensureStagingDir`, library/Morloc/ProgramBuilder/Build.hs) and removes
  it only on an error the build itself catches; a SIGKILL (memguard, a
  timeout) leaves it, and later builds have other pids, so none removes it.
  The golden tree holds 8, dated 2026-09-02 to 2026-10-17, two of them
  from a suite run stopped on 2026-10-17.
- Stale text: `morloc-nexus --help` says launchers pass the wrapper (they
  pass the manifest); ProgramBuilder/Install.hs names
  `exe/<name>/manifest.json` (real: `exe/<name>/<name>-build/manifest.json`);
  morloc-runtime-types/src/schema.rs:39 and data/morloc/morloc.h:161 call
  Int an array of limbs (one limb is inline); stream.rs:152 says the salt is
  per process (per registry).
- [read] morloc-runtime/src/ipc.rs is dead code: `send_and_receive` (:25)
  and `read_packet` are public and uncalled (callers use ipc_ffi.rs), and
  `read_packet` underflows `offset - 32` (:80). Delete the module.
- **morloc.h drifts from the Rust runtime and nothing checks it.** [read]
  The header's `morloc_tag_test_expression_s` (morloc.h:689-692) holds
  `subject, constructor` (two expression pointers); `MorlocTagTestExpression`
  (manifest_ffi.rs:247) holds `subject, tag: u8`. The header has no
  ctor-field or ctor-make struct, and its expression union lacks
  `ctor_field_expr` and `ctor_make_expr`. morloc.h:436-442 lacks metadata
  kinds 0x08 FOOTER_STATUS and 0x09 PROGRAM_FILE (runtime-types
  packet.rs:191, :207); kind 0x02 XXHASH is defined and unused. c_abi_layout.rs
  `expression_structs_match_the_header` (:525) checks neither struct, and
  `expression_union_is_wide_enough` (:672) compares only size and
  alignment, so missing union members pass. Neither side tests that
  BaseTypes.hs `mlcKind*` (:195-196) equals morloc.h `MLC_KIND_*`. Fix:
  mirror the structs and kinds, and check members and kinds in tests.
- **ABI version never gated.** [read] Spec gap: no item covers the ABI
  version. library/Morloc/Abi.hs:18-22 says provisioning compares
  `morloc_abi_version()` and `morloc-nexus --abi-version` against
  `abiVersion` and refuses a mismatch; nothing does. `abiVersion` is only
  printed (executable/Subcommands.hs:795) and unit-tested
  (test-suite/AbiTests.hs). tooling/morloc-manager mim/src/provision.rs:8
  calls the gate deferred and relies on one release tag. A runtime built
  from another release's source loads silently. [reproduced] A program
  built before `morloc init` installs a newer runtime keeps its generated
  pool code against the new libraries: after 7205b50c an older R pool fails
  every call with `could not find function ".mlc_entry"`, which the new
  librmorloc dispatch calls. Nothing marks a built program with the runtime
  it was built for.
- **Crate versions claim a lockstep they lack.** [read]
  morloc-runtime-types (0.87.0) and rustmorloc (0.95.0) say "Version
  locked with morloc-runtime" (0.112.0); scripts/bump-version.sh:36 bumps
  only nexus, manifest and runtime.
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

- **The Rust source type-variable mask is keyed by the bare source name.**
  [read] Two Rust files that each define `f` share one mask (Rust.hs
  `buildSrcTypeVarMask`). Crate-root `include!` already forbids the two
  definitions; for source-type-forms, which owns per-file scope.
- **A sourced file's identity is its spelling, not the file.** [read,
  2026-11-02; needs a design] Python bindings and import paths are now
  injective in the written path (case kept, normalised, plane-relative),
  but identity is still lexical and settled per backend: `sub/../x.py`,
  a symlink, or an absolute and a relative spelling of one file are two
  files (loaded twice, or bound under two names); the C++ member includes
  headers by spelling, source hooks run per spelling, and Nexus source
  hashes and Realize/EffectBoundary `sameSource` compare raw paths. A
  Python file with no extension cannot be imported (its path takes the
  dotted branch). A dotted name resolves under the first source root that
  has it, so a program `x/h.py` answers for a plane `x/h.py`, and a
  program file may be found in the run-time working directory. Fixing it
  needs one identity, settled where a Source is made, and a root model
  that holds under `morloc install --build` (cwd inside the plane),
  `--build-dir` and `-o dir/name` builds, and a moved build: an attempt on
  2026-11-02 that classified roots from the build's working directory made
  an installed program load from the plane instead of its mirror.
  [reproduced 2026-11-02, UNSOUND; design in
  /work/plans/error-clusters/briefs/source-identity-design.md] SRC-8 checks
  one file while the pool loads another: a `--build-dir` program run from a
  directory holding a decoy `x.py` loaded the decoy; a `cp -a` copy of a
  make build loads its own edited `x.py` while SRC-8 checks the original,
  and refuses to run once the original is moved. By reading, an installed
  program loads its Python/R code from the plane, not its `exe/<module>`
  mirror, which SRC-8 records. Python also appends the run-time working
  directory to `sys.path`, so a sourced file's bare sibling import can be
  answered from wherever the program runs; R pools have the same
  fallback. `morloc make -o ../out/prog` with MORLOC_BUILD_HOOK set fails
  "invalid program name": the `-o` value becomes the program key.
  `@datafile` paths resolve against the build root, not the declaring
  module's directory (Reduce.hs).
- **`cargo clippy` fails on morloc-runtime.** [reproduced 2026-10-15]
  `clippy::not_unsafe_ptr_arg_deref` (deny by default) at
  morloc-runtime/src/stream.rs `shared_write_subpacket` (:4342): a safe
  public function takes an `AbsPtr` (`*mut u8`) it passes on to
  `voidstar::flatten_portable_in_place`.
  The crate's own lint denials (json.rs casts) go unchecked while this
  stops the run. About 60 safe `pub fn`s in morloc-runtime take raw
  pointers (heuristic).
- [reproduced] `morloc init -f` exits 0 when data/lang/cpp/cppmorloc.cpp
  fails to compile, leaving the previous libcppmorloc.a installed. Strict
  conda mode now aborts; outside it the shim build is an `optionalStep`
  that only warns (SystemConfig.hs:320).
- [observed once] A `morloc make` run with the current compiler on PATH
  (`~/.local/bin/morloc`, built 2026-10-17) produced a Rust pool whose code
  only morloc 0.100.2 emits (`rustmorloc::FrameGuard`, removed in
  7205b50c). The environment sets `MORLOC_BIN=/opt/morloc-runtime/morloc`,
  which is 0.100.2 (2026-09-03), and `morloc make` provisions through
  `MORLOC_BUILD_HOOK` (mim sync), so some step ran the older compiler. A
  rebuild in the same directory, and a build in a fresh one, both used the
  current compiler. Repro attempt: the #179 program under
  /work/plans/issues-2026-10-05/r5-rust-loop-clone/mapped, built in a new
  directory right after a compiler reinstall.
- **A refused rebuild leaves its staging tree.** [reproduced, 2026-10-21]
  Against ART-2/ART-3. With a plain `q-build/` (no marker) or a symlink
  `r-build`, `morloc make -o q main.loc` fails "Refusing to overwrite
  '.../q-build': not a morloc build directory" and leaves
  `q-build.tmp.<pid>/`. Build.hs:198 `liftIO $ swapIn ...` raises an
  IOException (`ioError . userError`, :345) that the MorlocMonad
  `catchError` (:200) does not catch, so `discardStagingDir` never runs;
  any IO exception in `buildAll` escapes the same way. Repro: /work/plans/model-audit-2026-10-21/repro/I1/p1/.
- **A failed owner record leaks an unsweepable run directory.** [read,
  2026-10-21] morloc-nexus/src/process.rs make_tmpdir (:1691, the owner
  write at :1715-1717): if writing or renaming `.owner.tmp` fails it
  returns Err after mkdtemp, leaving `/tmp/morloc.XXXXXX`; the startup
  sweep skips directories without `.owner` (process.rs:1769). Remove the directory on that path.
- **Strings crossing two runtime-type copies.** [read, 2026-10-23]
  rustmorloc statically links its own copy of morloc-runtime-types beside
  the one in libmorloc.so. Strings and CSchema built by one copy and freed by
  the other change allocator if a user's Rust pool sets a
  `#[global_allocator]`. Strings the caller frees now come from `malloc`
  (malloc_cstr), but a CSchema is Box/CString/Vec. Manifest expression
  strings (manifest_ffi.rs ~815, ~1048, ~1277) are never freed (the nexus
  is short-lived).
- **`morloc init` rebuilds the runtime unless incremental, whatever `-f`
  says.** [read 2026-11-04, from mim's list] SystemConfig.hs:181 is
  `force == ForceOverwrite || not runtimeBuilt || not incremental`; the
  comments at :172-179 and :327-330 describe other behaviour.
- **Dead C++ search paths.** [read 2026-11-04, from mim's list]
  CodeGenerator/Pools/CAbi/Members/Cpp.hs:2876 and :2890-2891 search
  `STATE/src/<base>/...`, which nothing writes.
- **Stale comments on locations.** [read 2026-11-04, from mim's list]
  Namespace/State.hs:878-890 and SystemConfig.hs:186-191 place expose
  targets and include/lib under `$MORLOC_HOME` (now the state root's
  modules dir, `exposeDirsFor`, Module.hs:289); morloc-nexus cli.rs:331
  says exe defaults to `$MORLOC_HOME/exe` (it is under STATE,
  Config.hs:96); Build.hs:225 names the build hook `mim-env`. The router
  scan path and transport comments in nexus cli.rs/main.rs are unchecked.
