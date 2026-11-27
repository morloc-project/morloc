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
- [read, 2026-11-15] A generated record missing from the pool's record
  map ends the compiler with a Haskell `error`, not a sourced message
  (CodeGenerator/Pools/CAbi/Members/Cpp.hs `Record missing from recmap`,
  Rust.hs `record missing from recmap`, and Rust.hs RecF `has no concrete
  mapping`).
- [read, 2026-11-15] Dead code: the `IAccess` printers are never reached,
  nothing builds an `IAccess` (Grammars/Translator/Generic.hs:1254-1259,
  CppPrinter.hs:86-88, RustPrinter.hs:99-101). The comment at
  data/lang/py/pymorloc.c:2191-2196 cites a code-generator note that does
  not exist and calls the variant pair form interim.
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
  alignment, so missing union members pass. Fix: mirror the structs and
  kinds, and check members and kinds in tests.
- **Init still has windows a concurrent build can see.** [read
  2026-11-22] Gap beside LIFE-6. `morloc init -f` removes the init-owned
  lib and include files (SystemConfig.hs, the force clean) before the
  language init scripts write them again, so a `morloc make` or a Python
  or R pool started meanwhile finds them missing (a clean failure, not a
  crash). Each header is renamed into place, but a C++ build during init
  can still take some headers old and some new; LIFE-6 checks neither the
  archive nor the headers. `morloc make` checks the header and binding
  markers (installedRuntimeMismatches) without the init lock, so during
  `init -f`, which clears the markers before the shims rebuild, a build
  can find a language unconfigured and compile against bindings being
  replaced.
- **Within one morloc version, a built program runs on a newer runtime
  unchecked.** [reproduced] ART-5 compares version strings, which do not
  change between development commits, and the build id is per run. A
  program built before `morloc init` installs a newer runtime keeps its
  generated pool code against the new libraries: after 7205b50c an older R
  pool fails every call with `could not find function ".mlc_entry"`, which
  the new librmorloc dispatch calls.
- **The compiler cannot tell an uninitialized runtime source edit.** [read
  2026-11-26] The compiler embeds only morloc.h from data/rust. After an
  edit to the runtime source without `morloc init -f` (with MORLOC_RUST_DIR
  at the edited tree), `morloc make` passes and Rust and C++ pools build
  against the previous runtime and its persisted copy. The same holds the
  other way since the compiler carries the Rust binding (SHIM-6): an init
  that reuses a persisted runtime source older than the compiler's tree
  pairs this compiler's rustmorloc with that source's morloc-runtime-types,
  which fails at the pool's compile when they disagree. Releases are not
  affected: the compiler and the runtime source come from one tag, and
  provisioning always re-runs init. A fix pins the compiler to the runtime
  source it was built from (a build id embedded at compile time), which
  would make every runtime edit need `stack install`; needs a ruling.
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
- **A killed init leaves hidden staging files.** [read 2026-11-23] Gap
  beside ART-2. The init scripts stage into `.<name>.tmp.<pid>` beside each
  header, archive and library, and pymorloc into
  `opt/.pymorloc-build.<pid>/`, removing them on exit through a trap; a
  SIGKILL skips the trap, and nothing sweeps them later.
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
- **The shared Rust target dir grows without bound: pool outputs.**
  [measured 2026-11-15, again 2026-11-23] Spec gap: model/runtime/artifacts.md
  says nothing about `STATE/cache/rust-build`, its lifetime or its bound.
  `makeTheMaker` (CodeGenerator/Pools/CAbi/Members/Rust.hs, target dir
  ~3830) builds every pool into that one target dir under a crate named
  `pool_<hash>`, then `cp`s the binary out; nothing ever removes the
  crate's outputs (`release/pool_X`, `deps/pool_X-*`, `build/pool_X-*`,
  `.fingerprint/pool_X-*`). Each new build dir (every golden run, every
  temp dir) adds one crate, median 24 MB, max 352 MB. In the dev env on
  2026-11-23 the dir was 128 GB: 92 GB of pool binaries in `deps/`
  (5247, of which 1685 were stale rebuilds of a name, no longer linked
  from `release/`), 19 GB of `build/pool_*` build-script dirs (4.4 MB
  each). The top-level `release/pool_*` files are hardlinks into `deps/`,
  so removing only them frees nothing. `mim clean` (mim ENV-12) removes
  pool outputs after the fact.
- **The shared Rust target dir grows without bound: dependency copies.**
  [measured 2026-11-23] Same spec gap. Libraries took about 22 GB of
  `deps/` (rlib 16.2, rmeta 3.7, proc-macro .so 2.2), with many builds of
  each crate: zerocopy 0.8.47 built 26 times with identical features,
  cf1_rs 36 x ~380 MB, piscem_rs 45, arrow_array 54, morloc_nexus 19.
  Cargo's per-unit hash covers rustc, profile and the hashes of all
  dependencies, so a difference anywhere below a crate forks it. Observed
  sources of difference:
  - the release profile: `lto` is `thin` by default and `off` in test
    runs (`rust:lto=off`), and every change to the profile `makeCargoDocs`
    writes (Rust.hs ~3761: debug, rustflags) starts a new set; zerocopy
    has 10 rustc/profile combinations dated 09-14 to 11-21;
  - the rustc version (one upgrade, to 1.99.1, around 11-01);
  - per-pool resolution: each pool is its own Cargo project with its own
    Cargo.lock and feature unification, so syn appears with 6 feature
    sets and 6 versions (1.0.109, 2.0.117, 3.0.5/6/9/10); same-profile
    zerocopy builds differ only in their zerocopy-derive, which differ
    only in their syn.
  Superseded builds are never removed, so this part grows too, slower
  than pool outputs.
- **`morloc eval` leaves empty work dirs.** [observed 2026-11-15, cause
  not found] `MORLOC_HOME/tmp` holds 342 empty `morloc-eval-*` dirs dated
  2026-10-01 to now, though Subcommands.hs:557 removes the dir in a
  `bracket`. A SIGKILL (the MCP eval wall-clock kill, memguard) skips a
  bracket; unconfirmed.
- **Inits of two homes sharing one state relink each other's nexus.**
  [read 2026-11-26] Gap beside LIFE-6. The cargo target dir
  (Config.rustBuildDir) belongs to the state, but the nexus build script
  reads MORLOC_HOME (morloc-nexus/build.rs, rerun-if-env-changed), which
  install.sh sets per home. Inits of two homes on one state therefore
  relink target/release/morloc-nexus for each other, and install.sh copies
  it after cargo has released the target lock, while the init lock
  (withInitLock) is per home. The same holds for
  target/release/libmorloc_runtime.a when the two homes build different
  sources: one init's link reads the archive the other just rebuilt. A
  truncated nexus or archive fails closed (the exec or the link fails). The
  unsound case: home A ends with B's libmorloc and nexus, which agree, so
  LIFE-6 passes, beside A's own header, and A's pools compile against A's
  header and load B's library, and nothing catches it. Fix options: a runtime target dir per home (one runtime
  compile per home, no cross-home reuse; needs a ruling), or a lock on the
  shared target held across build and copy (model first).
- **The persisted runtime source keeps its original mtimes.** [read
  2026-11-26] Init copies the source with a tar pipe (SystemConfig.hs
  copyTree), which preserves mtimes, and cargo decides a path dependency's
  freshness by mtime. A persisted morloc-runtime-types whose files are older
  than the artifacts in the shared rust-build cache (for example a source
  unpacked with its archive mtimes, or one switched back to an older tree)
  can leave a Rust pool linking a stale runtime-types build, since the crate
  version does not change within a development cycle.
- **A language init skips is a warning even in strict mode.** [read
  2026-11-26] SystemConfig.hs's language loop warns and continues when a
  language's tools are not in the conda prefix, so an init asked to build
  that language exits 0 without it; a caller that re-runs init until the
  binding exists loops.
- **R's init breaks when R_HOME is set.** [reproduced 2026-11-26] R's
  front-end script prints `WARNING: ignoring environment value of R_HOME`
  to stdout when `$R_HOME` is set to another home, before handling its
  arguments (`R_HOME=/x R RHOME` prints the warning, then the home).
  data/lang/r/init.sh's `R_HOME=$(R RHOME)` then captures two lines and
  links against `-L"<warning>\n<home>/lib"`.
- **Old copies of the Rust binding are never removed.** [read 2026-11-26]
  Init writes `$MORLOC_HOME/lang/rust/rustmorloc-<hash>` for its compiler
  (SHIM-6) and leaves other compilers' copies, since nothing tells whether a
  build of another compiler still reads one. Each rustmorloc edit in
  development leaves another copy (about 300 KB) behind.

