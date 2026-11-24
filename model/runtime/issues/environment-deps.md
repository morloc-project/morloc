# Declared environment dependencies

- **Unknown package.yaml keys are ignored.** [read] Gap: no spec item for
  package.yaml validation. `FromJSON PackageMeta`
  (library/Morloc/Namespace/State.hs) reads only the keys it knows, so a
  misspelled `tools-deps` or `py-dep`, or `tool-deps` read by a compiler
  older than the key, drops the dependencies without a word and the
  program fails later at run time.

- **A pin or snapshot cannot name a module outside the core org.** [read]
  Gap: no spec item says where a pinned module is fetched from. A pin is a
  bare `name git-hash` (package.yaml `morloc-deps`, `parseMorlocDep` in
  library/Morloc/Namespace/State.hs; snapshot lines, `parseSnapshotLine` in
  library/Morloc/Module.hs), and `autoInstallDep` (Module.hs) turns it into
  `name@hash:<h>`, which installs from the plane's core org (`morloclib`).
  The biolib modules live in the `morloc-bio` org and are imported by bare
  name (`import bio`), so a biolib umbrella's pins or a deposited
  `biolib.txt` snapshot would fetch `morloclib/bio`, which does not exist.
  They work only when every module is already installed locally. Design
  open: an owner per pin (`morloc-deps` entry with `github:`/`org:`, a
  snapshot column), an org per snapshot file, or a per-umbrella default
  org.

- **`morloc init` and `morloc make` read a compiler variable differently.**
  [read] Gap: no spec item says how a strict environment's toolchain is
  checked. The init-time check (`resolveToolName`/`resolveToolLocation` in
  library/Morloc/CodeGenerator/SystemConfig.hs) takes `$CC`/`$CXX` as one
  program name, so `CC='ccache x86_64-conda-linux-gnu-cc'` or
  `CXX='g++ -m64'` resolves to nothing: strict `toolAvailableInEnv` skips
  the C++ shim and `exportArchiveTools` runs `$CC -print-prog-name=ar` on
  the whole string. The make-time check (`toolsOutsidePrefix`) splits the
  value as cc-rs and the shell do. Init should use the same resolver.

- **The make-time strict check reads only environment variables.** [read]
  `toolsOutsidePrefix` does not see cargo configuration
  (`target.<triple>.linker`, `build.rustc`, `[env]` in a `config.toml`),
  `-C linker=` in `RUSTFLAGS`/`CARGO_ENCODED_RUSTFLAGS`, `RUSTC_WRAPPER`,
  or `CARGO_BUILD_TARGET`, so a host linker or compiler chosen there passes.
  Asking cargo (`cargo config get`, or the `cargo build -v` command lines)
  would see what the build really runs.

- **A prefix link to a host compiler passes the strict check.** [read]
  `toolsOutsidePrefix` and `checkCondaCoherence` canonicalize a tool's
  directory, not the tool, because pixi links `$CONDA_PREFIX/bin` entries
  into a shared package store outside the prefix. A link
  `$CONDA_PREFIX/bin/gcc -> /usr/bin/gcc` therefore counts as the
  environment's. Telling the two apart needs the package store's location.

- **The Futhark `ispc` backend may not build.** [speculative]
  `futharkBackends` accepts `ispc`, whose output includes a `.kernels.ispc`
  file, but `buildOne` (library/Morloc/CodeGenerator/Guest/Futhark.hs)
  compiles only the emitted C, so the kernels may never be built or linked.
  Needs a reproduction.

The items below moved from mim's issue list (morloc-manager
model/issues/LIST.md), checked against the code 2026-11-04 [read].

- **`morloc install owner/name` may parse as core module `owner`.**
  `moduleInstallParser` (library/Morloc/Module.hs:1354) has no `eof`;
  with no source prefix `parseModname Nothing` (:1402) parses a core
  module and drops `/name`, on the classic path (:822) and in
  `extractModuleName` (:765). Registry installs try
  `tryParseRegistryModule` first (:816). Unreproduced.
- **The default config file is never read.** `--config` defaults to ""
  (executable/UI.hs:551), which Subcommands.hs:131 turns into
  `loadMorlocConfig Nothing`, loading no file (Config.hs:118-121). So
  `registry`, `lang_overrides` and `plane` are unreachable without
  `--config`. The default is named `$MORLOC_HOME/config` (Config.hs:49).
  See the manual item in spec-and-docs.md.
- **`installed_hash` records the requested pin.** Module.hs:1111-1113 takes
  the snapshot, else the pin, else Nothing; the fetched HEAD is never read.
- **Registry installs and `install --build` disagree on the path.** Install
  writes `plane/<owner>/<name>` (Module.hs:818); `--build` looks in
  `plane/<name>` (Subcommands.hs:232).
- **envspec `modules` is first-wins, the resolver closer-wins.**
  `dedupFst` in EnvSpec.hs:200 (defined :265) versus `addPin`
  (Module.hs:128).
- **An empty `MORLOC_HOME`/`MORLOC_STATE` is unset to Haskell, set to the
  nexus.** Config.hs:86, :147, :256 treat empty as unset; morloc-nexus
  main.rs:39 and :51 do not (main.rs:413 does for `MORLOC_STATE`).
- **`install` writes wrappers and completions under `$MORLOC_HOME`.**
  Install.hs:70 (`bin`) and :108 (`completions`), and the uninstall side
  (Subcommands.hs:1290, :1319), use `configHome`, not the state root.
- **A plugin language's lang.yaml that fails to parse is dropped
  silently, and its record kinds are never read.** [read, 2026-11-15]
  Gap in GEN-2 (a malformed descriptor is refused naming the file and
  key). Frontend/API.hs buildLangMap' `scanLangDir` returns Nothing on
  any parse error, so every `source Plug ...` then fails "unknown
  language"; plugin languages get no registry entry, so their
  `generated_types` are ignored (Frontend/API.hs psTypeKinds and
  Restructure.hs withDefaultKinds read the built-in registry only).
- **A build outside a managed environment checks only C++ headers and tool
  executables.** [reproduced, 2026-11-19] Gap: outside a managed environment
  nothing provisions declared dependencies (by design), and PKG-14/PKG-16
  check cpp-deps `import` headers and tool-deps `bin` names, but `py-deps`
  and `r-deps` are not checked, so a missing one fails at pool load (`No
  module named 'pydantic'` on the CI runners). Design open: check imports at
  build, or at pool start, naming the dependency.
- **A system g++ in a managed environment cannot link against the prefix.**
  [reproduced, 2026-11-19] With strict mode off and `CXX=/usr/bin/g++`, a
  pool with `cpp-deps: {htslib: {libs: [hts]}}` compiles against P/include
  but fails to link: the system driver runs the environment's `ld` from
  PATH, which links conda's sysroot libpthread/libdl (`undefined reference to
  ... @GLIBC_PRIVATE`). Mixing a host compiler with the environment's
  libraries is outside PKG-13; mim's strict mode refuses such a compiler.
  Design open: refuse a host compiler in any managed build, or document it.

- **The manual still documents the removed `dependencies` field.** [read,
  2026-11-19] lang-cpp.asc and ecosystem.asc describe `dependencies` (two
  different ways); PKG-12 removed it in favour of cpp-deps `libs`, and the
  manual has no `libs`, tool-deps `bin` or PKG-13/17 behaviour yet. Also
  arch-eco-modules.asc, lang-parameters.asc, deploy-dependencies.asc:70,
  interface-install.asc:214.

- **The Python binding build discards the environment's CFLAGS.**
  [read, 2026-11-19] data/lang/py/init.sh exports
  `CFLAGS="$SANITIZE_FLAGS"`, empty unless sanitizing, so the activation's
  CFLAGS (`-isystem $CONDA_PREFIX/include`, ...) never reach setuptools.
  Impact unknown: setup.py names its include dirs itself.

- **The python version window is never checked against the binding.**
  [reproduced, 2026-11-19] Gap: no spec item says how the runtime window in
  data/lang/py/requirements.yaml is verified. LangSupportTests asserts only
  the ceiling string; nothing builds pymorloc.c at the window's ends. The
  window was wrong when written: `<3.14` admitted 3.13, which also failed
  (`_PyLong_AsByteArray` gained an argument in 3.13; compile 627b58b74's
  pymorloc.c against conda python-3.13.15 headers), and it stayed stale
  after 4662b83b7 fixed that. The ceiling is now `<3.15`; 3.14.7 compiles
  and links with every Py symbol resolved, but no program has run on 3.14
  (no cp314 numpy offline). pymorloc.c uses private API that can change
  again: `_PyLong_AsByteArray`, `_PyLong_FromByteArray`, `_PyLong_NumBits`,
  `_PyObject_GetDictPtr`; public replacements exist since 3.13
  (`PyLong_AsNativeBytes`, `PyLong_FromNativeBytes`).
