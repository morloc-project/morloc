# Installed language bindings (SHIM)

How `morloc init` records the language bindings it builds (the C++, Python
and R shims), how a binding is judged current, and how another tool asks.
The compiler that builds a binding is the one authority on whether it is
current; mim asks it (`morloc init --check`) rather than reading markers.

### SHIM-1 A binding is current when its marker records this compiler's inputs
Intent: ruled 2026-11-26
Tests: spec-shim-1-1, spec-shim-1-2, spec-shim-1-3, spec-shim-1-4
Code: conforms 2026-11-26

`$MORLOC_HOME/opt/lang-configured/<language>` records, one field per line,
what the binding was built from and against: the runtime build id in
`lib/morloc.build-id`, the interpreter's identity, the sanitize setting, and
a hash of the binding's sources (the header, its init script and files).
The identity is Python's `EXT_SUFFIX` (version and ABI flags) with numpy's
major version (the binding is built against numpy's C API), or R's major and
minor version; C++ has none. A binding is current when its record matches
and its artifact exists (`opt/pymorloc<EXT_SUFFIX>`, `lib/librmorloc.so`,
`lib/libcppmorloc.a`). Otherwise it is absent (no marker), stale (another
runtime build, other sources, another sanitize setting or interpreter, or
the artifact is gone), unavailable (its interpreter cannot be found or run),
or outside-prefix (in a managed environment, the interpreter on PATH is not
the environment's). Init and `--check` find interpreters by one rule: on
PATH, and in a managed environment only inside its prefix; a managed
environment that cannot be resolved makes every binding with an interpreter
unavailable. Init skips (in strict mode, fails on) a language whose
interpreter cannot be used. An incremental init rebuilds what is not current
and skips the rest.
Why: the user ruled the compiler the one authority on bindings (2026-11-26).

### SHIM-2 `morloc init --check` reports the runtime and the bindings
Intent: proposed
Tests: spec-shim-2-1
Code: conforms 2026-11-26

`morloc init --check [--lang L]...` prints one JSON object and exits 0:
`{"bindings_check": 1, "runtime": {"ok": B[, "reason": S]},
"languages": [{"lang": L, "state": S, "reason": S}, ...]}`, with each state
one of `ok`, `absent`, `stale`, `unavailable`, `outside-prefix` (SHIM-1), for
the given languages (`py`, `r`, `cpp`; any other name is refused) or all of
them. It takes none of init's build flags, builds and writes nothing, takes no
lock, and reads the sanitize setting from the home's build config. The
runtime is ok when the installed header is this compiler's, the build id is
recorded, and the installed nexus accepts the installed libmorloc, reports
the recorded build id, and speaks this compiler's ABI. `morloc lang-support`
carries `"bindings_check": 1` so a caller can tell this command exists.

### SHIM-3 `morloc make` refuses a binding whose record is not current
Intent: ruled 2026-11-26
Tests: spec-shim-3-1
Code: conforms 2026-11-26

Before compiling pools, `morloc make` checks the header, the recorded build
id, and the record of each language the program's pools use (SHIM-1),
reading files only: it runs no interpreter, since the interpreter that
loads a binding is chosen when the program runs (SHIM-5 checks it there). It
refuses with the command that repairs the home, or, in a read-only
environment, with the command that rebuilds it.

### SHIM-4 An incremental init keeps the home's SLURM and sanitize settings
Intent: proposed
Tests: spec-shim-4-1
Code: conforms 2026-11-26

An incremental init (`MORLOC_INIT_INCREMENTAL`, mim's on-demand language
add) builds with the SLURM and sanitize settings the home's build config
records; its flags can turn a setting on, never off. A full init takes its
flags as given.

### SHIM-5 A binding refuses to load in an interpreter it was not built for
Intent: ruled 2026-11-26
Tests: spec-shim-5-1, spec-shim-5-2
Code: conforms 2026-11-26

A Python pool that cannot import pymorloc (its module is named for the
interpreter it was built for) fails naming the running Python and the
command that repairs it. An R pool whose binding was compiled for another R
major and minor version stops before any call, naming both versions and the
command that repairs it.

### SHIM-6 The Rust binding is the compiler's, installed by its contents
Intent: ruled 2026-11-26
Tests: spec-shim-6-1, spec-shim-6-2, spec-shim-6-3
Code: conforms 2026-11-26

Generated Rust code calls rustmorloc, so the crate is the compiler's: its
source is `data/lang/rust/rustmorloc`, embedded file by file (all ASCII).
Every init writes it, if absent, to
`$MORLOC_HOME/lang/rust/rustmorloc-<hash of its files>` (staged, then renamed
into place with a `.complete` stamp), leaving other compilers' copies;
from there its relative dependency on the runtime types reaches the home's
persisted runtime source. Rust pools depend on that directory and start
from its `Cargo.lock`; init builds its dependencies into the shared cache
with the default pool profile. `morloc make` refuses a Rust program when it
is missing, and `morloc init --check` reports it as language `rust`. Its C
declarations are checked against `morloc.h` in the source tree, and the
installed header must be this compiler's (SHIM-3), so the binding and the
installed libmorloc share one C ABI; a mismatch in the Rust interface of
`morloc-runtime-types` fails the pool's compile.

### SHIM-7 Init builds and keeps only a runtime of this compiler's version
Intent: proposed
Tests: spec-shim-7-1, spec-shim-7-2, spec-shim-7-3, spec-shim-7-4
Code: conforms 2026-11-27

Init builds the runtime from `MORLOC_RUST_DIR` or the home's persisted copy,
not from the compiler. It refuses a source whose `include/morloc.h` is not
the header the compiler was built with, or whose runtime crates are not at
the compiler's version. Every init refuses a `$MORLOC_HOME/rust` that is a
link to such a source (init keeps a link, and Rust pools build against it),
and rebuilds an installed runtime whose nexus reports another version: the nexus refuses every manifest from another
morloc version (ART-5), so such a runtime would run no program. Every
runtime crate and the Rust binding carry the compiler's version, in their
Cargo.toml and in their own entries in the two Cargo.lock files;
`scripts/bump-version.sh` sets them all.
