# Runtime binder and pool names share scope with user sources

Each pool puts the runtime binder (rustmorloc, cppmorloc and the libmorloc
C API, rmorloc, pymorloc) and its own generated names in the scope where
user sources live, so a user file can collide with them. Spec gap:
model/language/sources.md says nothing about which names a sourced file
may use.

- [reproduced, 2026-10-20, d352b5d4] Rust: a sourced file with `use
  rustmorloc::arrow_schema::{DataType, Field, Schema};` fails with E0252
  (`Schema` defined twice), because the pool template imports
  `rustmorloc::Schema` at the crate root (data/lang/rust/pool.rs `use`
  lines). Sourced files are `include!`d at the root, so any of those
  imports (`CString`, `OnceLock`, `ToVoidstar`, ...) and any generated root
  item (`schema`, `init_schemas`, `local_dispatch`, `PoolConfig`, `m<N>`,
  ...) collides the same way (reproduced: helpers named `schema`,
  `init_schemas`, `m1` fail with E0428). golden/arrow-conformance works
  around it with `Schema as ArrowSchema`.
- [reproduced, d352b5d4] C++: user headers are included after the precompiled header,
  which brings the libmorloc C API into the global namespace (`typedef
  struct Schema`, about 180 functions), and before cppmorloc.hpp, whose
  helpers are partly global (`to_vector`, `wrap_field`, `mpk_pack`, ...).
  pool.cpp adds globals (`g_tmpdir`, `foreign_call`, `MorlocException`,
  `m<N>`). A user `struct Schema` is a redefinition, a user `std::string
  g_tmpdir` a conflicting declaration, and a user `foreign_call` an
  ambiguous overload.
- [reproduced, d352b5d4] R, a silent wrong value: user files are `source()`d into `globalenv()`, which also holds
  every pool name: the rmorloc `.Call` wrappers (`morloc_*`), pool
  internals (`mlc_*`, `.mlc_*`), manifolds (`m<N>`) and generic names
  (`main`, `args`, `status`, `tmpdir`, `socket_path`, `global_state`,
  `worker_init`). Sources load after the pool defines these, so a user
  definition replaces one silently and later lookups by name see it. The
  dispatch table captures the top-level manifolds before sources load, but
  manifold bodies look up `morloc_get_value`, `mlc_schema_table` and other
  manifolds by name at call time: with `ap <- function(f, x) f(x)` and
  `viaR2 n = ap (\x -> inc (inc x)) n`, a user `m1657 <- function(...)
  -1000L` (the lambda's manifold) makes `viaR2 3` print -1000, not 5.
- [read] Manifold locals and sourced names share a frame: a manifold's
  formals and temporaries are `n<N>`, `s<N>`, `helper<N>` (Grammars/Common.hs
  argNamer and friends), and R and Python call sourced functions by their
  bare or module-qualified name in that frame. An R sourced function named
  `n3` is shadowed by a manifold parameter `n3`, silently when it is passed
  as a value. Needs reserved local names per language, from the same namer
  machinery as the manifold names.
- [read] Python identifies a manifold function by its `__name__`
  (`_mlc_manifold_id` in data/lang/py/pool.py): a host function a user
  source returns that happens to be named `m5` is reified as manifold 5,
  and the far side calls the wrong function.
- [read] Generated R code calls base functions (`c`, `list`, `lapply`,
  `paste0`, `tryCatch`, ...) by name from the global environment, where a
  user source may have redefined them, as R's own code would see them.
  Needs a ruling: the user's responsibility, a load-time check that none
  is redefined, or reserved aliases.
- [read] Python: a builtin source (no `from` path) is emitted as a bare
  name in pool.py's module scope, which holds the pool's own names
  (`dispatch`, `global_state`, `mlc_*`), so `source Py ("dispatch" as d)`
  binds d to the pool's dispatch table rather than failing.
- Python files are isolated: each sourced file is its own module and calls
  are qualified (`__morloc_src_<name>.f`).
