# Runtime binder and pool names share scope with user sources

Each pool puts the runtime binder (rustmorloc, cppmorloc and the libmorloc
C API, rmorloc, pymorloc) and its own generated names in the scope where
user sources live, so a user file can collide with them. Spec gap:
model/language/sources.md says nothing about which names a sourced file
may use.

- [read] Rust: sourced files are `include!`d at the crate root, where
  generated code still names the standard prelude (`Vec`, `Option`,
  `Result`, `Box`, `String`) and its macros (`vec!`, `format!`) unqualified,
  and native type forms (`Vec<$1>`) are user-written text. A user file that
  defines `struct Vec` or `macro_rules! vec` changes what generated code
  means. Runtime items and generated items are already reached by path or
  a reserved `__morloc` name.
- [reproduced, d352b5d4] C++: user headers are included after the precompiled header,
  which brings the libmorloc C API into the global namespace (`typedef
  struct Schema`, about 180 functions), and before cppmorloc.hpp, whose
  helpers are partly global (`to_vector`, `wrap_field`, `mpk_pack`, ...).
  pool.cpp adds globals (`g_tmpdir`, `foreign_call`, `MorlocException`,
  `m<N>`). A user `struct Schema` is a redefinition, a user `std::string
  g_tmpdir` a conflicting declaration, and a user `foreign_call` an
  ambiguous overload.
- [read] C++: binder templates call their own helpers unqualified with
  dependent arguments (`to_voidstar`, `from_voidstar`, `to_vector`,
  `wrap_field`), so argument-dependent lookup also finds a user's global
  function of that name; a forwarding template `template<class... A> auto
  to_vector(A&&...)` is a better match and is called silently.
- [read] C++, R, Python: libmorloc exports C symbols with unprefixed names
  (`pack`, `hex`, `quoted`, `shinit`, `remote_call`, `pool_main`,
  `daemon_run`, ...). C linkage ignores namespaces, so a user `extern "C"`
  declaration of one with another signature is an error, and a user library
  exporting one is picked by the pool's calls through symbol interposition.
  Needs a ruling: rename the exports to a reserved prefix (an ABI change) or
  list them as reserved names.
- [read] Manifold locals and sourced names share a frame: a manifold's
  formals and temporaries are `n<N>`, `s<N>`, `helper<N>` (Grammars/Common.hs
  argNamer and friends), and R, Python and Rust call sourced functions by
  their bare or module-qualified name in that frame. An R or Rust sourced
  function named `n3` is shadowed by a manifold parameter `n3`, silently
  when it is passed as a value. Needs reserved local names per language, from the same namer
  machinery as the manifold names.
- [read] Generated R code calls base functions (`c`, `list`, `lapply`,
  `paste0`, `tryCatch`, ...) by name from the global environment, where a
  user source may have redefined them, as R's own code would see them.
  Needs a ruling: the user's responsibility, a load-time check that none
  is redefined, or reserved aliases.
- [read] Python: a builtin source (no `from` path) is emitted as a bare
  name in pool.py's module scope, which holds the pool's own names
  (`dispatch`, `global_state`, `mlc_*`), so `source Py ("dispatch" as d)`
  binds d to the pool's dispatch table rather than failing.
- [reproduced, 2026-10-21] R's JIT compiles a small function only where
  its environment is the global one, so the R runtime's functions in
  `.__morloc` run interpreted (about 0.5 us more per call than compiled)
  except the twelve pool.R compiles at load (about 20 ms per pool start).
  Shipping the runtime as an installed R package would have R byte-compile
  its namespace once, at install.
- Python files are isolated: each sourced file is its own module and calls
  are qualified (`__morloc_src_<name>.f`).
- [read, 2026-10-23] C++ declares no reserved prefixes or names in its
  lang.yaml, so SRC-6's build-time check covers nothing for C++: manifolds
  are `m<N>`/`m<N>_remote` (not the `__mm_` prefix), and the pool's macros
  (`PROPAGATE_ERROR`, `PROPAGATE_INFRA_ERROR`, `PROPAGATE_FAIL_PACKET`,
  `MLC_INTERNAL_ABORT`, `MLC_CATCH_FORCED_UNWIND`) expand a sourced function
  of the same name. Clean part: rename C++ manifolds to `__mm_<N>`, list the
  pool's prefixes and names, and extend ManifoldNameTests to C++ with a
  check that every global the pool templates define is covered. The R list
  misses `.morloc.source`, `.morloc.pooldir`, `.morloc.libdir` (`.morloc_`
  does not match `.morloc.`).
