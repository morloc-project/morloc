# Modules

- **An external dotted import resolves its local imports from the wrong
  root.** [read, 2026-11-13] MOD-6. `import foo.bar` from outside foo
  takes `takeDirectory importPath` as bar's package root
  (library/Morloc/Frontend/API.hs:225), while foo reaching `.bar` uses
  foo's root. bar's own `import .x` then names a different file depending
  on which import loaded bar first. The MOD-6 test covers only the
  in-package case.
  Under MOD-5 a program importing both `l/x` and `l/x.a`, where `a.loc`
  imports `.b`, reaches `b.loc` under two keys and is refused.

- **A program's own source of a term an imported module also sources is
  ambiguous.** [reproduced, 2026-11-20] `source Rust from "m.rs"
  ("my_require" as require)` beside root-rust's `require` gives "Ambiguous
  source declarations"; every function added to a root module breaks
  programs that implemented it. Ruled 2026-11-20: the program's own source
  shadows an imported one; two imported sources stay ambiguous.
  Repro: /work/plans/migration-bugs-2026-11-20/repro/root-source-collision.
