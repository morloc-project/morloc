# Modules

- **An external dotted import resolves its local imports from the wrong
  root.** [read, 2026-11-13] MOD-6. `import foo.bar` from outside foo
  takes `takeDirectory importPath` as bar's package root
  (library/Morloc/Frontend/API.hs:245), while foo reaching `.bar` uses
  foo's root. bar's own `import .x` then names a different file depending
  on which import loaded bar first. The MOD-6 test covers only the
  in-package case.
  Under MOD-5 a program importing both `l/x` and `l/x.a`, where `a.loc`
  imports `.b`, reaches `b.loc` under two keys and is refused.
- **A file declaring two unrelated modules builds.** [reproduced,
  2026-11-25] Gap in module-names.md: no item says what a file holding
  `module main (foo)` and `module myinstances (bar)`, neither importing the
  other, means. `morloc make foo.loc` builds it and ignores the second
  module. test-suite/integration/suites/errors/multi-rooted-dag expects a
  refusal (it used to reach an internal compiler-bug path), so it fails
  until this is ruled.
