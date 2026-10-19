# Diagnostics and docstrings

- [read] POLY-6 says the error names the variable; for a variable from a
  library signature the name may be a generated one (e.g. `itemType_6`).
  Only the `@` suffix is stripped (Infer.undeterminedError).
  [reproduced] Two variables written with one name print as that name twice:
  `ingest loadSource 3 p q` (biolib stream, both signatures say `a`) gives
  "nothing in the program fixes 'a', 'a'", which does not say which terms
  they belong to.
- [read] Violates DOC-3. A `@mime` on an optional or effect alias is
  dropped where the alias is expanded: Restructure.writtenAliasDocs carries
  the alias's other directives into the signature but not its media type,
  because the signature-level check (Docstrings.rejectAuthoredMime) would
  read it as written there.
- [reproduced] The manual's docstring-inheritance example
  (docs repo, types-newtype.asc "Docstring inheritance") shows
  `Return: CipherText` and says metavars are not printed in `--help`; the
  compiler prints `type: Str`, `Return: Str`. Repro:
  `/work/plans/issues/164/repro/a2-crypt/`.
- [reproduced] Unused imports warned at library/Morloc/Frontend/Treeify.hs:38
  (Control.Monad.IO.Class).
- [reproduced] A `match` whose branches differ in type (a value beside a
  `do` suspension) is reported as "Branches of guard/if have incompatible
  types" with "then-branch" and "else-branch", constructs the program does
  not contain: `match` reaches Typecheck.hs:1476 through IfS. Repro: in a
  `do` block, `match r | (Err e) = Err e | (Ok xs) = do { f xs; Ok xs }`.
- [reproduced] Violates DIAG-5. A call of a named, non-recursive morloc
  function is beta-reduced into its caller (LambdaEval `reduce`), so it has
  no generated function and no trace line: with `inner x = boom (boom x)`,
  `middle x = inner (boom x)`, `outer x = middle (boom x)` and a C++ `boom`
  that throws past 2, `outer 0` reports only `at outer`. The application is
  named only on some reduction paths, and a name on an index with no
  generated function prints nothing. With the frame stack (STATE-5) an
  inlined named call could push its label around its inlined statements
  and need no function of its own.
