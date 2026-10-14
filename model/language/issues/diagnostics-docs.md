# Diagnostics and docstrings

- [read] POLY-6 says the error names the variable; for a variable from a
  library signature the name may be a generated one (e.g. `itemType_6`).
  Only the `@` suffix is stripped (Infer.undeterminedError).
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
