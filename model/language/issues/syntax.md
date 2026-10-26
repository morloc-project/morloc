# Syntax

- **A block opened mid-line is closed only by `)`, `]` or `}`.** [reproduced]
  Violates LEX-4 (a block is delimited by indentation). Frontend/Lexer.hs
  `indentCheck` has no counterpart of Haskell's parse-error(t) rule, so a
  `do` block opened in the middle of a line is not closed by a `,`, `;`,
  `:` or `where` later on that line, which belongs to an enclosing
  construct. Rejected: `(do bump x, 1)` at ','; `? x == 0 = do bump 0 : do
  bump x` at ':'; `do { k <- bump x; r <- do bump k; r }` at the second ';';
  `t x = do bump x where ...` at 'where'; and the same with a record literal
  (`(do {on = True, n = x}, 1)`). Writing each block on its own lines works.
  Open: close blocks opened on the current line when one of these
  separators arrives and an enclosing context can take it.
- **A binding without `let` in an explicit `do` block is reported as a
  record.** [reproduced] `do { y = 1; y }` now fails "expected one of: '}',
  ','" at the ';', since after `do` a `{` followed by `name =` opens a
  record literal (LEX-4); the message no longer points at the missing
  `let`.
- **A triple-quoted string with an interpolation is not dedented.**
  [reproduced, 2026-10-21] Violates LIT-5. `a x = """` / `    hi #{x}` /
  `      there` / `    """` gives `"\n    hi yo\n      there\n    "` for
  `a yo` (expected `"hi yo\n  there"`); same for `'''`. Lexer.hs:720
  `processMultilineString` runs only on the no-interpolation path; the
  `#{` branch (:728) emits raw segments. Repro: /work/plans/model-audit-2026-10-21/repro/A/docs/strs2.loc.
- **Lexical gaps.** [read/reproduced, 2026-10-21]
  - LEX-3's keyword list omits `deriving` (Lexer.hs:541 reserves it).
  - LEX-1 says `--` always opens a comment, but an operator run spans it:
    `(+--)` is an operator, `1 +-- note` is not a comment. Candidates: `--`
    opens a comment anywhere; or only at a token start (Haskell).
  - LIT-1 rejects `0xfg` but `12abc` lexes as `12 abc` and `1e` as `1 e`.
  - Lexer.hs:425 lexes `-NaN` as `NaN`; LIT-3 does not list it.
  - `\'` is accepted in `'''` and `"""` strings (Lexer.hs:747) but not in
    `"`; LIT-5 says any escape outside its list is an error.
  - OP-6 candidate (a) names `subtract`, which root lacks.
  - `x <+> y = ...` is a parse error; only `(<+>) x y = ...` defines an
    operator. OP-2 does not say so.
  - BIND-5 says `x@a` binds an expression's value; the code reads it as a
    label ("Undefined label 'x'").
  - `()` is not a pattern (`f () = 1`: "expected a pattern"); PAT-1 is
    silent.

- **A module name with a keyword after a hyphen does not parse.**
  [reproduced, 2026-10-26] `module bad-let (badLet)` fails with
  "unexpected 'let' ... expected identifier", though `stream-r` and other
  hyphenated names parse; the lexer reads the segment after `-` as a
  keyword. The spec (module-names.md) does not say which module names are
  legal, so either the name is legal and the lexer is wrong, or the rule is
  missing.
