# Syntax

- **A binding without `let` in an explicit `do` block is reported as a
  record.** [reproduced] `do { y = 1; y }` now fails "expected one of: '}',
  ','" at the ';', since after `do` a `{` followed by `name =` opens a
  record literal (LEX-4); the message no longer points at the missing
  `let`.
- **Lexical gaps.** [read/reproduced, 2026-10-21]
  - LIT-5 lists the escapes `\n \t \r \0 \\ \"`, but a `"""` or `'''`
    string also accepts `\'` (Lexer.hs `lexStringLit`). Candidates: add
    `\'` to LIT-5 for every string, or for triple-quoted ones only.
  - LEX-3's keyword list omits `deriving` (Lexer.hs:498 reserves it).
  - LEX-1 says `--` always opens a comment, but an operator run spans it:
    `(+--)` is an operator, `1 +-- note` is not a comment. Candidates: `--`
    opens a comment anywhere; or only at a token start (Haskell).
  - LIT-1 rejects `0xfg` but `12abc` lexes as `12 abc` and `1e` as `1 e`.
  - Lexer.hs:403 lexes `-NaN` as `NaN`; LIT-3 does not list it.
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
- **A constructor cannot take the unit type.** [reproduced, 2026-11-01]
  `data Box = Box ()` fails "unexpected ')'" at the `)`; the data
  constructor argument grammar (Parser.y) has no `()` alternative, though
  `()` is a type elsewhere (`f :: () -> Int`). Spec gap: sum-types.md does
  not say whether a constructor argument may be `()`.
