# Syntax

- **A `do` block whose first statement is a record literal does not
  parse.** [reproduced, 0.109.0] Spec gap in LEX-4: `{` directly after
  `do` always opens an explicit-brace block, so `f s = do` followed on the
  next line by `{on = False, n = 0}` (or `do {on = False}` on one line)
  fails "unexpected '='", while the grammar's `do_stmt` admits any
  expression. Cause: Frontend/Lexer.hs `startLayoutCtx` takes any
  `TokLBrace` after a layout keyword as explicit. Proposed rule, needs a
  ruling: after `do` (not `where` or `let`), `{` followed by a lowercase
  name and `=` (not `==`) begins a record literal in a layout block; no
  statement can begin with `name =`, so no legal program changes meaning.
  Workaround: move the literal into a function, or put a statement first.
  Repro: /work/plans/rnaseq-bugs/rparse/b.loc.
