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
