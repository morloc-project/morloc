# Limits of generated code

Generated code that a language's own tools refuse.

- [read] R's parser bounds nesting at 50 contexts (`contextstack overflow`),
  each `(`, `[`, `{` and `if` one. A chain of more than about 50 `&&` or
  `||` operands is written as nested parenthesized operators, and a guard
  chain (`? c1 = .. : ? c2 = .. : ..`) of about 25 cases as nested
  `if`/`else` blocks, so either fails to load in an R pool. Spec gap: no
  item bounds the nesting of generated code. Flat forms exist for both
  (an `else if` chain; a `repeat` block that breaks at the first operand
  that decides the result).
