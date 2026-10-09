# Base types

`Unit`, `Bool`, `Str`: value spaces and operations. Prefix: BASE.

### BASE-1 `Unit` has one value
Intent: proposed
Code: unaudited

The value `()` is the only value of `Unit`.

### BASE-2 `Bool` has two values
Intent: proposed
Code: unaudited

`True` and `False` are the values of `Bool`; on the command line they are
the JSON `true` and `false`.

### BASE-3 `&&` and `||` short-circuit
Intent: proposed
Code: unaudited

`a && b` does not evaluate `b` when `a ~> False`, and `a || b` does not
evaluate `b` when `a ~> True`, whichever languages `a` and `b` run in.
`&&` is `infixr 3` and `||` is `infixr 2`.

### BASE-4 A `Str` value survives every language that can hold it
Intent: proposed
Code: unaudited

A `Str` is a sequence of Unicode characters that may include U+0000. Passed
to a language that cannot represent it, it is rejected: a literal when the
program is compiled, a runtime value at the boundary it crosses, as an error
naming the language and the position of the offending character.

### BASE-5 What a `Str` is a sequence of
Intent: open
Code: unaudited

Is a `Str` a sequence of Unicode code points, or of bytes? This decides
whether invalid UTF-8 is a legal `Str`, and whether the length of `"e"` with
an accent is 1 or 2. Candidates: (a) code points, valid Unicode only;
(b) bytes, with literals encoded as UTF-8.
