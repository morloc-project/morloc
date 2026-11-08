# Base types

`Unit`, `Bool`, `Str`: value spaces and operations. Prefix: BASE.

### BASE-1 `Unit` has one value
Intent: proposed
Code: conforms 2026-10-21

The value `()` is the only value of `Unit`.

### BASE-2 `Bool` has two values
Intent: proposed
Code: conforms 2026-10-21

`True` and `False` are the values of `Bool`; on the command line they are
the JSON `true` and `false`.

### BASE-3 `&&` and `||` short-circuit
Intent: ruled 2026-10-16
Tests: spec-base-3-1, spec-base-3-2, spec-base-3-3
Code: conforms 2026-10-16

`a && b` does not evaluate `b` when `a ~> False`, and `a || b` does not
evaluate `b` when `a ~> True`, in every language, wherever `a` and `b` run.
They are special forms, not functions: every other application evaluates
its arguments first (EVAL-1), and no module may define or declare either.
Used as a value, each is the function of two `Bool`s it denotes. `&&` is
`infixr 3` and `||` is `infixr 2`.

### BASE-4 A `Str` value survives every language that can hold it
Intent: proposed
Code: deviates 2026-11-08 (runtime/issues/strings.md)

A `Str` is a sequence of Unicode characters that may include U+0000. Passed
to a language that cannot represent it, it is rejected: a literal when the
program is compiled, a runtime value at the boundary it crosses, as an error
naming the language and the position of the offending character.

### BASE-5 A `Str` is a sequence of Unicode characters
Intent: ruled 2026-11-08
Code: deviates 2026-11-08 (runtime/issues/strings.md, language/issues/values.md)

A `Str` is text: a sequence of Unicode code points, held as valid UTF-8.
Bytes that are not valid UTF-8 are not a `Str`; a value that carries them
fails where it crosses a boundary (BASE-4). Every operation on a `Str`
counts in characters, so the length of `"\u00e9"` is 1 in every language.
Binary data is `Vector n U8`.

