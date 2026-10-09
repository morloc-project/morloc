# Literals

Syntax of numeric, string, boolean, list, tuple and record literals. Prefix: LIT.

## Numbers

### LIT-1 Integer literals are decimal, hexadecimal, octal or binary
Intent: proposed
Code: unaudited

`42`, `0xff`, `0o755`, `0b101`; the base letter and hex digits are case
insensitive. A prefixed literal containing a digit invalid for its base is a
lexical error, never a literal followed by a name.

### LIT-2 A real literal has a decimal point or an exponent
Intent: ruled 2026-10-09
Code: deviates (issues/typecheck.md)

`1.0`, `1e0` and `6.022E23` are real literals; `1` is an integer literal.
An integer literal takes a real type from a context that expects one; a
real literal never takes an integer type. `Inf` and `NaN` are real literals.

### LIT-3 A `-` against a digit is part of the literal
Intent: proposed
Code: unaudited

`-1`, `-1.5` and `-Inf` are atomic literals, legal wherever a literal is.
Any other prefix `-e` is `negate e`.

### LIT-4 An out-of-range numeric literal
Intent: open
Code: unaudited

A literal that does not fit the type it is written into is rejected
(NUM-5), but only when code is generated, so `morloc typecheck` accepts it. Candidates:
(a) it is a type error whenever the type is known at typechecking;
(b) it is a realization error, raised only for the precision the literal is
realized at.

## Text and structure

### LIT-5 String literals
Intent: proposed
Code: unaudited

A string is double-quoted. The escapes are `\n \t \r \0 \\ \"` and any other
is a lexical error. In a `"""` or `'''` string, leading spaces through the
first newline and trailing spaces through the last are dropped, then the
smallest common indentation is removed from every line. `#{e}` splices `e`
into the string, and requires `G |- e : Str`.

### LIT-6 Container literals
Intent: proposed
Code: unaudited

`True`, `False` and `Null` are literals. `[e1, ..., en]` is a list,
`(e1, ..., en)` with n >= 2 a tuple of arity n, `()` the unit value. A record
literal `{k1 = e1, ...}` names every field of its record exactly once, in any
order, and its record type comes from context.
