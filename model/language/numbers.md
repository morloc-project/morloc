# Numbers

`Int`, fixed-width integers and floats: value spaces, literals, arithmetic
across languages. Prefix: NUM.

## Integers

### NUM-1 `Int` is each language's default integer
Intent: ruled 2026-10-04
Code: conforms 2026-10-21

`[[Int]]_L` is L's default integer: 32-bit in C++ and R, 64-bit in Rust,
unbounded in Python. Width and overflow behavior differ by language. `Int`
is for values whose width does not matter; anything that may approach 2^31
uses a fixed-width type.

### NUM-2 Fixed-width integers have one value space in every language
Intent: proposed
Code: conforms 2026-10-21

`I8`..`I64` and `U8`..`U64` hold the same values in every language that
has a native type of that width. Where a language has none, NUM-3 applies.

### NUM-3 R holds 64-bit integers as `integer64`, without -2^63 or 2^63 and up
Intent: ruled 2026-11-01
Code: conforms 2026-11-01

In R, `I64` and `U64` are bit64 `integer64`, a signed 64-bit integer that
reserves -2^63 as its NA. So R's `I64` holds -2^63+1 .. 2^63-1, without the
`I64` minimum, and R's `U64` holds 0 .. 2^63-1. A value outside these ranges
fails as it crosses into R (NUM-4), and one computed in R is NA there and
fails as it leaves. These are R's limits and not deviations: a program that
needs those values runs the work in another language.

### NUM-4 A value that does not fit is an error at the boundary
Intent: proposed
Code: conforms 2026-10-21

When an integer crosses into a language whose type cannot hold it, the call
fails. It is never truncated or wrapped.

### NUM-5 A numeric literal has any type of its class
Intent: ruled 2026-11-08
Code: deviates (compiler/issues/specialization.md)

An integer literal has type `a` with `Integral a`; a real literal has type
`a` with `Numeric a`. Every `Numeric` type is `Integral`, so an integer
literal is accepted at `Real`. The constraint is generalized with the
definition holding the literal, like any other (POLY-8):

```morloc
inc x = x + 1      -- Integral a => a -> a
half x = x / 2     -- Numeric a => a -> a
```

A literal's type variable that nothing determines defaults, which no other
type variable does (CLS-18): to `Int`, or to `Real` if `Numeric` is
required, when every constraint on it is `Integral`, `Numeric` or a class of
`internal`. So `show 5` is `"5"`, `show (1 / 2)` is `"0.5"`, and `5 == 5` is
`True`. A literal that does not fit the type it gets is a compile error.

### NUM-6 Integer `//` and `%` truncate in every language
Intent: ruled 2026-10-03
Code: conforms 2026-10-21

`//` rounds toward zero and `%` takes the sign of the dividend. A zero
divisor never yields a value. In C++ both stay the native operators, at no
cost, so a zero divisor ends the process there.

Why: floor semantics with catchable errors were rejected as too slow.

### NUM-7 Converting between integer types is explicit
Intent: proposed
Code: conforms 2026-10-21

No integer type converts to another implicitly. A widening that always fits
is total (`into`). Every other conversion, including any from `Int` and
from `U32` or wider into `Int`, is checked (`tryInto`) and fails the call
when the value does not fit.

## Floats

### NUM-8 `//` on reals floors
Intent: proposed
Code: conforms 2026-10-21

`Real`, `F32` and `F64` division with `//` rounds toward negative infinity.

### NUM-9 Is `Real` a fixed width?
Intent: open
Code: unaudited

Is `Real` a 64-bit float in every language, or, like `Int`, each
language's default float? Candidates: (a) always IEEE double; (b) native
default.
