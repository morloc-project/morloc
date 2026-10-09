# Numbers

`Int`, fixed-width integers and floats: value spaces, literals, arithmetic
across languages. Prefix: NUM.

## Integers

### NUM-1 `Int` is each language's default integer
Intent: ruled 2026-10-04
Code: unaudited

`[[Int]]_L` is L's default integer: 32-bit in C++ and R, 64-bit in Rust,
unbounded in Python. Width and overflow behavior differ by language. `Int`
is for values whose width does not matter; anything that may approach 2^31
uses a fixed-width type.

### NUM-2 Fixed-width integers have one value space in every language
Intent: proposed
Code: unaudited

`I8`..`I64` and `U8`..`U64` hold the same values in every language that
has a native type of that width. Where a language has none, NUM-3 applies.

### NUM-3 R holds 64-bit integers as doubles
Intent: ruled 2026-10-04
Code: unaudited

In R, `I64` and `U64` are doubles, exact below 2^53. This is R's limit and
not a deviation.

### NUM-4 A value that does not fit is an error at the boundary
Intent: proposed
Code: unaudited

When an integer crosses into a language whose type cannot hold it, the call
fails. It is never truncated or wrapped.

### NUM-5 Integer literals are `Int` and are bounds-checked at their type
Intent: proposed
Code: unaudited

An integer literal is `Int` unless its context gives another integer type.
A literal that does not fit the type it is given is a compile error.

### NUM-6 Integer `//` and `%` truncate in every language
Intent: ruled 2026-10-03
Code: unaudited

`//` rounds toward zero and `%` takes the sign of the dividend. A zero
divisor never yields a value. In C++ both stay the native operators, at no
cost, so a zero divisor ends the process there.

Why: floor semantics with catchable errors were rejected as too slow.

### NUM-7 Converting between integer types is explicit
Intent: proposed
Code: unaudited

No integer type converts to another implicitly. A widening that always fits
is total (`into`). Every other conversion, including any from `Int` and
from `U32` or wider into `Int`, is checked (`tryInto`) and fails the call
when the value does not fit.

## Floats

### NUM-8 `//` on reals floors
Intent: proposed
Code: unaudited

`Real`, `F32` and `F64` division with `//` rounds toward negative infinity.

### NUM-9 Is `Real` a fixed width?
Intent: open
Code: unaudited

Is `Real` a 64-bit float in every language, or, like `Int`, each
language's default float? Candidates: (a) always IEEE double; (b) native
default.
