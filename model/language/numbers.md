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
Tests: spec-num-5-1, spec-num-5-2, spec-num-5-3, spec-num-5-4, spec-num-5-5
Code: deviates (compiler/issues/specialization.md)

An integer literal has type `a` with `Integral a`; a real literal has type
`a` with `Numeric a`. Every `Numeric` type is `Integral`, so an integer
literal is accepted at `Real`. The constraint is generalized with the
definition holding the literal, like any other (POLY-8):

```morloc
inc x = x + 1      -- Integral a => a -> a
half x = x / 2     -- Numeric a => a -> a
```

A literal's type variable that nothing determines defaults (NUM-10), so
`show 5` is `"5"`. A literal that does not fit the type it gets is a
compile error.

### NUM-10 An undetermined numeric type variable defaults to `Int` or `Real`
Intent: ruled 2026-11-08
Tests: spec-num-10-1, spec-num-10-2, spec-num-10-3, spec-num-10-4, spec-num-10-5
Code: deviates (compiler/issues/specialization.md)

A type variable that nothing at a use determines (CLS-18) defaults when
one of its constraints is `Integral` or `Numeric` and each of them is
`Integral`, `Numeric` or a one-argument class declared in `internal`
(`Eq`, `Ord`, `Show`, `Hashable`). It becomes `Real` if `Numeric` is among
them, and `Int` otherwise. Where the constraint came from does not matter:
a literal (NUM-5), a generalized definition (POLY-8), or a method.

```morloc
x = 5                     -- Integral a => a
half y = y / 2            -- Numeric a => a -> a

s1 = show 5               -- "5"
s2 = show x               -- "5": a defaults to Int
s3 = show (half 3)        -- "1.5": Numeric, so Real
s4 = show zero            -- "0": zero is a method of Integral
s5 = 5 == 5               -- True
```

These stay errors:

```morloc
s6 = show mempty          -- Monoid is not a numeric class
s7 = show (x <> x)        -- Semigroup is declared in root, not internal
```

No numeric type is optional, so a value whose numeric type is not yet
known, used where `?T` is expected, has type `T` and is converted to `?T`
(OPT-7): with `f :: ?Int -> Int -> Bool`, `f e e where e = 3 + 4` is
accepted, `e` an `Int`.

An unsigned export is a use, so its own type defaults too:
`foo x y = x + y + 42.0` is a command on `Real`. A variable of a signature
is never defaulted: a signed generic export stays generic (SIG-5).

Why: this is Haskell's rule (Report 4.3.4). Once `x = 5` is generalized,
`show x` must mean what `show 5` means; tying defaults to where a variable
came from would make a definition's meaning change when it is named.

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
