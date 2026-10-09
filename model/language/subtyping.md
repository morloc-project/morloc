# Subtyping

The `<:` relation and where it is applied. Effect rows: see effects.md (EFF). Prefix: SUB.

### SUB-1 An expected type is met by subtyping
Intent: proposed
Code: unaudited

Where a term of inferred type `T` meets an expected type `U` (an argument,
an annotation, a declared signature), `T <: U` must hold. Otherwise the
program is rejected.

### SUB-2 Dimensions are compared for equality
Intent: proposed
Code: unaudited

    n == m
    -----------------
    F n a <: F m a

for a Nat position, after both sides are reduced (DIM-2).

### SUB-3 A known dimension is a subtype of an unknown one
Intent: proposed
Code: unaudited

`F 3 a <: F a`, where the second omits the Nat argument (DIM-4).

### SUB-4 The only value coercion is into an optional
Intent: proposed
Code: unaudited

`T <: ?T` (OPT-3). No other implicit conversion exists between distinct
types: `Int` is not a subtype of `Real`, and a suspension `<E> T` is neither
a subtype nor a supertype of `T`.

### SUB-5 Variance through type constructors
Intent: open
Code: unaudited

Does `<:` lift through other types? The cases that need an answer:
`[T] <: [?T]`, `(T, U) <: (?T, U)`, a record field, and function types,
where `(?T -> U) <: (T -> U)` would make arguments contravariant. Candidates:
(a) covariant in data, contravariant in arguments; (b) no lifting, so a
coercion applies only to a whole value. The compiler does (b) today; (a)
would require ALIAS-12's per-parameter variance.
