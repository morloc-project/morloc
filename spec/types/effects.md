# Effects

Morloc tracks side effects through the type system to make composition properties statically visible. The signature of a composed function reports the union of all effects introduced by its components, so the type of a top-level export answers questions like "does this pipeline touch the network?" or "is this computation non-deterministic?" by inspection.

The effect system is intentionally narrow: it propagates labels through composition. It does not provide effect handlers, semantic interpretation of labels, or polymorphism over containers of effects.

## Effect Annotations

An effect annotation wraps a type and names the side effects produced when the value is forced:

```morloc
randint :: Int -> Int -> <Rand> Int
readFile :: Str -> <IO> Str
parse :: Str -> <Error> Result
```

Multiple labels are separated by commas; order is not significant:

```morloc
fetch :: Url -> <IO, Net> Bytes
```

Labels are user-chosen identifiers in upper case. The compiler does not interpret them; they are tags that propagate through composition. A library establishes its effect vocabulary by attaching labels to source-imported primitives.

## Operational Meaning

A value of type `<E> T` is a *suspended computation* that, when forced, produces a `T` and is permitted to exhibit the effects in `E`. Forcing happens explicitly, never as a side effect of binding or projection. A function returning `<E> T` is partially applicable: each force may produce a different result.

This is what distinguishes `<Rand> Int` from `Int`. A binding `r = randint 0 9` has type `<Rand> Int` and represents a callable sampler; each force draws a new value. The same binding with type `Int` would be a single sampled constant.

## Effect Sets

An effect set is one of:

- a finite set of labels, written `<L1, L2, ...>` (the empty set is `<>`)
- a variable, used internally during inference
- a union of two effect sets

The order of labels is irrelevant. Duplicates collapse. The empty effect set marks a value as pure but suspended (rare in practice; pure values normally have no annotation at all).

## Subtyping

The effect-subtyping rule is:

```
  E1 subset of E2       T1 <: T2
  -----------------------------------
       <E1> T1  <:  <E2> T2
```

Read covariantly: a value that produces *fewer* effects can be used where *more* effects are allowed. This direction is the only one permitted.

The converse, narrowing, is rejected:

```morloc
rint :: <IO, Error> Int
a :: <IO> Int
a = rint                  -- type error: Error is not in <IO>
```

Pure types are subtypes of any effect type via coercion (see below), but the reverse coercion from `<E> T` to `T` is not available. The only way to extract a pure `T` from a suspended `<E> T` is to force it inside a do-block.

See [[subtyping.md]] for the full subtyping relation.

## Pure-to-Effect Coercion

A pure value lifts implicitly to any effect type:

```
  T  coerces to  <E> T   for any E
```

This is what makes the trailing expression of a do-block work without an explicit `pure`:

```morloc
mkConfig :: Real -> Int -> <Rand> Config
mkConfig lr bs = do
  seed <- randint 0 999999
  {lr = lr, bs = bs, seed = seed}
```

The final record literal is pure; it coerces to `<Rand> Config` to match the declared return.

The lift inserts a thunk wrapper at compile time. At runtime, forcing the thunk just yields the pure value.

### Design rationale: capabilities, not commitments

Two readings of `<E> T` exist in the effect-typing literature:

- **Commitment**: `<E> T` means "this value DOES perform E." Pure values are structurally distinct and must be lifted with `pure`/`return`. Haskell (`IO`), Idris `Eff`, Frank, and Effekt take this position.
- **Capability**: `<E> T` means "this value MAY perform E." The row is an upper bound on permitted effects, not a claim about performed effects. A pure value inhabits any effect slot trivially. Koka takes this position; so does Morloc.

Morloc chose capabilities because:

1. **Ergonomics.** `pure` at every boundary is noise. `@catch (@load p) 42` should not require `@catch (@load p) (pure 42)`. Do-block trailing expressions should not require `pure`. Pure exports like `foo :: <IO> Int; foo = 42` should typecheck without ceremony.

2. **No correctness issue.** Subsumption `E1 ⊆ E2` is monotone in the "more effects" direction; a pure value is the trivial case that uses none of the granted capabilities. The reading is internally consistent.

3. **The "dishonesty" objection dissolves.** A literal `4 :: <Rand> Int` is not a lie under the capability reading — 4 names a value in the set of random-Int outcomes where the probability of 4 is exactly 1. The same intuition already applies to classical subtyping: `5 :: Number` is not dishonest despite 5 being an integer.

### Implementation site

The pure-into-EffectU subtype rule lives in `library/Morloc/Typecheck/Internal.hs`, above the InstantiateL arm. It fires for both concrete and existential LHS; the existential case is what makes `@catch (f x) fb` typecheck when the fallback is bare in a polymorphic effect row.


## Forcing Effects

A suspended value is forced in one of two ways:

### Do-block bind

Inside a `do` block, the `<-` operator forces the right-hand side and binds the resulting pure value:

```morloc
do
  x <- randint 0 9     -- forces <Rand> Int, binds x :: Int
  ...
```

A do-block has type `<E> T` where `E` is the union of effects of all forced sub-expressions and `T` is the type of the trailing expression.

### Unit force

The `!` prefix operator forces a single suspended value at expression position:

```morloc
!randint 0 9   :: Int
```

A `!e` expression has the inner type of `e` but propagates `e`'s effect set to the enclosing term. Using `!` inside a function body widens that function's effect set:

```morloc
addRand :: Int -> <Rand> Int
addRand x = x + !randint 0 9
```

If the enclosing function's declared effect set does not cover the forced effect, the program is rejected (see [[#effect-checking]]).

## Effect Inference

The effect set of an expression is determined structurally:

```
  effects(x)             =  E      where x has type <E> T
  effects(x)             =  {}     where x has pure type
  effects(f a)           =  effects(f) union effects(a)
  effects(!e)            =  effects(e)
  effects(\x. e)         =  effects(e) minus effects bound by lambdas under e
  effects(let x = e1 in e2)
                         =  effects(e1) union effects(e2)
  effects(do { ... })    =  union of effects of all forced sub-expressions
  effects({f1 = e1, ...})  =  {} if all ei pure; otherwise the record is ill-typed
                              (use a do-block to construct effectful records)
  effects(<E> T literal annotation)
                         =  E
```

Effects do *not* propagate through unforced thunks. A reference to `randint :: Int -> Int -> <Rand> Int` in a position where the function value itself is the meaning (e.g. passing it as an argument) carries no effect; only application or forcing does.

## Effect Checking

For every function definition with a declared signature, the inferred effect set of the body must be a subset of the declared effect set:

```
  inferred = effects(body)
  declared = effect set in the type signature
  -------------------------------------------
  rule:   inferred subset of declared
```

Three failure modes follow directly from this rule:

**Widening (rejected).** Body has an effect not in the signature:

```morloc
addRand :: Int -> Int               -- declared no effects
addRand x = x + !randint 0 9        -- body has <Rand>
                                    -- ERROR: <Rand> not in <>
```

**Narrowing (rejected).** A value with more effects is bound to a slot with fewer:

```morloc
rint :: <IO, Error> Int
a :: <IO> Int
a = rint                            -- ERROR: <Error> not in <IO>
```

**Argument widening (rejected).** A function expects an argument with a specific effect set; the passed argument's effect set is not a subset:

```morloc
f :: <IO, Error> Int -> Int
f = ...
g :: <Rand> Int -> Int
g x = f x                           -- ERROR: <Rand> not in <IO, Error>
```

**Declared-but-unused (allowed, may warn).** The signature lists effects the body does not introduce. The pure-to-effect coercion lifts the body silently; this is sometimes intentional (forward-compatible stubs). A future `-Weffect-declared-unused` may flag it.

## Source Signatures

Foreign primitives are the only origin of effects, because foreign bodies are opaque to the compiler. The author of a source binding declares effects in the imported signature:

```morloc
source Py from "rand.py" ("randint :: Int -> Int -> <Rand> Int")
source Py from "io.py"   ("readFile :: Str -> <IO> Str")
```

The compiler does not validate these claims; they are an assertion by the library author. A pipeline composed of such primitives inherits effects through normal inference.

## Restrictions

- Effect labels in source signatures are required; the compiler does not infer them across the FFI boundary.
- Effects cannot be parameterized (no `forall e. <e> T`); generic higher-order combinators accept effectful arguments through normal unification (`(a -> b)` with `b = <E> T`).
- Effects in record and tuple literals are disallowed; construct effectful records inside a do-block.
- The `Error` effect short-circuits within `sequenceE`/`mapE` (fail-fast). Validation-style error collection is expressed as data, not as the `Error` effect.

## See Also

- [[subtyping.md]] -- general subtyping rules
- [[inference.md]] -- bidirectional type inference
- [[../language/expressions.md]] -- expression syntax including `do` and `!`
