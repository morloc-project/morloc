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

The order of labels is irrelevant. Duplicates collapse. The empty row is a row: `<> T` is a suspension that performs nothing, a distinct type from `T`. A do-block that forces nothing has this type.

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

There is no coercion between `T` and `<E> T` in either direction. The only way from `<E> T` to `T` is to run it inside a do-block, and the only way from `T` to `<E> T` is `do v` (see "The law" below).

See [[subtyping.md]] for the full subtyping relation.

## The law

Morloc's suspensions follow call-by-push-value. Every term denotes a value;
`<E> T` is the value `U(F_E T)`: a thunk of a computation that may perform
the effects in `E` and yields a `T`. The rules:

1. **Two sorts.** `T` and `<E> T` are different types with no coercion
   either way. Effect labels index `F`; a row is an upper bound on what a
   run may do; rows compare by inclusion; the compiler gives labels no
   operational meaning.

2. **Introduction.** `do { s1; ...; sn; tail }` builds the thunk
   `thunk(s1; ...; sn; tail')`. It is the only construct that builds a
   suspension from statements and the only place `return` appears: a pure
   tail `v` is `return v`; a suspension tail `t` is `force t` (so
   `do t = t`). A `do` that forces nothing still builds a thunk:
   `do 42 :: <> Int`. This is the only lift: `foo (do 42)` fills a
   `<e> Int` slot; `foo 42` is a type error.

3. **Elimination.** `x <- t` is `force t to x`; a bare statement is
   `force t to _`. Nothing else runs a suspension: not application, not
   binding, not projection, not passing, not returning, not crossing a
   pool. `!e` is sugar for a `<-` at the nearest enclosing do-block.
   Forcing a value that is not a suspension is a type error.

4. **Per use.** Every force runs the computation again. The compiler
   never shares, hoists, memoizes, duplicates or drops a run. Pure work is
   call-by-value and may be reordered; only row work is ordered. The
   timing of a pure abort (a `@throw`, a host exception) is unspecified.

5. **Application.** `f x` where `f :: A -> <E> C` denotes
   `thunk((force f) x)`; it runs nothing. Pure application is eager.
   Arguments are values: a pure argument expression is evaluated once at
   the application and captured; a suspension argument is captured as a
   thunk.

6. **Rows.** `<E1> T1 <: <E2> T2` iff `E1` is a subset of `E2` and `T1 <: T2`. A row
   variable with no other constraint is `<>` at ground. `<E> (<E'> T)` is
   two thunk layers, never merged with `<E,E'> T`; it is written only with
   parentheses (`<E> <E'> T` is a parse error), and a do-block builds one
   when it is checked against that type: its suspension tail is then
   returned rather than run.

7. **Positions.** A suspension is the same value in every position:
   argument, parameter, `let`, return, record field, list element, sum
   arm, optional payload, instance of a type variable, wire. In every
   pool it is a closure of no arguments and shares all of `A -> B`'s
   machinery, including crossing a pool boundary as a closure that calls
   back to its home pool once per run.

8. **The host adapter.** A host language has no thunks: calling is
   forcing. At a sourced function the compiler translates:
   `eager[[D]] = D`; `eager[[<E> R]] = () -> eager[[R]]`;
   `eager[[A -> R]] = eager[[A]] -> eager[[R]]`, with the terminal `<E>`
   absorbed into the call. So `f :: A -> B -> <E> C` is called as
   `f(x, y)`, `g :: B -> <E> C` as `g(y)`, and `h :: <E> C` as `h()`;
   `f x` on the morloc side is the thunk of that call; a morloc function
   handed to a host at an `A -> <E> C` slot is passed as `\a -> force(g a)`;
   a suspension handed to a host is the callable itself. The program's
   caller is a host too: a suspension at the root of a command's argument
   is built from the value supplied, the suspension at the root of its
   result is run for the caller, and a suspension below the root of
   either is rejected when the program is built. A remote pool is not a
   host: nothing is adapted, the callee's entry point runs one layer, and
   the caller holds `thunk(rpc)`.

9. **Instrumentation.** A directive on a suspension (`log@action`,
   benchmark labels, `cache: true` on an effectful manifold) wraps the
   run, never the application, which is free. `cache@` builds an
   idempotent suspension by the user's explicit choice.

10. **Handlers.** `escapable effect E` means E has handlers; only a
    sourced function can be one (`<E, e> a -> <e> a`), and it discharges
    only effects of its own language. The inescapable-argument rule is a
    lint on declared signatures.

Vocabulary: `<E> T` is a *suspension*; `E` is its *row* of *effects*. A
suspension is never "erased", "stripped" or "an annotation on `T`"; it is
only ever run.

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

### Inline force

The `!` prefix operator is sugar for a `<-` bind at the nearest enclosing do-block: `!e` in an expression is `x <- e` inserted above the statement that contains it, with `x` in its place.

```morloc
addRand :: Int -> <Rand> Int
addRand x = do (x + !randint 0 9)
```

desugars to

```morloc
addRand x = do
  r <- randint 0 9
  x + r
```

so the effect is performed by the do-block that contains the `!`, which must exist and must carry the row.

If the enclosing function's declared effect set does not cover the forced effect, the program is rejected (see [[#effect-checking]]).

## Effect Inference

The effect set of an expression is determined structurally:

```
  effects(x)             =  {}     holding a suspension performs nothing
  effects(f a)           =  effects(f) union effects(a)
                                   the call-by-value work of the arguments
  effects(x <- t)        =  E      where t has type <E> T
  effects(!e)            =  effects(e <- ...)
  effects(\x. e)         =  effects(e) minus effects bound by lambdas under e
  effects(let x = e1 in e2)
                         =  effects(e1) union effects(e2)
  effects(do { ... })    =  union of effects of all forced statements
  effects({f1 = e1, ...})  =  union of effects(ei); a field may hold a suspension
```

Only a force performs effects. A reference to `randint :: Int -> Int -> <Rand> Int`, a suspension held in a variable, a suspension stored in a record or list, and an application `randint 0 9` all carry no effect; the effect appears where the suspension is run.

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

**Declared-but-unused (allowed, may warn).** The signature lists effects the body does not introduce, as in `stub :: Int -> <IO> Int; stub k = do (k + 1)`, whose body is a `<> Int` and satisfies `<IO> Int` by row inclusion. This is sometimes intentional (forward-compatible stubs). A future `-Weffect-declared-unused` may flag it. The body must still be a suspension: `stub k = k + 1` is a type error.

## Source Signatures

Foreign primitives are the only origin of effects, because foreign bodies are opaque to the compiler. The author of a source binding declares effects in the imported signature:

```morloc
source Py from "rand.py" ("randint :: Int -> Int -> <Rand> Int")
source Py from "io.py"   ("readFile :: Str -> <IO> Str")
```

The compiler does not validate these claims; they are an assertion by the library author. A pipeline composed of such primitives inherits effects through normal inference.

## Restrictions

- Effect labels in source signatures are required; the compiler does not infer them across the FFI boundary.
- A row may carry one variable (`<IO, e> T`); a suspension instantiates a type variable like any other value (`id (randint 0 9)`, a list `[<Rand> Int]`, a record field).
- A data-reading intrinsic (`@load`, `@read`, `@next`, `@open`, `@stdin`) yields data: its result type may not contain a suspension or a function. Inference can solve the result to one, as when a loaded value is one arm of a `match` whose other arm is a do-block; the build refuses the read and asks for an annotation, and the intended form is `(Ok v) = do v`.
- Failure is NOT an effect. A fallible operation returns `Try e a` (declared in the `internal` module) and `@try` converts an otherwise-uncaught native throw into one; there is no `Err` effect and no `@catch`. An effect row describes what a call may DO, and failing is a property of what it returns.

## See Also

- [[subtyping.md]] -- general subtyping rules
- [[inference.md]] -- bidirectional type inference
- [[../language/expressions.md]] -- expression syntax including `do` and `!`
