# Effects

Suspensions: their types, how they are built and run, effect rows. Prefix: EFF.

Morloc suspensions follow call-by-push-value: `<E> T` is a thunk of a
computation that may perform the effects in `E` and yields a `T`. Effect
labels are tags. The compiler gives them no operational meaning.

## Suspensions

### EFF-1 A suspension is a value distinct from its result
Intent: ruled 2026-09-12
Code: unaudited

`<E> T` and `T` are different types. A suspension is never erased,
stripped, or treated as an annotation on `T`: in every language it is a
callable of no arguments, and the only way from `<E> T` to `T` is to run it.

Why: the doctrine "effects are erased" produced #54 and #58.

### EFF-2 A suspension is the same value in every position
Intent: ruled 2026-09-12
Code: unaudited

Argument, parameter, `let`, return value, record field, list element, sum
arm, optional payload, instance of a type variable, and wire: a suspension
is a suspension in each, and crosses a pool boundary as a closure that calls
back to its home pool once per run. Its wire form is WIRE's to state.

### EFF-3 Every run of a suspension runs the computation again
Intent: ruled 2026-09-12
Code: unaudited

The compiler never shares, hoists, memoizes, duplicates or drops a run.
Only explicit instrumentation chosen by the user (ANN) may make a
suspension idempotent.

## Building and running

### EFF-4 `do` is the only way to build a suspension from statements
Intent: proposed
Code: unaudited

`do { s1; ...; sn; tail }` builds one suspension. A pure tail is returned;
a suspension tail is run. A `do` that runs nothing still builds a
suspension: `do 42 :: <> Int`.

### EFF-5 There is no coercion between `T` and `<E> T`
Intent: proposed
Code: unaudited

A pure value in a suspension slot is a type error; `do v` is the only lift.
A suspension where a pure value is expected is a type error.

Why: the 2026-09-12 ruling allowed either a constant suspension or a type
error, never a bare value; the type error was chosen.

### EFF-6 Only a bind runs a suspension
Intent: proposed
Code: unaudited

`x <- t` and a bare statement `t` in a do-block run `t`. Application,
binding, projection, passing, returning and crossing a pool run nothing.
Running a value that is not a suspension is a type error.

### EFF-7 `!e` is a bind at the nearest enclosing do-block
Intent: proposed
Code: unaudited

`!e` inserts `x <- e` above the statement that contains it, with `x` in its
place. A `!` with no enclosing do-block is a type error.

## Rows

### EFF-8 Rows compare by inclusion
Intent: proposed
Code: unaudited

    E1 subset of E2      T1 <: T2
    -----------------------------
        <E1> T1 <: <E2> T2

Label order is irrelevant and duplicates collapse. Narrowing is rejected.
A row variable with no other constraint is `<>` at ground.

### EFF-9 Nested suspensions are never merged
Intent: proposed
Code: unaudited

`<E> (<E'> T)` is two layers and is not `<E,E'> T`. It is written only with
parentheses; `<E> <E'> T` is a parse error.

### EFF-10 A body's effects must fit its declared row
Intent: proposed
Code: unaudited

The effects a definition's body runs must be a subset of the row in its
signature. Declaring effects the body does not run is legal.

### EFF-11 Effects originate only in source signatures
Intent: proposed
Code: unaudited

A sourced function's declared row is an unchecked claim by its author.
Every other row is inferred from composition.

### EFF-12 Escapable effects and handlers
Intent: open
Code: unaudited

The current model says `escapable effect E` gives `E` handlers, and only a
sourced function `<E, e> a -> <e> a` can be one. Is this part of the
language, or should handlers be removed until there is a use for them?
