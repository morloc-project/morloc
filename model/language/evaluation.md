# Evaluation

Evaluation order, strictness, sharing of named values, and what may be reordered. Prefix: EVAL.

## Strictness

### EVAL-1 Pure application is strict
Intent: proposed
Code: unaudited

A pure argument expression is evaluated once, at the application, however
many times the function uses it. A suspension argument is passed as the
suspension, not run (see `EFF`).

### EVAL-2 Pure work may be reordered; only effects are ordered
Intent: proposed
Code: unaudited

The compiler may move, parallelize or reorder pure work. Only effect
statements in a `do` block keep their written order, and none is shared,
duplicated or dropped (see `EFF`). When a pure abort happens is unspecified.

## Named values

### EVAL-3 A named value is computed at most once per evaluation of its scope
Intent: proposed
Code: unaudited

A `where` or `let` binding, a pure `let` in a `do` block, or a top-level
constant is computed at most once each time its scope is evaluated, at the
nearest point every use passes through. A top-level value is computed at
most once per command. A value read by several languages is computed once
and passed to each.

### EVAL-4 Placement respects `@try` regions and effect barriers
Intent: proposed
Code: unaudited

A value read only inside a `@try` argument is computed inside it, so its
failure is that `@try`'s `Err`. A pure `let` in a `do` block is never
computed before an effect statement written above it.

### EVAL-5 Whether an unused pure `let` is computed
Intent: open
Code: unaudited

The manual says an unused `where` binding is never computed but an unused
`let` "still runs where it is written". EVAL-3 places a value at its uses,
and an unused value has none. Observable when the value fails. Either (a) an
unused pure binding is never computed, `let` or `where`; or (b) `let` is
computed where written and `where` is not.

### EVAL-6 Where a value used only inside a lambda is computed
Intent: open
Code: unaudited

The manual says once, when the lambda is built; EVAL-3 would place it in the
body, once per call. Either (a) at construction, so a failing value fails
even if the lambda is never called; (b) on each call; or (c) at construction
only when it does not depend on the lambda's parameters and cannot fail.
