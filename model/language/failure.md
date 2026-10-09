# Failure

`Try`, `@try`, `@throw`: which failures are values and which can be caught. Prefix: FAIL.

### FAIL-1 Failure is a value, not an effect
Intent: proposed
Code: unaudited

A fallible operation returns `Try e a`, an ordinary `data` type with arms
`Err e` and `Ok a`. There is no failure effect, and no catch construct for
effects.

### FAIL-2 `@throw` abandons the computation
Intent: proposed
Code: unaudited

    @throw :: Str -> a

`@throw msg` never returns; it raises a recoverable failure carrying `msg`.
It has no effect row.

### FAIL-3 `@try` turns a recoverable failure into `Err`
Intent: proposed
Code: unaudited

    @try :: <e> a -> <e> (Try Str a)

A completed evaluation of the argument is `Ok v`. A recoverable failure that
escapes it is `Err msg`, carrying the failure's message. The effect row
passes through unchanged, and a pure argument gives a pure result.

### FAIL-4 `@try` catches every recoverable failure, and nothing else
Intent: ruled 2026-07-18
Code: unaudited

Morloc throws and the native exceptions of sourced code are alike
recoverable failures, and `@try` catches both. Internal morloc errors
(bugs) and interrupts are not recoverable failures and bypass `@try`.

### FAIL-5 A failure crosses languages unchanged
Intent: proposed
Code: unaudited

A recoverable failure raised in a call realized in another language than the
`@try` is caught as if it were raised in the same language.

### FAIL-6 A pool that dies
Intent: open
Code: unaudited

When the process running a call is killed (out of memory, an external
kill), is that (a) not recoverable and bypasses `@try`, like an interrupt,
or (b) a recoverable failure that `@try` turns into `Err`?
