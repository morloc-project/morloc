# Failure

`Try`, `@try`, `@throw`: which failures are values and which can be caught. Prefix: FAIL.

### FAIL-1 Failure is a value, not an effect
Intent: proposed
Code: conforms 2026-10-21

A fallible operation returns `Try e a`, an ordinary `data` type with arms
`Err e` and `Ok a`. There is no failure effect, and no catch construct for
effects.

### FAIL-2 `@throw` abandons the computation
Intent: proposed
Code: conforms 2026-10-21

    @throw :: Str -> a

`@throw msg` never returns; it raises a recoverable failure carrying `msg`.
It has no effect row.

### FAIL-3 `@try` turns a recoverable failure into `Err`
Intent: proposed
Code: conforms 2026-10-21

    @try :: <e> a -> <e> (Try Str a)

A completed evaluation of the argument is `Ok v`. A recoverable failure that
escapes it is `Err msg`, carrying the failure's message. The effect row
passes through unchanged, and a pure argument gives a pure result.

### FAIL-4 `@try` catches every recoverable failure, and nothing else
Intent: ruled 2026-07-18
Code: conforms 2026-10-21

Morloc throws and the native exceptions of sourced code are alike
recoverable failures, and `@try` catches both. Internal morloc errors
(bugs) and interrupts are not recoverable failures and bypass `@try`.

### FAIL-5 A failure crosses languages unchanged
Intent: proposed
Code: conforms 2026-10-21

A recoverable failure raised in a call realized in another language than the
`@try` is caught as if it were raised in the same language.

### FAIL-6 A pool that dies is not a recoverable failure
Intent: ruled 2026-10-28
Code: conforms 2026-10-29
Tests: spec-fail-6-1

When the process running a call ends during it (out of memory, an external
kill, a crash), the failure is not recoverable: it bypasses `@try`, like an
interrupt, and the run fails with the call-failure status. A pool whose
callee died reports that death, not an internal error of its own.
Running out of shared memory ends every pool (ruled 2026-10-29), so it
fails the run the same way.
