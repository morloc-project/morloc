# Annotations

Caching, logging, benchmark labels, unrolling: what instrumentation may change and what it may not. Prefix: ANN.

### ANN-1 Default behaviour is sound; only an opt-in fast mode may drop checks
Intent: ruled 2026-10-06
Code: unaudited

Without options, a program performs every runtime check the spec requires.
A mode that drops checks for speed must be explicitly requested and
documented as accepting that risk.

### ANN-2 Instrumentation wraps the run of a suspension, never the application
Intent: proposed
Code: unaudited

`cache`, `log` and `benchmark` on a label wrap each run of the labeled
computation. Building a suspension by application is free and is not
instrumented (see `EFF`).

### ANN-3 Logging and benchmarking never change a program's value or stdout
Intent: proposed
Code: unaudited

Log and benchmark lines go to stderr. A program with every label's `log` and
`benchmark` turned off computes and prints the same result.

### ANN-4 A cached call returns the value the uncached call would
Intent: proposed
Code: unaudited

`cache: true` is the user's claim that the labeled call is idempotent. A
hit requires the same code and equal argument values. A call that fails is
not memoized.

### ANN-5 What a cache key must cover
Intent: open
Code: unaudited

The manual keys a call on the generated pool source, `hash-include` files and
build parameters. A sourced file the pool loads rather than contains (a
Python or R module) may then change without a miss, and the stale hit is a
wrong value. Either (a) the key covers every sourced file and what it loads;
or (b) it covers sourced files, and the user lists the rest.

### ANN-6 Which language a benchmark row names
Intent: open
Code: unaudited

The manual says `{lang}` is the language that ran the work, and also that a
call crossing a pool is timed on the caller's side, so a Python call shows
`cpp`. Either (a) the row names the callee's language and times the callee;
or (b) it names the caller's and includes the round trip.
