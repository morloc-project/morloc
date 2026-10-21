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
Code: conforms 2026-10-13

`cache`, `log` and `benchmark` on a label wrap each run of the labeled
computation. Building a suspension by application is free and is not
instrumented (see `EFF`).

### ANN-3 Logging and benchmarking never change a program's value or stdout
Intent: proposed
Code: conforms 2026-10-21

Log and benchmark lines go to stderr. A program with every label's `log` and
`benchmark` turned off computes and prints the same result.

### ANN-4 A cached call returns the value the uncached call would
Intent: proposed
Code: conforms 2026-10-21

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

### ANN-7 A label wraps each call of a function made through the labeled name
Intent: ruled 2026-10-07
Tests: spec-ann-7-1
Code: conforms 2026-10-13

`lab@h` at a use site is `h` with the label's hooks around every completed
call made through that reference: one log line pair, one benchmark sample,
one cache lookup or one remote job per call. The hooks wrap exactly one call
of `h`. Calls `h` makes itself, recursive ones included, are not labeled.
What `h`'s body computes does not change where the hooks go.

### ANN-8 Only a function or a suspension may carry a label
Intent: ruled 2026-10-13
Tests: spec-ann-8-1, spec-ann-8-2, spec-ann-8-3
Code: conforms 2026-10-13
Why: ruled 2026-10-07 for functions only; suspensions admitted by ANN-10.

A label is written on a name (`lab@h`), never on a larger expression; a
computation is labeled by naming it as a function or a suspension. Labeling
a name of any other type is an error.

### ANN-9 The arguments of a labeled call are evaluated by the caller
Intent: ruled 2026-10-13
Tests: spec-ann-9-1
Code: conforms 2026-10-13

A labeled call receives its arguments as values, evaluated where the call is
made, outside the hooks. A cache key covers those values. A remote job runs
the labeled function on values computed by its parent: the process that
submitted it, which may itself be a remote job.

### ANN-10 A label on a suspension wraps each run of it
Intent: ruled 2026-10-13
Tests: spec-ann-10-1, spec-ann-10-2, spec-ann-10-3
Code: conforms 2026-10-13

`lab@act` where `act :: <E> T` is a suspension of the same type whose every
run is one event (ANN-2): one log line pair, one benchmark sample, one cache
lookup or one remote job per run. A cached run is keyed on the values the
suspension captured, a captured function by its identity and the values it
captured in turn. A labeled call whose result is a suspension is the same:
the hooks wrap each run of the suspension it returns, not the call, so work
the call does before building the suspension is outside them.

### ANN-11 A cache or a remote job needs a function with an identity
Intent: proposed
Tests: spec-ann-11-1, spec-ann-11-2, spec-ann-11-3
Code: conforms 2026-10-13

A label asking for `cache` or `remote` on a function or suspension bound by
a lambda or a let is an error: neither can name what it runs. `log` and
`benchmark` labels are allowed there. A name bound in a `where` is a
definition and may carry any label.

### ANN-12 A remote result outlives its call only when the label caches it
Intent: ruled 2026-10-13
Tests: spec-ann-12-1
Code: conforms 2026-10-13

A remote job's inputs and result pass through files on a filesystem the
parent and the job share. They are removed once the parent has read the
result, so a later call submits a new job. A label that also asks for
`cache` stores the result as any cached call does (ANN-4). A job that fails
keeps its files, so its stderr can be read.

