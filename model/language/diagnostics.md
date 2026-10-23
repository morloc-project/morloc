# Diagnostics

The compiler's error contract: never crash, every rejection located, warnings. Prefix: DIAG.

### DIAG-1 The compiler never crashes
Intent: proposed
Code: deviates (issues/diagnostics-docs.md)

On any input, the compiler either builds the program or rejects it with a
diagnostic. An internal error, an uncaught exception or a crash is a defect.

### DIAG-2 Every rejection names a source location
Intent: proposed
Tests: spec-diag-2-1
Code: deviates (issues/programs.md)

A rejection names the file, line and column of the construct that caused
it, whether it is in a `.loc` file, a docstring, or a configuration file the
build reads.

### DIAG-3 A failure in a built program exits non-zero and says where
Intent: proposed
Code: conforms 2026-10-21

An error in a run goes to stderr and the process exits non-zero. An error
raised in a pool names the function and its source position. A panic ends
the program with status 70, as `PANIC` states.

### DIAG-5 A failure's trace names the morloc calls it passed through
Intent: ruled 2026-10-16
Tests: spec-diag-5-1, spec-diag-5-2
Code: deviates #175

The trace of a failure, printed by the program and carried in the message
of a `@try` `Err`, lists innermost first each call of a named morloc
function between the point of failure and where it is reported or caught,
each with its language and the source position of the function's
definition. A sub-expression inlined into
its caller is not listed. The printed trace shows a run of identical
consecutive lines once, followed by the number of repeats; the `@try`
message keeps every line. A program built with `--debug` lists every
generated function with its arguments: a frame is named by its morloc name,
or by the sourced function it calls, and placed at its call site, or at its
definition when it is an exported head. An argument is dumped at most once
per content; past the dump budget it is listed bare and the trace ends with
the number not saved.

### DIAG-4 Whether a misplaced directive is an error
Intent: open
Code: unaudited

A docstring directive written where it has no meaning is kept as prose and
the build warns. Either (a) it stays a warning; or (b) it is a rejection,
since the program then silently lacks the interface it declares.
