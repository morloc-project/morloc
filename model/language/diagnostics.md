# Diagnostics

The compiler's error contract: never crash, every rejection located, warnings. Prefix: DIAG.

### DIAG-1 The compiler never crashes
Intent: proposed
Code: unaudited

On any input, the compiler either builds the program or rejects it with a
diagnostic. An internal error, an uncaught exception or a crash is a defect.

### DIAG-2 Every rejection names a source location
Intent: proposed
Code: unaudited

A rejection names the file, line and column of the construct that caused
it, whether it is in a `.loc` file, a docstring, or a configuration file the
build reads.

### DIAG-3 A failure in a built program exits non-zero and says where
Intent: proposed
Code: unaudited

An error in a run goes to stderr and the process exits non-zero. An error
raised in a pool names the function and its source position. A panic ends
the program with status 70, as `PANIC` states.

### DIAG-4 Whether a misplaced directive is an error
Intent: open
Code: unaudited

A docstring directive written where it has no meaning is kept as prose and
the build warns. Either (a) it stays a warning; or (b) it is a rejection,
since the program then silently lacks the interface it declares.
