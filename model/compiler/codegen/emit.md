# Emit (EMIT)

The generated pools: which parts of a program become functions of their
own, and what each generated function costs at run time.

### EMIT-1 A sub-expression becomes a function only where one is needed
Intent: ruled 2026-11-03
Code: deviates #175

Inside a pool, an application is emitted as a function of its own only
when it is one of:

- a call of a named morloc function (its target, or the native entry of a
  recursive one);
- a closure: a lambda, a partial application, or a suspension;
- a labeled call (benchmark, log or cache labels attach to it);
- a pool entry point or the callee of a call from another pool;
- any application, when the program is built with `--debug`.

Every other application is emitted as an expression at its use in the
caller. Serves RECUR-4.

### EMIT-2 Inlining an application never moves its evaluation
Intent: ruled 2026-11-03
Tests: spec-emit-2-1
Code: conforms 2026-10-16

An application emitted under EMIT-1 is evaluated where the call it replaces
was: its statements go immediately before the statement that uses its
value, in the same block. A position that is not evaluated eagerly, an arm
of a conditional, the second operand of `&&` or `||` (BASE-3), or a rung
of EMIT-4, is a block of its own, so what is inlined there stays there.

### EMIT-3 A generated function adds no per-call error handler
Intent: ruled 2026-10-16
Tests: spec-emit-3-1, spec-emit-3-2
Code: conforms 2026-11-04

A generated function that is not a boundary has no handler for a failure
and no destructor that inspects one. The trace of DIAG-5 is assembled
where the failure is reported or caught. In Python the failure's own
traceback carries it. R walks the calls on its stack at the boundary. In
C++ and Rust a named function pushes its label on the thread's frame
stack (STATE-5) and pops it on its normal path. The pop is placed so the
function's result is neither copied nor moved more often than with no
pop. A returned local is still returned by name, and any other result
initializes a local of the return type that is then returned by name. A
named function's body always ends in a return, and the compiler rejects
one that does not. The boundaries of STATE-6 are the only generated
handlers. Serves RECUR-4 and DIAG-5.

### EMIT-4 A chain of `&&` or `||` is written flat
Intent: ruled 2026-11-03
Tests: spec-emit-4-1, spec-emit-4-2, spec-emit-4-3
Code: conforms 2026-11-03

A chain of operands of one of the two operators loads and runs at any
length. A run of operands that need no statements is written as the
language's operator between them, with no parentheses inside the run, and
at most 16 operands to a run. The chain is a sequence of rungs: one
boolean holds the chain's value so far, and each later rung, the run or
the operand with its statements, is a block entered only while that value
leaves the chain undecided (true for `&&`, false for `||`). An operand that
reads a value bound in an earlier operand stays inside that operand's
block. A chain of mixed operators keeps the parentheses its grouping
needs, and a chain inside a larger expression is parenthesized as a whole.

Why: R's parser refuses about 25 nested blocks. R parses a run as nested
calls, as deep as the run is long: evaluating it fails at about 5000
operands, and its byte compiler is superlinear in the depth (one run of
256 operands compiles in 2.6 s, 1024 operands in runs of 16 in 1.1 s;
CHAIN-5). A rung costs one test of the boolean.

### EMIT-5 No sequence adds a block level per element
Intent: ruled 2026-11-03
Tests: spec-emit-5-1, spec-emit-5-2
Code: deviates (../issues/codegen-limits.md)

The block depth of generated code is bounded by the nesting of the source
expression it comes from, never by the length of a sequence in it: a chain
of `&&` or `||` (EMIT-4), a chain of guards, or the guards of a tail loop.
