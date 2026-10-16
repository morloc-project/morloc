# Emit (EMIT)

The generated pools: which parts of a program become functions of their
own, and what each generated function costs at run time.

### EMIT-1 A sub-expression becomes a function only where one is needed
Intent: ruled 2026-10-16
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
Intent: ruled 2026-10-16
Tests: spec-emit-2-1
Code: conforms 2026-10-16

An application emitted under EMIT-1 is evaluated where the call it replaces
was: its statements go immediately before the statement that uses its
value, in the same block. A position that is not evaluated eagerly, an arm
of a conditional or the second operand of `&&` or `||` (BASE-3), is a block
of its own, so what is inlined there stays there.

### EMIT-3 A generated function adds no per-call error handler
Intent: ruled 2026-10-16
Code: deviates #175

A generated function records where it is without a handler of its own for
a failure: its location is collected only when a failure unwinds through
it, and the trace of DIAG-5 is assembled where the failure is reported or
caught, at the pool's entry point or the enclosing `@try`. Any trace state
kept outside the failure is moved into the failure wherever control leaves
generated code, so a failure caught and discarded in sourced code leaves no
trace behind. Serves RECUR-4 and DIAG-5.
