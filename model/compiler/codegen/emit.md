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
- an operand of `&&` or `||` (BASE-3), so a chain is the language's own
  operator rather than nested conditionals, whose depth R's parser bounds;
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
Tests: spec-emit-3-1
Code: conforms 2026-10-16

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
