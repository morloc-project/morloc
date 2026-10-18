# Python

What sourced Python code must do, and what morloc does for it. These are
the Python forms of rules stated once for every language; each item cites
its rule. Prefix: PY.

Rules for Python stated elsewhere: NATIVE-5 (records), NUM-1 (`Int` is the
native default integer), WIRE-4 (`Vector n Int`), RECUR-3 (the
interpreter's recursion limit).

### PY-1 A sourced Python function never changes an argument in place
Intent: ruled 2026-10-18
Code: unaudited

A morloc value read twice may be one shared Python object, so a sourced
function never changes an object it is given: no `list.sort()`, `append`,
item or attribute assignment, `dict.update`, or in-place numpy operation
(`a += 1`, `a.sort()`, `out=`) on an argument. A function that needs to
change a value copies it first. The rule is not checked; a program that
breaks it has no defined result, since whether another reader sees the
change depends on where the compiler shares and copies (EVAL-7). It holds
until morloc has a way to say where a value may be changed.

### PY-2 A Python result may share objects with its arguments
Intent: proposed
Code: unaudited

A sourced Python function may return an argument, a part of one, or a
numpy view onto one. Reference counting keeps what it refers to alive,
and PY-1 keeps it unchanged, so the sharing cannot be observed.
