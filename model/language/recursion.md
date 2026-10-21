# Recursion

Tail calls, recursion depth, and recursion across languages. Prefix: RECUR.

### RECUR-1 Functions may be recursive and mutually recursive
Intent: proposed
Code: conforms 2026-10-21

A term may refer to itself, and terms may refer to each other in a cycle.
The recursion is carried out in the language that realizes the terms.

### RECUR-2 Morloc owes tail-call optimization in every language
Intent: ruled 2026-10-05
Tests: spec-recur-2-1
Code: conforms 2026-10-16

A call in tail position does not grow the stack, in every target language,
including languages that do not optimize tail calls themselves. Tail
recursion runs in constant stack space at any depth. Measured by DEPTH-3.

### RECUR-3 A host's own recursion limit is the host's, not morloc's
Intent: ruled 2026-10-05
Code: conforms 2026-10-21

Non-tail recursion in Python or R that reaches that language's own
recursion limit fails as the same recursion written natively would. That
limit is not a morloc defect.

### RECUR-4 Recursion reaches the depth the language itself allows
Intent: ruled 2026-10-16
Tests: spec-recur-4-1
Code: conforms 2026-10-16

Non-tail recursion realized in a language L reaches the depth that the same
recursion written natively in L reaches on the same stack, less a constant
that does not grow with depth. This holds for recursion written in morloc,
where one level costs one native frame, and for recursion in sourced code,
below which morloc places a constant number of frames. Failing at that
depth is L's limit (RECUR-3); failing short of it is morloc's. In R,
recursion written in morloc reaches at least 80% of the native depth.
Measured by DEPTH-1 (written in morloc) and DEPTH-2 (sourced).

Why: R's generated code nests evaluation more deeply per level than
hand-written R; ruled acceptable 2026-10-16.

### RECUR-5 A compiled pool runs calls on a native main thread's stack
Intent: ruled 2026-10-16
Tests: spec-recur-5-1
Code: conforms 2026-10-16

A C++ or Rust pool runs every call on a worker thread with the stack a
native program's main thread gets: the soft stack limit (`ulimit -s`, 8 MiB
by default), or 8 MiB when that is unlimited. So recursion in sourced C++
or Rust reaches the depth it reaches natively (RECUR-4). At the ceiling the
pool crashes, as the native program would (RECUR-3).

### RECUR-6 Tail calls that cross a language boundary
Intent: open
Code: unaudited

If `isEven` is realized in Python and `isOdd` in C++, each tail call crosses
a pool. Does RECUR-2 hold across the crossing, so depth is unbounded, or is a
cross-language cycle bounded by the depth of nested crossings?
