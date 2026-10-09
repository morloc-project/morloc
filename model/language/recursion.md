# Recursion

Tail calls, recursion depth, and recursion across languages. Prefix: RECUR.

### RECUR-1 Functions may be recursive and mutually recursive
Intent: proposed
Code: unaudited

A term may refer to itself, and terms may refer to each other in a cycle.
The recursion is carried out in the language that realizes the terms.

### RECUR-2 Morloc owes tail-call optimization in every language
Intent: ruled 2026-10-05
Code: unaudited

A call in tail position does not grow the stack, in every target language,
including languages that do not optimize tail calls themselves. Tail
recursion runs in constant stack space at any depth.

### RECUR-3 A host's own recursion limit is the host's, not morloc's
Intent: ruled 2026-10-05
Code: unaudited

Non-tail recursion in Python or R that reaches that language's own
recursion limit fails as the same recursion written natively would. That
limit is not a morloc defect.

### RECUR-4 Morloc does not spend several native frames per recursion level
Intent: ruled 2026-10-05
Code: deviates #175

One level of morloc recursion costs about one native frame in the language
that runs it, so morloc reaches about the depth the same recursion written
natively reaches.

### RECUR-5 The depth ceiling of non-tail recursion in compiled languages
Intent: open
Code: unaudited

Non-tail recursion in C++ and Rust runs on worker threads whose stacks may
be smaller than a native program's main stack. Open: (a) size worker stacks
to match a native main thread; (b) make the size configurable; (c) leave it.
And at the ceiling: a crash, or a failure catchable by `@try`?

### RECUR-6 Tail calls that cross a language boundary
Intent: open
Code: unaudited

If `isEven` is realized in Python and `isOdd` in C++, each tail call crosses
a pool. Does RECUR-2 hold across the crossing, so depth is unbounded, or is a
cross-language cycle bounded by the depth of nested crossings?
