# Effect boundaries

Suspensions made explicit wherever a value meets a calling convention.
Prefix: EBND.

### EBND-1 Every boundary agrees with its declared type
Intent: proposed
Code: unaudited

After boundary insertion, at every point where a value's declared type and
the convention of the code receiving it differ by a suspension layer, an
explicit run or an explicit suspension stands between them. A checker
verifies this on every tree before lowering; a failure is an internal error.

### EBND-2 A root manifold takes exactly its command's inputs
Intent: proposed
Code: unaudited

The manifold for an exported function takes one argument per input in its
type, since the nexus sends a command's arguments by type (CMD). Any other
count is an internal error.

### EBND-3 No pass after boundary insertion adds or removes a run
Intent: proposed
Code: unaudited

Later passes may move and rename code, but the number of runs of each
suspension on every path is fixed once boundaries are inserted (EFF-3).
