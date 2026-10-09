# Data crossing

How each type's values cross between languages: wire form, round trip, copying and sharing. Prefix: WIRE.

### WIRE-1 A value crosses between languages unchanged
Intent: proposed
Code: unaudited

A value of a serializable type that crosses from one language to another
arrives equal to the value sent, and crossing back returns the original. The
transport (inline, shared memory or temp file) and its thresholds never
change the value delivered.

### WIRE-2 `Int` arrives as the receiving language's default integer
Intent: proposed
Code: unaudited

`[[Int]]_L` is NUM-1's. A program that needs one width everywhere uses a
fixed-width type.

### WIRE-3 An `Int` that does not fit the receiving language fails the crossing
Intent: proposed
Code: unaudited

The crossing fails, naming the value and the receiving language; it never
wraps or truncates (NUM-4).

### WIRE-4 A Python `Vector n Int` is a numpy array of dtype object
Intent: ruled 2026-10-07
Code: unaudited

`Int` is unbounded in Python, so its vectors keep Python integers. A vector
of a fixed-width element type gets the matching native dtype: `I64` is
`int64`.

### WIRE-5 A suspension crosses as a callable of no arguments, never as its result
Intent: ruled 2026-09-12
Code: unaudited

`<E> T` crossing between languages is a closure that runs its computation in
its home language each time it is forced. It is never run by the crossing and
never serialized under `T`'s schema (see `EFF`).

### WIRE-6 A function crosses as a closure over its home language
Intent: proposed
Code: unaudited

A function value passed to another language is called there as a native
callable; each call runs the function in the language where it was built
(see `EFF`).
