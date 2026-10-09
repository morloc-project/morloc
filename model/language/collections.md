# Collections

Lists, tuples and maps: construction, access, slicing. Prefix: COLL.

### COLL-1 A list is homogeneous and of any length
Intent: proposed
Code: unaudited

`[T]` is the same type as `List T`. Every element of a `[T]` has type `T`.

### COLL-2 A tuple of n types is `TupleN` of those types, for any n
Intent: proposed
Code: unaudited

`(T1, ..., Tn) == TupleN T1 ... Tn` for n >= 2, and there is no upper bound
on n. Tuple types and tuple values share the same parenthesized syntax.

### COLL-3 Indexing and slicing follow Python's semantics
Intent: proposed
Code: unaudited

`.[i]` picks an element, `.[i:j]` a sub-range and `.[i:j:k]` a strided
sub-range. A negative index counts from the end, out-of-range slice bounds
are clamped, and a slice of a `[T]` is a `[T]`. The result is the same in
every language.

### COLL-4 An index out of range
Intent: open
Code: unaudited

What does `.[i] xs` mean when `i` is outside `xs`? Candidates:

- (a) a recoverable failure that `@try` catches (see failure.md);
- (b) a `Try` or `?T` result, which changes the type of `.[i]`;
- (c) whatever the realizing language does, which breaks COLL-3's promise
  that the result does not depend on the language.

### COLL-5 A map with a repeated key
Intent: open
Code: unaudited

`Map k v` is a distinct type over a list of key-value pairs. When a value
arriving as pairs repeats a key, is the result (a) the first pair,
(b) the last pair, or (c) a failure? The answer must not depend on the
language that builds the map.
