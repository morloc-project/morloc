# Streams

Stream types, file-backed streams, `@collect`, `@fold`. Prefix: STRM.

## File streams

### STRM-1 A stream's element type is fixed at open and checked against the file
Intent: proposed
Code: unaudited

`@open`, `@append`, `@stdin`, `@stdout` and `@stderr` take their element type
from an inline ascription. Opening a file whose recorded schema differs is an
`Err` at open time, before any element is read or written.

### STRM-2 A closed `OStream` reads back as the elements written, in order
Intent: proposed
Code: unaudited

Compression level, `@flush` and buffer size change sub-packet boundaries,
never the elements. An `IStream` yields them in order, one batch per `@next`,
and `Ok []` at the end.

### STRM-3 The type parameter of `IFile` versus `IStream` and `OStream`
Intent: open
Code: unaudited

`IStream a` and `OStream a` are parameterized by the element type, but
`IFile` is written `IFile [a]` (`@stream :: IFile [a] -> <IO> (IStream a)`).
Either (a) `IFile` takes the element type like the other two; or (b) the
difference is intended and the rule states what `IFile a` with non-list `a`
means.

## Collect

### STRM-4 A `@collect` command's output is every batch its sink receives, in order
Intent: proposed
Code: unaudited

The producer is handed a sink and calls it once per batch. An action applies
to everything the command streams, whatever the shape of its body: several
`@collect`s, or one in a branch, give the action the whole output in order.

### STRM-5 What `@fold` promises when `@init` and `@combine` break their laws
Intent: open
Code: unaudited

`@init` must be an identity for `@combine`, and `@combine` associative, and
commutative if the producer is threaded. Nothing checks this, so the answer
can vary with thread count. Either (a) the result is unspecified, the user's
obligation; or (b) accumulators merge in a fixed order so only identity and
associativity matter; or (c) a fold over a threaded producer is opt-in.
