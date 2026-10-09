# Arguments and output

Argument shapes, parsing, stdin, `@parse`, `@render`, output rendering. Prefix: CLI.

## Arguments

### CLI-1 An argument is read by its wire type
Intent: proposed
Code: unaudited

A number, a `Bool` or a `Str` is its argv token, verbatim. Any other type is
inline JSON or a path to a file whose format (JSON, MessagePack, a morloc
packet) is detected from its bytes, not its name. The shape follows the wire
form, not the type's name: a type that packs to `[(a, b)]` reads as one.

### CLI-2 Argument shape is one atom per docstring field
Intent: proposed
Code: unaudited

`source: inline|file`, `form: list|bytes|bytes-only|packet` and `check.path`
each take one value, and the `list.` forms do the same per element. No field
takes an OR chain, and `auto` cannot be written. A combination the wire type
does not allow is a build error at the docstring line that wrote it.

### CLI-3 At most one argument per command reads standard input
Intent: proposed
Code: unaudited

A second `-` in one run is a run-time error. An `@stdin` argument is a `Str`,
the last positional, and at most one per command; anything else is a build
error.

### CLI-4 What `@default` is written in
Intent: open
Code: unaudited

The directive reference says `@default <json>`, but an unrolled `data`
argument takes a bare constructor (`@default dot`). Either (a) a default is
always the JSON of the value; or (b) a default is written as the argv token
the argument would accept.

## Output

### CLI-5 A command writes its result to stdout in the `-f` form
Intent: proposed
Code: unaudited

A `-f` form the result type cannot produce is an error, never an
approximation. In JSON, a top-level `()` or `Null` prints nothing unless
`--keep-null` is given; binary forms always write the value, and a `null`
inside a value is always written.
