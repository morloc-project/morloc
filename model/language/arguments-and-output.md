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

## Output actions

### CLI-6 An output action is a flag that applies a handler to the output
Intent: proposed
Code: unaudited

`--' @with -s/--name=f` and `--' @render -s/--name=f` each add a flag
`-s/--name[=PATH]` to the command. `f :: A -> B` with `A` unifying with what
the command writes to stdout. Given a `PATH`, the action writes its output
there instead of to stdout, and several actions given paths may run in one
run. `@with` keeps `B` typed, so it is written in the `-f` form (CLI-5).

### CLI-7 Which output reaches standard output
Intent: proposed
Code: unaudited

In order, the first that applies:

1. `--no-stdout`: nothing.
2. An action flag given without a path. Two such flags in one run are an
   error.
3. The `@default` action, unless `-f` was given or that action was given a
   path.
4. The command's own value, in the `-f` form.

At most one action per command is `@default`; a second is a build error.

### CLI-8 A `@render` action writes its handler's bytes verbatim
Intent: proposed
Code: deviates model/runtime/issues/cli-directives.md (return type unchecked)

The handler returns `Str`, `[Str]`, `Vector U8` or `[Vector U8]`, and its
bytes are written without quoting or escaping. Any other return type is a
build error at the directive.

### CLI-9 `-f` on a run whose stdout is a `@render` action
Intent: open
Code: unaudited

The code ignores `-f` when an action flag picks a `@render` action, which
CLI-5 forbids. Either (a) `@render` actions are exempt from `-f`; or (b)
`-f` together with a `@render` action flag is an error.

## Help

### CLI-10 An absent optional help field is left empty
Intent: ruled 2026-10-10
Tests: spec-cli-10-1
Code: unaudited

When a value help would show was not declared (a description, for one), its
slot is printed empty. Help never fills it with a different field, such as
a media type or a type name.

### CLI-11 The Return block of a command with output actions
Intent: ruled 2026-10-10
Tests: spec-cli-11-1
Code: conforms 2026-10-10

A command without actions prints `Return: T` and then its return
description lines. A command with actions prints `Return:` and then:

- `type:` what the command writes when no action writes stdout (CLI-7 case
  4); for a streaming command, the batch element type.
- `desc:` the return description, when there is one.
- `actions:` one row per action in declaration order: the flag, the type of
  its output (its media type when it has one), `(raw bytes)` for a
  `@render` action, and `default` for the `@default` action.

### CLI-12 `--json-help` carries every epilogue verbatim
Intent: ruled 2026-10-10
Tests: spec-cli-12-1
Code: conforms 2026-10-10

`program.epilogues` and each command's `epilogues` list that docstring's
`@epilogue` blocks in order, one string per block, its lines joined by
newlines and nothing trimmed. No blocks is `[]`.
