# Interfaces

One built program, reached through several interfaces. Prefix: IFACE.

The interfaces are the command line (CLI), the daemon's HTTP API, the
daemon's local socket protocol (LP), the MCP server (stdio and HTTP), and
the serving front end (`/call`, `/mcp`, `/eval`, `/discover`), which
forwards to a daemon over LP. A call is a command, its arguments and an
output selection.

## The invariant

### IFACE-1 A call means the same thing on every interface
Intent: ruled 2026-10-12
Code: deviates model/language/issues/interfaces.md

For one call, every interface that accepts it computes the same value,
selects the same output, and delivers the same bytes in that interface's
envelope, or fails with the same class of failure (IFACE-10). An interface
that cannot carry a call refuses it before running anything, and its
discovery says so (IFACE-11). No interface returns a different answer, a
partial answer, or an answer with its output lost.

### IFACE-2 The front end adds no behavior of its own
Intent: proposed
Code: deviates model/language/issues/interfaces.md

A call through the serving front end behaves as the same call made to the
program's daemon directly: same output selection, same failure class.
Routing, authentication, the per-module name prefix and its own limits
(IFACE-12) are its only additions.

## Commands

### IFACE-3 Every interface serves the same commands
Intent: ruled 2026-10-12
Code: deviates model/language/issues/interfaces.md

Every exported command (CMD-1) is reachable on every interface. Two
arguments mean something narrower off the CLI:

- A file path (`IFile`, `IStream`, `OStream`, or a `check.path`
  argument) names a file as the server would open it, relative to the
  server's working directory. No interface transfers a file from the
  client.
- A call off the CLI has no standard input. An argument that may be read
  from stdin (`stdin: true`) is required there, and is given its value
  like any other argument.

## Arguments

### IFACE-4 Off the CLI an argument is its JSON value
Intent: ruled 2026-10-12
Code: conforms 2026-10-12

The CLI carries an argument as an argv token, read by CLI-1 and the
argument's text-form directives (`source:`, `form:`, `@parse`). Every
other interface carries it as JSON, and that JSON is the argument's value
in its wire form. The text-form directives apply only on the CLI.

### IFACE-5 An omitted optional argument takes its default everywhere
Intent: ruled 2026-10-12
Code: deviates model/language/issues/interfaces.md

An argument with a default may be left out on every interface and gets the
value the CLI gives it: its default, read by the same rules (CLI-1, its
text-form directives, CLI-4), on the server. A default naming a file names
the server's file (IFACE-3). A boolean flag left out is false. No
interface requires a caller to fill a slot the CLI lets them omit.
Every interface but the CLI takes the arguments as an object keyed by
argument name (MCP's `arguments`; `args` on HTTP and LP), and HTTP and LP
also as the positional array, one slot per argument; only the object form
can leave an argument out. The names are the keys `--json-help` gives:
a record argument's fields by their own keys, and the whole record by
its key only where the CLI takes it whole. A name given twice, a name no
argument has, or a flag that is not a boolean is a bad request.

## Output selection

### IFACE-6 Every output action is selectable on every interface
Intent: ruled 2026-10-12
Tests: spec-iface-6-1, spec-iface-6-2, spec-iface-6-3, spec-iface-6-4
Code: deviates model/runtime/issues/mcp.md

Every `@with` and `@render` action of a command (CLI-6) can be selected on
every interface, including MCP, by the action's long name, and runs the
same handler on the same output. An action that the CLI runs on saved
output (no fresh-run entry) is no exception. `raw` names the command's own
value; an action cannot be named `raw`.

### IFACE-7 A call that selects nothing gets the CLI's output, except on LP
Intent: ruled 2026-10-12
Code: conforms 2026-10-12

With no selection, CLI-7 cases 3 and 4 apply on every interface but LP:
the `@default` action if there is one, else the command's own value. A
bare LP call is the command's own value.

Why: an LP client is a program, not a person: another morloc front end or
a language binding that decodes the typed value from its schema. A
`@default` action is a presentation for a reader; applying it unasked
would hand such a client bytes its schema does not describe. An LP client
that wants an action names it.

## Output

### IFACE-8 The output reaches the caller
Intent: proposed
Code: deviates model/runtime/issues/mcp.md

Everything a call writes as its output, including a streaming command's
batches and a stream action's output, is delivered to the caller that
made the call, and nothing of it reaches another channel. A streaming
command's result off the CLI is its batches, gathered.

### IFACE-9 A value is the same JSON everywhere
Intent: proposed
Code: deviates model/language/issues/interfaces.md

A typed value delivered as JSON is the JSON the CLI prints with `-f json`,
on every interface. A `@render` action's output is its handler's bytes
(CLI-8) on every interface, with its `@mime` type or, without one,
`text/plain; charset=utf-8` for `Str` and `application/octet-stream` for
bytes. A value whose type carries `@mime` is its bytes (IFACE-19). Whether JSON floats must round trip, and how a non-finite
float is written, is open (WIRE).

### IFACE-10 One failure, one class
Intent: ruled 2026-10-12
Code: deviates model/language/issues/interfaces.md

Every failure falls in one class, and each interface reports it. An HTTP
or socket error body names the class (`bad-request`, `not-found`,
`timeout`, `recovering`, `failure`, `panic`, and `unauthorized` or
`forbidden` for NET-1), so classes that share a status stay apart.

| Class | CLI exit | HTTP | MCP |
|---|---|---|---|
| unknown command | 2 | 404 | invalid params |
| unknown action, bad argument | 2, 1 | 400 | invalid params |
| an action this interface cannot run (IFACE-1) | - | 400 | invalid params |
| a limit (IFACE-12) | - | 413 request, 500 reply | invalid request, `isError` |
| the call fails | 1 | 500 | `isError` |
| recovering from a pool's death | - | 503, `Retry-After` | `isError` |
| a panic (PANIC, DAEMON-7) | 70 | 500, then the daemon ends | internal error |

The call fails when it raises (`@throw`, FAIL), when a pool dies under it,
when it meets a runtime error, or when its top-level value is `Err`: on
every interface, as on the CLI, except to an LP packet client, which
decodes the value itself. The class survives forwarding.

Why: 4xx says the request is at fault. A failing call may be the server's
fault (a missing file, a bug in sourced code), so it is 500, not 422.

## Discovery

### IFACE-11 Every interface describes the same program
Intent: proposed
Code: deviates model/language/issues/interfaces.md

`--json-help`, `/discover` (daemon and front end) and MCP `tools/list` are
views of one description: the same commands, argument names, optionality,
defaults, types (the general, not the concrete, schemas), descriptions and
actions with their kinds and media types. A command or action an interface
refuses (IFACE-3) is listed by that interface with the reason.

## Limits

### IFACE-12 Interfaces may fail differently, never pass differently
Intent: ruled 2026-10-12
Code: deviates model/language/issues/interfaces.md

An interface may refuse a call for a reason of its own: request or reply
size, connection count, what its transport or its host can carry. Such a
refusal is loud: it names the limit, is the limit class (IFACE-10), and
comes before the work or before the reply is built, never as a truncated
or altered answer. A call no interface refuses behaves the same on all of
them (IFACE-1). No interface bounds how long a call may run (DAEMON-6).

### IFACE-13 Eval, typecheck and bind are gated alike
Intent: ruled 2026-10-12
Code: deviates model/language/issues/interfaces.md

Eval, typecheck and bind are off unless the server is started with
`--eval`, on the daemon as on the front end (not-found otherwise). Then the
front end's credential rule applies: over HTTP they need the bearer token
when one is set, and are refused (forbidden) when none is set unless
`--eval-allow-no-auth` or `MORLOC_EVAL_ALLOW_NO_AUTH` waives it; raw TCP,
which carries no credential, is refused unless waived; a local socket,
which only its owner can reach (NET-4), needs only `--eval`. Every server
that offers them returns the same result shape.

## MCP

### IFACE-14 One MCP tool per command
Intent: ruled 2026-10-12
Code: conforms 2026-10-12

A command is one tool. Its output selection is an optional string
argument `_render` whose values are `raw` and every action (IFACE-6), with
the IFACE-7 selection as its default; any other value, a non-string or
`null` is an invalid-params error. A tool with actions declares no
`outputSchema`; its description lists the actions as CLI-11 does. An
object result carries `structuredContent`.

Why: one tool per action breaks dotted-name clients and multiplies the
tool list; the other interfaces have one entry per command. The
selector shares one namespace with the command's arguments, and no
argument name can begin with `_`.

### IFACE-15 MCP content follows the output's type
Intent: ruled 2026-10-12
Code: conforms 2026-10-12

`image/png`, `image/jpeg`, `image/gif` and `image/webp` are an image
block, `audio/*` an audio block (media type compared without parameters,
lower case). Any other output is an embedded resource with its media
type: `text` for a `Str` result that is valid UTF-8, `blob` otherwise.
Each resource has a URI unique to the call. The server declares no
`resources` capability: a result's resource is never listed or read back.

## Proposed additions

### IFACE-16 `@stdin` off the CLI
Intent: open
Code: unaudited

A command whose body opens `@stdin` reads the standard input of the
process serving it. Off the CLI no call has a standard input of its own,
so the read takes whatever the server's stdin holds, or waits for input
that never comes. This is a request the server cannot meet, not a fault
of the call. Open: whether such a command is refused off the CLI, or its
`@stdin` is at end of input there; today a served process's fd 0 is
`/dev/null` (DAEMON-15), so it reads end of input.

### IFACE-17 An argument's checks apply on every interface
Intent: proposed
Code: deviates model/language/issues/interfaces.md

A check on an argument (`check.path`) is applied to the argument's value
on every interface, before the call runs, and a value that fails it is a
bad argument (IFACE-10).

### IFACE-18 A liveness probe needs no credential and reveals nothing
Intent: ruled 2026-10-12
Code: conforms 2026-10-12

`GET` or `HEAD /health` is answered on every HTTP server without a
credential, with only whether the server answers. Anything more (pool
liveness, versions, load) is answered only to a request that carries the
credential NET-1 requires.

Why: orchestrators and load balancers probe health without the
application's token; a probe that reveals nothing costs nothing to open.

### IFACE-19 A `@mime` value is its bytes on every interface
Intent: ruled 2026-10-12
Tests: spec-iface-19-1
Code: conforms 2026-10-12

A value whose type carries `@mime` is delivered as its bytes with that
media type on every interface, the CLI included: a `@mime` `Str` is its
text, unquoted, and a `@mime` byte vector its bytes. This holds for a
command's own value and for an action's output, on stdout or in a file.
On the CLI, `-f` or `-p` names another form instead.

