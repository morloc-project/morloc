# Interfaces

A module's functions, reached through several interfaces. Prefix: IFACE.

The interfaces are import (another morloc program calls the functions in
its own code), the command line (CLI), the daemon's HTTP API, the daemon's
local socket protocol (LP), the MCP server (stdio and HTTP), and the
serving front end (`/call`, `/mcp`, `/eval`, `/discover`), which forwards
to a daemon over LP. A call is a command, its arguments and an output
selection.

Import calls a function directly, as one morloc function calls another
(IFACE-24). Every other interface
calls the command (CMD-1) built from the function and its docstrings: these
are the command interfaces. The command interfaces other than the CLI (HTTP,
LP, MCP and the front end) are the served interfaces.

## The invariant

### IFACE-1 Every interface calls the same function
Intent: proposed
Code: deviates model/language/issues/interfaces.md

Each interface calls the module's function with argument values. Given the
same values, every interface makes the same call.

Everything around the call belongs to the interface and may differ: how
arguments are read (text, files, JSON), which defaults fill missing
arguments, which checks run on them, which errors can occur, and how the
result is rendered (`@render`, `@with`). Each of these is specified by its
own item.

### IFACE-24 Docstring directives do not change the function
Intent: proposed
Code: conforms 2026-11-01

A function's docstring directives (`@arg`, `@default`, `@check.path`,
`@render`, `@with`, `@parse`, `@mime` and the rest) describe its command
and nothing else. They do not change which implementation is chosen, how
the function is placed, or what it computes. A program that imports the
function calls it with no directive applied.

### IFACE-2 The front end passes calls through to a daemon
Intent: proposed
Code: deviates model/language/issues/interfaces.md

The serving front end sends each call to a daemon of the program and
answers as that daemon answers. It adds only routing, authentication, the
per-module name prefix and its own limits (IFACE-12). It changes nothing
else in the request or the response.

### IFACE-20 A built program calls its own build
Intent: proposed
Code: deviates model/language/issues/interfaces.md

Every interface of a built program calls the functions of that build. A
daemon started on the program's behalf, by the front end's router or
otherwise, is the same build, or it does not start.

### IFACE-21 A function cannot tell which interface called it
Intent: proposed
Code: deviates model/runtime/issues/mcp.md

Given the same argument values and the same state of the world (files,
clock, working directory, environment), a function behaves the same
whichever interface called it. It never reads the server's standard input
or writes the server's standard output, and it is never stopped partway
because of the interface that called it.

### IFACE-22 A request calls exactly what it names, or is refused
Intent: proposed
Code: deviates model/runtime/issues/mcp.md

Each command interface reads a request by its own rules: CLI-1 and the text-form
directives on the CLI, IFACE-4 and IFACE-5 elsewhere. The request calls
the command, action and arguments it names. A name that matches nothing is
refused; it is never matched by prefix or corrected. Reading a request may
read files, such as a CSV or parquet argument on the CLI, but writes
nothing. A refused request calls nothing.

### IFACE-23 The response holds the whole result
Intent: proposed
Code: deviates model/language/issues/interfaces.md

From a command interface's response the caller can recover the whole
result, which rendering produced it, and, for a failure, its class
(IFACE-10). An interface may
refuse to send a result it cannot carry (IFACE-12), but it never cuts,
changes or drops part of it.

## Commands

### IFACE-3 Every command interface serves the same commands
Intent: ruled 2026-10-12
Code: deviates model/language/issues/interfaces.md

Every exported command (CMD-1) is reachable on every command interface.
Two arguments mean something narrower on a served interface:

- A file path (`IFile`, `IStream`, `OStream`, or a `check.path`
  argument) names a file as the server would open it, relative to the
  server's working directory. No interface transfers a file from the
  client.
- A call on a served interface has no standard input. An argument that
  may be read from stdin (`stdin: true`) is required there, and is given
  its value
  like any other argument.

## Arguments

### IFACE-4 On a served interface an argument is its JSON value
Intent: ruled 2026-10-12
Code: conforms 2026-10-12

The CLI carries an argument as an argv token, read by CLI-1 and the
argument's text-form directives (`source:`, `form:`, `@parse`). Every
served interface carries it as JSON, and that JSON is the argument's value
in its wire form. The text-form directives apply only on the CLI.

### IFACE-5 An omitted optional argument takes its default on every command interface
Intent: ruled 2026-10-13
Code: conforms 2026-10-13

An argument with a default may be left out on every command interface
and gets the value the CLI gives it: its default, read by the same rules (CLI-1, its
text-form directives, CLI-4), on the server. A default naming a file names
the server's file (IFACE-3). A boolean flag left out takes its declared
default, false unless declared. No
command interface requires a caller to fill a slot the CLI lets them omit.
Every served interface takes the arguments as an object keyed by
argument name (MCP's `arguments`; `args` on HTTP and LP), and HTTP and LP
also as the positional array, one slot per argument; only the object form
can leave an argument out. The names are the keys `--json-help` gives:
a record argument's fields by their own keys, and the whole record by
its key only where the CLI takes it whole. A name given twice, a name no
argument has, or a flag that is not a boolean is a bad request.

## Output selection

### IFACE-6 Every output action is selectable on every command interface
Intent: ruled 2026-10-12
Tests: spec-iface-6-1, spec-iface-6-2, spec-iface-6-3, spec-iface-6-4
Code: deviates model/runtime/issues/mcp.md

Every `@with` and `@render` action of a command (CLI-6) can be selected on
every command interface, including MCP, by the action's long name, and runs the
same handler on the same output. An action that the CLI runs on saved
output (no fresh-run entry) is no exception. `raw` names the command's own
value; an action cannot be named `raw`.

### IFACE-7 A call that selects nothing gets the CLI's output, except on LP
Intent: ruled 2026-10-12
Code: conforms 2026-10-12

With no selection, CLI-7 cases 3 and 4 apply on every command interface but LP:
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
command's result on a served interface is its batches, gathered.

### IFACE-9 A value is the same JSON on every command interface
Intent: proposed
Code: deviates model/language/issues/interfaces.md

A typed value delivered as JSON is the JSON the CLI prints with `-f json`,
on every command interface. A `@render` action's output is its handler's
bytes (CLI-8) on every command interface, with its `@mime` type or, without one,
`text/plain; charset=utf-8` for `Str` and `application/octet-stream` for
bytes. A value whose type carries `@mime` is its bytes (IFACE-19). A finite float is written as WIRE-9 says; how a non-finite
float is written is open.

### IFACE-10 One failure, one class
Intent: ruled 2026-10-12
Code: deviates model/language/issues/interfaces.md

Every failure falls in one class, and each command interface reports it. An HTTP
or socket error body names the class (`bad-request`, `not-found`,
`timeout`, `recovering`, `failure`, `panic`, `limit` for a request and
`reply-limit` for a reply (NET-10), and `unauthorized` or `forbidden` for
NET-1), so classes that share a status stay apart.

| Class | CLI exit | HTTP | MCP |
|---|---|---|---|
| unknown command | 2 | 404 | invalid params |
| unknown action, bad argument | 2, 1 | 400 | invalid params |
| an action this interface cannot run (IFACE-12) | - | 400 | invalid params |
| a limit (IFACE-12) | - | 413 request, 500 reply | invalid request, `isError` |
| the call fails | 1 | 500 | `isError` |
| recovering from a pool's death | - | 503, `Retry-After` | `isError` |
| a panic (PANIC, DAEMON-7) | 70 | 500, then the daemon ends | internal error |

The call fails when it raises (`@throw`, FAIL), when a pool dies under it,
when it meets a runtime error, or when its top-level value is `Err`: on
every command interface, as on the CLI, except to an LP packet client, which
decodes the value itself. The class survives forwarding.

Why: 4xx says the request is at fault. A failing call may be the server's
fault (a missing file, a bug in sourced code), so it is 500, not 422.

## Discovery

### IFACE-11 Every command interface describes the same program
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

### IFACE-16 `@stdin` on a served interface
Intent: open
Code: unaudited

A command whose body opens `@stdin` reads the standard input of the
process serving it. On a served interface no call has a standard input
of its own, so the read takes whatever the server's stdin holds, or waits for input
that never comes. This is a request the server cannot meet, not a fault
of the call. Open: whether such a command is refused on a served
interface, or its `@stdin` is at end of input there; today a served process's fd 0 is
`/dev/null` (DAEMON-15), so it reads end of input. IFACE-21 rules out
reading the server's standard input, which leaves refusing the call before
it runs.

### IFACE-17 A command's argument checks run on every command interface
Intent: proposed
Code: deviates model/language/issues/interfaces.md

A check on a command's argument (`check.path`, and that an `IFile` exists)
runs before the function is called, on every command interface. A value
that fails is a bad argument (IFACE-10). Import runs no checks. `-` meaning
standard input is CLI syntax; on a served interface `"-"` is an ordinary
path.

### IFACE-18 A liveness probe needs no credential and reveals nothing
Intent: ruled 2026-10-12
Code: conforms 2026-10-12

`GET` or `HEAD /health` is answered on every HTTP server without a
credential, with only whether the server answers. Anything more (pool
liveness, versions, load) is answered only to a request that carries the
credential NET-1 requires.

Why: orchestrators and load balancers probe health without the
application's token; a probe that reveals nothing costs nothing to open.

### IFACE-19 A `@mime` value is its bytes on every command interface
Intent: ruled 2026-10-12
Tests: spec-iface-19-1
Code: conforms 2026-10-12

A value whose type carries `@mime` is delivered as its bytes with that
media type on every command interface, the CLI included: a `@mime` `Str` is its
text, unquoted, and a `@mime` byte vector its bytes. This holds for a
command's own value and for an action's output, on stdout or in a file.
On the CLI, `-f` or `-p` names another form instead.

