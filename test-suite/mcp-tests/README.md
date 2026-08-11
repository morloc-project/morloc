# Morloc MCP test suite

Tests `morloc-nexus mcp <target>` -- the JSON-RPC-over-stdio server that exposes
a compiled morloc program's exported functions as [MCP](https://modelcontextprotocol.io)
tools. It is the MCP analogue of `../daemon-tests`.

The emphasis is on **how docstring / type features map to the MCP tool surface**:
`tools/list` shaping (forward) and the named-argument -> positional-call
inversion (backward), plus the protocol handshake, error semantics, tool-surface
exclusions, and the fd-discipline guarantee.

## Running

Requires a rebuilt toolchain (the MCP server lives in the nexus, which is
rebuilt by `morloc init -f`):

```sh
morloc init -f            # rebuild libmorloc + the nexus after MCP code changes
./run-tests.sh            # all groups (comprehensive)
./run-tests.sh shape call # only these groups
```

Groups: `shape`, `call`, `error`, `protocol`, `fd`, `exclude`.

### Under `stack test`

A representative subset runs automatically as the **MCP** group of
`morloc-integration-test` (`test-suite/integration/Morloc/Test/McpTests.hs`),
alongside the Daemon group. It drives this same `mcp_client.py` as a
subprocess, so the session/handshake logic has one implementation. `stack
test` requires the rebuilt nexus too (`morloc init -f` first), exactly like the
daemon integration tests. This standalone `run-tests.sh` remains the
exhaustive suite.

## Files

| File | Role |
|------|------|
| `run-tests.sh` | orchestrator + assertions (bash) |
| `mcp_client.py` | stdio JSON-RPC session driver (`list`/`call`/`raw`/`jget`) |
| `shapes.loc` + `shapes.py` | one export per docstring/type -> MCP mapping; drives shape/call/error/fd/protocol |
| `excluded.loc` + `excluded.py` | `@stdin` exclusion (+ a control command); no special deps |
| `excluded_table.loc` + `excluded_table.py` | Arrow `Table` exclusion; needs pyarrow (group SKIPs if it can't build) |

`mcp_client.py` captures the server's stderr to a file and treats **any non-JSON
line on stdout as a hard protocol violation**. Because the server aliases fd 1
onto fd 2 before spawning pools, the protocol stream must be pure JSON; a stray
byte fails the test loudly instead of silently mis-parsing.

## Docstring / type -> MCP mapping matrix

This is the contract the suite pins down. "Property name" is the key under
`inputSchema.properties`; the client passes arguments under these names, and the
server inverts them to the positional array `daemon_dispatch` expects.

### Command level

| morloc source | MCP effect | tested by |
|---|---|---|
| `module (f, g, ...)` export list | which functions become tools | `shape`: count == exports |
| leading `--'` command docstring | tool `description` | `shape`: `addOne` description |
| `--' name: foo` | overrides the tool name | `shape`: `greet` -> `hello` |
| pure vs remote (has a pool) | pure -> in-process `morloc_eval`; remote -> pool round-trip; both are tools | `call`: `pureAnswer` (pure) + others (remote) |
| `--' with: -x/--long=term` | synthesizes an **internal** command that is NOT a tool (only the base command is); the terminal is a CLI-only formatter | covered by the `!internal` filter + the `--mcp-tools` golden; not re-driven here |

### Argument level -- positionals

| morloc source | MCP effect | tested by |
|---|---|---|
| `--' metavar: N` | property **name** = lowercased metavar | `shape`: `addOne` -> `n` |
| (no metavar) | property name = `arg0`, `arg1`, ... | `shape`: `sumMany` -> `arg0` |
| plain typed positional | property is required | `shape`: `addOne.required = [n]` |
| `?T` positional | not required + type unions `"null"` | `shape`: `maybeDouble` |
| `--' many: true` | property is an `array` of the element type | `shape`: `sumMany` |
| `Int`/`Real`/`Str`/`Bool` | `integer`/`number`/`string`/`boolean` | `shape`: scalar types |
| `[a]`, `(a,b)` | `array`; `array` with `prefixItems` (tuple) | `shape` (list); tuples via unit tests |
| a non-Bool arg with `--' arg: --x` + `--' default: v` | a top-level **option**: a non-required property named for the option, default `v` filled by the inverse | `shape`+`call`: `scale`'s `factor` |

### Argument level -- records

Records reach the CLI two ways, and MCP mirrors the author's choice:

| morloc source | MCP effect | tested by |
|---|---|---|
| record with `--' unroll: true`, no record-level `arg:` | **flat**: one property per field, keyed by the **field name**; each field carries its `desc:` + `(default: ...)`; a field is **not required** when it has a default (which unrolled fields always do), so the inverse fills omitted fields | `shape`+`call`: `configFlat` |
| record with `--' arg: --x` | **whole-object**: one property named for the record type (lowercased); the client passes the entire object, or omits it to get the field defaults | `shape`+`call`: `configWhole` |
| field `--' arg:` + `--' default:` (non-Bool) | that field's option + typed default | `call`: `configFlat` defaults |
| field `--' true:`/`--' false:` (Bool) | that field's flag, property type `boolean` | `shape`: `removeCaches` |

Note the deliberate asymmetry: in the **flat** form, omitting a field fills its
default per-field; in the **whole-object** form, defaults apply only when the
*entire* object is omitted (a supplied object is passed through verbatim, so its
inner `required` fields are enforced by the pool, not the nexus).

### Return level

| morloc source | MCP effect | tested by |
|---|---|---|
| record / `Map` return | `structuredContent` (the object) **and** an object `outputSchema` | `shape`+`call`: `mkResult` |
| scalar / list return | a single `text` content block; **no** `outputSchema` (MCP requires object output there) | `shape`: `addOne` has no `outputSchema` |
| `Nil` / `()` return | `text` block, no `outputSchema` | (implicit) |

### Inputs that do NOT affect the MCP schema

These CLI-shape docstrings describe how bytes are *sourced/parsed* on the command
line and have **no** MCP representation (MCP always delivers typed JSON): `--'
source: ...`, `--' form: ...`, `--' check.path: ...`, `--' list.* ...`,
`--' quoted`/literal. They remain visible via `--json-help`.

### Exclusions (dropped from `tools/list`)

A command is excluded when it cannot be served correctly over MCP:

| condition | reason | tested by |
|---|---|---|
| a positional with `--' stdin: true` | fd 0 is the JSON-RPC input stream | `exclude`: `readStdin` |
| any argument or the return is an Arrow `Table` | `Table` is unmarshalable to/from JSON both ways | `exclude`: `readTable`/`sumTable` (needs pyarrow) |
| any argument or the return is a stream handle (`IFile`/`IStream`/`OStream`) | would need per-call stdout capture | unit test (these types do not appear in exported signatures in practice) |
| a property-name collision (two args collapsing to one key) | fail-closed; the whole command is dropped | unit test / guarded by `validateFlagRevCollisions` |

## Protocol edge cases (`protocol` group)

- `initialize` echoes a supported `protocolVersion`; an unsupported request
  version gets the server's own (`2025-06-18`).
- `tools/list` / `tools/call` before initialization -> a JSON-RPC error.
- the `notifications/initialized` notification (no `id`) yields **no** response.
- `ping` is answered even before `initialize`.
- request `id` is echoed verbatim as sent (integer or string).
- unknown method -> `-32601`; unknown tool / bad or missing arguments ->
  `-32602`; execution failures / `@throw` -> a **result** with `isError: true`
  (never a protocol error).
- EOF on stdin ends the session with a clean `exit(0)`.

## fd-discipline (`fd` group)

`noisyAdd`'s pool implementation writes `STRAY-POOL-STDOUT` to stdout. Because
the server re-homes fd 1 before spawning any pool, that write lands on stderr;
the call returns a clean `42` and the client (which rejects non-JSON stdout)
confirms the protocol stream was never touched.

## Not yet covered (future work)

- An end-to-end assertion that `--mcp-tools` (static) and the live server's
  `tools/list` are byte-identical (they share the shaper, so this is an oracle).
- Table-argument marshaling (once `Table` JSON support lands, these move from
  "excluded" to "supported").
- Streaming `@stdout` / `@stdin` tools (need per-call output capture).
