# Interfaces

Holes against model/language/interfaces.md. Paths are under data/rust/.
MCP-specific entries are in model/runtime/issues/mcp.md. Entries marked
[read, survey] come from one read-only pass over the code and have not been
re-checked.

## Commands (IFACE-3, IFACE-16)

- **MCP leaves out file-path and `stdin:` commands.** [read] Violates
  IFACE-3. morloc-nexus/src/json_help.rs:824-833 drops any command with an
  `IFile`/`IStream`/`OStream` in its schemas, and json_help.rs:978 any
  command with a `stdin: true` positional. HTTP, LP and front-end `/call`
  serve both, opening a path on the server (morloc-runtime/src/json.rs:
  696-705), which IFACE-3 now rules correct. A remote caller can name any
  file the server can read; NET-1's credential is the only gate.
- **The working directory a relative path resolves against is unstated
  for a router-started daemon.** [speculative] IFACE-3 resolves against
  the server's working directory; what that is for a daemon the router
  starts (DAEMON-12) is not specified.
- **`@stdin` off the CLI reads the server's stdin.** [reproduced, 0.109.0]
  Gap at IFACE-16; not blocking (user, 2026-10-12). rust-stdio golden
  built as `rs`: MCP lists `doubler`, `checkSum`, `producer`; `tools/call
  checkSum` returns `0`, `isError:false`, as `./rs checkSum < /dev/null`
  does, because the MCP stdio server saw end of input. A daemon whose
  stdin is a terminal or an open pipe would wait instead.
- **`daemon -h` and `mcp -h` list only the MCP-servable commands.**
  [read, survey] morloc-nexus/src/serve_help.rs:17,34; the daemon serves
  more.

## Arguments (IFACE-4, IFACE-5, IFACE-17)

- **The daemon skips `check.path`.** [read] Violates IFACE-17. Every
  daemon argument goes through the plain `parse_cli_data_argument`
  (morloc-runtime/src/daemon_ffi.rs:2367); only the CLI applies the
  argument's checks (dispatch.rs:752-771). Skipping `source:`, `form:` and
  `@parse` there conforms to IFACE-4.
- **The socket protocol is unspecified.** Gap in model/runtime/network.md:
  NET-4..NET-7 cover the transport, but nothing states the LP request and
  response (methods, fields, `media`, packet mode, error kinds).
- **The daemon applies no defaults and requires exact arity.**
  [read, survey] daemon_ffi.rs:2240-2295; flags, options and groups each
  take a positional slot. Violates IFACE-5. MCP fills defaults
  (morloc-nexus/src/mcp.rs:534-566). Front-end `/discover` says "POST
  positional args" without saying options and flags take slots
  (mcp.rs:2094).
- **MCP turns CLI default strings into JSON by heuristic.** [read, survey]
  json_help.rs:843-896: `strip_cli_quotes` drops quotes without decoding
  escapes, so a default `"a\tb"` reaches MCP as backslash-t. Depends on
  CLI-4.
- **Request bodies are read differently by the daemon and the front end.**
  [read, survey] `{"args":5}`: daemon treats as no args
  (morloc-runtime/src/http_ffi.rs:551-556), front end forwards the 5
  (mcp.rs:2577). A bare JSON scalar: daemon runs with no args
  (http_ffi.rs:561), front end answers 400 (mcp.rs:2580). A non-UTF-8 body
  is silently empty on the daemon (http_ffi.rs:417).
- **The router drops arguments that are not JSON.** [read]
  `serialize_request_to_json` (morloc-runtime/src/router_ffi.rs:839-842)
  forwards a request with no args instead of rejecting it: a third
  body behavior beside the daemon's and the front end's.
- **`--json-help` marks every argument group required.** [read, survey]
  json_help.rs:550; MCP marks a whole-object group optional, and an
  unrolled field with a default optional (json_help.rs:1073, 1109).

## Output selection (IFACE-7)

- **A bare call fires `@default` on daemon HTTP only.** [read] LP
  (daemon_ffi.rs:2186-2191) and MCP (json_help.rs:1175) return the own
  value by design; front-end `/call` drops `?render=` (see mcp.md). The
  same bare call gives different output from the daemon and from the front
  end.

## Output (IFACE-9)

- **A `@render` result is a JSON string over HTTP.** [read] The
  multi-output-stream golden records `ok 20005: ...` in a JSON envelope
  for `?render=all`; CLI-8 says the handler's bytes. A `[Str]` or
  `[Vector U8]` handler comes back as a JSON array.
- **A `@mime` value is encoded four ways.** [read, survey] CLI: JSON;
  HTTP and front-end `/call`: raw bytes with `Content-Type`; LP: JSON
  unless `media:true` (only the router sends it, router_ffi.rs:863); MCP:
  a content block (mcp.md). See also cli-directives.md.
- **Non-finite floats.** [read, survey] Lists and records write the
  strings `"nan"`/`"inf"` (morloc-runtime/src/json.rs:1818-1822) where the
  advertised schema says `number`; a Table writes bare `nan`
  (arrow_ffi.rs:349-350), invalid JSON, which HTTP then wraps as one JSON
  string (daemon_ffi.rs:1292-1295) and MCP sends as invalid text. Floats
  are written `%.15g` (json.rs:1522), so no JSON interface round trips.

## Failure (IFACE-10)

- **A user failure is classified INTERNAL.** [read, survey] Violates
  IFACE-10. A pool FAIL
  (`@throw`) and an eval error get DAEMON_ERROR_INTERNAL
  (daemon_ffi.rs:2375, 2533): HTTP 500, the panic status. MCP stdio runs
  pool recovery after every `@throw` (mcp.rs:654). The CLI exits 1.
- **LP drops the failure class.** [read, survey] `WireResponse` has no
  kind; `daemon_parse_response` defaults to INTERNAL
  (daemon_ffi.rs:1159-1167). Front-end `/call` answers 500 for unknown
  commands, bad arguments and recovery alike (mcp.rs:2602-2604); the
  daemon gives 404/400/503.
- **A top-level `Err` fails only on the CLI.** [read, survey]
  morloc-nexus/src/dispatch.rs:1567-1601; HTTP and LP return it as `ok`,
  MCP as `isError: false`. Violates IFACE-10.
- **Daemon HTTP edge cases.** [read, survey] No 401 reason text
  (http_ffi.rs:257-268, sent as `401 Unknown`); a wrong method is 404, not
  405 (http_ffi.rs:593-603); CORS `Allow-Methods`
  omits DELETE although `DELETE /bindings` exists (http_ffi.rs:320);
  `Content-Length` is found by substring search over all headers
  (http_ffi.rs:195-197).

## Discovery (IFACE-11)

- **Daemon `/discover` and front-end `/discover/<module>` differ.**
  [read, survey] The front end serves `--json-help` (mcp.rs:1891). The
  daemon (morloc-runtime/src/manifest_ffi.rs:2173-2388) publishes the
  concrete, language-hinted schemas that json_help.rs:609-613 says must
  not be published, only the first description line, CLI-syntax defaults,
  no argument keys, no streaming flag, no groups, and `renders` without the
  `@render`/`@with` distinction. `--json-help` gives every action
  `mime: null`.

## Limits (IFACE-12)

- **Body caps disagree.** [read, survey] Daemon HTTP 4 MiB
  (http_ffi.rs:10), LP 64 MiB (daemon_ffi.rs:19), MCP and front end 64 MiB.
  A front-end body near 64 MiB plus the envelope exceeds the child's LP
  cap and the child closes without replying: a generic 500. The LP
  writer's 32-bit length wraps at 4 GiB (daemon_ffi.rs:2744-2749); the
  router allocates whatever length the child declares (router_ffi.rs:720).
- **MCP over HTTP serializes every method behind one lock.**
  [read, survey] mcp.rs:1323-1390: a long `tools/call` blocks every
  session's `ping`, `tools/list` and `initialize`. The front end holds the
  per-module lock across the whole forward (mcp.rs:1685-1690), so calls to
  one module run one at a time although the daemon has 4-32 workers.
- **Concurrent calls to one streaming command may clash.** [speculative]
  "@stdout already open in this nexus" (morloc-runtime/src/stream.rs:
  2437-2446).

- **An LP reply of 4 GiB or more gets a wrong length.** [read]
  `write_lp_message` (morloc-runtime/src/daemon_ffi.rs:2744-2749) writes
  `len & 0xFFFFFFFF` unchecked, and the router reads whatever length a
  child declares (router_ffi.rs:715-720). Violates IFACE-12 (refuse loudly).

## Front end and its children (IFACE-1, IFACE-2)

- **The router starts a child daemon from whatever `morloc-nexus` it
  finds.** [read] `find_morloc_nexus` (morloc-runtime/src/router_ffi.rs:
  331-383) searches `$MORLOC_NEXUS`, `$MORLOC_HOME/bin`, `$PATH`,
  `~/.local/bin`, never the front end's own binary, and children start
  lazily and after each crash. A child of another build can ignore request
  fields the front end relies on.
- **The front end's argument shapes go stale against a rebuilt
  manifest.** [read] The front end builds positional arrays from shapes
  cached at startup (`mcp_by_name`); a child started later reads the
  current manifest. Reordered arguments of one type bind silently to the
  wrong slots. ART-5 compares only the morloc version string.
- **The daemon request and response C mirrors have no layout test.** [read]
  `DaemonRequest`/`DaemonResponse` (daemon_ffi.rs), `daemon_request_t`/
  `daemon_response_t` (data/morloc/morloc.h) and `CDaemonResponse`
  (morloc-nexus/src/mcp.rs:182) are kept in step by hand; c_abi_layout.rs
  does not cover them.

## Output bytes (IFACE-9)

- **A null or `()` render result differs by interface.** [read] The CLI's
  raw writer prints nothing for a top-level null (morloc-nexus/src/
  dispatch.rs:1924-1928); the daemon's `voidstar_raw_to_bytes` errors.
- **A failed media serialization is classed INTERNAL.** [read]
  daemon_ffi.rs `emit_raw_media` -> `set_packet_error`; MCP stdio then runs
  pool recovery (mcp.rs:668) for a call that failed on its own.

## Eval (IFACE-13)

- **The daemon exposes eval, typecheck and bind unconditionally.**
  [read, survey] http_ffi.rs:435-513, sandboxed but with no opt-in and no
  credential rule; the front end requires `--eval` plus a token or waiver
  (mcp.rs:1626-1642).
- **Eval results have two shapes.** [read, survey] Daemon `/eval` returns
  parsed JSON (`"result":2`, daemon_ffi.rs:1606-1614); front-end `/eval`
  a string (`"result":"2"`, mcp.rs:2325). The front end's MCP `eval`
  success lacks `isError: false`.
