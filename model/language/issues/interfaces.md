# Interfaces

Holes against model/language/interfaces.md. Paths are under data/rust/.
MCP-specific entries are in model/runtime/issues/mcp.md. Entries marked
[read, survey] come from one read-only pass over the code and have not been
re-checked.

## Rulings needed

- **IFACE-17 (`check.path` on every interface) is proposed.** The resolver
  does not apply argument checks off the CLI until it is ruled. Open with
  it: a failed check is bad-request; a supplied `IFile` that does not exist
  is bad-request; a default file that does not exist is a call failure.

## Commands (IFACE-3, IFACE-16)

- **The working directory a relative path resolves against is unstated
  for a router-started daemon.** [speculative] IFACE-3 resolves against
  the server's working directory; what that is for a daemon the router
  starts (DAEMON-12) is not specified.
- **`@stdin` off the CLI reads end of input.** [reproduced, 0.109.0;
  read since] Gap at IFACE-16; not blocking (user, 2026-10-12). A served
  process's fd 0 is now `/dev/null` (DAEMON-15), so a call that opens
  `@stdin` reads nothing rather than the server's own stdin or a hang;
  whether such a call should be refused instead is IFACE-16's question.
- **A remote caller names any server path to read or write.** [read]
  Gap in model/runtime/network.md: IFACE-3 rules that a file path names
  the server's file, and MCP now serves `IFile`, `IStream` and `OStream`
  arguments (morloc-nexus/src/json_help.rs `command_to_tool_shape`), so an
  `OStream` argument lets a caller write wherever the server can. NET-1's
  credential is the only gate; nothing states what it grants or whether a
  server may confine paths.
- **`daemon -h` and `mcp -h` list only the MCP-servable commands.**
  [read] morloc-nexus/src/serve_help.rs:17,34 lists `servable_commands`,
  which leaves out a command returning a file handle; the daemon serves
  it.

## Arguments (IFACE-4, IFACE-17)

- **The daemon skips `check.path`.** [read] Violates IFACE-17. A daemon
  argument is read by `call_arg_packet` (morloc-runtime/src/cli.rs) with no
  checks; only the CLI applies them (morloc-nexus/src/dispatch.rs
  `preprocess_cli_value`).
- **The socket protocol is unspecified.** Gap in model/runtime/network.md:
  NET-4..NET-7 cover the transport, but nothing states the LP request and
  response (methods, fields, `media`, packet mode, error kinds).
- **An `IFile` given as `"-"` skips the existence check.** [read]
  morloc-nexus/src/dispatch.rs `check_ifile_arg` runs only for
  `ArgValue::Value`; a test asserts a JSON `"-"` passes (~l.2352).
- **Internal argument errors are classed bad-request.** [read]
  morloc-runtime/src/daemon_ffi.rs: any `call_arg_packet` or
  `make_call_packet` error, SHM exhaustion included, answers 400
  (IFACE-10).

## Output (IFACE-9)

- **Non-finite floats.** [read, survey] Lists and records write the
  strings `"nan"`/`"inf"` (morloc-runtime/src/json.rs:1818-1822) where the
  advertised schema says `number`; a Table writes bare `nan`
  (arrow_ffi.rs:349-350), invalid JSON, which HTTP then wraps as one JSON
  string (daemon_ffi.rs:1292-1295) and MCP sends as invalid text. Floats
  are written `%.15g` (json.rs:1522), so no JSON interface round trips.

## Failure (IFACE-10)

- **A wrong method on a daemon path is 404, not 405.** [read, survey]
  morloc-runtime/src/http_ffi.rs `http_to_daemon_request` answers any
  unmatched method and path "Unknown HTTP endpoint"; the MCP server and the
  front end answer 405 with `Allow`.
- **A call ended by another call's pool recovery is classed internal.**
  [read] When a pool crash starts recovery, the pools are killed under the
  calls still running on them, which fail as internal errors ("pool
  died"); only a shutdown remaps them to recovering
  (morloc-runtime/src/daemon_ffi.rs `dispatch`, `POOLS_STOPPED`). The
  caller cannot tell its call is retryable. Daemon and MCP alike. A call
  admitted while another call is checking for a dead pool, before that
  check closes the request gate, fails the same way.

## Discovery (IFACE-11)

- **Daemon `/discover` and front-end `/discover/<module>` differ.**
  [read, survey] The front end serves `--json-help` (mcp.rs:1891). The
  daemon (morloc-runtime/src/manifest_ffi.rs:2173-2388) publishes the
  concrete, language-hinted schemas that json_help.rs:609-613 says must
  not be published, only the first description line, CLI-syntax defaults,
  no argument keys, no streaming flag, no groups, no default or
  required-ness for an unrolled `data` argument (`populate_arg` copies
  neither), and `renders` without the `@render`/`@with` distinction.
  `--json-help` gives every action `mime: null`. The front end serves
  `--json-help` as its `/discover/<module>`, whose `required` is the CLI's:
  a `stdin: true` positional shows `required: false` although `/call`
  requires it (IFACE-3; morloc-manifest `call_args::required_off_cli`).
  It also names every record argument by its key, though `/call` takes
  that key only for a record the CLI takes whole (`group_option` set).

## Limits (IFACE-12)

- **Body caps disagree.** [read, survey] Daemon HTTP 4 MiB
  (http_ffi.rs:10), LP 64 MiB (daemon_ffi.rs:19), MCP and front end 64 MiB.
  A front-end body near 64 MiB plus the envelope exceeds the child's LP
  cap and the child closes without replying: a generic 500. The LP
  writer's 32-bit length wraps at 4 GiB (daemon_ffi.rs:2744-2749); the
  router allocates whatever length the child declares (router_ffi.rs:720).
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
- **The front end's tool list goes stale against a rebuilt manifest.**
  [read] `tools/list` comes from shapes cached at startup (`mcp_by_name`);
  a child started later reads the current manifest. A call names its
  arguments, so it is refused (unknown or missing argument) rather than
  misbound, but the advertised tools are wrong until the front end
  restarts. ART-5 compares only the morloc version string.
## Eval (IFACE-13)

- **The manual says loopback eval needs no token; the code locks it.**
  [read] docs src/content/deploy-eval.asc ("On the default loopback bind,
  eval needs no token") against the front end's `eval_needs_token`
  (morloc-nexus/src/mcp.rs, no bind check) and the daemon's
  `eval_refusal`: eval with no token set is refused unless waived, whatever
  the bind. One side is wrong; the code follows the front end.
- **Eval request bodies are read differently.** [read] The front end's
  `/eval` (morloc-nexus/src/mcp.rs `frontend_eval_api`) reads the body with
  `from_utf8_lossy` and accepts `expr`, `expression` or a bare string; the
  daemon's (morloc-runtime/src/http_ffi.rs) accepts only `expr` and refuses
  a body that is not UTF-8.
- **Eval results have two shapes.** [read, survey] Daemon `/eval` returns
  parsed JSON (`"result":2`, daemon_ffi.rs:1606-1614); front-end `/eval`
  a string (`"result":"2"`, mcp.rs:2325). The front end's MCP `eval`
  success lacks `isError: false`.
