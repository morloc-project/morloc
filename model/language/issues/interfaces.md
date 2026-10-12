# Interfaces

Holes against model/language/interfaces.md. Paths are under data/rust/.
MCP-specific entries are in model/runtime/issues/mcp.md. Entries marked
[read, survey] come from one read-only pass over the code and have not been
re-checked.

## Rulings needed

- **IFACE-5 says an omitted flag is false; the CLI gives its default.**
  A flag may default to true (library/Morloc/CodeGenerator/Docstrings.hs
  ~l.1066; morloc-nexus/src/phase2.rs ~l.1271-1280 reads `"true"`/`"1"`).
  IFACE-1 (ruled) asks for the CLI's value, IFACE-5 (ruled) for false.
  Proposed amendment: "A boolean flag left out takes its declared default,
  false unless declared." The resolver follows IFACE-1 until ruled.
- **IFACE-17 (`check.path` on every interface) is proposed.** The resolver
  does not apply argument checks off the CLI until it is ruled. Open with
  it: a failed check is bad-request; a supplied `IFile` that does not exist
  is bad-request; a default file that does not exist is a call failure.

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
- **`@stdin` off the CLI reads end of input.** [reproduced, 0.109.0;
  read since] Gap at IFACE-16; not blocking (user, 2026-10-12). A served
  process's fd 0 is now `/dev/null` (DAEMON-15), so a call that opens
  `@stdin` reads nothing rather than the server's own stdin or a hang;
  whether such a call should be refused instead is IFACE-16's question.
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
- **An `IFile` given as `"-"` skips the existence check.** [read]
  morloc-nexus/src/dispatch.rs `check_ifile_arg` runs only for
  `ArgValue::Value`; a test asserts a JSON `"-"` passes (~l.2352).
- **Internal argument errors are classed bad-request.** [read]
  daemon_ffi.rs ~l.2476 and ~l.2626: SHM exhaustion and other internal
  failures while building arguments answer 400 (IFACE-10).
- **`--json-help` marks every argument group required.** [read, survey]
  json_help.rs:550; MCP marks a whole-object group optional, and an
  unrolled field with a default optional (json_help.rs:1073, 1109).

- **The C manifest drops an unrolled `data` argument's default.** [read]
  morloc-runtime/src/manifest_ffi.rs `populate_arg` (Arg::Alt branch,
  ~l.1671-1687) copies neither `default_val` nor `required`, so the daemon
  cannot default an unrolled `data` argument (IFACE-5).
- **MCP's keyed-group fallback to field keys cannot run.** [read]
  morloc-nexus/src/mcp.rs `resolve_slot` reads flattened field keys for a
  keyed group (~l.589), but `resolve_arguments` rejects those keys as
  unknown first (~l.538-541).
- **MCP lists a whole-object group as optional when a field is required.**
  [read] json_help.rs ~l.1028-1029: `tools/list` says the group may be
  omitted, and the call then fails "Required field ... missing".
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
