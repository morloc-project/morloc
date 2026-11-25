# MCP tool calls

Governed by model/language/interfaces.md (IFACE-6, IFACE-8, IFACE-12,
IFACE-14, IFACE-15). Repro programs: web/morloc-studio/public/assets/
home-examples/smiles (`structure` with `@mime`-typed `@render` actions) and
test-suite/golden/interop/multi-output-stream (streaming commands). Paths are
under data/rust/.

- **A call that streams its output is refused off the CLI.** Violates
  IFACE-6 and IFACE-8 (the output must reach the caller). It is refused
  before it runs: every streaming command returns `Collect` (STRM-7), so
  the manifest shows it (a streaming command's own value, a stream-kind
  action: daemon_ffi.rs `resolve_render_target`). Replay-only actions (no
  `entry`) are refused the same way. Needs per-call stdout capture (plan step 7,
  /work/plans/interface-consistency-plan.md).
- **The MCP server's stdio server runs in one-shot mode.** [read]
  morloc-nexus/src/cli.rs:911 `mcp_args_to_config` leaves `daemon_flag`
  false, so `stdio_server::start` (main.rs) runs with DAEMON_MODE off: a
  broken pipe on a stdio stream exits the server with 141
  (stdio_server.rs:349 `pipe_closed_resp`) instead of failing one call, and
  the recovery gate at stdio_server.rs:316 is skipped.
- **The front end's MCP path checks the line cap only after copying.**
  [read] Violates IFACE-12 (refused before the reply is built): the child
  base64s the result, the front end decodes it into a fresh buffer
  (`Frontend::forward`, `.to_vec()`), and only `media_result` compares it
  with the cap. A request field telling the daemon the caller's cap would
  refuse it first.
- **DAEMON-15 is checked by path, not by file.** [read, 2026-10-21] A
  served path is refused when, with each directory on the way resolved and
  each final link followed, it is a stdio sentinel, `/dev/std*` or a
  descriptor link under `/dev/fd` or any `/proc/<pid>/[task/<tid>/]fd`.
  Still open: a link swapped between the check and the open; the real path
  of a file the server's stdout was redirected to; the check's file system
  calls run while the nexus decodes a request, so a hung mount stalls it.
  A check by identity (fstat of the opened file against every descriptor
  the server holds, with OStream opened without `O_TRUNC` until it
  passes) would close the first two; reads are opened in the pool, so the
  server's identities would have to be shared with pools. Outside a
  server, the race guard on the nexus's own stdio (`reject_dev_stdio_path`,
  stream.rs:2962, and `is_stdin_device`, stream.rs:2883) still matches
  literal paths only.
- **The daemon ignores a query key it does not know.** [read, 2026-11-04]
  morloc-runtime-types http_query.rs `render_selection` keeps only
  `render=` pairs, so `/call/f?rendr=json` or a bare `?render` runs the
  default output instead of being refused (IFACE-22).

The items below moved from mim's issue list (morloc-manager
model/issues/LIST.md), checked against the code 2026-11-04 [read].

- **An IPv6 loopback host can never bind.** morloc-nexus/src/mcp.rs:1336
  (mcp HTTP) and :2116 (front end) build the address as
  `format!("{}:{}", bind_host, port)`, giving `::1:9000`; IPv6 needs
  brackets.
- **`MORLOC_EVAL_ALLOW_NO_AUTH=0` waives the eval lock.** cli.rs:870
  treats any non-empty value as true.
- **`/call` reaches commands MCP excludes.** The front end's `/call`
  (`frontend_call_api`, mcp.rs:2717-2745) checks only `api_modules`
  (:2730); the stream-argument and `@stdin` exclusions of
  `build_tool_shapes` (json_help.rs:777) are not applied.
- **401 bodies and OPTIONS differ between servers.** The daemon answers
  `{"status":"error","class":"unauthorized","error":"unauthorized"}`
  (daemon_ffi.rs:3204); the front end and single-program mcp answer
  `{"error":"unauthorized"}` (mcp.rs:2233, :1561). Single-program mcp
  (`build_response`, mcp.rs:1549) has no OPTIONS handling.
- **MCP `notifications/cancelled` is ignored.** mcp.rs:506 drops unknown
  notifications, so a cancelled call runs on. A crashed child daemon stays
  down until the next forward restarts it (`Frontend::forward`,
  mcp.rs:1887).
- **`GET /eval` with eval off is 405, not 404.** The method check
  (mcp.rs:2252-2255) runs before `frontend_eval_api`'s not-exposed 404
  (mcp.rs:2508). Compare the daemon's opposite case under IFACE-10.
- **A `__` collision in module or command names aborts the front end.**
  mcp.rs:2009-2029 exits 1 on purpose (fail closed). Needs a ruling: refuse
  at registration, or abort the whole front end as now.
- **Possible enhancement, not a defect: an HTTP call has a fixed cost
  that connection reuse could cut.** [measured, 2026-11-13] A scalar
  result took 0.60 ms over HTTP and an 8 KB result 0.68 ms (setup not
  recorded); a raw-socket client calling the daemon directly measured
  0.13-0.16 ms (TCP connect+accept+close alone 61 us), so the front end's
  router_forward (router_ffi.rs:628-760: a new unix connection per
  forward, JSON re-serialized, two writes) is the suspect. Candidates:
  daemon keep-alive (pools-and-ipc.md), and a router connection per
  child daemon. Both need rules first: an idle keep-alive connection must
  not hold one of the DAEMON-16 call slots, and a retry on a stale pooled
  connection must never run an effectful call twice. Deferred (user,
  2026-11-14).
