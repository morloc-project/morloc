# MCP tool calls

Governed by model/language/interfaces.md (IFACE-6, IFACE-8, IFACE-12,
IFACE-14, IFACE-15). Repro programs: web/morloc-studio/public/assets/
home-examples/smiles (`structure` with `@mime`-typed `@render` actions) and
test-suite/golden-tests/multi-output-stream (streaming commands). Paths are
under data/rust/.

- **A call that streams its output is refused off the CLI.** Violates
  IFACE-6 and IFACE-8 (the output must reach the caller), and IFACE-21
  where the refusal comes partway through the run; refused before it
  runs when the manifest shows it (a streaming command's own value, a
  stream-kind action: daemon_ffi.rs `resolve_render_target`), and by
  DAEMON-15 at the `@stdout` open otherwise (`<IO> ()` producers such as
  stdio-filter `producer`, value commands that stream along the way such
  as multi-output-value `noisy`). Replay-only actions (no `entry`) are
  refused the same way. Needs per-call stdout capture (plan step 7,
  /work/plans/interface-consistency-plan.md).
- **The MCP server's stdio server runs in one-shot mode.** [read]
  morloc-nexus/src/cli.rs:910 `mcp_args_to_config` leaves `daemon_flag`
  false, so `stdio_server::start` (main.rs) runs with DAEMON_MODE off: a
  broken pipe on a stdio stream exits the server with 141
  (stdio_server.rs `pipe_closed_resp`) instead of failing one call, and
  the recovery gate at stdio_server.rs:311 is skipped.
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
  `is_stdin_device`, handle_scan.rs:260) still matches literal paths only.
- **The daemon ignores a query key it does not know.** [read, 2026-11-04]
  morloc-runtime-types http_query.rs `render_selection` keeps only
  `render=` pairs, so `/call/f?rendr=json` or a bare `?render` runs the
  default output instead of being refused (IFACE-22).

The items below moved from mim's issue list (morloc-manager
model/issues/LIST.md), checked against the code 2026-11-04 [read].

- **An IPv6 loopback host can never bind.** morloc-nexus/src/mcp.rs:1163
  (mcp HTTP) and :1947 (front end) build the address as
  `format!("{}:{}", host, port)`, giving `::1:9000`; IPv6 needs brackets.
- **`MORLOC_EVAL_ALLOW_NO_AUTH=0` waives the eval lock.** cli.rs:869
  treats any non-empty value as true.
- **`/call` reaches commands MCP excludes.** The front end's `/call`
  (mcp.rs:2545-2580) checks only `api_modules`; the stream-argument and
  `@stdin` exclusions of `build_tool_shapes` (mcp.rs:315) are not applied.
- **401 bodies and OPTIONS differ between servers.** The daemon answers
  `{"status":"error","class":"unauthorized","error":"unauthorized"}`
  (daemon_ffi.rs:3125); the front end and single-program mcp answer
  `{"error":"unauthorized"}` (mcp.rs:2063, :1392). Single-program mcp
  (`build_response`, mcp.rs:1380) has no OPTIONS handling.
- **MCP `notifications/cancelled` is ignored.** mcp.rs:499 drops unknown
  notifications, so a cancelled call runs on. A crashed child daemon stays
  down until the next forward restarts it (mcp.rs:1647).
- **`GET /eval` with eval off is 405, not 404.** The method check
  (mcp.rs:2077-2085) runs before `frontend_eval_api`'s not-exposed 404
  (mcp.rs:2339). Compare the daemon's opposite case under IFACE-10.
- **A `__` collision in module or command names aborts the front end.**
  mcp.rs:1849-1860 exits 1 on purpose (fail closed). Needs a ruling: refuse
  at registration, or abort the whole front end as now.
- **An HTTP call costs about 0.6 ms fixed.** [measured, 2026-11-13]
  A scalar result takes 0.60 ms over HTTP and an 8 KB result 0.68 ms,
  so nearly all of a small call is fixed cost. Not investigated.
