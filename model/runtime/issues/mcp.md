# MCP tool calls

Governed by model/language/interfaces.md (IFACE-6, IFACE-8, IFACE-12,
IFACE-14, IFACE-15). Repro programs: web/morloc-studio/public/assets/
home-examples/smiles (`structure` with `@mime`-typed `@render` actions) and
test-suite/golden-tests/multi-output-stream (streaming commands). Paths are
under data/rust/.

- **A call that streams its output is refused off the CLI.** Violates
  IFACE-6 and IFACE-8 (the output must reach the caller); refused before it
  runs when the manifest shows it (a streaming command's own value, a
  stream-kind action: daemon_ffi.rs `resolve_render_target`), and by
  DAEMON-15 at the `@stdout` open otherwise (`<IO> ()` producers such as
  stdio-filter `producer`, value commands that stream along the way such
  as multi-output-value `noisy`). Replay-only actions (no `entry`) are
  refused the same way. Needs per-call stdout capture (plan step 7,
  /work/plans/interface-consistency-plan.md).
- **The MCP server's stdio server runs in one-shot mode.** [read]
  morloc-nexus/src/cli.rs:892 `mcp_args_to_config` leaves `daemon_flag`
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
- **A served call can write into the server's stdout and stderr through
  a descriptor path or a symlink.** [reproduced, 2026-10-21] Violates
  DAEMON-15. `writeTo path = do { Ok s <- @open path :: <IO> (Try Str
  (OStream Int)); @write 0 s [10, 20, 30]; @close s; 1 }` served by
  `morloc-nexus daemon --http-port=...`: `/call/writeTo` with
  `["/proc/<daemon pid>/fd/1"]` writes the stream into the daemon's stdout;
  `.../fd/2` truncates its log; a symlink to `/dev/stdout` is accepted.
  `/dev/stdout`, `/dev//stdout`, `/proc/self/fd/1` are refused.
  `names_server_stdio` (stream.rs:2923) matches path text, and the
  custodian opens the file in the nexus (SLOT-10). Fix: compare the opened
  file's dev/ino with fds 0-2. DAEMON-15 should say a path is compared by
  the file it opens. Repro: /work/plans/model-audit-2026-10-21/repro/I2/d15/main.loc.
- **The daemon's HTTP parser truncates path and query at 255 bytes.**
  [read, 2026-10-21] http_ffi.rs:209-214 copies into 256-byte arrays, so a
  longer `/call/<cmd>` or `?render=` is routed by its prefix, not refused.
