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
- **The daemon's HTTP parser truncates path and query at 255 bytes.**
  [read, 2026-10-21] http_ffi.rs:209-214 copies into 256-byte arrays, so a
  longer `/call/<cmd>` or `?render=` is routed by its prefix, not refused.
