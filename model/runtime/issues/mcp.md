# MCP tool calls

Governed by model/language/interfaces.md (IFACE-6, IFACE-8, IFACE-14,
IFACE-15). Repro program for the render entries:
web/morloc-studio/public/assets/home-examples/smiles (`structure :: Smiles
-> Molecule` with `@render` --mol/--pdb/--xyz/--png/--svg, each target type
`@mime`-tagged). Paths are under data/rust/.

- **Every non-raw `_render` fails.** [reproduced, 0.109.0] `morloc-nexus mcp
  ./smiles` lists `_render` enum `[raw, mol, pdb, xyz, png, svg]` for
  `structure`; `tools/call structure {"_1":"CCO","_render":"pdb"}` (and
  `png`) returns isError "Unknown command: mlcp_structure_pdb". [read]
  morloc-nexus/src/mcp.rs:473-482 resolves `_render` through
  `McpToolShape::dispatch_for` (morloc-nexus/src/json_help.rs:797-812) to
  the action's entry and sends that name as the command; every `mlcp_*`
  entry is `internal`, and morloc-runtime/src/daemon_ffi.rs:2152 refuses
  internal commands (649883e9, #139). HTTP `?render=` survives because it
  sends the public name and re-points to the entry after the filter
  (daemon_ffi.rs:2170-2183), gated on `current_output_http()`. The router
  path (mcp.rs:2526-2534) forwards the entry name the same way
  [speculative: same failure, not run]. Keep the #139 filter; give MCP the
  render selector HTTP has. No test runs `tools/call` with a valid
  renderer: daemon-tests/run-tests.sh:522-524 asserts the 404 on an
  internal entry, and golden-tests/multi-output-stream/serve.sh:19 sends
  `_render:"all"`, which `dispatch_for` rejects before dispatch.
- **A rendered result breaks the advertised `outputSchema`.** [read] A
  record-returning command advertises `outputSchema`
  (json_help.rs:1210-1214); a render result is `{content, isError}` with no
  `structuredContent` (mcp.rs:693-698), which MCP requires when a tool
  declares an output schema. Shows once the first entry is fixed. Options:
  no `outputSchema` on tools with renders, one tool per render, or the
  structured value alongside the rendered block.
- **Media mapped by top-level type only.** [read; reproduced for the raw
  case: `structure :: Smiles -> MolFile` with `@mime chemical/x-mdl-molfile`
  returns a `resource` blob of base64 on a plain `tools/call`] `media_content_block`
  (mcp.rs:703-724): `image/*`/`audio/*` -> image/audio block; `text/*` ->
  text block, mime dropped, invalid UTF-8 replaced silently
  (`from_utf8_lossy`); anything else -> `resource` with a base64 `blob`.
  So `image/svg+xml` goes out as an image block, which raster-only clients
  (including the Claude API: png/jpeg/gif/webp) reject; and `Str`-typed
  text formats (chemical/x-pdb, x-mdl-molfile, x-xyz, application/json)
  reach the model as opaque base64. Valid UTF-8 or a `Str` return belongs
  in `resource.text` with its mimeType.
- **Every resource has URI `morloc://return`.** [read] mcp.rs:718; the
  server declares only the `tools` capability.
- **No outbound size cap.** [read] `MAX_LINE_BYTES` (mcp.rs:138) bounds
  inbound lines only; a render is base64'd (4/3) into one unbounded stdout
  line.
- **Router HTTP API drops `?render=`.** [read] `frontend_call_api` routes on
  the path with the query stripped (mcp.rs:2018) and builds a call with no
  render [speculative: end to end, not run], so
  `/call/<module>/structure?render=png` through mim's front end would
  return raw JSON, and a bare call would skip the `@default` action that
  the daemon's own HTTP fires (CLI-7 case 3).
- **`_render` enum includes `@with` actions.** [read] json_help.rs:1163-1173
  adds every terminal with an entry, not only `@render`, while the
  property describes them as renderers.
- **A call that streams to `@stdout` is refused off the CLI.** Violates
  IFACE-8 (the output must reach the caller); DAEMON-15 refuses the open so
  nothing is lost. multi-output-stream/main-py.loc built as `mos`: MCP
  `tools/call stream` is `isError` "@stdout is not available to a call
  served by a daemon or an MCP server". Covers `@collect` commands,
  stream-kind actions, `<IO> ()` producers (stdio-filter `producer`) and
  value commands that stream along the way (multi-output-value `noisy`).
  Replay-only actions (no `entry`) are unreachable on every served
  transport (daemon_ffi.rs:303-314; MCP omits them). Needs per-call
  stdout capture (plan step 7, /work/plans/interface-consistency-plan.md).
- **The MCP server's stdio server runs in one-shot mode.** [read]
  morloc-nexus/src/cli.rs:892 `mcp_args_to_config` leaves `daemon_flag`
  false, so `stdio_server::start` (main.rs) runs with DAEMON_MODE off: a
  broken pipe on a stdio stream exits the server with 141
  (stdio_server.rs `pipe_closed_resp`) instead of failing one call, and
  the recovery gate at stdio_server.rs:311 is skipped.
- **A `@render` result without `@mime` is JSON, not bytes, off the CLI.**
  [read] Media bytes are requested only when the entry has a `@mime`
  (mcp.rs:481, daemon_ffi.rs:2219), so a `Str` render reaches MCP/HTTP
  JSON-quoted and a `Vector U8` render as an int array; CLI-8 says the
  handler's bytes are written verbatim.
