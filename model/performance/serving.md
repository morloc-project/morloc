# Serving (SERVE)

What a result costs on its way out of a daemon: over HTTP, and over MCP,
against the CLI printing the same value. The daemon and the MCP server
are already running, so no item here pays START.

### SERVE-1 An HTTP result costs about one copy per byte more than the CLI
Intent: proposed
Bench: serve-json
Model: t(x) = c + b*x; x bytes of result JSON, for an HTTP call (`http`) and for `./nexus reals n` (`cli`); http.over = http.b - cli.b
Measured: 2026-11-13-0250-c4022bfa7+dirty-w4, load 1.6-1.7. http.over 2.69 ns per byte (http.b 7.99, cli.b 5.30). Before the hand-written envelope (installed build 02:24, load 2.8): 6.69 ns (12.12 against 5.43).
Goal: http.over <= 1 ns

The envelope is written around the result text, which is validated once
and sent from where the call left it, without a joined copy. What is left
over the CLI is that pass, the socket and the client's read. The CLI
series pays nexus and pool start in its intercept; only slopes compare.

### SERVE-2 An HTTP call has a fixed cost below a millisecond
Intent: open
Bench: serve-json
Model: t = c; one HTTP call of a scalar command (`call.c`), and one `/health` probe that dispatches nothing (`health.c`), each the mean of 200 calls in sequence
Measured: 2026-11-13-0250-c4022bfa7+dirty-w4, load 1.6-1.7. call.c 254 us, health.c 187 us. Before: 257 and 191 us.

`call.c - health.c` is the dispatch, the pool round trip included
(XL-1); `health.c` is the connection: accept, worker hand-off, request
parse and close. Where the rest of a call's time goes is not yet known.

### SERVE-3 MCP adds little to a result over HTTP
Intent: proposed
Bench: serve-json
Model: t(x) = c + b*x; x bytes of result JSON, one `tools/call` over stdio (`mcp`) against an HTTP call of the same size in the same run (`mcp.http`); mcp.over = mcp.b - mcp.http.b; `call.c` the mean of 200 scalar calls
Measured: 2026-11-13-0250-c4022bfa7+dirty-w4, load 1.6-1.7. mcp.over 5.20 ns per byte over HTTP, a scalar tools/call 62 us. Before: 13.2 ns, 59 us.
Goal: mcp.over <= 4 ns

An MCP result is the JSON text again as a JSON string, so it is escaped
once and the client reads it twice; only an object is also parsed, for
`structuredContent`. Results are capped by the 64 MiB JSON-RPC line, so
the sweep stops near 40 MB.
