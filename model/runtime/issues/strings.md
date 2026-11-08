# Str bytes and encodings

BASE-5 is ruled (2026-11-08): a `Str` is valid UTF-8 text, and invalid
UTF-8 is not a `Str`. A value that is not valid UTF-8 must fail where it
crosses a boundary (BASE-4), naming the language and the byte position.
Fix plan: /work/plans/output-path-perf/PLAN.md, step 1.

- **A non-UTF-8 byte in a `Str` drops output silently.** [reproduced,
  2026-11-08, 3b4a189a9] Violates BASE-4. `ShmReader::read_str`
  (morloc-runtime/src/json.rs:80) returns `""` on invalid UTF-8
  (`unwrap_or("")`), so raw output prints nothing for that body, with
  exit 0. JSON output loses it the same way. Callers: `write_str_body`
  (json.rs:1147), `pretty_print_voidstar` (json.rs:1247), `json_escape`
  (json.rs:1471). `subpacket_json_inner` (morloc-nexus/src/stdio_bridge.rs:384)
  uses `to_string_lossy`, which replaces the bytes. Repro (mosm):
  `printf '>a\nACGT\n>bin\nAC\xffGT\n>b\nGGCC\n' > u.fa;
  mosm -f packet convert u.fa > u.dat; mosm filter u.dat` prints nothing,
  exit 0.
- **More lossy conversions of value bytes.** [read, 2026-11-08] Violates
  BASE-4.
  - daemon `result_json` and `mlc_show` output (daemon_ffi.rs ~1304,
    ~1941), and a call's stdout (~1659, ~1725);
  - nexus `cli_token_to_json` (dispatch.rs ~567) and the top-level error
    text (~1291);
  - MCP responses (mcp.rs ~907, ~921).
- **C++ and R send invalid UTF-8 unchecked.** [read, 2026-11-08] Violates
  BASE-4. Neither the C++ encoder (`to_voidstar` for `std::string`,
  data/lang/cpp) nor the R encoder (rmorloc.c `CHAR()` reads at ~728,
  ~736, ~1265, ~1270) validates. An R string marked latin1 leaves as
  latin1 bytes.
- **Python accepts bytes for a `Str` unchecked.** [read, 2026-11-08]
  Violates BASE-4. pymorloc.c ~928, ~1375 and ~1428 take
  `bytes`/`bytearray` for `MORLOC_STRING` and copy them raw. Ruled
  2026-11-08: accept them and validate them.
- **R decodes a `Str` with the native encoding mark.** [read, 2026-11-08]
  `mkCharLen` (rmorloc.c ~1956, ~2008) marks the string native, not
  UTF-8. In a non-UTF-8 locale R misreads every non-ASCII `Str`. The fix
  is `mkCharLenCE(.., CE_UTF8)`.
- **Outside bytes reach pools without a UTF-8 check.** [read, 2026-11-08]
  Violates BASE-4. These copy file bytes into a pool without validating
  any `Str` in them: `slice_runs`/`copy_runs` (stream.rs ~6975-7122), the
  generic slice copy (~6893-6922) and IFile element extraction. Arrow
  `utf8` columns are never walked (null_check.rs ~251).
- **Native decode errors give no position.** [read, 2026-11-08] Violates
  BASE-4's naming rule. Python reports "Failed to parse data as string"
  (pymorloc.c ~1915). Rust reports "Str field is not valid UTF-8"
  (rustmorloc lib.rs ~2379). Neither names the byte position or the
  producer.
- **The nexus panics on non-UTF-8 arguments.** [read, 2026-11-08]
  morloc-nexus/src/main.rs:105 and :145 read argv with `std::env::args()`,
  which panics on an argument that is not valid Unicode. The fix is
  `args_os`, rejecting invalid arguments with a message that names them.
