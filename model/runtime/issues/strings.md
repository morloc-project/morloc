# Str bytes and encodings

BASE-5 is ruled (2026-11-08): a `Str` is valid UTF-8 text. FMT-10 holds
the runtime's rule. What remains:

- **Some text conversions in the nexus and daemon are lossy.** [read,
  2026-11-08] `cli_token_to_json` (morloc-nexus dispatch.rs:568), the
  top-level error text (dispatch.rs:1296, daemon_ffi.rs:2060
  `top_level_err`), a daemon reply's error text (daemon_ffi.rs:1351
  `envelope`) and a forked subcommand's output (daemon_ffi.rs:1812, 1844
  `classify_failed_command`) convert lossily. Their
  input is text the renderers produced, which is UTF-8 by FMT-10, except
  in a program built with `--unsafe-skip-utf8-check`, where invalid bytes
  are replaced silently. Make them strict, or document the opt-out's
  reach.
- **Python's decode error names no producer.** [reproduced, 2026-11-08]
  A Str that reaches Python unchecked (only with the opt-out) fails with
  "'utf-8' codec can't decode byte 0xff in position 2" from pymorloc.c:2016,
  naming the position but not the language that made it.
- **Perf: the header fast path resolves each Str separately.**
  [read, 2026-11-08] `RebaseWalk::headers` (voidstar.rs) now resolves and
  validates every `Str` body on its own: a `rel2abs` lookup in SHM mode
  and a SIMD call per string. Bodies that lie side by side could be
  validated as one span.
- **Perf: slice and element results are checked again.** [read,
  2026-11-09] `require_text` (stream.rs) runs the outside-data check over
  every IFile slice and element result, even when its sub-packet was
  relocated and so already checked. Only zero-copy (File) sources need it.
- **Perf: an imported table's text is checked on every import.** [read,
  2026-11-09] `shm_to_batch` (arrow_shm.rs) checks every text column before
  each import, so a table the program built, or one `validate` just checked,
  is checked again on every print and every Rust-side import.
- **Perf: R converts a latin1 string twice.** [read, 2026-11-09] The size
  and write passes of the R encoder each call `r_str_utf8`, so a latin1 or
  native-locale string is converted twice, both copies held until the call
  returns.
- **A stage's saved stream is read back as data from outside.** [reproduced,
  2026-11-22] FMT-10 says the opt-out turns off the check of the bodies the
  program builds. A render action on a command that streams `Str` runs on
  the stream the stage saved to a file, and the replay reads it with the
  outside check, so under `MORLOC_SKIP_UTF8_CHECK=1` it fails ("in data
  read from outside", `_mlc_replay`; golden stdout-raw-producers,
  `stagedCpp 1 -a`), while the same bytes rendered from a stream of other
  batches print (stdout-raw-bodies, `bad 1 -t`). Needs a ruling: mark a
  stage file as the program's own, or say the opt-out stops at a stage. If
  it is marked, decide it from the path the nexus created for the stage,
  never from metadata in the file, which anyone can write.
