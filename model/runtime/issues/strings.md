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
- **MessagePack checks text whatever its origin.** [read, 2026-11-22]
  `unpack_from` (mpack.rs, the `SerialType::String` arm of the unpack
  walk) runs `first_invalid` on every msgpack Str, ignoring the origin and
  the opt-out. So under `MORLOC_SKIP_UTF8_CHECK=1` a Str the program built
  that crosses as a FILE+MSGPACK argument (the form used when shared
  memory is off) fails, even locally, while FMT-10 says the opt-out turns
  off the check of the bodies the program builds.
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
