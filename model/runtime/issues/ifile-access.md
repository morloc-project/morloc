# IFile access from morloc code

- **`.[i] f` on an IFile does not compile in a Rust pool.** [reproduced,
  2026-10-15, c4da6cf3] The index reaches `rustmorloc::ifile_walk` as a bare
  `i64` (`&[(0 as i64)]`), but the shim takes `&[Option<i64>]`: rustc E0308
  "expected `Option<i64>`, found `i64`". Slices wrap each bound in
  `__to_index__` (Express.hs `expressBracketSlice`); `expressBracketIndex`
  passes the index unwrapped, which the nexus walker accepts, so the golden
  test `rust-ifile-walk` (evaluated on the nexus) does not see it. Repro: a
  Rust-pool function `firstLen g = size (.bases (.[0] g))` with
  `g :: IFile [FastaEntry]` (bio).

- **A projection after an IFile bracket decodes whole elements.** [reproduced,
  2026-10-15] `.[:].header g` lowers to a walk of `.[:]` (every element
  materialized) followed by a map of `.header`, and `.[i].header g` to a walk
  of `.[]` followed by `.header`; the runtime walker accepts the whole chain
  (`morloc-nexus view --pattern ".[:].name"`), so the projection could run
  in it. On a 3-sequence FASTA stream with a 250 Mb sequence, the names cost
  1.75 GB peak and one header 0.8 GB. Gap in model/runtime/streams.md: no
  item says what an IFile accessor decodes.

- **`@open` of a missing file does not name it.** [reproduced, 2026-10-15]
  `@open p :: <IO> (Try Str (IStream a))` on a path that does not exist
  returns `Err "IO error: No such file or directory (os error 2)"`; the
  message carries neither the path nor the operation, so a program that
  throws it reports an unplaceable error. Gap in model/runtime/streams.md:
  no item says what an `@open` error states.
- **`@stdin` checks the element schema at the first `@next`.**
  [reproduced, 2026-10-21] Gap in STRM-1, which says a mismatch is an
  `Err` at open; for a pipe the schema arrives with the bytes. STRM-1 needs
  a stdin clause.
- **A stream refuses a second compression level.** [reproduced,
  2026-10-21] Gap in STRM-2. `@write 3 o [1,2,3]` then `@write 0 o [4,5]`
  fails "@write level mismatch ... All sub-packets must share a level";
  the buffered `[1,2,3]`, never flushed, is dropped when the failed run
  ends, as SLOT-13 says. No item says a stream has one level, and `@write`
  takes one per call. Ruled 2026-10-29: one level per output stream. Not
  yet in STRM-2 or the code; held for a session on compression as a whole.
- **An OStream in path form cannot be decoded.** [read, 2026-10-29] A
  handle crossing a cache, a remote call or another nexus is rewritten to
  its path (handle_scan.rs), and a CLI or JSON OStream argument arrives as
  a path, but `open_typed` sends OSTREAM to `open_dispatch`, which refuses
  it, so the call always fails. The storage schema is now at hand (FMT-9);
  what reopening a writer means (create, truncate, append, a value decoded
  twice) has no rule. Gap in STRM-1.
- **Files written before FMT-9 that hold a handle schema no longer
  parse.** [read, 2026-10-29] A saved packet or stream whose schema has a
  bare `F`, `O` or `I` (a record with an IFile field) fails with "a stream
  handle schema must name what its file stores". The ABI version moved
  with the change; no item says whether data files outlive an ABI version.
  The untyped `mlc_open` stays exported, unchecked, for `morloc-nexus
  view`, which reads a file at its own schema.
