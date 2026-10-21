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
- **UNSOUND: `@open` of an IFile never checks the element schema, nor
  does an IStream opened in the nexus.** [reproduced, 2026-10-21] Violates
  STRM-1. After writing an `OStream Int`, `@open path :: <IO> (Try Str
  (IStream Real))` in the nexus reads `[5e-324,...]`; an `IFile [Real]`
  open in the nexus or a Python pool reads the Int bits as doubles. A
  pool IStream open does fail "schema mismatch". Causes: the nexus `open`
  node carries only the handle schema (CodeGenerator/Nexus.hs:1115-1135,
  manifest `"schema": "I"`) and eval_ffi.rs:1452-1500 calls
  `mlc_probe_packet`/`mlc_open` with no element schema, so the check in
  stream.rs:7295-7323 never runs; pools lower IFile to `IIntrinsicOpen`
  with no schema (Translator/Imperative.hs:2742-2745). Repro:
  /work/plans/model-audit-2026-10-21/repro/E/strm/main.loc, b.loc.
- **`@stdin` checks the element schema at the first `@next`.**
  [reproduced, 2026-10-21] Gap in STRM-1, which says a mismatch is an
  `Err` at open; for a pipe the schema arrives with the bytes. STRM-1 needs
  a stdin clause.
- **A stream refuses a second compression level.** [reproduced,
  2026-10-21] Gap in STRM-2. `@write 3 o [1,2,3]` then `@write 0 o [4,5]`
  fails "@write level mismatch ... All sub-packets must share a level";
  the buffered `[1,2,3]` is lost and the file has no footer. No item says
  a stream has one level, and `@write` takes one per call. Either the
  level belongs on the open, or it is per sub-packet. The failed write
  losing earlier data is a bug either way.
