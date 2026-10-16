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
