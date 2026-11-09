# IFile access from morloc code

- **`.[i] f` on an IFile does not compile in a Rust pool.** [reproduced,
  2026-10-15, c4da6cf3] The index reaches `rustmorloc::ifile_walk` as a bare
  `i64` (`&[(0 as i64)]`), but the shim takes `&[Option<i64>]`: rustc E0308
  "expected `Option<i64>`, found `i64`". Slices wrap each bound in
  `__to_index__` (Express.hs `expressBracketSlice`); `expressBracketIndex`
  passes the index unwrapped, which the nexus walker accepts, so the golden
  test `rust-ifile-walk` (evaluated on the nexus) does not see it. Repro: a
  Rust-pool function `firstLen g = size (.bases (.[0] g))` with
  `g :: IFile [FastaEntry]` (bio). [reproduced 2026-11-02] A second
  site: `.[1].y f` on `IFile [Point]` (Express.hs `dispatchPatternStruct`)
  also passes bare bounds.

- **A projection after an IFile bracket decodes whole elements.** [reproduced,
  2026-10-15] `.[:].header g` lowers to a walk of `.[:]` (every element
  materialized) followed by a map of `.header`, and `.[i].header g` to a walk
  of `.[]` followed by `.header`; the runtime walker accepts the whole chain
  (`morloc-nexus view --pattern ".[:].name"`), so the projection could run
  in it. On a 3-sequence FASTA stream with a 250 Mb sequence, the names cost
  1.75 GB peak and one header 0.8 GB. Gap in model/runtime/streams.md: no
  item says what an IFile accessor decodes.

  [reproduced 2026-11-02, briefs/B18-B19.md] Of four access shapes over a
  100 MB file (two 50 MB sequences), only `.[:].header` inside a Python
  pool is fused (17 MiB peak); on the nexus `.[:].header` and `.[1].header`
  peak at 202 MiB, and `.[1].header` in a Python pool at 252 MiB: the
  nexus `IntrMap` arm has no fusion, and neither path looks through a
  `.[i]` result.
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

- **A slice holds several whole copies of its data and runs 3x slower
  than the stream path.** [reproduced, 2026-11-08] There is no spec item
  yet; it is proposed as SLICE-1/2 in /work/plans/output-path-perf.
  mosm `cut 0:100000` on an IFile: 2.5 s and a 1.67 GB peak for 240 MB of
  output; the same slice through stdin takes 0.9 s.
  - `slice_runs` (stream.rs ~6975) locates every touched sub-packet before
    it copies any. `copy_runs` then builds one result block, and the
    binding decodes it whole.
  - The generic route (a step other than 1, or a field tail) holds every
    touched sub-packet for the whole walk, then `consolidate`s the parts,
    a second copy.
  - SHM growth doubles (SHM-12), and volumes stay pre-faulted.
  - Peak per holder: to be measured (plan step 0); mosm bench/slice.sh.
- **`_mlc_ifile_walk` may leak its block when decoding throws.**
  [speculative, 2026-11-08] `_mlc_ifile_walk` and `_mlc_stream_layout`
  (data/lang/cpp/cppmorloc.hpp) call `shfree_cpp` only after
  `from_voidstar` returns, so an exception skips the free. It is unknown
  whether the eval arena reclaims the block.

- **The open-time index walk is debug-only, so an undercounting index
  gives wrong slice elements in a release build.** [read, 2026-11-08]
  SLOT-30 (ruled 2026-11-02) walks the sub-packets at open only in a debug
  build or with `MORLOC_VERIFY_INDEX=1`. A footer index that passes the
  release checks (order, spacing, total) but gives one sub-packet fewer
  elements than it holds, and the next one more, maps a global index to
  the wrong element, with no error. For example, claimed counts [3, 9]
  over actual [5, 7] read element 5 as element 3. A count larger than
  the sub-packet fails at `plan_run`'s bound check. Candidate fix at no
  I/O cost: every slice and index read already loads the sub-packet's
  root `Array`, so compare its `size` with the index count there and fail
  on a mismatch.
- **Three index tests fail under `cargo test --release`.** [reproduced,
  2026-11-08, c3a7b3bb3] `an_ifile_refuses_permuted_counts`,
  `..._skips_the_first_sub_packet` and `..._skips_the_last_sub_packet`
  (stream.rs ~10210-10236) assert the debug-only walk of SLOT-30, so they
  pass only in a debug build. They should set `MORLOC_VERIFY_INDEX`, or be
  marked debug-only.

- **A repeated slice decompresses its interior sub-packets again.**
  [read, 2026-11-08] By SLOT-34, a slice caches only its end sub-packets,
  so a loop of identical or overlapping slices over a compressed IFile
  pays decompression of every interior sub-packet on every call, even
  when the cache has room. This trades CPU for memory: a one-shot slice
  no longer leaves up to `MORLOC_IFILE_CACHE_BYTES` held. Revisit the
  policy with the piece-wise slice (plan step 3b). The user prefers caching
  interior sub-packets while there is room (2026-11-08). Tried and
  reverted the same day (patch in /work/plans/output-path-perf/patches/):
  the bound (`handle_bytes`) is shared by all of a handle's slots, but a
  slot evicts only its own entries. A slice's interiors then fill the bound
  for sibling slots, block decode-ahead reservations (SLOT-27, which
  reserve without evicting), and make the slice's last end evict other
  reads' entries. Caching interiors needs cross-slot eviction and
  cold-inserted entries (clock bit 0) first.

- **A field walk reads only the first sub-packet.** [read, 2026-11-09]
  `shared_ifile_general` (stream.rs) materializes sub-packet 0 and walks it
  with the value schema. A path that reaches it with a root index
  (`.[].x`, not special-cased by `shared_ifile_walk`) can only reach
  elements of the first sub-packet of a multi-sub-packet IFile:
  `f.[1500].x` over three sub-packets of 1000 fails its bounds check, or
  reads the wrong element.
