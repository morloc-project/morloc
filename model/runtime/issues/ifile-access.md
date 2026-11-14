# IFile access from morloc code

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

- **A release build does not notice a footer that drops a sub-packet.**
  [read, 2026-11-12] Gap in SLOT-30: each read now checks a sub-packet's
  root count against its index entry, but a footer whose entries all match
  their sub-packets while one sub-packet is missing from the index loses
  that sub-packet's elements with no error, outside the debug-only open
  walk. Candidate: compare the file's data extent with the sum of the
  indexed sub-packet lengths at open (no I/O beyond the footer): the first
  entry at the body start, the last sub-packet ending at the footer. Needs
  a ruling: SLOT-30 (ruled 2026-11-02) allows gaps ("the last sub-packet
  ends before the footer"), and every writer must then be shown to leave
  none (custody writer, @concat, the nexus stdout relay
  stdio_server.rs finish_logical_packet, append).
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
- **Strided slices of Str or Str-holding records keep extra costs.**
  [measured in-process, 2026-11-13] SLOT-38 made strided and field-tail
  slices one run per sub-packet with one sub-packet held; left: a [Str]
  result is sized and copied in two passes, so at level 3 each interior
  sub-packet is decompressed twice (`.[::7]` 0.70 -> 1.16 s in-process,
  for ~8 MB peak instead of everything held; uncompressed, pass 2 faults
  back in the pages release_passed dropped after pass 1; a budgeted
  first-pass cache needs a ruling); a record holding a Str is still deep-copied per element
  and consolidated (~2x its output in memory: a space-generic size walk is
  needed, SizeWalk resolves only in SHM); at level 3 a stride k
  decompresses k times a unit slice's data.
