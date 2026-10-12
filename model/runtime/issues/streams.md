# Stream writers

Written streams go through the custodian (streams.md SLOT-10..16). Changes
here touch locks and ownership: model first.

- Open question (performance): the nexus's stdout lock
  (morloc-nexus/src/stdio_server.rs `do_write`, `STDOUT_SLOT`) is held
  across transcoding and `write(1)`. Only stdout writes and the final
  footer take it, and their frames must reach the descriptor in order, so
  holding it across the write is required; what it costs is that two
  pools streaming to stdout cannot transcode in parallel. Worth moving the
  transcoding out only if a benchmark of concurrent stdout writers shows
  it.
- Open question (SLOT-15): should close and flush return as soon as their
  marker is queued, with the wait moved to where a finished file is needed
  (opening it to read, `@save`/`@concat`, reopening the path, the end of
  the call, the end of the run)? Ruled: decide by benchmark against the
  waiting form, which is implemented. Measured 2026-10-10
  (/work/plans/streams-single-writer/BENCH.md): open+write+close of a small
  stream 2.5 ms (2.4 ms before the custodian), @write+@flush of one element
  17 us (10 us before). Only a flush-per-element writer pays visibly. If
  deferred, where is a custodian write error reported: the next operation on
  the stream, the end of the call, or the end of the run?
- Open question (SLOT-10, SLOT-13): an OStream opened by a process that
  dies without closing it, where no sweep covers that process (R and Python
  fork workers, user forks), stays open until the run ends: it pins a
  writer thread, a descriptor, the flock and its buffers. A reopen of the
  path finishes it (as the kernel dropping the flock allowed before).
  Should the custodian also finish it on its own when the opener is gone,
  at the cost of other live writers of the handle?
- [measured] Possible speedup (SLOT-11): appending an element with
  nested pointers (`[[Str]]`, optionals of arrays) walks the flattened
  element to move its relative pointers to its place in the buffer
  (stream.rs `append_one_element`). A `[[Str]]` element costs 305 ns, of
  which flattening is 100 ns (stream_bench_string_elements with
  BENCH_DEEP=1, 2026-10-12); a `Str` element, which skips the walk
  (`shallow_array_slots`), 135 ns with 41 ns flattening. Flattening at the
  buffer offset would drop the walk, about 80 ns per nested element, but
  the flattener re-reads pointers it wrote, so every pointer write changes.
- Open question (SLOT-16): a stdout write returns once its batches are
  queued, so raw text a pool prints to its inherited stdout while it streams
  to `@stdout` can land inside a frame; before, a single-threaded pool's
  prints landed between frames (a threaded pool's could already split one).
  Either way the print corrupts a packet or JSON stream. Waiting for each
  queued batch to be written restores the old placement but serialises
  compression with production: trim -z 3 took 7.9 s with the wait, 3.8 s
  without, 4.3 s before the custodian (2026-10-10). Chosen for now (not
  ruled): no wait.
  Should pools' raw stdout go to stderr while a command streams to stdout?
- Open question (`@tell`): intrinsics.md gives `@tell` no failure value and
  it returns 0 when this process has no `@stdout` open, so a pool other
  than the one that opened the run's stdout reports 0 while elements have
  been written. Should `@tell` count the run's stdout from any pool?
- **A path left open by a daemon call may refuse to reopen in the next
  call.** [read] Gap in SLOT-15 ("its path reopens at once" covers only a
  user close). A nexus-evaluated daemon request that leaves an OStream open
  has it closed by the call sweep after the reply is sent
  (daemon_ffi.rs `sweep_call`), which does not wait for the writer
  (SLOT-19). A next request that opens the same path before the custodian
  has finished it fails: the custodian takes the file lock non-blocking
  (stream.rs `lock_stream_file`). Open: should an open of a path whose
  previous stream is still closing wait for that close?
- [measured] Nested numeric arrays get natural alignment, not the 64 bytes
  `Schema::array_data_alignment` (morloc-runtime-types schema.rs) states as
  the wire format's: lists built from JSON land at 0/8/32/40 mod 64, and
  lists read back from a written stream (IStream and IFile paths) at
  multiples of 8 only (test
  numeric_lists_read_from_a_stream_keep_their_natural_alignment checks
  the natural alignment). Spec gap: is 64 a guarantee or a preference?
  Still untested: a 16-byte requirement (an Arrow decimal buffer inside a
  table element written to a stream), where the writer's 8-byte steps
  (stream.rs `append_one_element`, write_behind.rs compaction) could
  misalign.
- [measured] Possible speedup (stdin): the nexus reads a stdin sub-packet
  only when a pool's `@next` asks for it (stdio_server.rs `do_next`), so a
  piped producer and the reader take turns. Counting 20M Ints in 200
  sub-packets (2026-10-12): from a file 0.36 s, stdin redirected from that
  file 0.39 s, the same file through `cat |` 1.20 s, where `cat | wc -c`
  alone takes 0.69 s. Reading the next sub-packet ahead would overlap the
  pipe with the reader (at best ~0.7 s), but makes the nexus hold a block
  no pool has asked for, to free at close, at the end of the run and on a
  dropped reader: model first. Small sub-packets pay the request instead:
  20000 sub-packets of 100 Ints take 0.50 s from stdin and 0.13 s from a
  file, ~18 us more per `@next`, which reading ahead does not remove.
