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
- **`@next` errors carry their prefix twice.** [reproduced, 0.109.0]
  `./rs producer | ./rs checkSum` (rust-stdio golden) prints "@next:
  @next: stdin ended after 23 byte(s) ...".
- **The daemon's remote path sets no call id and runs no sweep.** [read]
  Against SLOT-19. data/rust/morloc-runtime/src/daemon_ffi.rs:2359-2360 and
  2438-2439 set and sweep a call id on the pure path only; the remote
  branch (2441 on) does neither, and stream.rs:2485-2489 claims "the nexus
  dispatch always pre-sets a call_id", which holds only there.
- **A reader cannot tell a finished stream file from an unfinished one.**
  [reproduced 0.109.0, and read] Spec: SLOT-28 to SLOT-31 (streams.md,
  "Reading a stream file's end"); SLOT-31 is open. An IStream reads every
  complete batch and then reports the end of the stream in every end state
  (SLOT-29), so a program cannot tell a clean close from a file its writer
  never finished, and any count, statistic or copy over such a file is
  silently partial. lib/biolib rnaseq R23 hit it, and keeps the check in a
  separate `knownIssues` export.
  - No footer: `saveSource 0 p` of batches [1,2,3], [4,5], [6] writes 689
    bytes; with 300 bytes cut off, `@next` returns `Ok [[1,2,3]]`, `Ok
    [[4,5]]`, `Ok []`; with 500 cut, `Ok []` at once. Only a cut inside a
    sub-packet header errors. Cause: stream_format.rs:112 forward-scans
    when no footer parses.
  - Final footer, FAILED: the status is parsed (stream_format.rs ~563) and
    never consulted, so a file whose writer died inside the slot lock reads
    as a clean one. IFile opens it too (SLOT-30).
  - Temporary footer: written both while a stream is being written, when a
    reader may legitimately read it as it grows, and by a stream that will
    never change again (discarded without `@close` with its unflushed
    elements dropped, or a custodian that failed or was killed). The file
    does not say which. The writing tree's registry knows while the tree
    runs (the sweeps find open writers there); a reader on another machine
    (a SLURM job on a shared filesystem) or after the tree has ended
    cannot know.
  - Standard input carries no footer at all: an empty sub-packet or EOF
    ends the stream, so a writer that died mid-stream is not seen either.
  Needs a TLA+ model before any fix (tla/, beside StreamQueue.tla): a
  writer (writing, flushed, closed, paused, discarded, failed, killed at
  any step), the custodian's file states (SLOT-28's five, plus a
  sub-packet half-written), the registry's view of the writer (live, gone,
  unknown for another machine), and readers that open at any step and read
  to the end. Properties: a reader that reports a clean end has read every
  element its writer flushed before closing; a reader of a stream still
  being written is never told it failed; a reader is told of a failure in
  every state where the file will not change again and its writer did not
  close it. Candidate rule (to model, then rule): deliver every complete
  batch, then end with `Ok` for CLOSED or PAUSED, with an `Err` for FAILED,
  for no footer, and for a temporary footer whose writer the registry shows
  gone. Open: a temporary footer whose writer cannot be checked (another
  machine, or no registry) -- error, or read on; and whether a discard
  (SLOT-13) should instead finish its file with a final footer and a
  status of its own, which would make a temporary footer mean only "still
  being written, or killed".
- **A forked child starts with its parent's cache byte count.** [read]
  Gap in SLOT-23. `HandleSlots.cache_bytes` is shared memory of the
  process, so a child forked while its parent caches sub-packets inherits
  the parent's count; the child replaces the inherited caches without
  subtracting it, so its room under `MORLOC_IFILE_CACHE_BYTES` stays low and
  it caches (and decodes ahead, SLOT-26) less, possibly nothing. Speed only.
- **Several slots in one process may decode the same offsets ahead after a
  reset.** [read] `reset_istream_cursor` releases only the resetting slot's
  sub-packets decoded ahead (SLOT-26); another slot of the process keeps
  its own, and a later pass may decode the same offsets again. Both copies
  are correct and counted against the bound; they hold room until the
  cursor passes them.
- **Where a stream is cut can depend on spare-buffer timing.** [read, not
  reproduced] Against SLOT-20 ("every writer cuts at the same points").
  `append_flat_run` sets the index cap to `fits.min(cap)`, `fits` taken
  from the current block's size, which depends on whether `take_spare`
  returned a smaller earlier block or a fresh full-size one, that is on
  when the custodian released it; later doubling in `append_one_element`
  then starts from that cap. Payload bytes do not depend on the cap
  (compaction removes it), but sub-packet boundaries can. Reachable by an
  `Int` stream mixing inline and big ints. Candidate fix: with nothing
  buffered, set the cap without clamping to the block. Test: write one
  stream with and without a spare available and compare file hashes.
