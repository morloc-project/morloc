# Stream writers

Written streams go through the custodian (streams.md SLOT-10..16). Changes
here touch locks and ownership: model first.

- **The nexus's stdout lock is held across a blocking write.** [read]
  morloc-nexus/src/stdio_server.rs `do_write` keeps `STDOUT_SLOT` across
  transcoding and `write(1)`. The pool no longer waits under a slot lock
  for it (the custodian's thread does), but a slow stdout reader stalls the
  stderr-free paths sharing the lock.
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
- Open question (SLOT-10): the custodian runs one thread per open written
  stream. A program holding thousands of streams open at once holds as many
  threads in the nexus; a pool of writer threads would bound it.
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
  has it closed by the per-call sweep after the reply is sent
  (daemon_ffi.rs `sweeper_enqueue_call`), off the request thread. A next
  request that opens the same path before the custodian has finished it
  fails: the custodian takes the file lock non-blocking
  (stream.rs `lock_stream_file`). Open: should an open of a path whose
  previous stream is still closing wait for that close?
