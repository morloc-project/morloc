# Stream writers

Written streams go through the custodian (streams.md SLOT-10..16). Changes
here touch locks and ownership: model first.

- **The nexus's stdout lock is held across a blocking write.** [read]
  morloc-nexus/src/stdio_server.rs `do_write` keeps `STDOUT_SLOT` across
  transcoding and `write(1)`. The pool no longer waits under a slot lock
  for it (the custodian's thread does), but a slow stdout reader stalls the
  stderr-free paths sharing the lock, and the file's header comment (21-23)
  says the mutex is held only across the syscall and omits status 3
  (PIPE_CLOSED).
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
- **A closed reader on a `@collect` stream prints "run failed".**
  [reproduced, dev 2026-10-10] `./reads -f jsonl score reads.dat | head -3`
  prints the three values, then `Error: run failed` and `@stdout: downstream
  pipe closed` on stderr, every time; the `@render` path (`score reads.dat
  -t | head -1`) does the same. The golden `stdio-pipe-closed` holds that an
  early-closing reader leaves stderr clean, so this path (an `IStream` read
  in C++, scored by `psmap`, written under `@collect`) escapes that
  classification. Repro: web/morloc-studio/public/assets/home-examples/streams
  (`simulate 1000000` is enough); `test/check-examples.py streams` there
  fails on it.
- **A writer can write past its stream's buffer.** [read] Violates SHM
  memory safety. The buffer is allocated by its publisher with the
  publisher's `MORLOC_WRITE_BUFFER_BYTES` (the custodian, stream.rs ~3237;
  `open_stdio` ~2650; the channel opener ~10238), but every `@write` sizes
  its appends from its own process's value (stream.rs ~4010,
  `read_write_buffer_bytes_env`). A pool whose value is larger (user code
  setting the variable, or a different environment) writes beyond the
  block. Fix: record the buffer size in the slot at publication and read it
  from there.
- [read] `ready_slot_queue` gives a slot a new queue block when the old
  queue's writer has not exited yet, and the old block is never freed
  (about 1 KiB of shared memory per occurrence).
- [read] `stdout_element_count` (`@tell`) maps every error to 0, so a call
  from a pool that did not open stdout reports offset 0 silently.
- [read] `shared_close_handle`'s doc says a failed close leaves the slot
  open for a retry; `finish_stream` releases it regardless. Doc or
  behaviour must change.
