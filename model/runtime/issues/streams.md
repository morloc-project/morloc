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
  the call, the end of the run)? Ruled: decide by benchmark (a
  flush-per-element writer, many small streams) against the waiting form,
  which is implemented. If deferred, where is a custodian write error
  reported: the next operation on the stream, the end of the call, or the
  end of the run?
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
