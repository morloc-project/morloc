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
- **Pool prints reach stdout out of call order.** [reproduced, 2026-10-28]
  Gap: no item orders raw text that pools print. In
  /work/tests/sumtypes/04-graphics-csg-sdf/core, `prog-cpp test` (unittest
  harness sourced in Python) prints each `printGroup` header after the PASS
  lines of its group, though the header's result feeds those checks; with
  `PYTHONUNBUFFERED=1` the order is right, so two writers flush separately
  buffered stdout. Which second writer (a forked Python worker is the
  suspect) is speculative.
- Open question (`@tell`): intrinsics.md gives `@tell` no failure value and
  it returns 0 when this process has no `@stdout` open, so a pool other
  than the one that opened the run's stdout reports 0 while elements have
  been written. Should `@tell` count the run's stdout from any pool?
- **A path left open by a daemon call may refuse to reopen in the next
  call.** [read] Gap in SLOT-15 ("its path reopens at once" covers only a
  user close). A nexus-evaluated daemon request that leaves an OStream open
  has it closed by the call sweep after the reply is sent
  (stream.rs `sweep_call`), which does not wait for the writer
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
  @next: stdin ended after 23 byte(s) ...". [read] rustmorloc lib.rs:3864
  wraps in `format!("@next: {}", ...)` an error that stream.rs:2820-2821
  already formats as "@next: {}". The C++, Python and R wrappers are
  unchecked.
- **The daemon's remote path sets no call id and runs no sweep.** [read]
  Against SLOT-19. data/rust/morloc-runtime/src/daemon_ffi.rs:2545-2546 and
  2638-2639 set and sweep a call id on the pure path only; the remote
  branch (2640 on) does neither, and stream.rs:2500-2502 claims "the nexus
  dispatch always pre-sets a call_id", which holds only there.
- **An IStream cannot tell a stream still being written from an abandoned
  one.** [read] Spec: SLOT-29, SLOT-31 (open). FAILED files are refused and
  a pipe that ends without a final footer fails (SLOT-29, fixed
  2026-11-20), but an IStream of a file with no footer or a temporary
  footer still reads its complete batches and ends cleanly, because both
  states also occur while a writer is live: a file just opened has no
  footer until its first batch, and the custodian overwrites its last
  footer with each new batch. So a stream file cut short, or one whose
  writer was killed or discarded it, still reads as complete through an
  IStream and `@load` (IFile and `file --validate` refuse it).
  lib/biolib rnaseq R23 hit it, and keeps the check in a separate
  `knownIssues` export.
  Needs a TLA+ model before any fix (tla/, beside StreamQueue.tla): a
  writer (writing, flushed, closed, paused, discarded, failed, killed at
  any step), the custodian's file states (SLOT-28's six, plus a
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
- **A raised stream depth reaches a waiting producer only at its wait
  slice.** [read] Against SLOT-22's discipline (a waker moves the word it
  wakes on). When streams end, `CustodyQueue::set_cap` (custody.rs) raises
  the depth caps of SLOT-32 and calls `wake_all(&done)` without moving
  `done`, so a producer that compared under the old cap and is about to
  sleep misses the wake and waits up to `WAIT_SLICE` (100 ms). Speed only.
  A fix moves a word the producer sleeps on: a per-queue room word bumped
  by both `finish` and `set_cap`, which changes the QueueWake model.
- **The stream queue bound has no setting.** [read] Open ruling, SLOT-32.
  `write_behind::queue_bytes` is a fixed 1 GiB. A setting would be one more
  environment read, and source_rules.rs caps those (`MAX_ENV_READS`, 65)
  as a ratchet; adding one needs that cap raised, or an existing read
  folded into a shared settings read.
- **The SLOT-32 bound is per writing process, not per host.** [read] Gap in
  SLOT-32. The depths come from the writers one process hosts
  (`HOSTED`, custody.rs), so each process that hosts stream writers (the
  nexus of each concurrent run, a daemon) shares its own 1 GiB, and a host
  running k of them may queue k GiB; and an adopted stdout or stderr counts as a share until
  its first batch shows it uncompressed. Whether the bound should be
  host-wide (a shared-memory counter across runs) is undecided.

- **Every daemon call sweeps the whole stream registry.** [read 2026-10-15]
  Gap in SLOT-9. After each local call the daemon runs `stream::sweep_call`,
  which loads `state` in every registry slot (4096 slots of 512 bytes by
  default, up to 65536: 2-32 MiB of shared memory touched) to close streams
  the call left open, even when the call opened none. The call could record
  the slots it opens and sweep only those (a shared list where a custodian
  or another process opens for it).
- **Per-operation scans over growing process lists.** [read 2026-10-15, from
  a code survey; not measured] `stream.rs` `allocate_slot_cas_for` probes
  linearly per open and degrades as the registry fills;
  `cache_get_or_materialize` searches its entries per IFile index hit;
  `drop_ended_slots` walks every attached handle per doorbell ring and per
  fork-pool retire check; `unlock_and_close` filters every locked
  descriptor per close; `custody::shares` revisits every hosted writer per
  open, idle and end (quadratic in streams opened); `fold.rs` scans every
  thread's slot per fold put; `intrinsics.rs` scans every temp file per `@close` and per
  dispatch end; `arrow_shm.rs` `try_borrow` walks every live view per Arrow
  table returned and `borrow_forget` per view released (quadratic to
  release them all); `daemon_ffi.rs` counts remote jobs in the whole queue
  per accept. Each is bounded in practice today; an index (map or free
  stack) would make each O(1).
- **SLOT-28's table misstates where a PAUSED footer comes from.** [read,
  2026-10-21] The code writes a PAUSED footer (unflushed elements first)
  from the daemon call sweep (stream.rs:6481, `sweep_matching`), a reopen
  after the opener died (:3098, `finish_dead_openers_of`), a stdio claim
  whose owner died (:2386, `try_reclaim_stale_stdio_claim`),
  `pool_reclaim_stdio_after_dispatch` (:6517), and stdout/stderr at the end
  of a successful run (morloc-nexus main.rs:76-83, `custody_finish`). The
  table's one source,
  "a remote child returns the stream without @close", has no code path. So
  a stream left open by a daemon call ends PAUSED with its elements
  written, while at the end of a CLI run it is discarded (SLOT-13); SLOT-13
  and SLOT-19 give two endings without saying so.
- **Idle slots per handle are capped at 16.** [read, 2026-10-21] Violates
  SLOT-23 (one per processor): stream.rs:1091 `idle_slots_kept` clamps to
  16. Speed only.
- **SLOT-29 is silent on a present but corrupt footer.** [read,
  2026-10-21] stream_format.rs `parse_stream_file` records a damaged
  footer, or a final footer whose sub-packet index fails
  `check_subpacket_index`, as `footer_damaged` and falls back to a forward
  scan (:116-130); `@open` as an IFile then refuses it as "not cleanly
  closed (no final footer)" (stream.rs:2170-2175), without the reason.
  Belongs with the SLOT-31 ruling. `MORLOC_WRITE_BEHIND_DEPTH=0` is
  clamped to 1 silently (custody.rs:193).
- **The index walk reads every sub-packet.** [read, 2026-11-01] `@concat`,
  `@append`, and `@open` as an IFile in a debug build or with
  `MORLOC_VERIFY_INDEX=1` (SLOT-30), check a final footer's index against
  the sub-packets (stream_format.rs `verified_index`), at O(sub-packets):
  one header page each, and for a zstd sub-packet a fresh decoder
  (compression.rs `subpacket_elem_count`) that decodes up to a 128 KiB
  block for 8 bytes (estimated 50-100 us each, not measured). Candidates:
  reuse one decoder; a writer that ends the first block after the array
  header; a cache of verified (dev, ino, size, ctime). Unmeasured.
- **A footerless compressed stream's scanned element count is short.**
  [read, 2026-11-01] `parse_stream_file`'s fallback takes `element_count`
  from `forward_scan_subpackets`, which counts compressed sub-packets only
  in `compressed_uncounted`. Readers of that count are not traced.
- **A stdin batch block may be held until teardown.** [read, not
  reproduced, 2026-11-13] If the nexus fails to write its response
  (morloc-nexus stdio_server.rs ~311, ~455-460) or the pool fails reading
  the body (stream.rs ~2803), the batch block it was handed is not
  released until the shared memory is torn down.
- **R stream decoders leak their block when decoding raises.** [read
  2026-11-13] rmorloc.c morloc_mlc_next (3090), mlc_ifile_walk (3019),
  mlc_next_frame (3117) and mlc_stream_layout (3142) call from_voidstar
  without the shm_tracker that morloc_mlc_load uses (2809): an R
  error while decoding (an R allocation failure, R_TRY_READ outside
  runtime reads) leaks the SHM block and the parsed Schema;
  mlc_next_frame also leaks the block when parse_schema fails after the
  read. Violates SHM-4 (a reference is released once, by its holder): here
  it is never released.
  morloc_mlc_side_next (rmorloc.c ~3595, the fold side replay) has the
  same shape: an R error in from_voidstar longjmps past its shfree.
- **R `@next` and `@streamLayout` leak their parsed schema when the runtime
  call fails.** [read, 2026-11-14] rmorloc.c morloc_mlc_next (3099-3100)
  and morloc_mlc_stream_layout (3151-3152) parse the schema, then call
  `R_TRY(mlc_next, ...)` / `R_TRY(mlc_stream_layout, ...)`; `R_TRY`
  (rmorloc.c:113-117) raises the R error without freeing, so every failed
  `@next` (end of a bad stdin, a closed handle) leaks one malloc'd Schema.
  Parsing after the call, or `R_TRY_WITH(free_schema(schema), ...)`, frees
  it.

- **A second `@stderr` stream in one run loses the first.** [reproduced,
  2026-11-18] Gap: no item says what several stdio opens in one run make
  (SLOT-16 covers the queue only). In one-shot mode the stderr branch of
  morloc-nexus/src/stdio_server.rs (~716-724) never resets `header_done`;
  only stdout does, in `finish_logical_packet` (~1025). A command that
  opens `@stderr :: OStream Int`, writes `[1, 2]`, closes, then opens it
  again and writes `[3]` (`./nexus -f packet @ twoErr 2> err.pkt`) leaves a
  file that `morloc-nexus file --validate` reads as final, closed, one
  sub-packet, one element: the first stream's two elements are silently
  dropped. Readers disagree: `@load` of the same file gives `[1,2]`, so the
  second stream is what is lost there. The same shape on `@stdout` reads
  back as two sub-packets, three elements (`[1,2,3]`). Repro (root-py, no sources):
  `twoErr :: <IO> ()`, `twoErr = do { e <- @stderr :: <IO> OStream Int;
  @write 0 e [1, 2]; @close e; e2 <- @stderr :: <IO> OStream Int;
  @write 0 e2 [3]; @close e2 }`.
- **Pool text shares fd 2 (and fd 1) with typed stdio streams.** [read,
  2026-11-19] Gap: no item says who owns the process's stdio fds. Pools
  inherit fd 1 and fd 2 and write raw text there (prints, tracebacks,
  crash messages), while a typed `@stderr` (or `@stdout`) stream writes
  packet frames to the same fd, so the text can land inside the packet
  stream. Options weighed: (i) the nexus owns fd 1 and fd 2 and pool text
  goes through a pipe it keeps separate (one extra copy; crash text must
  still get out when the nexus is stuck); (ii) forbid typed `@stderr`;
  (iii) allow typed `@stderr` only with a guarantee no text reaches fd 2.
  The user leans to (i) but deferred it 2026-11-19: it needs a model
  first. The second-`@stderr` entry above belongs with it.
- **A `-f json` stdout stream left open at the end of a run exits 0.**
  [read 2026-11-20, code review] SLOT-40 covers only packet output:
  `end_stdout` (morloc-nexus stdio_server.rs) checks for an unfinished
  stream only for the packet and voidstar formats and returns `Ok` for
  every other, so a JSON stream whose footer never arrived (`json_open`
  still set) leaves `[1,2` on stdout and the run exits 0, where the packet
  run fails. Test `a_stdout_stream_still_open_at_the_end_is_failed` uses
  packet output only.
- **A stop can miss an idle writer's sleep and wait out its 100 ms slice.**
  [read 2026-11-22] Deviates from SLOT-22 ("Stopping ... wake
  unconditionally"). `signal_stop` (custody.rs) wakes the push count, but
  the sleeper's futex in `wait_pushed` compares the push count, which a
  stop does not move, so a wake sent between the writer's stop check and
  its futex call is lost; the writer sleeps up to `WAIT_SLICE` (100 ms)
  before `host_stop_all` can join it. Only latency at teardown; no test or
  model covers the window. A fix makes the writer sleep on a word the
  stopper moves (a stop/fail epoch bumped with the push count): a
  QueueWake protocol change, so model first.
- **A stop that arrives after a closer died leaves the slot until unmap.**
  [read 2026-11-22] In `await_release`, if the closer has died and a stop
  is seen before the next `closer_gone` check, the writer returns without
  releasing the slot, and nothing frees it until the segment is unmapped.
  Every Rust stop path found (`stop_before_unmap`, test `host_stop`)
  unmaps or releases afterwards; the C callers of
  `morloc_custody_stop_all` are not traced. SlotRelease.tla has no stop
  action, so the model does not cover this.
