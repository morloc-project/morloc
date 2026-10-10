# Streams (SLOT)

A stream's identity lives in a shared registry slot. A process reading a
stream keeps its own descriptor, mapping and cache for it; a written
stream's file is held by its writer in the nexus (SLOT-10). A handle
carries the slot index and the slot's generation when the handle was made.

### SLOT-1 A stale handle never touches the slot it used to name
Status: implemented
Checked by: a_stale_handle_never_touches_the_slot_it_used_to_name

Every operation compares the handle's generation with the slot's under the
slot lock before changing anything.

### SLOT-2 Every sealed batch is written
Status: retired

Writers no longer hold sealed batches of their own; SLOT-13 replaces this.

### SLOT-3 A written stream's file stays locked while the stream is open
Status: implemented
Checked by: a_stream_used_by_two_threads_stays_locked

The custodian holds the lock (SLOT-10); threads of a writing process take
turns on the stream's process-local state.

### SLOT-4 A process dying inside a slot does not hang the others
Status: implemented
Checked by: a_process_dying_inside_a_stream_does_not_hang_the_rest

The slot lock detects a dead holder; the slot is then poisoned and every
later operation on it fails.

### SLOT-5 Closing a stream frees its path in every process
Status: implemented
Checked by: a_stream_closed_by_another_process_frees_its_path

### SLOT-6 A disabled cache holds nothing
Status: implemented
Checked by: a_cache_of_capacity_zero_holds_nothing

### SLOT-7 A slot lock is never held across unbounded work
Status: retired

SLOT-12 replaces this item.

### SLOT-8 A versioned read is a correct seqlock
Status: implemented
Checked by: a_read_during_a_release_never_accepts_the_cleared_slot, a_read_overlapping_a_release_reports_the_handle_stale, every_versioned_read_rechecks_through_the_fence, tla:SlotSeqlock, tla:SlotSeqlock_clear_first.bug, tla:SlotSeqlock_no_fence.bug, tla:SlotSeqlock_no_writer_fence.bug

A reader holding a handle loads the slot's generation, copies the fields
it needs, then loads the generation again, and uses the copy only if both
loads match its handle (a field that changes while the slot is open, such
as the element count, is then merely a recent value); nothing it copied is parsed,
opened or followed before that. A pointer and its length are copied
together through a bounds check, so a torn pair is an error rather than a
read past the mapping. An open OStream's entry array and compression level
change under the slot lock and are never read this way. A reader with no
handle (a sweep's scan for slots to end) checks again under the
lock. An acquire load does not keep earlier reads
before it, so the second load follows an acquire fence; on a weakly ordered
processor the reads could otherwise complete after it. A writer moves the
generation before it changes any field or frees any block: releasing a slot
bumps the generation first and fences, so its clearing stores cannot be
seen ahead of the bump, and a reader that overlaps the release fails its
second load instead of accepting cleared fields. Opening publishes fields
then the new generation. Every field such a read copies is an atomic,
loaded and stored with relaxed ordering, so a copy that races a write is a
stale value rather than undefined behaviour; the fences above give the
order. The slot's diagnostics record is not atomic and is touched only
under the slot lock. Model: `tla/SlotSeqlock.tla`.

### SLOT-9 A process drops its slot for a stream that has ended
Status: implemented
Checked by: a_readers_slot_for_a_stream_another_process_closed_is_released, tla:LocalSweep, tla:LocalSweep_lock_holders_only.bug, tla:LocalSweep_drop_in_use.bug, tla:LocalSweep_reinstall_stale.bug

A process keeps a local slot per stream it uses: a mapping, a cache and
write buffers for one generation of the stream. Another process may end the
stream and never touch it again from here, so waiting for this process's
next use would keep the slot for the life of the process. Every release of
a registry slot advances the doorbell's count without waking anyone. At a
dispatch end when the count has moved since the process last looked, and
before a worker decides whether it can retire, a pass that never waits
drops every local slot not taken out by a thread whose generation is no
longer its stream's, and is skipped and retried later if a pass is already
running. A slot given back after its stream ended is dropped rather than
kept. No local slot holds a stream file's lock (SLOT-10). Model:
`tla/LocalSweep.tla`.

## Written streams: the custodian

Ruled 2026-10-10 (design: /work/plans/streams-single-writer/DESIGN.md).
Every process appends a written stream's elements to the slot's shared
write buffer under the slot lock. A full buffer, a flush and a close move
the buffer onto the stream's queue, which has its own lock; the nexus's
custodian takes batches off the queue in order, compresses, writes and
finalizes. Stdout and stderr streams use the same queue, with the nexus's
stdout writer as their consumer. Model: `tla/StreamQueue.tla` (order,
contiguous writes, a closed file holds every write, a reader waiting per
SLOT-15 sees a finished file, every stream ends; five broken variants).

### SLOT-10 Only the custodian touches a written stream's file
Status: implemented
Checked by: a_replaced_output_file_is_not_written, an_open_output_stream_does_not_keep_its_opener_from_retiring, a_stream_used_by_two_threads_stays_locked, tla:StreamQueue

No pool opens, locks, writes or finalizes the file of a stream it writes:
`@open` and `@append` ask the custodian, which opens and locks the file,
publishes the slot, and is the stream's only writer. It holds the file it
opened, so a file put in its path meanwhile is never written.

### SLOT-11 A stream's elements are in the order their writes took the slot lock
Status: implemented
Checked by: writes_from_two_processes_keep_each_write_whole, tla:StreamQueue

One write's elements are contiguous; writes from different threads and
processes interleave only between writes.

### SLOT-12 A slot-lock holder waits only on the custodian
Status: implemented
Checked by: outstanding_batches_never_exceed_the_queue_depth, a_full_queue_makes_the_producer_wait_until_the_custodian_takes_one, tla:StreamQueue, tla:StreamQueue_pop_locks.bug, tla:StreamQueue_enqueue_unlocked.bug

Under a slot lock a pool copies elements, moves SHM pointers and, when the
queue is full, waits for the custodian to make room. The queue holds at most
its depth of unfinished batches (`MORLOC_WRITE_BEHIND_DEPTH`, at most 7; 1
for an uncompressed stream), and a writer waits for room before reusing a
buffer, so a stream's shared memory does not grow with its length. The custodian takes no slot lock and waits on
no pool, so no wait cycle passes through a slot.

### SLOT-13 A returned write is written when its stream is flushed or closed
Status: implemented
Checked by: a_forked_writer_that_exits_without_flushing_loses_nothing, a_discarded_stream_keeps_what_was_queued_and_no_final_footer, a_stopped_writer_finishes_its_file_with_the_given_status, tla:StreamQueue_writer_frees.bug

A writer process that exits or dies after its write returned loses
nothing: its elements are in the buffer or the queue, in SHM, and are
written when any process flushes or closes the stream. A stream nobody
closes is discarded when its handle is dropped or the run ends: what was
queued is written, the unflushed elements are dropped, and the file keeps
its temporary footer. A run that ends abnormally loses what was queued: a
CLI run fails on a pool death, and daemon recovery stops the custodian for
the old namespace, which finishes each file with a failed-status footer
from what it has written, before unmapping (DAEMON-1).

### SLOT-14 A writer dying inside the slot lock fails the stream
Status: implemented
Checked by: a_writer_killed_inside_the_slot_lock_fails_the_stream

The slot is poisoned (SLOT-4): its close reports the death, batches queued
before it are still written, and the footer records the failure.

### SLOT-15 Close and flush return once their batches are in the file
Status: implemented
Checked by: a_flush_makes_its_elements_readable_from_the_open_file, a_closed_stream_refuses_writes_and_its_path_reopens_at_once, a_discarded_stream_keeps_what_was_queued_and_no_final_footer, tla:StreamQueue_reader_no_wait.bug, tla:StreamQueue_close_before_tail.bug

A close queues the unflushed elements and a close marker and waits for the
custodian to write them and the footer; writes after it fail, and its path
reopens at once. A flush waits likewise, so a reader of the open file sees
what was flushed. A discard writes what was queued, drops the unflushed
elements, and leaves the temporary footer. Whether close and flush should
instead return at once, with the wait moved to readers, waits on a
benchmark (issues/streams.md).

### SLOT-16 Stdout and stderr streams go through the queue
Status: implemented
Checked by: a_stdout_stream_reaches_the_nexus_before_its_close_returns

The nexus's stdout and stderr writer is the custodian's consumer. A pool
finishes the stdio streams a call left open before replying, so a call's
output precedes its result. A write returns once its batches are queued,
not written; text a pool prints to the shared stdout meanwhile may land
inside a batch (issues/streams.md).
