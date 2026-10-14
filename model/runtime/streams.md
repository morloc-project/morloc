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
order. Model: `tla/SlotSeqlock.tla`.

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
Checked by: outstanding_batches_never_exceed_the_queue_depth, a_full_queue_makes_the_producer_wait_until_the_custodian_takes_one, a_writer_fills_the_buffer_its_publisher_made_whatever_its_own_setting, tla:StreamQueue, tla:StreamQueue_pop_locks.bug, tla:StreamQueue_enqueue_unlocked.bug

Under a slot lock a pool copies elements, moves SHM pointers and, when the
queue is full, waits for the custodian to make room. The queue holds at most
its depth of unfinished batches (`MORLOC_WRITE_BEHIND_DEPTH`, at most 7; 1
for an uncompressed stream), and a writer waits for room before reusing a
buffer, so a stream's shared memory does not grow with its length. A
writer fills a buffer up to the sub-packet size its publisher fixed
(SLOT-20), whatever its own process's setting. The
custodian takes no slot lock and waits on no pool, so no wait cycle passes
through a slot.

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

### SLOT-17 Every process uses the slot count the registry was published with
Status: implemented
Checked by: every_process_takes_the_slot_count_the_registry_was_published_with

The process that creates the registry sizes it from its own setting
(`MORLOC_REGISTRY_SLOT_COUNT`) and publishes the count in the registry's
header. Every process that attaches maps the registry as it was made and
uses the published count, whatever its own setting, so a handle names the
same slot in every process; one whose mapping does not cover the count
refuses to attach.

### SLOT-18 A queue block is freed by exactly one of its slot and its last writer
Status: implemented
Checked by: an_opener_reuses_a_queue_whose_writer_has_left, the_writer_leaving_frees_a_queue_its_slot_gave_up, the_writer_leaving_keeps_a_queue_its_slot_still_uses, tla:QueueHandoff, tla:QueueHandoff_flag_and_pointer.bug, tla:QueueHandoff_no_recheck.bug

A slot keeps its queue block from one stream to the next. When a stream is
opened while the custodian's writer for the slot's previous stream has not
yet left, the slot takes a new queue and the old block goes to that
writer: one word on the queue holds a consumer flag, cleared by the writer
as it leaves, and an abandoned flag, set by the opener under the slot lock.
The writer frees the queue, with its spare buffers, if it finds the
abandoned flag set; an opener that sets the flag and finds the consumer
already gone clears it again and keeps the queue. So the block is freed
once, never while the slot uses it, and never leaked.

### SLOT-19 A queued close frees its slot once, after the footer
Status: implemented
Checked by: only_a_detached_or_dead_closer_leaves_the_slot_to_the_custodian, a_call_sweep_pauses_the_calls_streams_and_their_writer_frees_the_slots, tla:SlotRelease, tla:SlotRelease_detached_releases.bug, tla:SlotRelease_detached_waits.bug

A close names its closer on the queue. A waiting closer (`@close`, a
flush-and-close at the end of a run) frees the slot once the custodian has
written the footer (SLOT-15). The sweep that ends a nexus-evaluated
daemon call closes each stream the call left open detached: it queues the
close and returns, and the custodian frees the slot after writing the
footer, as it does for a waiting closer that died. Every free checks under
the slot lock that the slot still holds the stream being closed.

### SLOT-20 A write buffer grows to its stream's sub-packet size
Status: implemented
Checked by: a_write_buffer_starts_small_and_sub_packets_keep_their_size, written_elements_do_not_depend_on_a_reused_buffer, flattening_in_place_writes_the_flat_form_moved

The size a stream's sub-packets are cut at is fixed when the stream opens
and kept in its slot, so every writer cuts at the same points. The write
buffer's block starts small and, under the slot lock, is replaced by one at
least twice as large, up to that size, when an append needs more room; its
bytes move with it, since offsets inside a buffer are relative to it. A
refill takes a block of the size the previous buffer reached. An element is
written into the buffer at its place, every byte of the room it takes
defined, so nothing a reused buffer held before reaches a file.

### SLOT-21 A channel's reader and writer sleep until the channel changes
Status: implemented
Checked by: a_waiting_reader_wakes_when_a_batch_arrives, a_full_channel_holds_its_producer_until_a_reader_takes_a_batch, tla:ChannelWait, tla:ChannelWait_read_after_unlock.bug, tla:ChannelWait_no_bump.bug, tla:ChannelWait_no_register.bug

Every change to a channel (a batch queued or taken, its end or failure,
its release) is made under the slot lock and bumps the channel's sequence
word, and wakes the sleepers when any are registered. A reader with
nothing to take, or a writer with no room, registers and reads the word
under the lock, releases the lock and sleeps only while the word still
holds what it read, so a change made after the release ends the sleep at
once. It deregisters under the lock at its next look, which comes at
least every 100 ms, as a released slot's word may be reused.

### SLOT-22 A stream's writer and custodian wake each other only when one sleeps
Status: implemented
Checked by: a_full_queue_makes_the_producer_wait_until_the_custodian_takes_one, a_waiter_sees_its_entry_done_and_any_failure, tla:QueueWake, tla:QueueWake_check_before_bump.bug, tla:QueueWake_register_after_compare.bug

The custody queue takes no lock. A side about to sleep on a queue word
(the push count, for the custodian; the done count, for a writer or a
closer) reads the word, registers on its waiter count, and sleeps only
while the word still holds what it read. A side that moves a word moves it
first and then wakes its sleepers only when the count is non-zero; both
orders are sequentially consistent, so either the mover sees the
registration or the sleeper sees the moved word. Stopping and failing a
stream wake unconditionally.

### SLOT-23 Threads reading one file share its slots and its cache bound
Status: implemented
Checked by: threads_reading_one_file_share_its_cache_bound, threads_read_one_input_file_at_once, readers_sharing_a_cursor_read_each_sub_packet_once

A process keeps up to as many idle slots per handle as it has processors;
an operation takes one, or attaches another when all are out, and puts it
back, so threads reading one file each find a warm mapping and cache, and
never wait for each other. The decompressed sub-packets cached by all of
a handle's slots in the process stay within `MORLOC_IFILE_CACHE_BYTES`:
a slot evicts its own entries to make room, and serves a sub-packet
uncached when the handle has no room for it.


### SLOT-24 A batch's block outlives every compression job that reads it
Status: implemented
Checked by: compressed_subpackets_are_cut_into_frames_fixed_by_the_level, frames_of_a_payload_join_in_order_and_settle_returns

The custodian hands a compressed batch to the compression pool as one
split job, which compacts the block and submits a job per frame (ZIP-9).
The custodian releases the block only after the split job has finished
and every frame job has finished or was withdrawn before it started,
whatever any of them returned (a job that panics ends the process,
PANIC-1). No compression job waits on another, so the pool cannot deadlock however
few threads it has, and with none at all a job runs on the thread that
submits it.

### SLOT-25 A reader decodes its batch inside its claim
Status: implemented
Checked by: a_batch_decoded_in_place_is_the_batch_next_returns, a_decoder_that_fails_still_releases_its_claim, golden:next-decode-failure

`@next` from a C++ or Rust pool claims the next sub-packet as SLOT-23's
readers do and hands it to the pool's decoder while the process keeps the
claimed slot, with no slot lock held: an uncompressed sub-packet aligned
in the file is read where it is mapped, with its offsets relative to its
payload; any other is decompressed or copied into one shared-memory block
first, freed when the decoder returns. Nothing the decoder is given
outlives the call, since the slot's mapping may be replaced once it is
released (pages behind the cursor are dropped, idle mappings unmapped). A
decoder reports failure by return value, never by unwinding through the
runtime, and the claim is released either way.

### SLOT-26 A reader decodes ahead of its claim without claiming
Status: implemented
Checked by: compressed_sub_packets_decoded_ahead_are_read_in_order, readers_sharing_a_cursor_read_each_sub_packet_once, an_arena_ending_leaves_the_sub_packets_decoded_ahead, a_disabled_cache_decodes_nothing_ahead, a_reset_cursor_reads_the_stream_again

A reader whose previous claim ended where its new one starts (no other
reader claimed in between, so the sub-packets after are likely its own)
decodes the compressed sub-packets that follow its claim in the file, up
to the end of the data its claim saw under the slot lock, and keeps them in
its slot's cache by file offset (SLOT-27 says how). It claims nothing more:
the cursor moves only by claims. A later claim of a kept offset is served
from the cache; a kept sub-packet at an offset before a claim was claimed
by another reader and is released unused; resetting the cursor releases
them all. File bytes at an offset do not change while the stream is open
(the custodian writes only past the published end, SLOT-10, SLOT-15), so a
kept sub-packet is the one a claim would decode. Each block is reserved in
the handle's bound (SLOT-23) before it is decoded, and a cache of capacity
0 keeps nothing (SLOT-6). Kept blocks are the cache's references (SHM-7):
none is recorded in an arena while kept, and a served one is recorded as a
fresh block is. Speculation never fails a claim: a block that cannot be
reserved or allocated ends the pass, and a sub-packet that does not decode
is simply not kept.

### SLOT-27 Decoding ahead runs beside the reader and is joined before use
Status: implemented
Checked by: compressed_sub_packets_decoded_ahead_are_read_in_order, batches_read_in_place_beside_other_allocations_read_back_as_written, a_reset_cursor_reads_the_stream_again, tla:AheadJob, tla:AheadJob_drop_without_join.bug, tla:AheadJob_serve_before_join.bug

After a claim, a slot that holds less than a pass of sub-packets decoded
ahead starts one job on a thread of its own for the next pass. A pass
takes sub-packets until it holds twice as many frames as frame workers or
8 MiB, so small sub-packets decode many at once and large ones, whose
frames already decode in parallel, one at a time beside the reader. The job allocates the pass's blocks (or takes
blocks the slot handed it), decodes their frames on the frame workers,
relocates them, and frees nothing; it reads the compressed bytes where the
slot maps them, past the claim that started it. The slot joins the job
before it serves, keeps, releases or drops any of its blocks: at its next
claim, at a cursor reset, and before it unmaps its file. Pages a reader
drops lie before the end of its claim, so never under a running job. A
block whose batch the pool decoded in place (SLOT-25) is the caller's
alone once the decoder returns; while the slot decodes ahead it keeps up
to four such blocks, counted in the bound, as landings for later passes
instead of releasing them, and releases them when its claims stop
following one another, at a cursor reset and when it is dropped. A kept
block lands only a batch within an eighth (at least 64 KiB) of its size,
and the bytes past the batch are cleared before it is used, so no earlier
batch shows through it (SHM-6). The slot's byte counter is swapped to its
handle's (SLOT-23) only after the slot's pass is joined, so a pass's
reservations settle where they were made. A job that panics leaves the
blocks it allocated where the slot releases them. Before the stream
registry is unmapped, the passes of every idle slot are joined and their
blocks released. Joining waits for one pass at most: dropping or resetting
a slot, which joins, waits that long. Model: `tla/AheadJob.tla`.
