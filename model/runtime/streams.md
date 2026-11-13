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
for an uncompressed stream; lowered as more streams are written, SLOT-32,
so a batch queued before it was lowered may stand past it), and a writer
waits for room before reusing a buffer, so a stream's shared memory does not grow with its length. A
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
Checked by: a_flush_makes_its_elements_readable_from_the_open_file, a_closed_stream_refuses_writes_and_its_path_reopens_at_once, a_closed_file_is_synced_unless_it_is_a_call_temp, a_file_under_the_run_temp_root_is_not_synced_whoever_made_it, a_discarded_stream_keeps_what_was_queued_and_no_final_footer, tla:StreamQueue_reader_no_wait.bug, tla:StreamQueue_close_before_tail.bug

A close queues the unflushed elements and a close marker and waits for the
custodian to write them and the footer; writes after it fail, and its path
reopens at once. A flush waits likewise, so a reader of the open file sees
what was flushed. A close also makes the file's data durable (fdatasync),
except a file `@tmpfile` made in the writing process, which its call
removes, or any file under the run's temp root, which the run removes
(the custodian is often another process than the one that made it); an error closing the file (a filesystem that writes back at
close) fails the stream. A discard writes what was queued, drops the unflushed elements,
and leaves the temporary footer. Whether close and flush should
instead return at once, with the wait moved to readers, waits on a
benchmark (issues/streams.md).

### SLOT-16 Stdout and stderr streams go through the queue
Status: implemented
Checked by: a_stdout_stream_reaches_the_nexus_before_its_close_returns, a_stdout_sub_packet_reaches_the_sink_in_parts_that_make_the_packet

The nexus's stdout and stderr writer is the custodian's consumer. A
sub-packet reaches it as its header block, payload and padding, which it
writes in order without joining them. A pool
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
Checked by: a_write_buffer_starts_small_and_sub_packets_keep_their_size, written_elements_do_not_depend_on_a_reused_buffer, flattening_in_place_writes_the_flat_form_moved, where_a_batch_is_cut_does_not_depend_on_the_buffer_block

The size a stream's sub-packets are cut at is fixed when the stream opens
and kept in its slot, so every writer cuts at the same points. The write
buffer's block starts small and, under the slot lock, is replaced by one at
least twice as large, up to that size, when an append needs more room; its
bytes move with it, since offsets inside a buffer are relative to it. A
refill takes a block of the size the previous buffer reached. An element is
written into the buffer at its place, every byte of the room it takes
defined, so nothing a reused buffer held before reaches a file. Where a
sub-packet is cut never depends on the size of the block the buffer holds.

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


### SLOT-33 A stdout batch is compressed only when the output is packets
Status: implemented
Checked by: a_stdout_stream_takes_the_level_the_nexus_recorded, golden:stdout-stream-compression, golden:stdout-raw-bodies

Before it dispatches a call, the nexus records in the stream registry the
level stdout batches are written at, for the form it writes them in: for
output whose frames it
forwards (packets, voidstar), its `-z` level, or none, which leaves the
`@write` level; for every other form, which it decodes before writing,
level 0, so `@write` levels on `@stdout` need not agree there. A pool
writing to `@stdout` takes that level; `@stderr` and file streams keep
theirs. Each run has its own registry, so a nested run takes nothing from
an enclosing one. A daemon records none: it refuses `@stdout`.

### SLOT-34 A slice caches only the sub-packets at its ends
Status: implemented
Checked by: a_slice_caches_only_the_sub_packets_at_its_ends

An IFile slice reads the sub-packets it spans from the handle's cache when
they are there. Of those it decompresses, it keeps in the cache (SLOT-23)
only the first and the last, which a neighbouring slice may read again;
the others are freed when the slice returns. A slice across many
sub-packets therefore leaves at most two of them cached.

### SLOT-38 A strided or field-tail slice holds one sub-packet at a time
Status: implemented
Checked by: a_strided_slice_is_the_slice_copied_element_by_element, a_strided_slice_releases_each_sub_packet_it_has_passed

An IFile slice with a step other than 1, or with a field tail, takes one
run from each sub-packet it spans: the run's first element and count
follow arithmetically from the cumulative counts, one binary search per
run. It copies a run before it reads the next sub-packet, and once the
walk has passed a sub-packet, a decompressed copy that SLOT-34 does not
keep is freed and the pages of an uncompressed interior one are dropped
from the process's mapping. A fixed-width result (a fixed-width element,
or a field tail to one) is copied into a block sized once, by a strided
copy of each run. A Str result is sized by a first pass over the runs and
copied by a second, which decompresses each interior compressed
sub-packet twice; a second pass that finds other strings fails the slice.
Any other result is deep-copied element by element and consolidated into
one block. Every route gives the value the slice selects, and a fixed-width
result its bytes.

### SLOT-35 A `@write` batch that fails leaves no trace, or fails its stream
Status: implemented
Checked by: a_batch_with_a_failing_element_leaves_no_trace, a_batch_failing_in_placement_is_absent_or_fails_the_stream, the_first_failure_of_a_queue_is_the_one_reported

Under the slot lock and before the stream's level is pinned, `@write` to
an output stream or a channel resolves the path of every handle-form
stream field in its batch; the elements are then written with those
paths. A field naming a stream that is closed at that point, or one with
no path (a channel, stdio), fails the call before any element is
appended: no element of the batch reaches the file or a reader, the
stream's counts and level are those before the call, and later batches'
elements are written as if the call had not been made (an index capacity
the call grew stays, so later sub-packets may be cut elsewhere; cuts carry
no meaning, SLOT-11). Any other failure while
the batch is placed is undone the same way so long as none of its elements
has been handed on (queued for the file or pushed to a reader): what it
placed is uncounted. A buffer handed on that holds only
earlier batches' elements does not count. The batch's elements are counted
only once all are placed, and the level is pinned once the batch has
handed bytes on, or when it ends. A failure after a hand-off fails the
stream instead: the error
is returned, the batch's unhanded bytes are dropped, later writes are
refused with the failure, an output stream keeps its temporary footer
(SLOT-28) and a channel's reader gets the failure after what was handed
on. When the writer and the custodian fail a stream at once, the first
failure recorded is the one reported. A relptr of the write buffer's own bytes
that will not shift when its index grows is a defect: the writer panics
inside the slot lock, which fails the stream (SLOT-14). What was
handed on stays in the file, where a reader reads it as data (SLOT-29).
A failure that ends the process (SHM-17) is outside this item.

### SLOT-36 A batch is encoded in place and committed whole
Status: implemented
Checked by: an_in_place_batch_writes_the_bytes_the_copy_path_writes, in_place_runs_read_as_zero_and_mix_with_single_rooms, an_in_place_batch_that_fails_is_absent_or_fails_its_stream, a_thread_encoding_in_place_does_no_other_stream_operation, an_arena_rewound_inside_an_in_place_batch_keeps_its_blocks, an_in_place_element_that_ends_outside_its_room_poisons_the_stream, two_threads_writing_in_place_leave_their_batches_whole, an_in_place_batch_does_not_survive_a_fork_or_its_thread, an_arena_ended_inside_an_in_place_batch_discards_its_streams_after, a_run_of_elements_wider_than_a_sub_packet_is_placed_one_by_one, a_batch_left_open_at_the_end_of_its_call_is_aborted

A language binding may write a `@write` batch to an output stream (a file,
or an unstaged standard output or error) by
encoding each element straight into the stream's write buffer, inside one
transaction on the slot lock, which it begins, places elements into, and
commits or aborts. Channels, staged streams, and element types that are
tables or hold stream fields are refused at begin and written as SLOT-35
says, from the binding's value. An element is placed with exactly its
portable size and is cut into sub-packets as the copy path cuts it, so the
file's bytes are those the runtime's own encoding writes. Its room is
zeroed when it is placed, so a field the encoder leaves alone reads as
zero. The binding reports where its encoding ended at its next call; an
end outside the room is a defect. A room may end past the encoding, by
the size's alignment slack, and that tail stays zero. An `Int` placed as
its slot alone, in a run or a room of that size, that is not written
inline is a defect too.

A buffer that fills is sealed into the writer's pending list and, once the
queue has room, a spare or fresh block takes the slot's place; a buffer
that fills before the batch has placed anything in it holds earlier
batches' elements alone and is handed on at once, as outside a batch, so
every pending block holds some of the batch's own. An element larger than
a sub-packet gets a pending block of its own. Every block the transaction allocates is
the slot's from the start, so no eval arena releases it. Pending blocks
are queued (SLOT-12) and the element counts and the level pin change only
at commit, so an aborted batch leaves no trace and pins no level. Growing
the block or its index is published to the slot at once and survives an
abort. Abort frees the writer's private blocks, never giving them to the
spare ring, whose only producer is the custodian, and puts back the
buffer the batch started in with the counts it started from. A commit
cannot be undone once a block holding its batch's elements is queued:
when the stream refuses a later one, the writer frees the blocks it did
not queue, leaves the slot without a buffer and fails the stream. Refused
before that, the batch is absent.

While a transaction is open its thread does no other stream operation, so
encoder code that reaches another stream cannot hold two slot locks; a
close or discard of a stream meanwhile returns at once and runs when the
transaction ends, reporting a failure on stderr (an exception to SLOT-15);
one that ends in a defect leaves those streams to the run's end (SLOT-13).
A transaction still open when its call ends is a defect: it is aborted
and the process panics (PANIC-1). A thread that ends with a
transaction open ends its process, and a writer that dies inside the
transaction fails the stream (SLOT-14); its private blocks stay allocated
until the namespace ends. A forked child forgets a transaction its
parent's thread had open. Pending blocks are bytes outside the queue's
bound (SLOT-32) for the length of one batch.

The protocol of blocks, counts, spares, zeroing and the GIL is modelled in
`tla/EncodeInPlace.tla`, with a bug variant for each of its rules; the
refusals, fork and thread rules are checked by the tests above.

### SLOT-37 Every binding writes an output file's batches in place
Status: implemented
Checked by: an_in_place_batch_writes_the_bytes_the_copy_path_writes, python_waits_for_a_stream_lock_only_without_the_gil, golden:write-in-place, golden:write-in-place-failure-cpp, golden:write-in-place-failure-py, golden:write-in-place-failure-r, golden:write-in-place-wait-py

The C++, Rust, Python and R bindings write a `@write` batch to an output
file through SLOT-36's transaction, and with `mlc_write` from their own
value when begin refuses. Each encodes the canonical bytes, which a test
per binding and a golden writing the same values from several languages
compare byte for byte. An error in the encoder aborts the batch: C++ and
Rust through a guard, R in one `R_UnwindProtect` around the batch, whose
cleanup aborts the transaction when an R error unwinds it. R writes a
batch given as a list or as a character vector of Str in place, sizing
each element as it places it; a batch given as an atomic vector, factor
or integer64 vector takes the copy path.

A Python writer takes the slot lock with the GIL released, then sizes,
places and writes each element of a list; a batch given as bytes or a
numpy array takes the copy path. It
opens the batch with `MLC_WRITE_NOWAIT`: a placement that would wait for
queue room places nothing and returns `MLC_WRITE_WOULD_BLOCK`, and the
writer then waits in `mlc_write_wait` with the GIL released, sizes the
element again and places it; an abort, which runs the closes deferred
during the batch, runs with the GIL released too. Before its first wait a list batch is
replaced by a shallow copy, so the batch is one state of the list (its
elements as each is written); a bytearray, which another thread could
change during a wait, takes the copy path. The commit runs with the GIL
released. Python code that runs during the encode (a dict key's __eq__, a
finalizer) may release the GIL, and an element that no longer fits the
room it was sized for fails the batch. So that another Python thread then
given the GIL cannot deadlock with the writer, pymorloc.c makes no call
that may wait for an output stream's slot lock or queue with the GIL held
(`tla/EncodeInPlace.tla` models the GIL with a third Python thread that
mutates the writers' objects and takes the slot lock).

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
Checked by: a_batch_decoded_in_place_is_the_batch_next_returns, a_decoder_that_fails_still_releases_its_claim, golden:next-decode-failure, an_ifile_slice_decoded_in_place_is_the_slice_block, a_slice_checks_only_the_records_it_reads, a_slice_decoder_that_fails_releases_every_copy, a_slice_fails_when_its_handle_closes_between_pieces, a_dropped_slice_page_reads_back, slice_pieces_read_onto_one_list, a_slice_piece_that_fails_is_held_for_after_the_runtime, golden:ifile-slice-in-place, golden:ifile-slice-decode-failure

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

A unit-step IFile slice with no field tail, read from a C++ or Rust pool, is
handed to the same decoder one piece per sub-packet it spans, in order, on
the calling thread, after a callback with the total count. A piece is that
sub-packet's run clipped to the slice, under a header of the run's own: read
where it is mapped when the sub-packet is uncompressed and aligned,
otherwise from its shared-memory copy (SLOT-34 decides which copies are
kept). Only the clipped records and what they address are checked. Before
each piece the handle's generation is checked, so a close between pieces
fails the slice; nothing a piece is given outlives its callback, and a
failing callback ends the slice with every copy released. Pages of a wholly
read uncompressed sub-packet are dropped from the process's mapping.

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
none is recorded in an arena while kept, and a served one is given to the
arena (SHM-14). Speculation never fails a claim: a block that cannot be
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
alone once the decoder returns; while the slot decodes ahead it keeps one
such block, counted in the bound, as the landing of a later pass instead
of releasing it, and releases it when its claims stop
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

### SLOT-32 The streams one process writes share a bound on queued bytes
Status: implemented
Checked by: streams_share_the_queue_bytes_and_keep_no_more_spares_than_their_depth, many_compressed_streams_stay_within_the_queue_bytes

The process that writes streams (SLOT-10) holds the queue of each stream
it counts to a depth of at most `L / (n * s)`, never below one, where `L`
is 1 GiB, `n` the streams it counts and `s` the stream's sub-packet size
(SLOT-20). It counts a stream from its start until it ends, is closed,
fails or queues an uncompressed batch, and sets the depths again at each
of these; a writer stops being counted before it leaves its queue, so no
depth is set on a queue after its writer has left it. A producer waiting
under a lowered depth sees a raised one within its wait slice, and
batches queued before a depth was lowered stand until written. A stream
keeps at most one written buffer for reuse. A block larger than `s` (an
oversize element or a staged batch) is queued only into an empty queue,
so a stream holds at most two: one queued and one being filled.

So, beside blocks larger than `s`, the batches queued by the streams a
process counts take at most `max(L, n * s)` bytes once earlier depths
have drained; every stream it writes also holds its write buffer, the
block it is filling, at most one spare, and, when uncompressed, one
queued batch; and the compressed copy of each batch being written lives
in the writing process. No producer waits on any stream but its own.

## Reading a stream file's end

A stream file ends with a footer that indexes its sub-packets (FMT-8). The
custodian writes a temporary footer after each sub-packet it writes and
replaces it with a final footer, carrying a status, when the stream is
closed or stopped (custody.rs `emit`, `close`).
How a file ends records why its writer stopped; this section says how each
reader treats each ending. An issue (issues/streams.md) records what is
wrong with it.

### SLOT-28 A stream file ends in one of five states
Status: implemented
Checked by: a_stopped_writer_finishes_its_file_with_the_given_status, a_discarded_stream_keeps_what_was_queued_and_no_final_footer, a_writer_killed_inside_the_slot_lock_fails_the_stream, a_batch_failing_in_placement_is_absent_or_fails_the_stream

| End state | How a file gets it |
|---|---|
| final footer, CLOSED | `@close` (SLOT-15) |
| final footer, PAUSED | a remote child returns the stream without `@close`; the file is complete and may be appended to |
| final footer, FAILED | the stream's writer died inside the slot lock (SLOT-14), or daemon recovery stopped the custodian (SLOT-13) |
| temporary footer | the stream is still being written; or it was discarded, its handle dropped or its run ended without `@close`, with its unflushed elements dropped (SLOT-13); or its custodian failed to write (an I/O error such as a full disk), after which it writes nothing more; or a batch failed after part of it was queued (SLOT-35), after which what was queued is still written; or its custodian was killed after writing a batch |
| no footer | the custodian was killed before writing any footer; the machine stopped (power loss, kernel panic); or the file was cut short outside morloc (an interrupted copy, a full disk) |

Every footer follows the last complete sub-packet, so in the first four
states the data ends at the footer. A file whose bytes were cut after a
complete sub-packet is indistinguishable from the last state.

### SLOT-29 An input stream reads every complete batch and ends, whatever the end state
Status: draft

`@next` on an IStream walks the sub-packets in order and, past the last
one, returns an empty batch, in every end state. With a final or
temporary footer the walk stops at the footer; with no footer it scans
forward and stops at the last complete sub-packet, or fails if a
sub-packet's header extends past the end of the file. The footer's status
is parsed and not consulted: a FAILED file reads as a CLOSED one. A
reader of a file still being written sees the batches its writer had
flushed when the reader opened it (SLOT-15).

### SLOT-30 An indexed file opens only with a final footer
Status: implemented
Checked by: ifile_rejects_footerless_stream, an_index_out_of_order_is_not_trusted, index_counts_that_disagree_with_the_footer_are_not_trusted, an_ifile_refuses_permuted_counts, an_ifile_refuses_an_index_that_skips_the_first_sub_packet, a_read_refuses_a_sub_packet_whose_count_differs_from_the_index, a_writer_cannot_build_a_footer_whose_counts_disagree_with_its_total, concat_refuses_a_source_without_a_final_footer

`@open` as an IFile needs the per-sub-packet element counts that only a
final footer records, so it refuses a file in the temporary-footer or
no-footer state. It does not consult the status: a FAILED file opens.

The index is checked at open without reading the sub-packets (ruled
2026-11-02): its entries lie in order inside the body, at least a header
apart, the last sub-packet ends before the footer, and the counts add up
to the footer's total. A file that fails this reads as one without a final
footer. In a debug build (one with code checks), or with
`MORLOC_VERIFY_INDEX=1`, the open also walks the sub-packets and refuses a
file whose footer's index differs from them. In every build, a read
that loads a sub-packet fails when the sub-packet holds a different
number of elements than the index gives it. `@concat` and `@append`, which
write a new footer, always walk, and refuse a damaged footer or one that
differs from the sub-packets; `@concat` also refuses a source without a
final footer. A writer refuses to write a final footer whose counts do not
add up or whose entries are out of order.

### SLOT-31 What a reader reports at an ending that is not a clean close
Status: draft

Open; needs a ruling and a model (issues/streams.md). The endings in
SLOT-28 tell a reader why its writer stopped, except in one case: a
temporary footer is written both by a stream still being written, which a
reader may legitimately read while it grows, and by a stream discarded or
killed, whose file will never change again. The registry of the process
tree that writes a stream can tell the two apart while that tree runs; a
reader on another machine, or after the tree has ended, cannot.
