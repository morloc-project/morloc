# Streams (SLOT)

A stream's identity lives in a shared registry slot; each process keeps its
own descriptor, mapping and cache for it. A handle carries the slot index
and the slot's generation when the handle was made.

### SLOT-1 A stale handle never touches the slot it used to name
Status: implemented
Checked by: a_stale_handle_never_touches_the_slot_it_used_to_name

Every operation compares the handle's generation with the slot's under the
slot lock before changing anything.

### SLOT-2 Every sealed batch is written
Status: implemented
Checked by: a_batch_sealed_while_a_drain_finishes_is_still_drained, dispatch_end_writes_the_threads_sealed_batches

A stream with sealed batches stays listed until a drain that holds its
slot has written them.

### SLOT-3 A process keeps one locked descriptor per open written stream
Status: implemented
Checked by: a_stream_used_by_two_threads_stays_locked

Threads of one process take turns on a written stream's process-local
state rather than each attaching a descriptor.

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
Status: deviation

The slot lock is held across file writes, fsync, compression and calls to
the nexus, and the release pass waits on other processes' slot locks with
no deadline.

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
handle (a scan for an ending or orphaned slot) checks again under the
lock. An acquire load does not keep earlier reads
before it, so the second load follows an acquire fence; on a weakly ordered
processor the reads could otherwise complete after it. A writer moves the
generation before it changes any field or frees any block: releasing a slot
bumps the generation first and fences, so its clearing stores cannot be
seen ahead of the bump, and a reader that overlaps the release fails its
second load instead of accepting cleared fields. Opening publishes fields
then the new generation. Model: `tla/SlotSeqlock.tla`.

### SLOT-9 A process drops its slot for a stream that has ended
Status: implemented
Checked by: a_readers_slot_for_a_stream_another_process_closed_is_released, tla:LocalSweep, tla:LocalSweep_lock_holders_only.bug, tla:LocalSweep_drop_in_use.bug, tla:LocalSweep_reinstall_stale.bug

A process keeps a local slot per stream it uses: a mapping, a cache and
write buffers for one generation of the stream. Another process may end the
stream and never touch it again from here, so waiting for this process's
next use would keep the slot for the life of the process. Every release of
a registry slot advances the doorbell's count without waking anyone. The
release service's pass, run when woken and at least once a second, drops
every local slot not taken out by a thread whose generation is no longer
its stream's, releasing a file lock it holds as before. At a dispatch end
when the count has moved since the process last looked, and before a
worker decides whether it can retire, a pass that never waits drops such
slots that hold no file lock (a holder runs the release service), and is
skipped and retried later if a pass is already running. A slot given back
after its stream ended is dropped rather than kept. Model:
`tla/LocalSweep.tla`.
