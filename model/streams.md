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
Status: deviation

Plain fields are read while another process may write them, with no fence
before the second generation load; sound on x86, not guaranteed on ARM.
