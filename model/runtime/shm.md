# Shared memory (SHM)

A block's reference count is the number of holders that may still read it.

### SHM-1 The allocator is a block's first holder
Status: implemented
Checked by: threads_allocating_together_never_share_a_block

An allocation returns a block with one reference, held by the caller.

### SHM-2 No two holders are handed the same free block
Status: implemented
Checked by: processes_allocating_together_never_share_a_block, threads_allocating_together_never_share_a_block, a_child_forked_mid_allocation_can_allocate

A block is claimed under its volume's lock, by moving its count from zero
to one; the scan that finds it and the claim are one critical section.

### SHM-3 A sender hands the recipient its own reference
Status: implemented
Checked by: a_donated_reference_survives_the_sender_release, tla:ShmHandoff, tla:ShmHandoff_receiver_takes_reference.bug

A block named in a packet that leaves the process carries a reference the
sender took on the recipient's behalf before sending. The recipient
inherits it and never takes its own on arrival: there is always a window
before an arrival-side increment in which the sender may release.

### SHM-4 A reference is released once, by its holder
Status: implemented
Checked by: incref_refuses_a_free_block, incref_refuses_a_block_whose_last_reference_is_dropping, tla:ShmHandoff

Taking a reference on a free block, or on one whose last reference is
being dropped, fails instead of reviving it.

### SHM-5 A holder's death does not stop allocation
Status: implemented
Checked by: a_volume_whose_lock_holder_died_does_not_stop_allocation, a_dead_holder_poisons_the_lock

A process that dies holding a volume's lock leaves that volume's block
list possibly half edited. The volume is never allocated from again; the
allocator moves to another volume or grows a new one.

### SHM-6 A released block is scrubbed before it can be reused
Status: implemented
Checked by: a_released_block_reads_as_poison_and_a_new_one_as_zero, a_large_released_block_is_scrubbed_throughout, coalescing_invalidates_the_absorbed_block_header, every_process_poisons_as_the_program_does

The last release fills the block (zeros, or a marker byte under
`MORLOC_SHM_POISON=1`) and only then publishes it as free, with release
ordering; the allocator reads that state with acquire ordering. On x86-64
a block of 8 MiB or more is filled with streaming stores, which bypass
the cache and are not ordered by a release store, so a store fence ends
the fill before the publish. When free
blocks merge, the absorbed block's header is zeroed too, as is a remainder
too small to carry a header, so a free block reads as zero throughout and
a stale pointer to an absorbed block fails every header check. Whether
to poison is the program's choice, not each process's: the primary
volume's creator records its setting in the volume header and every
process that maps the volume follows it.

### SHM-7 Arguments and results are owned separately, even when one block
Status: implemented
Checked by: a_holders_release_leaves_the_arenas_reference, a_free_before_the_checkpoint_keeps_later_entries_in_the_rollback, an_arena_releases_cached_sub_packets_its_eval_left, tla:ArenaRelease, tla:ArenaRelease_evict_forgets.bug, tla:ArenaRelease_hit_unrecorded.bug, tla:ArenaRelease_handoff_keeps_entry.bug, tla:ArenaCheckpoint, tla:ArenaCheckpoint_length_checkpoint.bug

A result that aliases an argument has two holders and two references. On
a thread with an eval arena, the arena holds exactly the references
recorded in it: each allocation's, a reply reference the daemon adopts,
and each reference a stream read hands to the eval, from its cache or not.
Freeing one of those drops one entry for the block and releases one
reference. A reference held by a runtime structure (a stream's cache, a
cell, an Arrow view) is released without touching the arena, and a
hand-off of a recorded reference to such a holder drops one entry. A
checkpoint names the entries recorded after it, so a rollback releases
those and keeps the earlier ones, whichever were freed in between.

### SHM-8 A dead holder's references are recoverable
Status: implemented
Checked by: a_worker_that_ends_unretired_ends_the_pool, a_retiring_worker_leaves_the_pool_running, a_process_counts_the_references_it_holds, a_donated_reference_leaves_the_senders_count_and_joins_the_receivers, a_revoked_donation_returns_to_the_senders_count, a_closed_output_stream_leaves_no_reference_counted_to_its_process, a_settled_channel_leaves_no_reference_counted_to_its_process, an_open_output_stream_does_not_keep_its_opener_from_retiring, tla:WorkerExit, tla:WorkerExit_status_inferred.bug, tla:WorkerExit_retire_holding.bug, tla:WorkerExit_no_recovery.bug, tla:WorkerExit_flag_before_reap.bug

A block's count is shared by all its holders and names none of them, so a
dead holder's references are recovered by discarding the namespace, never
one by one. Each process counts the references it holds: allocating or
taking a reference adds one and releasing removes one, a donated reference
leaves the sender's count when donated and joins the receiver's when its
reply arrives, a reference a stream registry slot or channel queue holds
belongs to no process, a block another process allocated for this one (a
stdin batch from the nexus) is counted when it is released, and a forked
child starts at zero. A worker of a
fork pool (Python, R; NET-9) ends in one of two ways. It retires, which it
does only when idle and only if its language lets workers retire (Python:
after a garbage collection and its tracker flush), with its count at zero,
after writing its pid to the coordinator; or it ends any other way, by a
signal, an error, or an exit of any status without that token, and the
coordinator then ends the pool.
The exception is shutdown: it signals the pool's whole process group, so
the coordinator's shutdown flag is set before any worker can exit of it,
and a coordinator that finds its flag set after reaping a worker ends
normally rather than as failed; the namespace is discarded either way.
R workers never retire. When a pool ends, a daemon or MCP nexus runs its
coordinated recovery (DAEMON-1), which discards the whole namespace, and a
CLI run fails. A worker crash therefore restarts every pool of a daemon,
and a client that repeatedly crashes a worker reaches the recovery loop
guard, which stops the daemon. References held by children forked from user
code are not covered. Model: `tla/WorkerExit.tla`.

### SHM-9 A lock released by an unwinding panic marks what it protects as damaged
Status: implemented
Checked by: a_process_that_panics_inside_the_lock_poisons_it, a_thread_that_panics_inside_the_lock_poisons_it, a_thread_that_lives_on_after_a_caught_panic_does_not_wedge_the_lock, a_lock_taken_while_already_unwinding_is_released_normally, a_panic_caught_inside_a_section_entered_while_unwinding_poisons_the_lock, a_panic_inside_a_stream_poisons_it, tla:PanicExit, tla:PanicExit_unlock_on_unwind.bug

The volume lock is robust so that a holder's death inside the critical
section reaches other processes as a dead owner, which poisons the volume.
A section left any other way than by completing it -- a panic unwinding
through it, or an error return that means the block list is inconsistent --
must reach them the same way, never as a normal unlock. The section ends
by releasing its guard explicitly; a guard dropped without that marks the
lock poisoned and then unlocks, so every later taker, in any process or
thread, skips the volume. The lock is never left held by a live thread,
which a thread that catches the panic and goes on would otherwise keep
forever. A section that a destructor enters and completes during an unwind
releases normally; a panic inside it poisons, whatever the thread was doing
before. The stream slot lock is a recoverable lock whose only user, the
slot guard, marks its slot poisoned whenever it is dropped during an unwind
and then releases it.

### SHM-10 A release, a new reference and a pointer conversion take no process lock
Status: implemented
Checked by: a_release_a_new_reference_and_a_conversion_take_no_process_lock

They find a pointer's volume in a list of published volumes that grows
while the process runs and is emptied only by a recovery that has stopped
every thread that could read it (DAEMON-1), so no volume is unmapped under
them. A release's scrub then runs outside every lock.

### SHM-11 An allocation waits on another process only while holding no process lock
Status: implemented
Checked by: an_allocation_waits_on_no_process_lock, tla:AllocWait, tla:AllocWait_alloc_mutex.bug, tla:AllocWait_grow_under_volumes.bug, tla:AllocWait_uncounted_reader.bug, tla:AllocWait_loser_unlinks.bug

An allocation reads the published volumes (SHM-10) and waits for each
volume's own lock holding nothing else. When none has room, one thread of
the process grows at a time, under a lock that is fresh in a forked child:
it looks again, builds the new volume outside every other lock, and
publishes it under VOLUMES. A process that picks an index another process
took opens that volume instead and never removes its name.

### SHM-12 A process grows the program's memory only when no volume has room
Status: implemented
Checked by: a_process_out_of_room_uses_a_volume_another_made, a_volume_mapped_twice_keeps_its_first_mapping, tla:NamespaceGrow, tla:NamespaceGrow_no_lock.bug, tla:NamespaceGrow_no_remap.bug, tla:NamespaceGrow_map_uninit.bug, tla:NamespaceGrow_bump_after_init.bug

Volumes are numbered in order; the primary volume's header holds the next
number and a growth lock that reports a holder's death rather than
disabling itself. A process out of room maps every initialised volume
numbered below the next number and looks again; then it takes the growth
lock, maps and looks once more, and only then creates the next volume,
exclusively, at twice the size of the largest, counting it once the
create succeeds. A name that already exists was made by a creator that
died before counting it: it is used if initialised and skipped otherwise,
and never removed. The growth lock is taken after the process's own growth
lock (SHM-11) and before any volume's lock. Mapping a volume this process
already maps keeps the first mapping.


### SHM-13 A reference names the allocation it was made for
Status: implemented
Checked by: a_packet_naming_a_reused_block_is_refused_and_disarmed, a_packet_for_a_live_block_is_read, a_reclaimed_address_gets_a_new_generation

Every block carries a 16-bit generation beside its size. Claiming a block
for an allocation advances it (skipping 0), a block split off another
starts at the parent's, and a merge keeps the larger of the two, so a
header made again at an address starts past every allocation it held
before. A reference packet records the generation of the block it names
when it is built; 0 means unrecorded. The runtime reads a received
reference only through the check that the block still has that
generation; on a mismatch the read fails and the packet's relptr is
cleared, so that no later release through the packet reaches the block's
new owner. The check detects a reference used after its allocation ended
(a protocol slip); it does not prevent one, and a generation repeats
after 65535 claims of one address. Interior relptrs and releases by
address are not checked.
