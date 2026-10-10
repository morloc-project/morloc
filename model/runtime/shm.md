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
Checked by: a_released_block_reads_as_poison_and_a_new_one_as_zero

The last release fills the block (zeros, or a marker byte under
`MORLOC_SHM_POISON=1`) and only then publishes it as free, with release
ordering; the allocator reads that state with acquire ordering.

### SHM-7 Arguments and results are owned separately, even when one block
Status: deviation

A result that aliases an argument has two holders and two references. The
runtime has no release that drops a reference without also untracking the
eval arena's own entry for the same address, so a non-arena release on a
thread with an active arena leaks the arena's reference. This belongs to
the shared-memory references and pool runtime project, which records the
holder of each reference.

### SHM-8 A dead holder's references are recoverable
Status: implemented
Checked by: a_process_counts_the_references_it_holds, a_donated_reference_leaves_the_senders_count_and_joins_the_receivers, a_revoked_donation_returns_to_the_senders_count, a_closed_output_stream_leaves_no_reference_counted_to_its_process, a_settled_channel_leaves_no_reference_counted_to_its_process, an_open_output_stream_does_not_keep_its_opener_from_retiring, tla:WorkerExit, tla:WorkerExit_status_inferred.bug, tla:WorkerExit_retire_holding.bug, tla:WorkerExit_no_recovery.bug, tla:WorkerExit_flag_before_reap.bug

A block's count is shared by all its holders and names none of them, so a
dead holder's references are recovered by discarding the namespace, never
one by one. Each process counts the references it holds: allocating or
taking a reference adds one and releasing removes one, a donated reference
leaves the sender's count when donated and joins the receiver's when its
reply arrives, a reference a stream registry slot or channel queue holds
belongs to no process, a block another process allocated for this one (a
stdin batch from the nexus) is counted when it is released, and a forked
child starts at zero. A pool worker
that forks from a coordinator (Python fork mode, R) ends in one of two
ways. It retires, which a Python worker does only when idle, after a
garbage collection and its tracker flush, with its count at zero and no
stream file lock held (a lock only its holder releases), after writing its
pid to the coordinator; or it ends any other way, by a signal, an error, or an exit
of any status without that token, and the coordinator then ends the pool.
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
