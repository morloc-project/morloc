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
thread with an active arena leaks the arena's reference.

### SHM-8 A dead holder's references are recoverable
Status: deviation

References do not record their holder, so references held by a process
that died cannot be told apart from live ones and leak until the program
ends.
