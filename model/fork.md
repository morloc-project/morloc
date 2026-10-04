# Fork and exec (FORK)

A forked child gets a copy of its parent's memory and descriptors and only
the thread that called fork.

### FORK-1 A forked child never releases what its parent holds
Status: implemented
Checked by: a_value_dropped_in_a_forked_child_is_forgotten, the_exported_generation_changes_in_a_forked_child, a_forked_child_leaves_its_parents_cached_reads_alone, tla:ShmHandoff_child_releases.bug

Process-local state that holds shared-memory references or thread handles
records the fork generation it was made in. In a child the generation has
changed: using such state is an error and dropping it forgets it. The
language pools' trackers forget inherited entries the same way.

### FORK-2 A forked child can shut down without its parent's threads
Status: implemented
Checked by: a_forked_child_can_shut_down_without_its_parents_sweeper

Shutdown joins only threads the current process started.

### FORK-3 No descriptor crosses exec unless it is meant to
Status: implemented
Checked by: every_descriptor_is_created_close_on_exec, every_constructor_returns_close_on_exec_descriptors

Every descriptor is created close-on-exec. A descriptor reaches an exec'd
child only by being duplicated onto stdin, stdout or stderr, or by the
lifeline hand-off, which clears the flag on one descriptor for one child.

### FORK-4 Ending a stream frees its path whoever else holds the descriptor
Status: implemented
Checked by: a_forked_child_does_not_keep_a_finished_stream_locked

A file lock belongs to the open file description, which a forked child
shares. The opener unlocks explicitly when the stream ends rather than
relying on close.

### FORK-5 Every process-local lock is consistent in a forked child
Status: deviation

The stream registry's map and descriptor-list locks, the allocator's locks
and the release pass are held across fork; the write-behind, sweeper,
release-service and registry-segment locks are not, so a child forked
while another thread holds one deadlocks on first use.

### FORK-6 Workers fork from a process that has no other threads
Status: deviation

Python fork-mode and R pools fork workers after the runtime has started
its sweeper (and, in R, lifeline) threads.
