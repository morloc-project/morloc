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

### FORK-5 Every process-wide lock has a fork class
Status: deviation

Each lock in the registry below is either held across fork (`held`: the
prepare handler takes it and both sides release it, so the child sees a
consistent copy of what it guards) or reset in the child (`reset`: the child
gets a fresh lock and fresh state, and never touches the parent's). A lock
with no class can be inherited held by a thread the child does not have,
and the child then blocks on it forever. Today only the seven locks marked
`held (current)` are covered. Model: `tla/ForkLocks.tla` (`ForkLocks`
passes; `ForkLocks_uncovered.bug` shows the child blocking).

### FORK-6 Workers fork from a process that has no other threads
Status: deviation

Python fork-mode and R pools fork workers after the runtime has started
its sweeper (and, in R, lifeline) threads.

### FORK-7 Held locks are taken in one global rank order
Status: deviation

The prepare handler takes held locks in rank order, and no thread takes a
lower-ranked held lock while holding a higher-ranked one. Otherwise prepare
and a worker can each hold the lock the other waits for. Today's order
(release pass, local-slot map, locked descriptors, then the allocator's
four) is implied by handler registration order and not written down or
checked against the code. Model: `ForkLocks_misordered.bug` deadlocks.

### FORK-8 A reset lock is never the parent's in the child
Status: deviation

A reset lock and its state are replaced on first use in a new fork
generation; the parent's copy is forgotten, never released or waited on.

### FORK-9 A lazily initialised value cannot be mid-initialisation at fork
Status: deviation

A `Once` or `OnceLock` that another thread is initialising when the process
forks stays "running" in the child, and the child's first use waits forever.
About thirty such cells cache configuration lazily.

### FORK-10 No held lock is held across an unbounded wait
Status: deviation

Prepare waits for every held lock, so a thread that holds one while
waiting on another process makes the fork wait as long as that process.
Today the allocator's locks are held across other processes' volume locks
and volume creation; the release pass across other processes' slot locks,
compression jobs and nexus calls (also in prepare's own drain); the stats
segment across a wait of up to five seconds; the benchmark and tee files
across file I/O. Model: `ForkLocks_held_across_wait.bug` deadlocks.

### FORK-11 Code reachable in a forked child takes no standard-library lock
Status: deviation

Rust's standard streams and environment are guarded by locks of their own
with no fork handling. A child that prints a diagnostic while another
thread of the parent was printing at the moment of fork blocks forever.

### FORK-12 A forked child never returns into the dispatch loop
Status: deviation

A child forked from user code during a dispatch shares the parent's
connection; returning from the user function would send a second reply and
continue as an uncounted worker taking the parent's scheduler locks.

### FORK-13 A cached process identity is refreshed in a forked child
Status: deviation

A value derived from the process id and cached on first use names the
parent in a child that inherits it: logging reports the parent's pid and
fold cells carry the parent's tag.

## Classes and ranks

Every process-wide value's fork class, and the rank of each held lock, are
in the registry in `state.md`. Ranks come from the lock-order audit: every
edge in the code goes from a lower to a higher rank, and the graph has no
cycle.

Cross-process locks are not fork-handled: they live in shared segments and
the dead-holder rules apply (SLOT-4, SHM-5). They still have a place in the
order. A slot's lock ranks between RELEASE_PASS and PROCESS_LOCAL_SLOTS:
the release pass takes slot locks, and code holding a slot lock takes the
local-slot map, the descriptor list, the allocator, the sealed list and the
compression service. A volume's lock ranks after VOLUMES: the allocator
holds ALLOC_MUTEX and VOLUMES while it takes one. Test-only hooks
(BEFORE_STREAM_LOCK, DRAIN_GAP_HOOK) are compiled only into tests.
