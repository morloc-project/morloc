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
Status: implemented
Checked by: registry_rows_obey_their_class, a_child_forked_while_a_thread_holds_the_temp_dir_lock_can_read_it, a_child_forked_while_a_thread_holds_a_reset_lock_gets_a_fresh_one, tla:ForkLocks, tla:ForkLocks_uncovered.bug

Each lock in the registry below is either held across fork (`held`: the
prepare handler takes it and both sides release it, so the child sees a
consistent copy of what it guards) or reset in the child (`reset`: the child
gets a fresh lock and fresh state, and never touches the parent's). A lock
with no class can be inherited held by a thread the child does not have,
and the child then blocks on it forever. A held lock is declared as a
`Held` with its rank, and one prepare handler, registered when the library
loads, takes them all; a reset lock is declared `Reset` (FORK-8). The
guards live in one static slot: prepare fills it only once it holds every
held lock, and the after-fork handlers empty it before releasing them, so a
second forking thread, blocked on the first held lock, never sees it full.
Model: `tla/ForkLocks.tla` (`ForkLocks` passes;
`ForkLocks_uncovered.bug` shows the child blocking).

### FORK-6 Workers fork from a process that has no other threads
Status: implemented
Checked by: a_worker_forked_from_a_process_with_another_thread_never_runs, a_worker_forked_from_a_lone_thread_runs, a_forked_child_counts_only_its_own_threads, wanting_a_sweeper_starts_no_thread_and_a_childs_first_request_starts_one, tla:PoolFork, tla:PoolFork_guard_thread.bug, tla:PoolFork_eager_sweeper.bug, tla:PoolFork_wanted_by_thread.bug, tla:PoolFork_unpolled.bug, tla:PoolFork_count_before_fork.bug, tla:PoolFork_no_kill.bug, tla:PoolFork_ungated.bug

A forked child has only the thread that forked, so a pool that runs user
code in forked workers (Python fork mode, R) forks them from a coordinator
that starts no thread of its own. The coordinator loads no user code; it
watches the nexus's lifeline in its own wait loop rather than from a
thread; and attaching the stream registry marks the sweeper as wanted
without starting it, so the sweeper starts on the first sweep request in
whichever process sends one, a forked worker included. If no thread can be
started, the request is served by the thread that sent it.

A library may run threads of its own that it ends in its fork handler and
starts again later (OpenBLAS does), so the R coordinator forks through a
gate: the child waits while the parent, past every fork handler, counts its
live threads (on Linux, a thread already exiting is not counted), and runs
only if there is one. Otherwise the parent kills the child, which may be
stuck in a fork handler before its gate, and the fork fails loudly. The
Python coordinator, which loads no such library, counts before each fork
and on a refusal ends with every process it started.

The count sees threads that exist at the fork and still exist when it is
taken; a thread that ends in between is not seen. The guarantee is that the
coordinator starts no thread, which the model checks; the count is how a
change that breaks it is caught. Model: `tla/PoolFork.tla`.

### FORK-7 Held locks are taken in one global rank order
Status: implemented
Checked by: a_lock_taken_below_a_held_rank_is_refused, a_fork_from_a_thread_holding_a_runtime_lock_aborts, a_fork_from_a_thread_holding_a_reset_lock_aborts, tla:ForkLocks_misordered.bug, tla:ForkLocks_fork_while_holding.bug

The prepare handler takes held locks in rank order, and no thread takes a
lower-ranked held lock while holding a higher-ranked one. Otherwise prepare
and a worker can each hold the lock the other waits for. A thread that
forks while holding a held lock deadlocks in its own prepare handler, so
prepare aborts instead. Each thread records the ranks it holds; taking a
`Held` lock at or below one already held panics, and prepare aborts if the
forking thread holds any. Model: `ForkLocks_misordered.bug` and
`ForkLocks_fork_while_holding.bug` deadlock.

### FORK-8 A reset lock is never the parent's in the child
Status: deviation

A reset lock and its state are replaced on first use in a new fork
generation; the parent's copy is forgotten, never released or waited on.
In the runtime a reset value is declared `Reset`: a slot holding the
current instance tagged with the generation that built it, replaced by
compare-and-swap, so a child never waits on a lock and threads of one
generation share one instance. A thread forking while it holds a reset
lock would keep the parent's instance in the child, so prepare aborts then
too. Model: `tla/ResetPublish.tla`
(`ResetPublish` passes; the `store`, `no_generation` and `free_observed`
variants fail). Tests: a_child_forked_while_a_thread_holds_a_reset_lock_gets_a_fresh_one,
a_fork_from_a_thread_holding_a_reset_lock_aborts,
a_forked_child_never_resolves_its_parents_cell,
a_forked_child_leaves_its_parents_spooled_inputs,
a_forked_child_leaves_its_parents_temp_files,
a_forked_child_starts_its_own_sweeper_when_its_parent_had_one. A fresh
value that depends on the parent's state is seeded from it: the temp
registry's dispatch count starts at the forking thread's dispatch depth,
and a child restarts the sweeper when its parent had started one. The
emitted Futhark context lock is not yet reset.

### FORK-9 A lazily initialised value cannot be mid-initialisation at fork
Status: implemented
Checked by: no_runtime_value_waits_for_another_thread_to_initialise_it, a_child_forked_while_another_thread_builds_gets_a_value_at_once, tla:OncePublish_oncelock.bug

A `Once` or `OnceLock` that another thread is initialising when the process
forks stays "running" in the child, and the child's first use waits forever.
The runtime's lazily set values are published by compare-and-swap instead
(INIT-3), and its crates hold no standard once cell outside tests.

### FORK-10 No held lock is held across an unbounded wait
Status: deviation

Prepare waits for every held lock, so a thread that holds one while
waiting on another process makes the fork wait as long as that process.
Today the allocator's locks are held across other processes' volume locks
and volume creation; the release pass across other processes' slot locks,
compression jobs and nexus calls (also in prepare's own drain). Shared
segments are opened outside their locks (INIT-2), and the benchmark and
tee files are opened and written outside theirs. Model:
`ForkLocks_held_across_wait.bug` deadlocks.

### FORK-11 Code reachable in a forked child takes no standard-library lock
Status: deviation

Rust's standard streams and environment are guarded by locks of their own
with no fork handling. The fork handler holds the standard output and error
locks across fork, after every held lock, so a child inherits them
unlocked. The environment's lock has no public handle and cannot be held:
a child forked while another thread of the parent writes the environment
blocks on its first read. CPython's buffered streams have the same hazard
in the Python pool's forked workers.

### FORK-15 A forked child's views keep their blocks until the child is gone
Status: implemented
Checked by: an_inherited_owning_view_keeps_its_block_until_the_child_is_gone, an_inherited_reading_view_keeps_its_block_until_the_child_is_gone, an_inherited_language_view_keeps_its_block_until_the_child_is_gone, a_child_that_closes_its_descriptors_keeps_its_blocks_while_it_lives, a_descendant_with_its_ancestors_pid_cannot_release_the_ancestors_reference, tla:ViewFork, tla:ViewFork_unlocked.bug, tla:ViewFork_no_child_ref.bug, tla:ViewFork_child_releases.bug, tla:Lease, tla:Lease_no_staging.bug, tla:Lease_no_lock_check.bug

A forked child keeps every view its parent had: an Arrow view, owning or
reading, or a language's view of a block (a NumPy array over shared
memory). The parent may release its reference while the child still reads.
Every view holds a reference of its own (a reading view takes one when its
block is in a volume) and is recorded in one registry, a held lock, so the
fork sees exactly the views the child gets and every recorded block is
live. The fork handler takes one reference per recorded view for the
child's lease and releases none of them in the parent; the child releases
none of them either, so a block it read stays allocated until it is gone,
however it ends.

A lease is a file created and locked in a staging directory no reclaimer
reads, before the fork takes its locks; after the fork, with the locks
released, the parent writes the references and links the file into place,
never over an existing one. The child holds the lock through the open file
description it inherited and writes its process token into the file. A
process that takes a lease's lock, finds the file still the one it opened,
and finds its token's process gone, releases the references the lease lists
for the current shared namespace and removes it. Leases live in the run
directory, or else in the system temp directory, on a filesystem whose
locks a forked child inherits; on a network filesystem neither is used. If
no lease can be made, the references are kept and the fork says so. A
process that made leases reclaims free ones at its dispatch ends and before
giving up on an allocation, and an idle Python worker reclaims them too,
each at most every 250 ms; the nexus removes leases outside the run
directory at teardown and recovery, which discard the namespace.

A child whose process token is not told apart from a live process's (a
reused pid in another pid namespace started in the same clock tick) keeps
its lease until that process is gone. Models: `tla/ViewFork.tla`,
`tla/Lease.tla`.

### FORK-12 A forked child never returns into the dispatch loop
Status: implemented
Checked by: a_child_forked_during_a_dispatch_exits_instead_of_replying, a_child_forked_by_user_code_never_returns_from_the_dispatch, golden:fork-inside-call, tla:DispatchFork, tla:DispatchFork_unguarded.bug, tla:DispatchFork_by_pid.bug, tla:DispatchFork_reply_only.bug

A child forked from user code during a dispatch shares the parent's
connection and worker state; returning from the user function would send a
second reply, or run the worker's cleanup on the parent's state, and
continue as an uncounted worker taking the parent's scheduler locks. The
fork generation is recorded when a request is read, and a process whose
generation differs exits as soon as user code returns in it, however it
returns: the shared dispatch checks right after the call, the Python pool
on every path out of the call, and every reply on the request's connection
checks again. A child made without the fork handler (FORK-14) is outside
this item. Model: `tla/DispatchFork.tla`.

### FORK-13 A cached process identity is refreshed in a forked child
Status: implemented
Checked by: a_forked_child_logs_its_own_pid, a_forked_child_never_resolves_its_parents_cell

A value derived from the process id and cached on first use would name the
parent in a child that inherits it. Logging reads the pid when it writes,
and fold cells draw a new tag in the child, never equal to the parent's.

### FORK-14 State inherited across fork is owned by fork generation, never by pid
Status: implemented
Checked by: a_descendant_with_its_ancestors_pid_leaves_the_ancestors_buffers, a_descendant_with_its_ancestors_pid_cannot_release_the_ancestors_reference, a_descendant_with_its_ancestors_pid_ignores_the_ancestors_in_use_marks, a_descendant_with_its_ancestors_pid_opens_its_own_nexus_connection, a_descendant_with_its_ancestors_pid_does_not_own_the_program, a_descendant_with_its_ancestors_pid_waits_on_a_word_its_ancestor_holds, every_process_id_read_is_reviewed

Process-local state records the process that owns it, so a forked child
can tell an inherited copy from its own. A pid does not identify a process
along a fork chain: a descendant in another pid namespace, or one given a
dead ancestor's pid, has its ancestor's pid and would treat the inherited
copy as its own, waiting on threads it lacks or releasing what the ancestor
holds. The fork generation increases at every fork and memory reaches
another process only by fork, so it tells them apart. The counter changes
in the fork handler, so a child made without one (raw `clone`, `vfork`,
`_Fork`) that goes on running the runtime is outside this item; the
runtime starts processes with `posix_spawn` and exec. Only the copy of
the counter inside the runtime library changes; the nexus and the Rust
pool link their own copies and use none of the state above. Shared-memory
records that name a process to others, and cross-process locks, are out
of scope here (SLOT-4, SHM-5); every place that reads a pid is listed and
reviewed. The tests make the collision real with nested pid namespaces;
they are skipped where unprivileged namespaces are unavailable, except on
Linux CI, where a skip fails the run.

## Classes and ranks

Every process-wide value's fork class, and the rank of each held lock, are
in the registry, `registry.tsv`. Ranks come from the lock-order audit: every
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
