# Daemon (DAEMON)

### DAEMON-1 Shared memory is unmapped only when no request is running
Status: implemented
Checked by: `recovery_waits_for_requests_already_running_and_admits_none`, `tla:DaemonRecovery`, `tla:DaemonRecovery_fixed_delay.bug`, `tla:DaemonRecovery_unlocked_admission.bug`, `tla:DaemonRecovery_wait_before_kill.bug`

Admission and the recovery flag share one lock. Recovery closes admission,
kills the pools, and waits for every admitted request to finish before it
unmaps; if requests do not finish within a minute the daemon exits instead.
No request spans a recovery, so no pointer from an old generation reaches
the new one.

### DAEMON-2 Shared daemon state is synchronised
Status: implemented
Checked by: `no_crate_declares_a_static_mut`, `a_second_bind_of_one_expression_waits_for_the_first`

No mutable static. Work that can take seconds (compiling a binding) runs
outside the lock that guards the table it updates, and a second request
for the same work waits for the first.

### DAEMON-3 A child's exit status reaches the process waiting for it
Status: implemented
Checked by: `an_exit_recorded_before_a_spawn_is_not_the_new_childs`, `a_pool_that_dies_before_its_pid_is_recorded_is_reported_promptly`

Every reap records the status with a sequence number. A waiter accepts only
statuses recorded after it forked, so a recycled pid is never answered with
an older child's status, and a child that exits before its pid is recorded
is still seen.

### DAEMON-4 A signal does not drop a request
Status: implemented
Checked by: `a_signal_during_a_read_does_not_drop_the_message`

Reads and writes on client connections retry when interrupted.

### DAEMON-5 Shutdown does not unmap memory under running threads
Status: deviation

Process exit unmaps shared memory while detached threads may still be
reading it.

### DAEMON-6 Every wait on another process is bounded or ends when the peer dies
Status: implemented
Checked by: a_client_that_connects_and_never_sends_does_not_hold_the_reader, draining_a_writer_that_never_closes_stops_at_the_deadline, an_eval_past_its_wall_limit_is_stopped_with_everything_it_started, a_frontend_eval_past_its_wall_limit_is_stopped_with_everything_it_started, stopping_all_kills_registered_groups_and_every_group_added_later, a_killed_group_is_never_signalled_again, an_eval_leader_outlives_a_term_until_it_is_released, tla:DaemonShutdown, tla:DaemonShutdown_join_first.bug, tla:DaemonShutdown_no_kill.bug, tla:DaemonShutdown_unmap_on_give_up.bug, tla:DaemonShutdown_recovery_unmaps.bug, tla:DaemonShutdown_children_survive.bug

A call into a pool has no deadline: a program may run for days, and nothing
tells the caller how long a call should take. Such a wait ends when the
peer dies (end of file, the lifeline, an owner-dead lock, a liveness check)
and otherwise lasts as long as the work. A wait that is never legitimately
long is bounded: a request is read within a stall limit after its
connection is accepted; a readiness ping waits a few seconds per attempt,
and its retries end when shutdown is requested.

A forked `morloc eval` runs in its own process group, limited in CPU time
and in wall time; the wall limit stops the whole group. The group is
registered in a table of child groups while it runs, and its leader (a
shell that runs `morloc`, then ignores SIGTERM and waits on a pin the
daemon closes after unregistering it) stays unreaped until it is
unregistered, so the group id is never reused while anything may signal it; every signal goes through the table, none follows a
SIGKILL, and a thread signalling a group blocks signals meanwhile so a
signal handler that stops the table never waits on it. Stopping the pools
also stops every registered group and every group registered later, so no
eval outlives the daemon or the front-end. The pools of a program an eval
runs live in their own groups and end with that program's nexus, by the
lifeline.

Shutdown closes the listeners, refuses queued requests and every request
not yet admitted, waits a grace period for running ones, then stops the
pools and child groups so a worker inside a call to a wedged pool returns,
and waits again. If every worker returned, the daemon unmaps shared memory
and exits. Otherwise a worker may still be inside a wait that stopping the
pools does not end (a slow client, an evaluation in the daemon itself), so
the daemon unlinks shared memory without unmapping it, frees nothing, and
ends the process without running exit handlers. A pool crash recovery
whose wait for running requests runs out exits the same way. A watchdog
started with the shutdown ends the process with only async-signal-safe
steps (kill pools and child groups, unlink shared memory and registered
paths) if the shutdown has not finished in time, for instance because a
worker holds a stdio lock the teardown needs; a second shutdown signal
fires it at once. The watchdog and the normal exit each claim the exit
first, and only the one that claims it ends the process. Requests refused, and requests that fail once the pools
are stopped, are answered as unavailable (HTTP 503). Daemon test
`shutdown-wedged`. Model: `tla/DaemonShutdown.tla`.

