# Daemon (DAEMON)

### DAEMON-1 Shared memory is unmapped only when no request is running
Status: implemented
Checked by: `recovery_waits_for_requests_already_running_and_admits_none`, `a_stopped_writer_finishes_its_file_with_the_given_status`, `tla:DaemonRecovery`, `tla:DaemonRecovery_fixed_delay.bug`, `tla:DaemonRecovery_unlocked_admission.bug`, `tla:DaemonRecovery_wait_before_kill.bug`

Admission and the recovery flag share one lock. Recovery closes admission,
kills the pools, and waits for every admitted request to finish, then stops
the stream writers (SLOT-13) before it unmaps; if requests do not finish
within a minute the daemon exits instead.
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
Status: implemented
Checked by: retiring_at_exit_leaves_memory_readable_to_threads_still_running, no_segment_is_made_once_teardown_begins, tla:DaemonShutdown, tla:DaemonShutdown_unmap_at_exit.bug

Threads that no request accounts for -- stream writers and their
compressors, user
threads, detached connection threads -- may still read shared memory while
a process exits. So no exit path unmaps it: the nexus's teardown, a pool's
exit and the exit handler remove the names of the shared memory this
process owns, stop growth, and leave every mapping for the kernel to remove
with the last thread. Teardown hooks (the stream registry, the statistics
segment) likewise remove names and keep their mappings, and a stream
registry torn down is not made again until shared memory is set up again. Once the nexus's
teardown begins, no new segment is made, so its sweep of recorded segments
sees every one; a segment made as it began is removed by its maker. Only
recovery unmaps, under DAEMON-1.

### DAEMON-6 Every wait on another process is bounded or ends when the peer dies
Status: implemented
Checked by: draining_a_writer_that_never_closes_stops_at_the_deadline, an_eval_past_its_wall_limit_is_stopped_with_everything_it_started, a_frontend_eval_past_its_wall_limit_is_stopped_with_everything_it_started, stopping_all_kills_registered_groups_and_every_group_added_later, a_killed_group_is_never_signalled_again, an_eval_leader_outlives_a_term_until_it_is_released, tla:DaemonShutdown, tla:DaemonShutdown_join_first.bug, tla:DaemonShutdown_unmap_on_give_up.bug, tla:DaemonShutdown_recovery_unmaps.bug, tla:DaemonShutdown_children_survive.bug

A call into a pool has no deadline: a program may run for days, and nothing
tells the caller how long a call should take. Such a wait ends when the
peer dies (end of file, the lifeline, an owner-dead lock, a liveness check)
and otherwise lasts as long as the work. A wait that is never legitimately
long is bounded: a readiness ping waits a few seconds per attempt, and its
retries end when shutdown is requested. Reading a request is governed by
NET-2 for a remote client and NET-5 on a local socket.

A forked `morloc eval` runs in its own process group, limited in CPU time
and in wall time; the wall limit stops the whole group. The group is
registered in a table of child groups while it runs, and its leader (a
shell that runs `morloc`, then ignores SIGTERM and waits on a pin the
daemon closes after unregistering it) stays unreaped until it is
unregistered, or, killed from outside, until the reaper has marked its
slot dead (DAEMON-11), so the group id is never reused while anything may
signal it; every signal goes through the table, none follows a SIGKILL,
and a thread holding a group's slot blocks signals meanwhile so a
signal handler that stops the table never waits on it. Stopping the pools
also stops every registered group and every group registered later, so no
eval outlives the daemon or the front-end. The pools of a program an eval
runs live in their own groups and end with that program's nexus, by the
lifeline.

Shutdown closes the listeners, refuses queued requests and every request
not yet admitted, waits a grace period for running ones, then stops the
pools and child groups so a worker inside a call to a wedged pool returns,
and waits again. If every worker returned, the daemon removes the names of
shared memory without unmapping it (DAEMON-5) and exits. Otherwise a worker
may still be inside a wait that stopping the pools does not end (a slow
client, an evaluation in the daemon itself), so the daemon unlinks shared
memory, frees nothing, and ends the process without running exit handlers. A pool crash recovery
whose wait for running requests runs out exits the same way. A watchdog
started with the shutdown ends the process with only async-signal-safe
steps (kill pools and child groups, unlink shared memory and registered
paths) if the shutdown has not finished in time, for instance because a
worker holds a stdio lock the teardown needs; a second shutdown signal
fires it at once. The watchdog and the normal exit each claim the exit
first, and only the one that claims it ends the process. Requests refused, and requests that fail once the pools
are stopped, are answered as unavailable (HTTP 503). Daemon test
`shutdown-wedged`. Model: `tla/DaemonShutdown.tla`.

### DAEMON-7 A request handler's panic ends the daemon as failed
Status: implemented
Checked by: a_panicking_request_is_answered_500_and_the_daemon_shuts_down_as_failed, a_failure_before_serving_starts_is_not_forgotten, a_stdio_request_that_panics_in_the_daemon_fails_the_daemon_instead_of_exiting, a_packet_request_that_panics_is_answered_with_a_fail_packet, a_handler_that_panics_after_replying_gets_no_second_reply, a_connection_with_no_descriptor_to_spare_is_closed_unserved, a_request_that_does_not_panic_is_closed_after_its_handler, tla:DaemonShutdown, tla:DaemonShutdown_panic_continues.bug

A panic in a request handler is a runtime bug and may leave state half
updated, so the daemon does not go on taking requests. The worker hands the
handler its own descriptor for the connection and keeps the one it
accepted; with no descriptor to spare it closes the connection unserved.
The handler records whether its reply is a packet and whether any of a
reply has been written. When the handler panics, the worker answers that
connection as failed -- HTTP 500, a FAIL packet to a packet client, an
internal error otherwise -- unless a reply was begun, shuts the connection
down, records the panic and asks for shutdown; it never touches the
handler's descriptor, which may already be closed and reused. A panic the
nexus catches while serving the daemon fails it the same way, and a failure
recorded before the daemon starts serving still ends it. A binding the
handler was compiling is finished as failed during the unwind, so requests
waiting on it go on. The daemon then shuts down as DAEMON-6 says and exits
with the internal-error status of PANIC-1, the watchdog's exit included, so
a supervisor restarts it.
Requests already running finish within the grace period. The job queue's
lock is never held inside the catch, so a caught panic cannot poison it. Model:
`tla/DaemonShutdown.tla`.

This holds for a panic that unwinds through Rust frames to the worker. A
panic inside an `extern "C"` function the handler calls cannot unwind out
of it; DAEMON-8 covers that case. A shared-memory lock the panic unwound
through is never handed on as sound (SHM-9).

### DAEMON-8 A panic below a C boundary in a request ends the daemon as failed
Status: implemented
Checked by: rust_code_calls_the_rust_function_behind_a_c_entry_point, a_panic_below_a_c_abi_function_called_from_rust_reaches_the_catch

A panic below the daemon's handlers unwinds to the request's catch: no
Rust code in libmorloc calls one of libmorloc's own C entry points (see
DAEMON-9). A shared-memory allocation it interrupted is poisoned during the
unwind (SHM-9). The C callbacks libmorloc hands to other code (an Arrow
array's release, a dispatch's release) are still `extern "C"`; a panic
below one that Rust code invokes ends the process (PANIC-1).

### DAEMON-9 Rust code calls the Rust function behind a C entry point
Status: implemented
Checked by: rust_code_calls_the_rust_function_behind_a_c_entry_point

Each libmorloc C entry point is a shell in its file's private `c_abi`
module that calls the Rust function of the same name in the file; only
the C entry points' own callers in other languages, and tests, reach the
shells.

### DAEMON-10 A daemon removes only the endpoint files it made
Status: implemented
Checked by: a_daemon_removes_only_the_endpoint_files_it_made

On any exit -- a clean one, the shutdown watchdog's, a signal's, a panic's
-- a daemon removes its unix socket and its port file only if the file at
each path is still the one it created, so a daemon that has since taken
over the path keeps its own.

### DAEMON-11 A process group id is held while the nexus may signal it
Status: implemented
Checked by: tla:PoolGroup, tla:PoolGroup_no_pin.bug, tla:PoolGroup_kill_then_clear.bug, tla:PoolGroup_reap_unmarked.bug, tla:PoolGroup_spawn_unheld.bug, a_pool_group_outlives_its_pool_until_released, a_pin_ignores_sigterm_from_birth, a_group_whose_leader_exited_is_never_signalled_again, stopping_all_waits_for_a_held_group_then_kills_it, a_thread_that_reaps_waits_for_a_reap_in_progress

Each pool runs in a process group led by a pin: a shell started with
SIGTERM, SIGINT and SIGHUP blocked, which exits when the nexus closes its
pipe or when it is killed. The reaper takes the pool as soon as it exits,
but the group id stays held by the pin, so no signal the nexus sends to the
group reaches a process the id was handed to since. Every signal goes
through the group's slot in a table of groups, and none follows a SIGKILL.
The pool is started while its group's slot is held, so stopping every group
either kills the pool with the group or finds the group dead and the pool
is never started.

The nexus has one reaper, run by the SIGCHLD handler and by threads that
reap, and only one runs at a time. A thread waits for the reaper running,
so it never reports a child as alive while that child's reap is in
progress. The handler never waits, since it may have interrupted the
running reaper's own thread; it leaves its children to the one running,
which looks again before it stops. Before it reaps an
exited child it marks the slot of any group that child leads (a pool's pin,
or the leader of a forked eval in DAEMON-6) dead, so a pin or eval leader
killed from outside the nexus never leaves a slot naming a free id. Its
pool, still running, is then out of reach of signals and ends by its
lifeline when the nexus exits.

Stopping the pools sends SIGTERM through every group, waits up to 200 ms
for the pool processes (not their groups, which the pins keep) to exit,
kills every group, and reaps for up to 100 ms more.

### DAEMON-12 The router runs at most one daemon per program and reaps it
Status: implemented
Checked by: tla:RouterRestart, tla:RouterRestart_restart_without_stop.bug, tla:RouterRestart_raw_pid.bug, a_daemon_that_never_accepts_is_stopped_reaped_and_refused, an_exited_daemon_leaves_its_slot_before_it_is_reaped, signalling_all_reaches_every_live_group_and_no_killed_one

The router starts a program's daemon in its own process group, registers
the group in a table of groups, and waits for it to accept connections. A
daemon that does not accept within the start limit (30 s) is stopped and
reaped, and the request fails. A connection that fails stops and reaps the daemon it tried, unless
another request has already replaced it, before another is started, so no daemon outlives the router's record of
it. Stopping sends SIGTERM through the group's slot, waits up to 2 s, then
sends SIGKILL and reaps. Every signal to a daemon, including the shutdown
handler's, goes through the table; before the router reaps a daemon it
marks the slot dead, so no signal reaches a pid the kernel has reissued.
The status query counts an exited daemon as stopped even before it is
reaped.

### DAEMON-13 A run's shared memory is removed once the run has ended, however its nexus ended
Status: implemented
Checked by: tla:RunSweep, tla:RunSweep_no_wait.bug, tla:RunSweep_sweep_each.bug, tla:RunSweep_sweeper_in_group.bug, tla:RunSweep_startup_ignores_groups.bug, a_group_runs_until_every_member_has_exited, a_dead_run_is_swept_only_once_its_recorded_groups_have_ended, a_run_is_found_by_its_owners_pid_and_start, the_sweeper_waits_for_a_group_to_end_then_sweeps_its_dead_run, the_teardown_sweeper_runs_outside_the_group_it_waits_for

The run directory holds a marker for every shared-memory object of the
run, written before the object is made; a create whose marker cannot be
written makes nothing. Before it starts a pool, the nexus records the
pool's group in the run directory as `.group-<pgid>`, so once the nexus is
gone every group of the run is recorded. A nexus that exits cleanly removes
the names itself (DAEMON-5); one that is killed cannot, so its pools do.

A pool that sees its lifeline end ends its group as before (SIGTERM, the
grace period, SIGKILL) and also starts a sweeper, `morloc-nexus
sweep-run`, in a process group of its own, so the SIGKILL does not reach
it. The lifeline token names the nexus's executable, pid and start stamp;
the sweeper finds the run as the directory whose owner record matches
them. It waits until no member of the pool's group runs: a member killed
in the middle of making an object finishes the system call before it dies.
Then, if the run's owner is dead and no recorded group has a running
member, it sweeps the run: every object a marker names, every marker, the
run's temp root, and the directory. Groups only end, so the sweeper of the
last group to end sees every group ended; two sweepers that both sweep do
no harm. A process that is gone or awaits reaping runs nothing and counts
as ended. A later nexus's startup sweep removes a dead run under the same
condition. A sweeper whose group does not end within its limit (10 s),
or that cannot be started, leaves the run to that startup sweep.

Out of reach: a process that leaves its pool's group (setsid) escapes both
the SIGKILL and the sweeper's wait, and an object it makes after the sweep is not
removed; a group id reissued to a stranger's group that runs counts as
running, so that run waits for a later startup sweep.

### DAEMON-14 A pool's group ends when its pool process does
Status: implemented
Checked by: tla:PoolGroup, tla:PoolGroup_reap_only.bug, killing_a_group_by_its_id_goes_through_its_slot, golden:pool-main-death-ends-workers

A pool is its main process together with every process in its group: the
workers it forks (Python fork mode, R) and anything user code forks. A
worker serves calls only for its pool, so none outlives it. When the
reaper takes an exited pool process, whatever ended it and in every mode
(a one-shot run, a daemon, an MCP server), it first sends SIGKILL to the
pool's group through the group's slot (DAEMON-11). The kill is a slot
signal like any other: callable from the SIGCHLD handler, refused once the
group was killed, and it marks the slot dead. Every call the group was
serving then fails, as its connection closes, and the nexus reports the
pool's death. A one-shot run exits as failed; a daemon or MCP server
answers the call as failed and recovers its pools (DAEMON-6).

Out of reach: a group whose pin was killed from outside the nexus has a
dead slot (DAEMON-11), so a worker of its pool runs until its call
returns; workers do not watch the lifeline.
