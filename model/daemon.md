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

### DAEMON-6 Every wait on another process is bounded
Status: deviation

Calls to pools carry no deadline; a wedged pool blocks its caller, and the
daemon's shutdown, indefinitely.
