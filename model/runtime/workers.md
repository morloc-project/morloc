# Parallel helpers (WPOOL)

A C++ pool runs the spans of its parallel list calls (stdlib parallel-cpp's
`run_spans`) on the calling thread and on a process-wide set of helper
threads that libmorloc owns. The helpers outlive the calls; everything a
call leaves on a helper is handed back or cleared when the helper leaves
the call. `MORLOC_PAR_POOL=0` turns the helpers off: each call then starts
and joins threads of its own, as before the helpers existed. The switch
is kept for one release; it, its environment read and the spawn path go in
the next.
Model: `tla/WorkerPool.tla`.

### WPOOL-1 A parallel call's spans run on its caller and the runtime's helpers
Status: implemented
Checked by: tla:WorkerPool, tla:WorkerPool_nest1, tla:WorkerPool_nestfail, every_span_runs_once_on_the_caller_and_helpers, a_failed_span_stops_further_claims, a_caller_whose_helpers_never_picked_returns_without_waiting, golden:parallel-span-threads, golden:par-pool

A call posts a job of spans to the process's pool and runs spans itself
until none are left; helpers run the rest, once woken (WPOOL-12). A call of
fewer than two spans, or for one worker, runs inline and never touches the
pool, as does one of more than 2^32 - 1 spans. Every span runs at most
once; a job ends once, completed, or failed with its first failure, and no
span is claimed after the failure is visible. When the caller has run every
span before any helper picked the job, it removes the job and returns
without waiting for any helper.

### WPOOL-2 A caller works its own job, so nested calls cannot deadlock
Status: implemented
Checked by: tla:WorkerPool_caller_idle.bug, nested_jobs_from_every_helper_complete, golden:par-pool

The pool lock is never held across user code, and a caller waits only for
helpers already on its job, each of which is running a span; waits
therefore form a tree, whatever the nesting and however few helpers exist.
A span that makes a parallel call becomes that call's caller.

### WPOOL-3 A helper leaves nothing of a job on its thread
Status: implemented
Checked by: tla:WorkerPool_no_hook.bug, tla:WorkerPool_hook_after_leave.bug, tla:WorkerPool_hook_drops_trace.bug, tla:WorkerPool_flush.bug, the_hook_runs_before_the_helper_leaves_the_job, a_helper_runs_the_hook_after_each_pick, golden:par-pool, golden:parallel-span-threads

After the spans of each pick and before it leaves the job, a helper runs
the language pool's job-end hook. Its tracker entries move to the job, and
the caller adds them to its own tracker after the wait. Of its closure
traces (STATE-6) it keeps the one of the job's recorded failure, which no
longer belongs to the helper's thread, and erases the rest. Its frame
depth, release worklist (STATE-8), recursion environment and runtime-value
read flag must be clear; a hook that finds them otherwise, or fails, is a
broken runtime invariant (PANIC-13). The hook takes the job's lock only
when the helper left tracker entries or traces. Once it has left, a helper
never reads the job.

### WPOOL-4 Each span of a parallel call is a trace boundary
Status: implemented
Checked by: golden:par-pool

A span saves the frame depth on entry (STATE-6). A failure takes the
labels above it, restores the depth, and is raised again on the caller
after the wait, with its labels.

### WPOOL-5 The pool is replaced in a new fork generation
Status: implemented
Checked by: tla:WorkerPool_fork, tla:WorkerPool_forknest, tla:WorkerPool_no_gen_check.bug, tla:WorkerPool_no_gen_check_nest.bug, tla:WorkerPool_fail_lock_first.bug, a_forked_child_gets_a_fresh_pool, a_span_forking_on_a_helper_exits_in_the_child, golden:par-pool, golden:parallel-span-threads

The pool is a reset value (FORK-8): a call in a new generation gets a
fresh pool with no helpers, and never takes the parent's lock or waits on
its helpers. A span that returns or fails in a process whose generation
differs from the one its job was posted in exits there (FORK-17), on a
caller as on a helper, whether or not the function it ran checks. A failing
span checks before it takes the job's lock to record its failure, since in
a child that lock may be held by a thread the child does not have.

### WPOOL-6 The pool's helpers are bounded
Status: implemented
Checked by: tla:WorkerPool_cap, tla:WorkerPool_uncapped.bug, tla:WorkerPool_spawn_fail_keeps.bug, the_helpers_stay_under_the_ceiling, a_failed_start_gives_its_reservation_back, golden:par-pool

A post grows the pool to its job's workers less one, never past 255
helpers; a job holds at most its workers less one. The work units follow
the requested worker count, so only physical concurrency is bounded. A
post reserves the helpers it starts under the lock and starts them after
it; a start that fails gives its reservation back, and the call runs on
the helpers it has. The pool's count is its live helpers plus its
reservations.

### WPOOL-7 Helpers serve concurrent jobs round-robin, and no wakeup is lost
Status: implemented
Checked by: tla:WorkerPool_fair, tla:WorkerPool_fifo.bug, tla:WorkerPool_post_no_wake.bug, tla:WorkerPool_no_done_notify.bug, two_callers_share_the_helpers_in_turn, a_post_wakes_sleeping_helpers

A helper picks the next eligible job after the last one picked, in post
order. It keeps claiming spans of its job without the lock while that job
is the only one posted; otherwise it picks again after each span. A job is
eligible while it is posted, has unclaimed spans, has not failed and has
fewer helpers than it may hold, and has been woken (WPOOL-12). While a
woken job has room and a helper sleeps, a helper is awake to take it or
some thread holds the wake it will pass on (WPOOL-12); a helper leaving
a job is itself awake and picks again. The last helper to leave a job wakes the callers
waiting for theirs.

### WPOOL-8 A helper has a pool worker's stack and signal mask
Status: implemented
Checked by: a_helper_gets_the_recorded_signal_mask, golden:par-pool

A helper has STATE-7's stack and an alternate signal stack. It, and the
timer, are created
with the signal mask the pool recorded at its start, which its worker
threads have (normally empty), never with the mask of the thread that grew
the pool. Without a recorded mask, the mask is empty.

### WPOOL-9 Helpers and the timer are never joined
Status: implemented
Checked by: tla:WorkerPool_exit, tla:WorkerPool_exit_joins.bug, a_program_exits_with_idle_helpers

Exit joins no helper or timer and no shutdown counts them; a helper mid-span at
exit runs while static destructors run, as any thread does.

### WPOOL-10 Idle helpers retire
Status: implemented
Checked by: tla:WorkerPool_nest1, tla:WorkerPool_retire_no_dec.bug, helpers_past_the_available_cpus_retire_when_idle, golden:par-pool

A helper that has found no eligible job for a millisecond retires while
the pool has more started helpers than the available CPUs less one
(WPOOL-11), and otherwise after it has been idle for the pool workers' idle
timeout (`MORLOC_POOL_IDLE_TIMEOUT_MS`, 60 s by default, read once per
pool). It decides under the lock and counts itself out as it leaves; it is
not joined. Once idle for that millisecond it closes the connections it
keeps to other pools (NET-7), so an idle helper pins no worker of another
pool (NET-8, NET-9).

### WPOOL-11 The available CPUs are read at most once a second
Status: implemented
Checked by: the_available_cpus_follow_affinity_and_cgroup_quota, the_cpu_count_is_read_again_in_a_forked_child, cgroup_cpu_limit_reads_v1_and_v2

`morloc_available_cpus` is the CPUs the process may run on: its affinity
mask (or the CPUs online when that cannot be read), limited by the
smallest cgroup v1 or v2 CPU quota on its cgroup and their ancestors,
rounded up, and at least 1. In a C++ pool the parallel timer (WPOOL-12)
reads it every 0.9 s, whenever no job waits to be woken, and callers only
read the cached count, so no call pays for the files; the timer never
sleeps past its next read. Elsewhere, and in a forked child until its own timer
starts, a call reads it again when the cached count is more than a second
old or from another fork generation. Either way an affinity or quota
change is seen within about a second. The C++, Python and Rust parallel
modules take their default worker count from it.

### WPOOL-12 Helpers join a call only once it has run for a moment
Status: implemented
Checked by: tla:WorkerPool_pair, tla:WorkerPool_tfail, tla:WorkerPool_boundary_wake.bug, tla:WorkerPool_helpers_pick_unwoken.bug, tla:WorkerPool_post_no_wake.bug, tla:WorkerPool_post_never_notifies.bug, tla:WorkerPool_armed_read_early.bug, tla:WorkerPool_fail_wakes_own.bug, tla:WorkerPool_no_cascade.bug, a_call_its_caller_finishes_first_wakes_no_helper, a_job_still_running_after_the_delay_gets_helpers, a_long_first_span_does_not_hold_back_the_helpers, spans_waiting_for_each_other_get_a_helper, a_helper_awake_on_another_job_does_not_take_an_unwoken_job, a_caller_in_a_nested_call_still_gets_helpers, a_failed_timer_start_wakes_every_waiting_job, a_woken_job_gets_every_helper_it_may_hold, the_timer_sleeps_while_no_job_waits

A pool's timer thread, started by its first parallel call, wakes a job
still posted with spans left, whatever its caller is doing (inside a span,
however long, waiting on another span, or in a nested call): 10 to 20
microseconds after the post while it is looking at the jobs anyway, or as
soon as it runs when the post had to wake it. No helper takes a job before
it is woken, so a call its caller finishes sooner wakes, starts and waits
for no helper. The caller reads no clock. A post notifies the timer (one
condition-variable notify, under the lock) only when the timer is not armed,
that is, not due to look within the delay: it looks every delay while some
posted job waits to be woken, and for a millisecond after the last job is
removed; otherwise it sleeps until its next CPU-count read (WPOOL-11).
Waking a job wakes one sleeping helper, or starts one, and each helper that
joins a job while a woken job still has room passes the wake on the same
way before its first span. The caller claims spans from the back of the
job, where a shrinking schedule's are smallest; helpers claim from the
front. If the timer cannot be started, every job waiting for it is woken at
once and every later call wakes its own job at the post.
