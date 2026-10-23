# Process-wide state (STATE, INIT)

Every value that lives for the life of a process and can change -- a lock,
an atomic, a once-initialised cell, a thread-local, a lock inside a struct,
a mutable static in a language binder or in code the compiler emits -- is a
row in the registry below, with a class that says what happens to it
across fork.

### STATE-1 Every process-wide mutable value is registered with a fork class
Status: implemented
Checked by: every_process_wide_value_is_registered, every_registry_row_names_a_value_in_code, registry_rows_obey_their_class, deviating_rows_only_decrease

### STATE-2 Per-thread state a manifold builds does not outlive its thread
Status: implemented
Checked by: golden:threaded-sourced-callbacks, golden:spec-host-7-1, cschema_cache_is_freed_when_its_thread_ends, a_drain_leaves_nothing_behind_on_its_thread, deep_recursive_value_dropped_concurrently

A manifold may run on a thread a sourced function starts (HOST-7). A
`thread` row such a call can reach either frees its value when the thread
ends, or holds nothing once the frame that filled it returns. The schema
conversion cache is freed by its destructor. The recursive-value drain
keeps its worklist in the frame of the drop that started it, and a shared
recursive value is drained by the owner that finds itself unique when it
drops, on whichever thread that is. A thread that outlives the call (a
persistent host pool) keeps what a failed call left in its tracker; see
issues/rust-closures-threads.md.

### STATE-3 A packet a closure captures is only read by the closure's calls
Status: implemented
Checked by: golden:placement-representability

A Rust closure may hold a packet among its captures (a value the pool does
not decode). Its calls pass the packet on to the manifold the closure
calls, which reads it and never releases it, so threads calling the closure
at once (HOST-7) share it. Its lifetime is the enclosing call's; see
issues/rust-closures-threads.md.

### STATE-4 A Rust function value that can cross is found by its allocation
Status: implemented
Checked by: tla:FnRegistry, tla:FnRegistry_remove_after_free.bug, tla:FnRegistry_register_after_escape.bug, golden:defunc-record-closures-recross, golden:host-closure-cannot-cross

A Rust function value is a plain `Arc<dyn Fn>` (HOST-8), which carries
nothing morloc can read. A value that can cross pools (its signature
reaches a serialization site) keeps its origin, staged step and partial
application in an environment its closure owns, and a process-wide
registry maps the address of the value's allocation to that environment.
The entry is inserted before the constructor returns the value, so no
handle exists without it, and removed by the environment when the last
owner drops the value, before the allocation is freed, so an address in
the registry always names the live value made there. A clone keeps the
address; a value host code builds has no entry and fails to cross by name.
A lookup that would reinterpret an environment checks the value's type
first. The registry is `exec-only`: no process that links the Rust runtime
forks a worker (FORK-6). Model: `tla/FnRegistry.tla` (`FnRegistry` passes;
`remove_after_free` and `register_after_escape` lose an entry).

### STATE-5 A thread's frame stack lists the named morloc calls active on it
Status: implemented
Checked by: a_boundary_takes_the_calls_above_its_depth_innermost_first_and_restores_it, the_crash_report_names_the_innermost_recorded_call, each_thread_has_its_own_frame_stack, tla:FrameStack, tla:FrameStack_ring.bug, tla:FrameStack_pop_before_value.bug, tla:FrameStack_no_pop_without_return.bug, golden:spec-diag-5-1, golden:spec-emit-3-1, golden:spec-recur-4-1, golden:pool-crash-report

In the C++ and Rust pools each thread has a frame stack: a depth and the
labels of the calls below it. A call of a named function (DIAG-5) pushes
its label on entry and pops it once its result is computed, before its
locals are released; a failure skips the pop. With no failure in flight,
entries 1..depth are exactly the named calls on the thread that have not
computed their result, outermost first, and the crash report names the
top one. While a failure unwinds, the entries above the depth of the
boundary that will catch it (STATE-6) are the named calls it has unwound
through. A thread reaches generated code only through a boundary, and
the first boundary it enters reserves its stack: lazily committed address
space, left out of core dumps, for 2^28 calls or eight per 16 bytes of the
thread's machine stack, whichever is more. That is more calls than the
machine stack holds however many the optimizer inlines into one frame, so
the stack drops no entry and a push makes no check. Where the system will
not reserve that much, the reservation is the eight per 16 bytes alone. A
guard page follows it, so a stack that outgrows it faults rather than
overwrites. When no reservation can be made, a C++ boundary fails
the call it was entering; in Rust, as on any failed allocation, the pool
ends.

A thread's stack holds only that thread's calls. A closure run on another
thread starts at that thread's depth. Its failure, carried back and raised
on the calling thread, gains the calling thread's calls at that thread's
next boundary. A forked child keeps the forking thread's stack.

Pushing writes the label and the depth; popping writes the depth. Neither
copies or moves the result.
Model: `tla/FrameStack.tla` (`FrameStack` passes; `ring`, a fixed ring of
slots tagged with their depth, loses active calls; `pop_before_value`
drops the call whose result fails; `no_pop_without_return` leaves a pushed
call behind).

### STATE-6 A trace boundary leaves the frame stack at the depth it found
Status: implemented
Checked by: a_closure_boundary_carries_the_calls_a_panic_unwound_and_restores_the_depth, a_closed_pipe_passes_a_closure_boundary_unchanged_and_restores_the_depth, tla:FrameStack, tla:FrameStack_reset_at_dispatch.bug, tla:FrameStack_no_closure_boundary.bug, tla:FrameStack_pass_keeps_depth.bug, golden:spec-diag-5-1, golden:frame-stack-suspension-boundary

The boundaries are a pool's dispatch, `@try` and every closure: a function
value code outside morloc may call and whose failure it may catch,
whether a lambda, a partial application or a suspension. The thunk `@try`
makes of a pure body is run by the `@try` at once and is no boundary. Each
saves the depth on entry and sets no depth but that one. Left normally,
it finds the saved depth; dispatch checks this, which covers every
boundary inside it, and a mismatch is a broken runtime invariant that
ends the pool (PANIC-13). Left by a failure, it restores the saved depth
on every path. A user failure also takes the labels above the saved depth,
innermost first, into its message: `@try` and dispatch report it, and a
closure raises it again as a failure carrying that message. A closed pipe
or an infrastructure error passes through unchanged and carries no trace.
So sourced code that catches a failure, or enters a nested dispatch on the
same thread, finds the stack as it left it. Model: `tla/FrameStack.tla`
(`reset_at_dispatch`, `no_closure_boundary` and `pass_keeps_depth` each
leave a stack that no longer lists the calls).

### STATE-7 A pool worker thread has a native main thread's stack
Status: implemented
Checked by: golden:spec-recur-5-1

A C++ or Rust pool's worker threads, which run every call, are created
with the stack a native program's main thread gets: the soft stack limit
(`ulimit -s`), or 8 MiB when that is unlimited or unreadable. This carries
out RECUR-5.

### STATE-8 A value morloc builds is released in bounded stack
Status: implemented
Checked by: golden:deep-release-user-mapped-cpp, golden:deep-value-paths-cpp, golden:recursion-deep-value-cpp

In C++ every constructor arm morloc allocates, of a generated or a
user-mapped type, built in generated code or read from a packet, is held by
a `std::shared_ptr<const T>` (CPP-6) whose allocator destroys it in place
while fewer than 128
releases are nested on the thread. Past that depth the arm's contents move,
or are copied when the arm cannot be moved, to a heap object on the
thread's worklist and the arm is destroyed at once,
so neither the pointer's control block nor a `weak_ptr` ever sees a
deferred step; the outermost release empties the worklist, and the
worklist is freed when its thread ends. An arm that can be neither moved
nor copied, and a value user code builds itself, are released as C++
releases them. In Rust a generated type's arms are
`RecBox`es, which release a deep chain the same way.

### INIT-1 A process-wide value is initialised once, by a fork-safe primitive
Status: implemented
Checked by: tla:LazyInit, tla:LazyInit_unlocked.bug, tla:LazyInit_unheld.bug, golden:futhark-basic

Lazy initialisation built by hand (a null check, then a set) lets two
threads both initialise, and the second can destroy what the first built.
The primitive is either a value set at startup, before the process has
threads, or a `lazy` value whose initialiser runs under a lock and checks
again once it holds it; that lock must be held across fork so a child never
inherits it mid-initialisation (see FORK-5, FORK-9), or be the lock of a
reset value (FORK-8), which a child never sees. The emitted Futhark
context is built under the lock of its reset state. Model: `tla/LazyInit.tla` (`LazyInit` passes;
`LazyInit_unlocked.bug` uses a destroyed value; `LazyInit_unheld.bug` leaves
the child blocked).

### INIT-2 A value whose construction waits on another process is built outside its lock
Status: implemented
Checked by: first_uses_on_two_threads_build_one_registry, tla:LazyInit_outside, tla:LazyInit_outside_blind.bug, tla:LazyInit_locked_waits.bug

A held lock taken across construction makes prepare wait for it (FORK-10):
opening a shared segment can wait seconds for the process sizing it. Such
a value is built without the lock; the lock is taken only to check again
and install it. A thread that finds a value already installed discards its
own, releasing only what it made: a segment is unmapped, never unlinked,
since the installed mapping names the same segment, and only an installed
segment joins the crash-sweep list. The stream registry, the statistics
segment, the benchmark and tee files, and the runtime configuration (whose
environment reads can wait on a thread forking through the standard
library, which holds the environment lock across prepare) follow it.

### INIT-3 A value set on first use without a lock is published by compare-and-swap
Status: implemented
Checked by: first_uses_on_many_threads_share_one_value_and_one_side_effect, a_child_forked_while_another_thread_builds_gets_a_value_at_once, no_runtime_value_waits_for_another_thread_to_initialise_it, tla:OncePublish, tla:OncePublish_oncelock.bug, tla:OncePublish_effect_each.bug

The standard library's once cells make later callers wait while one thread
initialises; a fork in that window leaves the child waiting forever on a
thread it does not have (FORK-9). A runtime value set on first use is
instead built by each first caller without a lock and published by
compare-and-swap: a caller that loses drops its own value and takes the
published one, and only the caller that won performs the value's side
effects, such as publishing the run directory to the environment or
starting the lifeline's watcher. Nothing waits, so a child either inherits
the published value or builds its own. Between the publication and the
side effect another thread, or a child forked in that window, sees the
value without the effect; a side effect that panics is not retried.
Model: `tla/OncePublish.tla`.

## Classes

- `held` -- taken by the prepare handler across fork, in `rank` order; the
  child sees a consistent copy (FORK-5, FORK-7).
- `reset` -- replaced by a fresh value in a forked child (FORK-8).
- `unreachable` -- coordinates threads a child does not have; a child never
  reaches it (FORK-12).
- `exec-only` -- lives in a process that forks only to exec (the nexus and
  the daemon); the child runs async-signal-safe code until exec.
- `startup` -- set once before the process starts threads; the child sees
  the same value.
- `lazy` -- created on first use: under a lock held across fork and checked
  again once taken (INIT-1), or built without one and published by
  compare-and-swap (INIT-3).
- `fork-scoped` -- describes one process; a child recomputes or forgets it.
- `counter` -- an atomic whose inherited value is harmless in a child
  (statistics, sequence numbers, flags).
- `thread` -- per-thread state; in a child only the forking thread's
  survives, and it describes that thread.
- `instance` -- a lock inside an object owned by another registered value.
- `paired` -- a condition variable that waits on a registered lock.
- `test-only` -- compiled only into tests.

A row's `cites` lists the deviation items it does not yet meet; an empty
`cites` means it meets its class today. `rank` is given for `held` rows only.

## Registry

The registry is `registry.tsv`: one row per value, tab-separated, with the
header `id class rank cites`. Empty fields are empty strings; `cites` is a
comma-separated list.

## Fork sites

Every call to `fork` outside tests, including std's `Command::pre_exec`
(whose closure runs in the child before exec), and what its child does:
`exec`, code that is `signal-safe`, or a pool `worker` that runs any code,
forked only through the thread-count gate (FORK-6). Processes are started
with `posix_spawn`, which runs no fork handler; a new `fork` call must be
listed here.

| Site | Child |
|---|---|
| morloc-runtime/fork_policy.rs::morloc_fork_worker | worker |
| morloc-nexus/mcp.rs::frontend_eval | exec |
| morloc-nexus/orchestrate.rs::run_child | exec |
