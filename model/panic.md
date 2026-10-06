# Panics (PANIC)

A panic is a bug in morloc: a broken invariant in the nexus, libmorloc or
a pool's runtime code. Bad input, an error raised by user code, a peer
that died and a full disk are expected failures, returned as errors and
never raised as panics. After a panic, state may be torn, possibly in
shared memory that other processes use. So a panic ends its process at
once, unless a catch scope holds it, and a catch scope only answers its
client before the process shuts down. Every process reacts the same way.
Model: `tla/PanicExit.tla`.

The internal-error status is exit status 70 (EX_SOFTWARE). A process
exits with it only when a panic ended it.

### PANIC-1 A panic ends its process unless a catch scope holds it
Status: implemented
Checked by: a_panic_outside_a_catch_scope_exits_with_the_internal_error_status, a_panic_inside_a_catch_scope_reaches_the_catch, a_panic_while_unwinding_exits_with_the_internal_error_status, a_libmorloc_panic_on_a_thread_outside_any_catch_scope_exits_with_the_internal_error_status, a_panic_in_a_forked_child_leaves_the_parents_run_alone, every_host_installs_the_panic_hook, tla:PanicExit, tla:PanicExit_hook_exits_in_scope.bug

Each copy of the Rust standard library installs one hook when it is first
entered: the nexus at start, libmorloc when its host starts (the nexus,
the C++ and Rust pools' main, the Python module's initialisation and the R
library's); the Rust pool shares libmorloc's standard library and so its
hook. A morloc throw in the Rust pool is a result, not a panic: it unwinds
without the hook. The hook formats its report into a fixed
buffer and writes it with write(2); naming the thread, or a backtrace when
one is asked for, may allocate. If the panicking thread is inside a catch
scope, is not already unwinding, holds no runtime lock, and the panic is
not a fatal one (a poisoned lock or a lock-rank violation) -- and, when
the scope is a host's around its user code, the thread is not in the
host runtime's own frames -- the hook returns and the panic unwinds to
the catch. It reports a panic a request or library scope holds as an
internal error, and leaves a user panic to the call's failure.
Otherwise the hook runs the panic exit: it kills the process groups of the
pools and child processes it started, removes the shared memory and paths
it registered, and calls _exit(70), even when another thread is already
tearing the process down. libmorloc's hook in the nexus process runs the
nexus's panic exit; in a pool it calls _exit(70). It writes no run
summary, since that takes locks. A robust lock held at _exit reaches other processes as held by a
dead owner, so what it protects is treated as damaged. A second panic
during unwinding also exits 70. A callback a C caller invokes runs outside
any catch scope, since its panic could not unwind to one. A forked child of
the nexus before exec, where a spawn's pre-exec step and libmorloc's fork
handlers run, owns none of its parent's pools, shared memory or paths: the
panic exit there only calls _exit(70).

### PANIC-2 Only request frames catch a panic
Status: deviation

The catch scopes are the daemon's request worker, the MCP HTTP and front
end connection threads, the stdio server's workers and the MCP stdio
loop. The one other catch scope is a call into a third-party format library
(Arrow IPC, Parquet, CSV, JSON) on bytes the program was handed: such a
library may panic on malformed input and cannot be made not to, so the
call returns a decode error and the process goes on. A panic there while
holding a runtime lock, or a fatal one, still ends the process; a
shared-memory allocation it was in is poisoned (SHM-9), and the eval arena
frees its blocks during the unwind. Nothing
else catches a panic; an `extern "C"` function is never a catch scope,
since a panic cannot unwind out of one. A request frame calls the Rust
functions behind the `extern "C"` wrappers it would otherwise use. A panic
in libmorloc during a nexus request cannot reach the nexus's catch, since
the two do not share a standard library, so it ends the process by
PANIC-1 without a reply.

Missing: code below the daemon's handlers still calls libmorloc's own
`extern "C"` functions (DAEMON-8).

### PANIC-3 A caught panic answers its request as failed, then ends the process
Status: implemented
Checked by: a_panic_in_a_forked_child_leaves_the_parents_run_alone, an_http_request_that_panics_is_answered_500_and_the_server_exits_with_the_internal_error_status, a_jsonrpc_call_that_panics_is_answered_with_an_internal_error_and_the_process_exits, a_stdio_request_that_panics_is_answered_as_failed_and_the_process_exits, a_panicking_request_is_answered_500_and_the_daemon_shuts_down_as_failed, a_command_that_exits_with_the_internal_error_status_is_an_internal_error, a_request_finding_the_server_state_poisoned_is_refused_without_using_it, a_stdio_request_that_panics_in_the_daemon_fails_the_daemon_instead_of_exiting, tla:PanicExit, tla:PanicExit_unlock_on_unwind.bug, tla:PanicExit_hook_exits_in_scope.bug, tla:PanicExit_continue_after_catch.bug, tla:PanicExit_reply_twice.bug

The catch answers the request it was serving -- HTTP 500, JSON-RPC error
-32603, a FAIL packet, or the daemon's internal error -- unless any of a
reply was already written, in which case it shuts the connection down;
the MCP stdio loop, whose replies share stdout, exits instead. The process
then takes no new requests -- a server answers a new request 503 -- lets
requests already running finish within the DAEMON-6 grace period, and
exits 70. Requests running during the grace period do not use state the
panic may have torn (PANIC-4). A catch on a thread serving the daemon fails
the daemon, whose own shutdown waits for its requests (DAEMON-7). The daemon's `/eval` reports a child's exit 70 as an internal
error, not as a fault in the caller's expression.

### PANIC-4 A lock a panic unwinds through is marked damaged
Status: implemented
Checked by: a_poisoned_libmorloc_lock_ends_the_process, a_poisoned_lock_inside_a_format_library_call_still_ends_the_process, a_panic_holding_a_runtime_lock_inside_a_format_library_call_ends_the_process, a_request_finding_the_server_state_poisoned_is_refused_without_using_it, tla:PanicExit, tla:PanicExit_unlock_on_unwind.bug

A guard of a lock shared within the process, dropped while its thread is
panicking, poisons what the lock protects. A thread that finds a poisoned
lock treats it as a failure and never uses the data. SHM-9 is the same
rule for locks shared between processes.

### PANIC-5 A pool never answers a panic
Status: implemented
Checked by: golden:py-systemexit-ends-pool, a_libmorloc_panic_on_a_thread_outside_any_catch_scope_exits_with_the_internal_error_status, a_panic_holding_a_runtime_lock_inside_a_format_library_call_ends_the_process, tla:PanicExit_continue_after_catch.bug

A panic in a pool's runtime code -- a Rust panic outside user code, a C++
infrastructure error -- ends the pool by PANIC-1. It is never turned into
a FAIL packet or an error return: user `@try` and `@catch`, Python
`except Exception` and R `tryCatch` would catch it and the pool would go
on serving with torn state. The Rust pool marks its runtime's frames
(decoding, encoding, foreign calls), and libmorloc's hook lets no catch
hold a panic inside one. A C++ pool's internal error exits 70. A Python
exception no handler catches (SystemExit, an interpreter error) ends the
pool in either pool mode. The nexus recovers from the pool's end as SHM-8
says, as it would from a crash.

### PANIC-6 An error in user code is the call's failure
Status: implemented
Checked by: golden:rust-user-panic, golden:rust-error

An exception, error or condition a foreign function raises that its
language's error handling catches (a Python `Exception`, an R condition,
a C++ exception, a panic in user Rust code) is that call's result: the
pool answers with a FAIL packet carrying the message and goes on serving,
and a `@try` in the same pool catches it as it would a throw. A panic on
a thread the user's code started belongs to no call, and ends the pool. A
panic in the destructor of a user value that the Rust pool drops after the
call has made its result is ignored: the result stands. The Rust pool tells it from a panic in its own code by a
per-thread count of its runtime's frames: a panic inside the dispatch
guard with none of them active is the user's, and is answered with a FAIL
packet naming the panic.

### PANIC-7 A background thread has no catch scope
Status: implemented
Checked by: a_libmorloc_panic_on_a_thread_outside_any_catch_scope_exits_with_the_internal_error_status, a_panic_outside_a_catch_scope_exits_with_the_internal_error_status

A thread that serves no request -- the stream sweeper, the lifeline, the
shutdown watchdog, the release service, write-behind compression, a
child's output reader -- catches nothing, so its panic ends the process by
PANIC-1. It never dies alone, which would leak, orphan children, lose the
shutdown bound or hang its consumer.

### PANIC-8 The build unwinds on panic
Status: implemented
Checked by: every_crate_refuses_to_build_without_unwinding

Catch scopes and the generated Rust pool need panics to unwind. The
nexus, libmorloc, the Rust pool's runtime and the generated Rust pool
fail to compile under any other panic strategy.

### PANIC-9 Every frame of the Rust pool's runtime is marked
Status: deviation

PANIC-5 and PANIC-6 tell a runtime panic from a user panic by the frames
the Rust pool's runtime marks. Its decoding, encoding, foreign and remote
calls, caching and spawning are marked. A panic in runtime code that is
not marked becomes the call's failure instead of ending the pool.

Missing: the generated glue between calls, recursive-schema scopes, the
closure and partial-application machinery, and string interpolation are
not marked. Marking the user's calls instead, which the compiler emits as
a closed set, would make every other frame the runtime's.

