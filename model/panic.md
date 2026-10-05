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
Status: deviation

Each copy of the Rust standard library installs one hook when it is first
entered: the nexus at start, libmorloc in the initialisation every host
calls, and the Rust pool. The hook formats its report into a fixed buffer
and writes it with write(2); only a backtrace, when one is asked for,
allocates. If the panicking thread is inside a catch scope and is not
already unwinding, the hook returns and the panic unwinds to the catch.
Otherwise the hook runs the panic exit: it kills the process groups of the
pools and child processes it started, removes the shared memory and paths
it registered, and calls _exit(70), even when another thread is already
tearing the process down. It writes no run summary, since that takes
locks. A robust lock held at _exit reaches other processes as held by a
dead owner, so what it protects is treated as damaged. A second panic
during unwinding also exits 70. A callback a C caller invokes runs outside
any catch scope, since its panic could not unwind to one. The only nexus
code that runs in a forked child is a spawn's step before exec, which makes
only libc calls and cannot panic.

Missing: libmorloc installs no hook, so a panic in its code aborts (134)
when it reaches an `extern "C"` function and kills only its thread
otherwise.

### PANIC-2 Only request frames catch a panic
Status: deviation

The catch scopes are the daemon's request worker, the MCP HTTP and front
end connection threads, the stdio server's workers and the MCP stdio
loop. Nothing else catches a panic; an `extern "C"` function is never a
catch scope, since a panic cannot unwind out of one. A request frame calls
the Rust functions behind the `extern "C"` wrappers, so a panic anywhere in
its request reaches its catch. Decoding foreign bytes returns an error
rather than catching a panic, since bad bytes are an expected failure.

Missing: request handlers call `extern "C"` wrappers (DAEMON-8); seventeen
Arrow entry points catch panics and return them as errors.

### PANIC-3 A caught panic answers its request as failed, then ends the process
Status: implemented
Checked by: an_http_request_that_panics_is_answered_500_and_the_server_exits_with_the_internal_error_status, a_jsonrpc_call_that_panics_is_answered_with_an_internal_error_and_the_process_exits, a_stdio_request_that_panics_is_answered_as_failed_and_the_process_exits, a_panicking_request_is_answered_500_and_the_daemon_shuts_down_as_failed, a_command_that_exits_with_the_internal_error_status_is_an_internal_error, a_request_finding_the_server_state_poisoned_is_refused_without_using_it, a_stdio_request_that_panics_in_the_daemon_fails_the_daemon_instead_of_exiting, tla:PanicExit, tla:PanicExit_unlock_on_unwind.bug, tla:PanicExit_hook_exits_in_scope.bug, tla:PanicExit_continue_after_catch.bug, tla:PanicExit_reply_twice.bug

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
Status: deviation

A guard of a lock shared within the process, dropped while its thread is
panicking, poisons what the lock protects. A thread that finds a poisoned
lock treats it as a failure and never uses the data. SHM-9 is the same
rule for locks shared between processes.

Missing: the MCP servers refuse a request that finds their state poisoned;
about twenty sites elsewhere recover a poisoned mutex and use its data.

### PANIC-5 A pool never answers a panic
Status: deviation

A panic in a pool's runtime code -- a Rust panic outside user code, a C++
infrastructure error -- ends the pool by PANIC-1. It is never turned into
a FAIL packet or an error return: user `@try` and `@catch`, Python
`except Exception` and R `tryCatch` would catch it and the pool would go
on serving with torn state. The nexus recovers from the pool's end as
SHM-8 says, as it would from a crash.

Missing: the Rust pool aborts (134); C++ infrastructure errors abort;
Python thread mode loses one thread to a BaseException and keeps serving.

### PANIC-6 An error in user code is the call's failure
Status: deviation

An exception, error or condition raised by a foreign function, of any
type, is that call's result: the pool answers with a FAIL packet carrying
the message and goes on serving. A panic in a user Rust function is such
an error. The Rust pool tells it from a panic in its own code by a
per-thread count of runtime frames entered: a panic with no runtime frame
entered since the user function was called is the user's.

Missing: the Rust pool aborts on any panic other than a morloc throw. C++,
Python and R already answer user errors with a FAIL packet.

### PANIC-7 A background thread has no catch scope
Status: deviation

A thread that serves no request -- the stream sweeper, the lifeline, the
shutdown watchdog, the release service, write-behind compression, a
child's output reader -- catches nothing, so its panic ends the process by
PANIC-1. It never dies alone, which would leak, orphan children, lose the
shutdown bound or hang its consumer.

Missing: these threads die alone; the release service is restarted;
write-behind returns a panic as a job error.

### PANIC-8 The build unwinds on panic
Status: deviation

Catch scopes and the generated Rust pool need panics to unwind. The
nexus, libmorloc and the Rust pool fail to compile under any other panic
strategy.

Missing: nothing checks the strategy at build time.
