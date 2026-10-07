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
Checked by: a_panic_in_a_signal_handler_ends_the_process_whatever_scope_it_interrupts, the_pools_own_hook_ends_the_process_on_a_panic_outside_a_catch, the_pools_own_hook_lets_a_panic_in_a_host_scope_unwind, a_panic_outside_a_catch_scope_exits_with_the_internal_error_status, a_panic_inside_a_catch_scope_reaches_the_catch, a_panic_while_unwinding_exits_with_the_internal_error_status, a_libmorloc_panic_on_a_thread_outside_any_catch_scope_exits_with_the_internal_error_status, a_panic_in_a_forked_child_leaves_the_parents_run_alone, every_host_installs_the_panic_hook, tla:PanicExit, tla:PanicExit_hook_exits_in_scope.bug

Each copy of the Rust standard library installs one hook when it is first
entered: the nexus at start, libmorloc when its host starts (the nexus,
the C++ and Rust pools' main, the Python module's initialisation and the R
library's); the Rust pool, whose standard library is its own wherever it
does not share libmorloc's, also installs one in its own that takes
libmorloc's decision. A morloc throw in the Rust pool is a result, not a panic: it unwinds
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

A signal handler is never a catch scope, whatever scope the thread it
interrupted was in. Every handler, and every libmorloc function a nexus
handler calls, runs its body in a signal frame: a panic there writes one
fixed line naming its location with write(2) and calls _exit(70), calling
nothing that is not async-signal-safe.

### PANIC-2 Only request frames catch a panic
Status: implemented
Checked by: a_format_library_panic_is_a_decode_error_and_the_process_goes_on, a_poisoned_lock_inside_a_format_library_call_still_ends_the_process, a_panic_holding_a_runtime_lock_inside_a_format_library_call_ends_the_process, a_panicking_release_callback_ends_the_process_with_70

The catch scopes are the daemon's request worker, the MCP HTTP and front
end connection threads, the stdio server's workers and the MCP stdio
loop. The one other catch scope is a third-party format library (Arrow,
Arrow IPC, Parquet, CSV) decoding bytes the program was handed into the
library's own values: such a library may panic on malformed input and
cannot be made not to, so the decode returns an error and the process
goes on. That catch covers the decode alone, which takes no lock and
writes no shared memory; morloc's conversion of the decoded values into a
block, a read of a block morloc built itself, and a foreign producer's
release callback all run outside it. A panic in the decode while holding
a runtime lock, or a fatal one, still ends the process. Nothing else
catches a panic; an `extern "C"` function is never a catch scope, since a
panic cannot unwind out of one. A request frame calls the Rust functions
behind the `extern "C"` wrappers it would otherwise use. A panic in
libmorloc during a nexus request cannot reach the nexus's catch, since the
two do not share a standard library, so it ends the process by PANIC-1
without a reply.

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
Checked by: golden:py-systemexit-ends-pool, golden:r-internal-error-ends-pool, a_libmorloc_panic_on_a_thread_outside_any_catch_scope_exits_with_the_internal_error_status, a_panic_holding_a_runtime_lock_inside_a_format_library_call_ends_the_process, tla:PanicExit_continue_after_catch.bug

A panic in a pool's runtime code -- a Rust panic outside user code, a C++
infrastructure error -- ends the pool by PANIC-1. It is never turned into
a FAIL packet or an error return: user `@try` and `@catch`, Python
`except Exception` and R `tryCatch` would catch it and the pool would go
on serving with torn state. The Rust pool tells a panic in its runtime
from one in user code as PANIC-9 says. A C++ pool's internal error exits
70; an R worker's exits 70 and its pool ends. A Python
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
call has made its result fails the call (PANIC-10). The Rust pool tells a
user panic from one in its own code as PANIC-9 says, and answers it with
a FAIL packet naming the panic.

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

### PANIC-9 The Rust pool tells a user panic from a runtime panic by where it was raised
Status: implemented
Checked by: a_source_belongs_to_the_user_the_runtime_or_neither, the_first_user_or_runtime_frame_below_the_panic_decides, frames_above_the_panic_machinery_do_not_decide, the_catch_below_the_panicking_code_is_not_the_panic_machinery, an_untrusted_trace_is_the_runtimes, a_trace_without_the_pools_classifier_frame_is_untrusted, an_unresolved_frame_of_unknown_code_is_the_runtimes, a_trace_without_files_is_the_runtimes, a_std_panic_raised_for_user_code_is_the_users, a_std_panic_raised_by_runtime_code_inside_a_user_generic_is_the_runtimes, a_panic_at_a_user_line_is_the_users, a_classification_does_not_depend_on_the_working_directory, golden:rust-panic-sites, golden:rust-runtime-panic, golden:rust-user-panic

User Rust code and the Rust pool's runtime share one language, one panic
mechanism and one standard library, so a panic inside the pool's catch
around user code is the user's only if user code raised it. The hook
decides from where it was raised, before anything unwinds, at no cost
until a panic occurs. The pool registers its generated source file and the
user sources it includes; the runtime's crates know their own source
directories. A panic located in a user source is the user's; one in the
runtime's crates is the runtime's. One located in the generated source, the
standard library or a third-party crate is decided by its caller: the hook
captures a backtrace and walks it from the frame below the panic machinery
that raised the panic (not the catch further down), skipping standard
library and third-party frames; the first frame in a user source or the
runtime decides, and an unresolved frame of other code counts as the
runtime's. The walk trusts its paths only if the pool's classifier frame,
in the generated source, and the runtime's own frames, above the panic
machinery, are recognised; otherwise, or
with no deciding frame, as in a pool built without line tables, the panic
is the runtime's. Rust pools build with line tables so inlined frames are
named. Paths are compared as written and absolute; the backtrace is read in
std's full format, which names files absolutely whatever the working
directory. The walk costs time in proportion to the stack's depth, paid
only by a panic whose location is ambiguous.

### PANIC-10 A result built before a failing destructor is freed
Status: implemented
Checked by: a_reply_built_before_the_manifold_unwinds_is_released_by_the_dispatch

A panic in the destructor of a user value dropped after the Rust pool has
built a call's result packet fails the call, and the dispatch frees the
packet; its shared-memory block is tracked and freed with the dispatch.

### PANIC-11 Every user panic is attributed to the user
Status: implemented
Checked by: a_file_below_a_user_directory_is_the_users_unless_claimed_or_hidden, golden:rust-user-panic-attribution

A panic raised by code the user supplies is a user panic: a sourced file,
a file below a sourced file's directory or a local crate's (unless a
runtime rule claims it or the path passes through a hidden directory), and
a sourced binary operator, which the pool applies through a shim in a
generated user file at no runtime cost.

### PANIC-12 A deployed Rust pool keeps its line tables
Status: implemented
Checked by: golden:rust-panic-sites, golden:rust-user-panic-attribution

The walk of PANIC-9 needs the pool's line tables wherever the pool runs.
On macOS the pool is built with packed debug information and its bundle is
copied beside it, since cargo otherwise keeps line tables in the build
cache's object files.

### PANIC-13 A broken runtime invariant in the Rust pool ends the pool
Status: implemented
Checked by: loading_a_missing_file_fails_with_a_reason, loading_a_null_path_fails_with_a_reason, reading_malformed_json_fails_with_a_reason, reading_a_null_string_fails_with_a_reason, saving_to_a_null_path_fails_with_a_reason, a_runtime_value_failure_without_a_reason_ends_the_pool, a_runtime_handle_failure_without_a_reason_ends_the_pool, a_runtime_failure_with_a_reason_is_a_catchable_error, a_closed_pipe_passes_through_try, a_closed_pipe_fails_the_call_with_its_own_message, a_callee_failure_without_a_reason_ends_the_process_even_inside_a_catch_scope, a_callee_whose_pipe_closed_ends_the_callers_call_past_try, a_pipe_closed_packet_is_a_failure_that_says_so, golden:rust-thunk-capture, golden:stdio-pipe-closed

A check whose failing value can come from outside the runtime -- a client
or peer packet, a file, a user value, handle or type mapping, bytes a user
parser produced -- is an input check, and its failure is an error user
code can catch. A check only a defect in morloc can fail is an invariant,
and its failure ends the pool. The libmorloc calls the Rust pool makes
give a reason for every failure input can cause; libmorloc's intrinsics
treat a failure of their own callee without one as a defect. The Rust
pool ends on such a call's failure that carries no reason, and on a write
walk that leaves the block it allocated. The C++ pool ends the same way,
and Python and R end on a reason-less `@load` failure. A closed downstream pipe is
neither: in every pool language it ends the call and no catch holds it; a
callee answers it with a fail packet marked as a closed pipe, which its
caller raises again; and the nexus decides the exit status.

### PANIC-14 A failed check on data the runtime built ends the pool
Status: implemented
Checked by: a_region_outside_a_runtime_built_value_ends_the_pool, a_region_outside_an_input_value_is_a_catchable_error, a_fold_call_with_a_null_value_ends_the_process

A structural check that fails on a value libmorloc or the Rust pool built
from its own schema -- a value `@read` produced, a fold accumulator, a
stream layout -- is a runtime defect and ends the pool, as does a fold
call with a null argument or a slot index out of range. A stale fold
handle stays a catchable error: a user's thread can outlive its call.

### PANIC-15 Every sourced Rust function's panic is the user's
Status: implemented
Checked by: a_frame_at_a_registered_call_of_the_generated_source_is_the_users, golden:rust-user-panic-outside-sources, golden:rust-user-panic-call-positions, golden:rust-runtime-panic

A panic in a function a sourced Rust file makes callable -- one it
re-exports, a standard-library or dependency path, one in a file pulled
in from outside the sourced file's directory -- is a user panic. The
compiler records the position (line and column, as backtraces give them)
of each call of a sourced function in the generated source: the start of
the callee's name, and for a later argument group of a curried function,
the method that applies it. The walk counts a frame at such a position as
the user's. A frame elsewhere in the generated source is generated code
and never the user's, whatever else shares its line.

### PANIC-16 A sourced call beside generated code is the user's
Status: implemented
Checked by: a_closure_convention_frame_forwards_to_its_caller, golden:rust-user-panic-call-positions

A panic in a sourced function is a user panic wherever the generated source
calls it: with an argument computed inline, beside other code on its line,
or as a later argument group of a curried function. A frame of the
runtime's closure convention (a `MorlocFnN::callN` of this runtime's own
crate, inlined or not) only calls the function value it was given, so the
walk passes over it to its caller; a generated closure it calls has its own
frame deeper in the stack, which decides first.
