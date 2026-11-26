# Process lifecycle (LIFE)

How a run starts its pools, decides they are ready, and reports its exit
status.

### LIFE-1 A pool is started with three trailing arguments
Status: draft

The manifest's `exec` command, then the socket path, the run directory and
the shared-memory base name. Environment adds `MORLOC_POOL_HASH` and
`MORLOC_LIFELINE`. Standard streams are inherited unchanged.

### LIFE-2 A pool is ready when it answers a ping
Status: implemented
Checked by: a_pool_is_seen_ready_as_soon_as_it_answers, a_pool_that_dies_while_starting_is_reported_at_once

The nexus connects to the pool's socket, retrying every few milliseconds
while it does not yet exist, sends one ping and waits on that connection
for the answer. The pool is ready the moment it answers. The wait ends
early, with an error, when the pool's process exits or shutdown is
requested; it has no other limit (DAEMON-6), since a pool may spend long
loading its libraries. A probe the pool closes unanswered is sent again.

### LIFE-3 Exit status of a one-shot run
Status: draft

0 success; 1 call failure (user error, pool death, unreadable argument,
runtime error); 2 usage error (unknown command or option); 70 panic only
(PANIC); 128+N signal N; 141 broken stdout pipe. A pool exits 71 when
shared memory runs out (SHM-17); the run still exits 1. A pool or nexus
that refuses another build of morloc exits 75, and so does the run
(LIFE-6). A pool reply that reports
the closed pipe ends the run with 141 and nothing on stderr, the same as
the nexus finding the pipe closed itself, whichever comes first.
A pool reply that reports a dead peer (LIFE-5) ends the run with 1 and
its message.

### LIFE-5 A call during which a peer died reports the death, whatever user code did
Status: implemented
Checked by: golden:spec-fail-6-1, golden:cpp-catch-cannot-hold-pool-death

A pool process counts the peer deaths it sees: a call it makes whose
connection is lost before the whole reply (the connect fails, a send on a
new connection fails, the read fails), and a reply that itself reports a
dead peer. The failed call fails in the caller's language like any failed
call, so user code unwinds and may catch it, but when the count moved
while a dispatch ran, that dispatch replies with a dead-peer packet
(status 3) instead of its value (FAIL-6). A closed downstream pipe is
counted the same way and answers with the closed-pipe status (PANIC-13).
The count is per process, so another dispatch running in the same pool at
the time also reports the death.

### LIFE-4 The run directory is under /tmp with an owner record
Status: draft

`/tmp/morloc.XXXXXX` (mode 0700), owner record `pid start_time boot_id
pidns`, written by rename before anything else lands there. Fixed under
/tmp so a later run's startup sweep finds dead runs.

### LIFE-6 A process runs only on the build of morloc it was made with
Status: implemented
Checked by: a_process_runs_only_on_the_build_it_was_made_with, the_build_id_is_the_hash_of_the_runtime_source_wherever_it_lies, a_pool_refuses_another_build, every_binding_checks_the_build_it_was_made_with

The build id is per run: it catches an install changing under a run, not
a program built against an older runtime, whose compatibility
MORLOC_ABI_VERSION alone governs, so a runtime update never forces a
rebuild. The id is a hash of the runtime source (the Rust workspace's
runtime, types and manifest crates, the lock file and the workspace's
`include/morloc.h`), so a build and its persisted copy agree; only Rust
sources, the Cargo files and that header count.
libmorloc reports it as `morloc_build_id()`; init fails when the nexus it
installed refuses the libmorloc it installed. The
nexus, installed with libmorloc, refuses a libmorloc whose id differs from
its own, and passes the id to its pools as `MORLOC_BUILD_ID`, replacing any
it inherited. A pool calls `morloc_require_build` before anything else
reaches libmorloc and refuses when the libmorloc it loaded differs from
the nexus's id; the nexus checks itself the same way first. The bindings installed beside libmorloc and loaded at run
time, pymorloc and rmorloc, record at init (`morloc-nexus --build-id`) the
id they were built against and refuse a loaded libmorloc of another.
Code compiled into a program (libcppmorloc in a C++ pool, the runtime
crates in a Rust pool) records no id. A refusing process prints "morloc
was upgraded while this program was starting; run it again" and exits 75.
A run whose pool exits 75 while starting ends with 75 and adds nothing to
the lines its refusing pools print. A daemon whose respawned pool exits 75
says that morloc was upgraded and that it must be restarted, and exits 75
rather than retrying. `morloc_require_build`'s signature never
changes. Init writes each binding, header and archive beside its
destination and renames it onto it, file by file, since the include
directory also holds module headers.
