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
(PANIC); 128+N signal N; 141 broken stdout pipe. A pool reply that reports
the closed pipe ends the run with 141 and nothing on stderr, the same as
the nexus finding the pipe closed itself, whichever comes first.

### LIFE-4 The run directory is under /tmp with an owner record
Status: draft

`/tmp/morloc.XXXXXX` (mode 0700), owner record `pid start_time boot_id
pidns`, written by rename before anything else lands there. Fixed under
/tmp so a later run's startup sweep finds dead runs.
