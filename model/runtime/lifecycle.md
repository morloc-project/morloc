# Process lifecycle (LIFE)

How a run starts its pools, decides they are ready, and reports its exit
status.

### LIFE-1 A pool is started with three trailing arguments
Status: draft

The manifest's `exec` command, then the socket path, the run directory and
the shared-memory base name. Environment adds `MORLOC_POOL_HASH` and
`MORLOC_LIFELINE`. Standard streams are inherited unchanged.

### LIFE-2 A pool is ready when it answers a ping
Status: draft

The nexus pings with exponential backoff and fails at once if the pool
process has exited.

### LIFE-3 Exit status of a one-shot run
Status: draft

0 success; 1 call failure (user error, pool death, unreadable argument,
runtime error); 2 usage error (unknown command or option); 70 panic only
(PANIC); 128+N signal N; 141 broken stdout pipe.

### LIFE-4 The run directory is under /tmp with an owner record
Status: draft

`/tmp/morloc.XXXXXX` (mode 0700), owner record `pid start_time boot_id
pidns`, written by rename before anything else lands there. Fixed under
/tmp so a later run's startup sweep finds dead runs.
