# TODO

- [x] Zombie process / resource exhaustion — fixed via three-phase cleanup in
  nexus.c, SIGCHLD handler, and C++ pool SIGTERM handler

- [ ] Crash recovery leaks resources for Python/R pools — when a pool is killed
  with SIGKILL mid-execution, the nexus exits but doesn't clean up
  /dev/shm/morloc-* and /tmp/morloc.* (C++ only passes because its SIGTERM
  handler enables graceful shutdown before escalation to SIGKILL)

- [ ] R pool has no signal handling — no SIGTERM handler, uses
  mcparallel(detached=TRUE) creating untracked children, no periodic mccollect
  to reap finished children. detached=FALSE was attempted but causes ~60MB/run
  memory leak from accumulated mccollect results.

- [ ] Nexus doesn't detect mid-execution pool crash until socket timeout — no
  proactive check that pool PIDs are still alive during command execution
  (SIGCHLD marks pid as -1 but nothing acts on it until the next socket
  operation fails)
