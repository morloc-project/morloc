# Topology

## Processes

```
nexus (one per program run, or one long-running daemon / MCP server)
  |-- pool per language (C++, Python, R, Rust, Julia), fork+exec
  |     |-- Python/R fork-mode workers, fork without exec
  |     |-- user code may fork further (multiprocessing, mclapply, fork())
  |-- `morloc eval` / `typecheck` children (daemon only), fork+exec
  `-- sbatch children (SLURM), fork+exec
```

- The nexus creates the shared-memory namespace (volumes and the stream
  registry) and is its only owner: only it unlinks the names while it
  runs. Once a killed nexus's pools have ended, their reapers remove
  them (DAEMON-13).
- Every process maps the same volumes. A relative pointer names a block by
  volume index and offset, so it means the same block in every process.
- A pool exits when its nexus does: it watches a lifeline pipe whose write
  end only the nexus holds.
- A pool's workers end when its main process does: the nexus kills the
  pool's group as it reaps the pool (DAEMON-14).

## Threads

- nexus/daemon: an accept thread (also runs pool-crash recovery), 4-32
  request workers, a SIGCHLD handler that reaps every child.
- pool: dispatch workers (C++, Rust: a job queue in libmorloc; Python
  thread mode: Python threads; Python fork mode and R: forked worker
  processes), the stream sweeper thread, the lifeline thread (C++, R, Rust,
  Julia).
- write-behind compression jobs for output streams.

## Communication

- One unix-socket connection per call, carrying one request packet and one
  reply packet. Small values travel inline; large ones as a relative
  pointer into shared memory.
- Streams: a registry slot in shared memory names the file, its schema and
  its cursor; each process keeps its own descriptor and mapping.
