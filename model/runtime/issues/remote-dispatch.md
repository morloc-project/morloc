# Remote dispatch (SLURM)

A remote label (`remote:` in a labeled group) makes each call a SLURM job
that replays a call packet with the program's nexus. No item covers remote
dispatch beyond ANN-9, ANN-10 and ANN-12. Goldens `spec-ann-9-1` and
`spec-ann-12-1` run the path against a mock `sbatch`/`sacct` (`morloc make
--slurm`).

- **A job inherits its parent run's private environment.** [reproduced]
  sbatch exports the caller's environment by default, so the job's nexus
  sees MORLOC_LIFELINE, MORLOC_NEXUS_STDIO_SOCK and MORLOC_BENCH_RECORDS of
  the run that submitted it. Harm beyond a job's benchmark rows going to the
  parent's tmpdir is unverified. [reproduced 2026-11-02] Also
  MORLOC_POOL_HASH, MORLOC_MANIFEST_PATH, MORLOC_NEXUS_PATH and
  MORLOC_PROG_NAME; one shared list of private names should drive both
  this and the pool environment filter.
- **An empty sacct answer polls forever.** [read] slurm_job_end_state
  treats no output as not finished, so a missing `sacct` or a job purged
  from accounting makes remote_call sleep forever. Empty output is also
  normal for a few seconds after submission, so the fix needs a rule for
  how long "unknown" may last. [reproduced 2026-11-02] With sacct missing
  the shell prints `sh: 1: sacct: not found` every second; spawn it
  directly and treat a spawn failure as an error. The SLURM bridge path
  also polls forever if the bridge dies after submission [read].
- **Job directories outlive a killed parent.** [read] Each job's files sit
  in their own directory under the cache base's `_remote/` and are removed
  by the parent after it reads the result (ANN-12). A parent killed while
  polling leaves the directory and the job running; a failed job's directory
  is kept on purpose and nothing removes it later. Open: should a parent
  cancel its jobs (`scancel`) and remove their directories when it dies, and
  what clears failed jobs' directories?
- **A gather or fold fed from a remote job fails at run time.** [reproduced
  2026-11-15] `@collect r@produce` hands the sink to a SLURM job, a separate
  process with its own shared memory. A native fold's id names no entry
  there ("a native fold handle reached another process", C++ and Rust,
  whose folds always hold their slots natively); the file route's stream
  handle names no slot there either ("stream handle ...: generation
  mismatch"), for a Python fold (FOLD-9) and for a gather in Python or C++.
  So no route works for a remote producer; FOLD-9 and SIDE-4 say nothing
  about one. Reproduction: `spec-ann-9-1`'s `main.yaml` and `mock-bin`,
  `morloc make --slurm`, and `run = @collect r@produce` with a fold
  handler (`@render --n=show @fold=add @init=zero @combine=merge`) and
  `produce sink = for b in 1..3: sink([b])` in C++, Rust or Python; or a
  plain gather handler. Open: should the compiler refuse a sink that
  reaches a remote call (handleCrosses sees a direct `MonoPoolCall ...
  RemoteCall`, not one made by another pool), or should a job write its
  batches to a file the parent replays?
