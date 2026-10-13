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
  parent's tmpdir is unverified.
- **An empty sacct answer polls forever.** [read] slurm_job_end_state
  treats no output as not finished, so a missing `sacct` or a job purged
  from accounting makes remote_call sleep forever. Empty output is also
  normal for a few seconds after submission, so the fix needs a rule for
  how long "unknown" may last.
- **Job directories outlive a killed parent.** [read] Each job's files sit
  in their own directory under the cache base's `_remote/` and are removed
  by the parent after it reads the result (ANN-12). A parent killed while
  polling leaves the directory and the job running; a failed job's directory
  is kept on purpose and nothing removes it later. Open: should a parent
  cancel its jobs (`scancel`) and remove their directories when it dies, and
  what clears failed jobs' directories?
