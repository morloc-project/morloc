# slurm-bare-metal-mock

Round-trip the bare-metal SLURM dispatch path with mocked `sbatch` and
`sacct`. Verifies that:

1. `Express.hs::decideRemoteness` emits `RemoteCall` for a labeled call
   with `remote:` resources when `slurm-support: true` is set.
2. `libmorloc::remote_call` reads `MORLOC_NEXUS_PATH` (exported by the
   nexus to every pool) instead of falling back to a hardcoded
   `./nexus`.
3. `submit_morloc_slurm_job` shells out to `sbatch --parsable ...`,
   reads back the job id, and polls `slurm_job_is_complete` via
   `sacct`.
4. The wrap command `<nexus> --call-packet ...` reaches a pool and
   produces a result packet readable from `.morloc-cache/`.

The mock shims in `./mock-bin/` make sbatch run the wrap synchronously
(so the result is on disk by the time sacct is polled) and report a
fixed job id. This collapses the host/compute-node distinction onto a
single machine, which is fine for the codegen + runtime contract;
real-cluster correctness (separate compute-node pools, shared FS) is
covered by the `slurm-container-mock` test.

## Prerequisite

`morloc init -f --slurm` must have been run at least once. This writes
`slurm-support: true` to `~/.config/morloc/build.yaml`, which the
compiler reads to enable `RemoteCall` codegen. Without this, the
labeled call falls through `decideRemoteness` to `ForeignCall` and no
sbatch is ever invoked.

## Running

```sh
make
diff exp.txt obs.txt
```

Not registered in `test-suite/Main.hs`: enabling SLURM modifies global
morloc state, which is unsuitable for the always-on stack-test bath.
The user runs this manually after `morloc init -f --slurm`.
