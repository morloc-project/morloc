# slurm-container-mock

End-to-end exercise of the containerized SLURM dispatch path:
`morloc-manager run --slurm-bridge` -> driver container ->
`libmorloc::remote_call` -> `MORLOC_BRIDGE_SOCKET` -> host bridge
thread -> `apptainer exec <env-sif> sh -c '<nexus> --call-packet ...'`
-> sbatch -> compute-node nexus -> result packet to shared cache ->
back to driver.

The mocks in `./mock-bin/` go on the **host** PATH only. The container
has neither `sbatch` nor `sacct`, so the test fails immediately if the
bridge isn't doing its job.

## Prerequisites

Run once on the host:

```sh
# 1. Enable RemoteCall codegen globally.
morloc init -f --slurm

# 2. Create an Apptainer env, install the root libs.
morloc-manager new e2e-slurm --engine apptainer
morloc-manager run -- morloc init -f --slurm
morloc-manager run -- morloc install root root-py
morloc-manager select e2e-slurm
```

The env's `.sif` lives under `~/.local/share/morloc/environments/e2e-slurm/sif/`.
`morloc-manager run --slurm-bridge` reads `layered_sif.unwrap_or(base_sif)`
from the env config and bind-mounts the bridge socket into the container.

## Running

```sh
make
diff exp.txt obs.txt
```

The shim's `--wrap` parser strips `apptainer exec '<sif>' sh -c '<inner>'`
and runs the inner nexus command on the host. The host has the same
morloc runtime as the container (path-mirrored via Apptainer's `$HOME`
auto-mount), so the recursive `nexus --call-packet` invocation finds
the pool the driver started and the call round-trips.

Not registered in `test-suite/Main.hs`: requires Apptainer plus
host-side morloc-manager + state mutation. Run manually.
