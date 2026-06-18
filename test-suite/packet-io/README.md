# packet-io rugged tests

Wire-format correctness and round-trip tests for the packet I/O path,
the Layer-1/2/3 SHM encoding work, and the compression pipeline.

## Tiers

| Tier | Script | Coverage |
| ---- | ------ | -------- |
| A | `cargo test -p morloc-runtime` | Unit-level: encoding helpers, vol_idx hint walk, read_binary_with_hint round-trip. Sub-second. |
| B | `tier-b-roundtrip.sh` | Single-process round-trip across (no compression, z=1, z=3) and transcoding (z=3 -> z=1). Catches bug #1 (legacy reader missing vol_idx hint). |
| C | `tier-c-crossvol.sh` | Workload large enough that the geometric grow allocator places its sub-arrays into multiple SHM volumes. Verifies indexed relptrs survive cross-volume references. |
| D | `tier-d-crosspool.sh` | Byte-level wire equivalence between Python and C++ pools. Catches any divergence between cppmorloc.cpp and pymorloc.c on the encoded relptr format. |
| E | `tier-e-stress.sh` | Size sweep spanning the 64 KiB inline threshold and the 1 MiB Layer-2 fast-path threshold. Includes per-iteration leak audit of /dev/shm and /tmp/morloc.*. |

## Conventions

- Each tier compiles `workload/main.loc` into its own temp directory
  so it can run independently.
- Each tier prints per-step wall time (`<label>  <ms>ms  rc=<n>  bytes=<m>`).
- Failure mode is a non-zero exit and a clear `FAIL` line. Success is
  a `PASS` line and exit 0.
- `run-all.sh` invokes every tier and prints a final summary table.

## Interface

```
./tier-b-roundtrip.sh [outer_count] [str_len]
./tier-c-crossvol.sh  [outer_count] [str_len]
./tier-d-crosspool.sh [outer_count] [str_len]
./tier-e-stress.sh
./run-all.sh
```

Defaults are tuned for "few seconds" runs; raise the parameters for
heavier exercises.

## Adding a tier

1. Drop a new `tier-X-<name>.sh` in this directory.
2. `source "$(dirname "$0")/common.sh"` for `time_call`, `compile_workload`,
   `count_shm`, `count_tmp`.
3. Make it produce a `PASS` / `FAIL` line and a non-zero exit on failure.
4. Add it to the `TIERS` list in `run-all.sh`.
