# Stress Tests

Tests for process management, resource cleanup, and stability under load.

## Interface

All tests take a golden test directory and one or more nexus calls:

```bash
./<test>.sh <golden-test-dir> <call> [<call> ...] [-- options...]
```

A call is a nexus subcommand with arguments, e.g. `"foo '[1,2,3]'"`.

## Tests

**zombie-stress.sh** `[-- iterations]` — Runs the nexus repeatedly (default 50)
and checks that no shared memory segments or temp directories accumulate.

**concurrent-stress.sh** `[-- concurrent rounds]` — Launches multiple nexus
invocations simultaneously (default 10x10) to test for resource leaks under
contention.

**crash-recovery.sh** `[-- iterations]` — Kills a pool process mid-execution
with SIGKILL (default 10 iterations) and verifies the nexus exits promptly
without leaking resources.

**valgrind-check.sh** — Runs the nexus under valgrind and checks for large
memory leaks (>4KB) or file descriptor leaks (>3 extra). Requires valgrind.

**deep-recursion.sh** — Takes no arguments. Builds the programs under
`deep-recursion/` (one single-pool instance per language from
`tree.loc.tmpl`, plus a Python/C++ cross-pool module) and runs tail
recursion over recursive `data` values, mutual recursion, non-tail recursion
and a nexus print/parse round trip at depth 10000 (`MORLOC_TEST_LEVEL=long`:
100000). Cases known to fail carry the issue that tracks them and report
XFAIL; the marking is strict, so a fixed case reports XPASS and fails the
suite until its marking is removed. `MORLOC_STRESS_LANGS="py cpp"` restricts
the single-pool instances (the Rust build is most of the run time).

## Examples

```bash
# Single test, single workload
./zombie-stress.sh ../golden-tests/interop-3a-cp "foo '[1,2,3]'" -- 100

# Crash recovery on R-only workload
./crash-recovery.sh ../golden-tests/argument-form-1-r "foo 2" -- 20

# Run all tests across all language combinations
./run-all.sh

# Run only zombie and concurrent tests
./run-all.sh zombie concurrent
```

## run-all.sh

Loops the workload tests across six workloads covering every language
combination: C++ only, Python only, R only, C++/Python, C++/R, Python/R.
Then runs `deep-recursion.sh` once (select it alone with `./run-all.sh deep`).
