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

Loops all stress tests across six workloads covering every language combination:
C++ only, Python only, R only, C++/Python, C++/R, Python/R.
