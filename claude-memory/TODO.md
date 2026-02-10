# TODO

## Zombie Process / Resource Exhaustion in Test Suite

**Problem:** Running `stack test` spawns many parallel processes (morloc make,
gcc, g++, python, R) that create zombie (defunct) processes. These zombies
accumulate and exhaust the system's process table, causing subsequent tests
(and the test harness itself) to fail with "Resource temporarily unavailable"
or "Cannot fork" errors.

**Symptoms:**
- `stack test` shows many tests FAIL with `obs.txt: openBinaryFile: does not exist`
- Error messages include: `failed to create OS thread: Resource temporarily unavailable`
- `ps aux | grep -c defunct` shows ~2000 zombie processes
- Zombies are parented to PID 1 (orphaned)
- Even `-j 1` doesn't help once zombies accumulate from a previous run

**To reproduce:**
1. Run `stack test` (all 531 golden tests)
2. If it crashes partway, run `ps aux | grep -c defunct` to see zombie count
3. Subsequent `stack test` runs will fail immediately due to accumulated zombies

**Root cause hypothesis:** The golden test harness runs `make clean && make` for
each test, which spawns `morloc make` (Haskell), then `gcc` and `g++` (and
sometimes `python3` and `Rscript`). The test harness may not properly wait for
all child processes, leaving them as zombies. The issue may be in:
- The test runner (`test-suite/Main.hs` or the tasty framework)
- The `morloc make` subprocess management
- The `make` invocations in each test Makefile

**Workaround:** Restart the container/session to clear zombie processes, then
run tests. Or run small batches: `stack test --test-arguments="-p serial-form-1"`

**Fix needed:** Investigate the test harness process management. Ensure all
child processes are properly waited on. Consider using `waitForProcess` or
similar in the Haskell test code.
