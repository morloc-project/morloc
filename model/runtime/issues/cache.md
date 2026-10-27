# Result cache and run logs

There is no cache spec yet; these are gaps until one is written.

- **Stale result after editing a sourced file.** [reproduced] `foo x =
  a@bump x`, label `a: {cache: true}`, foo.py `def bump(x): return x + 1.0`.
  `./prog foo 7` -> 8; change to `x + 100.0`, rebuild, `./prog foo 7` -> 8
  (stale), `./prog foo 8` -> 108. The key's fingerprint hashes the generated
  pool source (library/Morloc/Data/PoolHash.hs:52-104), which only imports
  foo.py. Library sources and the runtime version are not in the key
  either. Open: what the key must cover.
- **Unverified 64-bit keys.** [read] Neither key nor data hash is checked on
  a hit; a data-hash collision skips the write (cache.rs:579-585); an empty
  pool hash is 0, so programs sharing a label share keys (cache.rs:109-117).
- **Padding hashed.** [read] cache.rs ~800, ~817 hash all `width` bytes;
  padding is not zero (shared-memory.md). Spurious misses, never wrong hits.
- **Absolute path in the key file.** [read] A cache shared under another
  mount path (Docker, SLURM) misses or reads another cache's file.
- **Nested program writes the outer summary.json.** [reproduced] A program
  run from a pool inherits `MORLOC_SUMMARY` / `MORLOC_RUN_DIR`;
  morloc-nexus/src/orchestrate.rs:360 strips them only for replay children.
- [read] morloc-nexus/src/main.rs:416: relative `--log-dir` stays relative
  for nested programs.
- [observed] Violates NET-4: cache dir and log run dirs are 0755, and
  `.morloc-debug` lands in the CWD.
- [observed] Benchmark records count cache hits as calls.
- [read] Dead hit/miss counters with a false comment (cache.rs:209-253);
  `start.json` described in run.rs:20-21, never written (golden
  `workdir-basics` skipped).
