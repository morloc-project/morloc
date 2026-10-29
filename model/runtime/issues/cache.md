# Result cache and run logs

There is no cache spec yet; these are gaps until one is written.

- **A cached result is keyed by what the pool sources, not by what those
  files import.** [read, 2026-10-29] The key covers the pool's source text,
  the files its language sources, `hash-include` files and the compiler
  version; a header or module those files load in turn is covered only when
  it is listed under `hash-include`.
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
