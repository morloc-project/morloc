# Performance issues

Analyses of measured slow cases: cause, evidence, solutions reviewed by
the devil's advocate, tests. Each is a child of an entry in
runtime/issues/LIST.md, which stays the one list of known issues; correctness
bugs found along the way are recorded there, not here. Delete a file when
its issue is fixed.

From lib/biolib/runtime-bench (runtime/issues/encode-throughput.md):

- [stream-copies.md](stream-copies.md): stream copies: compressed batches land in shared memory, stdio and frame joins copy, then views (flatten into the buffer, decode in place, move the index fixed)
- [stream-compression-pipeline.md](stream-compression-pipeline.md): compressed streams: which process grows at level 3 (frames per level, decode-ahead, per-process queue bound, oversize copies fixed)
- [compare-harness.md](compare-harness.md): compare.py never compares a bench with its own past, misses unreported Goals, ignores load, noisy tolerance; five benches never run; Int in benches; XL-2 control optimized away
- [model-records.md](model-records.md): ZIP-5 limits fail in recorded runs, `Measured: pending` despite runs, ZIP-5/6 frame size mismatch, ZIP-4 answered by data, ZIP-8 title, stale COPY-3/6 prose
- [stdlib-algorithms.md](stdlib-algorithms.md): `concat`/`concatMap` are quadratic in the number of lists
