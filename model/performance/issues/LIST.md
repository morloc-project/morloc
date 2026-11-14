# Performance issues

Analyses of measured slow cases: cause, evidence, solutions reviewed by
the devil's advocate, tests. Each is a child of an entry in
runtime/issues/LIST.md, which stays the one list of known issues; correctness
bugs found along the way are recorded there, not here. Delete a file when
its issue is fixed.

From lib/biolib/runtime-bench (runtime/issues/encode-throughput.md):

- [stream-copies.md](stream-copies.md): stream reads and writes: remaining batch copies (S0, R2, frame joins, writes left on the copy path), an in-place batch below the buffer size holds a sealed buffer, borrowed views, open rulings
- [stdout-copies.md](stdout-copies.md): raw stdout copies each batch up to three times; the pool's sink is the critical path of the out stage
- [whole-list-gather.md](whole-list-gather.md): a whole-list render gathers its batches through a temp file
- [stream-compression-pipeline.md](stream-compression-pipeline.md): compressed streams: which process grows at level 3 (unmeasured)
- [compare-harness.md](compare-harness.md): noisy tolerance; five benches never run; Int in benches; XL-2 control optimized away
- [model-records.md](model-records.md): ZIP-5 limits fail in recorded runs, `Measured: pending` despite runs, ZIP-5/6 frame size mismatch, ZIP-4 answered by data, ZIP-8 title, stale COPY-3/6 prose
- [stdlib-algorithms.md](stdlib-algorithms.md): parallel's streaming fold-maps are quadratic for list and Str monoids
- [call-and-stream-copies.md](call-and-stream-copies.md): measured 2026-11-13: per-packet schema conversion and R schema parsing, mid-size inline values slower than by reference, Python/R @next not in place, channel landing copy, whole-list @load vs the in-place slice, slices left on the copy path, the JSON render copy (small), C++ function values copy list arguments
