# Performance issues

Analyses of measured slow cases: cause, evidence, solutions reviewed by
the devil's advocate, tests. Each is a child of an entry in
runtime/issues/LIST.md, which stays the one list of known issues; correctness
bugs found along the way are recorded there, not here. Delete a file when
its issue is fixed.

From lib/biolib/runtime-bench (runtime/issues/encode-throughput.md):

- [stream-copies.md](stream-copies.md): stream copies: compressed batches land in shared memory, stdio and frame joins copy, then views (flatten into the buffer, decode in place, move the index fixed)
- [stream-compression-pipeline.md](stream-compression-pipeline.md): compressed streams: memory bounded per stream, not per process; oversize elements copied four times (frames per level fixed; decode-ahead fixed, SLOT-26, SLOT-27)
