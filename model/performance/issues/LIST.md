# Performance issues

Analyses of measured slow cases: cause, evidence, solutions reviewed by
the devil's advocate, tests. Each is a child of an entry in
runtime/issues/LIST.md, which stays the one list of known issues; correctness
bugs found along the way are recorded there, not here. Delete a file when
its issue is fixed.

From lib/biolib/runtime-bench (runtime/issues/encode-throughput.md):

- [stream-copies.md](stream-copies.md): stream reads and writes copy each batch several times; flatten into the buffer, decode inside the claim, then views
- [stream-compression-pipeline.md](stream-compression-pipeline.md): `@flush` runs compression on one thread; memory bounded per stream, not per process; reads decode on one thread; frames per level
