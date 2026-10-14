# Performance issues

Analyses of measured slow cases: cause, evidence, solutions reviewed by
the devil's advocate, tests. Each is a child of an entry in
runtime/issues/LIST.md, which stays the one list of known issues; correctness
bugs found along the way are recorded there, not here. Delete a file when
its issue is fixed.

From lib/biolib/runtime-bench (runtime/issues/encode-throughput.md):

- [rust-vector-encode.md](rust-vector-encode.md): Rust clones every batch bound out of a `Try` (C++ moves); needs a Rust use-after-move check; borrowed slices later
- [stream-copies.md](stream-copies.md): stream reads and writes copy each batch several times; flatten into the buffer, decode inside the claim, then views
- [stream-compression-pipeline.md](stream-compression-pipeline.md): compressed streams: memory bounded per stream, not per process; oversize elements copied four times; no decode-ahead (frames per level fixed)
