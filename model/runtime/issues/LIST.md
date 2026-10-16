# Known issues

Each file holds a group of related issues, each framed against the model:
a spec item the code violates, or a gap in the spec. Evidence levels:
reproduced, read (verified by reading), speculative. Delete an entry when
it is solved, and a file when it is empty.

- [failure-classification.md](failure-classification.md): misclassified failures (peer death exits 70, bad user data kills the pool) and unbounded or bounded waits
- [wire-bounds.md](wire-bounds.md): unchecked lengths and offsets in packets, stream files and allocation sizes
- [shared-memory.md](shared-memory.md): new blocks not zeroed (SHM-6), registry slot count, two speculative races, stale spec text; a free in an eval arena scans the arena (quadratic scopes); the block census counts only mapped volumes; an allocation that misses its cursor walks the volume
- [value-encoding.md](value-encoding.md): R integer literals ignore their type (U64 map fails by input order), R `==` on I64 compares bit patterns; non-ASCII schema names, missing record keys, MessagePack decoder bugs, record alignment, canonical bytes; Python and R `F32` scalars lose signalling NaNs (WIRE-7)
- [cache.md](cache.md): stale results after a source edit, unverified keys, log and summary leaks into nested runs
- [cli-directives.md](cli-directives.md): cross-language @fold, @render type check and -f, multi-output gaps
- [mcp.md](mcp.md): streaming calls refused off the CLI, MCP stdio server in one-shot mode, front-end line cap checked after copying
- [nexus-cli.md](nexus-cli.md): `daemon` mistakes an option's value for the program; `-f voidstar` output of a large value is labelled a shared-memory reference
- [tables-tensors.md](tables-tensors.md): column type inference, NaN in JSON, tensor dims unchecked, table spec mismatches
- [build-and-text.md](build-and-text.md): non-atomic build swap, stale help text and comments, dead code, morloc.h expression structs drifted from Rust; ABI version never gated, crate versions out of lockstep, mlcKind/morloc.h untested; a pool's newer crate pins dropped from the environment lock, which keeps a table per program; a native type form does not bring in its defining file (needs a ruling)
- [streams.md](streams.md): written streams: nexus stdout lock, open questions on deferred completion, dead openers, stdout placement, @tell, reopen after a daemon call; measured speedups (nested element append, stdin read-ahead); a forked child inherits its parent's cache byte count; slots may decode one offset ahead twice after a reset; the per-process queue bound (SLOT-32) wakes late, has no setting, and is not host-wide; every daemon call sweeps the whole registry; per-operation scans over growing slot, cell, temp-file and view lists
- [alignment.md](alignment.md): 64-byte numeric array alignment not delivered on every path; guarantee or preference, and for which sizes (deferred)
- [kept-connections.md](kept-connections.md): kept caller connections into Python and R fork pools; affinity deadlock, options (deferred)
- [flaky-tests.md](flaky-tests.md): intermittent test failures with no known cause; the integration suite deletes every morloc run's shm and tmp dirs; a nexus waits forever when its run dir is deleted; a forked test child blocks on a lock held at fork; bare-forked children hold other tests' descriptors; a rare full-run hang
- [pools-and-ipc.md](pools-and-ipc.md): pool readiness latency, per-pid sweep race, R close-on-exec, C++ path truncation; macOS sockets not close-on-exec from birth; a socket made across a fork stays open in the child
- [remote-dispatch.md](remote-dispatch.md): SLURM jobs: inherited parent environment, endless poll on empty sacct, job directories left by a killed parent
- [rust-closures-threads.md](rust-closures-threads.md): function values on callback threads: host panics end the pool, late calls unspecified, refcount contention, boxed direct closures; opaque capture front-end error
- [encode-throughput.md](encode-throughput.md): perf: stream decode copies every field (slower than parsing the text), @write slower than a synced plain write, @flush per batch ~1.8x with zstd, table arguments over TBL-1/TBL-2 (record lists through JSON); analyses and reviewed fixes in [../../performance/issues/](../../performance/issues/LIST.md)
- [environment-deps.md](environment-deps.md): declared tools resolve through the inherited PATH; unknown package.yaml keys are ignored
- [ifile-access.md](ifile-access.md): IFile index does not compile in Rust pools; projections after an IFile bracket decode whole elements
