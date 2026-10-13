# Known issues

Each file holds a group of related issues, each framed against the model:
a spec item the code violates, or a gap in the spec. Evidence levels:
reproduced, read (verified by reading), speculative. Delete an entry when
it is solved, and a file when it is empty.

- [failure-classification.md](failure-classification.md): misclassified failures (peer death exits 70, bad user data kills the pool) and unbounded or bounded waits
- [wire-bounds.md](wire-bounds.md): unchecked lengths and offsets in packets, stream files and allocation sizes
- [shared-memory.md](shared-memory.md): new blocks not zeroed (SHM-6), registry slot count, two speculative races, stale spec text
- [value-encoding.md](value-encoding.md): non-ASCII schema names, missing record keys, MessagePack decoder bugs, record alignment, canonical bytes
- [cache.md](cache.md): stale results after a source edit, unverified keys, log and summary leaks into nested runs
- [cli-directives.md](cli-directives.md): cross-language @fold, @render type check and -f, multi-output gaps
- [mcp.md](mcp.md): streaming calls refused off the CLI, MCP stdio server in one-shot mode, front-end line cap checked after copying
- [nexus-cli.md](nexus-cli.md): `daemon` mistakes an option's value for the program
- [tables-tensors.md](tables-tensors.md): column type inference, NaN in JSON, tensor dims unchecked, table spec mismatches
- [build-and-text.md](build-and-text.md): non-atomic build swap, stale help text and comments, dead code, morloc.h expression structs drifted from Rust; ABI version never gated, crate versions out of lockstep, mlcKind/morloc.h untested
- [streams.md](streams.md): written streams: nexus stdout lock, open questions on deferred completion, dead openers, stdout placement, @tell, reopen after a daemon call; measured speedups (nested element append, stdin read-ahead)
- [alignment.md](alignment.md): 64-byte numeric array alignment not delivered on every path; guarantee or preference, and for which sizes (deferred)
- [kept-connections.md](kept-connections.md): kept caller connections into Python and R fork pools; affinity deadlock, options (deferred)
- [flaky-tests.md](flaky-tests.md): intermittent test failures with no known cause
- [pools-and-ipc.md](pools-and-ipc.md): pool readiness latency, per-pid sweep race, R close-on-exec, C++ path truncation
