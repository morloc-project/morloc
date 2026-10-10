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
- [tables-tensors.md](tables-tensors.md): column type inference, NaN in JSON, tensor dims unchecked, table spec mismatches
- [build-and-text.md](build-and-text.md): non-atomic build swap, stale help text and comments, dead code
- [streams.md](streams.md): written streams: nexus stdout lock; open questions on deferred completion, dead openers, a thread per stream
- [python-pool.md](python-pool.md): thread-mode signal handlers never reach a manifold; fork-mode counter temp dirs leak
