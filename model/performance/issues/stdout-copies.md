# Raw stdout copies each batch up to four times

Child of runtime/issues/encode-throughput.md. The file write path is in
stream-copies.md; this file covers stdout. Fix plan:
/work/plans/output-path-perf/PLAN.md, step 2.

## Evidence

mosm `filter` (gencode v50, 1.5 GB out, `-t 1`): copies of a batch after
the handler returns its `Str`:

1. `_mlc_write` stages it with `to_voidstar` into SHM;
2. `append_one_element` into the slot write buffer;
3. `compact_sealed_buffer` (custody.rs), no memmove in practice;
4. `send_stdio` joins the header and the payload (custody.rs ~801).

Items 3-4 run in the nexus, which then writes the bodies from the joined
buffer. With `-z N -f raw` the pool also compresses each batch, and the
nexus decompresses it, materializes it in SHM and renders it.

## Measurements

2026-11-09 at 7760abbdc, mosm `filter -t 1`, 1.48 GB raw out, temporary
timers: the pool's sink is the critical path of the out stage (1.25 s).
The sink took 1.14 s: the `toFastaEntries` render 0.26 s, a list literal
copy of the body 0.17 s, `_mlc_write` (stage and append) 0.59 s, frees
and `@try` about 0.12 s. The list copy is gone since (a let bound in a
`@try` body moves; golden copy-census-render-cpp); mosm's wall time did
not change beyond noise, and the pool's peak fell 272 -> 257 MiB. The nexus
join (item 4) costs about 0.26 s of nexus CPU, off the critical path.
Scrub on free about 0.19 s, split across both processes.

## Compression on stdio slots

With `-z N` and a non-packet output form (or an `@write` level above 0 on
`@stdout`), the pool compresses each stdout sub-packet and the nexus
decompresses it to render it. A first fix set the pools' stdout level to 0
through their environment (2026-11-08, reverted before commit). It could
not be made correct that way:

- `MORLOC_STDOUT_COMPRESSION_LEVEL` applies to every stdio slot, stderr
  included;
- the output form can be decided after the pools start (MCP, serve, a
  recovery restart);
- the value leaks into nested morloc runs;
- a second variable would raise the environment-read ratchet
  (source_rules.rs `MAX_ENV_READS`).

The raw writer declines a compressed sub-packet, which then takes the
materialize path (correct, slower). The pool compresses before the nexus
sees the bytes, so the level has to be chosen where the pool knows the
output form, not in the nexus. Or the nexus could decompress into a
local buffer and run the raw writer on it, skipping the materialize.
Separately, a nexus started without `-z` inherits an
enclosing run's `MORLOC_STDOUT_COMPRESSION_LEVEL` and passes it on to its
pools (main.rs ~455), so a nested run's stdout is compressed at the outer
`-z`.
