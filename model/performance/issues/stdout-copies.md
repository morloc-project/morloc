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
buffer. A stdout batch is compressed only for packet output (SLOT-33).

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
