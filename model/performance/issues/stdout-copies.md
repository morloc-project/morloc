# Raw stdout copies each batch six or seven times

Child of runtime/issues/encode-throughput.md. The file write path is in
stream-copies.md; this file covers stdout. Fix plan:
/work/plans/output-path-perf/PLAN.md, step 2.

## Evidence

mosm `filter` (gencode v50, 1.5 GB out, `-t 1`, load 3-4): the out stage
takes 1.54 s; the C++ baseline's `fwrite` takes 0.16 s. Copies of a batch
after the handler returns its `Str`:

1. a list literal copies its element (compiler/issues/codegen-copies.md);
2. `_mlc_write` stages it with `to_voidstar` into SHM;
3. `append_one_element` into the slot write buffer;
4. `compact_sealed_buffer` (custody.rs);
5. `send_stdio` joins the header and the payload (custody.rs ~801);
6. the nexus `materialize`s the sub-packet into SHM (stdio_bridge.rs:309);
7. `print_voidstar_raw`: a UTF-8 scan, then a 64 KiB `BufWriter`.

Items 4-7 run in the nexus. With `-z N -f raw`, the pool also compresses
each batch and the nexus decompresses it (stream.rs ~3995). The measured
share of each copy is pending (plan step 0.4).

## Measurements

2026-11-08, c3a7b3bb3, mosm `filter -t 1`, 1.48 GB raw out, temporary
timers (/work/plans/output-path-perf/PLAN.md, "Stdout copy timing"):

- pool: stage 0.154 s, append 0.302 s;
- nexus: compact 0.002 s, join about 0.26 s, materialize 0.294 s, render
  and write 0.171 s, sink total 0.596 s;
- scrub on free 0.194 s, split across both processes.

The nexus path (about 0.85 s, serial) is the critical path.

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

Do it with plan step 2b, on the stdout slot itself, where the nexus
handles the bytes. Separately, a nexus started without `-z` inherits an
enclosing run's `MORLOC_STDOUT_COMPRESSION_LEVEL` and passes it on to its
pools (main.rs ~455), so a nested run's stdout is compressed at the outer
`-z`.
