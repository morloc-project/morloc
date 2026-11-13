# Encoding (ENC)

The cost of getting values into and out of a program through the CLI, and
the size of a value in each form. Formats themselves are in
`runtime/formats.md` (FMT).

### ENC-1 Reading an argument file costs a constant plus a cost per element
Intent: proposed
Bench: encoding
Model: t(n) = c + b*n per form (json, mpk, packet) and element type ([U64], [Real], [Bool], [Str]); C++ consumer
Measured: 2026-10-15-0459-6915296a+dirty-simd-audit, load 1.9. b in ns per element:

| element | json | mpk | packet |
|---|---|---|---|
| U64 | 71 | 28 | 13 |
| Real | 87 | 30 | 14 |
| Bool | 45 | 12 | 1.1 |
| Str (8 bytes) | 247 | 64 | 44 |

Limit: in:Reals.packet.b <= 100 ns, in:Reals.mpk.b <= 300 ns, in:Reals.json.b <= 1000 ns, in:Bools.packet.b <= 5 ns

A voidstar file is not read back: one of a large value cannot be
(runtime/issues/nexus-cli.md).

### ENC-2 Bytes per element are fixed by the element type and the form
Intent: proposed
Bench: encoding
Model: size(n) = h + s*n, s exact
Measured: 2026-10-13, a2e21e97+dirty (the per-element sizes are deterministic, so the load at the time does not affect them). s in bytes:

| element | json | mpk | voidstar, packet |
|---|---|---|---|
| Int (0..n) | 5 | 3 | 16 |
| U64 (0..n) | pending | pending | pending |
| Real | 5 | 9 | 8 |
| Bool | 5.5 | 1 | 1 |
| Str (8 bytes) | 11 | 9 | 24 |

Limit: Ints.packet.bytes <= 16 B, U64s.packet.bytes <= 8 B, Reals.packet.bytes <= 8 B, Bools.packet.bytes <= 1 B, Strs.packet.bytes <= 24 B

An `Int` is a `j` slot (FMT-6): 16 bytes whatever the native width, by
design, since it must cover integers of any size. `Int` is never used where
performance matters; use a fixed-width type (U64, I64, I32). The `U64s`
kind is measured beside `Ints` to show the difference. JSON and MessagePack
sizes depend on the values (here 0..n-1 and multiples of 0.5).

### ENC-3 Writing a result costs a constant plus a cost per element
Intent: proposed
Bench: encoding
Model: t(n) = c + b*n per `-f` form and element type, written to a file with `-o`
Measured: 2026-10-15-0459-6915296a+dirty-simd-audit, load 1.9. b in ns per element:

| element | json | mpk | voidstar | packet |
|---|---|---|---|---|
| U64 | 46 | 28 | 24 | 18 |
| Real | 96 | 34 | 23 | 21 |
| Bool | 21 | 8.8 | 3.8 | 3.2 |
| Str (8 bytes) | 88 | 69 | 101 | 112 |

Limit: out:Reals.packet.b <= 100 ns, out:Reals.json.b <= 1000 ns, out:Bools.packet.b <= 10 ns

A C++ `[Bool]` is packed and unpacked a word at a time (runtime-bench
before this: 2.7 ns per element encode, 1.7 decode, in the codec alone).
JSON floats are written by ryu (WIRE-9); `%.15g` cost about 260 ns per
`Real` in the writer. Open: `[Str]` written as voidstar or packet costs
more per element than as JSON.

### ENC-4 A pure command reads an argument file at a constant cost per element
Intent: proposed
Bench: pure-args
Model: t(n) = c + b*n per element type ([Str], [?Real], [[I64]]); a JSON file read and written back by a pure command, no pool
Measured: 2026-10-20-0959-74a5fc0a+dirty, load 0.8. b in ns per element: Strs 616, OptReals 337, Nested 639 (r2 >= 0.994)
Limit: pure:Strs.json.b <= 2000 ns, pure:OptReals.json.b <= 2000 ns, pure:Nested.json.b <= 2000 ns

A value whose elements are separate blocks (strings, present optionals,
inner lists) is merged into one block inside the command's eval arena;
the cost of freeing the pieces must not grow with their number
(runtime/shm.md SHM-14). b includes writing the result.

### ENC-5 An R @write of an atomic vector costs a rate per element near C++'s
Intent: proposed
Bench: stream-write-r
Model: t(n) = c + b*n for `@write` of n elements ([Real], [I32], [Bool]) to a stream file at level 0, and for generating them alone; `<lang>.<kind>.encode` = b_write - b_gen in ns per element, R and C++
Measured: pending

The R encoder writes a numeric, integer or logical vector straight from
its storage, with the checks each element's type requires, and allocates
nothing per element. A `[Real]` that is not bit64 integer64 is one copy.

### ENC-6 @save writes a value at a rate near a stream write
Intent: proposed
Bench: save-load
Model: t(n) = c + b*n for `@save` of n `Real`s from a C++ pool at levels 0 and 3, for `@load` of the file, and for generating the list alone; `save<lvl>.encode` = b_save - b_gen in ns per element
Measured: pending

At level 0 the flat form goes straight to the file through a buffered
writer, with no copy of the whole value; compressing flattens the value
once and compresses that buffer. The file is synced before it is renamed
into place, which is part of b; the bench writes to tmpfs, so the sync
costs little there. The bytes match the flattened packet
exactly (`intrinsics::packet_emit_tests`).

### ENC-7 A JSON @stdout stream costs a rate per element near `-f json`
Intent: proposed
Bench: save-load
Model: t(n) = c + b*n for one `@write` of n `Real`s to `@stdout`, rendered by the nexus as JSON; `stdout.json.render` = b_stream - b_gen in ns per element, compared with ENC-3 out:Reals.json.b
Measured: pending

Each sub-packet is rendered to one string by the runtime, and the
nexus writes the element body from that string without copying it.

### ENC-8 A @stdout stream of large Str bodies writes at a rate near write(2)
Intent: proposed
Bench: stdout-raw
Model: t(n) = c + b*n for 64 bodies of n bytes from a C++ pool to stdout (/dev/null), as raw render output and as a `-f packet` stream, and for building the bodies alone; `<form>.write` = (b_form - b_gen) / 64 in ns per byte, compared with `control.write`, a C++ write(2) of the same bodies
Measured: pending

The custodian hands the nexus a sub-packet as its header block, payload
and padding, and the nexus writes them with one `writev` (SLOT-16): no
buffer joins them, so the nexus holds no copy of the largest sub-packet.
Raw output writes each body from where it lies in the payload.

### ENC-9 A unit-step IFile slice reads at near an IStream's rate per element
Intent: proposed
Bench: ifile-slice
Model: t(n) = c + b*n for `.[0:n]` of a file of n elements ([Real], [Str]) and for an `@next` loop over it, C++ and Rust, levels 0 and 3; `<lang>.<kind>.z<level>.ratio` = b_slice / b_next
Measured: 2026-11-12-0510-24a2303bd+dirty, load 2.5-2.6. ratio: cpp Reals z0 1.60, z3 2.14; cpp Strs z0 1.80, z3 1.77; rs Reals z0 1.15, z3 1.74; rs Strs z0 1.37, z3 1.53. Before the in-place slice (2026-11-12 sweep, C++ .[0:16M] of 128 MB): 162 ms against 30 ms by @next, 5.4x.
Goal: cpp.Reals.z0.ratio <= 1.5, cpp.Reals.z3.ratio <= 1.5, rs.Reals.z0.ratio <= 1.5, rs.Reals.z3.ratio <= 1.5, cpp.Strs.z0.ratio <= 1.5, cpp.Strs.z3.ratio <= 1.5, rs.Strs.z0.ratio <= 1.5, rs.Strs.z3.ratio <= 1.5
Limit: cpp.Reals.z0.ratio <= 2.5, cpp.Reals.z3.ratio <= 2.5, rs.Reals.z0.ratio <= 2.5, rs.Reals.z3.ratio <= 2.5, cpp.Strs.z0.ratio <= 2.5, cpp.Strs.z3.ratio <= 2.5, rs.Strs.z0.ratio <= 2.5, rs.Strs.z3.ratio <= 2.5

A slice is decoded one sub-packet piece at a time where the piece lies
(runtime/streams.md SLOT-25), as `@next` decodes its batch, into a list
sized once; no shared-memory block holds the result. Only the clipped
records are checked.

### ENC-10 A one-element IFile slice costs the same whatever the file's size
Intent: proposed
Bench: ifile-slice
Model: per = (t(k) - t(0)) / k for k = 10000 one-element slices `.[i:i+1]` of a file of n Reals, n = 1M, 4M, 16M; `<lang>.points.z<level>.growth` = per(16M) / per(1M)
Measured: 2026-11-12-0510-24a2303bd+dirty, load 2.5-2.6. growth: cpp z0 0.24, z3 0.81; rs z0 0.61, z3 0.97.
Limit: cpp.points.z0.growth <= 1.5, cpp.points.z3.growth <= 1.5, rs.points.z0.growth <= 1.5, rs.points.z3.growth <= 1.5

A slice locates its sub-packets by two binary searches and checks only
the records it reads, so neither the file's length nor the size of the
sub-packet it reads from enters its cost, beyond decompressing a
sub-packet that SLOT-34 has not kept.

### ENC-11 A whole-list @collect costs little more than making its batches
Intent: proposed
Bench: gather
Model: t(g) = c + b*g for g GiB gathered: b batches of k 256-count U64 histograms through `@collect` into a whole-list `@render` handler, against a control that makes and counts the same batches where they are made; `<lang>.extra` = b_gather - b_control, `<lang>.offcpu` = the same for wall minus CPU (negative when the gather runs threads), C++ and Python
Measured: 2026-11-13-0233-c4022bfa7+dirty-after-w1-b2-w2, load 0.96-1.7. extra: cpp 1.66 s/GiB (gather 1.72, control 0.058), py 9.44 s/GiB (gather 10.6, control 1.17). @load decoded in place against into one block (same build, LD_PRELOAD control): 1.74 against 2.33 s/GiB. Before (2026-11-13-0159, which timed the to-path route): cpp 1.77, py 8.02.
Goal: cpp.extra <= 0.3 s/GiB, py.extra <= 1.0 s/GiB
Limit: cpp.extra <= 2.5 s/GiB, py.extra <= 10 s/GiB

A handler that takes the whole list holds it in memory whatever the
mechanism, so the gather needs no file: the batches are moved into one
list where the handler runs. 

### ENC-13 A strided or field-tail IFile slice costs near a unit slice of its output
Intent: proposed
Bench: ifile-slice
Model: per pair, t and peak RSS of a strided `.[0:n:k]` (k = 2, 7), reversed `.[::-1]` or field-tail `.[0:n].1` slice against a unit slice `.[0:m]` with the same output count m, over [Real], (I64, Real, Real) and [Str], C++, Rust and Python, levels 0 and 3; `<lang>.<kind>.<form>.z<level>.ratio` = t_strided / t_unit, `.rssx` = peak RSS over the unit slice's
Measured: 2026-11-13-0246-c4022bfa7+dirty-w6, load 2.2-4.0, ratio/rssx: cpp z0 reals k2 1.80/1.68, k7 1.55/1.32, rev 1.39/1.81, tups k2 1.34/1.42, field 1.63/1.67, strs k2 1.38/1.29; cpp z3 reals k2 1.61/1.64, field 1.57/1.92, strs k2 1.69/1.66; rs z0 reals k2 1.45/1.55, k7 1.36/1.11, rev 1.29/1.73, field 1.44/1.50; py z0 reals k2 0.97/0.88. before, installed build 2026-11-13 02:32, C++, load 2.1-2.5, best of 3, s and peak RSS MiB. [Real] 128 MB z0: k2 0.759/408 against unit 8M 0.099/84 (7.7x); k7 0.275/233 against unit 2.29M 0.062/41 (4.4x); rev 1.245/651 against unit 16M 0.156/145 (8.0x). z3: k2 0.926/481 against unit 8M 0.139/101 (6.7x); rev 1.399/727. (I64, Real, Real) 120 MB z0: k2 0.465/309 against unit 2.5M 0.135/100 (3.4x); `.1` 0.517/308 against [Real] unit 5M 0.078/62 (6.6x). [Str] 4M z0: k2 0.271/327 against unit 2M 0.185/141 (1.5x); z3 0.403/339 against 0.234/166 (1.7x). Earlier, 600 MiB, load 0.4-1.0: `.[0:n:2]` 3.44 s/1.84 GB, `.[0:n].1` 2.32 s/1.44 GB, unit 0.50 s/625 MB.
Goal: cpp.reals.k2.z0.ratio <= 1.5, cpp.reals.rev.z0.ratio <= 1.5, cpp.tups.field.z0.ratio <= 1.5, rs.reals.k2.z0.ratio <= 1.5, rs.reals.rev.z0.ratio <= 1.5, rs.tups.field.z0.ratio <= 1.5

A slice with a step other than 1, or with a field tail, takes one run per
sub-packet it spans, found arithmetically (runtime/streams.md SLOT-38), and
holds one sub-packet at a time. A fixed-width result is a strided copy into
a block sized once. Against a unit slice of the same output it reads k
times the records (k for a stride k, the record over the field for a
tail), so at level 3 it decompresses k times as much: the goal covers the
uncompressed forms whose touched bytes are near the output's. A [Str] result
is sized and copied in two passes, so at level 3 it decompresses each
interior sub-packet twice; a record holding a Str is still copied element by
element and consolidated, near 2x its output in memory.

Peak RSS counts the file's mapped pages a slice has touched, and a stride
k touches k times the pages of a unit slice of the same output, though they
are clean and reclaimable; so `rssx` is recorded but holds no Limit until
the bench measures the anonymous memory a slice keeps.
