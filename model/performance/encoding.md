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
