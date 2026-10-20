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
Measured: pending (a smoke run on 2026-10-20 at load 7.7 fitted b = 600, 290 and 570 ns)
Limit: pure:Strs.json.b <= 2000 ns, pure:OptReals.json.b <= 2000 ns, pure:Nested.json.b <= 2000 ns

A value whose elements are separate blocks (strings, present optionals,
inner lists) is merged into one block inside the command's eval arena;
the cost of freeing the pieces must not grow with their number
(runtime/shm.md SHM-14). b includes writing the result.
