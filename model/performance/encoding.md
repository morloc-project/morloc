# Encoding (ENC)

The cost of getting values into and out of a program through the CLI, and
the size of a value in each form. Formats themselves are in
`runtime/formats.md` (FMT).

### ENC-1 Reading an argument file costs a constant plus a cost per element
Intent: proposed
Bench: encoding
Model: t(n) = c + b*n per form (json, mpk, packet) and element type ([U64], [Real], [Bool], [Str]); C++ consumer
Measured: pending
Limit: in:Reals.packet.b <= 100 ns, in:Reals.mpk.b <= 300 ns, in:Reals.json.b <= 1000 ns

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
Measured: pending
Limit: out:Reals.packet.b <= 100 ns, out:Reals.json.b <= 1000 ns
