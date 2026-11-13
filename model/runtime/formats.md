# Binary formats (FMT)

The binary formats processes exchange and write to disk: packets, shared
memory, voidstar values and stream files. The docs' Build Architecture
chapter describes them in full; these items are the rules a test can check.

### FMT-1 A packet is a 32-byte header, `offset` bytes of metadata, `length` bytes of payload
Status: draft
Magic 0x0707f86d at offset 0; plain/version/flavor reserved 0; mode u16 at
10, whose bit 0 asks on a request, and answers on a reply, that the
connection be kept for the next call, and whose bit 1, on a header-only
reply, says the callee let a kept connection go without running the
request it carried (NET-7), the other bits reserved 0;
command at 12 (type byte first); offset u32 at 20; length u64 at 24.
Integers are little-endian.

### FMT-2 Packet types: 0 data, 1 call, 2 ping, 3 stream, 4 footer
Status: draft
Call: entrypoint at 13, mid u32 at 16, payload = argument data packets
concatenated. Data: source 13, format 14, compression 15, encryption 16,
status 17.

### FMT-3 Metadata blocks are `mmh`, kind u8, size u32, body
Status: draft
Region zero-padded to a multiple of 32. Kinds 1 schema, 3 volume index,
4 frame index, 5 subpacket index, 6 stream diag, 7 final footer, 8 footer
status, 9 program file (empty body: the file a file-reference packet names
was written by this program, its cache or a value one pool hands another,
so a reader takes it as a value the program built, PANIC-14). (2 xxhash:
defined, unused.)

### FMT-4 A relative pointer is volume index (bits 62-48) and offset (47-0); -1 is null
Status: draft
Volume 0 is never mapped; in a self-contained buffer, volume 0 offsets are
relative to the buffer start.

### FMT-5 Volume and block headers
Status: draft
Volume: magic 0xFECA0DF0 (stored last), name[128], index i32, size u64,
reserved u64, lock, cursor; padded to 16. Block: magic 0x0CB10DF0 (merged:
0x0CB1DEAD), refcount u32 (0 free, all ones tearing down), size u64; data
16-aligned, sizes multiples of 16.

### FMT-6 Voidstar slot layout per schema code
Status: draft
Tests: group_result_has_the_fmt6_layout, golden view-pattern-group-layout, a_unit_field_writes_nothing_past_its_record
See docs table. `z` (unit) is 0 bytes: nothing is written or read for it.
`s`/`a` 16 bytes {count, relptr}; `j` {limbs, word|relptr};
`?` relptr; `v` tag byte, 7 zero bytes, relptr to arm tuple; `t` C-struct
rules aligned to max field; `m` same with alignment 8 (an open question);
numeric array data 64-byte aligned.

### FMT-7 Schema counts and name lengths are base-64, low digit first
Status: draft
Digits 0-9a-zA-Z+/; >= 64 written `=` <low digit> <rest>. Name lengths are
BYTES.

### FMT-8 A stream file is head, data batches, footer, 8-byte tail
Status: draft
Tail = footer length u32 LE + bytes 07 07 f8 6d. Footer holds subpacket
index, final marker, status (0 closed, 1 paused, 2 failed; 3 repaired is reserved).

### FMT-9 A stream handle's schema names what its file stores
Status: draft
Tests: every_printed_schema_parses_and_reprints, golden spec-strm-1-1
`F`, `O` or `I`, then a count (FMT-7) and that many bytes: the canonical,
closed schema of the file's contents, `a` for `IFile a` and `[a]` for
`IStream a` and `OStream a`. Every point that makes a handle from a path or
a slot compares it with what the file or slot holds (STRM-1). A type that
holds itself through a handle has no closed form and is refused at build.

### FMT-10 A Str is valid UTF-8 on the wire, and no output form drops one
Status: implemented
Checked by: the_first_invalid_byte_is_found, a_str_that_is_not_text_fails_every_read_of_a_file, a_str_column_that_is_not_text_is_refused, a_msgpack_str_that_is_not_text_is_refused, golden:spec-base-4-1

A `Str` body is valid UTF-8 text (BASE-5). A binding whose language can
hold other bytes (C++ `std::string`, an R string built from raw bytes or
marked as bytes, Python `bytes` given for a `Str`) checks each body as it
encodes it, and an R string marked latin1 (read as CP1252, as R reads it),
or native in a locale that is neither UTF-8 nor ASCII, is translated to
UTF-8. A failure names the language and the byte
position. A body read from outside (a file, standard input, a packet, a
MessagePack argument) is checked as it is decoded, slice and element reads
included, and every Arrow column holding text is checked as a table is
imported. An output form never prints a body it cannot carry as empty: raw
output writes the bytes, and JSON output fails, naming the byte position.
`MORLOC_SKIP_UTF8_CHECK=1`, or a program built with `--unsafe-skip-utf8-check`,
turns off the encoders' check only; a process reads the setting once.
