# Binary formats (FMT)

The binary formats processes exchange and write to disk: packets, shared
memory, voidstar values and stream files. The docs' Build Architecture
chapter describes them in full; these items are the rules a test can check.

### FMT-1 A packet is a 32-byte header, `offset` bytes of metadata, `length` bytes of payload
Status: draft
Magic 0x0707f86d at offset 0; plain/version/flavor/mode reserved 0; command
at 12 (type byte first); offset u32 at 20; length u64 at 24. Integers are
little-endian.

### FMT-2 Packet types: 0 data, 1 call, 2 ping, 3 stream, 4 footer
Status: draft
Call: entrypoint at 13, mid u32 at 16, payload = argument data packets
concatenated. Data: source 13, format 14, compression 15, encryption 16,
status 17.

### FMT-3 Metadata blocks are `mmh`, kind u8, size u32, body
Status: draft
Region zero-padded to a multiple of 32. Kinds 1 schema, 3 volume index,
4 frame index, 5 subpacket index, 6 stream diag, 7 final footer, 8 footer
status. (2 xxhash: defined, unused.)

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
See docs table. `s`/`a` 16 bytes {count, relptr}; `j` {limbs, word|relptr};
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
index, final marker, status (0 closed, 1 paused).
