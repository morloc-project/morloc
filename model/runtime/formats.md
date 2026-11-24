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
status 17, generation u16 at 18 (little-endian; the SHM-13 generation of a
reference's block, 0 for inline data).

### FMT-3 Metadata blocks are `mmh`, kind u8, size u32, body
Status: draft
Region zero-padded to a multiple of 32. Kinds 1 schema, 3 volume index,
4 frame index, 5 subpacket index, 6 stream diag, 7 final footer, 8 footer
status, 9 program file (empty body: the file a file-reference packet names
was written by this program, its cache or a value one pool hands another,
so a reader takes it as a value the program built, PANIC-14), 10 a program
file mark taken off an argument of a call packet read from a file (FMT-10;
readers skip it). (2 xxhash: defined, unused.)

### FMT-4 A relative pointer is volume index (bits 62-48) and offset (47-0); -1 is null
Status: draft
Volume 0 is never mapped; in a self-contained buffer, volume 0 offsets are
relative to the buffer start.

### FMT-5 Volume and block headers
Status: draft
Volume: magic 0xFECA0DF0 (stored last), name[128], index i32, size u64,
poison u64 (SHM-6), lock, cursor, next_index u32 and grow_lock (SHM-12),
kind u32 and committed u64 (SHM-18); padded to 16. Block: magic 0x0CB10DF0,
refcount u32 (0 free, all ones tearing down), size in bits 0-47 and
generation in bits 48-63 of a u64 (SHM-13); a merged header is all zero;
data 16-aligned, sizes multiples of 16.

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
index, final marker, status (0 closed, 1 paused, 2 failed, 3 repaired by
`morloc-nexus file --repair`; SLOT-28).

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
Checked by: the_first_invalid_byte_is_found, a_str_that_is_not_text_fails_every_read_of_a_file, a_str_column_that_is_not_text_is_refused, a_msgpack_str_that_is_not_text_is_refused, raw_output_writes_a_str_that_is_not_text_byte_for_byte, a_debug_build_checks_the_text_of_its_own_raw_output_unless_the_check_is_off, a_stdout_sub_packet_is_materialized_as_the_programs_own_value, a_debug_build_checks_the_text_of_its_own_stdout_unless_the_check_is_off, a_stage_file_is_the_programs_own_only_where_the_nexus_puts_it, bytes_are_trusted_by_the_descriptor_they_were_read_from_never_by_their_name, a_call_packet_read_from_a_file_passes_every_argument_as_outside_data, a_call_argument_that_runs_past_the_packet_is_refused, a_call_read_from_a_file_refuses_shared_memory_and_spills_inline_values, golden:spec-base-4-1, golden:stdout-raw-bodies, golden:stdout-raw-producers

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
turns off the check of the bodies the program builds, never that of data
read from outside; a process reads the setting once. The output the stage
of a multi-output run saves is a value the program built, and each replay
reads it as one, a stream or a value, whether the pools or the nexus read
it. That is decided from where the file is, never from what it holds, and
only by the nexus of a replay child, which takes the last input of the
hidden replay command (not a user interface) as the program's own when it
is `stream.pkt` or `value.pkt` in a `morloc-stage-XXXXXX` directory of the
nexus's temporary directory, the directory owned by the user and closed to
everyone else, the file regular, not a link, with one link, owned by the
user and writable by no one else. The nexus opens it without following a
link and keeps it open, and trusts only that inode, in that process and
the streams it opens for its pools; the origin of bytes is that of the
descriptor they were read from, never of a name looked up again. Every
other file, one where a stage saves its output or named by a packet marked
as the program's included, read as a command argument, from stdin, by a
call packet or by the daemon, is data from outside. A call packet read
from a file (call-packet mode) has every argument read as data from
outside: the nexus writes each inline argument to a new file of its own
and passes it by name, which a pool reads as data from outside, takes the
program-file mark off each file argument, and refuses an argument in
shared memory or one that runs past the packet. Another
process of the same user is outside this guarantee. The program's own
values skip only the text, fixed-length and enum checks; every relptr and
extent check still runs, and in a debug build a stage file that fails a
check is a defect (PANIC-14). The frames of a run's
own output streams are values the program built (DIM-3). Raw output
writes their bodies without scanning them, except in a debug build, which
checks them as DIM-3 checks the program's values and which the setting
also turns off; every path to them checked each body where it was made:
C++ in its leaf writer and, for a container standing for a `Str`, after
writing its bytes, in the in-place and the copy paths alike; Python in its
write pass, in place and as a copy; R in its size pass, which runs before
every write of a value (in place, as a copy, and into a new block for a
call or a crossing), and as each element of a character vector is placed
in place; and a body read from outside as it was decoded. A Rust `String`
is text by its own invariant: a sourced function that breaks it (with
`from_utf8_unchecked`) is undefined behaviour in Rust, outside morloc's
checks. JSON output of a body that is not text fails without naming it as
read from outside.
