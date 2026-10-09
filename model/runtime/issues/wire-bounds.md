# Unchecked lengths in packets and stream files

Gap in FMT-1: nothing states that every length and offset read from a peer
or a file is checked before use. Fix each with a test that feeds a
malformed packet or file. Paths under data/rust; RT =
morloc-runtime-types/src/packet.rs, FFI = morloc-runtime/src/packet_ffi.rs.

- [read] morloc-runtime/src/ipc_ffi.rs:578-621: size parsed from the first
  `recv` even when it returned fewer than 32 bytes.
- [read] FFI:1048-1058 `read_morloc_call_packet`: argument size not checked
  against the packet end.
- [read] FFI:855: pointer payload read as 8 bytes without `length >= 8`.
- [speculative] FFI:75-80, RT:1305, morloc-nexus/src/dispatch.rs:1134:
  `32 + offset + length` can wrap.
- [read] RT:1264 `decode_subpacket_index`: `count * 16` unchecked; aborts
  `morloc-nexus file` on a corrupt footer.
- [read] RT:990-991: a footer sets `offset = length = body length`, so the
  generic size counts the body twice. Decide what the fields mean.
- [speculative] RT:990, 1236: footer and index lengths cast `as u32`; an
  index over 4 GiB is written corrupt.
- [speculative] morloc-runtime/src/stream.rs:5103: batch `32 + offset +
  length` unchecked; can move an IStream cursor backwards.
- [read] morloc-runtime-types/src/shm_types.rs:113, morloc-runtime/src/shm.rs:1235:
  `align_up` in `shmalloc` overflows near `usize::MAX` to a 0-byte block.
- [read] FFI:753, 1457, 1779, 2152: file-source paths truncated at 4096
  bytes; non-UTF-8 paths become "".
- [read] `get_morloc_data_packet_value` rejects inline JSON that
  `packet.rs::get_data_value` accepts.
- [read] FMT-1: the `version` field is never read, and the header is written
  in host byte order though FMT-1 says little-endian.
