# Unchecked lengths in packets and stream files

Gap in FMT-1: nothing states that every length and offset read from a peer
or a file is checked before use. Fix each with a test that feeds a
malformed packet or file. Paths under data/rust; RT =
morloc-runtime-types/src/packet.rs, FFI = morloc-runtime/src/packet_ffi.rs.

- [read] Readers that take a packet pointer without its length
  (`get_morloc_data_packet_value`, `read_morloc_call_packet`,
  `morloc_packet_size`) check every length against the packet's own header,
  not against the bytes the caller holds. Taking `(ptr, len)` is an ABI
  change (/work/plans/brittleness-2026-10-23/1-abi-bundle-proposal.md).
- [read] RT:990-991: a footer sets `offset = length = body length`, so the
  generic size counts the body twice. Decide what the fields mean.
- [speculative] RT:990, 1236: footer and index lengths cast `as u32`; an
  index over 4 GiB is written corrupt.
- [read] packet_ffi.rs's other file-source readers (the FILE branches near
  the voidstar, msgpack and print paths) and cache.rs truncate a path at
  4096 bytes and read a non-UTF-8 one as ""; `get_morloc_data_packet_value`
  refuses both.
- [read] `get_morloc_data_packet_value` rejects inline JSON that
  `packet.rs::get_data_value` accepts.
- [read] FMT-1: the `version` field is never read, and the header is written
  in host byte order though FMT-1 says little-endian.
- [read] `PacketHeader::total_size()` (morloc-runtime-types packet.rs)
  returns `offset + length` without the 32-byte header; a caller taking it
  for the packet's size is 32 bytes short.
