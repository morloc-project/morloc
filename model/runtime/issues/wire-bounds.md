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
- [read] RT:843-844 (`make_footer_packet`): a footer sets `offset = length = body length`, so the
  generic size counts the body twice. Decide what the fields mean.
- [speculative] RT:844, 853: footer and metadata block lengths cast
  `as u32`; an index over 4 GiB is written corrupt.
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
- [read] morloc-runtime-types compression.rs `subpacket_elem_count`: `off + 32` and
  `start + length` are unchecked sums.
- [read, 2026-11-01] stream_format.rs `try_read_footer` (~537-550): a
  tail footer length below 32 passes the `footer_start + 32 > size` test,
  then `footer_slice[..32]` panics. A corrupt outside file crashes the
  reader instead of returning Err.
- [read, 2026-11-01] cache.rs `hash_morloc_packet` (~924), CALL branch:
  each nested `arg_size` is not bounded by the outer length before
  `from_raw_parts`. The same walk in `read_morloc_call_packet` is bounded.
- [read, 2026-11-01] stream_format.rs `parse_stream_file`: subpacket index
  offsets are never checked for order or range, and `ForwardScan.partial`
  (set when a sub-packet runs past the end) is discarded.
- [speculative, 2026-11-01] ipc_ffi.rs ~698 `let copied =
  have.min(packet_length)` drops bytes past the first packet from one
  4096-byte head read; a pipelined peer would lose its second packet. The
  declared length is malloc'd before any byte arrives (NET-10).
- [read, 2026-11-01] packet.rs `PacketHeader::total_size` has no caller.
