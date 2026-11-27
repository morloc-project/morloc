# Unchecked lengths in packets and stream files

Gap in FMT-1: nothing states that every length and offset read from a peer
or a file is checked before use. Fix each with a test that feeds a
malformed packet or file. Paths under runtime; RT =
morloc-runtime-types/src/packet.rs, FFI = morloc-runtime/src/packet_ffi.rs.

- [read] Readers that take a packet pointer without its length
  (`get_morloc_data_packet_value`, `read_morloc_call_packet`,
  `morloc_packet_size`) check every length against the packet's own header,
  not against the bytes the caller holds. Taking `(ptr, len)` is an ABI
  change (/work/plans/brittleness-2026-10-23/1-abi-bundle-proposal.md).
- [read] RT:841-842 (`make_footer_packet`): a footer sets `offset = length = body length`, so the
  generic size counts the body twice. Decide what the fields mean.
- [read] FMT-1: the `version` field is never read, and the header is written
  in host byte order though FMT-1 says little-endian.
- [read, 2026-11-01] stream_format.rs `parse_stream_file` discards
  `ForwardScan.partial` (set when a sub-packet runs past the end).
- [speculative, 2026-11-01] ipc_ffi.rs ~698 `let copied =
  have.min(packet_length)` drops bytes past the first packet from one
  4096-byte head read; a pipelined peer would lose its second packet. The
  declared length is malloc'd before any byte arrives (NET-10).
- [reproduced 2026-11-02] `@stdin` IStream allocates a stdin sub-packet's
  claimed length before its bytes arrive (`stdio_bridge::read_into_shm`):
  a 69-byte input claiming 2^40 bytes makes a 1 TiB file-backed volume
  before failing (exit 1). Do not rerun at that size. The value lands in
  one SHM block and there is no SHM realloc, so reading first means a
  copy of every large legitimate packet; needs a choice (bounded direct
  allocation, then staging past a size?).
- [read 2026-11-02] daemon_ffi.rs `reply_reference` reads the reply
  through packet_ffi.rs `rptr_payload_of` (:937-949), which checks the
  packet type and length but not the magic.
- [read 2026-11-04] Metadata lengths still cast with `as u32`:
  packet_ffi.rs `new_metadata_size as u32` and `extended_offset as u32`
  (original metadata plus a frame index, as compression.rs's now-checked
  one), morloc-runtime-types compression.rs:965, cache.rs:1069,
  intrinsics.rs:171, stream_format.rs:894. `meta_len_u32` is crate-private to
  morloc-runtime-types.
- [read 2026-11-04] ipc.rs `read_packet` skips `offset - 32` bytes of
  metadata (offset is the metadata length: offset 0 underflows, offset 64
  leaves 32 metadata bytes as payload) and `send_and_receive` skips none;
  both allocate `length` before any byte. Public API, no internal callers.
- [read 2026-11-04] The FILE-packet writers (`make_data_indirection_packet`,
  `make_program_file_packet`, `make_table_file_packet`) accept a path of
  any length; every reader refuses one past 4096 bytes, so the refusal
  happens in another process, far from where the path was made.
- [read 2026-11-04] The FILE-source readers take a packet pointer whose
  size comes from its own header (`read_file_payload`,
  `stream_file_payload`), so their bounds rest on the caller.
