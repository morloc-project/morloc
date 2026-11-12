# Remaining copies on call and stream paths

From an audit of the output-path work (2026-11-12): a copy census of the
call paths and the stream paths and a benchmark sweep beyond mosm (load
1.4-3.0). Each item is read in code unless marked measured; none is
scheduled.

- **Every packet rebuilds its schema.** [read] Building an inline packet
  (packet_ffi.rs make_data_packet_with and its siblings) runs
  CSchema::to_rust three times and schema_to_string once;
  get_morloc_data_packet_value_from converts before it checks the
  source, so a by-reference value pays for a schema it never uses.
  Python and R also parse the schema string on every put_value and
  get_value (pymorloc.c, rmorloc.c); C++ and Rust parse once. Estimated
  1-4 us per call for a record of 5-10 named fields. Fix: cache the Rust
  Schema on the CSchema (a layout change, so ABI), and give Python/R
  generated code schema handles. XL-1 uses only I64; add a record
  argument series to xl-calls first.
- **A small value takes about six copies to cross.** [read] A Python
  argument of 64 KiB or less: to_voidstar into a fresh SHM block, a size
  walk, flatten into a zeroed Vec, copy into the packet, scrub the block,
  copy into a PyBytes, copy again into the call packet, and on a Python
  callee two more PyBytes copies; the reply repeats it. About 1-2 us of
  fixed cost per tiny value, ~50 us near 64 KiB. Fixes: send the call
  header and argument packets as an iovec; build Python packets in place
  (PyBytes_FromStringAndSize(NULL, n)) and hand arguments as memoryview
  slices; encode small values straight into the packet. XL-3 sizes skip
  the 1K-256K band, so the inline threshold is unmeasured.
- **Python and R @next copy what C++ and Rust decode in place.** [read;
  measured roughly] pymorloc.c and rmorloc.c call plain mlc_next, which
  copies an uncompressed sub-packet into SHM, relocates it, decodes, and
  scrubs: about 0.1-0.15 s per 600 MiB more than C++. Fix: a
  mlc_next_with path for each (R materializes before any longjmp; Python
  copies numpy out of the mapping rather than viewing it).
- **A channel write lands its buffer again.** [read] flush_write_buffer_at
  (stream.rs) copies the sealed write buffer into a fresh landing, shifts
  its records and relocates it, before the channel node takes it; the
  binding's staging copy comes first. Fix: hand the sealed block itself
  to the channel and take a spare (SLOT-4 ownership). No channel
  throughput bench exists; add one first.
- **The whole-list gather still goes through a temp file.**
  (whole-list-gather.md) A cheaper variant than an SHM chain: keep the
  file and decode with a mlc_next_with loop into a list sized from the
  stream's length, which removes the load copy, the collect copy, two
  walks and two scrubs (~0.1-0.15 s of ~0.2 s on mosm stat) with no new
  lifetime model.
- **Most IFile slices keep the copy path.** Steps other than 1, field
  tails and Python/R slices still build a whole result block; the
  in-place piece path (SLOT-25) covers unit-step C++/Rust slices only.
- **The JSON render copies once more than it must.** The runtime returns
  rendered JSON as a malloc'd C string copied from a Rust String; the
  nexus then slices it. Removing the copy needs a new FFI.
