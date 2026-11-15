# Remaining copies on call and stream paths

From an audit of the output-path work (2026-11-12), re-evaluated with
measurements on 2026-11-13 (load 0.6-2.1 unless noted; scratch programs
under the session scratchpad agent-call/, agent-stream/, agent-chan/).
None is scheduled.

- **Every packet converts its schema.** [read] Building a packet
  (packet_ffi.rs make_data_packet_with :1225) still runs CSchema::to_rust
  once and schema_to_string once (build_schema_metadata_rs :213). R
  parses the schema string on every put_value and get_value
  (rmorloc.c:2470/2677). Parse time jumps between 7 and 8 fields (1.1 to
  1.9 us, cause unknown). Fix: cache the Rust Schema and the metadata
  block on the CSchema, filled eagerly (ABI), and give R schema handles.
- **Mid-size inline values cost more than sending them by reference.**
  [measured] Per call, inline minus MORLOC_INLINE_SIZE=0: 1 KiB -1.1 to
  +0.9 us, 4 KiB +3 to +5, 16 KiB +7, 64 KiB +52 to +62 (about 20 us of
  it heap trimming, pools-and-ipc.md). A tiny value costs 0.2-0.5 us, not
  the 1-2 us first estimated. The census: two Python object walks, two
  runtime size walks, a zeroed Vec, the packet copy, a scrub, a PyBytes
  copy and the call-packet copy; a Python callee also lands the payload
  in SHM, relocates and scrubs it, where C++ reads it in place. The
  header and argument packets already go out as one iovec. Fixes:
  lower the inline threshold to 1-2 KiB (topology.md states 64 KiB; needs
  a ruling), stop the heap churn, then a Python inline-read fast path.
  Python callers accept only bytes, so memoryview arguments are out.
- **Python and R @next copy what C++ and Rust decode in place.**
  [measured] Plain mlc_next lands the sub-packet in SHM, relocates,
  decodes and scrubs; the in-place path keeps one check walk. At z3 the
  decode-ahead block is already handed over, so only the scrub is saved.
  Excess over C++ per 536 MiB: R Vector U8 +0.28 s and [Real] +0.27 s
  (removable part about 0.3 s/GiB), Python numpy +0.05 s (an in-place
  numpy must copy out anyway, so about 0), Python [Real] +1.7 s (PyFloat
  construction, not removable here). No real program reads streams from
  Python or R. Fix: mlc_next_with for each; in R the decode callback must
  catch R errors (R_tryCatchError) and re-raise after the call returns, since
  R_UnwindProtect only runs cleanup while the longjmp crosses the runtime.
- **A channel write lands its buffer again.** [measured] flush_write_buffer_at
  (stream.rs:3987-4025) copies the sealed buffer into a fresh landing,
  shifts its records and relocates it, all under the slot lock; elements
  larger than the buffer already skip it (stream.rs:4278-4299). A/B
  against the oversize path: 30-45 ms/GiB, 15-20% of a channel that only
  copies, 3% or less of a parsing pipeline. Channels come only from
  `@parse` over an IStream; their users are C++ readers in /work/tests.
  Fix: flatten each write straight into an exact landing, as the staged
  path does, and delete the channel branch.
- **The whole-list gather still goes through a temp file.**
  (whole-list-gather.md) [measured] C++ and Rust @load of a stream file now
  decode each sub-packet in place (ENC-11: 1.74 against 2.33 s/GiB in one
  build); the gather still costs 1.60 s/GiB over making the batches in C++
  and 8.18 s/GiB in Python (2026-11-14), the encode, custody, file and decode a native
  gather would remove. Python @load of a list now decodes in place too
  (in-run A/B 6.81 -> 5.88 s/GiB, load 7), unless any part of the type
  reads as a NumPy array or an Arrow table, which could view a piece that
  does not outlive the call; R @load still takes the block path, and needs its decode
  callbacks to catch R errors rather than longjmp through the runtime
  (R_tryCatchError, the condition re-raised after the call returns).
- **Python and R slices keep the copy path; Str strides cost a second
  pass.** Strided and field-tail slices run per sub-packet now (SLOT-38,
  ENC-13); leftovers in ifile-access.md. The unit-step C++ miss of the 1.5x
  goal is first touch of the result vector (165k faults per 600 MiB), not
  a copy; ENC-9 needs a fresh-list control.
- **The JSON render copies once more than it must.** The runtime copies
  rendered JSON from a Rust String into a malloc'd C string, about 2-3% of
  a large serve (SERVE-1) and under 1 us of a small one; the CLI does not
  use this path.
- **Calling a C++ function value copies a list argument.** [measured
  2026-11-14] A morloc function value in C++ is a `std::function` whose
  parameters are by value, so every call of a sink or callback copies a
  non-scalar argument (cpp-copy-count: 4 allocations per batch, one of them
  this copy). Rendering every function type's non-scalar parameters as
  `const T&` (patch: /work/plans/output-path-stages-notes/
  cpp-const-ref-function-args.patch) gives 3 and passed 245 C++, closure and
  callback goldens, but changes the C++ type user headers receive: a header
  that pattern-matches `std::function` parameter types breaks (biolib
  convert-cpp convert.hpp `cv_fn<SinkFn>`), and a header declaring a
  by-value `std::function` parameter gets a converting wrapper that hides
  `MorlocClosure` from `target<>()`, so the closure can no longer cross a
  pool. A by-value callee that moves from its parameter then copies even
  when the caller had an rvalue. Needs a ruling: is the C++ spelling of a
  function type part of the interface users write against?
- **A Rust fold step copies its accumulator every put.** [read
  2026-11-15] HOST-8 gives a Rust step the type `Fn(&A, &B) -> A`, so a
  step that returns a grown accumulator clones it: O(n) per put, O(n^2)
  over a fold whose accumulator grows with the input (a Vec or Map of
  every key seen). C++ moves the checked-out value into step (golden
  fold-copy-count); Python and R mutate in place. Taking the accumulator
  by value (`Fn(A, &B) -> A`) changes the Rust type user code is written
  against. Needs an interface ruling.
