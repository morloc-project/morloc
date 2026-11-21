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
- **A C++ sink still copies its batch into the suspension it returns.**
  [measured 2026-11-15] cpp-copy-count is 3 allocations per batch: the
  batch, the batch copied into the suspension (a closure reads a kept
  parameter by reference, OWN-7), and the sink's captured list copied into
  it. Moving an owned argument at its last read (in dev since the native
  gather) already removed the by-value `std::function` call copy, so the
  `const T&` rendering of function parameters
  (/work/plans/output-path-stages-notes/cpp-const-ref-function-args.patch)
  gains nothing more there and is rejected: it changes the C++ type user
  headers receive (biolib convert.hpp `cv_fn<SinkFn>`) and a converting
  wrapper hides `MorlocClosure` from `target<>()`, so closures could no
  longer cross a pool. Smaller fix for the suspension copy: a closure that
  keeps a parameter takes it by value (an OWN-7 extension; expected 3 -> 2);
  only if a benchmark shows the gain. The captured-list copy needs shared
  capture storage.
- **A @fold step that is not a sourced function copies its accumulator
  every batch, in Rust and C++.** [read 2026-11-21] COPY-7 holds only for a
  step or combine that is a sourced function of the fold's language, passed
  whole (Express.hs bareFoldSteps). A step written in morloc, a partial
  application, a step in another language, or one whose call is adapted
  reaches the fold as a closure over a generated function whose parameters
  are references, so a by-value parameter under it is given a copy. Fix
  direction: let that closure, and the generated function under it, take
  the accumulator by value; the same question as HOST-8's callback modes
  (/work/plans/rust-param-modes-analysis.md).
- **The Rust stdlib's folds copy their accumulator every step.** [read
  2026-11-20, DA review] COPY-7's cost, outside `@fold`:
  root-rust core.rs `morloc_fold_vec` / `morloc_fold_deque` (~309-315, and
  ~598) call the closure as `f(&acc, x)`, and stream-rust / parallel-rust
  (stream.rs ~8-18, parallel.rs ~412-426) do the same, so a closure that
  grows its accumulator clones it per element. Fixing these needs HOST-8 to
  let a host function take a callback's argument by value (bench:
  fold-growth rs.hof.ref).
- **Every non-vector container copy is entered in the converted-value
  table.** [read, 2026-11-16] GEN-14. cppmorloc.hpp sizes and writes a
  set, map, list or deque through `mlc_converted`, so each one costs a hash
  insert and lookup and a shared allocation even when nothing inside it
  converts; Rust's `Converted` holds a boxed value per entry the same way,
  and a batch write keeps every element's converted fields until the batch
  is written. Only copies that can hold a converting field need a shared
  identity: a trait marking such element types (generated per record) would
  let the rest take the plain copy.
- **Writing into shared memory translates every pointer.** [measured,
  2026-11-16] The write walk calls `abs2rel` (Rust) / `abs2rel_cpp` (C++)
  for each relative pointer it writes, which made writing into a shared
  block 25-45% slower than the same walk into a base-relative buffer
  (Rust W1 3.68 vs 2.72 ms, C++ W3 10.2 vs 7.0 ms; benchmark in the GEN-9
  re-plan notes, /work/plans/generated-native-types/REPLAN-WORK.md). One
  translation per block, then the offset within it, would remove most of
  that. The same measurement found one write walk into a heap buffer plus
  copy plus relocation 1.2-2.2x slower than the two walks, the relocation
  pass costing 4-15x the copy, so the two walks stay.
