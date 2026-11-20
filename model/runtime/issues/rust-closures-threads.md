# Function values on threads a sourced function starts

What remains after Rust function values became thread-safe
(`Arc<dyn Fn>`, HOST-7, HOST-8, STATE-2, STATE-4). Tests:
golden spec-host-7-1, spec-host-8-1, rust-function-arg-threads.

## A host panic on a callback thread ends the pool [read, open ruling]

PANIC-6 says a panic on a thread the user's code started ends the pool.
Under HOST-7 a manifold runs on such threads, so a panic in sourced Rust
code called from a function value under `pmap` exits with status 70, while
the same call under `map` is the call's failure, catchable by `@try`.
`@throw` and foreign failures are not affected (they unwind without the
hook; spec-host-7-1 `tryThrow`, `tryForeign`).

- morloc-runtime-types/src/panic.rs:165 `decide`: scope 0 means Exit; only
  `dispatch_guard` (rustmorloc lib.rs) opens a host scope, on the pool thread.
- Candidate: the `IntoCallbackN` adapter (or the manifold entry) opens
  `SCOPE_HOST` when the thread has no scope, so the panic unwinds to the
  sourced function, whose join carries it to the pool thread's scope. Open:
  what a detached thread's panic then does (PANIC-7 says a thread never dies
  alone).
- Traceback frames pushed on the worker are lost with its thread; the
  message survives.

## A function value called after its sourced callee returned [read, spec gap]

HOST-7 covers calls made before the callee returns. Host code can now move
a function value into a thread it never joins, or keep it in a static, and call
it later. Nothing says what such a call does: the dispatch that received it
has ended, SHM blocks tracked on that thread are freed only at its exit,
and after the peer pools exit a foreign call fails. Captures are owned by
the value, so this is not a dangling read in morloc-built closures; the
spec should still state the outcome.

## Recursive values shared across threads contend on refcounts [measured]

A projection out of a recursive value clones the child's arm (one
atomic increment and decrement per node visit). Single-threaded, `Arc` and
`Rc` measured equal (13-30 ns per node, ratio 0.83-1.01, noisy machine).
Threads walking ONE shared tree contend: a 2^21-node walk x4 took 110 ms
on one thread and 620 ms on two (separate trees: 148 ms and 214 ms). So
`pmap` over elements that share subtrees can be slower than `map`.
Candidate: match projections borrow instead of clone. Benchmark source:
a Rust replica of the generated walk (Rc vs Arc arms).

## Registry costs where nothing crosses [read]

STATE-4's registry is paid by every closure whose signature can cross, as
the crossing analysis matches signatures: a closure built and applied in
place still pays a second allocation and two shard write locks (about +40
ns, measured in /work/plans/rust-idiomatic-fn-values). And `apply1_N` skips
its registry lookup only while no staged value is alive; every reflected
proxy is staged, so once one is alive each partial application of any
function value takes a shard read lock, which threads applying values at
once contend on. Candidates: register lazily (on first reify), a per-value
staged marker visible without a lookup, or a narrower crossing analysis.

## A closure capturing a packet holds a pointer into its call [read]

A closure may capture a value the pool holds only as a packet
(golden placement-representability, mirror-function.loc: `let g = pyM in
rsApply (\r -> r + g (pyMk r)) k`). The capture is
`rustmorloc::PacketRef(s4)`, where `s4` is the enclosing manifold's
incoming argument packet, which lives until that call returns. Nothing
stops such a closure from outliving the call (returned, or stored in a
record field and sent on), and then the pointer dangles. Not reproduced;
a closure with a packet capture has no dispatch entry, so it cannot be
reified, which may be what keeps it in the call today.

## The schema conversion cache restarts on every worker [speculative]

CSCHEMA_CACHE is per thread and `parallel` starts fresh threads per call,
so each worker rebuilds every conversion it needs on every call. Not
measured.

## A captured value of an opaque type fails in the front end [reproduced]

`type Shared` with `type Rust => Shared = "Shared"`, then
`addTo s xs = seqMap (\x -> x + sharedGet s) xs` fails with
"Cannot find constructor in VarF \"Shared\"  finalType=Shared" pointing at
the lambda. The same message appears for a Rust function taking a type
mapped only in Python (`go :: Blob -> [Int] -> [Int]`) [now 2026-11-02 a
located "Could not resolve type for variable 'Blob'"]. The capture case is
a missing case or an unrealizable program; the message names neither, and
it reproduces in C++ too (compiler/issues/opaque-types.md).

## A failed call on a persistent host thread leaves tracked state [read]

SHM_TRACKER and TRACEBACK (rustmorloc lib.rs) are per thread and emptied by
`dispatch_flush` on the dispatch thread. A function value that fails on a
thread that outlives the call (a rayon or other persistent pool) skips
`release_packet` while unwinding, so the blocks it tracked and the frames
it pushed stay on that thread until it exits. Each caught failure there
leaks its argument blocks for the life of the process. Not reproduced.
- **Merged functions make the panic classifier misattribute a panic.**
  [reproduced 2026-11-20] PANIC-9, PANIC-5, PANIC-6 violated in optimized
  builds. LLVM merges functions with identical code at opt-level 2 and 3,
  which pools use (thin LTO joins the user crate and rustmorloc). The
  merged body keeps one copy's debug info, so the backtrace walk can name
  the other side's file. A runtime panic can then be treated as the user's
  (@try continues past it), or a user panic as the runtime's (the pool exits
  70). Inlining is not involved: inlined frames are listed and walked
  correctly. Repro: `cargo test --release -p rustmorloc -- panic_classifier`
  fails `a_std_panic_raised_by_runtime_code_inside_a_user_generic_is_the_runtimes`
  (exits 10, user, instead of 11). Its runtime closure compiles to the same
  code as `child_user_steps`'s `user_steps(0)`, and the dumped trace shows
  that function's frames. It passes in release with
  `RUSTC_BOOTSTRAP=1 RUSTFLAGS="-Z merge-functions=disabled"`.
  `-C llvm-args=-enable-merge-functions=false` has no effect. The hole
  needs a panic located in non-track_caller std or dependency code (a
  located panic is decided from its location, and the Location constant
  keeps the bodies distinct), in a function whose code is identical to one
  on the other side, in the same codegen unit. Linker identical-code
  folding (ld64 by default on macOS; lld or mold `--icf`) is a second
  source. Needs a ruling: disable merging in pool builds through
  RUSTC_BOOTSTRAP (unsupported but used by the Linux kernel), or another
  cost (e.g. opt-level 1). A dynamic user/runtime scope and detecting the
  merge from the trace were rejected (DA notes:
  /work/.claude/agent-memory/devils-advocate/panic-classifier-merged-functions-review.md).
- **A C++ pool's dispatch catch can throw out of the pool's C entry.**
  [read 2026-11-20, DA review] PANIC-6 gap. `cpp_local_dispatch` and
  `cpp_remote_dispatch` (pool.cpp) build the fail packet's message with
  std::string inside their catch handlers; a bad_alloc there escapes the
  dispatcher into the C pool loop (terminate or undefined behaviour) instead
  of failing the call. `_mlc_try`'s handlers allocate the same way, but there
  the new exception only escapes the `@try`.
