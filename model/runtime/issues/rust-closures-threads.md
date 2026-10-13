# Function values on threads a sourced function starts

What remains after Rust function values became thread-safe
(`rustmorloc::FnValueN`, HOST-7, HOST-8, STATE-2). Tests:
golden spec-host-7-1, spec-host-8-1, rust-function-arg-threads.

## A host panic on a callback thread ends the pool [read, open ruling]

PANIC-6 says a panic on a thread the user's code started ends the pool.
Under HOST-7 a manifold runs on such threads, so a panic in sourced Rust
code called from a function value under `pmap` exits with status 70, while
the same call under `map` is the call's failure, catchable by `@try`.
`@throw` and foreign failures are not affected (they unwind without the
hook; spec-host-7-1 `tryThrow`, `tryForeign`).

- morloc-runtime-types/src/panic.rs:152 `decide`: scope 0 means Exit; only
  `dispatch_guard` (rustmorloc lib.rs) opens a host scope, on the pool thread.
- Candidate: the `ThinFn` adapter (or the manifold entry) opens
  `SCOPE_HOST` when the thread has no scope, so the panic unwinds to the
  sourced function, whose join carries it to the pool thread's scope. Open:
  what a detached thread's panic then does (PANIC-7 says a thread never dies
  alone).
- Traceback frames pushed on the worker are lost with its thread; the
  message survives.

## A function value called after its sourced callee returned [read, spec gap]

HOST-7 covers calls made before the callee returns. Host code can now move
a `FnValueN` into a thread it never joins, or keep it in a static, and call
it later. Nothing says what such a call does: the dispatch that received it
has ended, SHM blocks tracked on that thread are freed only at its exit,
and after the peer pools exit a foreign call fails. Captures are owned by
the value, so this is not a dangling read in morloc-built closures; the
spec should still state the outcome.

## Recursive values shared across threads contend on refcounts [measured]

A projection out of a recursive value clones the child's `RecBox` (one
atomic increment and decrement per node visit). Single-threaded, `Arc` and
`Rc` measured equal (13-30 ns per node, ratio 0.83-1.01, noisy machine).
Threads walking ONE shared tree contend: a 2^21-node walk x4 took 110 ms
on one thread and 620 ms on two (separate trees: 148 ms and 214 ms). So
`pmap` over elements that share subtrees can be slower than `map`.
Candidate: match projections borrow instead of clone. Benchmark source:
a Rust replica of the generated walk (Rc vs Arc RecBox).

## A capturing closure at a direct sourced argument is boxed [measured]

A closure passed straight to a sourced function is built as an `Arc`
trait object and handed over through `ThinFn` as a boxed `Fn`: two
allocations per call and two indirect calls per element, no inlining.
`seqSum (\x -> x + k) n` costs about 2 ns per element more than
`seqSum addSeven n` (1e8 calls: 0.44 s vs 0.24 s). Candidate: emit a
concrete `move` closure over the capture tuple at that site, and let
StaticArgs pass a copied helper's captures through.

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
mapped only in Python (`go :: Blob -> [Int] -> [Int]`). Either is a
missing case or an unrealizable program; the message names neither.

## A failed call on a persistent host thread leaves tracked state [read]

SHM_TRACKER and TRACEBACK (rustmorloc lib.rs) are per thread and emptied by
`dispatch_flush` on the dispatch thread. A function value that fails on a
thread that outlives the call (a rayon or other persistent pool) skips
`release_packet` while unwinding, so the blocks it tracked and the frames
it pushed stay on that thread until it exits. Each caught failure there
leaks its argument blocks for the life of the process. Not reproduced.
