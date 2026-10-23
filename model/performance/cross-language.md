# Cross-language calls (XL)

The cost of a call that leaves one pool for another, against the same call
made inside one pool.

### XL-1 A foreign call costs about 20 us per round trip
Intent: ruled 2026-10-13
Bench: xl-calls
Model: t(n) = c + b*n; the outer language's loop calls a function sourced in the inner language n times with an I64; b is one round trip
Measured: pending
Goal: py->cpp.b <= 20 us, cpp->py.b <= 20 us, cpp->rs.b <= 20 us, rs->cpp.b <= 20 us, py->rs.b <= 20 us, rs->py.b <= 20 us
Limit: py->cpp.b <= 40 us, cpp->py.b <= 40 us, cpp->rs.b <= 40 us, rs->cpp.b <= 40 us, py->rs.b <= 40 us, rs->py.b <= 40 us

The floor on the reference host is about 13 us per round trip. The goal
is about 20 us; the code was near 30 us when this was ruled; past 40 us is
a failure. The pairs with R are measured but not
bound: R's own call cost (XL-2) is part of b, and no target is ruled for it.
A preliminary run under heavy load (load average 10, not citable) put the
R pairs at 90-135 us.

### XL-2 A call inside one pool costs what the language's own call costs, plus a constant
Intent: proposed
Bench: xl-calls
Model: b_morloc(L) = b_native(L) + w_L; b from `<L>-><L>` and `native-<L>`, the same loop calling the function directly
Measured: results/2026-10-16-2156-7dd39628+dirty-frame-stack: b py 78.6 ns (native 56.0), cpp 0.73 ns (0.41), r 84027 ns (83182), rs 0.88 ns (native loop optimized away)
Limit: py->py.b <= 1000 ns, r->r.b <= 100000 ns, cpp->cpp.b <= 20 ns, rs->rs.b <= 20 ns

w_L is the generated wrapper's cost. The R limit allows for the native
loop's own cost: `I64` is a `bit64` integer in R, whose arithmetic costs
about 80 us per iteration.

### XL-3 Crossing a value costs a constant plus a cost per element or byte
Intent: proposed
Bench: xl-payload
Model: t(n) = c + b*n; a value of size n built in the source language, its length taken in the target. `<kind>:<src>-><dst>.cross` = b(src->dst) - b(src->src), the slope the crossing adds to building the value
Measured: pending
Limit: reals:py->cpp.cross <= 200 ns, reals:cpp->py.cross <= 200 ns, str:cpp->py.cross <= 20 ns, strs:cpp->py.cross <= 1000 ns

Kinds: `reals` = [Real] (n elements), `str` = Str (n bytes), `strs` = [Str]
of 8-byte strings (n elements). The table crossing measured on 2026-09-13
(~150 ms fixed + 0.7 ms/MB for Arrow tables, 6.5 ms/MB for a plain
[Real]) predates this bench and is not covered by it. Rust's per-element
vector encoding (runtime/issues/encode-throughput.md, section 1) predicts
that Rust's slopes for `reals` are several times C++'s.

### XL-4 Building and measuring a value in one pool never touches the wire
Intent: proposed
Bench: xl-payload
Model: b(L->L) = cost of building the value natively in L
Measured: pending

The control for XL-3. A slope here far above the language's own cost of
building a list would mean an in-pool composition is being serialized.
