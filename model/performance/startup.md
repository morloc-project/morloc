# Startup (START)

The fixed cost of a run: what a command costs before it does any work.
This is the intercept of every other chapter's models.

### START-1 The nexus alone starts in a few milliseconds
Intent: proposed
Bench: startup
Model: t = c_nexus; `./nexus -h`, no pool
Measured: pending
Limit: help <= 30 ms

### START-2 Each pool language has its own start cost
Intent: proposed
Bench: startup
Model: t(L) = c_nexus + c_L; one call of an identity sourced in L
Measured: pending
Limit: cpp.wall <= 60 ms, rs.wall <= 60 ms, py.wall <= 200 ms, r.wall <= 800 ms

c_L is dominated by the interpreter or binary loading: R's is an order of
magnitude above Python's, which is several times C++'s and Rust's.

### START-3 Pools a run needs start concurrently
Intent: proposed
Bench: startup
Model: t(S) = c_nexus + max over L in S of c_L + e*|S|, e small
Measured: pending
Limit: all4.wall <= 800 ms

A run that needs Python, C++, R and Rust pays about R's start, not the sum
of the four. `pyCpp` and `all4` are the evidence; the note on `all4.wall`
gives the sum and the maximum of the single-pool starts of the same run.
