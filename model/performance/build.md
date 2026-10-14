# Build (BUILD)

`morloc make` wall time for a program, by the pool languages it needs.

### BUILD-1 A build costs the front end plus each compiled pool's compile
Intent: open
Bench: build
Model: t(S) = c_front + sum over compiled L in S of c_L (C++, Rust), with Python and R near zero
Measured: pending

Open: whether compiled pools build concurrently, which would make the sum
a maximum. The Rust pool dominates where it is present (thin LTO and the
cargo target-dir lock, see the 2026-09-14 Rust build work); a preliminary
build of the stdlib parallel bench took 8 s for C++ and 203 s for Rust
under heavy load, not citable.
