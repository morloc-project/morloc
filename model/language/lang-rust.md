# Rust

What sourced Rust code and mapped Rust types must do, and what morloc does
for them. These are the Rust forms of rules stated once for every language;
each item cites its rule. Prefix: RUST.

Rules for Rust stated elsewhere: HOST-8 (function values, `Send + Sync`),
NATIVE-3 and NATIVE-4 (a record needs a user struct), NUM-1 (`Int` is the
native default integer), OWN (how generated Rust borrows, clones and
moves).

### RUST-1 A sourced Rust function never takes `&mut`
Intent: ruled 2026-10-18
Code: unaudited

Morloc passes a `Copy` argument by value and any other value as `&T`,
so a parameter of type `&mut T` does not build. A type with interior
mutability (`Cell`, `RefCell`, `Mutex`, atomics) is not changed through an
argument either; rustc does not detect that, and it is the program's error.
This is EVAL-7's rule that sourced code never changes an argument, and it
holds until morloc has a way to say where a value may be changed.

### RUST-2 A sourced Rust function returns an owned value
Intent: ruled 2026-10-18
Code: unaudited

A sourced Rust function's result borrows from none of its arguments: its
type has no lifetime tied to a parameter. Morloc binds results as owned
values, so a borrowing result does not build (EVAL-7).

### RUST-3 A mapped Rust type may be read from several threads at once
Intent: ruled 2026-10-18
Code: unaudited

A mapped type is `Sync` in practice, not only in its declaration: reading
it through `&T` is safe while other threads do the same with the same value
(EVAL-7). Morloc may share one value between parallel workers instead of
cloning it.
