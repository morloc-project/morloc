# Rust

What sourced Rust code and mapped Rust types must do, and what morloc does
for them. These are the Rust forms of rules stated once for every language;
each item cites its rule. Prefix: RUST.

Rules for Rust stated elsewhere: HOST-8 (function values, `Send + Sync`),
GEN (the types morloc generates), NUM-1 (`Int` is the
native default integer), OWN (how generated Rust borrows, clones and
moves).

### RUST-1 A sourced Rust function never takes `&mut`
Intent: ruled 2026-10-18
Code: conforms 2026-10-21

Morloc passes a `Copy` argument by value and any other value as `&T`,
so a parameter of type `&mut T` does not build. A parameter whose type in
the morloc signature is a type variable, or an optional or tuple holding
one, is `&T` at every instantiation, since generic host code cannot depend
on whether its instantiation is `Copy`. A type with interior
mutability (`Cell`, `RefCell`, `Mutex`, atomics) is not changed through an
argument either; rustc does not detect that, and it is the program's error.
This is EVAL-7's rule that sourced code never changes an argument, and it
holds until morloc has a way to say where a value may be changed.

### RUST-2 A sourced Rust function returns an owned value
Intent: ruled 2026-10-18
Code: conforms 2026-10-21

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

### RUST-4 A mapped Rust enum is written as Rust code usually is
Intent: ruled 2026-11-15
Tests: spec-rust-4-1
Code: conforms 2026-11-15

A `data` type mapped to a user's enum (`data Rust => Expr = "Expr"`) has
one arm per constructor: a unit variant for a constructor with no fields,
else a tuple variant with one field per morloc field, in order. Each field
holds its morloc type `T` as `T`, `Box<T>` or `Arc<T>`, and a field of type
`?T` as `Option<T>`, `Option<Box<T>>` or `Option<Arc<T>>`. Morloc reads and
writes every field at its morloc type, so a field holding another type is a
build error naming both. Releasing a deep value of the enum is the enum's
own business (STATE-8). Morloc may take fields out of a value it holds alone
without running the enum's own `Drop`. An optional field is read as a copy,
never borrowed in place, since a pointer may leave no optional in memory to
lend: a copy that is deep behind a `Box` and a count behind an `Arc`.

Why: replaces 2026-10 contract, where every arm held one `Box` of a tuple of
its fields.
