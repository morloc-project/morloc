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

Morloc passes a `Copy` argument by value and gives or lends any other
value as its parameter's type asks (RUST-5), so a parameter of type
`&mut T` does not build. A parameter whose type in
the morloc signature is a type variable, or an optional or tuple holding
one, is `&T` at every instantiation, since generic host code cannot depend
on whether its instantiation is `Copy`. For the source of a class method
that signature is the method's in its class, so a parameter of a class
variable is `&T` in every instance, `Copy` or not: in `class Show a where
show :: a -> Str`, the source of `instance Show U64` takes `&u64`. One
generic source can then implement the method for many instances. HOST-10,
which calls the source at the instance's types, fixes the arguments it is
given, not how each is passed. A type with interior
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
Code: conforms 2026-11-25

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

### RUST-5 A sourced Rust parameter of a concrete type takes the value or a reference to it
Intent: ruled 2026-11-20
Tests: spec-rust-5-1
Code: conforms 2026-11-20

A parameter of a sourced Rust function whose morloc type is a concrete
type that is not `Copy` is written `T`, `&T`, `&str` for a `Str` or `&[A]`
for a list, never `&mut` or a generic or `impl Trait` type. Morloc passes
each argument to match: when the call is the argument's last use it is
given, so a `T` parameter takes it with no copy; otherwise it is lent, so
a `T` parameter takes a copy and the others a reference. A copy happens at
most once per argument and only where the caller still needs the value.
A `T` parameter whose type is not `Clone` builds only where the argument
is given. Any other parameter type is a build error naming this rule;
a generic one fails as rustc's "type annotations needed". A type-variable
parameter, and a function parameter, is a reference as RUST-1 and HOST-8
say, and so is an argument a closure passes on (a fold's step is called
with its accumulator lent: issues/call-and-stream-copies.md).
