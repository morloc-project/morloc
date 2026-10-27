# Host calls

How morloc and sourced code call each other: arguments, callbacks, suspensions. Prefix: HOST.

### HOST-1 Morloc calls a sourced function by the arity of its source signature
Intent: ruled 2026-09-24
Code: conforms 2026-10-21

The number of arguments passed in each call to a sourced function follows its
source signature. On the morloc side `a -> b -> c` and `a -> (b -> c)` are
one type (see `APP`).

### HOST-2 A function crossing into or out of host code is called with every argument at once
Intent: ruled 2026-10-27
Tests: spec-host-2-1, spec-host-2-2
Code: conforms 2026-10-27

A function a source is given, a function it returns, and a function inside
either (a field, an element) are called with all of their arguments in one
call. `(a -> b -> c)` and `(a -> (b -> c))` are one type here as everywhere
(HOST-1), so a callback of either spelling is called with two arguments.
Only two things end the arguments of such a call: an effect, and a type
variable of the source's signature. `(a -> <IO> (b -> c))` is called with
one argument and returns a suspension (HOST-5). A parameter `(a -> b -> c)`
is called with two arguments and returns `c`, even where `c` is a function.
Morloc passes any function of the type called that way, however it was
defined.

### HOST-3 `rsize` declares the call groups of a curried foreign function
Intent: proposed
Code: deviates (issues/host-calls.md)

`--' @rsize n1 n2 ...` gives the sizes of the leading call groups; the last
group takes the rest. Each size is at least 1 and leaves at least one
argument for the group after it, or the build is rejected. An effect ends
a call: in `mk :: Int -> <IO> (Int -> Int)` the source is called with one
argument when the suspension is forced, and the function it returns is a
value of its own; `rsize` counts only the arguments before the first
effect.

### HOST-4 Parentheses never set call groups
Intent: ruled 2026-10-27
Tests: spec-host-4-1
Code: conforms 2026-10-27

`f :: Real -> ([Real] -> [Real])` is called as `f :: Real -> [Real] ->
[Real]` is: with both arguments at once. Only `rsize` splits a sourced
function's calls (HOST-3), and only an effect or a type variable of the
signature splits a callback's (HOST-2).

### HOST-5 A suspension at a host boundary is a callable of no arguments
Intent: proposed
Code: conforms 2026-10-21

A host has no thunks: calling is forcing. The adapter between morloc and a
host is decided by type at the boundary, follows the source signature, and
absorbs the terminal suspension of a sourced function into its call. The
rules are `EFF`'s.

### HOST-6 A foreign exception is a failure of the call that raised it
Intent: proposed
Code: conforms 2026-10-21

An exception escaping a sourced function, in any language and across any
number of pools, reaches morloc as a failure that `@try` can catch (see
`FAIL`). A pool that is killed outright is not such a failure.

### HOST-7 A function value may be called on threads its sourced callee starts
Intent: proposed
Tests: spec-host-7-1
Code: conforms 2026-10-21

A sourced function may call a function value it was given from threads it
starts and joins before it returns, concurrently, as a parallel map does.
This holds for every function value: a named function, a lambda and what it
captures, a partial application, one taken from a list or a record field,
one built by host code, and one from another language. Each call computes
what it would on the calling thread. A failure that morloc or another pool
raises in such a call (`@throw`, a foreign exception) unwinds to the sourced
function, and reaches `@try` around its call if the sourced function
propagates it; a panic in host code on such a thread is PANIC-6's.

### HOST-8 A Rust function value is a plain `Arc<dyn Fn>` taking every argument by reference
Intent: proposed
Tests: spec-host-8-1
Code: conforms 2026-10-21

[[a1 -> .. -> an -> b]]_Rust, held as a value, is
`std::sync::Arc<dyn Fn(&A1, .., &An) -> B + Send + Sync>`, called `f(&x1, ..)`;
[[<E> T]]_Rust held as a value is `std::sync::Arc<dyn Fn() -> T + Send + Sync>`.
Every argument of a function type is a reference, whatever its type, so a
morloc type has one Rust spelling in concrete and generic host code alike.
A sourced function's parameter that is itself a function is generic, any
`F: Fn(&A1, ..) -> B`; a function inside a parameter (a field, an element)
has the stored spelling. A sourced function may return a function as any
`impl Fn(&A1, ..) -> B + Send + Sync + 'static`, or return a stored value it
was given, which keeps its identity when its signature says so (HOST-9). Host code names no morloc type. Everything a
function value holds, and so every value a morloc closure captures and every
value a recursive type holds, must be `Send + Sync`; otherwise the build
fails, with a note naming this rule before rustc's report. A value a closure
captures in its serialized form (a packet the pool does not decode) is only
read by the closure's calls (STATE-3).

### HOST-9 A function made by host code stays in the pool that made it
Intent: ruled 2026-10-26
Tests: spec-host-9-1, spec-host-9-2, spec-host-9-3, spec-host-9-4, spec-host-9-5
Code: conforms 2026-10-26

A function made by host code has no identity outside the pool that made
it, so it never crosses a pool boundary and is never serialized. Morloc
follows where each function value was made, at every position of its type
(a function's result, an effect's value, a container's elements, a
record's fields), through every construct and every call between pool
trees:

- a sourced call makes every function in its result, except at a position
  whose type in the source's signature is a type variable that a parameter
  also holds: there the result is what the arguments hold
  (`relay :: a -> a` returns what it was given; `relay :: (Int -> Int) ->
  (Int -> Int)` returns a function it made);
- a call that stops at a call group boundary (HOST-3) returns a function
  the host made;
- a function-typed parameter of a function passed to a sourced function
  holds functions the host made (the host calls it with its own);
- a lambda, a partial application and a suspension morloc builds are
  morloc's, and hold what they capture;
- a function passed to a sourced function returns what it returns where
  that host calls it, so a function another pool makes cannot come back
  out of it.

Every expression that holds or receives a function made by host code, and
the expression that consumes it, runs in that host's language (COST-15);
an argument computed from one is computed there and only its value
crosses. A recursive function given one is copied per pattern of what it
is given and built in that language. A source chosen for a call that makes
such a function is chosen for the call alone (COST-1); a use that needs
another pool is refused with advice to import only the module that sources
it in the language needed. A call given such a function, whose term has a
source with the same call groups in the host's language, may use that
source instead of the one placement first chose. A program that would carry one across pools
(into a callback of another language, through another pool's scope, out of
an export, into a conditional that may hold functions of two pools) or
serialize one (an intrinsic, a label that caches or logs) is refused at
build.
