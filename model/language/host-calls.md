# Host calls

How morloc and sourced code call each other: arguments, callbacks, suspensions. Prefix: HOST.

### HOST-1 Morloc calls a sourced function by the arity of its source signature
Intent: ruled 2026-09-24
Code: conforms 2026-10-21

The number of arguments passed in each call to a sourced function follows its
source signature. On the morloc side `a -> b -> c` and `a -> (b -> c)` are
one type (see `APP`).

### HOST-2 A sourced function calls a callback by the callback's type as written
Intent: ruled 2026-09-24
Tests: spec-host-2-1
Code: conforms 2026-10-21

A callback parameter written `(a -> b -> c)` in the source signature is
called with both arguments at once; one written `(a -> (b -> c))` is called
with one argument and returns a function. Morloc passes any function of that
type in that grouping, however it was defined.

### HOST-3 `rsize` declares the call groups of a curried foreign function
Intent: proposed
Code: deviates (issues/host-calls.md)

`--' rsize: n1 n2 ...` gives the sizes of the leading call groups; the last
group takes the rest. Each size is at least 1 and leaves at least one
argument for the group after it, or the build is rejected.

### HOST-4 Whether parentheses in a sourced function's own type set its call groups
Intent: open
Code: unaudited

HOST-2 honors parentheses in a callback's type, but the manual says writing
`f :: Real -> ([Real] -> [Real])` does not change how `f` is called; only
`rsize` does. Either (a) the outer type's parentheses set the groups too,
replacing `rsize`; or (b) only `rsize` does, and the asymmetry is the rule.

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
was given, which keeps its identity. Host code names no morloc type. Everything a
function value holds, and so every value a morloc closure captures and every
value a recursive type holds, must be `Send + Sync`; otherwise the build
fails, with a note naming this rule before rustc's report. A value a closure
captures in its serialized form (a packet the pool does not decode) is only
read by the closure's calls (STATE-3).

### HOST-9 A function a sourced function passes in stays in that sourced function's pool
Intent: proposed
Tests: spec-host-9-1
Code: deviates model/compiler/issues/realization.md

A function-typed parameter of a lambda passed to a sourced function is a
function made by host code, whatever the host passes, and so is the matching
parameter of a recursive function that such a function is given. It has no
identity outside the host's pool, so it never crosses a pool boundary:

- an application of it runs in the host's language;
- an argument computed from it for a sourced function of another language
  is computed in the host's language, and only its value crosses;
- a recursive function given it is built in the host's language, a copy per
  host language and per set of argument positions that receive one, its
  recursion included; a function value such a copy may apply to it is built
  there too;
- passing it, or a closure that captures it, to a function of another
  language is a build error, as is any program that would serialize it.
