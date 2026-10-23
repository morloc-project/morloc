# Newtypes

`newtype`: a distinct type over a representation, and conversion to and from it. Prefix: NEWT.

### NEWT-1 A newtype is distinct from its representation
Intent: proposed
Code: unaudited

`newtype N = T` makes `N` a new type that crosses language boundaries in
the form of `T`. `N == T` never holds, and a value moves between them only
through an explicit conversion (`pack`, `unpack`; NEWT-7, NEWT-8). `N` owns
its instances and its native forms.

### NEWT-2 A newtype with no native form takes its representation's
Intent: ruled 2026-10-08
Tests: spec-newt-2-1
Code: conforms 2026-10-09

If `N` has no `type L => N = ...` form, `[[N]]_L` is `[[T]]_L`, following
the representation chain through further newtypes.

### NEWT-3 A newtype parameter need not appear in its representation
Intent: proposed
Tests: spec-newt-3-1
Code: conforms 2026-10-09

`newtype Buffer (n :: Nat) a = List a` is legal. This is where a type
that adds a kind or a dimension to its representation is declared (ALIAS-2).

### NEWT-4 A native form needs a way to be built
Intent: proposed
Tests: spec-newt-4-1, spec-newt-4-2
Code: deviates (issues/values.md)

A newtype with a native form in `L` that the language's binding cannot build
from the representation needs a `Packable` instance in `L`. Without one the
program is rejected.

### NEWT-5 A declaration with no right-hand side is a primitive
Intent: proposed
Code: unaudited

`newtype X a` and `type X a` both declare an opaque nominal type with no
morloc representation. It is either a built-in wire primitive, which the
compiler serializes itself, or a type that lives in one language: its
native forms say what it is, it has no `Packable` instance (NEWT-9), and a
value of it cannot cross between languages.

### NEWT-6 Representation chains do not cycle
Intent: proposed
Code: unaudited

`newtype A = B` with `newtype B = A`, or any longer cycle, is a declaration
error.

### NEWT-7 A newtype with a representation can be unwrapped
Intent: ruled 2026-10-18
Tests: spec-newt-7-1, spec-newt-7-2, spec-newt-7-3, spec-newt-7-4, spec-newt-7-5, spec-newt-7-6, spec-newt-7-7, spec-newt-7-8, spec-newt-7-9, spec-newt-7-10, spec-newt-7-11, spec-newt-7-12
Code: conforms 2026-10-21

`newtype N vs = T`, declared where the compiler's `Packable` class is in
scope, gives `unpack :: N vs -> T`, as if `instance Packable T (N vs)` gave
it. A `deriving (Packable)` clause where that class is not in scope is an
error. Records, `data` and
the runtime handles (`IFile`, `IStream`, `OStream`, `Cell`,
`PatternChain`) are not covered, and only the compiler's `Packable` class
(methods `pack` and `unpack`) is given these instances. The conversion
changes no value, so a more specific instance never hides it. Within
one language `L` it is available only when `N` declares no form in `L`;
otherwise it runs where `N` has none, and the value crosses to it, or the
build is rejected (REAL-3). Explicit instances for the same head are
alternatives to it (REAL-1).

### NEWT-8 `deriving (Packable)` adds `pack`
Intent: ruled 2026-10-18
Tests: spec-newt-8-1, spec-newt-8-2, spec-newt-8-3, spec-newt-8-4, spec-newt-8-5, spec-newt-8-6, spec-newt-8-7, spec-newt-8-8, spec-newt-8-9
Code: conforms 2026-10-21

`newtype N vs = T deriving (Packable)` also gives `pack :: T -> N vs`, a
representation cast realized as in NEWT-7. It is rejected unless every
parameter of `N` occurs in `T` outside Nat arithmetic and type functions,
so that `T` determines the newtype a value is packed into. Only a newtype
may derive `Packable`, and a runtime handle may not.

Why: `pack` asserts what `N` adds to `T`; a parameter `T` does not
determine (a dimension, a phantom tag) would be claimed unchecked.

### NEWT-9 A type with a `Packable` instance has a representation
Intent: ruled 2026-10-18
Tests: spec-newt-9-1, spec-newt-9-2, spec-newt-9-3, spec-newt-9-4, spec-newt-9-5, spec-newt-9-6, spec-newt-9-7
Code: conforms 2026-10-21

An instance `Packable W (N as)` requires `newtype N vs = T`, and `W` is
`T[vs := as]` as types (aliases expanded, Nat expressions normalized). The
head may specialize `N`'s parameters; the wire type follows. Built-in wire
primitives are exempt. A record or `data` type has a wire form of its own and
no `Packable` instance.

Why: a wire form fixed at the declaration is the same for every program
that uses the type; one given only by instances could differ between
programs that exchange data.
