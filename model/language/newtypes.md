# Newtypes

`newtype`: a distinct type over a representation, and conversion to and from it. Prefix: NEWT.

### NEWT-1 A newtype is distinct from its representation
Intent: proposed
Code: unaudited

`newtype N = T` makes `N` a new type that crosses language boundaries in
the form of `T`. `N == T` never holds, and a value moves between them only
through an explicit conversion (`pack`, `unpack`). `N` owns its instances and
its native forms.

### NEWT-2 A newtype with no native form takes its representation's
Intent: ruled 2026-10-08
Code: unaudited

If `N` has no `type L => N = ...` form, `[[N]]_L` is `[[T]]_L`, following
the representation chain through further newtypes.

### NEWT-3 A newtype parameter need not appear in its representation
Intent: proposed
Code: unaudited

`newtype Buffer (n :: Nat) a = List a` is legal. This is where a type
that adds a kind or a dimension to its representation is declared (ALIAS-2).

### NEWT-4 A native form needs a way to be built
Intent: proposed
Code: deviates (unfiled: types-newtype.asc says the form is dropped silently)

A newtype with a native form in `L` that the language's binding cannot build
from the representation needs a `Packable` instance in `L`. Without one the
program is rejected.

### NEWT-5 A declaration with no right-hand side is a primitive
Intent: proposed
Code: unaudited

`newtype X a` and `type X a` both declare an opaque nominal type with no
morloc representation. Its native forms and its `Packable` instance say what
it is.

### NEWT-6 Representation chains do not cycle
Intent: proposed
Code: unaudited

`newtype A = B` with `newtype B = A`, or any longer cycle, is a declaration
error.
