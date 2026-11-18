# Native types

Per-language type mappings (`type Py => X = ...`, `record Cpp => R`) and their agreement. Prefix: NATIVE.

### NATIVE-1 A native form is a template over the type's parameters
Intent: proposed
Tests: spec-native-1-1, spec-native-1-2, spec-native-1-3, spec-native-1-4, spec-native-1-5, spec-native-1-6, spec-native-1-7, spec-native-1-8, spec-native-1-9, spec-native-1-10, spec-native-1-11, spec-native-1-12, spec-native-1-13, spec-native-1-14, spec-native-1-15
Code: deviates (../compiler/issues/native-forms.md)

`type L => C x1 ... xn = "s" y1 ... ym` gives `[[C U1 ... Un]]_L` as the
text `s` with each `$i` replaced by the i-th type item of `y1 ... ym`, at
the use's arguments. The text is opaque to morloc and is checked only by
language `L`. The declared parameters are like a function's: the list after
the form may name any of them, in any order, a specialized parameter by its
type. A form with no
`$i` is its text as written, and its list may be absent. `$i` counts the
list's type items only: a Nat, Str or Rec item is never rendered, so `$i`
cannot name one. A `$i` that names no type item of the list, or a `$i`
with no list, is a compile error. The code deviates (see the issue file):
a type form's list must name the parameters in declaration order, and
later passes pair a list with the parameters by position. Open: may `$i` render a
literal kind argument (`"std::array<int,$1>" n` at `n = 3`)? Each variable
of a specialized form takes the argument in its own position. A
specialized form is the type's form only at arguments it matches; at other
arguments the type has no form of its own in `L` (NATIVE-2). Open: which
form applies when two specializations both match (today the one
specialized furthest left)?

### NATIVE-2 A type without its own form in a language uses its expansion's
Intent: proposed
Code: unaudited

An alias has the native forms of its expansion (ALIAS-1, ALIAS-6). A newtype
with no form in `L` at its arguments has its representation's (NEWT-2).

### NATIVE-3 In C++ and Rust a record needs a user-defined struct
Intent: retired 2026-11-14
Code: unaudited

Replaced by GEN-1: a record with no user-written form gets a generated one.

### NATIVE-4 An unmapped record in C++ or Rust is a compile error
Intent: retired 2026-11-14
Code: unaudited

Replaced by GEN-1: there is no unmapped record.

### NATIVE-5 Native forms for records in Python and R
Intent: retired 2026-11-14
Code: unaudited

Replaced by GEN-1, GEN-2, GEN-3 and GEN-11.

### NATIVE-6 Two native forms for one type and language
Intent: open
Code: unaudited

If two modules in a program give `T` different forms in `L`, is that a load
error, or does the form in scope where the type is declared win (MOD-14)?

### NATIVE-7 A Python list is a list whatever its elements
Intent: ruled 2026-10-28
Tests: spec-native-7-1, spec-native-7-2
Code: deviates (issues/values.md)

In Python, `[[T]]_Py` is a `list` for every `T`, `U8` and nullary `data`
included, whether the program builds the value or receives it from another
language. A compact buffer of one-byte values is `Vector n U8`, or
`Vector n D` for a nullary `data` `D`: a numpy `uint8` array whose bytes must
be valid values of the element type. A `Vector n T` literal built in Python
has the dtype a received `Vector n T` has (NATIVE-8).

### NATIVE-8 A Packable literal has the form of a received value
Intent: ruled 2026-10-28
Tests: spec-native-8-1, spec-native-8-2
Code: deviates (issues/values.md)

A literal of a Packable type whose wire form is a list has, in the pool that
builds it, the native form a value of that type received from another
language has. In Python a `Vector n T` literal has the numpy dtype of a
received one. In R a literal over a list of scalars is the atomic vector the
decoder builds for the element type: `raw` for `U8`, `integer64` for `I64`,
`logical` for `Bool`, `character` for `Str`, a factor for a nullary `data`.
Its elements are checked as values sent to another language are.

### NATIVE-9 One native form per type
Intent: ruled 2026-10-28
Tests: spec-native-9-1, spec-native-9-2, spec-native-9-3, spec-native-9-4, spec-native-9-5, spec-native-9-6, spec-native-9-7, spec-native-9-8, spec-native-9-9, spec-native-9-10, spec-native-9-11
Code: deviates (issues/values.md)

In every language L and for every type T with a schema, a value of T built
in L by a literal, whatever its elements, or decoded in L by `@read`,
`@load`, `@unpack` or `@cellget`, or handed to a folding handler's
combine, has the native form of a T that L receives from any language, L
included, and a T that `@show`, `@hash`, `@save` or `@cellput` writes is
taken apart as a T that L sends is: the same class, attributes, element
storage and element checks. The form is a function of T's schema, hints
included, and of nothing else. In Python a numpy array a literal builds or
a pool receives is read-only. In R a `Vector n U8` and a `Vector n D` (D a nullary
`data`) are `raw`, and a `[D]` is a factor.

### NATIVE-10 A Packable instance gives a language both methods or neither
Intent: ruled 2026-10-28
Tests: spec-native-10-1, spec-native-10-2, spec-native-10-3, spec-native-10-4
Code: conforms 2026-10-28

A `Packable` instance gives a language both `pack` and `unpack` for a type
with a form of its own in that language, or neither, else the program does
not build. The instance is every declaration of it (CLS-2), and a method
written in Morloc serves every language. A value that language receives is built by `pack` and one it sends is
taken apart by `unpack`, so with only one of them a received value and a
literal would differ (NATIVE-9). Where the type has no form of its own in a
language, its `unpack` there is free (NEWT-7).
