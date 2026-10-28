# Native types

Per-language type mappings (`type Py => X = ...`, `record Cpp => R`) and their agreement. Prefix: NATIVE.

### NATIVE-1 A native form is a template over the type's parameters
Intent: proposed
Code: unaudited

`type L => C x1 ... xn = "s" x1 ... xn` gives `[[C U1 ... Un]]_L` as the
text `s` with each `$i` replaced by `[[Ui]]_L`. The text is opaque to morloc
and is checked only by language `L`.

### NATIVE-2 A type without its own form in a language uses its expansion's
Intent: proposed
Code: unaudited

An alias has the native forms of its expansion (ALIAS-1, ALIAS-6). A newtype
with no form in `L` has its representation's (NEWT-2).

### NATIVE-3 In C++ and Rust a record needs a user-defined struct
Intent: ruled 2026-10-05
Code: deviates (issues/values.md)

A record used in C++ is a struct the user defines and links with
`record Cpp => R = "R"`; in Rust, with `record Rust => R`. Python and R need
no user-written native definition.

### NATIVE-4 An unmapped record in C++ or Rust is a compile error
Intent: proposed
Code: deviates (issues/values.md)

A program that needs a record in C++ or Rust with no linked struct is
rejected with an error naming the record and the language. It never builds
generated code that fails to compile.

### NATIVE-5 Native forms for records in Python and R
Intent: open
Code: unaudited

NATIVE-3 says Python and R need nothing, but the manual writes
`record Py => R = "dict"` and `record R => R = "list"`. Candidates: (a) such
lines are optional and name the only legal forms; (b) they are optional and
may name another class, built through `Packable`; (c) they are an error.

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
