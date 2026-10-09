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
Code: unaudited

A record used in C++ is a struct the user defines and links with
`record Cpp => R = "R"`; in Rust, with `record Rust => R`. Python and R need
no user-written native definition.

### NATIVE-4 An unmapped record in C++ or Rust is a compile error
Intent: proposed
Code: unaudited

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
