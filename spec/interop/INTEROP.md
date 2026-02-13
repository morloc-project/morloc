# Cross-Language Interoperability

Morloc's central design goal is composing functions across Python, C++, and R within a single program. This section describes how the language, type system, and runtime cooperate to make cross-language calls transparent to the programmer.

## Why Serialization Boundaries Exist

Each supported language has its own memory layout, calling conventions, and type system. A Python `list` and a C++ `std::vector` cannot share memory directly. Morloc bridges this gap through serialization: when data crosses a language boundary, it is packed into a language-neutral binary format (msgpack), transmitted, and unpacked on the other side.

The compiler's job is to:

1. Determine *where* language boundaries fall in a composition.
2. Insert serialization/deserialization operations at those boundaries.
3. Choose language implementations to *minimize* the number of boundaries.

## How It Works

A function like `map` may have implementations in Python, C++, and R. When the programmer writes `map f xs`, the compiler examines `f` to determine which languages can provide it. If `f` is only available in C++, the compiler selects C++'s `map` as well, avoiding a serialization boundary between `map` and `f`.

This process -- called *realization* -- happens entirely at compile time. The generated program contains no runtime language selection logic.

## Subfiles

- [[foreign-functions.md]] -- Source declarations, function binding, renaming
- [[type-mappings.md]] -- How general types resolve to Python, C++, and R types
- [[serialization.md]] -- Msgpack protocol, schema encoding, packing rules
- [[implementation-selection.md]] -- Realization algorithm and boundary minimization
