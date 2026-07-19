# Primitive Types

Morloc provides a set of built-in types that map to native representations across all supported languages.

## Core Types

| Type | Description |
|------|-------------|
| `Unit` | The unit type (no meaningful value) |
| `Bool` | Boolean (`True` or `False`) |
| `Int` | Default-width signed integer |
| `Real` | Default-width floating-point number |
| `Str` | Unicode string |

## Sized Integer Types

| Type | Width | Signedness |
|------|-------|------------|
| `I8` | 8-bit | Signed |
| `I16` | 16-bit | Signed |
| `I32` | 32-bit | Signed |
| `I64` | 64-bit | Signed |
| `U8` | 8-bit | Unsigned |
| `U16` | 16-bit | Unsigned |
| `U32` | 32-bit | Unsigned |
| `U64` | 64-bit | Unsigned |

The unsized `Int` type is equivalent to the platform's default integer width. For serialization, `Int` uses the msgpack `i8` (64-bit signed) schema.

## Sized Floating-Point Types

| Type | Precision |
|------|-----------|
| `F32` | IEEE 754 single precision |
| `F64` | IEEE 754 double precision |

The unsized `Real` type is equivalent to `Float64`.

## Collection Types

| Type | Description |
|------|-------------|
| `List a` or `[a]` | Homogeneous ordered collection |
| `Tuple` | Fixed-length heterogeneous product (e.g., `(Int, Str)`) |

List syntax `[a]` is sugar for `List a`.

Tuple types are written with parentheses and commas: `(Int, Str, Bool)`.

## Cross-Language Mapping

Each primitive type must have a concrete mapping for every language in which it is used. The standard library provides these mappings:

| Morloc | Python | C++ | R |
|--------|--------|-----|---|
| `Bool` | `bool` | `bool` | `logical` |
| `Int` | `int` | `int` | `integer` |
| `Real` | `float` | `double` | `numeric` |
| `Str` | `str` | `std::string` | `character` |
| `[a]` | `list` | `std::vector<A>` | `list` |

See [[type-declarations.md]] for how these mappings are declared and [[../interop/type-mappings.md]] for the complete mapping tables.
