# Serialization

When data crosses a language boundary, it is serialized to a binary format, transmitted, and deserialized on the other side. Morloc uses MessagePack (msgpack) as its serialization protocol.

## Schema Encoding

Each type has a compact schema string that describes its msgpack representation. Schemas are embedded in the manifest and used by both the nexus and language pools.

### Schema Syntax

| Schema | Type | Msgpack Format |
|--------|------|----------------|
| `z` | `Unit` | nil |
| `b` | `Bool` | boolean |
| `i1` | `Int8` | int8 |
| `i2` | `Int16` | int16 |
| `i4` | `Int32` | int32 |
| `i8` | `Int64` / `Int` | int64 |
| `u1` | `UInt8` | uint8 |
| `u2` | `UInt16` | uint16 |
| `u4` | `UInt32` | uint32 |
| `u8` | `UInt64` | uint64 |
| `f4` | `Float32` | float32 |
| `f8` | `Float64` / `Real` | float64 |
| `s` | `Str` | string |
| `[X]` | `List X` | array of X |
| `(X,Y)` | `(X, Y)` | array [X, Y] |
| `{k1:X,k2:Y}` | record | map {"k1": X, "k2": Y} |

### Annotated Schemas

Schemas may carry a display name prefix for CLI help text:

```
<double>f8        -- Real, displayed as "double"
<integer>i8       -- Int, displayed as "integer"
```

## Packing Rules

### Primitives

Primitives are packed directly as their msgpack counterparts: integers as msgpack integers (at the declared width), floats as msgpack floats, booleans as msgpack booleans, strings as msgpack strings.

### Lists

A `List a` is packed as a msgpack array. Each element is packed according to its element schema.

### Tuples

A tuple `(a, b, c)` is packed as a fixed-length msgpack array `[a, b, c]`.

### Records

Records are packed as msgpack maps with string keys:

```
{name = "Alice", age = 27}  -->  {"name": "Alice", "age": 27}
```

In C++, records may be packed as positional tuples for efficiency when both sides agree on field order.

## Serialization Insertion

The compiler automatically inserts serialization at language boundaries during the Serialize phase of code generation. For each cross-language call:

1. The caller's pool serializes the arguments using the argument schemas.
2. The serialized bytes are sent over the Unix socket.
3. The callee's pool deserializes the bytes into native types.
4. After execution, the result is serialized and sent back.
5. The caller deserializes the result.

## Language Bindings

Each language has a serialization library (compiled during `morloc init`):

- **Python**: `pymorloc` -- C extension providing `pack`/`unpack` functions
- **C++**: `cppmorloc` -- Template header with type-safe serialization
- **R**: `rmorloc` -- C extension for R type serialization

These libraries implement schema-driven serialization: given a schema string and a native value, they produce msgpack bytes, and vice versa.

## Cross-Language Data Representation

The same logical value may have different native representations:

| Morloc Value | Python | C++ | R | Msgpack |
|-------------|--------|-----|---|---------|
| `42` | `int(42)` | `int(42)` | `42L` | `0x2a` (positive fixint) |
| `[1,2,3]` | `[1,2,3]` | `vector{1,2,3}` | `list(1,2,3)` | array of 3 ints |
| `{x=1}` | `{"x":1}` | `struct{x:1}` | `list(x=1)` | map {"x": 1} |

The serialization layer normalizes these representations through the common msgpack format.
