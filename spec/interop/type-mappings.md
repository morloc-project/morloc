# Type Mappings

Type mappings define how morloc's general types resolve to concrete representations in each target language. Every general type used at a language boundary must have a concrete mapping for that language.

## Mapping Declaration Syntax

```morloc
type <Lang> => <Name> <params> = "<concrete-string>" <params>
```

Examples:

```morloc
type Py => Int = "int"
type Cpp => Int = "int"
type R => Int = "integer"

type Py => List a = "list" a
type Cpp => List a = "std::vector<$1>" a
type R => List a = "list" a
```

## Parameter Substitution

In the concrete type string, `$1`, `$2`, etc. are replaced by the rendered concrete types of the corresponding parameters:

```morloc
type Cpp => Map k v = "std::map<$1,$2>" k v
```

`Map Str Int` in C++ becomes `std::map<std::string,int>`.

For languages where containers are unparameterized (Python, R), parameters are listed but do not appear in the string:

```morloc
type Py => Map k v = "dict" k v
```

## Standard Mappings

### Primitive Types

| Morloc | Python | C++ | R |
|--------|--------|-----|---|
| `Bool` | `"bool"` | `"bool"` | `"logical"` |
| `Int` | `"int"` | `"int"` | `"integer"` |
| `Int8` | `"int"` | `"int8_t"` | `"integer"` |
| `Int16` | `"int"` | `"int16_t"` | `"integer"` |
| `Int32` | `"int"` | `"int32_t"` | `"integer"` |
| `Int64` | `"int"` | `"int64_t"` | `"integer"` |
| `UInt8` | `"int"` | `"uint8_t"` | `"integer"` |
| `UInt16` | `"int"` | `"uint16_t"` | `"integer"` |
| `UInt32` | `"int"` | `"uint32_t"` | `"integer"` |
| `UInt64` | `"int"` | `"uint64_t"` | `"integer"` |
| `Real` | `"float"` | `"double"` | `"numeric"` |
| `Float32` | `"float"` | `"float"` | `"numeric"` |
| `Float64` | `"float"` | `"double"` | `"numeric"` |
| `Str` | `"str"` | `"std::string"` | `"character"` |
| `Unit` | `"None"` | `"void"` | `"NULL"` |

### Collection Types

| Morloc | Python | C++ | R |
|--------|--------|-----|---|
| `List a` / `[a]` | `"list"` | `"std::vector<$1>"` | `"list"` |
| `(a, b)` | `"tuple"` | `"std::tuple<$1,$2>"` | `"list"` |
| `(a, b, c)` | `"tuple"` | `"std::tuple<$1,$2,$3>"` | `"list"` |

### Record Types

Records use per-declaration mappings:

```morloc
record Py => Person = "dict"
record Cpp => Person = "Person"       -- generates a struct
record R => Person = "list"
```

## Resolution Process

When the compiler needs the concrete type of a general type for a specific language:

1. Fully expand general type aliases.
2. Look up the language-specific mapping for the resulting type constructor.
3. Recursively resolve type parameters.
4. Substitute resolved parameters into the concrete string.

If no mapping exists for a required type in a required language, compilation fails with an error identifying the unmapped type.
