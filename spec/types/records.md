# Records

Records are named product types with labeled fields. Morloc provides three record-like forms: `record`, `object`, and `table`.

## Record Declaration

A `record` declares a type with named fields:

```morloc
record Person where
  name :: Str
  age :: Int
```

This introduces the type `Person` with fields `name` (of type `Str`) and `age` (of type `Int`).

## Object and Table Declarations

`object` and `table` are variants of `record` that convey intended semantics:

- **`object`** -- a record representing an opaque object (fields may include functions)
- **`table`** -- a record representing columnar/tabular data

All three forms have identical syntax and structural behavior. The distinction is advisory: it informs serialization strategy and documentation but does not affect type checking.

## Language-Specific Mappings

Records require concrete type mappings, just like other types:

```morloc
record Py => Person = "dict"
record Cpp => Person = "Person"
record R => Person = "list"
```

In C++, the compiler generates a `struct` definition. In Python and R, records map to dictionaries and named lists, respectively.

## Construction

Records are constructed with brace syntax, binding field names to values:

```morloc
alice = {name = "Alice", age = 27}
```

All fields must be provided at construction.

## Field Access

The `.` operator in prefix position extracts a field:

```morloc
.name alice       -- "Alice"
.age alice        -- 27
```

Field accessors are first-class functions and may be composed or passed as arguments:

```morloc
names = map .name people
```

## Records with Function Fields

Record fields may have function types:

```morloc
record Tools where
  f :: Int -> Int
  g :: Bool -> Int

tools = {f = add 1, g = \x -> if x then 1 else 0}
.f tools 5       -- 6
```

## Parameterized Records

Records may be parameterized:

```morloc
record Pair a b where
  fst :: a
  snd :: b
```

## Serialization

When records cross language boundaries, they are serialized as msgpack maps (keyed by field name) or as positional tuples, depending on the target language's conventions. The compiler inserts appropriate pack/unpack operations. See [[../interop/serialization.md]].
