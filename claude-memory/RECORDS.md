# Records

## Declaration

**General record:**
```morloc
record Person where
  name :: Str
  age :: Int
```

**Language-specific mappings:**
```morloc
record Py => Person = "dict"
record Cpp => Person = "struct"
record R => Person = "list"
```

## Construction

```morloc
alice = { name = "Alice", age = 27 }
```

## Field Access

```morloc
.name alice  -- Returns "Alice"
.age alice   -- Returns 27
```

## Records with Function Fields

```morloc
record Tools where
  f :: Int -> Int
  g :: Bool -> Int

tools = { f = add 1, g = \x -> if x then 1 else 0 }

.f tools 5  -- Returns 6
```

---
*See also: [[MORLOC_SYNTAX.md]], [[TYPECHECKING.md]]*
