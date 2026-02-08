# Morloc Syntax

## Module Declaration

```morloc
module main (foo, bar)  -- Export foo and bar
module m (*)            -- Export everything
```

## Imports

```morloc
import root-py          -- Import all
import foo (bar, baz)   -- Import specific terms
```

## Type Declarations

**General types:**
```morloc
foo :: Int -> Int
map a b :: (a -> b) -> [a] -> [b]
```

**Language-specific mappings:**
```morloc
type Py => Int = "int"
type Cpp => List a = "std::vector<$1>" a
```

## Source Declarations (FFI)

```morloc
source Py from "foo.py" ("add", "mul")
source Cpp from "foo.hpp" ("add", "mul")
source R from "foo.R" ("add", "mul")

-- Rename on import
source Py from "foo.py" ("morloc_paste" as paste)

-- From builtin
source Py ("abs")
```

After sourcing, declare types:
```morloc
add :: Int -> Int -> Int
```

## Function Definitions

**Pure morloc:**
```morloc
double x = (x, x)
add3 x y z = x + y + z
```

**Where clauses:**
```morloc
foo x = result where
  helper y = y + 1
  z = 42
```

**Multiple implementations:**
```morloc
mean :: [Real] -> Real
mean xs = div (sum xs) (size xs)        -- Pure morloc
source Cpp from "mean.hpp" ("mean")     -- C++ version
```

See [[IMPLEMENTATION-POLYMORPHISM.md]] for details.

## Infix Operators

**Fixity declarations:**
```morloc
infixl 6 +, -     -- Left-associative, precedence 6
infixr 5 :        -- Right-associative, precedence 5
infix  4 ==, !=   -- Non-associative, precedence 4
```

Precedence range: 0-9 (higher = tighter)

**Usage:**
```morloc
f = show . (+) 42     -- Composition
g = show $ 1 + 2      -- Application

-- Operator sections
addFive = (+) 5
increment = (+ 1)
```

See [[INFIX-OPERATORS.md]] for full details.

## Function Composition

```morloc
infixl 8 .
foo = f . g . h

-- Partial application
addTwo = add 2

-- Lambdas
foo = map (\x -> x + 1)
```

## Records

```morloc
record Person where
  name :: Str
  age :: Int

alice = { name = "Alice", age = 27 }
.name alice  -- Access field
```

See [[RECORDS.md]] for details.

## Typeclasses

```morloc
class Monoid a where
  empty a :: a
  op a :: a -> a -> a

instance Monoid Int where
  empty = 0
  source Cpp from "foo.hpp" ("addInt" as op)
```

See [[TYPECLASSES.md]] for details.

## Current Limitations

- **No recursion** - Use higher-order functions instead
- **No let/in** - Use `where` clauses

---
*See also: [[IMPLEMENTATION-POLYMORPHISM.md]], [[INFIX-OPERATORS.md]], [[RECORDS.md]], [[TYPECLASSES.md]]*
