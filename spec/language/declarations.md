# Declarations

Top-level declarations define the structure of a morloc program: modules, imports, functions, types, and foreign bindings.

## Module Declaration

Every morloc file may begin with a module declaration specifying the module name and its exports:

```morloc
module main (foo, bar)     -- export foo and bar
module utilities (*)       -- export everything
```

If no module declaration is present, the file is treated as an anonymous module.

## Import Declarations

Imports bring names from other modules into scope:

```morloc
import root-py                  -- import all exports
import foo (bar, baz)           -- import specific names
import math (sin, cos, pi)      -- selective import
```

Multiple imports of the same function from different language modules create implementation polymorphism:

```morloc
import root-py (map, filter)
import root-cpp (map, filter)
-- map and filter now have both Python and C++ implementations
```

## Type Signatures

Type signatures declare the general type of a function:

```morloc
add :: Int -> Int -> Int
map a b :: (a -> b) -> [a] -> [b]
```

Type variables (lowercase) introduce parametric polymorphism. They may be listed after the function name and before `::` to indicate universal quantification.

## Function Definitions

Functions are defined by equation:

```morloc
double x = (x, x)
add3 x y z = x + y + z
```

A function may have both a morloc definition and one or more foreign implementations. The compiler selects among available implementations at compile time.

```morloc
mean :: [Real] -> Real
mean xs = div (sum xs) (size xs)         -- pure morloc definition
source Cpp from "stats.hpp" ("mean")     -- C++ implementation
```

## Type Alias Declarations

Type aliases give names to type expressions:

```morloc
type Filename = Str
type Matrix a = [[a]]
```

Language-specific type declarations map general types to concrete representations:

```morloc
type Py => Int = "int"
type Cpp => Int = "int"
type R => Int = "integer"

type Py => List a = "list" a
type Cpp => List a = "std::vector<$1>" a
type R => List a = "list" a
```

The `$1`, `$2`, ... syntax in language-specific type strings refers to the positional type parameters. See [[../types/type-declarations.md]].

## Record Declarations

Records define named product types with labeled fields:

```morloc
record Person where
  name :: Str
  age :: Int
```

Language-specific record mappings:

```morloc
record Py => Person = "dict"
record Cpp => Person = "Person"
record R => Person = "list"
```

Object and table declarations are variants of record declarations. See [[../types/records.md]].

## Typeclass Declarations

Typeclasses define overloaded interfaces:

```morloc
class Monoid a where
  empty a :: a
  op a :: a -> a -> a
```

## Instance Declarations

Instances provide typeclass implementations for specific types:

```morloc
instance Monoid Int where
  empty = 0
  source Cpp from "monoid.hpp" ("addInt" as op)
  source Py from "monoid.py" ("addInt" as op)
```

See [[../types/typeclasses.md]].

## Source Declarations (Foreign Function Interface)

Source declarations bind foreign functions to morloc names:

```morloc
source Py from "module.py" ("add", "mul")
source Cpp from "module.hpp" ("add", "mul")
source R from "module.R" ("add", "mul")
```

Renaming on import:

```morloc
source Py from "module.py" ("python_add" as add)
```

Importing built-in functions (no file path):

```morloc
source Py ("abs", "len")
```

See [[../interop/foreign-functions.md]].

## Fixity Declarations

Fixity declarations specify operator precedence and associativity:

```morloc
infixl 6 +, -
infixr 5 :
infix  4 ==, !=
```

See [[operators.md]].
