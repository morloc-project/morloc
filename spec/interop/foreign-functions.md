# Foreign Functions

Foreign function declarations bind functions from Python, C++, or R source files to morloc names. This is morloc's FFI (Foreign Function Interface).

## Source Declarations

The `source` keyword declares where a foreign function lives:

```morloc
source Py from "module.py" ("add", "mul")
source Cpp from "module.hpp" ("add", "mul")
source R from "module.R" ("add", "mul")
```

Each declaration specifies:
- **Language**: `Py`, `Cpp`, or `R`
- **Source file**: path to the implementation file (relative to the module)
- **Function list**: names of functions to import from that file

After a source declaration, the imported names must be given morloc type signatures:

```morloc
source Py from "stats.py" ("mean", "stdev")

mean :: [Real] -> Real
stdev :: [Real] -> Real
```

## Renaming

Foreign functions can be renamed on import to match morloc naming conventions or avoid conflicts:

```morloc
source Py from "module.py" ("python_add" as add)
source Cpp from "module.hpp" ("cpp_multiply" as mul)
```

The foreign name appears first, followed by `as` and the morloc name.

## Built-in Imports

Functions from a language's standard library can be imported without a file path:

```morloc
source Py ("abs", "len", "sorted")
source Cpp ("std::sort" as sort)
```

The compiler generates the appropriate import/include statement for the target language.

## Operator Binding

Operators can be bound to foreign functions using parenthesized syntax:

```morloc
source Py from "pipe.py" ("pipe" as (|>))
```

## Multiple Implementations

The same morloc function may have source declarations in multiple languages:

```morloc
source Py from "stats.py" ("mean")
source Cpp from "stats.hpp" ("mean")
source R from "stats.R" ("mean")

mean :: [Real] -> Real
```

This creates implementation polymorphism: the compiler selects the most efficient implementation at compile time. See [[implementation-selection.md]].

## Interaction with Pure Definitions

A function may have both a pure morloc definition and foreign implementations:

```morloc
mean :: [Real] -> Real
mean xs = div (sum xs) (size xs)           -- pure morloc
source Cpp from "stats.hpp" ("mean")       -- C++ override
```

When the compiler can use the C++ implementation (e.g., the caller is also in C++), it prefers the native implementation over the composed morloc version.

## Source File Resolution

Source file paths are resolved relative to the module directory. For installed packages, this means relative to the package's installation directory.
