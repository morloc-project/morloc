# Implementation Polymorphism

## Core Design Philosophy

Morloc provides **implementation polymorphism**: the same function can have multiple language-specific implementations. The compiler automatically selects the most efficient one at compile time.

## How It Works

**Multiple Implementations, One Name:**
```morloc
import root-py (map, add)   -- Python implementations
import root-cpp (map)        -- C++ implementation of map
import root-r (map)          -- R implementation of map

doubleAll xs = map (add 1) xs
```

The compiler chooses Python's `map` because `add` is Python-only (minimizes cross-language serialization).

**Collapse Around Specialized Functions:**
```morloc
import root-py (map, filter)
import root-cpp (map, filter, expensiveImageFilter)

processImage imgs = map expensiveImageFilter (filter isValid imgs)
```

Since `expensiveImageFilter` is C++-only, the compiler uses C++ for `map` and `filter` too.

## Root Module Architecture

- `root` - General (language-agnostic) signatures
- `root-py`, `root-cpp`, `root-r` - Language-specific implementations
- Importing multiple roots gives the compiler implementation choices

## Compile-Time Selection

Implementation selection happens during compilation, not runtime:
1. Compiler analyzes function dependencies
2. Selects implementations to minimize language boundaries
3. Generates efficient nexus dispatch code

No runtime overhead.

## Benefits

- **Language-agnostic code** - Use `map`, `fold`, `filter` without specifying language
- **Automatic optimization** - Compiler minimizes serialization
- **Clean composition** - `f . g . h` optimizes automatically
- **Type-driven** - Selection based on types and context

## Explicit Language Control

Use different names when you need explicit language selection:

```morloc
-- foopy.loc
source Py from "foo.py" ("pyAdd", "pyMul")

-- foocpp.loc
source Cpp from "foo.cpp" ("cppAdd", "cppMul")

-- main.loc
import foopy (pyAdd)
import foocpp (cppMul)

mixedOps x = pyAdd (cppMul x 5) 10  -- Forces language boundary
```

## Mental Model

Morloc functions have:
1. **General signature** (language-agnostic type)
2. **Zero or more implementations** (language-specific code)

When writing `map (add 1) xs`, compiler thinks: "add is Python-only, so use Python's map to avoid boundaries."

## Practical Advice

**Default to polymorphism:**
```morloc
import root-py (map, filter, fold)
import root-cpp (map, filter, fold)
import root-r (map, filter, fold)
```

**Add specialized functions:**
```morloc
source Cpp from "image.hpp" ("applyKernel")
```

**Let it collapse:**
```morloc
process = map applyKernel . filter isValid
-- Collapses to C++ if applyKernel is C++-only
```

## Inspect Compiler Choices

```bash
morloc dump main.loc    # See intermediate representations
morloc make -v main.loc # Verbose selection info
```

---
*See also: [[MORLOC_SYNTAX.md]], [[CODEGEN.md]], [[TYPECHECKING.md]]*
