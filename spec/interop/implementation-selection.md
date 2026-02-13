# Implementation Selection

When a function has implementations in multiple languages, the compiler must choose which to use. This process -- called *realization* -- minimizes the number of serialization boundaries in the generated program.

## The Realization Problem

Consider:

```morloc
import root-py (map, add)
import root-cpp (map)

doubleAll xs = map (add 1) xs
```

`map` has Python and C++ implementations. `add` has only Python. The compiler must choose: if it selects C++'s `map`, then `add 1` must be serialized into C++ and back -- an unnecessary boundary. If it selects Python's `map`, everything runs in one language with no serialization.

## Selection Algorithm

The realization algorithm works as follows:

1. **Build the dependency graph.** For each function call, record which implementations are available and which functions it calls.

2. **Propagate language constraints.** Functions with only one language implementation constrain their callers. In the example above, `add` being Python-only forces `map` to prefer Python.

3. **Score candidate implementations.** For each function with multiple implementations, score each candidate by counting the serialization boundaries it would introduce.

4. **Select minimum-boundary implementations.** Choose the implementation that minimizes total serialization cost.

## Collapse Behavior

When a specialized function (available in only one language) is nested inside a polymorphic function (available in several languages), the outer function "collapses" to the same language:

```morloc
import root-py (map, filter)
import root-cpp (map, filter, applyKernel)

process imgs = map applyKernel (filter isValid imgs)
```

Since `applyKernel` is C++ only, both `map` and `filter` collapse to C++, avoiding two serialization boundaries.

## Explicit Language Control

When the programmer wants to force a specific language, they can use distinct names:

```morloc
import foopy (pyAdd)
import foocpp (cppMul)

mixedOps x = pyAdd (cppMul x 5) 10
```

Here the language boundary between `cppMul` and `pyAdd` is explicit and intentional. The compiler inserts serialization at that boundary.

## Compile-Time Only

Implementation selection is entirely static. The generated program contains no runtime dispatch logic for language choice. Each function call in the generated code targets a specific pool in a specific language.

## Validation

During realization, the compiler validates that:

- Every function in the program has at least one implementation.
- Every selected implementation has the necessary concrete type mappings.
- The serialization schemas are consistent at every boundary.

If validation fails, the compiler reports which functions lack implementations or which type mappings are missing.

## Inspecting Selections

The compiler's implementation choices can be inspected:

```bash
morloc dump script.loc       -- show intermediate representations
```

The dump output includes the realized program with language annotations on each function node.
