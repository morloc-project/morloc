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

## Specialization and Sharing

A definition is compiled once per type it is used at, not once per use.
Each such specialization is either copied into its uses or shared:

- **Copied.** Small specializations are copied into each use and realized
  there, so each copy may land in a different language.
- **Shared.** A larger specialization (over 32 nodes) is compiled once and
  called from each use. It is realized once per language its uses need,
  and uses that land in the same language call the same copy. Nesting
  definitions therefore costs build time and code size in proportion to
  the source, not exponentially in depth.

A use of a shared specialization keeps the definition's sourced
implementations as alternatives, so choosing an implementation per use
works as before; the shared copy is one more alternative, scored like the
others.

Sharing never changes what a program computes or how often. A
specialization stays copied when it:

- recurses back to itself (a recursion is compiled with the use it
  belongs to);
- does work before returning a function (a staged definition);
- reads a top-level constant, which is computed once per command at its
  use and would otherwise be computed on every call;
- has a cache, log, benchmark, label or remote setting, on the use or the
  definition, so that setting stays with its site.

A command evaluated without a language pool calls a shared specialization
as a named function in its manifest. One that takes a function as an
argument is copied into each call instead, since that evaluator holds no
function values.

## Named Values Are Computed Once

A function definition is code: it is realized separately wherever a use
needs it. A named value that is data (its type holds no function and no
suspension) is one value: it is computed once, in one language, and passed
to every reader, whatever language each reader lands in.

Because a named value is computed once, readers in different languages see
the same value even where two implementations of the function computing it
would differ.

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
