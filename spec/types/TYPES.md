# Type System

Morloc employs a two-level type system that mediates between a language-agnostic *general* type layer and language-specific *concrete* type layers. This design enables the compiler to verify cross-language composition statically: a function's general signature is checked once, while its concrete realizations are checked per language.

## Two-Level Design

**General types** (`TypeU`) are the programmer-facing types: `Int`, `Real`, `[a]`, `a -> b`. They carry no language-specific information. Type checking, inference, and subtyping operate at this level.

**Concrete types** (`Type`) are language-specific: Python's `int`, C++'s `std::vector<int>`, R's `integer`. The compiler resolves general types to concrete types through type declarations (e.g., `type Py => Int = "int"`). Concrete types determine serialization format and foreign function signatures.

A third, minimal representation (`TypeF`) is used at code generation boundaries where only structural shape matters.

## Role in Cross-Language Safety

When a Python function's output feeds into a C++ function's input, the compiler verifies:

1. Both functions share a compatible general type at the boundary.
2. Each side has a concrete type mapping for that general type.
3. A serialization schema exists to convert between the concrete representations.

If any check fails, the program is rejected at compile time.

## Judgment Forms

The type system uses bidirectional type checking with two primary judgments:

- **Synthesis**: given an expression, infer its type.
- **Checking**: given an expression and an expected type, verify compatibility.

Subtyping connects the two: a synthesized type may be a subtype of the expected type. See [[inference.md]] and [[subtyping.md]].

## Subfiles

- [[primitive-types.md]] -- Built-in types and sized variants
- [[type-declarations.md]] -- Type aliases and language-specific mappings
- [[records.md]] -- Record, object, and table types
- [[typeclasses.md]] -- Typeclass declarations and instances
- [[polymorphism.md]] -- Parametric polymorphism and quantification
- [[subtyping.md]] -- Subtyping rules and instantiation
- [[inference.md]] -- Bidirectional type inference
