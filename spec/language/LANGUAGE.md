# Language Reference

Morloc is a typed, functional workflow language for composing functions across Python, C++, and R under a unified type system. Programs are written in a declarative, ML-style syntax. The compiler resolves types, selects language implementations, and generates executables that orchestrate cross-language calls at runtime.

## Design Philosophy

- **Language-agnostic composition.** Functions from different languages compose as naturally as functions within a single language. The programmer writes workflows; the compiler handles language boundaries.
- **Types as contracts.** A single general type system mediates between languages. Each language maps general types to its own concrete types. The compiler verifies consistency across these mappings.
- **Pure and strict.** Morloc code is pure: side effects originate in foreign function implementations and are tracked by effect types (see [[../types/effects.md]]). Arguments are evaluated once, where they are applied. Definitions may recurse; a tail-recursive one runs as a loop.
- **Serialization is implicit.** When a function in one language calls a function in another, the compiler inserts serialization and deserialization automatically. The programmer never writes marshalling code.

## Core Concepts

- **Modules** organize code into namespaces with explicit exports. See [[LANGUAGE.md#modules]] and [[../modules/MODULES.md]].
- **Expressions** are the building blocks: function application, lambda abstraction, composition, conditionals, `let`, and where-clauses. See [[expressions.md]].
- **Declarations** define functions, type signatures, type aliases, records, typeclasses, and foreign function bindings. See [[declarations.md]].
- **Operators** support user-defined infix syntax with fixity declarations. See [[operators.md]].
- **Annotations** attach documentation and CLI metadata to exported functions. See [[annotations.md]].

## Subfiles

- [[lexical-structure.md]] -- Tokens, literals, comments, whitespace, indentation
- [[expressions.md]] -- Expression forms
- [[declarations.md]] -- Top-level declaration forms
- [[operators.md]] -- Operator syntax and fixity
- [[annotations.md]] -- Docstrings and metadata tags
