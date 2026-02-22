# Morloc Language Specification

This document is the master table of contents for the Morloc formal specification. It describes the language, its type system, its runtime behavior, and its compiler architecture.

## Conventions

- Syntax examples use `morloc` code blocks
- Type rules use standard inference notation
- Cross-references use `[[file]]` links
- This spec describes *what* the language does, not how the compiler implements it

## Table of Contents

### Language Reference

Surface syntax and semantics of morloc programs.

- [[language/LANGUAGE.md]] -- Overview: design philosophy and core concepts
- [[language/lexical-structure.md]] -- Tokens, literals, comments, whitespace, indentation
- [[language/expressions.md]] -- Application, lambda, composition, where-clauses, accessors
- [[language/declarations.md]] -- Functions, type aliases, records, typeclasses, instances
- [[language/operators.md]] -- Fixity declarations, precedence, and associativity
- [[language/annotations.md]] -- Docstrings and CLI argument metadata tags

### Type System

The formal type system underlying morloc's multi-language guarantees.

- [[types/TYPES.md]] -- Overview: two-level design and role in cross-language safety
- [[types/primitive-types.md]] -- Built-in types and sized variants
- [[types/type-declarations.md]] -- Type aliases, language-specific mappings, terminal types
- [[types/records.md]] -- Record, object, and table types
- [[types/typeclasses.md]] -- Typeclass declarations, instances, and constraint resolution
- [[types/polymorphism.md]] -- Parametric polymorphism and quantification
- [[types/subtyping.md]] -- Subtyping rules and instantiation
- [[types/inference.md]] -- Bidirectional type inference

### Module System

How morloc code is organized, shared, and resolved.

- [[modules/MODULES.md]] -- Overview: namespaces, planes, and the dependency DAG
- [[modules/imports-and-exports.md]] -- Module declarations, imports, exports, visibility
- [[modules/resolution.md]] -- Path resolution: local, system, and plane lookups
- [[modules/packages.md]] -- Package metadata, versioning, and installation

### Cross-Language Interoperability

Mechanisms enabling function composition across Python, C++, and R.

- [[interop/INTEROP.md]] -- Overview: unifying multiple languages under one type system
- [[interop/foreign-functions.md]] -- Source declarations, function binding, renaming
- [[interop/type-mappings.md]] -- General-to-concrete type resolution per language
- [[interop/serialization.md]] -- Msgpack protocol, schema encoding, packing rules
- [[interop/implementation-selection.md]] -- Realization algorithm and language selection

### Runtime System

How compiled morloc programs execute.

- [[runtime/RUNTIME.md]] -- Overview: the nexus-pool model
- [[runtime/execution-model.md]] -- Nexus lifecycle, pool management, dispatch, errors
- [[runtime/ipc.md]] -- Unix socket protocol, message format, data flow
- [[runtime/manifest.md]] -- JSON manifest schema
- [[runtime/cli.md]] -- Automatic CLI generation from type signatures

### Compiler Architecture

The compilation pipeline from source to executable.

- [[compiler/COMPILER.md]] -- Overview: stages, IRs, and design principles
- [[compiler/pipeline.md]] -- End-to-end compilation flow
- [[compiler/parsing.md]] -- Lexing, tokenization, expression parsing
- [[compiler/typechecking.md]] -- Bidirectional inference, subtyping, context threading
- [[compiler/codegen.md]] -- Two-phase lower-then-print model and per-language translators
- [[compiler/build.md]] -- Manifest writing, pool generation, compilation, deployment
