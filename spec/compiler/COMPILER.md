# Compiler Architecture

The morloc compiler transforms `.loc` source files into executable programs consisting of a nexus binary, a JSON manifest, and language-specific pool files. The compilation pipeline is structured as a sequence of transformations over progressively lower-level intermediate representations.

## Design Principles

- **Separation of concerns.** Parsing, type checking, realization, code generation, and building are distinct phases with well-defined interfaces.
- **Language-parametric code generation.** A shared imperative IR and a configurable lowering engine allow new target languages to be added by supplying a configuration record and a printer, without modifying the core pipeline.
- **Compile-time language selection.** The compiler resolves which language implements each function before any code is generated. There is no runtime dispatch logic for language selection.

## Intermediate Representations

| IR | Phase | Description |
|----|-------|-------------|
| `ExprI` | Parsing | Indexed expression AST with unique IDs per node |
| `AnnoS TypeU` | Type checking | Annotated syntax tree with general types |
| `AnnoS Type Lang` | Realization | Annotated tree with concrete types and language tags |
| `PolyExpr` | Parameterize/Express | Polymorphic manifold tree |
| `MonoExpr` | Segment | Monomorphic per-language tree with boundary markers |
| `SerialExpr` | Serialize | Tree with serialization/deserialization operations |
| `IStmt`/`IExpr` | Lower | Shared imperative IR |
| `MDoc` | Print | Rendered source text |

## Subfiles

- [[pipeline.md]] -- End-to-end compilation flow
- [[parsing.md]] -- Lexing, tokenization, expression parsing
- [[typechecking.md]] -- Bidirectional inference, subtyping, context threading
- [[codegen.md]] -- Lower-then-print model and per-language translators
- [[build.md]] -- Manifest writing, pool generation, compilation, deployment
