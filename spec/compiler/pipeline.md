# Compilation Pipeline

The morloc compiler transforms source files into an executable through seven major phases. Each phase produces an intermediate representation consumed by the next.

## Phase Summary

```
.loc files
  |  1. Parse
  v
ExprI (indexed AST) + DAG (module dependencies)
  |  2. Link & Merge
  v
Unified compiler state (signatures, types, sources)
  |  3. Restructure & Treeify
  v
AnnoS TypeU Many Int (annotated syntax tree, general types)
  |  4. Typecheck
  v
AnnoS Type One Lang (concrete types, language tags)
  |  5. Realize
  v
Validated program (all implementations resolved)
  |  6. Generate
  v
JSON manifest + language-specific pool source code
  |  7. Build
  v
Executable (nexus binary + manifest + pool files)
```

## 1. Parse

The parser reads `.loc` files and produces an indexed expression AST (`ExprI`). Each subexpression receives a unique integer ID. The parser handles indentation-sensitive syntax, operator precedence (via precedence climbing), and module structure.

Parsing is recursive over the module DAG: when an import is encountered, the imported module is located, parsed, and added to the dependency graph. Parser state (fixity tables, expression counters) accumulates across modules.

## 2. Link and Merge

**Link** transfers terms, types, typeclasses, and source declarations from the parsed DAG into the compiler's global state. Each module's exports are filtered according to its export list and the importing module's import list.

**Merge** resolves cases where the same function name is imported from multiple modules with different language implementations. If the general types match, the implementations are combined (implementation polymorphism). If the general types conflict, an error is reported.

## 3. Restructure and Treeify

**Restructure** expands type aliases, resolving general type names to their definitions.

**Treeify** converts the flat expression list into an annotated syntax tree (`AnnoS`), threading type annotations and language information through the tree structure.

## 4. Typecheck

The typechecker applies bidirectional type inference to the annotated syntax tree. It:

- Infers types for unannotated expressions (synthesis).
- Verifies annotated expressions match their declared types (checking).
- Resolves existential type variables.
- Checks subtyping at polymorphic boundaries.
- Eta-expands lambdas that return function types.

The output is a fully typed tree with concrete types and language annotations.

See [[typechecking.md]] for details.

## 5. Realize

Realization validates that every function in the typed program has at least one foreign implementation, selects language implementations to minimize serialization boundaries, and checks that all required concrete type mappings exist.

See [[../interop/implementation-selection.md]].

## 6. Generate

Code generation transforms the realized program into:

- A JSON manifest describing commands, pools, and argument schemas.
- Source code for each language pool.

This phase proceeds through several sub-phases:

1. **Parameterize**: thread function arguments through expression trees.
2. **Express**: create polymorphic manifold trees.
3. **Segment**: split trees at language boundaries into monomorphic per-language segments.
4. **Serialize**: insert msgpack serialization/deserialization operations.
5. **Emit**: translate each segment to target language source code via the shared imperative IR.

See [[codegen.md]] for details.

## 7. Build

The build phase writes generated files to disk:

- Copies the pre-compiled nexus binary.
- Writes the JSON manifest.
- Writes pool source files.
- Compiles C++ pools with the system C++ compiler.
- Sets executable permissions on interpreted pools.

See [[build.md]] for details.
