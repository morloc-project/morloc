# Code Generation

Code generation transforms the type-checked, realized program into a JSON manifest and language-specific pool source files. It uses a two-phase architecture -- lower to a shared imperative IR, then print to target language syntax -- to maximize code reuse across languages.

## Sub-Phases

Code generation proceeds through five sub-phases:

### 1. Parameterize

Thread function arguments through expression trees. Each manifold (a callable unit in the generated code) receives its argument list explicitly. Unused arguments are removed.

### 2. Express

Create polymorphic manifold trees that represent the program's call structure. Each manifold captures a function, its arguments, and its body. Lambdas, lists, tuples, and records are normalized into a uniform manifold representation.

### 3. Segment

Break polymorphic trees at language boundaries. Each manifold is assigned to a specific language. Where a manifold in language A calls a function in language B, a boundary marker is inserted. The result is a set of monomorphic, per-language manifold trees.

Manifold forms after segmentation:

| Form | Description |
|------|-------------|
| LocalRoot | Entry point callable from the nexus |
| LocalForeign | Calls a function in another pool |
| RemoteWorker | Called by another pool via the nexus |

### 4. Serialize

Insert serialization and deserialization operations at every language boundary. For each cross-language call, the compiler:

1. Determines the msgpack schema for each argument and the return value.
2. Wraps the caller's output in a serialize call.
3. Wraps the callee's input in a deserialize call.

The result is a tree where every value crossing a boundary is explicitly packed/unpacked.

### 5. Emit

Translate each serialized manifold tree into target language source code. This phase uses the two-phase lower-then-print architecture described below.

## Two-Phase Architecture

### Lowering

The lowering phase converts the compiler's manifold AST into a shared imperative IR (`IStmt` / `IExpr`). The IR supports:

- Variable assignment
- Function calls (local and remote)
- Serialize/deserialize calls
- List/map iteration
- Lambda expressions
- Return statements

Lowering is parameterized by a configuration record (`LowerConfig`) with approximately 30 fields covering:

| Category | Examples |
|----------|---------|
| Type rendering | How to render type names, template parameters |
| Accessors | Record field access, tuple indexing syntax |
| Constructors | List, tuple, record literal syntax |
| Calls | Local function calls, remote pool calls |
| Serialization | Pack/unpack function names and calling conventions |
| Statements | Let binding, return, function definition syntax |

A single generic fold (`defaultFoldRules`) walks the AST and produces the IR using the language's `LowerConfig`. This fold is written once and shared by all languages.

### Printing

The printing phase converts the imperative IR into rendered source text. Each language provides:

- `printExpr`: render an IR expression as source text.
- `printStmt`: render an IR statement as source text.
- `printProgram`: assemble a complete pool source file from rendered fragments.

## Per-Language Specifics

### C++

- **Stateful**: uses a monadic translator that tracks generated struct types, forward declarations, and serializer functions.
- **Type-aware**: `LowerConfig` type fields produce concrete C++ type strings (used for variable declarations, template parameters).
- **Struct generation**: anonymous record types produce `struct` definitions and corresponding serializer/deserializer functions.

### Python

- **Stateless**: uses a pure translator with only a counter for unique names.
- **No types**: `LowerConfig` type fields return nothing (Python is dynamically typed).
- **Import handling**: source file paths are converted to Python import statements.

### R

- **Stateless**: similar to Python.
- **R conventions**: 1-indexed access, `c()` for vectors, `$` for field access, `list()` for records.

## Pool Template Assembly

Each language has a pool template with `<<<BREAK>>>` markers. The code generator splits the template at these markers and interleaves generated code:

```
[template header]
[generated imports]
[template middle]
[generated function definitions]
[template dispatch section]
[generated dispatch table]
[template footer]
```

The result is a complete, self-contained pool source file.

## Manifest Generation

In parallel with pool generation, the compiler produces a JSON manifest describing:

- Pool definitions (language, executable command, socket name)
- Command definitions (subcommand name, manifold ID, pool index, schemas)
- Argument metadata (kind, metavar, type, default, description)
- Pure expression trees (for commands that need no pool)

See [[../runtime/manifest.md]].
