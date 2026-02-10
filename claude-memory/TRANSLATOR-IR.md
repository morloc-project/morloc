# Translator IR Architecture

## Overview

The translator subsystem converts the compiler's `SerialManifold`/`NativeExpr` AST into language-specific pool source code. It uses a shared imperative IR and a two-phase architecture (lower to IR, then print) to maximize code reuse across languages.

## File Layout

```
Grammars/
  Common.hs          -- Shared: PoolDocs, naming, records, dispatch, AST inversion
  Translator/
    Imperative.hs    -- Shared IR types + lowering engine
    Cpp.hs           -- C++ translator (monadic, ~592 lines)
    Python3.hs       -- Python translator (pure, ~223 lines)
    R.hs             -- R translator (pure, ~165 lines)
    PseudoCode.hs    -- Debug pseudocode renderer
    Printer/
      Cpp.hs         -- C++ syntax: printExpr, printStmt, printProgram
      Python3.hs     -- Python syntax: printExpr, printStmt, printProgram
      R.hs           -- R syntax: printExpr, printStmt, printProgram
```

## Translation Pipeline

Every language follows this pipeline:

```
[SerialManifold]                     -- input: list of manifold trees
  |
  v
preprocess (invertSerialManifold)    -- Common.hs: flatten let-bindings
  |
  v
fold with defaultFoldRules(cfg)      -- Imperative.hs: shared fold over AST
  |                                     cfg = language-specific LowerConfig
  v
[PoolDocs]                           -- intermediate: rendered MDoc fragments
  |
  v
buildProgram / buildProgramM         -- Imperative.hs: assemble IProgram
  |                                     extracts dispatch from [SerialManifold]
  v
IProgram                             -- uniform record: sources, manifolds, dispatch
  |
  v
Printer.printProgram                 -- per-language: fill pool template
  |
  v
MDoc -> Script                       -- final rendered source code
```

## Key Types

### PoolDocs (Common.hs)

Accumulator used during the fold. Each node produces a `PoolDocs`:

```haskell
data PoolDocs = PoolDocs
  { poolCompleteManifolds :: [MDoc]  -- fully rendered function definitions
  , poolExpr :: MDoc                 -- the inline expression for this node
  , poolPriorLines :: [MDoc]         -- statements preceding the expression
  , poolPriorExprs :: [MDoc]         -- expressions preceding this manifold
  }
```

`renderPoolDocs` combines `poolPriorExprs` and `poolCompleteManifolds` into the final output for one manifold tree.

### IProgram (Imperative.hs)

Uniform representation of a complete pool program:

```haskell
data IProgram = IProgram
  { ipSources        :: [MDoc]           -- rendered include/import statements
  , ipManifolds      :: [MDoc]           -- rendered manifold function definitions
  , ipLocalDispatch  :: [DispatchEntry]  -- local dispatch table entries
  , ipRemoteDispatch :: [DispatchEntry]  -- remote dispatch table entries
  }
```

Built by `buildProgram` (pure, for Python/R) or `buildProgramM` (monadic, for C++). Both extract dispatch from the original `[SerialManifold]` via `extractLocalDispatch`/`extractRemoteDispatch`.

### IStmt / IExpr (Imperative.hs)

Imperative IR used as an intermediate between the compiler AST and rendered MDoc. Used for serialize/deserialize expansion:

- `IStmt`: `IAssign`, `IMapList`, `IReturn`, `IExprStmt`, `IFunDef`
- `IExpr`: `IVar`, `ICall`, literals, `ISerCall`/`IDesCall`, `IAccess`, `ILambda`, `IPack`, `IRawExpr`
- `IType`: wrapper around `MDoc` for typed contexts (C++ needs types; Python/R pass `Nothing`)

The IR is lowered to `MDoc` by per-language `printExpr`/`printStmt` functions.

### LowerConfig (Imperative.hs)

~30-field record parameterizing all language-specific behavior. Each language provides one `LowerConfig`:

| Category | Fields | Purpose |
|----------|--------|---------|
| Naming | `lcSrcName` | Source function name rendering |
| Types | `lcTypeOf`, `lcSerialAstType`, `lcDeserialAstType`, `lcRawDeserialAstType`, `lcTemplateArgs`, `lcTypeMOf` | Type rendering (C++ returns `Just`; Python/R return `Nothing`) |
| Accessors | `lcRecordAccessor`, `lcDeserialRecordAccessor`, `lcTupleAccessor` | Field/element access syntax |
| Constructors | `lcListConstructor`, `lcTupleConstructor`, `lcRecordConstructor` | Literal construction syntax |
| Calls | `lcForeignCall`, `lcRemoteCall` | Inter-pool call rendering |
| Packing | `lcPackerName`, `lcUnpackerName` | User-defined pack/unpack functions |
| Serialization | `lcSerialize`, `lcDeserialize` | Custom ser/deser logic (C++ overrides; Python/R use `defaultSerialize`/`defaultDeserialize`) |
| Statements | `lcMakeLet`, `lcReturn`, `lcPrintExpr`, `lcPrintStmt` | Statement-level syntax |
| Patterns | `lcEvalPattern` | String interpolation and destructuring patterns |
| Manifolds | `lcMakeFunction`, `lcMakeLambda` | Function definition and partial application syntax |
| State | `lcNewIndex` | Counter for generating unique variable names |

### DispatchEntry (Common.hs)

```haskell
data DispatchEntry = DispatchEntry
  { dispatchId :: Int         -- manifold ID
  , dispatchArgCount :: Int   -- number of arguments
  }
```

Extracted from `[SerialManifold]` by `extractLocalDispatch` (skips remote workers) and `extractRemoteDispatch` (walks AST for `RemoteCall` nodes).

## Per-Language Specifics

### C++ (Cpp.hs)

- **Monadic**: Uses `CppTranslator` (StateT over Identity) for type rendering, dedup sets, record map
- **State**: counter, recmap, signature/manifold dedup sets, current manifold index
- **Extra phases**: `makeSignature` (forward declarations), `generateAnonymousStructs` + `generateSourcedSerializers` (struct typedef + serializer generation)
- **Custom serialization**: Overrides `lcSerialize`/`lcDeserialize` (records become tuples for msgpack)
- **Uses `buildProgramM`** because `translateSegment` runs in `CppTranslator`

### Python3 (Python3.hs)

- **Pure**: Uses `IndexM` (StateT Int over Identity) for counters only
- **Source handling**: Converts paths to Python module import syntax
- **Uses `buildProgram`** (pure)

### R (R.hs)

- **Pure**: Uses `IndexM` for counters
- **R-specific**: 1-indexed access, `c()` vs `list()` for vectors, `$` accessor
- **No dispatch in pool template**: R pool template has no dispatch section (3 sections vs 4/5 for C++/Python)
- **Uses `buildProgram`** (pure)

## Shared Infrastructure in Common.hs

- **`invertSerialManifold`**: Flattens nested let-bindings into sequential assignments (preprocessing step)
- **Naming**: `svarNamer` (serial vars: s0, s1...), `nvarNamer` (native: n0, n1...), `helperNamer`, `argNamer`, `manNamer`
- **Record collection**: `collectRecords` walks AST for anonymous struct types; `unifyRecords` merges structurally equivalent records into parameterized templates (C++ only)
- **Dispatch extraction**: `extractLocalDispatch`/`extractRemoteDispatch` build dispatch tables from manifold headers
- **Pattern evaluation**: `patternSetter` handles structural pattern matching (tuple/record getters/setters)

## Adding a New Language

1. Create `Translator/NewLang.hs`: define a `LowerConfig`, call `defaultFoldRules` in `translateSegment`, call `buildProgram`
2. Create `Printer/NewLang.hs`: implement `printExpr`, `printStmt`, `printProgram`
3. Add pool template to `data/pools/`
4. Register in `Generate.hs`

For dynamically-typed languages (like Python/R), this is lightweight — most `LowerConfig` type fields return `Nothing`. For statically-typed languages (like C++), expect ~100 extra lines for type rendering.

---
*See also: [[CODEGEN.md]], [[RUNTIME.md]]*
