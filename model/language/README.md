# Morloc language spec

This spec states what morloc programs mean: which programs the compiler
accepts, what type each term has, what each term computes, and what a value
looks like in each language it reaches. It is the statement of intent. The
code is measured against it, not the reverse.

Items, tests and the check follow the shared format in `../README.md`.
Language items name no compiler module, pass or data structure. If an item
can only be stated by pointing at code, the design behind it has not been
decided yet, and the item is `open`.

## Notation

ASCII only.

    G |- e : T          in context G, term e has type T
    T <: U              a value of type T may be used where U is expected
    T == U              T and U are the same type
    <E> T               a suspension that, forced, may perform the effects in E and yields T
    [[T]]_L             the representation of a T value in language L
    e ~> v              e evaluates to v

Inference rules put premises above a line of dashes and the conclusion below.

## Chapters

Every chapter is started; most hold a handful of seed items. A chapter may
split later.

### Surface syntax

| File | Prefix | Scope |
|---|---|---|
| [lexical.md](lexical.md) | LEX | characters, tokens, comments, indentation and layout, identifier classes |
| [literals.md](literals.md) | LIT | syntax of numeric, string, boolean, list, tuple and record literals |
| [operators.md](operators.md) | OP | infix operators: declaration, fixity, precedence, sections, import |
| [bindings.md](bindings.md) | BIND | top-level and local definitions, `where`, `let`, lambdas, `@` binders, shadowing |
| [patterns.md](patterns.md) | PAT | destructuring, `match`, guards, pattern chains, exhaustiveness |
| [docstrings.md](docstrings.md) | DOC | `--'` comments: what they attach to and which directives they carry |

### Modules

| File | Prefix | Scope |
|---|---|---|
| [module-names.md](module-names.md) | MOD | modules, imports, exports, and which declaration a name refers to across modules |

### Types

| File | Prefix | Scope |
|---|---|---|
| [kinds.md](kinds.md) | KIND | kinds of types (types, naturals, strings, constructors) and kind checking |
| [aliases.md](aliases.md) | ALIAS | `type` aliases: parameters, transparency, expansion |
| [newtypes.md](newtypes.md) | NEWT | `newtype`: a distinct type over a representation, and conversion to and from it |
| [native-types.md](native-types.md) | NATIVE | per-language type mappings (`type Py => X = ...`, `record Cpp => R`) and their agreement |
| [type-equality.md](type-equality.md) | TEQ | when two types are the same type |
| [base-types.md](base-types.md) | BASE | `Unit`, `Bool`, `Str`: value spaces and operations |
| [numbers.md](numbers.md) | NUM | `Int`, fixed-width integers, floats: value spaces, defaulting, arithmetic across languages |
| [collections.md](collections.md) | COLL | lists, tuples and maps: construction, access, slicing |
| [records.md](records.md) | REC | record declarations, literals, field access and update |
| [sum-types.md](sum-types.md) | SUM | `data` declarations, constructors, recursive and parameterized data |
| [optionals.md](optionals.md) | OPT | `?T`: missing values, idempotence, native null |
| [dimensions.md](dimensions.md) | DIM | Nat-indexed types (vectors, tensors), dimension arithmetic, runtime checks |
| [tables.md](tables.md) | TAB | `Table` types: schemas, column operations, nesting |

### Typing

| File | Prefix | Scope |
|---|---|---|
| [signatures.md](signatures.md) | SIG | what a signature declares: one term with many implementations, terms without signatures, generic terms |
| [polymorphism.md](polymorphism.md) | POLY | quantifiers, instantiation, generalization, scope of type variables |
| [subtyping.md](subtyping.md) | SUB | the `<:` relation and where it is applied |
| [application.md](application.md) | APP | application, partial application, arity, arrow grouping |
| [typeclasses.md](typeclasses.md) | CLS | classes, instances, constraints, instance selection, coherence |
| [effects.md](effects.md) | EFF | suspensions: their types, `do`, forcing, effect rows |
| [failure.md](failure.md) | FAIL | `Try`, `@try`, `@throw`: which failures are values and which can be caught |

### Evaluation

| File | Prefix | Scope |
|---|---|---|
| [evaluation.md](evaluation.md) | EVAL | evaluation order, strictness, sharing of named values, what may be reordered |
| [recursion.md](recursion.md) | RECUR | tail calls, recursion depth, recursion across languages |
| [intrinsics.md](intrinsics.md) | INTR | the `@` intrinsics (`@load`, `@read`, `@stdin`, ...): their types and meaning |
| [streams.md](streams.md) | STRM | stream types, file-backed streams, `@collect`, `@fold` |
| [annotations.md](annotations.md) | ANN | caching, logging, benchmark labels, unrolling: what instrumentation may change and what it may not |

### Realization

| File | Prefix | Scope |
|---|---|---|
| [sources.md](sources.md) | SRC | `source` declarations: files, names, signatures, the claims a source makes |
| [realization.md](realization.md) | REAL | choosing an implementation and a language for each call; implementations are interchangeable |
| [host-calls.md](host-calls.md) | HOST | how morloc and sourced code call each other: arguments, callbacks, suspensions |
| [data-crossing.md](data-crossing.md) | WIRE | how each type's values cross between languages: wire form, round trip, copying and sharing |
| [guest-languages.md](guest-languages.md) | GUEST | languages compiled into another language's pool |

### Programs

| File | Prefix | Scope |
|---|---|---|
| [commands.md](commands.md) | CMD | which exports become commands, and their names |
| [arguments-and-output.md](arguments-and-output.md) | CLI | argument shapes, parsing, stdin, `@parse`, `@render`, output rendering |
| [diagnostics.md](diagnostics.md) | DIAG | the compiler's error contract: never crash, every rejection located, warnings |

Runtime processes, threads, locks and shared memory are `../runtime/`'s.

## Complete chapters

A chapter listed here is held to the full standard by `../check.sh`: every
`ruled` item cites a test and conforms. Elsewhere those gaps are warnings.

