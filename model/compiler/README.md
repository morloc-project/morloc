# Compiler spec

The compiler's internal contracts, one file per pass boundary: what a pass
may assume of its input and must guarantee of its output. Every guarantee
here serves a rule in `../language/`, and cites it.

Unlike the language spec, this spec may name passes and intermediate forms,
since they are its subject. It still never names a source file or function:
an item must stay true if the pass is rewritten.

A boundary item fails loudly. When a pass receives input that breaks an
assumption, the compiler stops with an internal error naming the item; it
never repairs the input or guesses.

## Layout

- `frontend/`: from source text to a typechecked program with general types.
- `codegen/`: from implementations chosen per call to pools and a manifest.

## Planned files

| File | Prefix | Boundary |
|---|---|---|
| frontend/module-graph.md | MGRAPH | parsed modules and the import graph |
| frontend/names.md | NAME | every name resolved to one declaration |
| frontend/typecheck.md | TC | general types: what the typechecker accepts and produces |
| codegen/realize.md | RLZ | one implementation and one language per call |
| codegen/effect-boundaries.md | EBND | suspensions made explicit at every language boundary |
| codegen/serialize.md | SER | every crossing has a wire form |
| codegen/emit.md | EMIT | generated pools and the manifest |

Only `frontend/typecheck.md` and `codegen/effect-boundaries.md` are started.
