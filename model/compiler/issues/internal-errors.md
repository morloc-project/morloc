# Internal errors a user program reaches

Each violates DIAG-1: the input is a mistake (or a legal program), and the
compiler answers with an internal error, a raw `error` or `undefined`
instead of a located diagnostic. Type-level fixes for the classes these
belong to are in /work/plans/gadt/type-audit.md.

- **A missing native mapping crashes through raw `error`.** [read] C++
  `leakError` (Cpp.hs, VarF/AppF/RecF guards), "record missing from recmap"
  (Cpp.hs, Rust.hs), "Recursive record without explicit mapping" (Cpp.hs,
  Rust.hs), "has no concrete mapping" (Rust.hs). A program that lacks a
  mapping gets an unlocated crash instead of a diagnostic.
- **User-writable field types and names crash C++/Rust printers.** [read]
  Cpp.hs showDefType raises raw `error` on a FunT/AppT/EffectT field type;
  RustPrinter.hs raises raw `error` on a reserved Rust field name. Both are
  things a user can write and need a located rejection.
- **Native-form templates with `$0` or no arguments.** [read]
  Grammars/Macro.hs expandMacro: `$0` indexes `xs !! (-1)` and crashes; with
  an empty argument list (every argument kind-filtered or dropped) the
  template is returned with its `$N` left in the generated code. Fix:
  parse templates once at declaration and check their arity there.
