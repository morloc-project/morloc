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
- **A native form with no type list keeps its `$N` in generated code.**
  [reproduced 2026-11-14] Violates NATIVE-1. Restructure.hs checkFormLists
  checks `$N` only for a form written with a list (`formListProblem _ _ =
  Nothing`, Restructure.hs:1280), and Macro.hs:34 leaves an unfilled `$N`
  as text. `type Foo` / `type Cpp => Foo = "X<$1>"` with sourced
  `mk :: Int -> Foo`, `unbox :: Foo -> Int` passes the compiler, and g++
  fails "'$1' was not declared in this scope". It should be refused at the
  form.
- **A program reaches the catch-all of `toNexusExpr`.** [reproduced,
  2026-11-06] `never :: <IO> Int -> Int -> <IO> Int; never t y = do y` with
  `a2 k = never (runIt (mkio k)) k`, `runIt` and `mkio` sourced, makes
  `morloc make` die with "Unreachable value of type reached" (Nexus.hs). The
  node that reaches it is not identified.
