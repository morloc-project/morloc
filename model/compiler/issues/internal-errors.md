# Internal errors a user program reaches

Each violates DIAG-1: the input is a mistake (or a legal program), and the
compiler answers with an internal error, a raw `error` or `undefined`
instead of a located diagnostic. Type-level fixes for the classes these
belong to are in /work/plans/gadt/type-audit.md.

- **An over-applied intrinsic is an internal error.** [reproduced,
  2026-10-29] `f :: Int -> Int; f x = @try x 1` fails "internal compiler
  error" at Typecheck.hs synthE (`IntrTry expects 1 arg`). Desugar.hs
  etaExpandIntrinsic keeps every argument when `actual >= arity`, so the
  extra one reaches the per-intrinsic clause. `@write` with four arguments
  takes the same path to a raw `error` (`IntrWrite expects 3 args`), with no
  location [reproduced 2026-11-02].
  `@stream`/`@next` report a type error and `@eq` goes through
  checkIntrinsicArgs, so only some intrinsics are affected. Fix: desugar an
  over-application as an application of the saturated intrinsic to the
  rest. Repro: /work/plans/gadt/repro/try.loc.
- **A declaration overlapping a typeclass term hits `undefined`.** [read]
  Link.hs linkLocal, the `(Just (_, Just _))` arm of the AssE case. The SrcE
  case reports the same overlap properly. Two shapes (top level, where
  block) did not reach it; the triggering shape is unknown.
- **Alternatives of a term that differ in lambda shape.** [read]
  Valuecheck.hs raises raw `error` "Illegal empty lambda" when one
  alternative is a lambda and another is not (e.g. `\x -> f x` vs `f`).
  Not run.
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
