# Compiler state

- **withModuleLock drops state changes when its action throws an IO
  exception.** [read, 2026-10-28] Gap: no item says what state survives a
  caught exception. library/Morloc/Module.hs `withModuleLock` runs the action
  as plain IO and does `put` / `tell` only on a normal return, so a caller
  that catches an IOException continues with the state from before the
  action: indices handed out inside it are handed out again, and package
  metadata (`appendMeta`) and logs recorded inside it are lost.
- **Nothing bounds the compiler's heap.** [gap, 2026-10-28] The compiler
  monad held every earlier MorlocState through lazy fields, a lazy StateT and
  a lazy WriterT: 1713 MiB in use to build
  /work/tests/sumtypes/02-phylogenetics-pruning/core main-rust.loc, 203 MiB
  once strict. No test or benchmark checks residency (`+RTS -s` maximum
  residency on a known program), so a lazy `modify` or layer can bring it
  back unnoticed.
