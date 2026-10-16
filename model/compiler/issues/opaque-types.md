# Opaque sourced types

A type declared with no morloc definition and only per-language forms
(`type Box` with `type Cpp => Box = "Box"`), which morloc cannot
serialize unless a `Packable` instance gives it a wire form.

- [reproduced] A recursive function over an opaque type does not build.
  Spec gap: no item says where an opaque type may appear, so it is
  undecided whether this program is legal. Repro (C++; Rust is the same):

      type Box
      type Cpp => Box = "Box"
      source Cpp from "o.hpp" ("mk", "grow", "unbox")
      mk :: Int -> Box
      grow :: Box -> Box
      unbox :: Box -> Int
      up :: Int -> Box
      up n
        ? n == 0 = mk 0
        : grow (up (n - 1))
      deep :: Int -> Int
      deep n = unbox (up n)

  with `o.hpp` defining `struct Box { int v; }` and the three functions.
  `morloc make` fails at the call `up n` with `Cannot find constructor in
  VarF "Box"`, raised where a wire form is built
  (library/Morloc/CodeGenerator/Serial.hs, the `AliasIsNone` arm of
  makeSerialAST' finding no Packable instance). The same calls without the
  recursion build and run (`unbox (grow (mk 3))` gives 4). [speculative]
  The recursion's native entry keeps a serialized wrapper (the #90 fix),
  which asks for a wire form of its argument and result types even when
  no value crosses a pool.
- Workaround, verified: `newtype Box = Int` with
  `instance Packable Int Box` (golden:spec-emit-3-1 uses it for its
  copy-counting type). Open question for a ruling: should a wire form be
  required only where a value crosses a pool or reaches the nexus, with a
  located error naming the type there, or is an opaque type allowed only
  with a Packable instance?
