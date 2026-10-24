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

- **An opaque type crossing a boundary is rejected with an internal
  message.** [reproduced, 2026-10-21] NEWT-5's rejection is right but the
  diagnostic is "Cannot find constructor in VarF "int" finalType=Box":
  `type Box` with Py and Cpp forms, Py `mk :: Int -> Box`, Cpp
  `unbox :: Box -> Int`, `go x = unbox (mk x)`. Returning `Box` to the
  nexus gives the same message located at the export list. It should say
  `Box` has no wire form and cannot cross from py to cpp. Repro:
  /work/plans/model-audit-2026-10-21/repro/B/t/n5/a.loc, b.loc.

- **An opaque value bound in a do-block split across pools does not
  build.** [reproduced, 2026-10-24, also before the boundary pricing of
  e8aa73b0] With `type Box` / `type Cpp => Box = "Box"`, sourced
  `mk :: Int -> <IO> Box`, `grow :: Box -> Int -> <IO> Box`,
  `unbox :: Box -> Int` in C++ and `pyStep :: Int -> <IO> Int` in Python,

      run x = do
        b <- mk x
        y <- pyStep 1
        z <- pyStep y
        w <- pyStep z
        b2 <- grow b w
        u <- pyStep w
        v <- pyStep u
        unbox b2 + v

  fails `Could not resolve type for variable 'Box'`: the realizer places
  the statements after `b <-` in Python, so `b` must cross into the Python
  pool and back, and it has no wire form. Nothing in the realizer refuses
  a boundary that carries a free variable whose type cannot cross
  (COST-11 asks for placements code generation can build; REAL-3).
  Since references to a let-bound data value became free, nothing in the
  cost discourages such a split either.
  Repro files: /work/plans/realization-cost-model/repro2.
