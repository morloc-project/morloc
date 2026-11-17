# Spelling of values built from the wire

A C++ or Rust pool builds a value from its wire form, and spells or
marshals its native type from the serial form's head and carried
arguments. No case below gives a wrong value; each fails to build or fails
the call.

- **C++ has no marshaller for a newtype's form over a record.**
  [reproduced, 2026-11-17] Violates NATIVE-1. `newtype Forest = Tree` with
  `type Cpp => Forest = "forest_t"` (Tree recursive, so a Forest crosses as
  itself) builds, but a call fails at run time with "no marshaller for a
  value of this C++ type": Cpp.hs generateSourcedSerializers makes
  marshallers only for scope entries whose body is a record, and Forest's
  is its form's name. Probe p4 in /work/plans/nominal-types/scouts/bugs/4-renamed-head.md.
- **An applied alias whose arguments are all kinds is serialized as a bare
  name.** [read, 2026-11-16] Serial.hs makeSerialAST' sends it to the
  bare-name branch, which matches back-references by name only, so nested
  instances at different `Nat` arguments can be conflated.

