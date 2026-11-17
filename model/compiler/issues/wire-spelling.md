# Spelling of values built from the wire

A C++ or Rust pool that receives a value at a packed argument builds the
native value from its wire form, and spells the native type from the serial
form's head and carried arguments. Those can describe another type. Every
case below fails at build time; reproductions are in
/work/plans/nominal-types/tests (spec-gen-9-7-pending, adv1, wire-spelling).
Fix: spell a native type from its key (NOM-1, NOM-3), never from a serial
form (nominal-types stage B2b).

- **A back-reference under a renamed serial head names no enclosing
  declaration.** [read, 2026-11-16] Deviates from NOM-6. Where a type with a
  form of its own over another is serialized (`newtype Tree2 = Tree`),
  Serial.hs setSerialHead renames the head, so a back-reference inside it
  (`^Rose` under a `Forest` head) or a renamed back-reference (`^Tree2`)
  names no enclosing declaration.
- **An applied alias whose arguments are all kinds is serialized as a bare
  name.** [read, 2026-11-16] Serial.hs makeSerialAST' sends it to the
  bare-name branch, which matches back-references by name only, so nested
  instances at different `Nat` arguments can be conflated.

