# Spelling of values built from the wire

A C++ or Rust pool that receives a value at a packed argument builds the
native value from its wire form, and spells the native type from the serial
form's head and carried arguments. Those can describe another type. Every
case below fails at build time; reproductions are in
/work/plans/nominal-types/tests (spec-gen-9-7-pending, adv1, wire-spelling).
Fix: spell a native type from its key (NOM-1, NOM-3), never from a serial
form (nominal-types stage B2b).

- **A back-reference under a renamed serial head names no enclosing
  declaration, or the wrong one.** [reproduced, 2026-11-17] Deviates from
  NOM-6; UNSOUND in one shape. Serial.hs setSerialHead renames a container
  head to a newtype with a form of its own, but the body's back-references
  still name the parent. `newtype Forest = Tree` (`type Py => Forest =
  "dict"`, Tree recursive) gets `<dict>m23valj4kidsa^4Tree`: the runtime
  refuses the dangling `^Tree` (schema.rs), and a C++ pool aborts at schema
  registration, taking every command of the pool with it. The same for
  `newtype Bush = Rose Int`, `[Forest]`, a Forest field, and a renamed
  back-reference (`newtype Grove = Node` inside `data Node`: `^5Grove`).
  `Rose Bush` gets `&4Rose..v14Rose2<dict>v14Rose2ja^4Rosea^4Rose`: the
  inner `^Rose` binds to the outer `Rose Bush`, so a well-typed value is
  refused and an ill-typed one is accepted and passed on. Nothing checks
  that an emitted `^name` resolves. Programs and analysis:
  /work/plans/nominal-types/scouts/bugs/4-renamed-head.md.
- **An applied alias whose arguments are all kinds is serialized as a bare
  name.** [read, 2026-11-16] Serial.hs makeSerialAST' sends it to the
  bare-name branch, which matches back-references by name only, so nested
  instances at different `Nat` arguments can be conflated.

