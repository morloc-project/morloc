# Spelling of values built from the wire

A C++ or Rust pool builds a value from its wire form, and spells or
marshals its native type from the serial form's head and carried
arguments. No case below gives a wrong value; each fails to build or fails
the call.

- **An applied alias whose arguments are all kinds is serialized as a bare
  name.** [read, 2026-11-16] Serial.hs makeSerialAST' sends it to the
  bare-name branch, which matches back-references by name only, so nested
  instances at different `Nat` arguments can be conflated.

