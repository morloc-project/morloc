# Spelling of values built from the wire

A C++ or Rust pool builds a value from its wire form, and spells or
marshals its native type from the serial form's head and carried
arguments. No case below gives a wrong value; each fails to build or fails
the call.

- **A recursive alias whose parameters are all kinds does not build.**
  [reproduced, 2026-11-17] Violates ALIAS-7. `type R (n :: Nat) = (Int, [R
  n])` used at `R 3` fails with "Cannot find constructor in VarF "R"": the
  application, having no type arguments, reaches Serial.hs makeSerialAST'
  as a bare name, which the alias branch cannot expand without its
  argument. Nested instances at different arguments, which the bare name
  could conflate, cannot arise: ALIAS-4 refuses an alias that refers to
  itself at other arguments.
