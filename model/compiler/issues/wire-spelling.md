# Spelling of values built from the wire

A C++ or Rust pool that receives a value at a packed argument builds the
native value from its wire form, and spells the native type from the serial
form's head and carried arguments. Those can describe another type. Every
case below fails at build time; reproductions are in
/work/plans/nominal-types/tests (spec-gen-9-7-pending, adv1, wire-spelling).
Fix: spell a native type from its key (NOM-1, NOM-3), never from a serial
form (nominal-types stage B2b).

- **A user-mapped template record built from the wire drops its arguments.**
  [reproduced, 2026-11-16, also at 63042ce29] Violates GEN-9. With `record
  Cpp => Pair a = "pair_t<$1>" a` and a packed `Tenths`, `readP :: Pair Tenths
  -> Int` emits `pair_t<$1>` (C++ g++ error, Rust parse error): the record
  is typed from its serial form with an empty argument list
  (Grammars/Translator/Imperative.hs wireToNative). Passing the carried
  arguments instead breaks the cases below.
- **A renamed serial head keeps its parent's arguments, and can leave
  back-references dangling.** [read, 2026-11-16] Deviates from NOM-6. Where
  a type with a form of its own over another is serialized (`newtype Point
  = Pair Tenths`, `type Cpp => Point = "point_t"`), Serial.hs setSerialHead
  renames the head and keeps the parent's arguments, so containers of it
  are spelled `point_t<double>` (`[Point]`, `?Point`: g++ "'point_t' is not
  a template"), and a newtype with a template form of its own is spelled at
  its parent's arguments (`newtype W a = Pair (a, a)`, `"w_t<$1>" a`:
  `w_t<std::tuple<double,double>>`). A back-reference inside a renamed head
  (`^Rose` under a `Forest` head) or a renamed back-reference (`^Tree2` for
  `newtype Tree2 = Tree`) names no enclosing declaration.
- **An applied alias whose arguments are all kinds is serialized as a bare
  name.** [read, 2026-11-16] Serial.hs makeSerialAST' sends it to the
  bare-name branch, which matches back-references by name only, so nested
  instances at different `Nat` arguments can be conflated.

- **Rust writes an unpacked user record at its native argument.**
  [reproduced, 2026-11-17] Deviates from NOM-4. A user-mapped `UT a` holding a
  generated `G2 a` at a packed `a`, returned to the wire: the helper is
  `let __mm_helper0: UT<f64> = UT { x: unpackTenths(..), y: __mm_n.y }`
  (unpacked value, native spelling) and the nested `G2` is copied without
  unpacking its field; rustc refuses. C++ is correct.
  /work/plans/nominal-types/scouts/adversarial/c1-user-gen-packed/out-rust.loc.
- **Rust spells an unpacked user record with no parameters as its native
  struct.** [reproduced, 2026-11-17] Deviates from NOM-4. A user-mapped `H2`
  (no parameters) holding a packed `Date`: the unpacked value is spelled
  `H2T`, so the pack/unpack calls are type errors, and `DateT` lacks the
  serialization traits. C++ is correct. /work/plans/nominal-types/scouts/adversarial/c6-data-packed-arm/ctl/h2-rust.loc.
