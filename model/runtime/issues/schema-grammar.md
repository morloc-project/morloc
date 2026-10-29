# The schema grammar has stray definitions

The schema alphabet is written by the Haskell emitter (Serial.hs) and parsed
by the Rust runtime (morloc-runtime-types schema.rs); the round trip is
checked by morloc-runtime-types/tests/schema_corpus.rs. The C header still
carries its own copy.

- **A closure wire form nothing produces.** morloc.h declares
  `MORLOC_CLOSURE = 24` and `SCHEMA_CLOSURE 'C'` (with a comment describing
  a `[home][mid][n]` layout); closures travel as the tuple `t3sjaau1`, and the
  Rust runtime knows neither (a tag 24 panics in cschema.rs). Only two C++
  `case` labels (cppmorloc.hpp:668, 781) mention them. The `SCHEMA_*`
  defines in morloc.h are used nowhere. Removing them is a header change, so
  ABI_VERSION 14 -> 15 and every environment reprovisions: needs an OK, and
  should be one bump with the next ABI change.
- [speculative] `type Rose = [Rose]` as an argument gets schema
  `a&4Rose<list>aa^4Rose`, one list level more at each end of the cycle than
  `&4Rose<list>a^4Rose`. Same layout for pure lists, and it ran correctly.
