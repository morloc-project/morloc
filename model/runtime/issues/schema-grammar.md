# Schema grammar

The schema alphabet is written by the Haskell emitter (Serial.hs) and parsed
by the Rust runtime (morloc-runtime-types schema.rs); the round trip is
checked by morloc-runtime-types/tests/schema_corpus.rs.

- [speculative] `type Rose = [Rose]` as an argument gets schema
  `a&4Rose<list>aa^4Rose`, one list level more at each end of the cycle than
  `&4Rose<list>a^4Rose`. Same layout for pure lists, and it ran correctly.
