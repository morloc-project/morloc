# Schema grammar

The schema alphabet is written by the Haskell emitter (Serial.hs) and parsed
by the Rust runtime (morloc-runtime-types schema.rs); the round trip is
checked by morloc-runtime-types/tests/schema_corpus.rs.

- [speculative] `type Rose = [Rose]` as an argument gets schema
  `a&4Rose<list>aa^4Rose`, one list level more at each end of the cycle than
  `&4Rose<list>a^4Rose`. Same layout for pure lists, and it ran correctly.
- [read, 2026-11-16] Back-references are bound two ways. The parser
  (schema.rs width patching, null_check index_recur) and the compatibility
  check (FMT-11) bind `^name` to the nearest declaration of that name
  enclosing it; the Rust value walk (rustmorloc lib.rs RECUR_ENV,
  resolve_recur) binds it to the nearest one on the walk's dynamic stack,
  which differs when a name is shadowed after the walk jumps back through
  another back-reference (`&1Xm21f?&1Lm21g?&1Xm11h?^1L1r?^1X1si4`: at
  f.g.h.r the walk reads the inner X, 8 bytes, where the parser patched the
  outer X, 16 bytes). Not reproduced through the compiler: its schemas do
  not shadow a name across a back-reference today. But a file or stdin
  header carrying such a schema passes parsing and the schema check, and
  the walk then reads with wrong offsets and strides, possibly out of
  bounds: an unsound path reachable from outside input. Other language
  walkers (C, Python, R) not yet checked for the same binding.
- [read, 2026-11-16] A node declared twice (`&1A&1Bm11x?^1B`) keeps only
  the outer name: the declaration arm overwrites the inner arm's name, so
  in the parsed Schema every `^1B` names no enclosing declaration. It
  parsed because the back-reference was checked against the names seen
  while parsing. schema_to_string then renders `&1Am11x?^1B`, which does
  not parse, so canonicalize_schema_str and a handle's canonical storage
  can produce an unparseable string. Whether the compiler emits a doubly
  declared node is unknown.
