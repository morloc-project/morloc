# The schema grammar has several definitions

The schema alphabet is written by the Haskell emitter (Serial.hs ~289-381,
encode64) and parsed by the Rust runtime (morloc-runtime-types schema.rs
~390-414), and also by C header defines, two Haskell re-parsers and one Rust
re-parser, which disagree. [read, 2026-10-23]

- **`*` is emitted but not parsed.** The emitter writes `*` for
  SerialUnknown (Serial.hs:379); the runtime rejects it as an unknown
  character. No probe reached it, but nothing keeps it unreachable. Clean
  fix: `makeSerialAST'` on UnkF (Serial.hs ~844) throws a sourced error
  naming the type and language, and SerialUnknown is deleted; needs a full
  golden run, since serialAstToType's SerialUnknown clause suggests a path
  builds the node without rendering it.
- **A closure wire form nothing produces.** morloc.h declares
  `MORLOC_CLOSURE = 24` and `SCHEMA_CLOSURE 'C'` (with a comment describing
  a `[home][mid][n]` layout); closures travel as the tuple `t3sjaau1`, and the
  Rust runtime knows neither (a tag 24 panics in cschema.rs). Only two C++
  `case` labels (cppmorloc.hpp:668, 781) mention them. The `SCHEMA_*`
  defines in morloc.h are used nowhere. Removing them is a header change, so
  ABI_VERSION 14 -> 15 and every environment reprovisions: needs an OK, and
  should be one bump with the next ABI change.
- **The closure tuple literal is written four times** (Serial.hs:376,
  py/pool.py:32, r/pool.R:296, a Cpp.hs comment); the pools should take it
  from the compiler.
- **Two Haskell classifiers.** Docstrings.classifySchema has no IFile or
  enum category, Nexus.classifySchema does; their SchemaCat types differ.
  One owner should classify (the peel and predicate helpers were merged into
  Docstrings on 2026-10-23).
- **morloc-manifest cli_value.rs peels a hint up to the first `>`** (~66-69,
  ~89-92), so a nested template hint (`<std::map<$1,$2>>`) is misread; it
  does not peel `&` declarations and strips `?` before `<` although the
  emitter writes `<hint>?`.
- [speculative] `type Rose = [Rose]` as an argument gets schema
  `a&4Rose<list>aa^4Rose`, one list level more at each end of the cycle than
  `&4Rose<list>a^4Rose`. Same layout for pure lists, and it ran correctly.
