# Schemas, voidstar and codecs

- **Non-ASCII names break the schema.** [reproduced] Violates FMT-7 (name
  lengths are bytes). `encodeKey` (library/Morloc/CodeGenerator/Serial.hs:390)
  writes a character count; morloc-runtime-types/src/schema.rs:575-586 reads
  bytes. A record field whose name has one non-ASCII letter (U+00FC) gives
  `m24...j1bj` and fails at run time: "unknown schema character 'r'".
- **Missing record keys accepted.** [read] Violates REC-2. Python
  (pymorloc.c:946-950, 1466-1474) and R (rmorloc.c:743-746, 1407-1410)
  skip a missing key: a number becomes 0, an optional becomes pointer 0
  (volume 0) instead of null.
- [read] morloc-runtime/src/mpack.rs:401-403: Int limbs copied unaligned;
  json.rs:1537 reads them as `*const u64`.
- [read] mpack.rs:388-400: a 0- or 8-byte `bin` Int stores a pointer that is
  read back as the inline value.
- [read] Serial.hs:376 emits schema `*`, which schema.rs:396 rejects.
  Reachability unknown.
- [read] schema.rs:1141-1147, 1230-1233: `a:0` means "any length".
- [read] voidstar.rs:381-392: rebase turns an empty array's null into
  `shift - 1`; empty arrays lose a canonical form.
- [speculative] data/lang/cpp/cppmorloc.hpp:1393: bulk read checks element
  width but not numeric kind.
- [read, unreachable] nested tables handled three ways (voidstar.rs:476,
  1017, 1096); TAB-4 says tables may nest, the compiler rejects it.
- [reproduced] dangling `^` back-reference through an alias:
  /work/tests/sumtypes/03-procgen-bsp-dungeon/x10-load-recursive-nexus.
- **Record alignment.** Gap in FMT-6: records align to 8 (schema.rs:263) but
  pad width only to the largest field (:1117); the same fields as a tuple
  align to the largest field. Open: unify (a wire change).
- **Canonical bytes.** Gap: must equal values have equal bytes, padding
  included? Depends on shared-memory.md (SHM-6) and drives cache.md.
