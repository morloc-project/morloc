# Schemas, voidstar and codecs

- **R type map says U64 is numeric; the R codec gives integer64.** [read
  2026-10-16] root-r main.loc `type R => U64 = "numeric"` while rmorloc.c
  decodes U64 as bit64 integer64. Code generated from the map writes a U64
  literal as a plain double, so one above 2^53 is rounded. Mapping U64 to
  "bit64" instead makes a U64 list literal `c(5L, bit64::as.integer64(..))`,
  whose `c` dispatches on its first argument and strips the class.
- **Non-ASCII names break the schema.** [reproduced] Violates FMT-7 (name
  lengths are bytes). `encodeKey` (library/Morloc/CodeGenerator/Serial.hs:390)
  writes a character count; morloc-runtime-types/src/schema.rs:575-586 reads
  bytes. A record field whose name has one non-ASCII letter (U+00FC) gives
  `m24...j1bj` and fails at run time: "unknown schema character 'r'".
- **Missing record keys accepted.** [read] Violates REC-2. R
  (rmorloc.c:743-746, 1407-1410) skips a missing key: a number becomes 0,
  an optional becomes pointer 0 (volume 0) instead of null. (Python raises
  a KeyError.)
- [read] morloc-runtime/src/mpack.rs:401-403: Int limbs copied unaligned;
  json.rs:1537 reads them as `*const u64`.
- [read] mpack.rs:388-400: a 0- or 8-byte `bin` Int stores a pointer that is
  read back as the inline value.
- [read] Serial.hs:376 emits schema `*`, which schema.rs:396 rejects.
  Reachability unknown.
- [read] schema.rs:1141-1147, 1230-1233: `a:0` means "any length".
- [read] voidstar.rs:381-392: rebase turns an empty array's null into
  `shift - 1`; empty arrays lose a canonical form.
- **Python and R quiet signalling NaNs in `F32` scalars.** [read] Breaks
  WIRE-7. Both hold an `F32` scalar as a double: pymorloc.c's reader
  `PyFloat_FromDouble(*(float*)data)` and the R reader widen with a C cast,
  which sets the quiet bit on x86, and `morloc_f32_from_f64` narrows the
  same way. Vectors are bulk copies and keep the bits. Fix: move NaN
  payloads between widths by bits (f32 payload << 29) in both directions.
- [read, unreachable] nested tables handled three ways (voidstar.rs:476,
  1017, 1096); TAB-4 says tables may nest, the compiler rejects it.
- [reproduced] dangling `^` back-reference through an alias:
  /work/tests/sumtypes/03-procgen-bsp-dungeon/x10-load-recursive-nexus.
- **Record alignment.** Gap in FMT-6: records align to 8 (schema.rs:263) but
  pad width only to the largest field (:1117); the same fields as a tuple
  align to the largest field. Open: unify (a wire change).
- **Canonical bytes.** Gap: must equal values have equal bytes, padding
  included? Depends on shared-memory.md (SHM-6) and drives cache.md.
- **Voidstar byte order is unspecified.** Gap in FMT-6: FMT-1 fixes only
  the packet header as little-endian; a stream file of voidstar values
  moved between hosts of different byte order reads wrongly.

- **The JSON NUL guard accepts malformed scalars.** [read] Gap: no spec
  item governs the guard (morloc-runtime-types/src/null_check.rs
  `first_null_in_json_text`). Its scalar arm runs to the next `,` `]` `}`
  or space, so `[1"\u0000"]`, `[tru]` and `{"a":1:2}` scan as clean
  instead of malformed.
- **Lenient non-finite spellings.** [read] Gap in IFACE-9 (non-finite
  floats open): `parse_float` (morloc-runtime/src/json.rs) reads any
  spelling `f64::from_str` accepts ("Infinity", "NaN", any case), while
  the writer emits only "nan", "inf", "-inf".

- **A numpy integer returned as `Int` is refused.** [reproduced, dev
  6915296a+] Violates the docs' census example
  (docs src/content/features-tables.asc, `summarize`, shown printing
  54585374). `summarize :: Table n {state = Str, pop = Int} -> Int;
  summarize t = fold (+) 0 (getCol "pop" t)` with root-py, table-py and
  vector-py fails for every input, JSON included: "Expected int for
  MORLOC_INT, but got numpy.int64" (pymorloc.c py_size_step, the
  `PyLong_Check` in the MORLOC_INT arm, unchanged since 139b83e1). Cause
  unknown: what changed is that the fold now yields a numpy scalar.
