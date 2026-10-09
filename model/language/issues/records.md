# Records

- **Are written anonymous record types part of the language?** Design
  decision, not a bug; needs a ruling. Gap in REC-3, which says
  "Anonymous records are not supported" but was ruled about record
  literals with no type from context (REC-4). It does not say whether a
  type written inline, without a `record` declaration, is legal.
  [reproduced] On 0.109.0 they work: `pts :: [{x = Int, y = Int}]`, a
  record-list literal, and `c4 = .[:3].x pts` build and run in Python and
  C++ (`[0,1,2]`); the manual teaches them (features-patterns.asc:22-27).
  Candidates:
  (a) legal: an inline record type is a structural type, and REC-3 covers
  only literals with no type. Then decide TEQ-6 (is an inline type equal to
  a declared record with the same fields?), and how C++ and Rust get a
  native form without the user struct NATIVE-3 requires (C++ builds one
  today; how, and whether Rust does, is unchecked).
  (b) illegal: every record type needs a `record` declaration. Then REC-3
  gains a rule rejecting inline record types at the signature, and the
  manual's pattern chapter and existing programs using them must change.
