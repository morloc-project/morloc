# Values in each language

Values that are wrong, dropped or unsupported in some language. Reproduced
on 0.109.0 unless marked.

- **Native form dropped silently.** [reproduced] Violates NEWT-4.
  `import root-py; newtype Path = Str; type Py => Path = "pathlib.Path";
  source Py from "native.py" ("kind" as pathKind); pathKind :: Path -> Str`
  with `def kind(x): return type(x).__name__` builds without a warning;
  `./prog pathKind notes/report.txt` prints `"str"`.
- **Comparison on `data` with fields.** [reproduced] Violates SUM-5.
  `data Shape = Circle Real | Rect Real Real | Dot`, `lt a b = a < b`,
  `eq a b = a == b`. Python is correct. R: `==` works, `<` fails at run
  time "comparison of these types is not implemented" (root/main.loc:28).
  C++: `==` and `<` fail to build (root-cpp core.hpp:654, :659 "no match for
  operator==/operator<=" on `Shape`). Enum-only `data Color = Red | Green |
  Blue` works in all three.
- **`@savem`/`@load` corrupts 48-57.** [reproduced] Violates INTR-3.
  docs reports/0079 program: saving `v` then loading returns `v - 48` for
  48..57 (`49` -> `1`); 47 and 58 round-trip. The file holds the single
  byte `49`: an ASCII digit, so the reader takes it as JSON.
- **Table column sum fails in Python.** [reproduced] Violates WIRE-1.
  Census table built with `addCol`; `summarize t = fold (+) 0 (getCol "pop"
  t)` fails for CSV, Parquet, inline JSON and stdin input: "Expected int for
  MORLOC_INT, but got numpy.int64" (pymorloc.c:744, py_size_step). The
  manual shows `54585374`.
- **No rule forbids a sourced function mutating its argument.** [read] A
  gap in EVAL-7. In Python a morloc value read twice is one shared object,
  so a sourced `def f(xs): xs.sort(); return len(xs)` changes the value
  every other reader of `xs` sees, and whether it does depends on whether
  the compiler shared or copied it. EVAL-7 requires that copying be
  unobservable but says nothing about mutation; R copies on modify, C++ and
  Rust pass shared values as `const&`/`&`. Needs a ruling: forbid it
  (unchecked, as EVAL-7 is) or have the Python pool copy arguments.
- **A `where` binding that names a parameter is rejected as cyclic.**
  [reproduced, 2026-10-17, 9f24abe4] `f x = d where d = x` typechecks but
  `morloc make` fails "Self-referential or cyclic binding: 'd`1467'";
  `let d = x in d` builds. `hasGround` returns False for a bound variable
  (library/Morloc/Frontend/Valuecheck.hs, `groundCheck`), so a binding whose
  only definition is a parameter counts as ungrounded. Spec gap: no item
  says what a binding may name, and `typecheck` and `make` disagree.
