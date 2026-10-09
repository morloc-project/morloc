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
