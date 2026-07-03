# expose-tests

End-to-end tests for the `expose:` field in `package.yaml`. For each
language a fixture module declares files to expose, and a downstream
consumer references those files through the per-language canonical path
that the install pipeline sets up under `$MORLOC_HOME`.

```
              expose declared in           lands at                     consumer references
  C++         expose.cpp                   $MORLOC_HOME/include/<m>/    #include "<m>/foo.hpp"
  Python      expose.py                    $MORLOC_HOME/lib/python/<py_m>/   import <py_m>.pkg.sub.util
  R           expose.r                     $MORLOC_HOME/lib/R/<m>/      .morloc.source("<m>/util.R")
```

For Python, the module name has hyphens replaced with underscores so it
is a legal Python identifier (`tensor-cpp` -> `tensor_cpp`).

The test also asserts that `morloc uninstall <m>` wipes the exposed
copies, so the install/uninstall pair is symmetric.

## Module names

The fixture modules use the `morloc-test-` prefix so they will not
collide with anything the user has intentionally installed:

- `morloc-test-expose-cpp`
- `morloc-test-expose-py`
- `morloc-test-expose-r`

A bash `trap EXIT` cleans up every module the script touched, even on
failure.

## What each test exercises

- **cpp** -- expose'd `foo.hpp` does `#include "foo_helpers/util.hpp"`
  (relative), proving subtree structure is preserved on copy and that
  GCC's quote-include resolves co-located headers.
- **py**  -- expose'd subpackage with `__init__.py` at every level and a
  Python relative import (`from ..helpers import add_one`) across the
  subpackage tree.
- **r**   -- expose'd `util.R` does `source("helpers.R")` at the top
  level; resolves via the `chdir=TRUE` flag in `.morloc.source`.

Each consumer computes `doubler(util_add_one(7))` = `16`.

## Usage

```bash
./run-tests.sh           # run all three
./run-tests.sh cpp       # filter
./run-tests.sh cpp py    # subset
```

The script requires `morloc` on `$PATH` and assumes the standard
`$MORLOC_HOME` layout (`~/.local/share/morloc/` by default).

The fixture directory names match the morloc module names exactly --
`installModule` derives the install-target name from `basename(path)`,
not from `package.yaml`'s `name` field, so a fixture in
`./morloc-test-expose-cpp/` installs as the module
`morloc-test-expose-cpp`.

## Why not golden tests?

Golden tests under `test-suite/golden-tests/` use `morloc make` in an
isolated work directory and diff stdout against `exp.txt`. They do not
mutate global state.

These tests do the opposite: they exercise `morloc install`, whose
entire job is to write to `$MORLOC_HOME/src/...`,
`$MORLOC_HOME/include/...`, `$MORLOC_HOME/lib/python/...`,
`$MORLOC_HOME/lib/R/...`, and `$MORLOC_HOME/fdb/`. Any test that calls
`morloc install` is mutating the same global state the user develops
against, which means:

1. The test must clean up after itself (the trap handles this).
2. The test cannot run safely in parallel with other tests that touch
   the same `$MORLOC_HOME`.
3. Isolating into a per-test `$MORLOC_HOME` would require a fresh
   `morloc init -f` per run, which compiles libmorloc from source and
   takes minutes -- infeasible at the cadence of `stack test`.

The existing `install-tests/` directory uses bash for the same reason
(testing `morloc make --install`, also a global-state mutation). When
the work being verified *is* install-time machinery, bash + trap is the
right fit.
