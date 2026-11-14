# Sources

`source` declarations: files, names, signatures, the claims a source makes. Prefix: SRC.

### SRC-1 A sourced name is a foreign symbol, checked against the language's name patterns
Intent: proposed
Code: conforms 2026-10-21

The name in a `source` declaration must match the language's identifier or
operator pattern. Anything else, such as raw code (`lambda x: x`), is
rejected. The patterns are per-language data, not knowledge built into the
compiler.

### SRC-2 `as` and `--' name:` rename a sourced term, and are equivalent
Intent: proposed
Tests: spec-src-2-1, spec-src-2-2, spec-src-2-3
Code: conforms 2026-10-21

The parenthesized form with `as` and the block form with a `--' name:`
docstring declare the same thing. Without either, the morloc name is the
foreign name. An item of the parenthesized form names its foreign function
itself, as does a backtick-quoted item, so a `--' name:` above either is
an error.

### SRC-3 A sourced signature is a claim the compiler does not check
Intent: proposed
Code: conforms 2026-10-21

The general type of a sourced term, including its effect row, is the
author's assertion about the foreign code. Effect rows are required there and
never inferred (see `EFF`).

### SRC-4 Whether a backtick name is checked
Intent: open
Code: unaudited

A backtick name (`` `and` as (&&) ``) is emitted as infix text between the
two arguments; the manual says verbatim, for any infix operator. That admits
raw code, which SRC-1 rejects. Either (a) backtick text must match a
per-language list of keyword operators; or (b) it is verbatim, as an
exception to SRC-1.

### SRC-5 When a sourced name that does not exist is reported
Intent: open
Code: unaudited

A Python source naming a builtin or a missing function builds and fails when
called. Either (a) the build checks that each sourced name exists, where the
language allows; or (b) the failure is a run-time error naming the term.

### SRC-6 A source cannot take a name the pool reserves for itself
Intent: proposed
Code: deviates (../runtime/issues/binder-names.md)

A language reserves name prefixes for what its pool defines (lang.yaml
`ldReservedPrefixes`: Python `__mm_`, `__morloc`; R `.__mm_`, `.__morloc`,
`.morloc_`; Rust `__mm_`, `__morloc`, `__Morloc`, `__MORLOC`) and exact names
(`ldReservedNames`: Rust `main`). A sourced name, or any `::` segment of
one, that has a reserved prefix or is a reserved name is a compile error. The
R pool keeps its runtime in one environment, `.__morloc`, and its
manifolds as global `.__mm_<id>`; an R source that defines, replaces or
removes one of these fails to load, and so does a call that assigns one.
The helpers user R files call, `.morloc.source`, `.morloc.pooldir` and
`.morloc.libdir`, are bound globally. A source with no file names the
language's builtin of that name, never a name the pool defines (lang.yaml
`ldBuiltinQualifier`; Python reaches it through its `builtins` module), so
sourcing a pool function such as Python's `dispatch` fails at the call.

### SRC-7 A language may declare build steps for each file it sources
Intent: proposed
Code: conforms 2026-11-01
Tests: spec-src-7-1

A language's `lang.yaml` may list steps under `build: per_source:`, which
`morloc make` runs for every file the pool sources, after the pool is
written and before the program is swapped in. A step is `remove: PATTERN`,
which deletes the files matching the pattern (`*` only in its last path
component), or `check: COMMAND`, a shell command run only under
`-X <lang>:check=on` that fails the build on a non-zero exit with its
error output. `{{path}}`, `{{dir}}` and `{{stem}}` name the sourced file,
its directory and its name without extension; in a `check` command they are
shell-quoted. A `<lang>:check` value other than `on` or `off` is a build
error. A file that matches a `remove` pattern but cannot be removed fails
the build.

### SRC-8 A program refuses to run a sourced file that changed after its build
Intent: ruled 2026-11-01
Code: deviates (../runtime/issues/build-and-text.md)
Tests: spec-src-8-1, spec-src-8-2, spec-src-8-3

A pool of a language that loads its sources when it starts (Python, R)
records in the manifest each file it sources, by absolute path, with a
hash of its contents at build. Before the nexus starts any pool, it checks
every pool it is about to start; if a file is missing or its contents
differ, nothing starts and the run fails with exit status 1, naming the
file and saying to rebuild. A result is then never computed, or cached,
from code other than what was built. A compiled language's sources are
fixed into its build and are not checked. Files that a sourced file loads
in turn are not covered (../runtime/issues/cache.md).

An installed program records the copies of its package's files mirrored
into its install root, which are what it runs, so editing the package does
not stop it. Its manifest also records how to repeat its build. After
`morloc install --force`, every installed program whose recorded files no
longer match is rebuilt that way; the install fails if one cannot be
rebuilt. `--no-rebuild` names those programs instead.
