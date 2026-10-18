# Known issues

Each file holds a group of related issues, each framed against the model:
a spec item the code violates, or a gap in the spec. Evidence levels:
reproduced, read (verified by reading), speculative. Delete an entry when
it is solved, and a file when it is empty.

- [typecheck-output.md](typecheck-output.md): undetermined and non-`Type` types reach code generation (TC-3), locations lost after typechecking
- [codegen-ownership.md](codegen-ownership.md): deciding a payload take walks the manifold once per candidate; a `Try` from `r <- pull` is copied when unwrapped; one variable passed for two parameters gives a Python function a duplicate parameter; a record field passed to a by-reference parameter is cloned first; a C++ conditional moves its result twice; an owned Rust loop local stepped through a `match` copies its rest each step; a nested Rust constructor pattern copies each inner field it tests; a Rust loop stepped by a function returning part of its argument copies the part; local and remote-worker segments share a plan key; C++ copies what a sourced call read; a C++ source taking `T&` or returning a view breaks EVAL-7 unchecked
- [codegen-copies.md](codegen-copies.md): C++ and Rust copy projections only read, move only at some keeping positions, copy crossing-closure captures and loop-carried setters; C++ copies unchanged loop slots
- [realization.md](realization.md): containers priced by their cheapest element, let-bound and parameter values not priced where used, collapse and scoring disagree, function arguments priced twice, merged signatures drop docstrings
- [codegen-limits.md](codegen-limits.md): generated R nests long guard chains too deeply for its parser; a C++ conditional needs a default constructor; a recursive C++ builder of a `data` with a Packable-mapped field does not build
- [opaque-types.md](opaque-types.md): recursion over an opaque sourced type does not build (wire form demanded with no crossing)
