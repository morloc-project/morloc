# Known issues

Each file holds a group of related issues, each framed against the model:
a spec item the code violates, or a gap in the spec. Evidence levels:
reproduced, read (verified by reading), speculative. Delete an entry when
it is solved, and a file when it is empty.

- [typecheck-output.md](typecheck-output.md): undetermined and non-`Type` types reach code generation (TC-3), locations lost after typechecking
- [codegen-ownership.md](codegen-ownership.md): deciding a payload take walks the manifold once per candidate; a `Try` from `r <- pull` is copied when unwrapped; one variable passed for two parameters gives a Python function a duplicate parameter; a record field passed to a by-reference parameter is cloned first
- [realization.md](realization.md): containers priced by their cheapest element, let-bound and parameter values not priced where used, collapse and scoring disagree, function arguments priced twice, merged signatures drop docstrings
