# Known issues

Each file holds a group of related issues, each framed against the model:
a spec item the code violates, or a gap in the spec. Evidence levels:
reproduced, read (verified by reading), speculative. Delete an entry when
it is solved, and a file when it is empty.

- [aliases.md](aliases.md): order-dependent alias spelling (ALIAS-10), parallel unfolding mechanisms, an untested function-alias arity path
- [kinds-dimensions.md](kinds-dimensions.md): signature Nat variables are not rigid, unchecked newtype kinds, reserved-type kinds, operator classification, misplaced or renamed variables in kind diagnostics
- [diagnostics-docs.md](diagnostics-docs.md): generated names in POLY-6 errors, `@mime` lost on structural aliases, stale manual docstring example
- [typecheck.md](typecheck.md): programs that typecheck and fail later, unlocated rejections, Rec self-equality, integer literal in a point-free section, stale manual table examples
- [values.md](values.md): newtype native form dropped (NEWT-4), `data` comparison in R and C++ (SUM-5), `@savem`/`@load` digits, numpy.int64 table column in Python
- [programs.md](programs.md): single-export command name, argument numbering, `@check.path`/`@stdin` help, `@default` messages, unlocated CLI build errors, benchmark `{lang}`
- [manual.md](manual.md): manual text that contradicts a spec item or itself
- [typeclasses.md](typeclasses.md): compiler crashes in instance bodies, catch-all fallthrough depends on instance body (CLS-12), unchecked class obligations and superclasses (CLS-5)
- [records.md](records.md): design decision pending: are inline (anonymous) record types legal?
