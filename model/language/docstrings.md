# Docstrings

`--'` comments: what they attach to and which directives they carry. Prefix: DOC.

### DOC-1 A docstring attaches to what follows it
Intent: proposed
Code: unaudited

A `--'` block attaches to the next module header, signature, type inside a
signature, `source` list item, `type`/`newtype`/`record` declaration, or
record field. A line beginning `@keyword` is a directive, any other is prose;
prose lines join in order, `\@` escapes a leading `@`, and an unknown
directive is kept as prose with a warning.

### DOC-2 A docstring above a signature describes the term
Intent: ruled 2026-10-08
Code: unaudited

It is the term's description, never the description of the signature's
types or of the return type.

### DOC-3 A type's docstring is inherited wherever the type is used
Intent: ruled 2026-10-08
Code: unaudited

A docstring on a `type` declaration belongs to the type. Every argument or
return position whose type is that type carries the docstring.

### DOC-4 Term docstrings are never inherited on assignment
Intent: ruled 2026-10-08
Code: unaudited

`bar = foo` gives `bar` its own docstrings, or none; never `foo`'s. The
docstrings of the types in `bar`'s signature still apply (DOC-3).

### DOC-5 The nearest docstring wins
Intent: proposed
Code: deviates (unfiled: an alias of a function, effect or optional type loses its docstring)

For one argument position: a docstring written inline on the type in the
signature, else the docstring of the alias named there, else that of the next
alias down its chain, field by field. A `newtype` does not inherit its
representation's docstring.

### DOC-6 Prose lines whose first word ends in a colon
Intent: open
Code: unaudited

For compatibility, `--' Example: text` is read as the directive `Example`
rather than as prose. Candidates: (a) keep the colon form as a synonym for
`@keyword`; (b) retire it, so only `@` begins a directive and such lines are
prose.
