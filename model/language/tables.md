# Tables

`Table` types: schemas, column operations, nesting. Prefix: TAB.

### TAB-1 A table type carries its row count and schema
Intent: proposed
Code: unaudited

In `Table n r`, `n :: Nat` is the row count and `r :: Rec` maps column names
to column types. Both are erased at run time. `Table` is opaque: each
language supplies its own form.

### TAB-2 Merging schemas that share a column name is a type error
Intent: proposed
Code: unaudited

`r1 + r2` where `r1` and `r2` have a key in common is a type error, whether
the result type is written or inferred.

### TAB-3 Naming a column the schema does not have is a type error
Intent: proposed
Code: unaudited

A field lookup or projection of a schema by a name or names known at
compile time fails at typechecking when a name is not in the schema. It
never leaves an unreduced type for a later stage.

### TAB-4 A table may be nested in another value
Intent: ruled 2026-09-27
Code: deviates (temporary build-time rejection)

A `Table` may be an element of a tuple or list, a record field, or a stream
element, like any other value.

### TAB-5 Which types a column may hold
Intent: open
Code: unaudited

Columns of `Bool`, `Int`, `Real`, the sized numbers and `Str` work. Is a
column of lists, records or optionals (a) a type error, or (b) legal, mapped
to the matching nested column type? Either way, a column type the program
cannot build must not pass typechecking.

### TAB-6 A schema no argument determines
Intent: open
Code: unaudited

A function whose result schema is a variable bound by none of its arguments
lets the caller claim any schema, and nothing confronts the claim with the
columns that arrive. Candidates: (a) check the claimed schema at run time
where the value arrives, as DIM-3 does for dimensions; (b) reject such
signatures.
