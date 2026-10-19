# Records

Record declarations, literals, field access and update. Prefix: REC.

### REC-1 A record declaration names a fixed set of typed fields
Intent: proposed
Code: unaudited

`record R = R { f1 :: T1, ..., fn :: Tn }`, or the equivalent `where` form,
declares a type `R` with exactly the fields `f1 ... fn`. Field names are
unique within the declaration.

### REC-2 A record literal binds every declared field by name exactly once
Intent: proposed
Code: unaudited

Field order in a literal is irrelevant. A literal that misses a declared
field, names a field the record does not have, or names a field twice is a
compile error.

### REC-3 A record literal takes its type from context
Intent: ruled 2026-10-07
Code: unaudited

A record literal has the record type that a signature or an annotation
gives it; the field names alone never determine its type.

### REC-4 An instance never gives a record literal its type
Intent: ruled 2026-10-17
Tests: spec-rec-4-1, spec-rec-4-2
Code: conforms 2026-10-17

A record literal that no signature or annotation types is an anonymous
record. Instance resolution does not choose a type for it: a literal that
would get its type only from the instance a class method resolves to, as
in `{num = n, den = 2} == {num = 2 * n, den = 4}` with `instance Eq Q`, is
rejected at compile time, at the literal.

Why: ruled 2026-10-17 (#160); rejecting every untyped literal would remove
anonymous records, which pure nexus code, getters and views use.

### REC-5 Field access and update are getter and setter patterns
Intent: proposed
Tests: spec-rec-5-1
Code: unaudited

`.f r` is the value of field `f` of `r`. `.(.f = e) r` is a new record equal
to `r` except that `f` is `e`; `r` itself is unchanged. Naming a field the
record type does not have is a compile error.

### REC-6 A native record form names only the container
Intent: proposed
Code: unaudited

`record L => R = "name"` names the representation of `R` in language `L`.
Field names and types come from the general declaration and are not
repeated. What the named container must provide is in native-types.md.
