# Typecheck

What the typechecker may assume and must produce. Prefix: TC.

### TC-1 Every type name reaching the typechecker names one declaration
Intent: proposed
Code: unaudited

Name resolution has bound each type name to its declaring module (MOD-14).
The typechecker never resolves a type name by its spelling alone.

### TC-2 Aliases are compared by expansion
Intent: proposed
Code: unaudited

The typechecker keeps alias names in the types it reports, and wherever two
types are compared or decomposed it unfolds an alias head first, so no
judgment depends on whether an alias was written (ALIAS).

### TC-3 No undetermined type leaves the typechecker unreported
Intent: open
Code: unaudited

A type variable left undetermined after checking an export either defaults
by a stated rule or is a located error. Open: which variables may default,
and to what?
Issue #112 reports undetermined variables reaching code generation.
