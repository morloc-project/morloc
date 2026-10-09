# What the typechecker hands to code generation

Code generation receives types the typechecker should have rejected, and
reports them at the export list. Language-side entries are in
model/language/issues/typecheck.md.

- **Undetermined type reaches code generation.** [reproduced] Violates
  TC-3. `oops = getCol "poop" census` typechecks as `Vector 2 {state=Str,
  pop=Int}."poop"`; `make` fails "The type of this term is not determined:
  nothing in the program fixes 'a'".
- **Gap: no item states what a typechecked type may contain.** frontend/
  has no item saying every type leaving the typechecker is `Type`-kinded
  and has no unreduced type-level expression. `type R = Singleton "x" Int`,
  a bare `Vector`, a list column in a `Table`, and the case above all pass
  typecheck and fail when code generation tries to serialize them
  ("cannot serialize type ...", "A declared table column must be a
  primitive ..."). Candidate: TC-4, enforced by a check at the boundary
  that fails as an internal error.
- **Gap: locations are lost after typechecking.** No item says a
  diagnostic raised after the typechecker names the construct that caused
  it (DIAG-2). Every code-generation rejection above is reported at the
  module's export list (`main.loc:1:14`).
