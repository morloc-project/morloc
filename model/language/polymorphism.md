# Polymorphism

Quantifiers, instantiation, generalization, scope of type variables. Prefix: POLY.

### POLY-1 Type variables in a signature are universally quantified
Intent: proposed
Code: unaudited

A lowercase name in a type signature is a type variable, quantified over
the whole signature. Every use of the term may instantiate it differently.

### POLY-2 All polymorphism is resolved before the program runs
Intent: proposed
Code: unaudited

Every type variable in a realized program is instantiated to a concrete
type at compile time. No value crosses a language boundary at a type that
still contains a variable.

### POLY-3 A variable's kind comes from its declaration or its label
Intent: proposed
Code: unaudited

An unannotated type parameter has kind `Type`. `(n :: K)` in a type
declaration gives kind `K`. In a signature, `x@Int` binds a `Nat`, `x@Str`
a `Str` and `x@[Str]` a `List` to the value of that argument.

### POLY-4 Type variables in a local signature
Intent: open
Code: unaudited

In a `where` or `let` signature, does a type variable that also occurs in
the enclosing signature (a) denote the enclosing variable, or (b) a fresh
variable quantified over the local signature alone?

### POLY-5 Generalization of local definitions
Intent: open
Code: unaudited

Is a local definition without a signature (a) generalized, so that each use
may instantiate it differently, or (b) monomorphic, with one type fixed by
its uses?
