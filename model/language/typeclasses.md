# Typeclasses

Classes, instances, constraints, instance selection, coherence. Prefix: CLS.

### CLS-1 A class declares method signatures and nothing else
Intent: retired 2026-10-09
Code: unaudited

Replaced by CLS-7: a class may give a method a default body.

### CLS-2 The type at the use site selects the instance
Intent: proposed
Tests: spec-cls-2-1, spec-cls-2-2, spec-cls-2-3, spec-cls-2-4, spec-cls-2-5, spec-cls-2-6, spec-cls-2-7, spec-cls-2-8, spec-cls-2-9, spec-cls-2-10, spec-cls-2-11, spec-cls-2-12, spec-cls-2-13, spec-cls-2-14
Code: deviates (issues/typeclasses.md)

A method used at type `T` runs an implementation of the instance for `T`.
That instance's implementations are those it gives, in any language and any
module, plus the class default (CLS-8); they are alternatives in the sense
of SIG-2.

### CLS-3 A method name belongs to one class
Intent: proposed
Tests: spec-cls-3-1, spec-cls-3-2, spec-cls-3-3, spec-cls-3-4, spec-cls-3-5, spec-cls-3-6, spec-cls-3-7, spec-cls-3-8
Code: deviates (issues/typeclasses.md)

Two classes in scope that declare the same method name are a compile error.
Methods are not importable by name (MOD-10). A top-level declaration with
no signature named like a method of a class the module declares is a
compile error: a method is implemented in an instance. A local definition
of that name (in `where`) shadows the method as any local name does. A
method of a class the module declares, named like a term the module
imports with its whole module, is a compile error. Open: should either
name shadow the other instead (SIG-7)? And an unsigned declaration beside
a method of an imported class is today a new term that shadows the method
(issues/typeclasses.md). An instance defines only methods of its class.

### CLS-4 A constraint with no argument is a compile error
Intent: ruled 2026-09-23
Code: conforms 2026-10-21

`C => T`, where the constraint names a class but no type, is rejected.

### CLS-5 Superclasses and instance contexts are obligations
Intent: ruled 2026-10-29
Tests: spec-cls-5-1, spec-cls-5-2, spec-cls-5-3, spec-cls-5-4, spec-cls-5-5, spec-cls-5-6, spec-cls-5-7, spec-cls-5-8, spec-cls-5-9, spec-cls-5-10, spec-cls-5-11, spec-cls-5-12, spec-cls-5-13, spec-cls-5-14, spec-cls-5-15, spec-cls-5-16, spec-cls-5-17
Code: conforms 2026-11-05

`class C a => D a` requires an instance `C T` for every instance `D T`, and
a `D a` constraint grants the methods of `C`. `instance C a => C (F a)`
provides `C (F T)` wherever `C T` holds. A constraint `C T` on a term used
at `T` requires an instance `C T` whether or not a method of `C` is called.

### CLS-6 Two instances for one type
Intent: open
Code: unaudited

When two instances of one class for the same type are in scope (for
example, from two imports), is that (a) a compile error, or (b) one
instance whose implementations are alternatives, as with `root-py` and
`root-cpp`? Which instances are in scope at all is MOD-11.

### CLS-7 A class may give a method a default body
Intent: ruled 2026-10-09
Code: deviates (unfiled; a body in a class is a parse error)

Inside a `class` block, a definition of a method declared in that block is
the method's default.

### CLS-8 A default is one more implementation at every instance
Intent: ruled 2026-10-09
Code: deviates (unfiled; defaults do not exist)

At each instance of the class, the default is an implementation of the
method alongside those the instance gives. Realization chooses among them
as among any alternatives (REAL-1, REAL-2); being a default neither raises
nor lowers its standing. An instance may omit a method that has a default.

By REAL-1, every implementation an instance gives for a method must agree
with the default at that instance. An instance implementation that differs
on purpose is a false claim, not an override.

Why: a default is an ordinary morloc implementation, so one selection rule
covers it.

### CLS-9 What a class body may define
Intent: proposed
Code: unaudited

A definition in a class block that names anything other than a method
declared in that block is a compile error. Several definitions of one
method in a class block are alternative defaults (SIG-2).

### CLS-10 A default is typed once, at the class
Intent: proposed
Code: unaudited

A default is checked at its method's general type, assuming only the
class's own constraint and, through CLS-5, its superclasses. A default that
typechecks at some instances and not others is rejected at the class. Names
in the body resolve in the class's module; an instance's module need not
import them.

### CLS-11 A default reaches every declared instance
Intent: proposed
Code: unaudited

The default of `m` is an implementation of `m` at every instance of the
class in the program, whether or not the instance declaration has a body,
whichever module declares it, and whether or not any module gives another
implementation of `m` at that type.

### CLS-12 A method an instance omits, when a more general instance exists
Intent: open
Code: unaudited

`instance C T` gives no implementation of `m`; `instance C a` (or another
instance more general than `T`) does. Is `m` at `T` (a) owned by `C T`
alone, so it has only the default, or no implementation (REAL-3) if there
is none, or (b) resolved through the more general instance, and if so,
does the general instance's implementation join `C T`'s default as an
alternative or replace it? Today it is (b), replacing, whether or not
`instance C T` has a body; root's bodiless `instance Eq (List a)` depends
on it (issues/typeclasses.md).

### CLS-13 Defaults that call each other
Intent: open
Code: unaudited

Under CLS-8, realization may choose defaults for several methods at one
instance whose bodies call each other, with no implementation the instance
gives anywhere on the loop. The program then diverges, though a choice of
the instance's own implementations would not; that breaks REAL-1. Is this
(a) prevented by realization, which never chooses defaults that reach
themselves at the same instance type through defaults alone, and rejects
the build (REAL-3) when no other choice exists, (b) prevented at the class,
where defaults that call each other at the class variables are a compile
error, or (c) left as the program's meaning?

### CLS-14 An instance missing a method with no default
Intent: open
Code: unaudited

An instance for which no module gives any implementation of a method that
has no default is (a) rejected where the instance is declared, or (b)
rejected only where the method is used at that type (REAL-3), as today.
Implementations of one instance may come from several modules, so either
check needs the whole program.

### CLS-15 A class the compiler refers to is declared in `internal`
Intent: ruled 2026-11-02
Tests: spec-cls-15-1
Code: deviates (issues/typeclasses.md)

`root` is a prelude a program may replace (MOD-18); `internal` is what the
compiler assumes. A class the compiler names (to decide literal patterns,
to derive instances, to serialize) is declared in `internal`: `Packable`,
`Eq`, `Ord`, `Show`, `Hashable` and the indexing classes. `Eq` declares
`==` and `!=`, and `Ord` declares `<`, `<=`, `>` and `>=`, so an instance
chooses the implementation of each, and a language may use its own
operators where they are right for the instance's type.

Why: defined in `root` from `==` and `<=`, the four were one or two calls
more in every language, and a native source for them was chosen at every
type, bypassing the instance (CLS-21).

### CLS-16 The most specific matching instance is selected
Intent: ruled 2026-10-29
Tests: spec-cls-16-1, spec-cls-16-2, spec-cls-16-3, spec-cls-16-5, spec-cls-16-6, spec-cls-16-7, spec-cls-16-8, spec-cls-16-9, spec-cls-16-10, spec-cls-16-11, spec-cls-16-12, spec-cls-16-13, spec-cls-16-14, spec-cls-16-15, spec-cls-16-16, spec-cls-16-17, spec-cls-16-18
Code: deviates (issues/typeclasses.md)

Instances may overlap. At a use at type `T`, the instances whose heads match
`T` are candidates, and the one whose head is at least as specific as every
other candidate's is selected. Candidates with equal heads are one instance
for this rule (CLS-2, CLS-6). Two candidates with neither more specific than
the other are a compile error at the use. Which implementations a selected
instance has for a method it omits is CLS-12. Function types compare as
APP-1 has them: `a -> b` covers `a -> b -> c`, so at a function of two
numbers an instance at `a -> b -> c` is more specific than one at `a -> b`
(spec-app-4-4).

A method use selects without regard to language. A value a pool converts
has no use: the pool selects among the `Packable` instances it can call
(spec-cls-16-14), because the wire form, not the native value, is the
contract between languages (CLS-20). A type no such instance covers does not
build: with only `Packable [(Str, v)] (Map Str v)` in R, a `Map Int v` cannot
enter R.

A bracket step or a bracket chain's `map` on a list on the wire runs natively
in the nexus when the selected instance implements no such method or is the
standard library's (root and vector modules, from the module library): no pool
starts for it (ruled 2026-11-06, spec-cls-16-17). Any other instance is called
(spec-cls-16-18).

### CLS-17 An instance is selected by its head alone
Intent: ruled 2026-10-29
Tests: spec-cls-17-1, spec-cls-17-2, spec-cls-17-3
Code: conforms 2026-11-05

A head matches `T` when it instantiates to `T`. No coercion (OPT-1's
`T` to `?T`, effect widening) is applied to make a head match; a coercion
applies to a selected method's value as to any other term.

### CLS-18 A constraint nothing determines is a compile error
Intent: ruled 2026-11-05
Tests: spec-cls-18-1, spec-cls-18-2, spec-cls-18-3, spec-cls-18-4, spec-cls-18-5, spec-cls-18-6, spec-cls-18-7, spec-cls-18-8, spec-cls-18-9, spec-cls-18-10, spec-cls-18-11, spec-cls-18-12, spec-cls-18-13, spec-cls-18-14
Code: conforms 2026-11-07

A constrained type variable that does not occur in the signature's type
outside its constraints is an error at the signature. One that occurs but
that nothing at a use determines is an error at the use, even when only one
instance could match (`[] == []`, `showList [gen n]` with one `Gen`
instance). Neither is defaulted. Nat and effect-row variables are exempt:
a class argument may hold one that nothing determines (`size` of a `Vector
n a` whose `n` is unknown). An argument a dependency determines (CLS-20)
is not undetermined.

Why: an instance's existence elsewhere in a program would otherwise decide
whether distant code compiles, and the failure would not name the class.

### CLS-19 A sourced constrained term runs the selected instance
Intent: retired 2026-11-05
Code: unaudited

Replaced by CLS-21: a host function that agrees with an instance only at
some types is an instance at those types.


### CLS-20 Packable's wire argument is determined by the packed type
Intent: ruled 2026-11-05
Tests: spec-cls-20-1, spec-cls-20-2, spec-cls-20-3, spec-cls-20-4, spec-cls-20-5, spec-cls-20-6, spec-cls-20-7, spec-cls-20-8, spec-cls-20-9, spec-cls-20-10, spec-cls-20-11, spec-cls-20-12, spec-cls-20-13, spec-cls-20-14
Code: conforms 2026-11-05

`Packable a b` has the dependency `b -> a`: the instance selected by `b`
fixes `a` where a use leaves it open, in an instance's body too (`unpack
x` needs no annotation of the wire type). No two instances may have
heads that match the same `b` and differ in `a`; this is checked when
the program's instances are collected, before typechecking. A use's or a
context's `a` must agree with the selected instance's, its open
variables solved and Nat arithmetic evaluated (`Vector (m * n) a` is
`Vector 6 Real` at `m = 2`, `n = 3`). No other class has a dependency (a
class named `Packable` with another number of parameters has none), and
no other class argument is fixed by instance selection.

In one language an instance has at most one source of `pack` and one of
`unpack`, across all its declarations (NATIVE-10). More is a compile error
naming the sources.

### CLS-21 A host function that agrees only at some types is an instance
Intent: ruled 2026-11-05
Code: deviates (issues/typeclasses.md)

A sourced implementation of a constrained term is a candidate at every use
the constraints admit, so it must agree with every instance it may run
under. A host function that agrees only at some types (Python's `sorted`
agrees with `Ord` at `Real` but not at a type deriving `Ord`, DERIV-3) is
written as an instance of a class whose method is the term, at exactly
those types, beside a general instance with a morloc body. Root's `sort`,
`unique` and `groupSort` are methods of such classes (`class Ord a =>
Sortable a`, general instance `sort xs = sortBy (<=) xs`). CLS-16 selects
between the instances.

### CLS-22 An instance body gives only implementations
Intent: ruled 2026-11-05
Tests: spec-cls-22-1
Code: conforms 2026-11-05

A signature inside an instance body is a compile error at the signature;
a method's type comes from its class.
