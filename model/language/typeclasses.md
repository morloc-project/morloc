# Typeclasses

Classes, instances, constraints, instance selection, coherence. Prefix: CLS.

### CLS-1 A class declares method signatures and nothing else
Intent: retired 2026-10-09
Code: unaudited

Replaced by CLS-7: a class may give a method a default body.

### CLS-2 The type at the use site selects the instance
Intent: proposed
Tests: spec-cls-2-1, spec-cls-2-2, spec-cls-2-3, spec-cls-2-4, spec-cls-2-5, spec-cls-2-6
Code: deviates (issues/typeclasses.md)

A method used at type `T` runs an implementation of the instance for `T`.
That instance's implementations are those it gives, in any language and any
module, plus the class default (CLS-8); they are alternatives in the sense
of SIG-2.

### CLS-3 A method name belongs to one class
Intent: proposed
Code: conforms 2026-10-21

Two classes in scope that declare the same method name are a compile error.
Methods are not importable by name (MOD-10).

### CLS-4 A constraint with no argument is a compile error
Intent: ruled 2026-09-23
Code: conforms 2026-10-21

`C => T`, where the constraint names a class but no type, is rejected.

### CLS-5 Superclasses and instance contexts are obligations
Intent: ruled 2026-10-29
Code: deviates (issues/typeclasses.md)

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
type, bypassing the instance (CLS-19).

### CLS-16 The most specific matching instance is selected
Intent: ruled 2026-10-29
Tests: spec-cls-16-1, spec-cls-16-2, spec-cls-16-3, spec-cls-16-5
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

### CLS-17 An instance is selected by its head alone
Intent: ruled 2026-10-29
Code: deviates (issues/typeclasses.md)

A head matches `T` when it instantiates to `T`. No coercion (OPT-1's
`T` to `?T`, effect widening) is applied to make a head match; a coercion
applies to a selected method's value as to any other term.

### CLS-18 A constraint nothing determines is a compile error
Intent: ruled 2026-10-29
Code: deviates (issues/typecheck.md)

A constrained type variable that does not occur in the signature's type
outside its constraints is an error at the signature. One that occurs but
that nothing at a use determines is an error at the use. Neither is
defaulted.

### CLS-19 A sourced constrained term runs the selected instance
Intent: ruled 2026-10-29
Code: deviates (issues/typeclasses.md)

A sourced implementation of a term whose signature has a class constraint
uses the host language's own operations, not the instance (root's `sort`
uses the host's comparison). It is a candidate only at a use whose
constraints all select an instance at a primitive type, whose host
operation is the instance. Elsewhere the term runs a morloc implementation
that calls the selected instance's methods (`sort` as `sortBy (<=)`).
