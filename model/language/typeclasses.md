# Typeclasses

Classes, instances, constraints, instance selection, coherence. Prefix: CLS.

### CLS-1 A class declares method signatures and nothing else
Intent: proposed
Code: unaudited

A method body inside a `class` block is a parse error; there are no default
methods. Methods are not importable by name (MOD-10).

### CLS-2 The type at the use site selects the instance
Intent: proposed
Code: unaudited

A method used at type `T` runs the body of the instance for `T`. An
instance may give a method several implementations, in several languages,
and they are alternatives in the sense of SIG-2.

### CLS-3 A method name belongs to one class
Intent: proposed
Code: unaudited

Two classes in scope that declare the same method name are a compile error.

### CLS-4 A constraint with no argument is a compile error
Intent: ruled 2026-09-23
Code: deviates (unfiled; accepted today)

`C => T`, where the constraint names a class but no type, is rejected.

### CLS-5 Superclasses and instance contexts are obligations
Intent: proposed
Code: unaudited

`class C a => D a` requires an instance `C T` for every instance `D T`, and
a `D a` constraint grants the methods of `C`. `instance C a => C (F a)`
provides `C (F T)` wherever `C T` holds.

### CLS-6 Two instances for one type
Intent: open
Code: unaudited

When two instances of one class for the same type are in scope (for
example, from two imports), is that (a) a compile error, or (b) one
instance whose implementations are alternatives, as with `root-py` and
`root-cpp`? Which instances are in scope at all is MOD-11.
