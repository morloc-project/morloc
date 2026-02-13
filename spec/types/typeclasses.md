# Typeclasses

Typeclasses define families of types that share a common interface. They enable ad-hoc polymorphism: the same function name can have different implementations depending on the type at which it is used.

## Class Declaration

A typeclass is declared with the `class` keyword, listing its methods and their signatures:

```morloc
class Monoid a where
  empty a :: a
  op a :: a -> a -> a
```

The type variable `a` after the method name indicates which type the method is parameterized over. Each method's type signature may reference `a` and other type variables.

```morloc
class Eq a where
  eq a :: a -> a -> Bool
```

## Instance Declaration

An instance provides implementations for a typeclass at a specific type:

```morloc
instance Monoid Int where
  empty = 0
  source Cpp from "monoid.hpp" ("addInt" as op)
  source Py from "monoid.py" ("addInt" as op)
```

Instance methods may be defined as:
- Pure morloc expressions (e.g., `empty = 0`)
- Foreign function bindings (e.g., `source Cpp from ... ("fn" as method)`)
- References to existing functions

```morloc
instance Eq Int where
  eq = Int::eq

instance Eq Str where
  eq = Str::eq
```

## Constraint Resolution

When a function uses a typeclass method, the compiler resolves which instance to apply based on the concrete type at the call site:

```morloc
fold a b :: (b -> a -> b) -> b -> [a] -> b

sum :: [Int] -> Int
sum = fold op empty
```

Here, `op` and `empty` resolve to the `Monoid Int` instance. The compiler statically selects the appropriate implementation for each language.

## Constraints in Signatures

Typeclass constraints restrict the types at which a polymorphic function may be used. In morloc, constraints are currently resolved implicitly by the type checker rather than declared explicitly in signatures. The checker verifies that all typeclass methods used in a function body have instances available for the inferred type.

## Multi-Language Instances

A single instance may provide implementations in multiple languages:

```morloc
instance Monoid Int where
  empty = 0
  source Py from "monoid.py" ("addInt" as op)
  source Cpp from "monoid.hpp" ("addInt" as op)
  source R from "monoid.R" ("addInt" as op)
```

The compiler selects the language-specific implementation during realization, following the same rules as for ordinary foreign functions. See [[../interop/implementation-selection.md]].
