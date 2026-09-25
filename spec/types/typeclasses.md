# Typeclasses

Typeclasses define families of types that share a common interface. They enable ad-hoc polymorphism: the same function name can have different implementations depending on the type at which it is used.

## Class Declaration

A typeclass is declared with the `class` keyword, listing its methods and their signatures:

```morloc
class Semigroup a where
  (<>) :: a -> a -> a

class Semigroup a => Monoid a where
  mempty :: a
```

Each method's signature may reference the class variable `a` and other type
variables. `Semigroup a =>` makes `Semigroup` a superclass of `Monoid` (see
below).

## Instance Declaration

An instance provides implementations for a typeclass at a specific type:

```morloc
instance Semigroup Int where
  source Cpp from "monoid.hpp" ("addInt" as (<>))
  source Py from "monoid.py" ("addInt" as (<>))

instance Monoid Int where
  mempty = 0
```

Instance methods may be defined as:
- Pure morloc expressions (e.g., `mempty = 0`)
- Foreign function bindings (e.g., `source Cpp from ... ("fn" as method)`)
- References to existing functions

```morloc
instance Eq Int where
  source Py from "core.py" ("morloc_eq" as (==))
```

## Constraint Resolution

When a function uses a typeclass method, the compiler resolves which instance to apply based on the concrete type at the call site:

```morloc
fold :: (b -> a -> b) -> b -> [a] -> b

sum :: [Int] -> Int
sum = fold (<>) mempty
```

Here, `(<>)` and `mempty` resolve to the `Semigroup Int` and `Monoid Int`
instances. The compiler statically selects the appropriate implementation for each language.

## Constraints in Signatures

A signature declares the classes its type variables must belong to, before `=>`:

```morloc
max :: Ord a => a -> a -> a
same :: (Sz a, Eq a) => a -> a -> Bool
concat :: Monoid (f a) => [f a] -> f a
```

A constraint names the type it constrains: a type variable or a type built
from one (`Monoid (f a)`). A class with no argument (`Ord => a -> a -> Bool`)
is an error.

## Signatures Are Contracts

A signature is a contract on its definition, checked whether or not the
definition is used:

- **Rigid type variables.** The body must work at every instance of the
  signature's variables. A body that only works at `Int` is rejected under
  `a -> a`, even where every use is at `Int`. The same holds for length
  (`Nat`) and effect variables: a body may pass them through, never fix them.
- **Constraints are the only givens.** Every class method the body uses at a
  signature variable must follow from the declared constraints, closed under
  superclasses (`Mn a` provides the methods of `Sg a` when
  `class Sg a => Mn a`). A method used at a concrete type needs an instance
  there.
- **Scoped type variables.** A local signature or annotation (`e :: T`)
  inside a definition that names one of the enclosing signature's variables
  means that variable, not a fresh one.
- **Literals.** A numeric literal has a type variable's type only when the
  variable's constraints (closed under superclasses) include a class whose
  values have literals: `Integral` or `Numeric` for an integer literal,
  `Numeric` for a real literal. `inc :: Integral a => a -> a; inc x = x + 1`
  is accepted; `one :: a -> a; one x = 1` is not. Elsewhere a literal takes
  the numeric type its context expects (`3` fills an `Int8` slot), and
  otherwise `Int` or `Real`.

The signatures and instances of the root module (the one being compiled)
are checked. Imported modules' definitions are used as their signatures
state.

## Superclasses and Instance Contexts

A class may require another (`class Sg a => Mn a`). An instance of the
subclass exists only where the superclass instance exists.

An instance may carry a context: the instances it needs at its type's
parameters.

```morloc
instance Sz a => Sz (List a) where
  sz xs = fold (\n x -> n + sz x) 0 xs
```

The context is the instance body's givens: `sz x` at `a` follows from
`Sz a`. The context entails its superclasses, and each use of the instance
must satisfy it.

## Multi-Language Instances

A single instance may provide implementations in multiple languages:

```morloc
instance Semigroup Int where
  source Py from "monoid.py" ("addInt" as (<>))
  source Cpp from "monoid.hpp" ("addInt" as (<>))
  source R from "monoid.R" ("addInt" as (<>))
```

The compiler selects the language-specific implementation during realization, following the same rules as for ordinary foreign functions. See [[../interop/implementation-selection.md]].
