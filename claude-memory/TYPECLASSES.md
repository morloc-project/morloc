# Typeclasses

## Class Definition

```morloc
class Monoid a where
  empty a :: a
  op a :: a -> a -> a
```

## Instance Declaration

```morloc
instance Monoid Int where
  source Cpp from "foo.hpp" ("addInt" as op)
  source Py from "foo.py" ("addInt" as op)
  empty = 0
```

## Using Constraints

```morloc
fold a b :: (b -> a -> b) -> b -> [a] -> b

sum :: [Int] -> Int
sum = fold op empty
```

The type checker resolves typeclass constraints and selects appropriate instances.

## Example: Eq

```morloc
class Eq a where
  eq :: a -> a -> Bool

instance Eq Int where
  eq = Int::eq

instance Eq Str where
  eq = Str::eq
```

---
*See also: [[MORLOC_SYNTAX.md]], [[TYPECHECKING.md]]*
