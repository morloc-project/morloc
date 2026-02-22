# Type Declarations

Type declarations establish the relationship between morloc's general types and their language-specific concrete representations.

## General Type Aliases

A type alias introduces a new name for an existing type expression:

```morloc
type Filename = Str
type Matrix a = [[a]]
type Pair a b = (a, b)
```

Aliases are expanded during type resolution. They do not create new types.

## Language-Specific Type Declarations

The `type Lang => Name = "concrete" params` form declares how a general type maps to a concrete type string in a specific language:

```morloc
type Py => Int = "int"
type Cpp => Int = "int"
type R => Int = "integer"
```

For parameterized types, type parameters appear after the concrete string. Positional references (`$1`, `$2`, ...) in the string are substituted with the rendered concrete type parameters:

```morloc
type Cpp => List a = "std::vector<$1>" a
type Cpp => Map k v = "std::map<$1,$2>" k v
```

Here, `List Int` in C++ becomes `std::vector<int>`, and `Map Str Int` becomes `std::map<std::string,int>`.

For languages without parameterized type syntax, parameters are listed but ignored in the string:

```morloc
type Py => List a = "list" a
type R => List a = "list" a
```

## Terminal vs. Non-Terminal Types

A type is **terminal** if it should not be further reduced during type evaluation. Concrete language types (those declared with `type Lang => ...`) are terminal -- the compiler stops resolving once it reaches them.

A type is **non-terminal** if it is an alias that should be expanded. General type aliases (`type Name = ...`) are non-terminal.

This distinction matters during type evaluation: the compiler repeatedly applies type aliases until it reaches a terminal type or a fixed point.

## Type Evaluation

Type evaluation resolves a general type to its concrete representation for a given language through the following process:

1. Look up the type name in the scope.
2. If a general alias exists and is non-terminal, substitute and recurse.
3. If a language-specific mapping exists, substitute parameters and return the concrete type.
4. If no mapping is found, report an error (the type cannot be realized in that language).

## Scope

Type declarations accumulate in a scope: a mapping from type names to their definitions. Each definition records the parameters, the resolved type, and whether it is terminal. Multiple definitions for the same name (at different arities or for different languages) coexist in the scope.
