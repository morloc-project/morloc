# Polymorphism

Morloc supports parametric polymorphism: functions and types may be parameterized over type variables that are instantiated at each use site.

## Parametric Polymorphism

A polymorphic function operates uniformly over all types. Type variables (lowercase identifiers) in a signature are implicitly universally quantified:

```morloc
id a :: a -> a
map a b :: (a -> b) -> [a] -> [b]
fst a b :: (a, b) -> a
```

The type variable names after the function name and before `::` make the quantification explicit: `map a b :: ...` means "for all types `a` and `b`, ...".

## Universal Quantification

Universal quantification (`forall`) is the standard form of polymorphism. A universally quantified type promises that the function works for *any* instantiation of the type variable:

```
forall a. a -> a
```

In morloc syntax, universal quantification is implicit in type signatures. The type variables listed between the function name and `::` are universally quantified.

## Existential Quantification

Existential types arise internally during type inference. An existential variable (`a-hat`) represents an unknown type that the checker must solve. The programmer does not write existentials directly.

Existentials may carry constraints:
- **Type parameter constraints**: the existential is known to be parameterized (e.g., `a-hat` applied to some types).
- **Record constraints**: the existential is known to have certain fields.
- **Openness**: open existentials may acquire more constraints; closed existentials are fully determined.

## Higher-Rank Types

Morloc's type system supports higher-rank polymorphism, where polymorphic types may appear in argument positions:

```
(forall a. a -> a) -> Int
```

This type requires a function that is polymorphic -- not merely a function at some specific type. The bidirectional type checker handles higher-rank types through its synthesis/checking discipline: polymorphic arguments are checked, not synthesized.

## Multi-Arity Functions

Morloc normalizes curried functions to multi-argument form internally:

```
a -> b -> c    normalizes to    FunU [a, b] c
```

This does not affect the surface syntax, where `->` is right-associative as expected. The normalization is a compiler optimization that simplifies type checking for multi-argument functions.

## Eta Expansion

When a lambda body returns a function type, the compiler eta-expands:

```morloc
\x -> f         -- where f : a -> b
-- becomes
\x y -> f y     -- the hidden argument is made explicit
```

This ensures that all functions are fully applied in the generated code, which is necessary for correct cross-language dispatch.
