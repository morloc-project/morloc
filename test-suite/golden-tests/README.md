Many of these tests are created in sets of `*-forms-<id>`, e.g., serial-form-8.
These sets are intended to enumerate all possible combinations (or all to a
certain depth). How the combinations are enumerated is scrawled in my
ReMarkable tablet somewhere, but I'll try to also copy them here.

# `serial-form-*`

These are all combinations to depth 2 of simple serial forms, record serial
forms, and constructed serial forms.

 1. (S) simple - a data type that maps immediately to JSON. Example: `[(Str, Int)]`

 2. (C) constructed - a parameterized data type where the parameters do not
    fully describe the constructor inputs and a constructor must be provided by
    the programmer. Example: `Map a b`

 3. (R) record - a data type with parameters that are fully described
    (corresponds to "data" constructors in Haskell). These can automatically be
    serialized/deserialized using existing constructors in the target language
    and record accessors. Example: `data Person a = Person {age :: Int, info : a}`

The combinations are:

 1.  S - (Str,Int,Bool)
 2.  C - Map Str Int
 3.  R - Person Str
 4.  S(S) - `[(Str,Int)]`
 5.  S(C) - `[Map Str Int]`
 6.  S(R) - `[Person Str]
 7.  C(S) - `Map Str Int`
 8.  C(C) - `Map Str (Map Str Int)`
 9.  C(R) - `Map Str (Person Str)`
 10. R(S) - `Person Str`
 11. R(C) - `Person (Map Str Int)` 
 12. R(R) - `Person (Person Str)`

There are infinitely more complex forms, but all of these can be systematically
reduced/expanded in (de)serialization. So long as these base cases work,
everything should be awesome. I might add a 10th case for valid recursive
structures (trees) and maybe an 11th really deep structure just for good feels.

# Experimental tests

The `x-` tests cover unimplemented features with syntactic support. They are
just tests of the parser.
import pybase (map, add)
export foo

# Eta reduction

```
foo = map (add 1.0)
```

 1. Synthesize map type |- (a -> b) -> [a] -> [b]
 2. (type args map) == 2
 3. (expr args map) == 1
 4. So add an expr arg:
       `map (add 1.0)  -->  `\xs -> map (add 1.0) xs`
 5. And do the same for add
       `\xs -> map (\x -> add 1.0 x) xs`


If an expression has too many arguments:

```
bar x = add (mul 2 x)
foo y = bar 4 y 
```

This is not really an eta-reduction problem, but a currying problem. The type of
bar is `Real -> (Real -> Real)`. This should be equivalent to `Real -> Real ->
Real`, but due to the how functions are encoded the number of expected arguments
is set to 1, rather than 2. I need to normalize the function definitions.
Better, I should rewrite them to naturally curry and avoid all this non-sense
from the start.
