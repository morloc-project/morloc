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

 1. S(S) - `[(Str,Int)]`
 2. S(C) - `[Map Str Int]`
 3. S(R) - `[Person Str]
 4. C(S) - `Map Str Int`
 5. C(C) - `Map Str (Map Str Int)`
 6. C(R) - `Map Str (Person Str)`
 7. R(S) - `Person Str`
 8. R(C) - `Person (Map Str Int)` 
 9. R(R) - `Person (Person Str)`

There are infinitely more complex forms, but all of these can be systematically
reduced/expanded in (de)serialization. So long as these base cases work,
everything should be awesome. I might add a 10th case for valid recursive
structures (trees) and maybe an 11th really deep structure just for good feels.
