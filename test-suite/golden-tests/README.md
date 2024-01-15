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

# (un)packers

Packing and unpacking functions are special in morloc. The programmer does not
explicitly evoke them, rather they are used as needed in the compiled code. So
where should they be defined and how should they be imported? Types matter,
already we import type aliases and they are required for general type
inference. So packers should be imported explicitly as types. If they are not
imported, then an informative error should be raised. I had previously thought
of making them silently available everywhere, but that would cause hidden state.

A function is tagged as a (un)packer in the type signature:

```
packMap :: pack => [(a,b)] -> Map
unpackMap :: unpack => Map -> [(a,b)]
```

To this point, I've been treating packers as existing in their own
universes. But why must this be the case? They should just be normal morloc
functions. And yet they are special. It is necessary that all input/output pairs
have compatible serialization forms. But this can be typechecked. Also, it is
not necessary that all serial forms for all types are the same across the
program. This may be useful though for the morloc environment; if a given type
always has the same serialization form serialized data can be more easily passed
between systems. But this is not something morloc can enforce, rather this is a
decision the community must make. So the best morloc can do is typecheck the
packers.

Perhaps the best way to handle these packers is to make them a typeclass. Of
course, I'll need to add support for general typeclasses first, and that is a
large job. If packing functions are written just like any other typeclass, then
all my current language-specific serialization code could be be extracted from
the compiler. Instead, they could be standard morloc modules. And then you would
simply install them as you would install any other morloc modules. The same
could go for a fair bit of code generation, in fact. Such as hooks in the
manifolds, caching functions, etc.

But I should leave this for later. First I need to fix the current packing
paradigm and write the goddamn paper. Adding typeclasses can be the next
step. It will probably be a 6 month rewrite. One thing I should guarantee is
that code using the packable types should not change when I convert to
typeclasses. So the following will be backward compatible:

```
import map (Map)

foo :: Map a b -> b
foo = ...
```

Only the modules that actually define `Map` will need to change. Usually, I
should have one module for each of these types. They should encapsulate
everything that needs to be known about the type. Maybe. Actually, it might make
sense to have one general module defining a type, then many language-specific
modules that import the general module and add the language-specific instance,
and then maybe one base module that imports many language-specific modules. This
is what I am currently doing with `conventions`, `pybase`/`cppbase`/`rbase`, and
`base`.

The next paper, then would add typeclasses, replace (un)packers with a Packable
typeclass, and add other manifold functions such as debuggers (at various
places) and cachers, diagnostics etc. That is, in the next paper, I would loop
back to the original ideas of morloc. Also I'd extend the prelude library and
reach a rough useable version.
