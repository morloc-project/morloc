# Notes

This document is a freeform ramble on ratty topics

## Templates

The core functions of rat are manifold. These functions take nothing and return
nothing. They are conglomerations of the following elements:

 1. pure    - pure data transformation function
 2. cache   - cache output with 4 operations: 'has', 'get', 'put', and 'del'
 3. inputs  - list of other manifolds from which input is requested
 4. checks  - list of validator manifolds, which return booleans
 5. unpack  - function that extracts list of inputs from list of input containers
 6. run     - executes the pure function (locally, send to a cluster, whatever you say)
 7. pack    - packs the output in a container
 8. fail    - produces the output in case of an error
 9. effects - list of manifolds to call after caching result

The simplest way to make a manifold is with a closure, for example

```
def make_manifold(pure, inputs, checks, cache, effects, unpack, pack, run, fail, args):
    def manifold():
        if(cacher('has')):
            Mb = cacher('get')
        else:
            Ma = [m() for m in inputs]
            a = unpack(Ma)
            if( all([c() for c in checks]) ):
                b = run(a, args)
                Mb = pack(Ma, b)
            else:
               Mb = fail(Ma)
            cacher('put', Mb)
            for(effect in effects):
                effect()
        return Mb
    return manifold
```

However, we will often want to create many manifolds of a certain kind that
differ only in inputs but that have their own caches, so we might add a layer
to the maker

```
def make_manifold_class(pure, checks, cache_maker, effects, unpack, pack, run, fail, args):
    def manifold_class(inputs):
        cache = cache_maker()
        def manifold():
            if(cacher('has')):
                Mb = cacher('get')
            else:
                Ma = [m() for m in inputs]
                a = unpack(Ma)
                if( all([c() for c in checks]) ):
                    b = run(a, args)
                    Mb = pack(Ma, b)
                else:
                   Mb = fail(Ma)
                cacher('put', Mb)
                for(effect in effects):
                    effect()
            return Mb
        return manifold
    return manifold_class
```

Here is are are examples of how the cache makers might be implemented in
a backend

```
def make_datcache(filename):
    def cacher(op, x):
        switch(op):
            case 'has':
                return file_exists(filename)
            case 'get':
                return load_file(filename)
            case 'put':
                write_file(x, filename)
            case 'del':
                remove_file(filename)
            default:
                die()

def make_memcache():
    data = NULL
    def cacher(op, x):
        switch(op):
            case 'has':
                return is_null(data)
            case 'get':
                return data
            case 'put':
                data = x
            case 'del':
                data = NULL
            default:
                die()
```

There are a few very simple functions that all backends should have implemented
(though their names may vary).

```
def run(a, args):
    b = a(args)
    return b

def id(x, ...):
    return x

def null(...):
    return NULL

def true(...):
    return TRUE

def false(...):
    return FALSE

```

## Approach

My current approach (partially realized) is to

 1. lexical analysis of rat code, descending into imported code with flex
 2. parse rat tokens into an AST
 3. do syntax and semantic checking on the AST, emitting compile time errors
    and warnings 
 4. print rat intermediate language (RIL) from the AST.
 5. tokenize RIL with second flex program
 6. parse the tokenized RIL with a language-specific parser, perhaps loading
    the function space to allow a second level of syntax and semantic checking.
 7. construct the code for the final language

I have a few questions about this.

 1. Should a use a more featureful preprocessor? One that can concatenate all
    the code into a build file, expand macros, expand ifdefs, etc. Perhaps use
    C preprocessor before lexical analysis?

 2. Is RIL necessary? Maybe it would be easier to convert directly from the AST
    to the final language.

There are two ways a workflow can be viewed: a graph of functions, or a graph
of data. Of course the two can also be merged. If the functions are typed, the
data graph is implicit in the function graph, and can be extracted from it. An
anonymous function graph is also implicit in a data graph.

## On directions

I can write paths in two directions, from the bottom up, or from top down.

```
FinalResult <-- Piece3 <-- Piece1 Piece 2
```

Or

```
Piece1 Piece2 --> Piece3 --> FinalResult
```

The latter is reminiscent of a familiar shell pipeline. It mirrors the flow of
data from well to sink. The former is like mathematical composition.

A deeper example

```
FinalResult . ResultA ResultB
ResultA . X Y Z
X . E R T
Y . U
Z . whatever . yeah
ResultB . W . R
```

Where `.` represents composition. Phrases like `f . a b` indicates,
`f(a(),b())`. Since we are dealing with manifolds, no arguments are passed to
a and b, they know what to do. Multiple inputs prevents further chaining. `f
. a b . c` doesn't make sense. It could possibly mean that that `a` and `b`
both request input from `c`, but that is perhaps not very useful. It is better,
probably, to consider these branches as chain ends. Then drop a level into the
definitions of `a` and `b`.

```
f . x y;
x = a . c;
y = b . d . (e . i) . f . g h;
```

Where `c`, `g`, 'i', and `h` are wells, and `f` is a sink.

There is a recursive beauty to this, and it allows very clean abstractions, it
places the final goal at the top. It also clearly expresses the pull approach
the manifolds follow. It avoids the weird situation where I have to recurse to
the end of a path to find the sinks.

But why choose? I can allow both.

What if they are both used in the same statement? Should that be legal?

```
D <-- (A --> B) C 
```

```
C <-- A --> B
```

Hmm, `-->` should have higher precedence. Still mixing is pretty convoluted.

```
D <-- (B <-- A) --> C
```


```
Main = Analyze . Join . Map . Filter . Divide . Retrieve
```


sqrt(mean(runif(x)))

sqrt . mean . runif:1

log . median . runif:1


Nahhh, mixing the two needs to be illegal.

Why not express these as compositions?


## Composite output

```
A --> (B, C, D)
```

Where A has three outputs, say of types [b,c,d]. For these, we automatically
generate the linker functions B, C, and D.

## On failure

There are three kinds of failure.
  1. a validator trips, the fail function is called before the run function has
     even been called.
  2. run functions raises an exception.
  3. the run function succeeds, but the output is invalid (as assessed by
     a post-validator).

Currently, I directly handle failure of the first kind. The other two kinds can
be handled by writing checks into the pack function and passing the results in
the trappings of the pure output. This would be the monadic solution.

To this point, I have mostly thought about validators as operating on the
values of manifolds. But equally important, perhaps, is validation of
arguments. The manifold validators check the environment and the values of
other manifolds. But they cannot see the arguments of a particular manifold.
Perhaps one solution would be for a manifold to access another manifold by
reference, not value. That is, rather than accessing the stuff in the cache, it
accesses the manifold metadata.

No, there is a better way. Again, monads. I can pack the argument list into the
monad. Then you just change the unpack function of the validator to take the
argument list rather than just the pure value.

## Conditionals

Partitioning data is an important part of many workflows. `rat` needs a good
method for expressing binary trees. It isn't hard: `V1 ? V2 ? A, B, V3 ? C, D` etc
works fine. The grammar is:

```
cexp
  : COND VAL  VAL
  | COND cexp VAL
  | COND VAL  cexp
  | COND cexp cexp
```

Where the COND token is lexed with `{var} *\?` and has the value `{var}` (this
is necessary, I believe, to avoid ambiguity in 1 token look-aheads).

This allows clean expression of binary trees, e.g.

```
  V1 ? A,
  V2 ?
     V3 ? B,
     V4 ? F, G,
     V5 ? H,
     V6 ? I, J
  . x
```

Here each V. conditional statement is a function of x, y, and z. If the
function returns true, the left path is evaluated. When a leaf is reached, the
leaf function is called (on x,y,z by default).

This would compile into pseudocode like the following:

```
if (V1 x)
    A x
elif (V2 x)
    if (V3 x)
        B x
    elif (V4 x)
        F x
    else
        G x
else
    if (V5 x)
        H x
    elif (V6 x)
        I x
    else
        J x
where
x :: Foo
V1,V1,... :: Foo -> Bool
A,B,C,D,E,F,G,H,I,J :: Foo -> a
```

But how to loop this? That is, what if x is a list?

I'll discuss the possibilities in the context of this simple example:

```
V ? A, B . x
```

This operation has the signature:

```
(V -> Foo -> Bool) -> (A -> Foo -> a) -> (B -> Foo -> b) -> [Foo] -> ([a], [b])
```

There are some variants in the first expression:

```
V   :: Foo -> Bool      -- e1
V'  :: [Foo] -> Bool    -- e2
V'' :: [Foo] -> [Bool]  -- e3
```

For e1, rat would just have to map V across x in the compiled code. The
difference between e2 and e3 is more significant. e2 reduces the entire vector
to a single boolean. This will result in only a single leaf being selected and
all data being processed by the one function. e3 maps list to list, leading to
the data being partitioned between leafs. e3 would also return an ambiguous
type (unless all A-J had the same signatures).

Both uses are reasonable, and both should be allowed. But how to syntactically
distinguish them? This brings me back to the ever troublesome loop issue.

## On inputs, outputs and parameters

Since Rat is a workflow language, a multifurcating relative of linear shell
pipelines, connecting function inputs to function outputs is of prime
importance. However their are other inputs to a function, the constant
parameters. Take for example GNU grep. Grep has the general form

```
Grep :: [Line] -> Pattern -> [Line]
```

However Grep also has the flag `--invert-match`. You can add this parameter to the signature, of course:

```
Grep :: [Line] -> Pattern -> Bool -> [Line]
```

It has dozens of other options; adding all of them to the signature dilute the
clarity of the original. There are a several solutions

 1. Have a different name for each combination of arguments, e.g. grep, vgrep,
    egrep. But this quickly bloats the namespace.
 2. Make functions more atomic, then transform them, e.g. `invert grep`, but
    this requires extensive reworking of the grep implementation, something
    I want to avoid when possible. 
 3. Separate inputs from parameters. This is basically a general way to do
    option #2. Adding options changes the way the function works, but preserves
    the function signature.

I am going with #3. Continuing with the grep example, there are options in GNU
grep that alter the function type. For example, `--count` changes the signature
to

```
Grep' :: [Line] -> Pattern -> Integer
```

and `--file` changes the type to

```
Grep'' :: [Line] -> File -> [Line]
```

Of course you can also have both

```
Grep''' :: [Line] -> File -> Integer
```

GNU grep also allows you to read from a file, so for each Grep variant above,
the first argument can be swapped with `[File]`.

```
FGrep    :: [File] -> Pattern -> [Line]
FGrep'   :: [File] -> Pattern -> Integer
FGrep''  :: [File] -> File -> [Line]
FGrep''' :: [File] -> File -> Integer
```

So even after corralling type-conserving parameters into the @arg section, we
still have a combinatoric explosion. The problem is that grep does too much. We
should delegate reading files and counting lines to dedicated functions:

```
grep (read "world.txt") "waldo"
length . grep (read "world.txt") "waldo"
unnest . map (grep (read "world.txt")) (read "patterns.txt")
```

The last example uses currying, something not implemented in Rat. Also
something that I can't literally implement at all in Rat since it would require
changing client code (maybe, thar be hacks ...). However, if I create a `map`
builtin, I can compile this with a loop calling `grep (read "world.txt) x` for
all x in `(read "patterns.txt")`. However, this is inefficient. Grep has
optimizations for searching multiple patterns (or if it doesn't, it should) and
also this may require multiple loadings of the input document. This is a common
problem where vectorization is optimized and problems can't be efficiently
atomized. It is a very common problem in compiling R code. One solution to the
Grep issue would be to define Grep as

```
Grep :: [Line] -> [Pattern] -> [Line]
```

So it is always vectorized.

# A word on the future

Rat is dependent on infrastructure that does not yet exist. It is a weak
dependency, since Rat can work with any set of pure functions. But it would be
most powerful in the context of a functional database. Hoogle is the prototype.

The functional database obviously needs to allow lookup based on exact type
signatures. It also needs to understand the ontology, inheritance of types,
algebras. The starables of a type (orderable, comparable, printable, iterable,
numerable, etc). It needs to be able to trace paths between types, to enumerate
all possible paths between a specified set of inputs and outputs. There should
be compose and decompose operations. Slightly unrelated, but we should also
have methods to store and study typed workflows.

Such a database would consist of the function code, an ontology (specified in
Rat), and the signatures for each function (also in Rat).

# A word on Galaxy, KBase, IPlant desktop, and their ilk

These are similar to Rat in that they allow linking of functional units and
setting of their parameters.

Their point-and-click interfaces present a specialized subset of the
functionallity of a subset of all programs selected form a subset of all
fields. They cater to the non-programmer, offering "user friendly" access to
common workflows within specific fields. They sacrifice power for muggle
usability. Rat is far more general and allows user-written code. It could serve
as a basis for a muggle workflow engine, but is still useful to the programmer
(well, ok, it isn't useful to anyone yet).

The Galaxy class programs focus on linking wrappers for independent programs.
In contrast, Rat is a metaprogramming language for creating pure code. Since
Rat functions may be wrappers for standalone programs, Rat supersets the Galaxy
class. Rat can interface with the entire function set of any language with no
boilerplate (it isn't safe to do this, since you should at least have a type
signature). I should temper this by specifying the limitations of Rat,
specifically, it requires pure functions with types that are invariant to
parameter choise. This may necessitate writing wrappers for functions, for
example see the discussion of GNU grep above.
