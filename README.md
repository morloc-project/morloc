# Rat

A dataflow language that supports loops and conditionals

# Templates

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

# NOTES

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

Where `.` represents compositon. Phrases like `f . a b` indicates,
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

# TODO

[x] flex recursion into import code
[ ] refactor parser: use unions instead of strings
[ ] refactor the kludgy list handling
[ ] build explicit AST
[ ] handle intermediate language printing through AST
[ ] auto label manifold instance variable names
[ ] language specific flag to sections
[ ] make informative syntax error/warning messages
[ ]  - warnings for manifolds with that are missing elements without defaults
[ ]  - warnings for unused elements
[ ]  - warnings for missing type
[ ]  - errors for incompatible types, can I use the haskell type checker?
[ ] merge backends and frontend
[ ] other compile options
[ ]  - backend language
[ ]  - import search path
[ ]  - warnings
[ ]  - print tokens
[ ]  - print intermediate
[ ] type inference, can I use haskell?
