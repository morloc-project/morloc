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
