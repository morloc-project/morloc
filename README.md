[![travis build status](https://travis-ci.org/arendsee/morloc.svg?branch=master)](https://travis-ci.org/arendsee/morloc)
[![github release](https://img.shields.io/github/release/arendsee/morloc.svg?label=current+release)](https://github.com/arendsee/morloc/releases)
[![license: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# Morloc: a typed meta-programming language 

The goal of Morloc is provide a universal interface to functions across
computer languages that will allow programmers to share snippets of code that
anyone can snap together like Legos to make their own custom applications. The
common interface is a semantic type system that describes the relationships
between language-specific types, data formats, and abstract concepts. Based on
the types, the compiler generates the code needed to link functions between
languages and also to direct automation of mundane tasks such as data
validation, type/format conversions, data caching, distributed computing, and
file reading/writing. I am also designing a cross-language database that makes
functions searchable by type (a little like Hoogle). Ultimately, I hope to
build a GitHub-like community portal around Morloc where users can upload
packages of functions or import them into their own programs.

So far I have written two prototypes, described below.


Haskell prototype
=================

Compile and install the package as so (requires the Haskell utility `stack`):

```sh
git clone https://github.com/arendsee/morloc
cd morloc
stack build
stack install
```

Then you can run a Morloc script, you might start with the example in
`examples/sample1.loc`. This is just a toy script and I will have a better demo
soon. Also the type signatures in this demo are currently ignored by the
compiler.

```
# examples/sample1.loc
export ceiling
export rand_uniform

source "R" (
    "runif" as rand_uniform
  , "ceiling"
);

# impose a type signature on the imported function rand_uniform
rand_uniform :: n:Int, a:Num, b:Num -> xs:[c:Num] where (
    n > 0
  , len xs == n
  , c >= a
  , c <= b
);

# type signatures for ceiling (NOTE: adding constraints is optional)
ceiling :: [Num] -> [Int];

# A simple wrapper specializing the imported rand_uniform function
rand :: Int -> [Num];
rand n = rand_uniform n 0.0 1.0;
```

This script can be complied as follows:

```sh
morloc examples/sample1.loc
```

This will generate two files, `nexus.perl` and `pool.R`. Data from specific
nodes can now be accessed.

```sh
perl nexus.perl ceiling 4.3
perl nexus.perl rand_uniform 10 0 3
```

I am currently working on cleaning up this interface. Soon it will generate
a nice Perl program with automatically generated documentation and good
argument handling.


The Morloc Type System
======================

The type system and the syntax for specifying the type ontologies is still
under construction. But here is a little information on how it will work.

Morloc unifies all programming languages under a common type system. This is
a *semantic* type system, where the types, and the relations between them, are
described using ontologies.

One relation that can be defined between types is `a maps_to b`, which states
that any variable of type `a` can be uniquely converted to a variable of type
`b`, for example, `Int maps_to Double`. Some languages, such as Perl and
Javascript, do extensive automatic conversions. Perl will happily evaluate the
term `"42" + 1` to 43, for example. In Morloc, these sorts of automatic
conversions are defined in ontologies that can be customized by the programmer.

Types can also be specialized with constrains, for example:

```
Count :: x:Int where ( x > 0 )
```

This is can also be sued to place constraints on functions. A function is
a compound type that is composed of the types of its inputs, outputs, and
a list of constraints. Here is a signature for a function that generates *n*
random numbers between *a* and *b*.

```
rand :: n:Int, a:Num, b:Num -> xs:[c:Num] where (
    n > 0
  , len xs == n
  , c >= a
  , c <= b
);
```

The constraints are optional, and `rand` could instead just be written as:

```
rand :: Int, Num, Num -> [Num]
```

The addition of the constraints allows

 * Static analysis of the correctness of the program. 
 * Runtime checks of input (if desired, this will be a compiler flag)
 * Formal documentation of the behavior of the function

The type system is essential for specifying how data is passed between
languages. I'll elaborate more on this soon.

C prototype (obsolete)
======================

The following documentation is relevant only to Morloc v0.10.0.

## Installation

``` bash
git clone https://github.com/arendsee/morloc
cd morloc
# get the C version of Morloc
git reset --hard 55e9aa53
make && make install && make test
```

The `make install` command builds the Morloc home (`$HOME/.loc`) where source
code and binaries required for building Morloc projects is located (among other
things). It also links the Morloc executable (`loc`) to `$HOME/bin`, a folder
which is naively assumed to be in `$PATH`.

To install vim syntax highlighting

``` bash
mkdir -p ~/.vim/syntax/
mkdir -p ~/.vim/ftdetect/
cp vim-syntax/loc.vim ~/.vim/syntax/
echo 'au BufRead,BufNewFile *.loc set filetype=loc' > ~/.vim/ftdetect/loc.vim
```

## Dependencies

 1. bison >=3.0.4
 2. flex
 3. python3

Examples
========

## Hello World

![hello-world](docs/images/hello-world.png)

No executable program can be written in pure Morloc. There are no print
statements, no conditionals, no variables, no functions, no arithmetic
operators. Rather, programs are composed of functions from other languages.
Morloc is a purely metaprogramming language.

The Hello World example above uses the Bash function `echo`, which is given the
input string "Hello World".

Morloc scripts are partitioned into sections, each with its own syntax. Only
compositions can be written in the `@path` section. Languages are specified in
the `@lang` section.

To run a Morloc script:

```
$ morloc hello-world.loc
$ ./manifold-nexus.py main
hello world
```

The Morloc frontend first compiles the script into an intermediate language (Morloc
Intermediate Language, LIL). The simple language specifies the elements of the
program that will be generated.

```
EMIT  m0  sh
FUNC  m0  echo
INPP  m0  0  "hello world"  *
```

The instruction `EMIT` declares a new "manifold", with id `m0`, that will
contain `sh` (Bash) code. `FUNC` links the function `echo` to `m0` (there may
be multiple manifolds that wrap `echo`, but each will have a unique id). `INPP`
assigns the literal `"hello world"` to the 0th positional argument slot of
`echo`. Ignore the star for now.

The Morloc backend translates LIL into executable code. It first builds an
executable, named `manifold-nexus.py` by default, that is the interface to the
Morloc program. In this simple hello-world case, it contains the sole command
`main` (much more on this later).

Next it builds a working directory (by default in `.morloc/tmp`) where generated
code, temporary files, and caches reside.

For each language used in the Morloc script, a file named `call.<lang>` is
created. Each of these files contains a wrapper for each function used in the
Morloc script.

For this hello-world program, the following (somewhat simplified) `call.sh`
script is generated

``` bash
#!/usr/bin/env bash

outdir=$HOME/.morloc/tmp/morloc_0

m0 () {
    echo  "hello world"
}

manifold_exists() {
    type $1 | grep -q function
}

if manifold_exists $1
    $@
else
    exit 1 
fi
```

The `manifold-nexus.py` program passes `m0` to this script when
`./manifold-nexus.py main` is called. It then takes the output of `call.sh m0`
and sends it to the user.

## Composition

![composition](docs/images/composition.png)

The composition in the function above is identical to the shell command:

``` bash
man ls | grep -Po "[a-z]+ed" | sort | uniq
```

Entries in the `@arg` section attach arguments to functions. The composition in
the `@path` section should be as clean and minimal as possible. It should
express the pure and abstract sequence of data transforms at the core of the
analysis. Details, such as parameters, run environments, caching paradigms,
effects, debugging statements, and checking are all implemented in the modifier
sections. So far, I have introduced the `@lang` and `@arg` sections, but there
are many more to come.

Composition in Morloc is a bit different from conventional composition in, say,
Haskell. The `.` operator passes the elements on the right as the inputs to any
elements of the left that can take inputs.

This can be a simple chain `h . g . f`. But it can also pass multiple
arguments. For example, in `f . x y z`, the outputs of functions `x`, `y`, and
`z` are passed as the first three positional arguments of `f`. Importantly, it
is not the function `x`, or the manifold that wraps it, that is passed to `f`;
it is the output of `x`.

Here are the legal elements of composition:

 1. manifolds - `h . g . f`
 2. constants - `f . 1 "hello"`
 3. literals - ``f . `foo```
 4. functions - more on this later

Compositions can also be nested to arbitrary depth

```
a . (b . c) (t . r) . z
```

The above could be translated into the following conventional function:

```
a( b(c(z()))), t(r(z())) )
```

## Random random number generator

![modifiers](docs/images/modifiers.png)

The above script samples numbers from a normal distribution where all paramters
are in turn sampled from random distributions.

This example introduces several new things.

First of all, I switched to a new language, R. Also I wrote a bit of R source
code (see the `is.positive` function). This code will be passed verbatim to the
`call.R` executable.

A very important part of any non-trivial pipeline is data validation. This is
handled in Morloc by entries in the `@check` section. Above I attach a function to
the normal sampler that checks whether the standard deviation (which is drawn
N(2,1) distribution) is positive. If it isn't, no output is printed. The checks
not are limited to accessing the inputs of their parents. Rather, they are free
manifolds that can query any manifolds in the program with the sole restriction
that they return a boolean.

I have also added a cache system, specifying that the outputs of all `rnorm`
and `rbinom` manifolds are stored in memory. I only really need a cache on
`rnorm:sd`, which would otherwise be called twice: once by `rnorm:main` and
once by `is.positive`.

The string following the `:` (e.g. `rnorm:main`) is a label used to resolve
otherwise indistinguishable manifolds. They are not translated into LIL, since
internally all manifolds get unique ids (e.g. `m0`) and are not referred to by
they abstract names. The syntax `<rnorm:sd>` indicates that a specific
manifold (the `rnorm:sd` implicitly declared in the `@path` section) is to be
called, rather than a new `rnorm:sd`.

In the argument section, I am now using named arguments. The `=` is a Morloc
operator, it will be translated into the correct language-specific binder by
the frontend.

The code will be translated into the following LIL (skipping included code):

```
NSRC  R

    is.positive <- function(x){ all(x > 0) }

EMIT  m0  R
FUNC  m0  rnorm
INPM  m0  0  m1  *
INPM  m0  1  m2  *
INPM  m0  2  m3  *
CHEK  m0  m4
CACH  m0  memcache
EMIT  m1  R
FUNC  m1  rbinom
CACH  m1  memcache
FARG  m1  0  n  1
FARG  m1  1  size  100
FARG  m1  2  prob  0.1
EMIT  m2  R
FUNC  m2  rnorm
CACH  m2  memcache
FARG  m2  0  n  1
FARG  m2  1  mean  0
FARG  m2  2  sd  10
EMIT  m3  R
FUNC  m3  rnorm
CACH  m3  memcache
FARG  m3  0  n  1
FARG  m3  1  mean  2
FARG  m3  2  sd  1
EMIT  m4  R
FUNC  m4  is.positive
INPM  m4  0  m3  *
```

## Hooks and loops

![hooks](docs/images/hooks.png)

Morloc has no explicit syntax for any control structures. Instead it relies on
high-order functions defined in the source code. The above example demonstrates
the Morloc equivalent of a `for` loop. The `&(...)` phrase effectively takes
a composition and transforms it into a function that is passed as an argument
to `map`. Arguments passed into this composition are accessed with `$N` calls.
The `map` function is required to be defined in the source language (it is
imported here with `core/control`).

This script converts the previous random number generator into a function that
is mapped across a sequence of numbers. Note that is uses two language: R and
Bash ('sh').

The rather cryptic `@5` and `@1` sections attach arbitrary code (hooks) to
a manifold.

Below is a template R function with the locations of all 9 possible hooks.

```R
m1 = function (){
    # @0 e.g. m7()
    if(cache_chk("m1")){
        # @8
        b = cache_get("m1")
        # @9
    }
    else{
        # @2
        if( check() ){
            # @4
            b = fun([input], [arg])
            cache_put("m1", b)
            # @5
        } else {
            # @6
            b = fail()
            cache_put("m1", b)
            # @7
        }
        # @3
    }
    # @1
    return b
}
```

If a function is attached to the `@5` slot of a manifold, it will be executed
after the pure function is run but will not be called if a value is taken from
the cache. I use this above to 1) plot the output after the loop and 2) log the
results of each sampling.

`@1` is always called on exit. In the example above, the line

```
@1
null :: cleanup
```

calls the Bash `cleanup` function after the entire program is complete (`null`
is the terminal manifold).

## Types and ontologies

![types](docs/images/types.png)

Morloc currently supports simple types. The syntax is nearly identical to
Haskell, with the addition of two keywords: `?` and `Void`. `?` matches any
number of inputs of any type. `Void` implies no output (if on the right) or
input (if on the left).

Morloc does not yet support generic types, type constructors, algebraic types and
all that. But it will in the future.

Types are important for several reasons.

First, typechecking can catch incorrect connections at compile time. Indeed,
a fully typed workflow can be proven to be correct at a high level, assuming of
course the the user has correctly entered the type (unlike Haskell, Morloc
cannot typecheck "all the way down").

Also, the type signatures are a formal documentation succinctly describing the
behaviour of a function.

In the context of Morloc, however, the greatest value of types is that
a well-desiged type system can transcend language barriers. Two functions with
the same signature can, in theory, be swapped without altering the correctness
of the pipeline (at the type level), regardless of their language. 

Morloc distinguishes between "universal" and "native" forms of data. Calls
between manifolds of the same language, can transfer data using native data
structures, as they would in a conventional program. However, foreign calls
require the data is first transformed to a "universal" form, passed to the
foreign language, and then transformed into the foreign "native" form. Types
direct these transforms.

## Funding

This work is funded by the National Science Foundation grant:

[NSF-IOS 1546858](https://www.nsf.gov/awardsearch/showAward?AWD_ID=1546858)
Orphan Genes: An Untapped Genetic Reservoir of Novel Traits
