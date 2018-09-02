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


Minimal Example
===============

Compile and install the package as so (requires the Haskell utility `stack`):

```sh
git clone https://github.com/arendsee/morloc
cd morloc
stack build
stack install
```

`morloc` has a temporary dependency on a local SPARQL database. You may try
`fuseki`, though any will do. Eventually I will remove this dependency and
provide a local embedded graph database and remote SPARQL endpoint at
Morloc.IO.

Then you can run a Morloc script, you might start with the example in
`examples/sample1.loc`. This is just a toy script and I will have a better demo
soon. Also the type signatures in this demo are currently ignored by the
compiler.

```
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
morloc examples/sample1.loc http://localhost:3030/morloc
```

Replacing the SPARQL endpoint above with your local endpoint (again, sorry for
the temporary inconvenience).

This will generate two files, `nexus.perl` and `pool.R`. Data from specific
nodes can now be accessed.

```sh
perl nexus.perl ceiling 4.3
perl nexus.perl rand_uniform 10 0 3
```

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
