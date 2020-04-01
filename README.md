[![experimental](http://badges.github.io/stability-badges/dist/experimental.svg)](http://github.com/badges/stability-badges)
[![travis build status](https://travis-ci.org/morloc-project/morloc.svg?branch=master)](https://travis-ci.org/morloc-project/morloc)
[![github release](https://img.shields.io/github/release/morloc-project/morloc.svg?label=current+release)](https://github.com/morloc-project/morloc/releases)
[![license: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![DOI](https://zenodo.org/badge/75355860.svg)](https://zenodo.org/badge/latestdoi/75355860)

Morloc: a typed meta-programming language 
==========================================

The goal of Morloc is provide a universal interface to functions across
computer languages that will allow programmers to share snippets of code that
anyone can snap together to make their own applications. The common interface
is a semantic type system that describes the relationships between
language-specific types, data formats, and abstract concepts. Based on the
types, the compiler generates the code needed to link functions between
languages and also to direct automation of mundane tasks such as data
validation, type/format conversions, data caching, distributed computing, and
file reading/writing. I am also designing a cross-language database that makes
functions searchable by type (like [Hoogle](https://www.haskell.org/hoogle/)).
Ultimately, I hope to build a GitHub-like community portal around Morloc where
users can upload packages of functions or import them into their own programs.

## Status

This project is under active development and may change radically without
warning. All development is done in Manjaro Linux (an Arch-based distro) and
also tested on Ubuntu through travis. 

Pull requests and issue reports are welcome.

## Installation

Compile and install the package as so (requires the Haskell utility `stack`):

```sh
git clone https://github.com/morloc-project/morloc
cd morloc
stack build
stack install
```

## Simple example using math functions from C++

The following code uses only C++ functions (`sqrt`, `sum`, `mul` and `map`). 

```
import math (sqrt, sum, mul)
import cppbase (map)

export square;
export rms;

square x = mul x x;
rms xs = sqrt (sum (map square xs));
```

This script can be complied as follows:

```sh
morloc install math
morloc install cppbase 
morloc make examples/rootMeanSquare.loc
```

You will need to have GSL (GNU Scientific Library) installed already.

The `install` command clones the `math` and `cppbase` repos from github into the
local directory `~/.morloc/lib`. The `make` command will generate a file named
`nexus.pl`, which is an executable interface to the exported functions.

You can see the exported functions and the number of arguments they take:

```sh
$ ./nexus.pl
The following commands are exported
  square [1]
  rms [1]
```

Then you can call the exported functions:

```sh
$ ./nexus.pl rms [1,2,3]
3.741657
```

The `nexus.pl` executable dispatches the command to the compiled C++ program, `pool-cpp.out`.

## Language interop

`morloc` can compose functions across languages. For example

```
import math (fibonacci)
import rbase (plotPDF)

export fibplot

fibplot n = plotPDF (fibonacci n) "fibonacci-plot.pdf";
```

The `fibplot` function calculates Fibonacci numbers using a C++ function and
plots it using an R function. The R function `plotPDF` is a perfectly normal R
function with no extra boilerplate:

``` R
plotPDF <- function(x, filename){
  pdf(filename)
  plot(x)
  dev.off()
}
```

## Language-specific type specialization

A `morloc` program can import functions from multiple languages.

Currently data is passed between languages through a language-agnostic
intermediate format (JSON). For simple data types, generic JSON serialization
libraries may be able to handle these conversions. For example, the Python
`json` module will convert the JSON string `[1,2,3]` to the appropriate Python
list. For more advanced cases, type-specific handling can be added.

A function can be given a language-specific type, for example:

```
transpose :: forall a . Matrix a -> Matrix a
transpose py :: forall a . "numpy.matrix" a -> "numpy.matrix" a
```

The first definition is the Morloc type, where `Matrix a` represents a matrix
containing elements of generic type `a`. The second definition is
a python-specific type from the Python `numpy` module. This second definition
tells Morloc how to cast data into the python `transpose` function. 

Handling for specific types is determined by functions with the properties
`packs` and `unpacks`. These serialization functions are defined in a Morloc
module, for example:

```
source "py" from "scipy.py" (
   "unpackMatrix"
 , "packMatrix"
)

packMatrix   py :: packs   => forall a . Matrix a -> JSON;
unpackMatrix py :: unpacks => forall a . JSON -> Matrix a;
```

The Python code for handling the conversions is:

``` python
import json
import numpy as np

def unpackMatrix(jsonTxt):
    return(np.matrix(json.loads(jsonTxt)))

def packMatrix(x):
    return(json.dumps(np.matrix.tolist(x)))
```

`morloc` will then use these functions whenever it needs to cast data into
Python as Matrix types.


## The Morloc Type System

The first level of the `morloc` typesystem is basically System F extended
across languages. A given function will have a general type as well as a
specialized type for each language it is implemented in.

The map function has the types

```
map :: forall a b . (a -> b) -> [a] -> [b]
map Cpp :: forall a b . (a -> b) -> "std::vector<$1>" a -> "std::vector<$1>" b
map Python3 :: forall a b . (a -> b) -> list a -> list b
```

The general signature looks almost the same as the Haskell equivalent (except
that `morloc` universal quantification is currently explicit). The list type
constructors for C++ are very literally "type constructors" in that they are
used to create syntactically correct C++ type strings. If the type variable `a`
is inferred to be `int`, for example, then the C++ type `std::vector<int>` will
be used in the generated code. The same occurs in the python type constructors
`list`, except here the same Python type is generated regardless of the type of
`a`.

The following example is available in `examples/rmsWithTypes.loc`:

```
map Cpp :: forall a b . (a -> b) -> "std::vector<$1>" a -> "std::vector<$1>" b;
sum Cpp :: Num a => forall a . "std::vector<$1>" a -> a;
sqrt Cpp :: double -> double;
mul Cpp :: Num a => forall a . a -> a -> a;
square x = mul x x;
rms xs = sqrt (sum (map square xs)) ;
```

This example cannot be compiled since none of the functions are imported or
sourced, but it can be typechecked:

```
morloc typecheck examples/rmsWithTypes.loc
```

The typechecker associates each sub-expression of the program with a set of
types. The specific type information in `sqrt` is sufficient to infer concrete
types for every other C++ function in the program. The inferred C++ type of
`rms` is

```
"std::vector<$1>" double -> "std::vector<$1>" double
```

General types are also inferred for every sub-expression, but since no general
signatures were given, the general types remain polymorphic. Thus the general
type inferred for `rms` is `forall a b . a -> b`.

## The next level

System F is a solid foundation, but the ultimate goal is to be able to express
deep knowledge about the world. To this end, I am exploring the use of
description logic and ontologies for specifying the relationships between
types. This is the *semantic* layer of the type system.

![The user enters the Morloc script (A), which casts a string (AT1G30270.1) as a TairID and feeds it to the composition function. This function has the type signature (B) and expects input of type BioSeq. The required conversions are automatically performed following the type ontology (C). The conversions are performed by functions with the signatures shown in (D), where ?TairID indicates possible failure. These functions are given the convert role in (E). Since ProteinSeq is a BioSeq, any function of a BioSeq works automatically with ProteinSeq. The Morloc compiler](./figures/case-study.png)

One relation that can be defined between types is `a maps_to b`, which states
that any variable of type `a` can be uniquely converted to a variable of type
`b`, for example, `Int maps_to Double`. Some languages, such as Perl and
JavaScript, do extensive automatic conversions. Perl will happily evaluate the
term `"42" + 1` to 43, for example. In Morloc, these sorts of automatic
conversions are defined in ontologies that can be customized by the programmer.

Types can also be specialized with constraints, for example:

```
Count :: x:Int where ( x > 0 )
```

This is can also be used to place constraints on functions. A function is
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
languages.
