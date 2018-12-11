[![experimental](http://badges.github.io/stability-badges/dist/experimental.svg)](http://github.com/badges/stability-badges)
[![travis build status](https://travis-ci.org/morloc-project/morloc.svg?branch=master)](https://travis-ci.org/morloc-project/morloc)
[![github release](https://img.shields.io/github/release/morloc-project/morloc.svg?label=current+release)](https://github.com/morloc-project/morloc/releases)
[![license: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![DOI](https://zenodo.org/badge/75355860.svg)](https://zenodo.org/badge/latestdoi/75355860)

Morloc: a typed meta-programming language 
==========================================

The goal of Morloc is provide a universal interface to functions across
computer languages that will allow programmers to share snippets of code that
anyone can snap together to make their own custom applications. The common
interface is a semantic type system that describes the relationships between
language-specific types, data formats, and abstract concepts. Based on the
types, the compiler generates the code needed to link functions between
languages and also to direct automation of mundane tasks such as data
validation, type/format conversions, data caching, distributed computing, and
file reading/writing. I am also designing a cross-language database that makes
functions searchable by type (a little like
[Hoogle](https://www.haskell.org/hoogle/)). Ultimately, I hope to build
a GitHub-like community portal around Morloc where users can upload packages of
functions or import them into their own programs.

## Installation

Compile and install the package as so (requires the Haskell utility `stack`):

```sh
git clone https://github.com/morloc-project/morloc
cd morloc
stack build
stack install
```

`morloc` has a temporary dependency on the Java library Jena and its `arq`
command line tool. You will need to have `arq` in PATH to use Morloc
(currently). The `arq` dependency will be removed eventually, since the calls
require firing up JVM, and thus massivly slow down the compilation process.

## Minimal Example

You might start with the example in `examples/sample1.loc`. This is just a toy
script and I will have a better demo soon. Also the type signatures in this
demo are currently ignored by the compiler.

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
morloc make examples/sample1.loc
```

This will generate two files, `nexus.perl` and `pool.R`. Data from specific
nodes can now be accessed.

```sh
./nexus.perl ceiling 4.3
./nexus.perl rand_uniform 10 0 3
```

## The Morloc Type System

![The user enters the Morloc script (A), which casts a string (AT1G30270.1) as a TairID and feeds it to the composition function. This function has the type signature (B) and expects input of type BioSeq. The required conversions are automatically performed following the type ontology (C). The conversions are performed by functions with the signatures shown in (D), where ?TairID indicates possible failure. These functions are given the convert role in (E). Since ProteinSeq is a BioSeq, any function of a BioSeq works automatically with ProteinSeq. The Morloc compiler](./figures/case-study.png)

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


## Language-specific type specialization

A `morloc` program can import functions from multiple languages.

Currently data is passed between languages through a language-agnostic
intermediate format (JSON). For simple data types, generic JSON serlization
libraries may be able to handle these conversions. For example, the Python
`json` module will convert the JSON string `[1,2,3]` to the appropriate Python
list. For more advanced cases, type-specific handling can be added.

A function can be given a language-specific type, for example:

```
transpose :: Matrix a -> Matrix a
transpose py :: NumpyMatrix -> NumpyMatrix
```

The first definition is the Morloc type, where `Matrix a` represents a matrix
containing elements of generic type `a`. The second definition is
a python-specific type from the Python `numpy` module. This second definition
tells Morloc how to cast data into the python `transpose` function. 

Handling for specfic types is determined by functions with the properties `packs` and `unpacks`. These serialization functions are defined in a Morloc module, for example:

```
source "py" from "scipy.py" (
   "unpackMatrix"
 , "packMatrix"
)

packMatrix   py :: packs   => Matrix -> JSON;
unpackMatrix py :: unpacks => JSON -> Matrix;
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

