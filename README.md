[![experimental](http://badges.github.io/stability-badges/dist/experimental.svg)](http://github.com/badges/stability-badges)
[![travis build status](https://travis-ci.org/morloc-project/morloc.svg?branch=master)](https://travis-ci.org/morloc-project/morloc)
[![github release](https://img.shields.io/github/release/morloc-project/morloc.svg?label=current+release)](https://github.com/morloc-project/morloc/releases)
[![license: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![DOI](https://zenodo.org/badge/75355860.svg)](https://zenodo.org/badge/latestdoi/75355860)

`morloc` is a functional programming language where functions are imported from
foreign languages and unified through a common type system. The compiler
generates the code needed to compose functions across languages and also to
direct automation of mundane tasks such as data validation, type/format
conversions, data caching, distributed computing, and file reading/writing. In
the far future, I hope to develop `morloc` into a query language that returns
optimized programs from an infinite "library" of functions and compositions of
functions.

See [the manual](https://morloc-project.github.io/docs) for more information.

## Status

This project is under active development and may change radically without
warning. There are lots of bugs. You may peruse the issues page for a small
sample.

Pull requests and issue reports are very welcome.

All development and testing is done in Linux.

## Installation

Compile and install the package as so (requires the Haskell utility `stack`):

```sh
git clone https://github.com/morloc-project/morloc
cd morloc
stack install --fast
```

## Hello world!

```
export hello
hello = "Hello World"
```

The "export" keyword exports the variable "hello" from the module.

Paste this into a file (e.g. "hello.loc") and then it can be imported by other
`morloc` modules or directly compiled into a program where every exported term
is a subcommand.

```
morloc make hello.loc
```

This will generate a single file named "nexus.pl". The nexus is the executable
script that the user will interact with. For this simple example, it is the
only generated file. It is currently written in Perl. 

Calling "nexus.pl" with no arguemtns or with the `-h` flag, will print a help
message:

```
$ ./nexus.pl -h
The following commands are exported:
  hello [0]
```

The `[0]` states the number of arguments the "command" hello takes.

The command is called as so:

```
$ ./nexus.pl hello
Hello World
```

## Simple example using math functions from C++

The following code uses only C++ functions (`fold`, `map`, `add` and `mul`). 

```
import cppbase (fold, map, add, mul)

export square;
export sumOfSquares;

square x = mul x x;

sumOfSquares xs = fold add 0 (map square xs);
```

If this script is pasted into the file "example-1.loc", it can be compiled as
follows:

```sh
morloc install cppbase
morloc make example-1.loc
```

The `install` command clones the `cppbase` repo from github into the
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
$ ./nexus.pl sumOfSquares [1,2,3]
14
```

The `nexus.pl` executable dispatches the command to the compiled C++ program, `pool-cpp.out`.


## Language interop

`morloc` can compose functions across languages. For example:

```
import math (fibonacci)
import rbase (plotVectorPDF)

export fibplot

fibplot n = plotVectorPDF (fibonacci n) "fibonacci-plot.pdf";
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

How exactly this is accomplished depends on the language. Currently, for
Python3, R, and C++, I pass the `pack` and `unpack` functions two arguments:
the data and a type schema. For Python and R, these type schema are nested
lists specifying which functions to call to unpack each component of the data.
For C++, the schema is an undefined term with the desired type, its role is
simply to ensure currect template resolution.


## The Morloc Type System

The first level of the `morloc` type system is basically System F extended
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
import cppbase (fold, map, add, mul)

export square;
export sumOfSquares;

square x = mul x x;

sumOfSquares xs = fold add 0 (map square xs);
```

This example cannot be compiled since none of the functions are imported or
sourced, but it can be typechecked:

```
morloc typecheck examples/rmsWithTypes.loc
```

```
add :: Num -> Num -> Num;
add Cpp :: double -> double -> double;

mul :: Num -> Num -> Num;
mul Cpp :: double -> double -> double;

fold     :: forall a b . (b -> a -> b) -> b -> [a] -> b;
fold Cpp :: forall a b . (b -> a -> b) -> b -> "std::vector<$1>" a -> b;

map :: forall a b . (a -> b) -> [a] -> [b];
map Cpp :: forall a b . (a -> b) -> "std::vector<$1>" a
                                 -> "std::vector<$1>" b;

square x = mul x x;
sumOfSquares xs = fold add 0 (map square xs);
```

The typechecker associates each sub-expression of the program with a set of
types. The specific type information in `mul` is sufficient to infer concrete
types for every other C++ function in the program. The inferred C++ type of
`sumOfSquares` is

```
"std::vector<$1>" double -> double
```

The general type for this expression is also inferred as:

```
List Num -> Num
```

The concrete type of `mul` is currently written as a binary function of
doubles. Ideally this function should accept any numbers (e.g., an `int` and a
`double`). I intend to add this functionallity eventually, perhaps with a
Haskell-style typeclass system.


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
rand :: n:Int -> a:Num -> b:Num -> xs:[c:Num] where (
    n > 0
  , len xs == n
  , c >= a
  , c <= b
);
```

The constraints are optional, and `rand` could instead just be written as:

```
rand :: Int -> Num -> Num -> [Num]
```

The addition of the constraints allows

 * Static analysis of the correctness of the program. 
 * Runtime checks of input (if desired, this will be a compiler flag)
 * Formal documentation of the behavior of the function

The type system is essential for specifying how data is passed between
languages.
