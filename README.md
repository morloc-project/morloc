[![experimental](http://badges.github.io/stability-badges/dist/experimental.svg)](http://github.com/badges/stability-badges)
[![github release](https://img.shields.io/github/release/morloc-project/morloc.svg?label=current+release)](https://github.com/morloc-project/morloc/releases)
[![license: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![DOI](https://zenodo.org/badge/75355860.svg)](https://zenodo.org/badge/latestdoi/75355860)

`morloc` is a functional programming language where functions are imported from
foreign languages and unified under a common type system. The compiler
generates the code needed to compose functions across languages and also to
direct automation of mundane tasks such as data validation, type/format
conversions, data caching, distributed computing, and file reading/writing. The
endgame is to develop `morloc` into a query language that returns optimized
programs from an infinite library of functions and compositions of functions.

See [the manual](https://morloc-project.github.io/docs) for more information.

If you want to get straight to playing with code, go through the steps in the
installation section and then go to the project in `demo/01_sequence_analysis`.

## Status

This project is under active development with no stability guarantees until the
v1.0 release. Pull requests, issue reports, and private messages are very
welcome.

## Installation

Compile and install the package (requires the Haskell utility `stack`):

```sh
git clone https://github.com/morloc-project/morloc
cd morloc
stack install --fast
```

`morloc` also depends on the `JSON::XS` perl module from CPAN, which can be
installed as follows:

```sh
export PERL_MM_USE_DEFAULT=1
export PERL_CANARY_STABILITY_NOPROMPT=1
sudo perl -MCPAN -e 'install JSON::XS' 
```

For Python support, you need to download the `pymorlocinternals` library from
PyPi:

```sh
pip install pymorlocinternals
# or on Mac:
pip3 install pymorlocinternals
```

For R support, you need to install the `rmorlocinternals` library from github,
in an R session, run:

```sh
R> install.packages("devtools")
R> devtools::install_github("morloc-project/rmorlocinternals")
```

C++ support currently requires a GNU compiler that supports C++11.

`morloc` modules can be installed from the `morloc`
[library](https://github.com/morloclib) with the commands such as:

```sh
morloc install cppbase
morloc install pybase
morloc install rbase
morloc install math
```

The `morloc install` commands will install the modules in the
`$HOME/.morloc/lib` folder.

`morloc install conventions` can be used to install the `conventions` module, which is a dependency for most programs importing modules.

Last of all, if you are working in vim, you can install `morloc` syntax highlighting as follows:

``` sh
mkdir -p ~/.vim/syntax/
mkdir -p ~/.vim/ftdetect/
cp vim-syntax/loc.vim ~/.vim/syntax/
echo 'au BufRead,BufNewFile *.loc set filetype=loc' > ~/.vim/ftdetect/loc.vim
```

## Getting Started

```
export hello
hello = "Hello World"
```

The "export" keyword exports the variable "hello" from the module.

Paste this into a file (e.g. "hello.loc") and then it can be imported by other
`morloc` modules or directly compiled into a program where every exported term
is a subcommand.

```
morloc make -o nexus hello.loc
```

This will generate a single file named "nexus". The nexus file is the executable
script that the user will interact with. For this simple example, it is the
only generated file. 

Calling "nexus" with no arguments or with the `-h` flag, will print a help
message:

```
$ ./nexus -h
The following commands are exported:
  hello
    return: Str
```

The `return: Str` phrases states that hello returns a string value.

The command `hello` can be called as shown below:

```
$ ./nexus hello
Hello World
```

## Composing C++ Functions

The following code uses only C++ functions (`fold`, `map`, `add` and `mul`). 

```
import cppbase (fold, map, add, mul)

export square
export sumOfSquares

square x = mul x x

sumOfSquares xs = fold add 0 (map square xs)
```

If this script is pasted into the file "example-1.loc", it can be compiled as
follows:

```sh
morloc install cppbase
morloc make -o nexus example-1.loc
```

The `install` command clones the `cppbase` repo from github
[repo](https://github.com/morloclib/cppbase) into the local directory
`~/.morloc/lib`. The `morloc make -o nexus` command will generate a file named
`nexus`, which is an executable interface to the exported functions.

You can see typed usage information for the exported functions with the `-h` flag:

```sh
$ ./nexus -h
The following commands are exported:
  square
    param 1: Num
    return: Num
  sumOfSquares
    param 1: [Num]
    return: Num
```

Then you can call the exported functions (arguments are in JSON format):

```sh
$ ./nexus sumOfSquares '[1,2,3]'
14
```

The `nexus` executable dispatches the command to the compiled C++ program,
`pool-cpp.out`.


## Language interop

`morloc` can compose functions across languages. For example:

```
import math (fibonacci)
import rbase (plotVectorPDF, ints2reals)

export fibplot

fibplot n = plotVectorPDF (ints2reals (fibonacci n)) "fibonacci-plot.pdf"
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


## The Morloc Type System

The first level of the `morloc` type system is basically System F extended
across languages. A given function will have a general type as well as a
specialized type for each language it is implemented in.

The map function has the types

```
map :: (a -> b) -> [a] -> [b]
map Cpp :: (a -> b) -> "std::vector<$1>" a -> "std::vector<$1>" b
map Python3 :: (a -> b) -> list a -> list b
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

export square
export sumOfSquares

square x = mul x x

sumOfSquares xs = fold add 0 (map square xs)
```

This example cannot be compiled since none of the functions are imported or
sourced, but it can be typechecked:

```
morloc typecheck examples/rmsWithTypes.loc
```

```
add :: Num -> Num -> Num
add Cpp :: double -> double -> double

mul :: Num -> Num -> Num
mul Cpp :: double -> double -> double

fold     :: (b -> a -> b) -> b -> [a] -> b
fold Cpp :: (b -> a -> b) -> b -> "std::vector<$1>" a -> b

map :: (a -> b) -> [a] -> [b]
map Cpp :: (a -> b) -> "std::vector<$1>" a
                    -> "std::vector<$1>" b

square x = mul x x
sumOfSquares xs = fold add 0 (map square xs)
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
