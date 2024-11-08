[![build status](https://github.com/morloc-project/morloc/actions/workflows/.test.yml/badge.svg)](https://github.com/morloc-project/morloc/actions/workflows/.test.yml)
[![github release](https://img.shields.io/github/release/morloc-project/morloc.svg?label=current+release)](https://github.com/morloc-project/morloc/releases)
[![experimental](http://badges.github.io/stability-badges/dist/experimental.svg)](http://github.com/badges/stability-badges)
[![license: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![DOI](https://zenodo.org/badge/75355860.svg)](https://zenodo.org/badge/latestdoi/75355860)

`morloc` is a strongly-typed functional programming language where functions are
imported from foreign languages and unified under a common type system.

See [the manual](https://morloc-project.github.io/docs) for more information.

If you want to get straight to playing with code, go through the steps in the
installation section and then visit the project `demo/01_sequence_analysis`
or the less documented `demo/02_flu`.

## Status

This project is under active development with no stability guarantees until the
v1.0 release. Pull requests, issue reports, and private messages are very
welcome.

## Running morloc

`morloc` should run on Linux and macOS. For Windows, I suggest using [Windows Subsystem for
Linux](https://learn.microsoft.com/en-us/windows/wsl/install).

The easiest way to use `morloc` is through containers. Unless you love running
with daemons, I recommend using podman.

A container with the morloc executable and batteries included can be retrieved
from the GitHub container registry as follows:

```bash
$ podman pull ghcr.io/morloc-project/morloc/morloc-full:0.50.0
```

Now you can enter a shell with a full working installation of `morloc`:

```bash
$ podman run -v $PWD:/home -it ghcr.io/morloc-project/morloc/morloc-full:0.50.0 /bin/bash
```

The `v0.50.0` may be replaced with the desired `morloc` version.

Alternatively, you can set up a script to emulate a local `morloc` installation:

```bash
#!/bin/bash
mkdir -p ~/.morloc
podman run --rm \
           -e HOME=$HOME \
           -v $HOME/.morloc:$HOME/.morloc \
           -v $PWD:$HOME \
           -w $HOME \
           ghcr.io/morloc-project/morloc/morloc-full:0.50.0 morloc "$@"
```

Name this script `morloc`, make it executable, and place it in your `PATH`. The
script will mount your current working directory and your `morloc` home
directory, allowing you to install and use modules.

This script can serve as a drop-in replacement for a local morloc compiler. It
will compile any generated C++ code and build required internal shared
libraries.

## Installing from source

Unless you know what you are doing, I don't recommend building from
source. Doing so will require a working Haskell environment. Running examples
may also require installing Python, R, and suitable C++ compilers. If you still
want to build from source, I recommend you read the `morloc`
[Dockerfile](https://github.com/morloc-project/morloc/blob/master/container/ubuntu/base/Dockerfile). It
contains instructions for Alpine and will at least point you in the right
direction.

## Installing `morloc` modules

`morloc` modules can be installed from the `morloc`
[library](https://github.com/morloclib) with the commands such as:

```sh
morloc install types
morloc install conventions
morloc install base
morloc install cppbase
morloc install pybase
morloc install rbase
morloc install math
```

The `morloc install` commands will install the modules in the
`$HOME/.morloc/lib` folder.

`morloc install conventions` can be used to install the `conventions` module,
which is a dependency for most programs importing modules.

Last of all, if you are working in vim, you can install `morloc` syntax
highlighting as follows:

``` sh
mkdir -p ~/.vim/syntax/
mkdir -p ~/.vim/ftdetect/
cp vim-syntax/loc.vim ~/.vim/syntax/
echo 'au BufRead,BufNewFile *.loc set filetype=loc' > ~/.vim/ftdetect/loc.vim
```

## Getting Started

```
module hw (hello)

hello = "Hello World"
```

We create a module named `hw` and export the `hello` term.

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
module sos (*)  -- '*' means export every term

import cppbase (fold)

square :: Real -> Real
square x = mul x x

sumOfSquares :: [Real] -> Real
sumOfSquares xs = fold add 0.0 (map square xs)
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
    param 1: Real
    return: Real
  sumOfSquares
    param 1: [Real]
    return: Real
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
module fib (fibplot)

import math (fibonacci)
import rbase (plotVectorPDF, ints2reals)

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

The map function has the general type

```
map a b :: (a -> b) -> List a -> List b
```

Lowercase terms, such as `a` and `b`, represent generic variables. The `->`
delimited patterns represent functions. So `a -> b` represents a function that
takes a value of type `a` and returns `b`. `List a` is a parameterized type, in
this case a container of elements of type `a`. The generic variables need to be
explicitly declared (in the `map a b` expression).

`morloc` can derive the language-specific type signatures from the general one
if it knows the language-specific instances of `List`. We can tell the compiler
these mappings by defining language-specific type relations:

```
type Py => List a = "list" a
type Py => Int = "int"

type Cpp => List a = "std::vector<$1>" a
type Cpp => Int = "int"
```

The list type constructor for C++ is literally a "type constructor" in that it
is used to create a syntactically correct C++ type string. If the type variable
`a` is inferred to be `Int`, for example, then the C++ type `std::vector<int>`
will be used in the generated C++ signature. The same occurs in the python type
constructors `list`, except here the same Python type, `list`, is generated
regardless of the type of `a`.

This following example cannot be compiled since none of the functions are
imported or sourced:

```
type Cpp => Real = "double"
type Cpo => List a = "std::vector<$1>" a

add :: Real -> Real -> Real
mul :: Real -> Real -> Real
fold a b :: (b -> a -> b) -> b -> [a] -> b
map a b :: (a -> b) -> [a] -> [b]

square x = mul x x
sumOfSquares xs = fold add 0 (map square xs)
```

But it can be typechecked:

```
$ morloc typecheck examples/rmsWithTypes.loc
```

The typechecker associates each sub-expression of the program with a set of
types. The specific type information in `mul` is sufficient to infer concrete
types for every other C++ function in the program. The inferred C++ type of
`sumOfSquares` is

```
"std::vector<$1>" "double" -> "double"
```

The general type for this expression is inferred as:

```
List Real -> Real
```
