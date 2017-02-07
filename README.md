<!-- [![Build Status](https://travis-ci.org/arendsee/loc.svg?branch=master)](https://travis-ci.org/arendsee/loc) -->
<!-- The travis build fails because their system has a dated bison version -->

Language of Composition
=======================

## Installation

```
git clone https://github.com/arendsee/loc
cd loc
make && make install && make test
```

The `make install` command builds the LOC home (`$HOME/.loc`) where source code
and binaries required for building LOC projects is located (among other
things). It also links the LOC executable (`loc`) to `$HOME/bin`, a folder which
is naively assumed to be in `$PATH`.

To install vim syntax highlighting

```
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

No executable program can be written in pure LOC. There are no print
statements, no conditionals, no variables, no functions, no arithmetic
operators. Rather, programs are composed of functions from other languages. LOC
is a purely metaprogramming language.

The Hello World example above uses the Bash function `echo`, which is given the
input string "Hello World".

LOC scripts are partioned into sections, each with its own syntax. Only
compositions can be written in the `@path` section. Languages are specified in
the `@lang` section.

To run a LOC script:

```
$ loc hello-world.loc
$ ./manifold-nexus.py main
hello world
```

The LOC frontend first compiles the script into an intermediate language (LOC
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

The LOC backend translates LIL into executable code. It first builds an
executable, named `manifold-nexus.py` by default, that is the interface to the
LOC program. In this simple hello-world case, it contains the sole command
`main` (much more on this later).

Next it builds a working directory (by default in `.loc/tmp`) where generated
code, temporary files, and caches reside.

For each language used in the LOC script, a file named `call.<lang>` is
created. Each of these files contains a wrapper for each function used in the
LOC script.

For this hello-world program, the following (somewhat simplified) `call.sh`
script is generated

``` bash
#!/usr/bin/env bash

outdir=$HOME/.loc/tmp/loc_0

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

Composition in LOC is a bit different from conventional composition in, say,
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
