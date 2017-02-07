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

The LOC compiler builds an executable, named `manifold-nexus.py` by default,
that is the interface to the LOC program. In this simple hello-world case, it
contains the sole command `main` (much more on this later).

The compiler also builds a working directory (by default in `.loc/tmp`) where
generated code, temporary files, and caches reside.

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
and sends it to the user. `m0` is the internal id of the "manifold" that wraps
the `echo` function.

## Composition

![composition](docs/images/composition.png)
