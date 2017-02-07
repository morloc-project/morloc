<!-- [![Build Status](https://travis-ci.org/arendsee/loc.svg?branch=master)](https://travis-ci.org/arendsee/loc) -->
<!-- The travis build fails because their system has a dated bison version -->

# Language of Composition

# Installation

```
git clone https://github.com/arendsee/loc
cd loc
make && make install && make test
```

The `make install` command builds the LOC home (`$HOME/.loc`) where source code
and binaries required for building LOC projects is located (among other
things). It also links the LOC executable (`loc`) to `$HOME/bin`, a fold which
is naively assumed to be in `$PATH`.

To install vim syntax highlighting

```
mkdir -p ~/.vim/syntax/
mkdir -p ~/.vim/ftdetect/
cp vim-syntax/loc.vim ~/.vim/syntax/
echo 'au BufRead,BufNewFile *.loc set filetype=loc' > ~/.vim/ftdetect/loc.vim
```

# Dependencies

 1. bison >=3.0.4
 2. flex
 3. python3
