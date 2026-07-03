#!/bin/bash
set -e

MORLOC_HOME="$1"
BUILD_DIR="$2"
SANITIZE_FLAGS="$3"
INCLUDE_DIR="$MORLOC_HOME/include"
LIB_DIR="$MORLOC_HOME/lib"

# Allow overriding the C/C++ compilers via env vars.
: "${CC:=gcc}"
: "${CXX:=g++}"

# Install mlccpptypes if not present
if [ ! -d "$INCLUDE_DIR/mlccpptypes" ]; then
    git clone https://github.com/morloclib/mlccpptypes "$INCLUDE_DIR/mlccpptypes"
fi

# Install headers
cp "$BUILD_DIR/cppmorloc.hpp" "$INCLUDE_DIR/"
cp "$BUILD_DIR/mlc_arrow.hpp" "$INCLUDE_DIR/"

# Install nanoarrow headers
mkdir -p "$INCLUDE_DIR/nanoarrow"
cp "$BUILD_DIR/nanoarrow.h" "$INCLUDE_DIR/nanoarrow/"

# Compile nanoarrow.c
"$CC" -c -O2 -fPIC $SANITIZE_FLAGS -I"$INCLUDE_DIR" -o "$BUILD_DIR/nanoarrow.o" "$BUILD_DIR/nanoarrow.c"

# Compile cppmorloc.cpp
"$CXX" -c --std=c++20 -O2 $SANITIZE_FLAGS -I"$INCLUDE_DIR" -o "$BUILD_DIR/cppmorloc.o" "$BUILD_DIR/cppmorloc.cpp"

# Archive into libcppmorloc.a
ar rcs "$LIB_DIR/libcppmorloc.a" "$BUILD_DIR/cppmorloc.o" "$BUILD_DIR/nanoarrow.o"

# Compile precompiled header
cp "$BUILD_DIR/morloc_pch.hpp" "$INCLUDE_DIR/"
"$CXX" --std=c++20 -O2 $SANITIZE_FLAGS -I"$INCLUDE_DIR" -x c++-header "$INCLUDE_DIR/morloc_pch.hpp" -o "$INCLUDE_DIR/morloc_pch.hpp.gch"
