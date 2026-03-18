#!/bin/bash
set -e

MORLOC_HOME="$1"
BUILD_DIR="$2"
SANITIZE_FLAGS="$3"
INCLUDE_DIR="$MORLOC_HOME/include"
LIB_DIR="$MORLOC_HOME/lib"

# Install mlccpptypes if not present
if [ ! -d "$INCLUDE_DIR/mlccpptypes" ]; then
    git clone https://github.com/morloclib/mlccpptypes "$INCLUDE_DIR/mlccpptypes"
fi

# Install headers
cp "$BUILD_DIR/cppmorloc.hpp" "$INCLUDE_DIR/"
cp "$BUILD_DIR/mlc_arrow.hpp" "$INCLUDE_DIR/"
cp "$BUILD_DIR/mlc_tensor.hpp" "$INCLUDE_DIR/"

# Install nanoarrow headers
mkdir -p "$INCLUDE_DIR/nanoarrow"
cp "$BUILD_DIR/nanoarrow.h" "$INCLUDE_DIR/nanoarrow/"

# Compile nanoarrow.c
gcc -c -O2 -fPIC $SANITIZE_FLAGS -I"$INCLUDE_DIR" -o "$BUILD_DIR/nanoarrow.o" "$BUILD_DIR/nanoarrow.c"

# Compile cppmorloc.cpp
g++ -c --std=c++17 -O2 $SANITIZE_FLAGS -I"$INCLUDE_DIR" -o "$BUILD_DIR/cppmorloc.o" "$BUILD_DIR/cppmorloc.cpp"

# Archive into libcppmorloc.a
ar rcs "$LIB_DIR/libcppmorloc.a" "$BUILD_DIR/cppmorloc.o" "$BUILD_DIR/nanoarrow.o"

# Compile precompiled header
cp "$BUILD_DIR/morloc_pch.hpp" "$INCLUDE_DIR/"
g++ --std=c++17 -O2 $SANITIZE_FLAGS -I"$INCLUDE_DIR" -x c++-header "$INCLUDE_DIR/morloc_pch.hpp" -o "$INCLUDE_DIR/morloc_pch.hpp.gch"
