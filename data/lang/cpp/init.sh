#!/bin/bash
set -e

MORLOC_HOME="$1"
BUILD_DIR="$2"
INCLUDE_DIR="$MORLOC_HOME/include"
LIB_DIR="$MORLOC_HOME/lib"

# Install mlccpptypes if not present
if [ ! -d "$INCLUDE_DIR/mlccpptypes" ]; then
    git clone https://github.com/morloclib/mlccpptypes "$INCLUDE_DIR/mlccpptypes"
fi

# Install header
cp "$BUILD_DIR/cppmorloc.hpp" "$INCLUDE_DIR/"

# Compile cppmorloc.cpp -> libcppmorloc.a
g++ -c --std=c++17 -O2 -I"$INCLUDE_DIR" -o "$BUILD_DIR/cppmorloc.o" "$BUILD_DIR/cppmorloc.cpp"
ar rcs "$LIB_DIR/libcppmorloc.a" "$BUILD_DIR/cppmorloc.o"

# Compile precompiled header
cp "$BUILD_DIR/morloc_pch.hpp" "$INCLUDE_DIR/"
g++ --std=c++17 -O2 -I"$INCLUDE_DIR" -x c++-header "$INCLUDE_DIR/morloc_pch.hpp" -o "$INCLUDE_DIR/morloc_pch.hpp.gch"
