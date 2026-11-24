#!/bin/bash
set -e

MORLOC_HOME="$1"
BUILD_DIR="$2"
SANITIZE_FLAGS="$3"
INCLUDE_DIR="$MORLOC_HOME/include"
LIB_DIR="$MORLOC_HOME/lib"

# Allow overriding the C/C++ compilers and archiver via env vars. A conda
# toolchain ships a target-prefixed `ar` (morloc init exports $AR to it), so
# fall back to a bare `ar` only outside such an environment.
: "${CC:=gcc}"
: "${CXX:=g++}"
: "${AR:=ar}"

# Install mlccpptypes if not present
if [ ! -d "$INCLUDE_DIR/mlccpptypes" ]; then
    git clone https://github.com/morloclib/mlccpptypes "$INCLUDE_DIR/mlccpptypes"
fi

# Every installed file is written beside its destination and renamed onto it,
# so a concurrent build sees the old file or the new one, never a partial one.
# The include directory also holds morloc.h, mlccpptypes and module headers,
# so it is replaced file by file rather than as one directory.
STAGED=()
trap 'rm -f ${STAGED[@]+"${STAGED[@]}"}' EXIT
stage() {
    TMP="$(dirname "$1")/.$(basename "$1").tmp.$$"
    rm -f "$TMP"
    STAGED+=("$TMP")
}
install_file() {
    stage "$2"
    cp "$1" "$TMP"
    mv -f "$TMP" "$2"
}

# Install headers
install_file "$BUILD_DIR/cppmorloc.hpp" "$INCLUDE_DIR/cppmorloc.hpp"
install_file "$BUILD_DIR/cppmorloc_api.hpp" "$INCLUDE_DIR/cppmorloc_api.hpp"
install_file "$BUILD_DIR/mlc_arrow.hpp" "$INCLUDE_DIR/mlc_arrow.hpp"
install_file "$BUILD_DIR/mlc_rec.hpp" "$INCLUDE_DIR/mlc_rec.hpp"

# Install nanoarrow headers
mkdir -p "$INCLUDE_DIR/nanoarrow"
install_file "$BUILD_DIR/nanoarrow.h" "$INCLUDE_DIR/nanoarrow/nanoarrow.h"

# Compile nanoarrow.c
"$CC" -c -O2 -fPIC $SANITIZE_FLAGS -I"$INCLUDE_DIR" -o "$BUILD_DIR/nanoarrow.o" "$BUILD_DIR/nanoarrow.c"

# Compile cppmorloc.cpp
"$CXX" -c --std=c++20 -O2 $SANITIZE_FLAGS -I"$INCLUDE_DIR" -o "$BUILD_DIR/cppmorloc.o" "$BUILD_DIR/cppmorloc.cpp"

# Archive into libcppmorloc.a
stage "$LIB_DIR/libcppmorloc.a"
"$AR" rcs "$TMP" "$BUILD_DIR/cppmorloc.o" "$BUILD_DIR/nanoarrow.o"
mv -f "$TMP" "$LIB_DIR/libcppmorloc.a"

# Compile precompiled header
install_file "$BUILD_DIR/morloc_pch.hpp" "$INCLUDE_DIR/morloc_pch.hpp"
stage "$INCLUDE_DIR/morloc_pch.hpp.gch"
"$CXX" --std=c++20 -O2 $SANITIZE_FLAGS -I"$INCLUDE_DIR" -x c++-header "$INCLUDE_DIR/morloc_pch.hpp" -o "$TMP"
mv -f "$TMP" "$INCLUDE_DIR/morloc_pch.hpp.gch"
