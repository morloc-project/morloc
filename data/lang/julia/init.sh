#!/bin/bash
set -e

MORLOC_HOME="$1"
BUILD_DIR="$2"
SANITIZE_FLAGS="$3"
INCLUDE_DIR="$MORLOC_HOME/include"
LIB_DIR="$MORLOC_HOME/lib"
LANG_DIR="$MORLOC_HOME/lang/julia"

# Allow overriding the C compiler via env var (honors a conda toolchain's $CC).
: "${CC:=gcc}"

mkdir -p "$LANG_DIR"

# Install language descriptor and runtime files
cp "$BUILD_DIR/lang.yaml" "$LANG_DIR/"
cp "$BUILD_DIR/pool.jl" "$LANG_DIR/"
cp "$BUILD_DIR/MorlocRuntime.jl" "$LANG_DIR/"

# Compile juliabridge.c -> libjuliamorloc.so
"$CC" -shared -fPIC -O2 $SANITIZE_FLAGS -I"$INCLUDE_DIR" -o "$LIB_DIR/libjuliamorloc.so" \
    "$BUILD_DIR/juliabridge.c" -L"$LIB_DIR" -Wl,-rpath,"$LIB_DIR" -Wl,-rpath,'$ORIGIN' -lmorloc -lpthread
