#!/bin/bash
set -e

MORLOC_HOME="$1"
BUILD_DIR="$2"
INCLUDE_DIR="$MORLOC_HOME/include"
LIB_DIR="$MORLOC_HOME/lib"

# Write source to include dir (R CMD SHLIB expects it there)
cp "$BUILD_DIR/rmorloc.c" "$INCLUDE_DIR/"

# Compile
R CMD SHLIB "$INCLUDE_DIR/rmorloc.c" -o "$LIB_DIR/librmorloc.so" \
    -L"$LIB_DIR" -Wl,-rpath,"$LIB_DIR" -lmorloc -lpthread

# Clean up
rm -f "$INCLUDE_DIR/rmorloc.c" "$INCLUDE_DIR/librmorloc.o"
