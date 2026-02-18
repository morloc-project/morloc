#!/bin/bash
set -e

MORLOC_HOME="$1"
BUILD_DIR="$2"
OPT_DIR="$MORLOC_HOME/opt"

# Copy files to opt dir
cp "$BUILD_DIR/pymorloc.c" "$OPT_DIR/"
cp "$BUILD_DIR/setup.py" "$OPT_DIR/"
cp "$BUILD_DIR/Makefile" "$OPT_DIR/"

# Build pymorloc extension
make -C "$OPT_DIR" -f Makefile
