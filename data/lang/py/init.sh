#!/bin/bash
set -e

export MORLOC_HOME="$1"
BUILD_DIR="$2"
SANITIZE_FLAGS="$3"
OPT_DIR="$MORLOC_HOME/opt"

# Clean stale build artifacts
rm -f "$OPT_DIR"/pymorloc.cpython* "$OPT_DIR/pymorloc"
rm -rf "$OPT_DIR/build"

# Copy files to opt dir
cp "$BUILD_DIR/pymorloc.c" "$OPT_DIR/"
cp "$BUILD_DIR/setup.py" "$OPT_DIR/"
cp "$BUILD_DIR/Makefile" "$OPT_DIR/"

# Build pymorloc extension
export CFLAGS="$SANITIZE_FLAGS"
make -C "$OPT_DIR" -f Makefile
