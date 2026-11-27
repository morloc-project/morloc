#!/bin/bash
set -e

export MORLOC_HOME="$1"
BUILD_DIR="$2"
SANITIZE_FLAGS="$3"
OPT_DIR="$MORLOC_HOME/opt"

# LIFE-6: pymorloc records the build id of the libmorloc it is built against.
BUILD_ID=$("$MORLOC_HOME/bin/morloc-nexus" --build-id)

# Build in a staging directory and rename the extension into place, so a pool
# starting meanwhile imports the old extension or the new one, never none.
STAGE="$OPT_DIR/.pymorloc-build.$$"
trap 'rm -rf "$STAGE"' EXIT
rm -rf "$STAGE" "$OPT_DIR/build"
mkdir -p "$STAGE"
cp "$BUILD_DIR/pymorloc.c" "$BUILD_DIR/setup.py" "$BUILD_DIR/Makefile" "$STAGE/"

export CFLAGS="$SANITIZE_FLAGS -DMORLOC_BINDING_BUILD_ID=0x${BUILD_ID}ULL"
make -C "$STAGE" -f Makefile

BUILT=$(cd "$STAGE" && ls pymorloc.cpython*)
mv -f "$STAGE/$BUILT" "$OPT_DIR/$BUILT"
for f in "$OPT_DIR"/pymorloc.cpython*; do
    [ "$(basename "$f")" = "$BUILT" ] || rm -f "$f"
done
rm -f "$OPT_DIR/pymorloc"
cp "$STAGE/pymorloc.c" "$STAGE/setup.py" "$STAGE/Makefile" "$OPT_DIR/"
