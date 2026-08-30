#!/bin/bash
set -e

MORLOC_HOME="$1"
BUILD_DIR="$2"
SANITIZE_FLAGS="$3"
INCLUDE_DIR="$MORLOC_HOME/include"
LIB_DIR="$MORLOC_HOME/lib"

# Write source to include dir (R CMD SHLIB expects it there)
cp "$BUILD_DIR/rmorloc.c" "$INCLUDE_DIR/"

# Compile directly with gcc so SANITIZE_FLAGS can be passed at both compile
# and link time. R CMD SHLIB drops sanitizer flags from the link step, which
# would leave librmorloc.so with unresolved ubsan symbols (R itself is not
# built with ubsan, so dlopen would fail with:
#   "undefined symbol: __ubsan_handle_type_mismatch_v1_abort").
#
# We only link against R's core shared lib (libR). R's full --ldflags include
# libs the R interpreter uses (pcre2, tirpc, icu...) that may not be present
# as separate dev packages on the build machine; R-loadable shared objects
# resolve those symbols through the loaded R interpreter at dlopen time, not
# at link time.
R_CPPFLAGS=$(R CMD config --cppflags)
R_HOME=$(R RHOME)
${CC:-gcc} $R_CPPFLAGS -I"$INCLUDE_DIR" $SANITIZE_FLAGS -fpic -O2 \
    -c "$INCLUDE_DIR/rmorloc.c" -o "$INCLUDE_DIR/rmorloc.o"

# Platform-specific link flags. The R-loadable object keeps the `.so` NAME on
# macOS by convention (R strips lib.../.so to find R_init_rmorloc), but is
# LINKED as a Mach-O dynamic library that defers undefined symbols (R API +
# transitive R libs it doesn't link directly) to dlopen time -- mirroring what
# `R CMD SHLIB` emits on macOS. GNU-ld hardening flags don't exist on ld64,
# and the loader-origin token differs ($ORIGIN vs @loader_path).
if [ "$(uname -s)" = "Darwin" ]; then
    SHLIB_FLAGS="-dynamiclib -undefined dynamic_lookup"
    RPATH_ORIGIN="@loader_path"
else
    SHLIB_FLAGS="-shared -Wl,-Bsymbolic-functions -Wl,-z,relro"
    RPATH_ORIGIN='$ORIGIN'
fi
${CC:-gcc} $SHLIB_FLAGS $SANITIZE_FLAGS \
    -o "$LIB_DIR/librmorloc.so" "$INCLUDE_DIR/rmorloc.o" \
    -L"$LIB_DIR" -Wl,-rpath,"$LIB_DIR" -Wl,-rpath,"$RPATH_ORIGIN" -lmorloc -lpthread \
    -L"$R_HOME/lib" -lR

# Clean up
rm -f "$INCLUDE_DIR/rmorloc.c" "$INCLUDE_DIR/rmorloc.o"
