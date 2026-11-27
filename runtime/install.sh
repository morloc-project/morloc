#!/usr/bin/env bash
# install.sh -- build the morloc runtime from this directory and install it.
#
# Usage: bash install.sh PREFIX TARGET_DIR
#
# Builds libmorloc and morloc-nexus with cargo into TARGET_DIR, then installs
# PREFIX/lib/libmorloc.{so,dylib}, PREFIX/bin/morloc-nexus and
# PREFIX/include/morloc.h, all from this source, and records the build id of
# the pair in PREFIX/lib/morloc.build-id. They are built and checked in
# PREFIX/.runtime-staging, then renamed into place; the runtime-install suite
# (test-suite/integration/suites/runtime-install) checks what that guarantees.
#
# Honors $CC (the linker on Linux, default gcc), $STRIP (default strip on
# PATH, skipped when absent) and MORLOC_INIT_VERBOSE=1 (echo each command).

set -euo pipefail

if [[ $# -ne 2 ]]; then
    echo "usage: bash install.sh PREFIX TARGET_DIR" >&2
    exit 2
fi

prefix="$1"
target="$2"
src="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
stage="$prefix/.runtime-staging"

die() {
    echo "morloc runtime install failed: $*" >&2
    exit 1
}

step() {
    local what="$1"
    shift
    if [[ "${MORLOC_INIT_VERBOSE:-}" == 1 ]]; then
        echo "$*" >&2
    fi
    "$@" || die "$what"
}

: "${CC:=gcc}"
strip_tool="${STRIP:-}"
if [[ -z "$strip_tool" ]]; then
    strip_tool="$(command -v strip || true)"
fi

case "$(uname -s)" in
    Darwin) os=darwin lib=libmorloc.dylib ;;
    *) os=linux lib=libmorloc.so ;;
esac

rm -rf "$stage"
trap 'rm -rf "$stage"' EXIT
mkdir -p "$stage/lib" "$stage/bin" "$stage/include" "$prefix/lib" "$prefix/bin" "$prefix/include" ||
    die "creating $stage"

step "copying morloc.h" cp "$src/include/morloc.h" "$stage/include/morloc.h"

step "building libmorloc (cargo -p morloc-runtime)" \
    cargo build --release --manifest-path "$src/Cargo.toml" -p morloc-runtime --target-dir "$target"

# Linux links the staticlib whole so that every symbol, the runtime's
# internal state included, is exported to the language bindings; the cdylib
# would need a version script, which conflicts with Rust's own on aarch64.
# macOS uses the cdylib, whose install_name morloc-runtime/build.rs sets, and
# is never stripped: a bare strip can drop the symbols the bindings resolve.
if [[ "$os" == darwin ]]; then
    step "copying libmorloc" cp "$target/release/libmorloc_runtime.dylib" "$stage/lib/$lib"
else
    step "linking libmorloc" \
        "$CC" -shared -o "$stage/lib/$lib" \
        -Wl,--whole-archive "$target/release/libmorloc_runtime.a" -Wl,--no-whole-archive \
        -lpthread -lrt -ldl -lm
    if [[ -n "$strip_tool" ]]; then
        step "stripping libmorloc" "$strip_tool" "$stage/lib/$lib"
    fi
fi

# The nexus build links against the staged libmorloc (its build.rs reads
# MORLOC_HOME), so the nexus and libmorloc it is checked against below are
# the pair that gets installed.
step "building the nexus (cargo -p morloc-nexus)" \
    env MORLOC_HOME="$stage" \
    cargo build --release --manifest-path "$src/Cargo.toml" -p morloc-nexus --target-dir "$target"
step "copying the nexus" cp "$target/release/morloc-nexus" "$stage/bin/morloc-nexus"
# Apple Silicon kills an executable whose signature a strip broke.
if [[ "$os" == linux && -n "$strip_tool" ]]; then
    step "stripping the nexus" "$strip_tool" "$stage/bin/morloc-nexus"
fi

# LIFE-6: the nexus refuses a libmorloc of another build. The library search
# variables are cleared so that the staged nexus loads the staged libmorloc
# through its rpath.
if ! build_id="$(env -u LD_LIBRARY_PATH -u DYLD_LIBRARY_PATH "$stage/bin/morloc-nexus" --build-id 2> "$stage/build-id.err")"; then
    id_err="$(cat "$stage/build-id.err")"
    die "the morloc-nexus and libmorloc just built come from different builds (the source or the shared cargo target changed during the build); run the install again (\`morloc init -f\`): $id_err"
fi

# An install that stops part way leaves no recorded build id.
step "clearing the recorded build id" rm -f "$prefix/lib/morloc.build-id"
step "installing libmorloc" mv -f "$stage/lib/$lib" "$prefix/lib/$lib"
step "installing the nexus" mv -f "$stage/bin/morloc-nexus" "$prefix/bin/morloc-nexus"
step "installing morloc.h" mv -f "$stage/include/morloc.h" "$prefix/include/morloc.h"
echo "$build_id" > "$stage/lib/morloc.build-id" || die "recording the build id"
step "recording the build id" mv -f "$stage/lib/morloc.build-id" "$prefix/lib/morloc.build-id"
