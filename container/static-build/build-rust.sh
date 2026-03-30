#!/bin/sh
# Build portable libmorloc.so, morloc-nexus, and morloc-manager using Docker/Podman.
#
# Usage:
#   ./container/static-build/build-rust.sh
#
# Output:
#   ./out/libmorloc.so      (cdylib, musl-linked, no glibc dependency, ~2 MB)
#   ./out/morloc-nexus      (static binary, ~2 MB)
#   ./out/morloc-manager    (static binary, ~2 MB)
#
# The .so runs on any Linux x86_64 system. The binaries are fully static.

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"

# Detect container engine (prefer podman)
if command -v podman >/dev/null 2>&1; then
    ENGINE=podman
elif command -v docker >/dev/null 2>&1; then
    ENGINE=docker
else
    echo "Error: neither podman nor docker found" >&2
    exit 1
fi

echo "Building portable libmorloc.so, morloc-nexus, and morloc-manager with $ENGINE..."

mkdir -p "$PROJECT_DIR/out"

$ENGINE build \
    -t morloc-rust-static \
    -f "$SCRIPT_DIR/Dockerfile.rust" \
    "$PROJECT_DIR"

$ENGINE run --rm \
    -v "$PROJECT_DIR/out:/out" \
    morloc-rust-static

echo ""
echo "Binaries:"
ls -lh "$PROJECT_DIR/out/libmorloc.so" "$PROJECT_DIR/out/morloc-nexus" "$PROJECT_DIR/out/morloc-manager"
file "$PROJECT_DIR/out/libmorloc.so" "$PROJECT_DIR/out/morloc-nexus" "$PROJECT_DIR/out/morloc-manager"

# Verify no glibc dependency
if readelf -d "$PROJECT_DIR/out/libmorloc.so" 2>/dev/null | grep -qi glibc; then
    echo "WARNING: libmorloc.so has glibc dependency!"
else
    echo "OK: libmorloc.so has no glibc dependency"
fi
