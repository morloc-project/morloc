#!/bin/sh
# Build portable libmorloc.so, morloc-nexus, and morloc-manager.
#
# Usage:
#   ./container/static-build/build.sh
#
# Output:
#   ./out/libmorloc.so      (shared library, glibc >= 2.31)
#   ./out/morloc-nexus      (binary, glibc >= 2.31, links libmorloc.so)
#   ./out/morloc-manager    (static binary, runs on any Linux)

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

echo "Building libmorloc.so, morloc-nexus, and morloc-manager with $ENGINE..."

mkdir -p "$PROJECT_DIR/out"

$ENGINE build \
    -t morloc-rust-build \
    -f "$SCRIPT_DIR/Dockerfile" \
    "$PROJECT_DIR"

$ENGINE run --rm \
    -v "$PROJECT_DIR/out:/out" \
    morloc-rust-build

echo ""
echo "Binaries:"
ls -lh "$PROJECT_DIR/out/libmorloc.so" "$PROJECT_DIR/out/morloc-nexus" "$PROJECT_DIR/out/morloc-manager"
file "$PROJECT_DIR/out/libmorloc.so" "$PROJECT_DIR/out/morloc-nexus" "$PROJECT_DIR/out/morloc-manager"
