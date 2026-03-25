#!/bin/sh
# Build a fully static morloc-manager binary using Docker/Podman + Alpine.
#
# Usage:
#   ./container/static-build/build.sh
#
# Output:
#   ./out/morloc-manager    (static ELF binary, ~25-40 MB)
#
# The binary runs on any Linux x86_64 system without dependencies.

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

echo "Building static morloc-manager with $ENGINE..."

mkdir -p "$PROJECT_DIR/out"

$ENGINE build \
    -t morloc-static-build \
    -f "$SCRIPT_DIR/Dockerfile" \
    "$PROJECT_DIR"

$ENGINE run --rm \
    -v "$PROJECT_DIR/out:/out" \
    morloc-static-build

echo ""
echo "Binary: $PROJECT_DIR/out/morloc-manager"
ls -lh "$PROJECT_DIR/out/morloc-manager"
file "$PROJECT_DIR/out/morloc-manager"
