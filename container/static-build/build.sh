#!/bin/sh
# Build the portable morloc release assets: the compiler, the static
# morloc-manager, and the Rust workspace source (libmorloc.so + morloc-nexus are
# built from that source by `morloc init`, not prebuilt here).
#
# Usage:
#   ./container/static-build/build.sh
#
# Output:
#   ./out/morloc            (compiler, glibc >= 2.31)
#   ./out/morloc-manager    (static binary, runs on any Linux)
#   ./out/rust/             (Rust workspace source)

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

echo "Building the morloc compiler, morloc-manager, and rust source with $ENGINE..."

mkdir -p "$PROJECT_DIR/out"

# BuildKit is required for the Dockerfile's cache mounts (cargo registry +
# target dir persisted across builds). Podman uses buildah, which supports
# cache mounts natively; docker needs DOCKER_BUILDKIT=1.
DOCKER_BUILDKIT=1 $ENGINE build \
    -t morloc-rust-build \
    -f "$SCRIPT_DIR/Dockerfile" \
    "$PROJECT_DIR"

$ENGINE run --rm \
    -v "$PROJECT_DIR/out:/out" \
    morloc-rust-build

echo ""
echo "Assets:"
ls -lh "$PROJECT_DIR/out/morloc" "$PROJECT_DIR/out/morloc-manager"
file "$PROJECT_DIR/out/morloc" "$PROJECT_DIR/out/morloc-manager"
