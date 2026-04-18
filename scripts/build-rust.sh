#!/usr/bin/env bash
# Build static Rust binaries and/or container images locally.
#
# Usage:
#   ./scripts/build-rust.sh rust       Build static binaries to out/
#   ./scripts/build-rust.sh tiny       Build morloc-tiny container
#   ./scripts/build-rust.sh full       Build morloc-full container (requires tiny)
#   ./scripts/build-rust.sh all        Build binaries + both containers
#   ./scripts/build-rust.sh export     Export morloc-full image to tarball
#
# Environment:
#   MORLOC_VERSION   Container image tag (default: edge)
set -euo pipefail

cd "$(dirname "$0")/.."

MORLOC_VERSION="${MORLOC_VERSION:-edge}"

cmd_rust() {
    echo "=== Building static Rust binaries ==="
    podman build -t morloc-rust-build -f container/static-build/Dockerfile .
    mkdir -p out
    podman run --rm -v "$(pwd)/out:/out" morloc-rust-build
    echo "=== Output in out/ ==="
    ls -lh out/
}

cmd_tiny() {
    echo "=== Building morloc-tiny:${MORLOC_VERSION} ==="
    make -C container MORLOC_VERSION="$MORLOC_VERSION" build-tiny
}

cmd_full() {
    echo "=== Building morloc-full:${MORLOC_VERSION} ==="
    make -C container MORLOC_VERSION="$MORLOC_VERSION" build-full
}

cmd_all() {
    cmd_rust
    cmd_tiny
    cmd_full
}

cmd_export() {
    local tarball="/tmp/morloc-full-${MORLOC_VERSION}.tar"
    echo "=== Exporting morloc-full:${MORLOC_VERSION} to ${tarball} ==="
    podman save "ghcr.io/morloc-project/morloc/morloc-full:${MORLOC_VERSION}" -o "$tarball"
    ls -lh "$tarball"
}

usage() {
    echo "Usage: $(basename "$0") <command>"
    echo ""
    echo "Commands:"
    echo "  rust     Build static Rust binaries (morloc-manager, morloc-nexus, libmorloc.so)"
    echo "  tiny     Build morloc-tiny container"
    echo "  full     Build morloc-full container (requires tiny)"
    echo "  all      Build everything (binaries + containers)"
    echo "  export   Export morloc-full image to /tmp/ tarball"
    echo ""
    echo "Environment:"
    echo "  MORLOC_VERSION=edge  (default)"
}

case "${1:-}" in
    rust)   cmd_rust ;;
    tiny)   cmd_tiny ;;
    full)   cmd_full ;;
    all)    cmd_all ;;
    export) cmd_export ;;
    -h|--help|"")
        usage
        exit 0
        ;;
    *)
        echo "Unknown command: $1" >&2
        usage >&2
        exit 1
        ;;
esac
