#!/usr/bin/env bash
# Build the portable morloc release assets locally.
#
# Produces the same assets the release pipeline ships: the morloc compiler
# (glibc >= 2.31 floor) and the Rust workspace source. libmorloc.so +
# morloc-nexus are NOT built here -- `morloc init` builds them from the bundled
# source with the target toolchain. The mim environment manager is built and
# released from its own repository (morloc-project/morloc-manager).
#
# Usage:
#   ./scripts/build-rust.sh rust    Build the release assets to out/
set -euo pipefail

cd "$(dirname "$0")/.."

cmd_rust() {
    echo "=== Building portable release assets (compiler + rust source) ==="
    podman build -t morloc-rust-build -f container/static-build/Dockerfile .
    mkdir -p out
    podman run --rm -v "$(pwd)/out:/out" morloc-rust-build
    echo "=== Output in out/ ==="
    ls -lh out/
}

usage() {
    echo "Usage: $(basename "$0") rust"
    echo ""
    echo "Commands:"
    echo "  rust   Build release assets (morloc compiler, rust source) to out/"
}

case "${1:-}" in
    rust)    cmd_rust ;;
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
