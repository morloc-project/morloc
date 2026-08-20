#!/usr/bin/env bash
# Build the portable morloc release assets and/or container images locally.
#
# The "rust" command produces the same assets the release pipeline ships: the
# morloc compiler (glibc >= 2.31 floor), the static morloc-manager (musl), and
# the Rust workspace source. libmorloc.so + morloc-nexus are NOT built here --
# `morloc init` builds them from the bundled source with the target toolchain.
#
# Usage:
#   ./scripts/build-rust.sh rust       Build the release assets to out/
#   ./scripts/build-rust.sh manager    Build ONLY the static morloc-manager to out/
#   ./scripts/build-rust.sh tiny       Build morloc-tiny container
#   ./scripts/build-rust.sh full       Build morloc-full container (requires tiny)
#   ./scripts/build-rust.sh all        Build assets + both containers
#   ./scripts/build-rust.sh export     Export morloc-full image to tarball
#
# Environment:
#   MORLOC_VERSION   Container image tag (default: edge)
set -euo pipefail

cd "$(dirname "$0")/.."

MORLOC_VERSION="${MORLOC_VERSION:-edge}"

# Content fingerprint of the Rust source that enters the image (matching the
# .dockerignore target/ exclusion). Any edit -- committed or not -- changes it,
# which busts the manager build layer (see MANAGER_SRC_ID in the Dockerfile);
# an unchanged tree keeps the same id so the cache is reused. A bare git
# dirty-flag would not do: successive uncommitted edits share it, so the dev
# loop would keep serving a stale binary.
manager_src_id() {
    local hash
    if command -v sha256sum >/dev/null 2>&1; then hash=sha256sum
    elif command -v shasum >/dev/null 2>&1; then hash="shasum -a 256"
    else echo "manager_src_id: need sha256sum or shasum" >&2; return 1
    fi
    find data/rust -type f -not -path '*/target/*' -print0 \
        | LC_ALL=C sort -z \
        | xargs -0 cat \
        | $hash \
        | cut -d' ' -f1
}

cmd_rust() {
    local src_id
    src_id=$(manager_src_id)
    echo "=== Building portable release assets (compiler + static manager + rust source) ==="
    podman build --build-arg "MANAGER_SRC_ID=$src_id" \
        -t morloc-rust-build -f container/static-build/Dockerfile .
    mkdir -p out
    podman run --rm -v "$(pwd)/out:/out" morloc-rust-build
    echo "=== Output in out/ ==="
    ls -lh out/
}

# Build ONLY the static morloc-manager, using the exact stage the release
# pipeline ships (the Alpine/musl `musl-builder` in static-build/Dockerfile).
# This is the byte-for-byte production toolchain -- same base image, same musl
# static link -- so the binary you test here is the one users get, without the
# heavy compiler/GHC build that `rust` also does. Output: out/morloc-manager.
cmd_manager() {
    local src_id
    src_id=$(manager_src_id)
    echo "=== Building static morloc-manager + morloc-env (musl, production toolchain) ==="
    podman build --target musl-builder --build-arg "MANAGER_SRC_ID=$src_id" \
        -t morloc-manager-musl -f container/static-build/Dockerfile .
    mkdir -p out
    # The stage leaves the stripped binaries at /artifacts; copy just those out
    # of the image (no full-rootfs export). morloc-env is the in-env dependency
    # agent, shipped alongside the manager.
    local cid
    cid=$(podman create morloc-manager-musl)
    podman cp "$cid:/artifacts/morloc-manager" out/morloc-manager
    podman cp "$cid:/artifacts/morloc-env" out/morloc-env
    podman rm "$cid" >/dev/null
    echo "=== out/morloc-manager + out/morloc-env ==="
    # Confirm it is fully static (matching production), when `file` is available.
    if command -v file >/dev/null 2>&1; then
        file out/morloc-manager
        if file out/morloc-manager | grep -qE "statically linked|static-pie"; then
            echo "OK: morloc-manager is fully static"
        else
            echo "WARNING: morloc-manager is not statically linked (expected static musl)" >&2
        fi
    fi
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
    echo "  rust     Build release assets (morloc compiler, static morloc-manager, rust source)"
    echo "  manager  Build ONLY the static morloc-manager (production musl toolchain) to out/"
    echo "  tiny     Build morloc-tiny container"
    echo "  full     Build morloc-full container (requires tiny)"
    echo "  all      Build everything (binaries + containers)"
    echo "  export   Export morloc-full image to /tmp/ tarball"
    echo ""
    echo "Environment:"
    echo "  MORLOC_VERSION=edge  (default)"
}

case "${1:-}" in
    rust)    cmd_rust ;;
    manager) cmd_manager ;;
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
