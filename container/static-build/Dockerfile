# syntax=docker/dockerfile:1.7
# Portable build of the prebuilt morloc release assets: the morloc compiler and
# the Rust workspace SOURCE.
#
# libmorloc.so and morloc-nexus are NOT prebuilt here. `morloc init` builds them
# from the bundled Rust source with the env's own toolchain, so the runtime is
# ABI-coherent with the pools it links. Only the GHC-compiled compiler, which
# cannot be built from source at init time, ships prebuilt. The mim environment
# manager is built and released from its own repository
# (morloc-project/morloc-manager).
#
# Strategy:
#   - morloc : Haskell compiler, Ubuntu 20.04 (glibc >= 2.31)
#   - rust/  : the Rust workspace source (platform-independent)
#
# Build:
#   DOCKER_BUILDKIT=1 docker build -t morloc-rust-build -f container/static-build/Dockerfile .
#   docker run --rm -v $(pwd)/out:/out morloc-rust-build
#
# Output:
#   ./out/morloc  (compiler, glibc >= 2.31)
#   ./out/rust/   (Rust workspace source)

# ===========================================================================
# Stage 1: Build the morloc compiler (Haskell/stack) on Ubuntu 20.04
# (glibc >= 2.31).
#
# The compiler ships prebuilt (GHC cannot be rebuilt from source at init).
# Building at glibc 2.31 keeps it forward-compatible on any newer host. (morloc
# is a conventional dynamic-glibc Haskell binary -- its only extra runtime .so
# deps are libgmp + libz, satisfied by the host or the pixi env.)
# ===========================================================================
FROM docker.io/library/ubuntu:20.04 AS compiler-builder

ENV DEBIAN_FRONTEND=noninteractive

# Do NOT delete /var/lib/apt/lists here: the stack installer below runs its own
# `apt-get install` for its build deps (gcc, make, xz-utils, gnupg, libgmp-dev,
# ...) and needs the package lists present. This is a build-only stage, so
# image-size hygiene does not matter.
RUN apt-get update && apt-get install -y --no-install-recommends \
    git curl ca-certificates pkg-config libglib2.0-dev

# Install stack, which downloads its own GHC bindist (built against an old
# glibc), so the compiler's floor is set by this 20.04 base, not the host.
# -L is required: get.haskellstack.org redirects to the actual install script.
RUN curl --proto '=https' --tlsv1.2 -sSfL https://get.haskellstack.org/ | sh

WORKDIR /morloc

# --- Dependency layer: GHC + snapshot land in an image LAYER keyed on the
# dependency-determining files, so the GHA layer cache can persist it (a
# --mount=type=cache would not survive an ephemeral runner). ---
COPY stack.yaml stack.yaml.lock package.yaml ./
RUN stack build --no-run-tests --only-dependencies

# --- Source layer: only the project's own modules recompile per commit ---
COPY . .
RUN stack install --no-run-tests \
    && mkdir -p /artifacts \
    && cp /root/.local/bin/morloc /artifacts/morloc \
    && strip /artifacts/morloc

# ===========================================================================
# Stage 2: Collect and output
# ===========================================================================
FROM docker.io/library/ubuntu:20.04

COPY --from=compiler-builder /artifacts/morloc /out-staging/morloc

# The Rust workspace source (minus build artifacts). `morloc init` compiles
# libmorloc.so, morloc-nexus, and the rustmorloc rlib locally from these
# sources (resolved as $MORLOC_RUST_DIR).
COPY data/rust /out-staging/rust
RUN rm -rf /out-staging/rust/target

CMD ["sh", "-c", "cp /out-staging/morloc /out/ && cp -r /out-staging/rust /out/rust && echo 'Wrote morloc, rust/' && ls -lh /out/morloc"]
