# syntax=docker/dockerfile:1.7
# Portable build of the morloc release trio: the morloc compiler, the static
# morloc-manager, and the Rust workspace SOURCE.
#
# libmorloc.so and morloc-nexus are NOT prebuilt here. `morloc init` builds them
# from the bundled Rust source with the env's own toolchain, so the runtime is
# ABI-coherent with the pools it links (the prebuilt-vs-conda glibc mismatch is
# gone). Only the two things that cannot be built from source at init time ship
# prebuilt: the GHC-compiled compiler and the static bootstrap manager.
#
# Strategy:
#   - morloc          : Haskell compiler, Ubuntu 20.04 (glibc >= 2.31)
#   - morloc-manager  : fully static binary (Alpine/musl, runs on any Linux)
#   - rust/           : the Rust workspace source (platform-independent)
#
# Build:
#   DOCKER_BUILDKIT=1 docker build -t morloc-rust-build -f container/static-build/Dockerfile .
#   docker run --rm -v $(pwd)/out:/out morloc-rust-build
#
# Output:
#   ./out/morloc            (compiler, glibc >= 2.31)
#   ./out/morloc-manager    (static binary, runs on any Linux including NixOS)
#   ./out/rust/             (Rust workspace source)
#
# Caching:
#   BuildKit cache mounts persist cargo's registry/git/target dirs across
#   builds on the host. Subsequent builds do true incremental compilation.
#   To wipe the cache: `docker buildx prune` or `podman system prune`.

# ===========================================================================
# Stage 1: Build morloc-manager on Alpine (musl, fully static)
#
# morloc-manager has no native dependencies - pure Rust crates only.
# Building on Alpine produces a fully static musl binary that runs on any
# Linux, including NixOS and minimal containers.
# ===========================================================================
FROM docker.io/library/alpine:3.20 AS musl-builder

RUN apk add --no-cache curl gcc musl-dev

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --default-toolchain stable \
    && /root/.cargo/bin/cargo --version
ENV PATH="/root/.cargo/bin:${PATH}"

WORKDIR /build

COPY data/rust/ ./

RUN --mount=type=cache,id=morloc-cargo-registry,target=/root/.cargo/registry,sharing=locked \
    --mount=type=cache,id=morloc-cargo-git,target=/root/.cargo/git,sharing=locked \
    --mount=type=cache,id=morloc-target-musl,target=/build/target,sharing=locked \
    cargo build --release -p morloc-manager && \
    mkdir -p /artifacts && \
    cp target/release/morloc-manager /artifacts/morloc-manager && \
    strip /artifacts/morloc-manager

# ===========================================================================
# Stage 2: Build the morloc compiler (Haskell/stack) on Ubuntu 20.04
# (glibc >= 2.31).
#
# The compiler ships prebuilt (GHC cannot be rebuilt from source at init).
# Building at glibc 2.31 keeps it forward-compatible on any newer host. (morloc
# is a conventional dynamic-glibc Haskell binary -- its only extra runtime .so
# deps are libgmp + libz, satisfied by the host or the pixi env.)
#
# NOTE: the container image (container/tiny) also stack-builds the compiler;
# consolidating the two builds is a future CI optimization.
# ===========================================================================
FROM docker.io/library/ubuntu:20.04 AS compiler-builder

ENV DEBIAN_FRONTEND=noninteractive

# Do NOT delete /var/lib/apt/lists here: the stack installer below runs its own
# `apt-get install` for its build deps (gcc, make, xz-utils, gnupg, libgmp-dev,
# ...) and needs the package lists present (this is why container/tiny keeps
# them). This is a build-only stage, so image-size hygiene does not matter.
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
# Stage 3: Collect and output
# ===========================================================================
FROM docker.io/library/alpine:3.20

RUN apk add --no-cache file

COPY --from=musl-builder /artifacts/morloc-manager /out-staging/morloc-manager
COPY --from=compiler-builder /artifacts/morloc /out-staging/morloc

# The Rust workspace source (minus build artifacts). `morloc init` compiles
# libmorloc.so, morloc-nexus, and the rustmorloc rlib locally from these
# sources (resolved as $MORLOC_RUST_DIR).
COPY data/rust /out-staging/rust
RUN rm -rf /out-staging/rust/target

# Verify morloc-manager is static (no dynamic linker needed)
RUN file /out-staging/morloc-manager \
    && ! ldd /out-staging/morloc-manager 2>&1 | grep -q "=>" \
    && echo "OK: morloc-manager has no dynamic dependencies"

CMD ["sh", "-c", "cp /out-staging/morloc-manager /out-staging/morloc /out/ && cp -r /out-staging/rust /out/rust && echo 'Wrote morloc, morloc-manager, rust/' && ls -lh /out/morloc /out/morloc-manager && echo && file /out/morloc-manager"]
