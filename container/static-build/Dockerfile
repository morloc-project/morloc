# syntax=docker/dockerfile:1.7
# Portable build of libmorloc.so, morloc-nexus, and morloc-manager.
#
# Strategy:
#   - libmorloc.so: shared library built on Ubuntu 20.04 (glibc >= 2.31)
#   - morloc-nexus: dynamically linked to libmorloc.so (glibc >= 2.31)
#   - morloc-manager: fully static binary (Alpine/musl, runs on any Linux)
#
# Build:
#   DOCKER_BUILDKIT=1 docker build -t morloc-rust-build -f container/static-build/Dockerfile .
#   docker run --rm -v $(pwd)/out:/out morloc-rust-build
#
# Output:
#   ./out/libmorloc.so      (shared library, glibc >= 2.31)
#   ./out/morloc-nexus      (binary, glibc >= 2.31, links libmorloc.so)
#   ./out/morloc-manager    (static binary, runs on any Linux including NixOS)
#
# Caching:
#   BuildKit cache mounts persist cargo's registry/git/target dirs across
#   builds on the host. Subsequent builds do true incremental compilation.
#   To wipe the cache: `docker buildx prune` or `podman system prune`.

# ===========================================================================
# Stage 1: Build libmorloc.so + morloc-nexus on Ubuntu (glibc)
#
# morloc-nexus dynamically links libmorloc.so at runtime, so both must be
# built against the same libc. Ubuntu 20.04 gives glibc 2.31 forward compat.
# ===========================================================================
FROM docker.io/library/ubuntu:20.04 AS glibc-builder

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl ca-certificates gcc g++ make pkg-config \
    && rm -rf /var/lib/apt/lists/*

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --default-toolchain stable \
    && /root/.cargo/bin/cargo --version
ENV PATH="/root/.cargo/bin:${PATH}"

WORKDIR /build

COPY data/rust/ ./

# Single RUN: cache-mounted target/ is not visible to later layers, so we
# build morloc-runtime, link libmorloc.so via --whole-archive (exports ALL
# symbols so language extensions can see internal Rust runtime state),
# build morloc-nexus, then stash the artifacts in /artifacts/ for the
# final stage to pick up.
RUN --mount=type=cache,id=morloc-cargo-registry,target=/root/.cargo/registry,sharing=locked \
    --mount=type=cache,id=morloc-cargo-git,target=/root/.cargo/git,sharing=locked \
    --mount=type=cache,id=morloc-target-glibc,target=/build/target,sharing=locked \
    cargo build --release -p morloc-runtime && \
    mkdir -p /root/.local/share/morloc/lib /artifacts && \
    gcc -shared -o /root/.local/share/morloc/lib/libmorloc.so \
       -Wl,--whole-archive target/release/libmorloc_runtime.a -Wl,--no-whole-archive \
       -lpthread -lrt -ldl -lm && \
    cargo build --release -p morloc-nexus && \
    cp /root/.local/share/morloc/lib/libmorloc.so /artifacts/libmorloc.so && \
    cp target/release/morloc-nexus /artifacts/morloc-nexus && \
    strip /artifacts/libmorloc.so /artifacts/morloc-nexus

# ===========================================================================
# Stage 2: Build morloc-manager on Alpine (musl, fully static)
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
# Stage: Build the morloc compiler (Haskell/stack) on the SAME glibc floor
# (Ubuntu 20.04, glibc >= 2.31) as libmorloc.so / morloc-nexus.
#
# The compiler, libmorloc.so, and nexus form one dynamic trio provisioned
# together into a native environment; building them all at glibc 2.31 keeps
# them mutually loadable and forward-compatible on any newer host. (morloc
# is a conventional dynamic-glibc Haskell binary -- its only extra runtime
# .so deps are libgmp + libz, satisfied by the host or the pixi env.)
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

# Whole repo (the compiler needs its full source, not just data/rust).
COPY . /morloc
# Cache-mount stack's GHC bindist + compiled snapshot (~/.stack) and the project
# build (.stack-work), like the cargo stages above. `COPY . /morloc` invalidates
# this layer on ANY source change, so without these mounts every build re-ran
# `stack install` from scratch -- re-downloading GHC and recompiling all deps.
# With them, only the changed project modules recompile. The morloc binary is
# copied OUT to /artifacts in the SAME RUN (cache mounts are not visible to later
# layers), so the artifact still persists into the image.
RUN --mount=type=cache,id=morloc-stack-root,target=/root/.stack,sharing=locked \
    --mount=type=cache,id=morloc-stack-work,target=/morloc/.stack-work,sharing=locked \
    cd /morloc && stack install --no-run-tests \
    && mkdir -p /artifacts \
    && cp /root/.local/bin/morloc /artifacts/morloc \
    && strip /artifacts/morloc

# ===========================================================================
# Stage 3: Verify and output
# ===========================================================================
FROM docker.io/library/alpine:3.20

RUN apk add --no-cache file

COPY --from=glibc-builder /artifacts/libmorloc.so /out-staging/libmorloc.so
COPY --from=glibc-builder /artifacts/morloc-nexus /out-staging/morloc-nexus
COPY --from=musl-builder /artifacts/morloc-manager /out-staging/morloc-manager
COPY --from=compiler-builder /artifacts/morloc /out-staging/morloc

# Bundle the Rust crate sources (minus build artifacts). rustmorloc is an rlib
# locked to the consumer's rustc, so it can't be prebuilt like the binaries
# above; `morloc init` compiles it locally from these sources (resolved as
# $MORLOC_RUST_BIN/rust).
COPY data/rust /out-staging/rust
RUN rm -rf /out-staging/rust/target

# Verify morloc-manager is static (no dynamic linker needed)
RUN file /out-staging/morloc-manager \
    && ! ldd /out-staging/morloc-manager 2>&1 | grep -q "=>" \
    && echo "OK: morloc-manager has no dynamic dependencies"

CMD ["sh", "-c", "cp /out-staging/libmorloc.so /out-staging/morloc-nexus /out-staging/morloc-manager /out-staging/morloc /out/ && cp -r /out-staging/rust /out/rust && echo 'Wrote morloc, libmorloc.so, morloc-nexus, morloc-manager, rust/' && ls -lh /out/morloc /out/libmorloc.so /out/morloc-nexus /out/morloc-manager && echo && file /out/morloc-manager"]
