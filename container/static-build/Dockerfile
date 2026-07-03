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
# Stage 3: Verify and output
# ===========================================================================
FROM docker.io/library/alpine:3.20

RUN apk add --no-cache file

COPY --from=glibc-builder /artifacts/libmorloc.so /out-staging/libmorloc.so
COPY --from=glibc-builder /artifacts/morloc-nexus /out-staging/morloc-nexus
COPY --from=musl-builder /artifacts/morloc-manager /out-staging/morloc-manager

# Verify morloc-manager is static (no dynamic linker needed)
RUN file /out-staging/morloc-manager \
    && ! ldd /out-staging/morloc-manager 2>&1 | grep -q "=>" \
    && echo "OK: morloc-manager has no dynamic dependencies"

CMD ["sh", "-c", "cp /out-staging/libmorloc.so /out-staging/morloc-nexus /out-staging/morloc-manager /out/ && echo 'Wrote libmorloc.so, morloc-nexus, morloc-manager' && ls -lh /out/libmorloc.so /out/morloc-nexus /out/morloc-manager && echo && file /out/morloc-manager"]
