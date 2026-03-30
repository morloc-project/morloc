# Portable build of libmorloc.so, morloc-nexus, and morloc-manager.
#
# Strategy:
#   - libmorloc.so: shared library built on Ubuntu 20.04 (glibc >= 2.31)
#   - morloc-nexus: dynamically linked to libmorloc.so (glibc >= 2.31)
#   - morloc-manager: fully static binary (Alpine/musl, runs on any Linux)
#
# Build:
#   docker build -t morloc-rust-build -f container/static-build/Dockerfile .
#   docker run --rm -v $(pwd)/out:/out morloc-rust-build
#
# Output:
#   ./out/libmorloc.so      (shared library, glibc >= 2.31)
#   ./out/morloc-nexus      (binary, glibc >= 2.31, links libmorloc.so)
#   ./out/morloc-manager    (static binary, runs on any Linux including NixOS)

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

# Copy Cargo manifests first for dependency caching
COPY data/rust/Cargo.toml data/rust/Cargo.lock ./
COPY data/rust/morloc-runtime/Cargo.toml ./morloc-runtime/
COPY data/rust/morloc-nexus/Cargo.toml ./morloc-nexus/
COPY data/rust/morloc-manager/Cargo.toml ./morloc-manager/

# Create dummy source files for dependency pre-build
RUN mkdir -p morloc-runtime/src morloc-nexus/src morloc-manager/src \
    && echo "fn main() {}" > morloc-nexus/src/main.rs \
    && echo "fn main() {}" > morloc-manager/src/main.rs \
    && echo "" > morloc-runtime/src/lib.rs

RUN cargo build --release -p morloc-runtime 2>/dev/null || true

# Copy actual source
COPY data/rust/ ./

# Force rebuild after copying real source over dummy stubs
RUN touch morloc-runtime/src/lib.rs morloc-nexus/src/main.rs

# Build libmorloc.so from staticlib via gcc --whole-archive to export ALL
# symbols (internal Rust runtime state must be visible to language extensions)
RUN cargo build --release -p morloc-runtime

RUN mkdir -p /root/.local/share/morloc/lib \
    && gcc -shared -o /root/.local/share/morloc/lib/libmorloc.so \
       -Wl,--whole-archive target/release/libmorloc_runtime.a -Wl,--no-whole-archive \
       -lpthread -lrt -ldl -lm

# Build morloc-nexus (dynamically links libmorloc.so)
RUN cargo build --release -p morloc-nexus

RUN strip /root/.local/share/morloc/lib/libmorloc.so target/release/morloc-nexus

# ===========================================================================
# Stage 2: Build morloc-manager on Alpine (musl, fully static)
#
# morloc-manager has no native dependencies — pure Rust crates only.
# Building on Alpine produces a fully static musl binary that runs on any
# Linux, including NixOS and minimal containers.
# ===========================================================================
FROM docker.io/library/alpine:3.20 AS musl-builder

RUN apk add --no-cache curl gcc musl-dev

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --default-toolchain stable \
    && /root/.cargo/bin/cargo --version
ENV PATH="/root/.cargo/bin:${PATH}"

WORKDIR /build

# Copy Cargo manifests for dependency caching
COPY data/rust/Cargo.toml data/rust/Cargo.lock ./
COPY data/rust/morloc-runtime/Cargo.toml ./morloc-runtime/
COPY data/rust/morloc-nexus/Cargo.toml ./morloc-nexus/
COPY data/rust/morloc-manager/Cargo.toml ./morloc-manager/

RUN mkdir -p morloc-runtime/src morloc-nexus/src morloc-manager/src \
    && echo "fn main() {}" > morloc-nexus/src/main.rs \
    && echo "fn main() {}" > morloc-manager/src/main.rs \
    && echo "" > morloc-runtime/src/lib.rs

RUN cargo build --release -p morloc-manager 2>/dev/null || true

COPY data/rust/ ./

# Force rebuild: cargo may skip if it thinks the cached dummy binary is fresh
RUN touch morloc-manager/src/main.rs \
    && cargo build --release -p morloc-manager

RUN strip target/release/morloc-manager

# ===========================================================================
# Stage 3: Verify and output
# ===========================================================================
FROM docker.io/library/alpine:3.20

RUN apk add --no-cache file

COPY --from=glibc-builder /root/.local/share/morloc/lib/libmorloc.so /out-staging/libmorloc.so
COPY --from=glibc-builder /build/target/release/morloc-nexus /out-staging/morloc-nexus
COPY --from=musl-builder /build/target/release/morloc-manager /out-staging/morloc-manager

# Verify morloc-manager is static (no dynamic linker needed)
RUN file /out-staging/morloc-manager \
    && ! ldd /out-staging/morloc-manager 2>&1 | grep -q "=>" \
    && echo "OK: morloc-manager has no dynamic dependencies"

CMD ["sh", "-c", "cp /out-staging/libmorloc.so /out-staging/morloc-nexus /out-staging/morloc-manager /out/ && echo 'Wrote libmorloc.so, morloc-nexus, morloc-manager' && ls -lh /out/libmorloc.so /out/morloc-nexus /out/morloc-manager && echo && file /out/morloc-manager"]
