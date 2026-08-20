###############################################################################
# Stage 1: Build the morloc compiler from local source
FROM docker.io/library/ubuntu:24.04 AS morloc-build

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y git curl pkg-config libglib2.0-dev

# Install stack
RUN curl -SL https://get.haskellstack.org/ | sh

WORKDIR /morloc

# Dependency layer: copy only the dependency-determining files and build deps,
# so GHC + the compiled snapshot land in this layer. Keyed on the resolver +
# package deps, it is restored from the GHA layer cache unless those change --
# no re-download/recompile of GHC on every commit.
COPY stack.yaml stack.yaml.lock package.yaml ./
RUN stack build --no-run-tests --only-dependencies

# Source layer: the local checkout; only the project's own modules recompile.
COPY . .
RUN stack install --no-run-tests

###############################################################################
# Stage 2: Build the bootstrap manager + stage the Rust workspace source.
# libmorloc.so + morloc-nexus are NOT prebuilt: `morloc init` (stage 3) builds
# them from this source with the image's own toolchain.
FROM docker.io/library/ubuntu:24.04 AS rust-build

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl ca-certificates gcc make pkg-config \
    && rm -rf /var/lib/apt/lists/*

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --default-toolchain stable
ENV PATH="/root/.cargo/bin:${PATH}"

COPY data/rust/ /build/
WORKDIR /build

RUN cargo build --release -p morloc-manager \
    && strip target/release/morloc-manager

# Bundle the Rust crate sources (minus build artifacts) for `morloc init`.
RUN cp -r /build /rust-src && rm -rf /rust-src/target

###############################################################################
# Stage 3: Final image: compiler + toolchain; init builds the runtime from source
FROM docker.io/library/ubuntu:24.04

LABEL org.opencontainers.image.source=https://github.com/morloc-project/morloc
LABEL org.opencontainers.image.description="Morloc executable in a minimal container"
LABEL org.opencontainers.image.licenses=Apache-2.0

ENV DEBIAN_FRONTEND=noninteractive

COPY --from=morloc-build /root/.local/bin/morloc /bin/morloc
COPY --from=rust-build /build/target/release/morloc-manager /bin/morloc-manager
# Rust workspace source: morloc init compiles libmorloc.so, morloc-nexus, and
# the rustmorloc rlib from here.
COPY --from=rust-build /rust-src/ /opt/morloc-rust-bin/rust/

# Toolchain: g++ for C++ pools; the Rust toolchain both builds the runtime at
# init and compiles Rust pools at make time.
RUN apt-get update && apt-get install -y --no-install-recommends \
    g++ gcc make curl ca-certificates pkg-config \
    && rm -rf /var/lib/apt/lists/*

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --default-toolchain stable
ENV PATH="/root/.cargo/bin:${PATH}"

# Point morloc init at the bundled Rust source and build the runtime into the
# image so the container ships a ready-to-run MORLOC_HOME.
ENV MORLOC_RUST_DIR=/opt/morloc-rust-bin/rust
RUN morloc init -f
