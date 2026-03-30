###############################################################################
# Stage 1: Build the morloc compiler from local source
FROM docker.io/library/ubuntu:24.04 AS morloc-build

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y git curl pkg-config libglib2.0-dev

# Install stack
RUN curl -SL https://get.haskellstack.org/ | sh

# Copy the local source tree (no git clone -- builds exactly this checkout)
COPY . /morloc

# Build morloc
RUN cd /morloc && stack install --no-run-tests

###############################################################################
# Stage 2: Build Rust binaries (libmorloc.so + morloc-nexus + morloc-manager)
FROM docker.io/library/ubuntu:24.04 AS rust-build

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl ca-certificates gcc g++ make pkg-config \
    && rm -rf /var/lib/apt/lists/*

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --default-toolchain stable
ENV PATH="/root/.cargo/bin:${PATH}"

COPY data/rust/ /build/
WORKDIR /build

# Build libmorloc.so from staticlib
RUN cargo build --release -p morloc-runtime
RUN gcc -shared -o libmorloc.so \
    -Wl,--whole-archive target/release/libmorloc_runtime.a -Wl,--no-whole-archive \
    -lpthread -lrt -ldl -lm

# Install libmorloc.so where the nexus linker expects it
RUN mkdir -p /root/.local/share/morloc/lib \
    && cp libmorloc.so /root/.local/share/morloc/lib/

# Build morloc-nexus and morloc-manager
RUN cargo build --release -p morloc-nexus
RUN cargo build --release -p morloc-manager

# Strip all
RUN strip libmorloc.so target/release/morloc-nexus target/release/morloc-manager

# Collect into a single directory
RUN mkdir -p /rust-bin \
    && cp libmorloc.so /rust-bin/ \
    && cp target/release/morloc-nexus /rust-bin/ \
    && cp target/release/morloc-manager /rust-bin/

###############################################################################
# Stage 3: Final minimal image with compiler + pre-built Rust binaries

LABEL org.opencontainers.image.source=https://github.com/morloc-project/morloc
LABEL org.opencontainers.image.description="Morloc executable in a minimal container"
LABEL org.opencontainers.image.licenses=Apache-2.0

FROM docker.io/library/ubuntu:24.04
COPY --from=morloc-build /root/.local/bin/morloc /bin/morloc
COPY --from=rust-build /rust-bin/ /opt/morloc-rust-bin/

# morloc make builds C++ pools, so g++ is needed
RUN apt-get update && apt-get install -y --no-install-recommends g++ \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Point morloc init at pre-built binaries (no cargo needed at runtime)
ENV MORLOC_RUST_BIN=/opt/morloc-rust-bin
