# morloc-serve base image
#
# Minimal runtime for serving frozen morloc programs. Contains language
# runtimes (Python, R, C++ stdlib), the morloc compiler binary, and the
# morloc-nexus/libmorloc built by `morloc init`.
#
# Does NOT include GHC, Stack, or build toolchain.
#
# Build (from the container/ directory):
#   podman build --no-cache \
#     --build-arg MORLOC_VERSION=0.68.0 \
#     -t ghcr.io/morloc-project/morloc/morloc-serve:0.68.0 \
#     -f serve/Dockerfile ..
#
# Layer architecture:
#
#   +---------------------------------------+
#   | Frozen state (added by serve-image)   |  COPY'd at serve-image build time
#   +---------------------------------------+
#   | morloc-serve:<version> (this image)   |
#   |   morloc compiler binary              |
#   |   morloc-nexus (built by morloc init) |
#   |   libmorloc.so (built by morloc init) |
#   |   Python 3.x runtime                  |
#   |   R runtime                           |
#   |   C++ standard library (libstdc++)    |
#   +---------------------------------------+
#   | Ubuntu 24.04 base                     |
#   +---------------------------------------+

# Stage 1: get the morloc binary from the tiny image
ARG MORLOC_VERSION=edge
FROM ghcr.io/morloc-project/morloc/morloc-tiny:${MORLOC_VERSION} AS morloc-bin

# Stage 2: build the serve image
FROM docker.io/library/ubuntu:24.04

LABEL org.opencontainers.image.source=https://github.com/morloc-project/morloc
LABEL org.opencontainers.image.description="Minimal morloc runtime for serving frozen programs"
LABEL org.opencontainers.image.licenses=Apache-2.0

ENV DEBIAN_FRONTEND=noninteractive
ENV PATH="/root/.local/bin:/root/.local/share/morloc/bin:${PATH}"

# Install language runtimes and build tools needed at runtime
RUN apt-get update && apt-get install -y --no-install-recommends \
    python3 \
    python3-dev \
    r-base-core \
    g++ \
    gcc \
    make \
    curl \
    libgmp10 \
    libffi8 \
    zlib1g \
    && rm -rf /var/lib/apt/lists/*

# Install Rust toolchain (needed by morloc init to build libmorloc.so)
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"

# Copy the morloc compiler binary and Rust source from the tiny image
COPY --from=morloc-bin /bin/morloc /usr/local/bin/morloc
COPY --from=morloc-bin /opt/morloc-rust /opt/morloc-rust
ENV MORLOC_RUST_DIR=/opt/morloc-rust

# Run morloc init to build morloc-nexus and libmorloc.so (from Rust via cargo)
RUN morloc init

# Remove Rust toolchain to reduce image size (not needed at runtime)
RUN rustup self uninstall -y && rm -rf /root/.cargo /root/.rustup

# Create directories for frozen state (populated by serve-image)
RUN mkdir -p /root/.local/share/morloc/lib \
             /root/.local/share/morloc/fdb \
             /root/.local/share/morloc/bin

# Default entrypoint: nexus router serving all programs in fdb/
ENTRYPOINT ["morloc-nexus", "--router", \
            "--fdb", "/root/.local/share/morloc/fdb"]
