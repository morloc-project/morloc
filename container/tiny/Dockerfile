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
# Stage 2: Copy compiler binary and Rust runtime source

LABEL org.opencontainers.image.source=https://github.com/morloc-project/morloc
LABEL org.opencontainers.image.description="Morloc executable in a minimal container"
LABEL org.opencontainers.image.licenses=Apache-2.0

FROM docker.io/library/ubuntu:24.04
COPY --from=morloc-build /root/.local/bin/morloc /bin/morloc
COPY --from=morloc-build /morloc/data/rust/ /opt/morloc-rust/

# morloc make builds C++ pools, so g++ is needed
RUN apt-get update && apt-get install -y --no-install-recommends g++ \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

ENV MORLOC_RUST_DIR=/opt/morloc-rust
