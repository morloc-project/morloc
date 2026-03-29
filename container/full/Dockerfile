# Copy the morloc binary from the matching tiny image
ARG MORLOC_VERSION=edge
FROM ghcr.io/morloc-project/morloc/morloc-tiny:${MORLOC_VERSION} AS morloc-bin

FROM docker.io/library/ubuntu:24.04

LABEL org.opencontainers.image.source=https://github.com/morloc-project/morloc
LABEL org.opencontainers.image.description="A morloc container intended for interactive shell use"
LABEL org.opencontainers.image.licenses=Apache-2.0

COPY --from=morloc-bin /bin/morloc /bin/morloc
COPY --from=morloc-bin /opt/morloc-rust /opt/morloc-rust

WORKDIR $HOME

ENV DEBIAN_FRONTEND=noninteractive
ENV PATH="/root/.local/bin:/root/.local/share/morloc/bin:${PATH}"
ENV MORLOC_RUST_DIR=/opt/morloc-rust

RUN apt-get update

# Set the timezone, this avoids hanging later on
RUN TZ=Antarctica/Troll apt-get -y install tzdata

RUN apt-get install -y r-base python3 python3-dev python3-pip python3-numpy libgsl-dev git curl

# Install Rust toolchain (needed by morloc init to build libmorloc.so)
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"

RUN python3 -m pip install --break-system-packages --upgrade setuptools

# Setup the morloc home (builds libmorloc.so from Rust via cargo)
RUN morloc init
# Copy morloc-nexus to a system path so it remains on PATH even when
# morloc-manager bind-mounts over ~/.local/bin with a host directory
RUN cp /root/.local/bin/morloc-nexus /usr/local/bin/morloc-nexus
RUN echo "lang_python3 : python3" >> $HOME/.local/share/morloc/config

RUN morloc install stdlib

RUN apt-get install -y vim

# Copy over custom vimrc
COPY assets/vimrc /root/.vimrc
COPY assets/README /root/README

# Set up vim highlighting for morloc
RUN git clone https://github.com/morloc-project/vimmorloc \
  && mkdir -p ~/.vim/syntax/ \
  && mkdir -p ~/.vim/ftdetect/ \
  && cp vimmorloc/loc.vim ~/.vim/syntax/ \
  && echo 'au BufRead,BufNewFile *.loc set filetype=loc' > ~/.vim/ftdetect/loc.vim \
  && rm -rf vimmorloc

RUN git clone https://github.com/morloc-project/morloc ~/morloc

COPY assets/bashrc /etc/bash.bashrc

# Cleanup to reduce image size
RUN apt-get clean && rm -rf /var/lib/apt/lists/*
