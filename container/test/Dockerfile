FROM docker.io/library/ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y \
  git \
  gdb \
  curl \
  pkg-config \
  libglib2.0-dev \
  build-essential \
  libffi-dev \
  libgmp-dev \
  zlib1g-dev

# Install GHCup into /opt/ghcup (accessible to all users)
ENV GHCUP_INSTALL_BASE_PREFIX=/opt
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Add ghcup to PATH
ENV PATH="/opt/.ghcup/bin:${PATH}"

# Install Rust toolchain (needed by morloc init to build libmorloc.so)
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"

# Set the timezone, this avoids hanging later on
RUN TZ=Antarctica/Troll apt-get -y install tzdata

# hyperfine is needed for shell benchmarks
RUN apt-get install -y r-base python3 python3-dev python3-pip libgsl-dev git hyperfine
RUN python3 -m pip install --break-system-packages --upgrade setuptools numpy pyarrow

# Set up R environment
# stringi is needed for benchmarks
RUN Rscript -e 'install.packages("stringi", repos  = "https://cloud.r-project.org")'

# bit64 provides the 'integer64' S3 class that the R pool uses to
# represent IFile handles (a 64-bit unsigned integer ID).  R's native
# `integer` type is 32-bit, so without bit64 the per-language IFile
# type declaration in stdlib/root-r cannot be wired up and any morloc
# program that touches @open / IFile from R fails to typecheck.
RUN Rscript -e 'install.packages("bit64", repos = "https://cloud.r-project.org")'

# Install R Arrow package for cross-language Arrow IPC support
ENV LIBARROW_MINIMAL=true
ENV ARROW_S3=OFF
ENV ARROW_GCS=OFF
RUN Rscript -e 'install.packages("arrow", repos = "https://cloud.r-project.org")'

# Futhark: a hosted-guest language (`source Futhark` compiles into the C++ pool).
# Prebuilt static binary -- not in apt. Needed by the futhark-* golden tests.
# The c/multicore backends need no extra deps beyond the existing gcc/pthreads.
RUN apt-get update && apt-get install -y --no-install-recommends xz-utils \
 && curl -sSL https://futhark-lang.org/releases/futhark-0.26.4-linux-x86_64.tar.xz -o /tmp/futhark.tar.xz \
 && tar -xf /tmp/futhark.tar.xz -C /tmp \
 && make -C /tmp/futhark-0.26.4-linux-x86_64 install PREFIX=/usr/local \
 && rm -rf /tmp/futhark.tar.xz /tmp/futhark-0.26.4-linux-x86_64

# Cleanup to reduce image size
RUN apt-get clean && rm -rf /var/lib/apt/lists/*

# Create /home/dev with permissive access so any --user UID:GID can write here.
# morloc-manager bind-mounts .stack and .local into this directory.
RUN mkdir -p /home/dev && chmod 1777 /home/dev

COPY assets/bashrc /etc/bash.bashrc
