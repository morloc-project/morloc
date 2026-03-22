FROM docker.io/library/ubuntu:22.04

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

# Set the timezone, this avoids hanging later on
RUN DEBIAN_FRONTEND=noninteractive TZ=Antarctica/Troll apt-get -y install tzdata

# hyperfine is needed for shell benchmarks
RUN apt-get install -y r-base python3.10 libgsl-dev git pip hyperfine
RUN python3 -m pip install --upgrade setuptools numpy pyarrow

# Set up R environment
# stringi is needed for benchmarks
RUN Rscript -e 'install.packages("stringi", repos  = "https://cloud.r-project.org")'

# Install R Arrow package for cross-language Arrow IPC support
ENV LIBARROW_MINIMAL=true
ENV ARROW_S3=OFF
ENV ARROW_GCS=OFF
RUN Rscript -e 'install.packages("arrow", repos = "https://cloud.r-project.org")'

# Cleanup to reduce image size
RUN apt-get clean && rm -rf /var/lib/apt/lists/*

# Create /home/dev with permissive access so any --user UID:GID can write here.
# morloc-manager bind-mounts .stack and .local into this directory.
RUN mkdir -p /home/dev && chmod 1777 /home/dev

COPY assets/bashrc /etc/bash.bashrc
