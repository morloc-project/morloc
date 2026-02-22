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

# Install GHCup
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Add ghcup to PATH
ENV PATH="/root/.ghcup/bin:${PATH}"

# Set the timezone, this avoids hanging later on
RUN DEBIAN_FRONTEND=noninteractive TZ=Antarctica/Troll apt-get -y install tzdata

# hyperfine is needed for shell benchmarks
RUN apt-get install -y r-base python3.10 libgsl-dev git pip hyperfine
RUN python3 -m pip install --upgrade setuptools numpy

# Set up R environment
# stringi is needed for benchmarks
RUN Rscript -e 'install.packages("stringi", repos  = "https://cloud.r-project.org")'

# Cleanup to reduce image size
RUN apt-get clean && rm -rf /var/lib/apt/lists/*

COPY assets/bashrc /etc/bash.bashrc
