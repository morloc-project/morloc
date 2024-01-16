# Dockerfile for Morloc development environment
# This Dockerfile sets up an environment with Python, R, C++, and Haskell.

FROM ubuntu:22.04

WORKDIR /app

RUN apt-get update

# Set the timezone, this avoids hanging later on
RUN DEBIAN_FRONTEND=noninteractive TZ=Antarctica/Troll apt-get -y install tzdata

RUN apt-get install -y \
  r-base \
  python3.10 \
  python3-pip \
  libgsl-dev \
  git \
  curl

# Cleanup to reduce image size
RUN apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Set up Python environment
RUN pip3.10 install pymorlocinternals

# Set up C++ environment
RUN mkdir -p $HOME/.morloc/include \
    git clone https://github.com/morloclib/include/mlccpptypes $HOME/.morloc/mlccpptypes

# Set up R environment
RUN Rscript -e 'install.packages("remotes", repos  = "https://cloud.r-project.org")' \
    Rscript -e 'remotes::install_github("morloc-project/rmorlocinternals", dependencies=TRUE)'

# Build morloc
RUN apt-get update \
    curl -SL https://get.haskellstack.org/ | sh \
    stack upgrade \
    git clone https://github.com/morloc-project/morloc \
    cd morloc \
    stack install --fast \
    mkdir -p $HOME/.morloc/lib \
    mkdir -p $HOME/.morloc/tmp \
    echo "home : $HOME/.morloc" > ~/.morloc/config \
    echo "library : $HOME/.morloc/lib" >> $HOME/.morloc/config \
    echo "tmpdir : $HOME/.morloc/tmp" >> $HOME/.morloc/config \
    echo "lang_python3 : python3" >> $HOME/.morloc/config \
    morloc install base \
    morloc install conventions \
    morloc install rbase \
    morloc install pybase \
    morloc install cppbase \
    morloc install math

# Specify the default command to run when the container starts
CMD ["bash"]
