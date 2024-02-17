# Dockerfile for Morloc development environment
# This Dockerfile sets up an environment with Python, R, C++, and Haskell.

FROM ubuntu:22.04

WORKDIR /app

# set the path where morloc will be installed by stack
ENV PATH="/root/.local/bin:$PATH"

RUN apt-get update

# Set the timezone, this avoids hanging later on
RUN DEBIAN_FRONTEND=noninteractive TZ=Antarctica/Troll apt-get -y install tzdata

RUN apt-get install -y r-base python3.10 python3-pip libgsl-dev git curl

# Cleanup to reduce image size
RUN apt-get clean && rm -rf /var/lib/apt/lists/*

# Set up Python environment
RUN pip3.10 install pymorlocinternals

# Set up C++ environment
RUN mkdir -p $HOME/.morloc/include && git clone https://github.com/morloclib/mlccpptypes $HOME/.morloc/include/mlccpptypes

# Set up R environment
RUN Rscript -e 'install.packages("remotes", repos  = "https://cloud.r-project.org")'
RUN Rscript -e 'remotes::install_github("morloc-project/rmorlocinternals", dependencies=TRUE)'

# Build morloc
RUN apt-get update
RUN curl -SL https://get.haskellstack.org/ | sh
RUN git clone https://github.com/morloc-project/morloc && cd morloc && stack install --fast

# Make yourself at home
RUN mkdir -p $HOME/.morloc/lib
RUN mkdir -p $HOME/.morloc/tmp
RUN echo "home : $HOME/.morloc" > ~/.morloc/config &&\
    echo "library : $HOME/.morloc/lib" >> $HOME/.morloc/config &&\
    echo "tmpdir : $HOME/.morloc/tmp" >> $HOME/.morloc/config &&\
    echo "lang_python3 : python3" >> $HOME/.morloc/config

# Install the morloc modules that are required for testing
RUN morloc install prelude
RUN morloc install base
RUN morloc install conventions
RUN morloc install rbase
RUN morloc install pybase
RUN morloc install cppbase
RUN morloc install math

# # Run the standard morloc tests
RUN cd morloc && stack test --fast

# Specify the default command to run when the container starts
CMD ["bash"]
