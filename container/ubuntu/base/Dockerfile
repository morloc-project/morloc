###############################################################################
# Stage 1: Build the morloc compiler 
FROM alpine:3.19 as morloc-build

RUN apk update \
  && apk add --no-cache git \
  && apk add --no-cache gmp-dev \
  && apk add --no-cache build-base \
  && apk add --no-cache ncurses \
  && apk add --no-cache curl

# Build morloc
RUN curl -SL https://get.haskellstack.org/ | sh

RUN git clone https://github.com/morloc-project/morloc

# Build morloc
RUN cd morloc && stack install


###############################################################################
# Stage 2: Copy just the compiler binary
FROM ubuntu:22.04
COPY --from=morloc-build /root/.local/bin/morloc /bin/morloc

WORKDIR $HOME

RUN apt-get update

# Set the timezone, this avoids hanging later on
RUN DEBIAN_FRONTEND=noninteractive TZ=Antarctica/Troll apt-get -y install tzdata

RUN apt-get install -y r-base python3.10 python3-pip libgsl-dev git

# Set up Python environment
RUN pip3.10 install pymorlocinternals

# Set up C++ environment
RUN mkdir -p $HOME/.morloc/include \
  && git clone https://github.com/morloclib/mlccpptypes $HOME/.morloc/include/mlccpptypes

# Set up R environment
RUN Rscript -e 'install.packages("remotes", repos  = "https://cloud.r-project.org")'
RUN Rscript -e 'remotes::install_github("morloc-project/rmorlocinternals", dependencies=TRUE)'

# Setup the morloc home
RUN mkdir -p $HOME/.morloc/lib
RUN mkdir -p $HOME/.morloc/tmp
RUN echo "home : $HOME/.morloc" > ~/.morloc/config && \
    echo "library : $HOME/.morloc/lib" >> $HOME/.morloc/config && \
    echo "tmpdir : $HOME/.morloc/tmp" >> $HOME/.morloc/config && \
    echo "lang_python3 : python3" >> $HOME/.morloc/config

# Install the morloc modules that are required for the morloc tests to pass
RUN morloc install prelude
RUN morloc install types
RUN morloc install conventions
RUN morloc install base
RUN morloc install rbase
RUN morloc install pybase
RUN morloc install cppbase
RUN morloc install math

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

RUN git clone https://github.com/morloc-project/morloc \
  && mv morloc/test-suite/golden-tests ~/tests

# Cleanup to reduce image size
RUN apt-get clean && rm -rf /var/lib/apt/lists/*
