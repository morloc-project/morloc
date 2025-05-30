LABEL org.opencontainers.image.source=https://github.com/morloc-project/morloc
LABEL org.opencontainers.image.description="A morloc container intended for interactive shell use"
LABEL org.opencontainers.image.licenses=MIT

FROM docker.io/library/ubuntu:22.04
COPY --from=ghcr.io/morloc-project/morloc/morloc-tiny:0.53.5 /bin/morloc /bin/morloc

WORKDIR $HOME

RUN apt-get update

# Set the timezone, this avoids hanging later on
RUN DEBIAN_FRONTEND=noninteractive TZ=Antarctica/Troll apt-get -y install tzdata

RUN apt-get install -y r-base python3.10 libgsl-dev git pip python3-numpy
RUN python3 -m pip install --upgrade pip setuptools


# Set up R environment
RUN Rscript -e 'install.packages(c("rlang"), repos  = "https://cloud.r-project.org")'

# Setup the morloc home
RUN morloc init
RUN echo "lang_python3 : python3" >> $HOME/.morloc/config

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

RUN git clone https://github.com/morloc-project/morloc ~/morloc

# Cleanup to reduce image size
RUN apt-get clean && rm -rf /var/lib/apt/lists/*
