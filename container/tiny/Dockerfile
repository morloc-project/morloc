###############################################################################
# Stage 1: Build the morloc compiler 
FROM ubuntu:22.04 as morloc-build

RUN apt-get update && apt-get install -y git curl pkg-config libglib2.0-dev

# Build morloc
RUN curl -SL https://get.haskellstack.org/ | sh

# Get the latest morloc release
RUN git clone https://github.com/morloc-project/morloc && echo "hi"

# Build morloc
RUN cd morloc && stack install


###############################################################################
# Stage 2: Copy just the compiler binary

LABEL org.opencontainers.image.source=https://github.com/morloc-project/morloc
LABEL org.opencontainers.image.description="Morloc executable in a minimal container"
LABEL org.opencontainers.image.licenses=MIT

FROM ubuntu:22.04
COPY --from=morloc-build /root/.local/bin/morloc /bin/morloc

# morloc make builds C++ pools, so g++ is needed
RUN apt-get update && apt-get install -y g++
RUN apt-get clean && rm -rf /var/lib/apt/lists/*
