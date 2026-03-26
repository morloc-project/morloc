# Static build of morloc-manager using benz0li/ghc-musl.
#
# Uses Cabal with --enable-executable-static (same approach as Pandoc).
# This correctly handles static library resolution on both x86_64 and aarch64,
# unlike Stack's -optl-static which has library naming issues on aarch64 musl.
#
# Build:
#   docker build -t morloc-static-build -f container/static-build/Dockerfile .
#   docker run --rm -v $(pwd)/out:/out morloc-static-build
#
# Output: ./out/morloc-manager (fully static ELF binary)

FROM docker.io/benz0li/ghc-musl:9.10.3 AS builder

# Ensure static C libraries and hpack are present
RUN apk add --no-cache gmp-static zlib-static \
    && cabal update \
    && cabal install hpack --install-method=copy --installdir=/usr/local/bin

WORKDIR /build

# Copy dependency spec first for layer caching
COPY morloc-manager/package.yaml ./morloc-manager/
COPY LICENSE ./LICENSE

# Generate .cabal file from package.yaml and build dependencies
RUN cd morloc-manager \
    && hpack \
    && cabal build --only-dependencies \
       --enable-executable-static \
       --ghc-options='-split-sections -optc-Os -optl=-pthread'

# Copy full source
COPY morloc-manager/ ./morloc-manager/
RUN cd morloc-manager && hpack

# Build with static linking
RUN cd morloc-manager \
    && cabal build \
       --enable-executable-static \
       --ghc-options='-split-sections -optc-Os -optl=-pthread'

# Copy, verify, strip
RUN cp "$(cabal -v0 list-bin --project-dir=morloc-manager morloc-manager)" /morloc-manager
RUN file /morloc-manager
RUN strip /morloc-manager && ls -lh /morloc-manager

# ---- Output stage ----
FROM docker.io/library/alpine:3.20
COPY --from=builder /morloc-manager /usr/local/bin/morloc-manager
CMD ["sh", "-c", "cp /usr/local/bin/morloc-manager /out/morloc-manager && echo 'Wrote /out/morloc-manager'"]
