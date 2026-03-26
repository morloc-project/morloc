# Static build of morloc-manager using benz0li/ghc-musl.
#
# This image provides GHC built against musl with static archives for all
# boot libraries. Used by Pandoc and Stack's own release builds.
#
# Build:
#   docker build -t morloc-static-build -f container/static-build/Dockerfile .
#   docker run --rm -v $(pwd)/out:/out morloc-static-build
#
# Output: ./out/morloc-manager (fully static ELF binary)

FROM docker.io/benz0li/ghc-musl:9.6.7 AS builder

# Ensure static C libraries are present (gmp needed by GHC bignum backend)
RUN apk add --no-cache gmp-static zlib-static

# Diagnostic: dump arch, GHC lib layout, and verify static libs exist
RUN echo "=== Architecture: $(uname -m) ===" \
    && echo "=== GHC libdir: $(ghc --print-libdir) ===" \
    && echo "=== ghc-prim library-dirs: $(ghc-pkg field ghc-prim library-dirs --simple-output) ===" \
    && echo "=== ghc-bignum library-dirs: $(ghc-pkg field ghc-bignum library-dirs --simple-output) ===" \
    && find "$(ghc --print-libdir)" -name "libHSghc-prim*" -o -name "libHSghc-bignum*" \
    && echo "=== Static lib check OK ==="

WORKDIR /build

# Copy stack.yaml and dependency spec for layer caching
COPY container/static-build/stack-static.yaml /build/stack.yaml
COPY morloc-manager/package.yaml ./morloc-manager/
RUN rm -f ./morloc-manager/morloc-manager.cabal

# Build dependencies (cached layer)
RUN stack build --only-dependencies morloc-manager \
    --system-ghc --no-install-ghc \
    --ghc-options='-split-sections -optc-Os'

# Copy source and LICENSE (package.yaml references ../LICENSE)
COPY morloc-manager/ ./morloc-manager/
COPY LICENSE ./LICENSE
RUN rm -f ./morloc-manager/morloc-manager.cabal

# Build with static linking
RUN stack build morloc-manager \
    --system-ghc --no-install-ghc \
    --ghc-options='-split-sections -optc-Os -optl-static -optl-pthread' \
    --no-run-tests

# Copy, verify, strip
RUN cp "$(stack --system-ghc path --local-install-root)/bin/morloc-manager" /morloc-manager
RUN file /morloc-manager
RUN strip /morloc-manager && ls -lh /morloc-manager

# ---- Output stage ----
FROM docker.io/library/alpine:3.20
COPY --from=builder /morloc-manager /usr/local/bin/morloc-manager
CMD ["sh", "-c", "cp /usr/local/bin/morloc-manager /out/morloc-manager && echo 'Wrote /out/morloc-manager'"]
