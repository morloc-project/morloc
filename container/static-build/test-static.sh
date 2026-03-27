#!/bin/sh
# Test that the static morloc-manager binary works correctly.
#
# Prerequisites:
#   1. Run build.sh first (or docker build manually)
#   2. docker or podman available
#
# Usage:
#   ./container/static-build/test-static.sh
#
# Tests:
#   1. Verify binary exists and is statically linked (file + ldd)
#   2. Run --help in a FROM scratch container (the definitive static test)
#   3. Run --help on ubuntu, debian, fedora (cross-distro)
#   4. Run info subcommand in scratch (exercises more code paths)

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
BINARY="$PROJECT_DIR/out/morloc-manager"

# Detect container engine
if command -v podman >/dev/null 2>&1; then
    ENGINE=podman
elif command -v docker >/dev/null 2>&1; then
    ENGINE=docker
else
    echo "FAIL: neither podman nor docker found" >&2
    exit 1
fi

PASS=0
FAIL=0

pass() { PASS=$((PASS + 1)); echo "  PASS: $1"; }
fail() { FAIL=$((FAIL + 1)); echo "  FAIL: $1"; }

# ---- Test 0: Binary exists ----
echo "=== Test 0: Binary exists ==="
if [ ! -f "$BINARY" ]; then
    echo "Binary not found at $BINARY"
    echo "Run build.sh first:"
    echo "  ./container/static-build/build.sh"
    exit 1
fi
pass "binary exists"

# ---- Test 1: Verify static linking ----
echo ""
echo "=== Test 1: Static linking verification ==="

FILE_OUTPUT=$(file "$BINARY")
echo "  file: $FILE_OUTPUT"
if echo "$FILE_OUTPUT" | grep -q "statically linked"; then
    pass "file reports statically linked"
else
    fail "file does not report statically linked"
fi

LDD_OUTPUT=$(ldd "$BINARY" 2>&1 || true)
echo "  ldd: $LDD_OUTPUT"
if echo "$LDD_OUTPUT" | grep -q "not a dynamic executable\|not a regular file\|statically linked"; then
    pass "ldd confirms not dynamic"
else
    fail "ldd suggests dynamic linking"
fi

# ---- Test 2: FROM scratch (the definitive test) ----
echo ""
echo "=== Test 2: FROM scratch container ==="

SCRATCH_DF=$(mktemp)
cat > "$SCRATCH_DF" <<'DOCKERFILE'
FROM scratch
COPY morloc-manager /morloc-manager
ENTRYPOINT ["/morloc-manager"]
DOCKERFILE

# Build the scratch image
$ENGINE build -t mm-scratch-test -f "$SCRATCH_DF" "$PROJECT_DIR/out" 2>/dev/null

# Test --help
HELP_OUTPUT=$($ENGINE run --rm mm-scratch-test --help 2>&1 || true)
if echo "$HELP_OUTPUT" | grep -q "morloc-manager"; then
    pass "scratch: --help works"
else
    fail "scratch: --help failed"
    echo "  output: $HELP_OUTPUT"
fi

# Test info subcommand (exercises config reading, error handling)
INFO_OUTPUT=$($ENGINE run --rm mm-scratch-test info 2>&1 || true)
if echo "$INFO_OUTPUT" | grep -q "Installed versions\|No container engine\|Configuration not found"; then
    pass "scratch: info subcommand runs"
else
    fail "scratch: info subcommand failed"
    echo "  output: $INFO_OUTPUT"
fi

# Test run subcommand (should fail gracefully without engine)
RUN_OUTPUT=$($ENGINE run --rm mm-scratch-test run --help 2>&1 || true)
if echo "$RUN_OUTPUT" | grep -q "Run a command"; then
    pass "scratch: run --help works"
else
    fail "scratch: run --help failed"
    echo "  output: $RUN_OUTPUT"
fi

rm -f "$SCRATCH_DF"
$ENGINE rmi mm-scratch-test 2>/dev/null || true

# ---- Test 3: Cross-distro ----
echo ""
echo "=== Test 3: Cross-distro compatibility ==="

for DISTRO in docker.io/library/ubuntu:22.04 docker.io/library/debian:12 docker.io/library/fedora:40 docker.io/library/alpine:3.20; do
    OUTPUT=$($ENGINE run --rm -v "$PROJECT_DIR/out/morloc-manager:/mm:ro" "$DISTRO" /mm --help 2>&1 || true)
    if echo "$OUTPUT" | grep -q "morloc-manager"; then
        pass "$DISTRO: --help works"
    else
        fail "$DISTRO: --help failed"
        echo "  output: $OUTPUT"
    fi
done

# ---- Summary ----
echo ""
echo "=== Results: $PASS passed, $FAIL failed ==="
if [ "$FAIL" -gt 0 ]; then
    exit 1
fi
