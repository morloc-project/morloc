#!/usr/bin/env bash
# valgrind-check.sh - Check for memory leaks and leaked file descriptors
#
# Runs the nexus under valgrind with leak checking and fd tracking.
# Requires valgrind to be installed; skips gracefully if not available.
#
# Usage: ./valgrind-check.sh <golden-test-dir> <call>
#   e.g. ./valgrind-check.sh ../golden-tests/interop-3a-cp "foo '[1,2,3]'"

source "$(dirname "$0")/common.sh"

parse_args "$@"

echo "=== Valgrind Memory/FD Leak Check ==="

if ! command -v valgrind &>/dev/null; then
    echo "SKIP: valgrind not found"
    exit 0
fi

# Present is not the same as usable. Valgrind needs the dynamic linker's
# symbols, so a machine with a stripped ld.so and no glibc debuginfo has a
# valgrind that refuses to start anything. Find that out on `true` rather
# than by mistaking it for a leak in the nexus.
if ! valgrind --error-exitcode=0 true >/dev/null 2>&1; then
    echo "SKIP: valgrind cannot run here (usually missing glibc debuginfo)"
    exit 0
fi

compile_workload

VALGRIND_LOG="/tmp/morloc-valgrind-$$.log"

# Use first call only for valgrind (deterministic)
CALL="${CALLS[0]}"

# The nexus file is a shell wrapper that execs the shared morloc-nexus binary
# against the program's manifest. Valgrind cannot instrument through the exec,
# so run that binary directly. The wrapper names its manifest on a marker line
# for exactly this purpose; the build directory is keyed on the program name,
# so it must be read rather than assumed.
if head -1 ./nexus | grep -q '^#!'; then
    NEXUS_BIN=$(command -v morloc-nexus 2>/dev/null)
    MANIFEST=$(sed -n 's/^# morloc-manifest: //p' ./nexus | head -1)
    if [ -z "$NEXUS_BIN" ]; then
        echo "FAIL: morloc-nexus is not on PATH"
        exit 1
    fi
    if [ -z "$MANIFEST" ] || [ ! -f "$MANIFEST" ]; then
        echo "FAIL: nexus wrapper names no readable manifest (got '$MANIFEST')"
        exit 1
    fi
    VALGRIND_CMD="$NEXUS_BIN run $PWD/$MANIFEST $CALL"
else
    VALGRIND_CMD="./nexus $CALL"
fi

echo "Running under valgrind: $VALGRIND_CMD"
NEXUS_ERR="$WORK_DIR/valgrind-nexus.err"
# `|| EXIT_CODE=$?` is load-bearing: common.sh sets -e, so without it a
# non-zero valgrind exit kills this script before the status can be read,
# and every check below -- including the timeout branch -- becomes
# unreachable. The run would then fail with no verdict and no reason.
EXIT_CODE=0
eval timeout 60 valgrind \
    --leak-check=full \
    --show-leak-kinds=definite,indirect \
    --track-fds=yes \
    --log-file="$VALGRIND_LOG" \
    $VALGRIND_CMD > /dev/null 2>"$NEXUS_ERR" || EXIT_CODE=$?

# Log any nexus/valgrind stderr
if [ -s "$NEXUS_ERR" ]; then
    {
        printf "=== %s | %s | call: %s | %s ===\n" \
            "$STRESS_SCRIPT" "$(basename "$TEST_DIR")" "$CALL" "$(date '+%H:%M:%S')"
        cat "$NEXUS_ERR"
        echo ""
    } >> "$STDERR_LOG"
fi
rm -f "$NEXUS_ERR"

echo ""
if [ ! -f "$VALGRIND_LOG" ]; then
    echo "FAIL: No valgrind log produced"
    exit 1
fi

if (( EXIT_CODE == 124 )); then
    echo "FAIL: Timed out under valgrind"
    rm -f "$VALGRIND_LOG"
    exit 1
fi

# Extract definite leak count
DEFINITELY_LOST=$(grep 'definitely lost:' "$VALGRIND_LOG" | grep -oP '\d+(?= bytes)' | head -1)
DEFINITELY_LOST=${DEFINITELY_LOST:-0}
FD_LEAK=$(grep 'FILE DESCRIPTORS:' "$VALGRIND_LOG" | grep -oP '\d+(?= open)' | head -1)
FD_LEAK=${FD_LEAK:-3}
EXTRA_FDS=$((FD_LEAK - 3))  # subtract stdin/stdout/stderr

echo "Definitely lost: ${DEFINITELY_LOST} bytes"
echo "Extra file descriptors at exit: ${EXTRA_FDS}"

# Fail on large leaks (>4KB) or many leaked fds (>3)
if (( DEFINITELY_LOST > 4096 )); then
    echo ""
    echo "FAIL: Large memory leak detected (log: $VALGRIND_LOG)"
    cat "$VALGRIND_LOG"
    exit 1
fi
if (( EXTRA_FDS > 3 )); then
    echo ""
    echo "FAIL: File descriptor leak detected (log: $VALGRIND_LOG)"
    cat "$VALGRIND_LOG"
    exit 1
fi

rm -f "$VALGRIND_LOG"
echo "PASS"
