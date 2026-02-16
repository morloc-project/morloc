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

compile_workload

VALGRIND_LOG="/tmp/morloc-valgrind-$$.log"

# Use first call only for valgrind (deterministic)
CALL="${CALLS[0]}"

# The nexus file may be a shell wrapper (#!/bin/sh + exec mim "$0" "$@").
# Valgrind can't instrument through exec, so unwrap to call mim directly.
if head -1 ./nexus | grep -q '^#!'; then
    MIM_BIN=$(sed -n '2s/^exec \([^ ]*\) .*/\1/p' ./nexus)
    if [ -z "$MIM_BIN" ] || ! command -v "$MIM_BIN" &>/dev/null; then
        echo "FAIL: Cannot find mim binary from nexus wrapper"
        exit 1
    fi
    VALGRIND_CMD="$MIM_BIN ./nexus $CALL"
else
    VALGRIND_CMD="./nexus $CALL"
fi

echo "Running under valgrind: $VALGRIND_CMD"
NEXUS_ERR="$WORK_DIR/valgrind-nexus.err"
eval timeout 60 valgrind \
    --leak-check=full \
    --show-leak-kinds=definite,indirect \
    --track-fds=yes \
    --log-file="$VALGRIND_LOG" \
    $VALGRIND_CMD > /dev/null 2>"$NEXUS_ERR"
EXIT_CODE=$?

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
