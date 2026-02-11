#!/usr/bin/env bash
# crash-recovery.sh - Test nexus behavior when a pool crashes
#
# Starts the nexus in background, kills one of its pool child processes with
# SIGKILL, and verifies the nexus exits within a reasonable time without
# hanging. Also checks for resource leaks.
#
# Usage: ./crash-recovery.sh <golden-test-dir> <call> [<call> ...] [-- iterations]
#   e.g. ./crash-recovery.sh ../golden-tests/interop-3a-cp "foo '[1,2,3]'" -- 10

source "$(dirname "$0")/common.sh"

POSITIONAL=()
ITERATIONS=10
while [ $# -gt 0 ]; do
    if [ "$1" = "--" ]; then
        shift; ITERATIONS=${1:-10}; break
    fi
    POSITIONAL+=("$1"); shift
done
parse_args "${POSITIONAL[@]}"

MAX_WAIT_SECONDS=5

echo "=== Crash Recovery Test ==="
echo "Iterations: $ITERATIONS"
compile_workload

INITIAL_SHM=$(count_shm)
INITIAL_TMP=$(count_tmp)
FAILURES=0

for i in $(seq 1 "$ITERATIONS"); do
    # Start nexus in background with a random call
    local_call="${CALLS[RANDOM % ${#CALLS[@]}]}"
    eval ./nexus $local_call > /dev/null 2>&1 &
    NEXUS_PID=$!

    # Wait for pools to start
    sleep 0.1

    # Find and kill a pool child process
    POOL_PID=$(ps -o pid= --ppid "$NEXUS_PID" 2>/dev/null | head -1 | tr -d ' ') || true

    if [ -n "$POOL_PID" ]; then
        kill -9 "$POOL_PID" 2>/dev/null || true
    fi

    # Wait for nexus to exit (with timeout)
    HUNG=0
    ELAPSED=0
    while kill -0 "$NEXUS_PID" 2>/dev/null; do
        if (( ELAPSED >= MAX_WAIT_SECONDS * 10 )); then
            HUNG=1
            kill -9 "$NEXUS_PID" 2>/dev/null || true
            break
        fi
        sleep 0.1
        ELAPSED=$((ELAPSED + 1))
    done
    wait "$NEXUS_PID" 2>/dev/null || true

    SHM=$(( $(count_shm) - INITIAL_SHM ))
    TMP=$(( $(count_tmp) - INITIAL_TMP ))

    if (( HUNG )); then
        printf "Iteration %3d: HUNG (nexus did not exit within %ds)\n" "$i" "$MAX_WAIT_SECONDS"
        FAILURES=$((FAILURES + 1))
    elif (( SHM > 0 || TMP > 0 )); then
        printf "Iteration %3d: LEAK (shm=%d, tmp=%d)\n" "$i" "$SHM" "$TMP"
        FAILURES=$((FAILURES + 1))
    else
        printf "Iteration %3d: OK\n" "$i"
    fi
done

echo ""
echo "=== Summary ==="
echo "Failures: $FAILURES / $ITERATIONS"

if (( FAILURES > 0 )); then
    echo "FAIL"
    exit 1
fi
echo "PASS"
