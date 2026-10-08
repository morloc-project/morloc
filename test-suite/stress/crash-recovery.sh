#!/usr/bin/env bash
# crash-recovery.sh - Test nexus behavior when a pool crashes
#
# Starts the nexus in background, kills one of its pool child processes with
# SIGKILL, and verifies the nexus exits within a reasonable time without
# hanging. Also checks for resource leaks.
#
# Usage: ./crash-recovery.sh <golden-test-dir> <call> [<call> ...] [-- iterations]
#   e.g. ./crash-recovery.sh crash-workload "napCP 30" -- 10
#
# Each call must outlast the pool's startup by seconds: a call that returns
# first leaves no pool to kill.

source "$(dirname "$0")/common.sh"

POSITIONAL=()
ITERATIONS=10
while [ $# -gt 0 ]; do
    if [ "$1" = "--" ]; then
        shift; ITERATIONS=${1:-10}; break
    fi
    POSITIONAL+=("$1"); shift
done
parse_args ${POSITIONAL[@]+"${POSITIONAL[@]}"}

MAX_WAIT_SECONDS=5
POOL_WAIT_SECONDS=10

echo "=== Crash Recovery Test ==="
echo "Iterations: $ITERATIONS"
compile_workload

INITIAL_SHM=$(count_shm)
INITIAL_TMP=$(count_tmp)
FAILURES=0

for i in $(seq 1 "$ITERATIONS"); do
    # Start nexus in background with a random call
    local_call="${CALLS[RANDOM % ${#CALLS[@]}]}"
    iter_err="$WORK_DIR/iter-${i}.err"
    eval exec ./nexus $local_call 2>"$iter_err" > /dev/null &
    NEXUS_PID=$!

    # Wait for a pool to start, then give it time to enter the call. The
    # workload's calls sleep far longer than this, so the pool is killed
    # mid-call.
    POOL_PID=""
    for _ in $(seq 1 "$((POOL_WAIT_SECONDS * 10))"); do
        for p in $(pgrep -P "$NEXUS_PID" 2>/dev/null); do
            case "$(ps -o args= -p "$p")" in
                morloc-pool-pin*) ;;
                *) POOL_PID=$p; break ;;
            esac
        done
        [ -n "$POOL_PID" ] && break
        kill -0 "$NEXUS_PID" 2>/dev/null || break
        sleep 0.1
    done
    sleep 0.5

    KILLED=0
    if [ -n "$POOL_PID" ] && kill -9 "$POOL_PID" 2>/dev/null; then
        KILLED=1
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

    # Log any nexus stderr
    if [ -s "$iter_err" ]; then
        {
            printf "=== %s | %s | iteration %d | call: %s | %s ===\n" \
                "$STRESS_SCRIPT" "$(basename "$TEST_DIR")" "$i" "$local_call" "$(date '+%H:%M:%S')"
            cat "$iter_err"
            echo ""
        } >> "$STDERR_LOG"
    fi
    rm -f "$iter_err"

    SHM=$(( $(count_shm) - INITIAL_SHM ))
    TMP=$(( $(count_tmp) - INITIAL_TMP ))

    if (( ! KILLED )); then
        printf "Iteration %3d: NO POOL (nothing was crashed)\n" "$i"
        FAILURES=$((FAILURES + 1))
    elif (( HUNG )); then
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
