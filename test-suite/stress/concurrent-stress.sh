#!/usr/bin/env bash
# concurrent-stress.sh - Test concurrent nexus execution
#
# Usage: ./concurrent-stress.sh <golden-test-dir> <call> [<call> ...] [-- concurrent rounds]
#   e.g. ./concurrent-stress.sh ../golden-tests/interop-3a-cp "foo '[1,2,3]'" -- 10 10

source "$(dirname "$0")/common.sh"

POSITIONAL=()
CONCURRENT=10
ROUNDS=10
while [ $# -gt 0 ]; do
    if [ "$1" = "--" ]; then
        shift; CONCURRENT=${1:-10}; shift; ROUNDS=${1:-10}; break
    fi
    POSITIONAL+=("$1"); shift
done
parse_args "${POSITIONAL[@]}"

echo "=== Concurrent Stress Test ==="
echo "Concurrent: $CONCURRENT, Rounds: $ROUNDS"
compile_workload

INITIAL_SHM=$(count_shm)
INITIAL_TMP=$(count_tmp)
INITIAL_ZOMBIES=$(count_zombies)
RESOURCE_LEAK_ROUNDS=0

for round in $(seq 1 "$ROUNDS"); do
    PIDS=()
    for j in $(seq 1 "$CONCURRENT"); do
        run_nexus &
        PIDS+=($!)
    done

    EXEC_FAILURES=0
    for pid in "${PIDS[@]}"; do
        if ! wait "$pid" 2>/dev/null; then
            EXEC_FAILURES=$((EXEC_FAILURES + 1))
        fi
    done

    sleep 0.05

    SHM=$(( $(count_shm) - INITIAL_SHM ))
    TMP=$(( $(count_tmp) - INITIAL_TMP ))
    ZOMBIES=$(( $(count_zombies) - INITIAL_ZOMBIES ))

    printf "Round %3d: %d/%d succeeded, zombies=%d, shm=%d, tmp=%d\n" \
        "$round" "$((CONCURRENT - EXEC_FAILURES))" "$CONCURRENT" \
        "$ZOMBIES" "$SHM" "$TMP"

    if (( SHM > 0 || TMP > 0 )); then
        RESOURCE_LEAK_ROUNDS=$((RESOURCE_LEAK_ROUNDS + 1))
    fi
done

echo ""
echo "=== Summary ==="
FINAL_SHM=$(( $(count_shm) - INITIAL_SHM ))
FINAL_TMP=$(( $(count_tmp) - INITIAL_TMP ))
FINAL_ZOMBIES=$(( $(count_zombies) - INITIAL_ZOMBIES ))

echo "New zombies: $FINAL_ZOMBIES"
echo "Leaked SHM: $FINAL_SHM"
echo "Leaked tmpdir: $FINAL_TMP"
echo "Rounds with resource leaks: $RESOURCE_LEAK_ROUNDS / $ROUNDS"

if (( FINAL_SHM > 0 || FINAL_TMP > 0 )); then
    echo "FAIL: Resources leaked"
    exit 1
fi
echo "PASS"
