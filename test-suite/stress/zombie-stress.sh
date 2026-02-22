#!/usr/bin/env bash
# zombie-stress.sh - Test for resource accumulation over repeated runs
#
# Usage: ./zombie-stress.sh <golden-test-dir> <call> [<call> ...] [-- iterations]
#   e.g. ./zombie-stress.sh ../golden-tests/interop-3a-cp "foo '[1,2,3]'" -- 50

source "$(dirname "$0")/common.sh"

# Split args at "--" into calls and options
POSITIONAL=()
ITERATIONS=50
while [ $# -gt 0 ]; do
    if [ "$1" = "--" ]; then
        shift; ITERATIONS=${1:-50}; break
    fi
    POSITIONAL+=("$1"); shift
done
parse_args "${POSITIONAL[@]}"

echo "=== Zombie Stress Test ==="
echo "Iterations: $ITERATIONS"
compile_workload

INITIAL_ZOMBIES=$(count_zombies)
INITIAL_SHM=$(count_shm)
INITIAL_TMP=$(count_tmp)
FAILURES=0

printf "\n%-6s  %-8s  %-8s  %-8s\n" "ITER" "ZOMBIES" "SHM" "TMPDIR"
printf "%-6s  %-8s  %-8s  %-8s\n" "----" "-------" "---" "------"

for i in $(seq 1 "$ITERATIONS"); do
    run_nexus || true
    sleep 0.02

    ZOMBIES=$(( $(count_zombies) - INITIAL_ZOMBIES ))
    SHM=$(( $(count_shm) - INITIAL_SHM ))
    TMP=$(( $(count_tmp) - INITIAL_TMP ))

    if (( i % 10 == 0 || SHM > 0 || TMP > 0 )); then
        printf "%-6d  %-8d  %-8d  %-8d\n" "$i" "$ZOMBIES" "$SHM" "$TMP"
    fi

    if (( SHM > 0 || TMP > 0 )); then
        FAILURES=$((FAILURES + 1))
    fi
done

echo ""
echo "=== Summary ==="
FINAL_ZOMBIES=$(( $(count_zombies) - INITIAL_ZOMBIES ))
FINAL_SHM=$(( $(count_shm) - INITIAL_SHM ))
FINAL_TMP=$(( $(count_tmp) - INITIAL_TMP ))

echo "New zombies: $FINAL_ZOMBIES"
echo "Leaked SHM: $FINAL_SHM"
echo "Leaked tmpdir: $FINAL_TMP"
echo "Iterations with resource leaks: $FAILURES / $ITERATIONS"

if (( FINAL_SHM > 0 || FINAL_TMP > 0 )); then
    echo "FAIL: Resources leaked"
    exit 1
fi
echo "PASS"
