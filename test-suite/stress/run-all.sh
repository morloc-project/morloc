#!/usr/bin/env bash
# run-all.sh - Run all stress tests across language combinations
#
# Usage: ./run-all.sh [test...]
#   With no arguments, runs all tests. Pass test names to run a subset:
#   ./run-all.sh zombie concurrent

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GOLDEN="$SCRIPT_DIR/../golden-tests"

# Workloads: golden-test-dir + nexus calls
declare -A WORKLOAD_DIR=(
    [cpp]="$GOLDEN/argument-form-1-c"
    [py]="$GOLDEN/argument-form-1-py"
    [r]="$GOLDEN/argument-form-1-r"
    [cpp-py]="$GOLDEN/interop-3a-cp"
    [cpp-r]="$GOLDEN/interop-3a-rc"
    [py-r]="$GOLDEN/interop-3a-pr"
)
declare -A WORKLOAD_CALL=(
    [cpp]="foo 2"
    [py]="foo 2"
    [r]="foo 2"
    [cpp-py]="foo '[1,2,3]'"
    [cpp-r]="foo '[1,2,3]'"
    [py-r]="foo '[1,2,3]'"
)

WORKLOAD_ORDER=(cpp py r cpp-py cpp-r py-r)

PASSED=0
FAILED=0
SKIPPED=0
FAILURES=()

run_test() {
    local test_script="$1"
    local test_name="$2"
    local workload="$3"
    local dir="${WORKLOAD_DIR[$workload]}"
    local call="${WORKLOAD_CALL[$workload]}"

    printf "%-20s %-8s ... " "$test_name" "[$workload]"

    local output
    if output=$("$SCRIPT_DIR/$test_script" "$dir" "$call" 2>&1); then
        echo "PASS"
        PASSED=$((PASSED + 1))
    else
        local last_line
        last_line=$(echo "$output" | tail -1)
        if [[ "$last_line" == SKIP* ]]; then
            echo "SKIP"
            SKIPPED=$((SKIPPED + 1))
        else
            echo "FAIL"
            FAILED=$((FAILED + 1))
            FAILURES+=("$test_name [$workload]")
            # Print last 5 lines of output for context
            echo "$output" | tail -5 | sed 's/^/    /'
        fi
    fi
}

# Determine which tests to run
SELECTED=("$@")
should_run() {
    if [ ${#SELECTED[@]} -eq 0 ]; then return 0; fi
    for s in "${SELECTED[@]}"; do
        if [[ "$1" == *"$s"* ]]; then return 0; fi
    done
    return 1
}

echo "=== Morloc Stress Test Suite ==="
echo ""

for workload in "${WORKLOAD_ORDER[@]}"; do
    if should_run "zombie"; then
        run_test "zombie-stress.sh" "zombie" "$workload"
    fi
    if should_run "concurrent"; then
        run_test "concurrent-stress.sh" "concurrent" "$workload"
    fi
    if should_run "crash"; then
        run_test "crash-recovery.sh" "crash-recovery" "$workload"
    fi
    if should_run "valgrind"; then
        run_test "valgrind-check.sh" "valgrind" "$workload"
    fi
done

echo ""
echo "=== Results ==="
echo "Passed: $PASSED, Failed: $FAILED, Skipped: $SKIPPED"

if (( FAILED > 0 )); then
    echo ""
    echo "Failures:"
    for f in "${FAILURES[@]}"; do
        echo "  - $f"
    done
    exit 1
fi
echo "ALL PASSED"
