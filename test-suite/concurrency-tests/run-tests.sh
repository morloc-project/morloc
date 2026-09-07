#!/usr/bin/env bash
# run-tests.sh - Concurrency test suite for morloc cross-pool dispatch
#
# Tests bidirectional callbacks, deep callback chains, and high-concurrency
# unidirectional calls between Python and R pools.
#
# Usage: ./run-tests.sh [test...]
#   With no arguments, runs all tests. Pass partial names to filter:
#   ./run-tests.sh bidi deep

set -euo pipefail

# Milliseconds since the epoch. GNU date has %N; BSD date, which is what
# macOS ships, does not -- it emits a literal N that then poisons the
# arithmetic it feeds ("value too great for base") and, under errexit,
# takes the suite down before it runs anything. Probe once and fall back to
# python3, which these suites already require.
if [ "$(date +%N 2>/dev/null)" = "N" ]; then
    now_ms() { python3 -c 'import time; print(int(time.time() * 1000))'; }
else
    now_ms() { echo $(( $(date +%s%N) / 1000000 )); }
fi


SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TIMEOUT=10

PASSED=0
FAILED=0
TOTAL=0
FAILURES=()

if [[ -t 1 ]]; then
    GREEN=$'\033[32m' RED=$'\033[31m' YELLOW=$'\033[33m' BOLD=$'\033[1m' RESET=$'\033[0m'
else
    GREEN='' RED='' YELLOW='' BOLD='' RESET=''
fi

# Extract exported subcommands from a .loc file
get_exports() {
    local loc_file="$1"
    # Parse the module export list: module name (export1, export2, ...)
    head -1 "$loc_file" \
        | sed 's/^module[[:space:]]*[^(]*(//; s/).*//; s/,/ /g' \
        | tr -s ' '
}

run_single_test() {
    local loc_file="$1"
    local subcommand="$2"
    local work_dir="$3"

    TOTAL=$((TOTAL + 1))
    local test_label
    test_label="$(basename "$loc_file" .loc):$subcommand"
    printf "  %-35s " "$test_label"

    local output start_time elapsed rc
    start_time=$(now_ms)
    output=$(cd "$work_dir" && timeout "$TIMEOUT" ./nexus "$subcommand" 2>&1) && rc=0 || rc=$?
    elapsed=$(( $(now_ms) - start_time ))

    if [[ $rc -eq 0 ]]; then
        if (( elapsed >= 1000 )); then
            printf "%sPASS%s (%d.%01ds)\n" "$GREEN" "$RESET" "$((elapsed/1000))" "$(( (elapsed%1000) / 100 ))"
        else
            printf "%sPASS%s (%dms)\n" "$GREEN" "$RESET" "$elapsed"
        fi
        PASSED=$((PASSED + 1))
    elif [[ $rc -eq 124 ]]; then
        printf "%sTIMEOUT%s (>${TIMEOUT}s)\n" "$RED" "$RESET"
        FAILED=$((FAILED + 1))
        FAILURES+=("$test_label (timeout)")
    else
        printf "%sFAIL%s (rc=$rc)\n" "$RED" "$RESET"
        FAILED=$((FAILED + 1))
        FAILURES+=("$test_label")
        echo "$output" | tail -3 | sed 's/^/      /'
    fi
}

compile_and_run() {
    local loc_file="$1"
    local loc_basename
    loc_basename="$(basename "$loc_file")"
    local test_name="${loc_basename%.loc}"

    echo "${BOLD}[$test_name]${RESET}"

    local work_dir
    work_dir=$(mktemp -d)
    trap "rm -rf '$work_dir'" RETURN

    cp "$loc_file" "$work_dir/"
    cp "$SCRIPT_DIR/helpers"/* "$work_dir/" 2>/dev/null || true

    # Compile
    if ! (cd "$work_dir" && morloc make -o nexus "$loc_basename" > /dev/null 2>&1); then
        printf "  %-35s %sCOMPILE FAIL%s\n" "$test_name" "$RED" "$RESET"
        local exports
        exports=$(get_exports "$loc_file")
        for sub in $exports; do
            TOTAL=$((TOTAL + 1))
            FAILED=$((FAILED + 1))
            FAILURES+=("$test_name:$sub (compile)")
        done
        echo ""
        return
    fi

    local exports
    exports=$(get_exports "$loc_file")
    for sub in $exports; do
        run_single_test "$loc_file" "$sub" "$work_dir"
    done
    echo ""
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

echo "=== Morloc Concurrency Test Suite ==="
echo "Timeout: ${TIMEOUT}s per subcommand"
echo ""

for loc_file in "$SCRIPT_DIR"/*.loc; do
    test_name="$(basename "$loc_file" .loc)"
    if should_run "$test_name"; then
        compile_and_run "$loc_file"
    fi
done

echo "=== Results ==="
echo "${GREEN}Passed: $PASSED${RESET}, ${RED}Failed: $FAILED${RESET}, Total: $TOTAL"

if (( FAILED > 0 )); then
    echo ""
    echo "${RED}Failures:${RESET}"
    for f in "${FAILURES[@]}"; do
        echo "  ${RED}-${RESET} $f"
    done
    exit 1
fi
echo "${GREEN}${BOLD}ALL PASSED${RESET}"
