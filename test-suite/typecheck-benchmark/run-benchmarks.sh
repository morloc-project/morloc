#!/bin/bash
# Typecheck performance benchmarks
# Usage: ./run-benchmarks.sh [timeout_seconds] [warn_threshold_ms]
#
# All benchmarks should complete well under 1 second. Any result over the
# warning threshold (default 500ms) is flagged as SLOW.

TIMEOUT=${1:-10}
WARN_MS=${2:-500}
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BOLD='\033[1m'
RESET='\033[0m'

pass=0
fail=0

printf "${BOLD}%-25s %10s %8s${RESET}\n" "program" "time (ms)" "status"
printf "%-25s %10s %8s\n" "-------------------------" "----------" "--------"

for loc_file in "$SCRIPT_DIR"/*.loc; do
    name=$(basename "$loc_file" .loc)
    start_ns=$(date +%s%N)
    timeout "$TIMEOUT" morloc typecheck "$loc_file" > /dev/null 2>&1
    exit_code=$?
    end_ns=$(date +%s%N)
    diff_ms=$(( (end_ns - start_ns) / 1000000 ))

    if [ $exit_code -eq 124 ]; then
        color="$RED"
        status="TIMEOUT"
        fail=$((fail + 1))
    elif [ $exit_code -ne 0 ]; then
        color="$RED"
        status="ERROR"
        fail=$((fail + 1))
    elif [ $diff_ms -gt $WARN_MS ]; then
        color="$YELLOW"
        status="SLOW"
        fail=$((fail + 1))
    else
        color="$GREEN"
        status="ok"
        pass=$((pass + 1))
    fi

    printf "%-25s %10d ${color}%8s${RESET}\n" "$name" "$diff_ms" "$status"
done

echo ""
if [ $fail -eq 0 ]; then
    printf "${GREEN}${BOLD}All %d benchmarks passed${RESET} (warn threshold: ${WARN_MS}ms)\n" "$pass"
else
    printf "${RED}${BOLD}%d failed${RESET}, %d passed (warn threshold: ${WARN_MS}ms)\n" "$fail" "$pass"
fi
[ $fail -eq 0 ] && exit 0 || exit 1
