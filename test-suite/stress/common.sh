#!/usr/bin/env bash
# common.sh - Shared setup for stress tests
# Source this file: source "$(dirname "$0")/common.sh"
#
# After sourcing, the caller should call:
#   parse_args "$@"
#   compile_workload
#
# This sets:
#   WORK_DIR   - temp directory containing compiled nexus
#   CALLS[@]   - array of "subcommand args..." strings to invoke

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
WORK_DIR=$(mktemp -d)
NEXUS_TIMEOUT=15
STDERR_LOG="$SCRIPT_DIR/stress-test.log"
STRESS_SCRIPT="$(basename "${0}")"

cleanup() {
    jobs -p 2>/dev/null | xargs -r kill -9 2>/dev/null || true
    wait 2>/dev/null || true
    rm -rf "$WORK_DIR"
}
trap cleanup EXIT

count_zombies() {
    local n
    n=$(ps -eo stat 2>/dev/null | grep -c '^Z') || true
    echo "${n:-0}"
}

count_shm() {
    local n=0
    if ls /dev/shm/morloc-* &>/dev/null; then
        n=$(ls -1 /dev/shm/morloc-* 2>/dev/null | wc -l)
    fi
    echo "$n"
}

count_tmp() {
    local n=0
    if ls -d /tmp/morloc.* &>/dev/null; then
        n=$(ls -1d /tmp/morloc.* 2>/dev/null | wc -l)
    fi
    echo "$n"
}

# Parse: <test_dir> <call> [<call> ...]
# Each <call> is a quoted string like "foo '[1,2,3]'"
parse_args() {
    if [ $# -lt 2 ]; then
        echo "Usage: $(basename "$0") <golden-test-dir> <call> [<call> ...]" >&2
        echo "  <call> is a nexus invocation, e.g. \"foo '[1,2,3]'\"" >&2
        exit 1
    fi
    TEST_DIR="$(cd "$1" && pwd)"
    shift
    CALLS=("$@")
}

compile_workload() {
    local test_name
    test_name=$(basename "$TEST_DIR")

    # Extract the morloc source file from the Makefile
    local loc_file
    loc_file=$(grep 'morloc make' "$TEST_DIR/Makefile" | head -1 | grep -oP '[^ ]+\.loc')

    cp "$TEST_DIR"/*.loc "$WORK_DIR"/ 2>/dev/null || true
    cp "$TEST_DIR"/*.py "$WORK_DIR"/ 2>/dev/null || true
    cp "$TEST_DIR"/*.hpp "$WORK_DIR"/ 2>/dev/null || true
    cp "$TEST_DIR"/*.R "$WORK_DIR"/ 2>/dev/null || true

    cd "$WORK_DIR"
    echo "Compiling $test_name ($loc_file)..."
    morloc make "$loc_file" > /dev/null 2>&1
    echo "Done. Calls: ${CALLS[*]}"
}

# Run a random call from CALLS[@], logging stderr to STDERR_LOG
run_nexus() {
    local call="${CALLS[RANDOM % ${#CALLS[@]}]}"
    local _tmp_err
    _tmp_err=$(mktemp)
    eval timeout "$NEXUS_TIMEOUT" ./nexus $call > /dev/null 2>"$_tmp_err"
    local _rc=$?
    if [ -s "$_tmp_err" ]; then
        {
            printf "=== %s | %s | call: %s | %s ===\n" \
                "$STRESS_SCRIPT" "$(basename "$TEST_DIR")" "$call" "$(date '+%H:%M:%S')"
            cat "$_tmp_err"
            echo ""
        } >> "$STDERR_LOG"
    fi
    rm -f "$_tmp_err"
    return $_rc
}
