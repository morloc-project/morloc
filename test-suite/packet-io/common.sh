#!/usr/bin/env bash
# common.sh -- shared setup for packet-io rugged tests.
#
# Source from each tier's runner:
#   source "$(dirname "$0")/common.sh"
#
# Provides:
#   WORK_DIR        -- temp dir holding the compiled nexus + pools
#   PKT_DIR         -- subdir for packet artifacts (so tests can rm at will)
#   compile_workload -- builds the standard packet-io workload once
#   time_call <label> <cmd...> -- runs cmd, prints "  label  Xs  bytes=Y"
#   count_shm / count_tmp -- resource leak probes

# Intentionally NOT set -e: every tier wants to keep running after a
# failed step so the summary at the bottom can report which step broke.
# Each tier tracks failures explicitly via OVERALL_FAIL / rc checks.
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
WORK_DIR=$(mktemp -d -t morloc-packet-io.XXXXXX)
PKT_DIR="$WORK_DIR/pkts"
mkdir -p "$PKT_DIR"

# A long-running tier shouldn't fail on a transient timeout, but
# individual probes use a smaller TIMEOUT below.
TIMEOUT_SHORT=60
TIMEOUT_LONG=600

cleanup() {
    jobs -p 2>/dev/null | xargs -r kill -9 2>/dev/null || true
    wait 2>/dev/null || true
    rm -rf "$WORK_DIR"
}
trap cleanup EXIT

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

# Run a command and capture (wall_seconds, exit_code, output_size_bytes).
# Output goes to the file named after $1 (or /dev/null if $1 is "-").
# Side effects: writes "$WORK_DIR/.last_wall" / .last_rc / .last_size
time_call() {
    local label="$1"; shift
    local outfile="$1"; shift
    local start_ns end_ns wall_ms rc=0 size
    start_ns=$(date +%s%N)
    # `|| rc=$?` captures the exit code without bouncing off `set -e`
    # if the caller has it on.
    if [ "$outfile" = "-" ]; then
        timeout "$TIMEOUT_LONG" "$@" > /dev/null 2>>"$WORK_DIR/stderr.log" || rc=$?
    else
        timeout "$TIMEOUT_LONG" "$@" > "$outfile" 2>>"$WORK_DIR/stderr.log" || rc=$?
    fi
    end_ns=$(date +%s%N)
    wall_ms=$(( (end_ns - start_ns) / 1000000 ))
    if [ "$outfile" = "-" ]; then
        size=0
    else
        size=$(stat -c '%s' "$outfile" 2>/dev/null || echo 0)
    fi
    printf "  %-40s  %6d ms  rc=%d  bytes=%d\n" "$label" "$wall_ms" "$rc" "$size"
    echo "$wall_ms" > "$WORK_DIR/.last_wall"
    echo "$rc"      > "$WORK_DIR/.last_rc"
    echo "$size"    > "$WORK_DIR/.last_size"
    return $rc
}

# Compile the packet-io workload exactly once. Assumes `morloc make`
# and the standard root-py / root-cpp libraries are reachable.
compile_workload() {
    cp -r "$SCRIPT_DIR/workload/." "$WORK_DIR/"
    pushd "$WORK_DIR" > /dev/null
    echo "Compiling workload (morloc make main.loc)..."
    local t0 t1
    t0=$(date +%s%N)
    if ! morloc make -o nexus main.loc 2>"$WORK_DIR/build.err"; then
        echo "FAIL: morloc make failed; see $WORK_DIR/build.err" >&2
        sed -n '1,40p' "$WORK_DIR/build.err" >&2
        popd > /dev/null
        return 1
    fi
    t1=$(date +%s%N)
    printf "  %-40s  %6d ms\n" "morloc make" "$(( (t1 - t0) / 1000000 ))"
    popd > /dev/null
}
