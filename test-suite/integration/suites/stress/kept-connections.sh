#!/usr/bin/env bash
# kept-connections.sh - concurrent calls on kept connections into fork pools.
#
# Runs the `parallel` call of golden pool-kept-connections (four C++ threads
# calling Python, each call calling R, workers revoking held connections as
# they accept others) in several concurrent loops. Every run must exit 0 with
# the exact result: a call on a revoked connection is retried and runs once
# (NET-7, NET-11), never reported as the callee's death.
#
# Usage: ./kept-connections.sh [-- loops runs]   (default 4 loops x 15 runs)

source "$(dirname "$0")/common.sh"

LOOPS=4
RUNS=15
if [ "${1:-}" = "--" ]; then
    LOOPS=${2:-4}
    RUNS=${3:-15}
fi
TEST_DIR="$(cd "$SCRIPT_DIR/../../../golden/runtime/pool-kept-connections" && pwd)"
CALLS=("parallel 400")
EXPECTED=159600

echo "=== Kept Connection Stress Test ==="
echo "Loops: $LOOPS, Runs per loop: $RUNS"
compile_workload

loop() {
    local l=$1 i out rc
    for i in $(seq 1 "$RUNS"); do
        rc=0
        out=$(MORLOC_POOL_IDLE_TIMEOUT_MS=0 timeout 120 ./nexus parallel 400 2>"$WORK_DIR/err.$l.$i") || rc=$?
        if [ "$rc" -ne 0 ] || [ "$out" != "$EXPECTED" ]; then
            echo "loop $l run $i: exit $rc, output '$out'"
            sed 's/^/    /' "$WORK_DIR/err.$l.$i" | head -5
        fi
        echo "ran"
    done
}

for l in $(seq 1 "$LOOPS"); do
    loop "$l" > "$WORK_DIR/loop.$l" &
done
wait

FAILURES=$(cat "$WORK_DIR"/loop.* | grep -c '^loop ' || true)
RAN=$(cat "$WORK_DIR"/loop.* | grep -c '^ran$' || true)
grep -hv '^ran$' "$WORK_DIR"/loop.* || true
echo "$FAILURES failures in $RAN of $((LOOPS * RUNS)) runs"
if [ "$FAILURES" -ne 0 ] || [ "$RAN" -ne $((LOOPS * RUNS)) ]; then
    exit 1
fi
echo "PASS"
