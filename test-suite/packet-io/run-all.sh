#!/usr/bin/env bash
# run-all.sh -- run every packet-io tier in sequence.
#
# Each tier compiles its own workload (cheap once morloc is installed)
# and is independently runnable. Failures bubble up; per-tier wall time
# is printed in the summary at the end.

set -u

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

TIERS=(
    "tier-b-roundtrip.sh"
    "tier-c-crossvol.sh"
    "tier-d-crosspool.sh"
    "tier-e-stress.sh"
)

declare -a RESULTS=()
OVERALL_RC=0

for tier in "${TIERS[@]}"; do
    echo ""
    echo "############################################################"
    echo "## $tier"
    echo "############################################################"
    t0=$(now_ms)
    if bash "$SCRIPT_DIR/$tier"; then
        rc=0
    else
        rc=$?
        OVERALL_RC=1
    fi
    t1=$(now_ms)
    wall=$(( t1 - t0 ))
    RESULTS+=("$(printf '%-30s  %7d ms  rc=%d' "$tier" "$wall" "$rc")")
done

echo ""
echo "============================================================"
echo "== packet-io run-all summary"
echo "============================================================"
for line in "${RESULTS[@]}"; do
    echo "$line"
done
if (( OVERALL_RC == 0 )); then
    echo "PASS (all tiers)"
else
    echo "FAIL (one or more tiers)"
fi
exit "$OVERALL_RC"
