#!/usr/bin/env bash
# run-all.sh -- run every packet-io tier in sequence.
#
# Each tier compiles its own workload (cheap once morloc is installed)
# and is independently runnable. Failures bubble up; per-tier wall time
# is printed in the summary at the end.

set -u

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
    t0=$(date +%s%N)
    if bash "$SCRIPT_DIR/$tier"; then
        rc=0
    else
        rc=$?
        OVERALL_RC=1
    fi
    t1=$(date +%s%N)
    wall=$(( (t1 - t0) / 1000000 ))
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
