#!/usr/bin/env bash
# tier-c-crossvol.sh -- relptrs that span multiple SHM volumes.
#
# The geometric grow allocator doubles volume size on each grow, so a
# sufficiently large bundle places its outer array, the inner arrays,
# and the variable-length strings into different volumes. The relptrs
# stitching them have to encode different vol_idx values to be
# resolvable -- Layer-1's whole reason to exist.
#
# This tier forces that by generating a workload large enough to
# trigger at least 3 volume grows, then verifies sumBig recovers
# the exact fingerprint after a Py-pool identity round-trip.
#
# Usage: ./tier-c-crossvol.sh [outer_count] [str_len]
#   defaults to 8000 256  (~25 MiB voidstar; reliably crosses volumes)

source "$(dirname "$0")/common.sh"

OUTER=${1:-8000}
STRLEN=${2:-256}

echo "=== packet-io tier C: cross-volume relptr round-trip ==="
echo "outer_count=$OUTER  str_len=$STRLEN"
if ! compile_workload; then
    echo "FAIL (compile)"; exit 1
fi

cd "$WORK_DIR"

INIT_SHM=$(count_shm)
INIT_TMP=$(count_tmp)
OVERALL_FAIL=0

# Force RPTR dispatch (no inline) so the data sits in SHM and the
# pool reaches it cross-process.
export MORLOC_INLINE_SIZE=0

if ! time_call "gen -> raw.dat"     "$PKT_DIR/raw.dat"     ./nexus -f packet -z 0 genBig "$OUTER" "$STRLEN"; then
    echo "FAIL (gen)"; exit 1
fi
INPUT_SIZE=$(stat -c '%s' "$PKT_DIR/raw.dat")
echo "  input packet size: $INPUT_SIZE bytes"

# Volume count after gen: peek at /dev/shm (best-effort -- the runtime
# may have already torn down after the nexus exits; this is informational
# only).
SHM_PEAK=$(count_shm)
echo "  /dev/shm segments after gen: $SHM_PEAK"

# Round-trip through the Py pool: each step must build a fresh SHM
# population. Relptrs from the input have to resolve through the new
# process's freshly-mapped volumes.
time_call "id  -> id.z0.dat"        "$PKT_DIR/id.z0.dat"   ./nexus -f packet -z 0 identityPy "$PKT_DIR/raw.dat" || OVERALL_FAIL=1
time_call "id  -> id.z1.dat"        "$PKT_DIR/id.z1.dat"   ./nexus -f packet -z 1 identityPy "$PKT_DIR/raw.dat" || OVERALL_FAIL=1

# Reference + observed fingerprint.
time_call "sum raw"                 "$PKT_DIR/sum.raw.txt" ./nexus sumBig "$PKT_DIR/raw.dat" || OVERALL_FAIL=1
time_call "sum id.z0"               "$PKT_DIR/sum.id.z0.txt" ./nexus sumBig "$PKT_DIR/id.z0.dat" || OVERALL_FAIL=1
time_call "sum id.z1"               "$PKT_DIR/sum.id.z1.txt" ./nexus sumBig "$PKT_DIR/id.z1.dat" || OVERALL_FAIL=1

echo ""
echo "=== fingerprint diff ==="
REF="$PKT_DIR/sum.raw.txt"
FAIL=$OVERALL_FAIL
for f in id.z0 id.z1; do
    if diff -q "$REF" "$PKT_DIR/sum.$f.txt" > /dev/null; then
        printf "  %-30s  MATCH\n" "$f"
    else
        printf "  %-30s  DIFF (cross-volume relptr corruption)\n" "$f"
        echo "    expected:"; sed 's/^/      /' "$REF"
        echo "    observed:"; sed 's/^/      /' "$PKT_DIR/sum.$f.txt"
        FAIL=1
    fi
done

sleep 0.1
FINAL_SHM=$(( $(count_shm) - INIT_SHM ))
FINAL_TMP=$(( $(count_tmp) - INIT_TMP ))

echo ""
echo "=== summary ==="
echo "leaked /dev/shm segments:  $FINAL_SHM"
echo "leaked /tmp/morloc.* dirs: $FINAL_TMP"
if (( FAIL == 0 && FINAL_SHM == 0 && FINAL_TMP == 0 )); then
    echo "PASS"
    exit 0
fi
echo "FAIL"
exit 1
