#!/usr/bin/env bash
# tier-e-stress.sh -- size sweep + leak audit.
#
# Sweeps through workload sizes that straddle the wire-format
# transitions:
#   * MORLOC_INLINE_THRESHOLD = 64 KiB (inline MESG <-> RPTR boundary)
#   * Layer-2 fast-path threshold     = 1 MiB (legacy reader <-> pread)
# At each size, runs gen -> id -> sum and verifies fingerprint match
# plus no leaked SHM segments or temp dirs.
#
# Usage: ./tier-e-stress.sh

source "$(dirname "$0")/common.sh"

echo "=== packet-io tier E: size sweep + leak audit ==="
if ! compile_workload; then
    echo "FAIL (compile)"; exit 1
fi

cd "$WORK_DIR"

# (outer_count, str_len) pairs that produce payloads spanning
# 1 KiB .. ~100 MiB. Picked to ensure samples on both sides of the
# 64 KiB and 1 MiB transitions.
declare -a SIZES=(
    "2     0"     # ~ 1 KiB
    "20    0"    # ~10 KiB
    "40    32"   # ~80 KiB  (around inline threshold)
    "200   16"   # ~250 KiB
    "200   256"  # ~ 1.5 MiB (around Layer-2 fast-path threshold)
    "2000  256"  # ~15 MiB
)

OVERALL_FAIL=0
INIT_SHM=$(count_shm)
INIT_TMP=$(count_tmp)

for spec in "${SIZES[@]}"; do
    read -r OUTER STRLEN <<< "$spec"
    echo ""
    echo "--- size: outer=$OUTER  str_len=$STRLEN ---"
    rm -f "$PKT_DIR"/*

    BEFORE_SHM=$(count_shm)
    BEFORE_TMP=$(count_tmp)

    time_call "gen -> raw.dat"      "$PKT_DIR/raw.dat"     ./nexus -f packet -z 0 genBig "$OUTER" "$STRLEN" || OVERALL_FAIL=1
    time_call "id  -> id.dat"       "$PKT_DIR/id.dat"      ./nexus -f packet -z 0 identityPy "$PKT_DIR/raw.dat" || OVERALL_FAIL=1
    time_call "id  -> id.z1.dat"    "$PKT_DIR/id.z1.dat"   ./nexus -f packet -z 1 identityPy "$PKT_DIR/raw.dat" || OVERALL_FAIL=1

    time_call "sum raw"             "$PKT_DIR/sum.raw.txt"     ./nexus sumBig "$PKT_DIR/raw.dat"     || OVERALL_FAIL=1
    time_call "sum id"              "$PKT_DIR/sum.id.txt"      ./nexus sumBig "$PKT_DIR/id.dat"      || OVERALL_FAIL=1
    time_call "sum id.z1"           "$PKT_DIR/sum.id.z1.txt"   ./nexus sumBig "$PKT_DIR/id.z1.dat"   || OVERALL_FAIL=1

    REF="$PKT_DIR/sum.raw.txt"
    for f in id id.z1; do
        if diff -q "$REF" "$PKT_DIR/sum.$f.txt" > /dev/null; then
            printf "  fingerprint %-15s  MATCH\n" "$f"
        else
            printf "  fingerprint %-15s  DIFF\n" "$f"
            OVERALL_FAIL=1
        fi
    done

    sleep 0.1
    AFTER_SHM=$(count_shm)
    AFTER_TMP=$(count_tmp)
    SHM_DELTA=$(( AFTER_SHM - BEFORE_SHM ))
    TMP_DELTA=$(( AFTER_TMP - BEFORE_TMP ))
    printf "  leak probe shm=%+d tmp=%+d\n" "$SHM_DELTA" "$TMP_DELTA"
    if (( SHM_DELTA > 0 || TMP_DELTA > 0 )); then
        OVERALL_FAIL=1
    fi
done

sleep 0.2
FINAL_SHM=$(( $(count_shm) - INIT_SHM ))
FINAL_TMP=$(( $(count_tmp) - INIT_TMP ))

echo ""
echo "=== summary ==="
echo "leaked /dev/shm segments:  $FINAL_SHM"
echo "leaked /tmp/morloc.* dirs: $FINAL_TMP"
if (( OVERALL_FAIL == 0 && FINAL_SHM == 0 && FINAL_TMP == 0 )); then
    echo "PASS"
    exit 0
fi
echo "FAIL"
exit 1
