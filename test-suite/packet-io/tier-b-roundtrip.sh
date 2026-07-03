#!/usr/bin/env bash
# tier-b-roundtrip.sh -- single-process packet round-trip.
#
# Asserts: structural fingerprint (sumBig) is identical across
#   (uncompressed, z=1, z=3) and across (round-tripped via identity).
# Catches: any encoding regression in the producer/consumer wire path,
#   including bug #1 (vol_idx hint not handled by legacy reader).
#
# Usage: ./tier-b-roundtrip.sh [outer_count] [str_len]
#   defaults to 200 16 (small, fast: a few seconds wall)

source "$(dirname "$0")/common.sh"

OUTER=${1:-200}
STRLEN=${2:-16}

echo "=== packet-io tier B: single-process round-trip ==="
echo "outer_count=$OUTER  str_len=$STRLEN"
if ! compile_workload; then
    echo "FAIL (compile)"
    exit 1
fi

cd "$WORK_DIR"

INIT_SHM=$(count_shm)
INIT_TMP=$(count_tmp)
OVERALL_FAIL=0

# Step 1: generate the input packet (uncompressed reference). If this
# fails nothing else makes sense.
if ! time_call "gen -> raw.dat"      "$PKT_DIR/raw.dat"    ./nexus -f packet -z 0 genBig "$OUTER" "$STRLEN"; then
    echo "FAIL (gen)"; exit 1
fi

# Step 2: read raw, identity through Py pool, write packets at each
# compression level. Each identity step exercises a complete
# producer/consumer cycle (read raw -> SHM -> RPTR dispatch -> pool ->
# RPTR back -> write packet).
time_call "id  -> id.z0.dat"         "$PKT_DIR/id.z0.dat"  ./nexus -f packet -z 0 identityPy "$PKT_DIR/raw.dat" || OVERALL_FAIL=1
time_call "id  -> id.z1.dat"         "$PKT_DIR/id.z1.dat"  ./nexus -f packet -z 1 identityPy "$PKT_DIR/raw.dat" || OVERALL_FAIL=1
time_call "id  -> id.z3.dat"         "$PKT_DIR/id.z3.dat"  ./nexus -f packet -z 3 identityPy "$PKT_DIR/raw.dat" || OVERALL_FAIL=1

# Step 3: re-encode (transcode through compression boundary). The
# z=3 input gets decompressed by the loader, walked, then re-emitted
# at z=1 -- catches the bug where the decompress path forgot to apply
# the vol_idx hint correction.
time_call "id  -> id.z3.z1.dat"      "$PKT_DIR/id.z3.z1.dat" ./nexus -f packet -z 1 identityPy "$PKT_DIR/id.z3.dat" || OVERALL_FAIL=1

# Step 4: structural fingerprint over each variant. Print to stdout so
# the diff is human-readable on failure.
time_call "sum raw"                  "$PKT_DIR/sum.raw.txt"     ./nexus sumBig "$PKT_DIR/raw.dat"     || OVERALL_FAIL=1
time_call "sum id.z0"                "$PKT_DIR/sum.id.z0.txt"   ./nexus sumBig "$PKT_DIR/id.z0.dat"   || OVERALL_FAIL=1
time_call "sum id.z1"                "$PKT_DIR/sum.id.z1.txt"   ./nexus sumBig "$PKT_DIR/id.z1.dat"   || OVERALL_FAIL=1
time_call "sum id.z3"                "$PKT_DIR/sum.id.z3.txt"   ./nexus sumBig "$PKT_DIR/id.z3.dat"   || OVERALL_FAIL=1
time_call "sum id.z3.z1"             "$PKT_DIR/sum.id.z3.z1.txt" ./nexus sumBig "$PKT_DIR/id.z3.z1.dat" || OVERALL_FAIL=1

# Sanity-check sums match.
echo ""
echo "=== fingerprint diff ==="
REF="$PKT_DIR/sum.raw.txt"
FAIL=$OVERALL_FAIL
for f in id.z0 id.z1 id.z3 id.z3.z1; do
    if diff -q "$REF" "$PKT_DIR/sum.$f.txt" > /dev/null; then
        printf "  %-30s  MATCH\n" "$f"
    else
        printf "  %-30s  DIFF (corruption)\n" "$f"
        echo "    expected:"; sed 's/^/      /' "$REF"
        echo "    observed:"; sed 's/^/      /' "$PKT_DIR/sum.$f.txt"
        FAIL=1
    fi
done

# Resource hygiene.
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
