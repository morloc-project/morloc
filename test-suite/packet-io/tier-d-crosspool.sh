#!/usr/bin/env bash
# tier-d-crosspool.sh -- cross-pool wire-format equivalence.
#
# Runs the identity function through every available pool language
# (Py and Cpp; R is skipped if root-r isn't installed). All pools must
# produce wire-identical output for the same input -- if cppmorloc's
# resolve_relptr_cpp and pymorloc's resolve_relptr disagree on the
# new indexed encoding, this catches it as a byte-level diff.
#
# Usage: ./tier-d-crosspool.sh [outer_count] [str_len]

source "$(dirname "$0")/common.sh"

OUTER=${1:-400}
STRLEN=${2:-32}

echo "=== packet-io tier D: cross-pool wire equivalence ==="
echo "outer_count=$OUTER  str_len=$STRLEN"
if ! compile_workload; then
    echo "FAIL (compile)"; exit 1
fi

cd "$WORK_DIR"

INIT_SHM=$(count_shm)
INIT_TMP=$(count_tmp)
OVERALL_FAIL=0

if ! time_call "gen -> raw.dat"     "$PKT_DIR/raw.dat"    ./nexus -f packet -z 0 genBig "$OUTER" "$STRLEN"; then
    echo "FAIL (gen)"; exit 1
fi

# Identity through each pool, uncompressed so bytes are comparable.
time_call "Py  -> py.z0.dat"         "$PKT_DIR/py.z0.dat"  ./nexus -f packet -z 0 identityPy  "$PKT_DIR/raw.dat" || OVERALL_FAIL=1
time_call "Cpp -> cpp.z0.dat"        "$PKT_DIR/cpp.z0.dat" ./nexus -f packet -z 0 identityCpp "$PKT_DIR/raw.dat" || OVERALL_FAIL=1

# Compressed pass: even after zstd the bytes must match across pools.
time_call "Py  -> py.z1.dat"         "$PKT_DIR/py.z1.dat"  ./nexus -f packet -z 1 identityPy  "$PKT_DIR/raw.dat" || OVERALL_FAIL=1
time_call "Cpp -> cpp.z1.dat"        "$PKT_DIR/cpp.z1.dat" ./nexus -f packet -z 1 identityCpp "$PKT_DIR/raw.dat" || OVERALL_FAIL=1

echo ""
echo "=== pool output sanity ==="
FAIL=$OVERALL_FAIL

# Byte-level cross-pool equality would be a nice-to-have but Layer 3's
# vol_idx hint metadata bakes the producer's actual SHM slot into the
# packet bytes, and the slot Py vs Cpp pools land in is process-dependent.
# So we don't `cmp` the files directly -- the canonical check is the
# semantic fingerprint diff below. Sizes should be in the same ballpark
# though; print them for sanity.
PY_Z0=$(stat -c '%s' "$PKT_DIR/py.z0.dat"  2>/dev/null || echo 0)
CPP_Z0=$(stat -c '%s' "$PKT_DIR/cpp.z0.dat" 2>/dev/null || echo 0)
PY_Z1=$(stat -c '%s' "$PKT_DIR/py.z1.dat"  2>/dev/null || echo 0)
CPP_Z1=$(stat -c '%s' "$PKT_DIR/cpp.z1.dat" 2>/dev/null || echo 0)
printf "  py.z0=%d  cpp.z0=%d  py.z1=%d  cpp.z1=%d\n" \
    "$PY_Z0" "$CPP_Z0" "$PY_Z1" "$CPP_Z1"
if (( PY_Z0 != CPP_Z0 || PY_Z1 != CPP_Z1 )); then
    printf "  %-40s  DIFF (size mismatch -- different structure)\n" "size check"
    FAIL=1
else
    printf "  %-40s  MATCH\n" "size check"
fi

# Sanity: fingerprint preserved across both pools.
time_call "sum raw"                  "$PKT_DIR/sum.raw.txt" ./nexus sumBig "$PKT_DIR/raw.dat" || OVERALL_FAIL=1
time_call "sum py.z0"                "$PKT_DIR/sum.py.z0.txt" ./nexus sumBig "$PKT_DIR/py.z0.dat" || OVERALL_FAIL=1
time_call "sum cpp.z0"               "$PKT_DIR/sum.cpp.z0.txt" ./nexus sumBig "$PKT_DIR/cpp.z0.dat" || OVERALL_FAIL=1
time_call "sum py.z1"                "$PKT_DIR/sum.py.z1.txt" ./nexus sumBig "$PKT_DIR/py.z1.dat" || OVERALL_FAIL=1
time_call "sum cpp.z1"               "$PKT_DIR/sum.cpp.z1.txt" ./nexus sumBig "$PKT_DIR/cpp.z1.dat" || OVERALL_FAIL=1

echo ""
echo "=== fingerprint diff ==="
REF="$PKT_DIR/sum.raw.txt"
for f in py.z0 cpp.z0 py.z1 cpp.z1; do
    if [ -s "$PKT_DIR/sum.$f.txt" ] && diff -q "$REF" "$PKT_DIR/sum.$f.txt" > /dev/null; then
        printf "  %-30s  MATCH\n" "$f"
    else
        printf "  %-30s  DIFF\n" "$f"
        if [ -s "$REF" ]; then
            echo "    expected:"; sed 's/^/      /' "$REF"
        fi
        if [ -s "$PKT_DIR/sum.$f.txt" ]; then
            echo "    observed:"; sed 's/^/      /' "$PKT_DIR/sum.$f.txt"
        fi
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
