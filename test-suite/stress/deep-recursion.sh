#!/usr/bin/env bash
# deep-recursion.sh - recursion depth in every pool language.
#
# Builds the modules under deep-recursion/ (one single-pool instance per
# language from tree.loc.tmpl, plus a Python/C++ cross-pool module) and runs
# each exported function at a depth well past any host stack. Tail-recursive
# morloc functions lower to native loops, so they must succeed at any depth;
# the non-tail forms run shallow, as correctness checks only.
#
# Every case has an expected result. A case known to fail is marked with the
# issue that tracks it and is reported XFAIL; the marking is strict, so an
# XPASS fails the suite and says which marking to remove. Both counts are
# printed so a fix is visible as a change in this suite's output.
#
# Usage: ./deep-recursion.sh
#   MORLOC_TEST_LEVEL=long        depth 100000 instead of 10000
#   MORLOC_STRESS_LANGS="py cpp"  restrict the single-pool instances
#
# Runs with the user's default limits: no RUST_MIN_STACK, no ulimit changes.

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SRC="$SCRIPT_DIR/deep-recursion"
WORK_DIR=$(mktemp -d)
trap 'rm -rf "$WORK_DIR"' EXIT

if ! command -v morloc >/dev/null 2>&1; then
    echo "SKIP: morloc not on PATH"
    exit 1
fi

LANGS="${MORLOC_STRESS_LANGS:-py cpp r rust}"
case "${MORLOC_TEST_LEVEL:-short}" in
    long) DEPTH=100000 ;;
    *)    DEPTH=10000 ;;
esac
# Non-tail recursion is bounded by the host stack in every language (R's
# C-stack check trips at 300 levels of `total`); these run shallow.
NONTAIL=200
ROSE=100
# Each nested cross-pool call parks a worker; see issue 93. Kept shallow so
# the case passes and stays quick.
PINGPONG=50
RUN_TIMEOUT=120

if [[ -t 1 ]]; then
    GREEN=$'\033[32m' RED=$'\033[31m' YELLOW=$'\033[33m' RESET=$'\033[0m'
else
    GREEN='' RED='' YELLOW='' RESET=''
fi

PASSED=0 FAILED=0 XFAILED=0 XPASSED=0
PROBLEMS=()

# The depth at which a pool's value can no longer be freed: a chain's
# shared_ptr (C++) or Box (Rust) links unwind one destructor frame per level
# on the 2 MiB worker stack. Python and R free deep values iteratively. The
# (de)serializers themselves walk without recursion (issue 89), so up to this
# depth a tail loop's value crosses a manifold boundary at any size.
drop_limit() {
    case "$1" in
        cpp)  echo 65000 ;;
        *)    echo 0 ;;
    esac
}
# Projecting a field out of a Rust value clones the projected subtree, and
# the derived Clone recurses one frame per level (issue 91); it gives out
# before Rust's own destructor limit would.
clone_limit() {
    case "$1" in
        rust) echo 30000 ;;
        *)    echo 0 ;;
    esac
}

# Print the issue that explains a failure of this case, or nothing.
expected_failure() {
    local module=$1 fn=$2 depth=$3
    case "$module:$fn" in
        rust:chainCount|rust:chainReverse|rust:altCount)
            (( depth >= $(clone_limit rust) )) && echo "#91" ;;
        cpp:chainCount|cpp:chainReverse|cpp:altCount)
            (( depth >= $(drop_limit cpp) )) && echo "recursive-destructor" ;;
        py:roundtrip)
            (( depth >= 6000 )) && echo "#92" ;;
        cross:cppToPy|cross:pyToCpp)
            (( depth >= $(drop_limit cpp) )) && echo "recursive-destructor" ;;
        cross:pingPong)
            (( depth >= 300 )) && echo "#93" ;;
    esac
    return 0
}

# run_case MODULE FN DEPTH EXPECTED [ARG...]
# Runs ./MODULE FN ARG... (default ARG = DEPTH) and compares stdout.
run_case() {
    local module=$1 fn=$2 depth=$3 expected=$4
    shift 4
    local args=("$@")
    [[ ${#args[@]} -eq 0 ]] && args=("$depth")
    local xf out rc verdict
    xf=$(expected_failure "$module" "$fn" "$depth")
    if [[ "$fn" == roundtrip ]]; then
        out=$(cd "$WORK_DIR" && timeout "$RUN_TIMEOUT" ./"$module" chain "$depth" 2>/dev/null \
              | timeout "$RUN_TIMEOUT" ./"$module" treeCount - 2>/dev/null)
    else
        out=$(cd "$WORK_DIR" && timeout "$RUN_TIMEOUT" ./"$module" "$fn" "${args[@]}" 2>/dev/null)
    fi
    rc=$?
    if [[ $rc -eq 0 && "$out" == "$expected" ]]; then
        if [[ -n "$xf" ]]; then
            verdict="${RED}XPASS${RESET} (remove the $xf marking)"
            XPASSED=$((XPASSED + 1)); PROBLEMS+=("$module $fn $depth: XPASS, remove the $xf marking")
        else
            verdict="${GREEN}PASS${RESET}"; PASSED=$((PASSED + 1))
        fi
    else
        local got
        got=$(printf '%s' "$out" | head -c 60)
        [[ $rc -eq 124 ]] && got="timeout after ${RUN_TIMEOUT}s"
        if [[ -n "$xf" ]]; then
            verdict="${YELLOW}XFAIL${RESET} $xf"; XFAILED=$((XFAILED + 1))
        else
            verdict="${RED}FAIL${RESET} (rc=$rc, got '${got:-}', want '$expected')"
            FAILED=$((FAILED + 1)); PROBLEMS+=("$module $fn $depth: got '${got:-}' (rc=$rc), want '$expected'")
        fi
    fi
    printf "  %-5s %-16s %7d ... %s\n" "$module" "$fn" "$depth" "$verdict"
}

build() {
    local name=$1 loc=$2
    printf "  building %-5s ... " "$name"
    if (cd "$WORK_DIR" && morloc make -o "$name" "$loc" > "$name.build.log" 2>&1); then
        echo "ok"
        return 0
    fi
    echo "${RED}FAILED${RESET}"
    tail -5 "$WORK_DIR/$name.build.log" | sed 's/^/    /'
    FAILED=$((FAILED + 1)); PROBLEMS+=("build $name failed")
    return 1
}

echo "=== deep recursion (depth $DEPTH, languages: $LANGS) ==="

BUILT=()
for lang in $LANGS; do
    sed "s/LANG/$lang/g" "$SRC/tree.loc.tmpl" > "$WORK_DIR/$lang.loc"
    build "$lang" "$lang.loc" && BUILT+=("$lang")
done
cp "$SRC"/cross/main.loc "$WORK_DIR/cross.loc"
cp "$SRC"/cross/prim.py "$SRC"/cross/prim.hpp "$WORK_DIR"/
build cross cross.loc && BUILT+=(cross)

for lang in "${BUILT[@]}"; do
    [[ "$lang" == cross ]] && continue
    run_case "$lang" chainCount      "$DEPTH"   "$DEPTH"
    run_case "$lang" chainReverse    "$DEPTH"   "$DEPTH"
    run_case "$lang" altCount        "$DEPTH"   "$DEPTH"
    run_case "$lang" evenDeep        "$DEPTH"   true
    run_case "$lang" chainTotal      "$NONTAIL" "$NONTAIL"
    run_case "$lang" caterpillarSize "$ROSE"    "$((ROSE + 1))"
done
if [[ " ${BUILT[*]} " == *" py "* ]]; then
    run_case py roundtrip "$DEPTH" "$DEPTH"
fi
if [[ " ${BUILT[*]} " == *" cross "* ]]; then
    run_case cross cppToPy      "$DEPTH"    "$DEPTH"
    run_case cross pyToCpp      "$DEPTH"    "$DEPTH"
    run_case cross pingPongTree "$PINGPONG" "$PINGPONG"
    run_case cross pingPong     "$PINGPONG" "$PINGPONG"
    if [[ "${MORLOC_TEST_LEVEL:-short}" == long ]]; then
        run_case cross pingPong 300 300
    fi
fi

echo "passed: $PASSED, failed: $FAILED, xfail: $XFAILED, xpass: $XPASSED"
if (( ${#PROBLEMS[@]} > 0 )); then
    for p in "${PROBLEMS[@]}"; do echo "  ${RED}-${RESET} $p"; done
    exit 1
fi
exit 0
