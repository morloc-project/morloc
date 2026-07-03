#!/usr/bin/env bash
# run-tests.sh -- exercise the `expose` field end-to-end for C++, Python, R.
#
# For each language a fixture module declares files in its package.yaml
# `expose:` field. The runner installs the fixture, asserts that the
# declared files land at the per-language well-known paths under
# $MORLOC_HOME, builds a consumer that references the exposed paths in
# the canonical downstream form, runs the consumer, and finally
# uninstalls the fixture (which should also wipe the exposed copies).
#
# Module names start with `morloc-test-` so they will not collide with
# any modules the user has intentionally installed.
#
# Usage: ./run-tests.sh [cpp|py|r ...]
#   With no arguments, all three test groups run.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

MORLOC_HOME="${MORLOC_HOME:-$HOME/.local/share/morloc}"
INCLUDE_DIR="$MORLOC_HOME/include"
PY_LIB_DIR="$MORLOC_HOME/lib/python"
R_LIB_DIR="$MORLOC_HOME/lib/R"
SRC_DIR="$MORLOC_HOME/src/morloc/plane/default"

CPP_MOD="morloc-test-expose-cpp"
PY_MOD="morloc-test-expose-py"
PY_MOD_PY="morloc_test_expose_py"   # hyphen -> underscore (Python identifier)
R_MOD="morloc-test-expose-r"

PASSED=0
FAILED=0
TOTAL=0
FAILURES=()

if [[ -t 1 ]]; then
    GREEN=$'\033[32m' RED=$'\033[31m' BOLD=$'\033[1m' RESET=$'\033[0m'
else
    GREEN='' RED='' BOLD='' RESET=''
fi

# Modules touched by this script, for cleanup via trap.
TOUCHED_MODULES=("$CPP_MOD" "$PY_MOD" "$R_MOD")

# Belt-and-suspenders cleanup. `morloc uninstall` is the right primitive
# (it wipes the install dir, the fdb manifest, and the per-language
# exposed dirs), but rm -rf the same paths as a backstop in case the
# binary is half-broken during development of this very feature.
cleanup_module() {
    local name="$1"
    morloc uninstall --module "$name" >/dev/null 2>&1 || true
    rm -rf "$SRC_DIR/$name" \
           "$INCLUDE_DIR/$name" \
           "$R_LIB_DIR/$name" \
           "$MORLOC_HOME/fdb/$name.module"
    # Python form replaces hyphens with underscores.
    local py_name="${name//-/_}"
    rm -rf "$PY_LIB_DIR/$py_name"
}

cleanup_all() {
    for m in "${TOUCHED_MODULES[@]}"; do
        cleanup_module "$m"
    done
}

trap cleanup_all EXIT

# ======================================================================
# Assertion helpers
# ======================================================================

assert_eq() {
    local label="$1"; local expected="$2"; local actual="$3"
    TOTAL=$((TOTAL + 1))
    printf "  %-60s " "$label"
    if [[ "$actual" == "$expected" ]]; then
        printf "%sPASS%s\n" "$GREEN" "$RESET"
        PASSED=$((PASSED + 1))
    else
        printf "%sFAIL%s\n" "$RED" "$RESET"
        FAILED=$((FAILED + 1))
        FAILURES+=("$label")
        echo "      expected: $expected"
        echo "      actual:   $actual"
    fi
}

assert_file() {
    local label="$1"; local p="$2"
    TOTAL=$((TOTAL + 1))
    printf "  %-60s " "$label"
    if [[ -f "$p" ]]; then
        printf "%sPASS%s\n" "$GREEN" "$RESET"
        PASSED=$((PASSED + 1))
    else
        printf "%sFAIL%s\n" "$RED" "$RESET"
        FAILED=$((FAILED + 1))
        FAILURES+=("$label")
        echo "      file not found: $p"
    fi
}

assert_absent() {
    local label="$1"; local p="$2"
    TOTAL=$((TOTAL + 1))
    printf "  %-60s " "$label"
    if [[ ! -e "$p" ]]; then
        printf "%sPASS%s\n" "$GREEN" "$RESET"
        PASSED=$((PASSED + 1))
    else
        printf "%sFAIL%s\n" "$RED" "$RESET"
        FAILED=$((FAILED + 1))
        FAILURES+=("$label")
        echo "      should not exist: $p"
    fi
}

should_run() {
    local name="$1"
    if [[ ${#FILTERS[@]} -eq 0 ]]; then return 0; fi
    for pat in "${FILTERS[@]}"; do
        [[ "$name" == *"$pat"* ]] && return 0
    done
    return 1
}

run_consumer() {
    # Build the consumer to a tmp dir (so the test is reentrant) and run
    # its `result` export with arg 7. compute = doubler(util_add_one(7))
    # = doubler(8) = 16 in every language.
    local consumer_dir="$1"
    local label="$2"
    local work
    work=$(mktemp -d)
    cp -r "$consumer_dir"/. "$work"/
    local build_err
    if ! build_err=$(cd "$work" && morloc make -o exe main.loc 2>&1); then
        FAILED=$((FAILED + 1))
        TOTAL=$((TOTAL + 1))
        FAILURES+=("$label: build")
        printf "  %-60s %sFAIL%s\n" "$label: build" "$RED" "$RESET"
        echo "$build_err" | tail -10 | sed 's/^/      /'
        rm -rf "$work"
        return 1
    fi
    local actual
    actual=$("$work/exe" result 7 2>&1) || actual="ERROR rc=$?"
    assert_eq "$label: output" "16" "$actual"
    rm -rf "$work"
}

FILTERS=("$@")

echo "${BOLD}morloc expose tests${RESET}"
echo ""

# ======================================================================
# C++ test
# ======================================================================

if should_run "cpp"; then
    echo "${BOLD}[cpp] expose.cpp: namespaced headers + relative co-located include${RESET}"
    cleanup_module "$CPP_MOD"

    # Install fixture
    if ! morloc install --force "$SCRIPT_DIR/morloc-test-expose-cpp" >/dev/null 2>&1; then
        FAILED=$((FAILED + 1)); TOTAL=$((TOTAL + 1))
        FAILURES+=("cpp: install")
        printf "  %-60s %sFAIL%s\n" "cpp: install" "$RED" "$RESET"
        morloc install --force "$SCRIPT_DIR/morloc-test-expose-cpp" 2>&1 | tail -10 | sed 's/^/      /' || true
    else
        # Exposed files land under $MORLOC_HOME/include/<module>/.
        assert_file "cpp: foo.hpp exposed"        "$INCLUDE_DIR/$CPP_MOD/foo.hpp"
        assert_file "cpp: foo_helpers/util.hpp exposed" \
                    "$INCLUDE_DIR/$CPP_MOD/foo_helpers/util.hpp"

        # Consumer's src.hpp does #include "morloc-test-expose-cpp/foo.hpp";
        # foo.hpp's relative #include "foo_helpers/util.hpp" resolves via
        # the includer-directory rule.
        run_consumer "$SCRIPT_DIR/consumer-cpp" "cpp"
    fi

    # `morloc uninstall` should wipe the exposed dir, symmetric with install.
    morloc uninstall --module "$CPP_MOD" >/dev/null 2>&1 || true
    assert_absent "cpp: exposed dir gone after uninstall" "$INCLUDE_DIR/$CPP_MOD"
    echo ""
fi

# ======================================================================
# Python test
# ======================================================================

if should_run "py"; then
    echo "${BOLD}[py] expose.py: subpackage tree + relative imports${RESET}"
    cleanup_module "$PY_MOD"

    if ! morloc install --force "$SCRIPT_DIR/morloc-test-expose-py" >/dev/null 2>&1; then
        FAILED=$((FAILED + 1)); TOTAL=$((TOTAL + 1))
        FAILURES+=("py: install")
        printf "  %-60s %sFAIL%s\n" "py: install" "$RED" "$RESET"
        morloc install --force "$SCRIPT_DIR/morloc-test-expose-py" 2>&1 | tail -10 | sed 's/^/      /' || true
    else
        # Hyphen-to-underscore for the destination dir under lib/python.
        assert_file "py: top-level __init__.py exposed"  "$PY_LIB_DIR/$PY_MOD_PY/__init__.py"
        assert_file "py: pkg/__init__.py exposed"        "$PY_LIB_DIR/$PY_MOD_PY/pkg/__init__.py"
        assert_file "py: pkg/helpers.py exposed"         "$PY_LIB_DIR/$PY_MOD_PY/pkg/helpers.py"
        assert_file "py: pkg/sub/__init__.py exposed"    "$PY_LIB_DIR/$PY_MOD_PY/pkg/sub/__init__.py"
        assert_file "py: pkg/sub/util.py exposed"        "$PY_LIB_DIR/$PY_MOD_PY/pkg/sub/util.py"

        # Consumer's src.py does
        #   from morloc_test_expose_py.pkg.sub.util import util_add_one, doubler
        # which exercises hyphen-to-underscore AND a Python relative
        # import inside util.py (`from ..helpers import add_one`).
        run_consumer "$SCRIPT_DIR/consumer-py" "py"
    fi

    morloc uninstall --module "$PY_MOD" >/dev/null 2>&1 || true
    assert_absent "py: exposed dir gone after uninstall" "$PY_LIB_DIR/$PY_MOD_PY"
    echo ""
fi

# ======================================================================
# R test
# ======================================================================

if should_run "r"; then
    echo "${BOLD}[r] expose.r: namespaced scripts + relative source()${RESET}"
    cleanup_module "$R_MOD"

    if ! morloc install --force "$SCRIPT_DIR/morloc-test-expose-r" >/dev/null 2>&1; then
        FAILED=$((FAILED + 1)); TOTAL=$((TOTAL + 1))
        FAILURES+=("r: install")
        printf "  %-60s %sFAIL%s\n" "r: install" "$RED" "$RESET"
        morloc install --force "$SCRIPT_DIR/morloc-test-expose-r" 2>&1 | tail -10 | sed 's/^/      /' || true
    else
        assert_file "r: util.R exposed"    "$R_LIB_DIR/$R_MOD/util.R"
        assert_file "r: helpers.R exposed" "$R_LIB_DIR/$R_MOD/helpers.R"

        # Consumer's src.R does .morloc.source("morloc-test-expose-r/util.R");
        # util.R's relative source("helpers.R") resolves via chdir=TRUE.
        run_consumer "$SCRIPT_DIR/consumer-r" "r"
    fi

    morloc uninstall --module "$R_MOD" >/dev/null 2>&1 || true
    assert_absent "r: exposed dir gone after uninstall" "$R_LIB_DIR/$R_MOD"
    echo ""
fi

# ======================================================================
# Results
# ======================================================================

echo "=== Results ==="
echo "${GREEN}Passed: $PASSED${RESET}, ${RED}Failed: $FAILED${RESET}, Total: $TOTAL"

if (( FAILED > 0 )); then
    echo ""
    echo "${RED}Failures:${RESET}"
    for f in "${FAILURES[@]}"; do
        echo "  ${RED}-${RESET} $f"
    done
    exit 1
fi
echo "${GREEN}${BOLD}ALL PASSED${RESET}"
