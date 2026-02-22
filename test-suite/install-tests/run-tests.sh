#!/usr/bin/env bash
# run-tests.sh - Installation test suite for morloc
#
# Tests `morloc make --install` with package.yaml include fields and
# `morloc make --install --include` CLI flags across Python, C++, and R.
#
# Covers:
#   - Source files in cwd vs nested directories (src/)
#   - Directly and indirectly sourced files
#   - Whole-folder includes
#   - CLI --include flag
#
# Usage: ./run-tests.sh [test...]
#   With no arguments, runs all test groups. Pass partial names to filter:
#   ./run-tests.sh testpy testcpp testr

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

MORLOC_HOME="${MORLOC_HOME:-$HOME/.local/share/morloc}"
BIN_DIR="$MORLOC_HOME/bin"
EXE_DIR="$MORLOC_HOME/exe"
FDB_DIR="$MORLOC_HOME/fdb"

PASSED=0
FAILED=0
TOTAL=0
FAILURES=()

if [[ -t 1 ]]; then
    GREEN=$'\033[32m' RED=$'\033[31m' YELLOW=$'\033[33m' BOLD=$'\033[1m' RESET=$'\033[0m'
else
    GREEN='' RED='' YELLOW='' BOLD='' RESET=''
fi

# ======================================================================
# Test helpers
# ======================================================================

assert_test() {
    local label="$1"
    local expected="$2"
    local actual="$3"

    TOTAL=$((TOTAL + 1))
    printf "  %-55s " "$label"

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

assert_file_exists() {
    local label="$1"
    local filepath="$2"

    TOTAL=$((TOTAL + 1))
    printf "  %-55s " "$label"

    if [[ -e "$filepath" ]]; then
        printf "%sPASS%s\n" "$GREEN" "$RESET"
        PASSED=$((PASSED + 1))
    else
        printf "%sFAIL%s\n" "$RED" "$RESET"
        FAILED=$((FAILED + 1))
        FAILURES+=("$label")
        echo "      file not found: $filepath"
    fi
}

assert_dir_exists() {
    local label="$1"
    local dirpath="$2"

    TOTAL=$((TOTAL + 1))
    printf "  %-55s " "$label"

    if [[ -d "$dirpath" ]]; then
        printf "%sPASS%s\n" "$GREEN" "$RESET"
        PASSED=$((PASSED + 1))
    else
        printf "%sFAIL%s\n" "$RED" "$RESET"
        FAILED=$((FAILED + 1))
        FAILURES+=("$label")
        echo "      directory not found: $dirpath"
    fi
}

assert_not_exists() {
    local label="$1"
    local filepath="$2"

    TOTAL=$((TOTAL + 1))
    printf "  %-55s " "$label"

    if [[ ! -e "$filepath" ]]; then
        printf "%sPASS%s\n" "$GREEN" "$RESET"
        PASSED=$((PASSED + 1))
    else
        printf "%sFAIL%s\n" "$RED" "$RESET"
        FAILED=$((FAILED + 1))
        FAILURES+=("$label")
        echo "      should not exist: $filepath"
    fi
}

should_run() {
    local name="$1"
    if [[ $# -eq 0 ]] || [[ ${#FILTERS[@]} -eq 0 ]]; then
        return 0
    fi
    for pat in "${FILTERS[@]}"; do
        if [[ "$name" == *"$pat"* ]]; then
            return 0
        fi
    done
    return 1
}

# Build, install, test, and uninstall a module.
#
# Usage: run_install_test <test_name> <test_dir> <subcommand> <arg> <expected> [extra_make_args...]
#
# test_name:  group label
# test_dir:   directory containing main.loc + sources
# subcommand: the exported function to call
# arg:        argument to pass to the subcommand
# expected:   expected output
# extra_make_args: additional args for `morloc make` (e.g. --include foo.py)
run_install_test() {
    local test_name="$1"
    local test_dir="$2"
    local subcommand="$3"
    local arg="$4"
    local expected="$5"
    shift 5
    local extra_args=("$@")

    local work_dir
    work_dir=$(mktemp -d)

    # Copy test module to a temp working directory
    cp -r "$test_dir"/. "$work_dir"/

    # Build and install
    local build_err
    build_err=$(cd "$work_dir" && morloc make --install --force -o "$test_name" "${extra_args[@]}" main.loc 2>&1) || {
        TOTAL=$((TOTAL + 1))
        printf "  %-55s " "$test_name: build"
        printf "%sFAIL%s\n" "$RED" "$RESET"
        FAILED=$((FAILED + 1))
        FAILURES+=("$test_name: build")
        echo "$build_err" | tail -5 | sed 's/^/      /'
        rm -rf "$work_dir"
        return
    }

    local bin_path="$BIN_DIR/$test_name"
    local exe_path="$EXE_DIR/$test_name"

    # Check binary exists
    assert_file_exists "$test_name: binary installed" "$bin_path"

    # Check exe directory exists
    assert_dir_exists "$test_name: exe directory created" "$exe_path"

    # Check pools directory copied
    assert_dir_exists "$test_name: pools directory copied" "$exe_path/pools"

    # Return included-file checks to the caller via the callback pattern
    # (caller adds assert_file_exists calls after this function)

    # Run the installed program and check output
    local actual
    actual=$("$bin_path" "$subcommand" "$arg" 2>&1) || actual="ERROR: rc=$?"
    assert_test "$test_name: output correct" "$expected" "$actual"

    # Uninstall
    morloc uninstall --program "$test_name" >/dev/null 2>&1 || true

    # Verify uninstall cleaned up
    assert_not_exists "$test_name: binary removed after uninstall" "$bin_path"

    rm -rf "$work_dir"
}

# Collect filter arguments
FILTERS=("$@")

echo "${BOLD}morloc install tests${RESET}"
echo ""

# ======================================================================
# Python tests
# ======================================================================

# --- testpy1: direct source in cwd, include file via package.yaml ---
if should_run "testpy1"; then
    echo "${BOLD}[testpy1] Python: direct source in cwd, include via package.yaml${RESET}"

    TEST_DIR="$SCRIPT_DIR/testpy1"
    WORK_DIR=$(mktemp -d)
    cp -r "$TEST_DIR"/. "$WORK_DIR"/

    BUILD_ERR=$(cd "$WORK_DIR" && morloc make --install --force -o testpy1 main.loc 2>&1) || {
        TOTAL=$((TOTAL + 1))
        printf "  %-55s " "testpy1: build"
        printf "%sFAIL%s\n" "$RED" "$RESET"
        FAILED=$((FAILED + 1))
        FAILURES+=("testpy1: build")
        echo "$BUILD_ERR" | tail -5 | sed 's/^/      /'
        rm -rf "$WORK_DIR"
    }

    if [[ -d "$WORK_DIR" ]]; then
        assert_file_exists "testpy1: binary installed" "$BIN_DIR/testpy1"
        assert_dir_exists  "testpy1: exe directory created" "$EXE_DIR/testpy1"
        assert_dir_exists  "testpy1: pools directory copied" "$EXE_DIR/testpy1/pools"
        assert_file_exists "testpy1: helpers.py included" "$EXE_DIR/testpy1/helpers.py"

        ACTUAL=$("$BIN_DIR/testpy1" pygreet '"world"' 2>&1) || ACTUAL="ERROR: rc=$?"
        assert_test "testpy1: output correct" '"hello world"' "$ACTUAL"

        morloc uninstall --program testpy1 >/dev/null 2>&1 || true
        assert_not_exists "testpy1: cleaned up after uninstall" "$BIN_DIR/testpy1"

        rm -rf "$WORK_DIR"
    fi
    echo ""
fi

# --- testpy2: source in src/, include whole folder via package.yaml ---
if should_run "testpy2"; then
    echo "${BOLD}[testpy2] Python: source in src/, include folder via package.yaml${RESET}"

    TEST_DIR="$SCRIPT_DIR/testpy2"
    WORK_DIR=$(mktemp -d)
    cp -r "$TEST_DIR"/. "$WORK_DIR"/

    BUILD_ERR=$(cd "$WORK_DIR" && morloc make --install --force -o testpy2 main.loc 2>&1) || {
        TOTAL=$((TOTAL + 1))
        printf "  %-55s " "testpy2: build"
        printf "%sFAIL%s\n" "$RED" "$RESET"
        FAILED=$((FAILED + 1))
        FAILURES+=("testpy2: build")
        echo "$BUILD_ERR" | tail -5 | sed 's/^/      /'
        rm -rf "$WORK_DIR"
    }

    if [[ -d "$WORK_DIR" ]]; then
        assert_file_exists "testpy2: binary installed" "$BIN_DIR/testpy2"
        assert_dir_exists  "testpy2: exe directory created" "$EXE_DIR/testpy2"
        assert_dir_exists  "testpy2: src/ folder included" "$EXE_DIR/testpy2/src"
        assert_file_exists "testpy2: src/mathutil.py included" "$EXE_DIR/testpy2/src/mathutil.py"

        ACTUAL=$("$BIN_DIR/testpy2" pyadd '3' '4' 2>&1) || ACTUAL="ERROR: rc=$?"
        assert_test "testpy2: output correct" "7" "$ACTUAL"

        morloc uninstall --program testpy2 >/dev/null 2>&1 || true
        assert_not_exists "testpy2: cleaned up after uninstall" "$BIN_DIR/testpy2"

        rm -rf "$WORK_DIR"
    fi
    echo ""
fi

# --- testpy3: direct source + indirect import, include via --include CLI ---
if should_run "testpy3"; then
    echo "${BOLD}[testpy3] Python: indirect dependency, include via --include CLI${RESET}"

    TEST_DIR="$SCRIPT_DIR/testpy3"
    WORK_DIR=$(mktemp -d)
    cp -r "$TEST_DIR"/. "$WORK_DIR"/

    BUILD_ERR=$(cd "$WORK_DIR" && morloc make --install --force -o testpy3 --include "formatter.py" --include "fmtlib.py" main.loc 2>&1) || {
        TOTAL=$((TOTAL + 1))
        printf "  %-55s " "testpy3: build"
        printf "%sFAIL%s\n" "$RED" "$RESET"
        FAILED=$((FAILED + 1))
        FAILURES+=("testpy3: build")
        echo "$BUILD_ERR" | tail -5 | sed 's/^/      /'
        rm -rf "$WORK_DIR"
    }

    if [[ -d "$WORK_DIR" ]]; then
        assert_file_exists "testpy3: binary installed" "$BIN_DIR/testpy3"
        assert_dir_exists  "testpy3: exe directory created" "$EXE_DIR/testpy3"
        assert_file_exists "testpy3: formatter.py included" "$EXE_DIR/testpy3/formatter.py"
        assert_file_exists "testpy3: fmtlib.py included (indirect)" "$EXE_DIR/testpy3/fmtlib.py"

        ACTUAL=$("$BIN_DIR/testpy3" pyformat '"x"' '5' 2>&1) || ACTUAL="ERROR: rc=$?"
        assert_test "testpy3: output correct" '"x=5"' "$ACTUAL"

        morloc uninstall --program testpy3 >/dev/null 2>&1 || true
        assert_not_exists "testpy3: cleaned up after uninstall" "$BIN_DIR/testpy3"

        rm -rf "$WORK_DIR"
    fi
    echo ""
fi

# ======================================================================
# C++ tests
# ======================================================================

# --- testcpp1: direct source in cwd, include file via package.yaml ---
if should_run "testcpp1"; then
    echo "${BOLD}[testcpp1] C++: direct source in cwd, include via package.yaml${RESET}"

    TEST_DIR="$SCRIPT_DIR/testcpp1"
    WORK_DIR=$(mktemp -d)
    cp -r "$TEST_DIR"/. "$WORK_DIR"/

    BUILD_ERR=$(cd "$WORK_DIR" && morloc make --install --force -o testcpp1 main.loc 2>&1) || {
        TOTAL=$((TOTAL + 1))
        printf "  %-55s " "testcpp1: build"
        printf "%sFAIL%s\n" "$RED" "$RESET"
        FAILED=$((FAILED + 1))
        FAILURES+=("testcpp1: build")
        echo "$BUILD_ERR" | tail -5 | sed 's/^/      /'
        rm -rf "$WORK_DIR"
    }

    if [[ -d "$WORK_DIR" ]]; then
        assert_file_exists "testcpp1: binary installed" "$BIN_DIR/testcpp1"
        assert_dir_exists  "testcpp1: exe directory created" "$EXE_DIR/testcpp1"
        assert_dir_exists  "testcpp1: pools directory copied" "$EXE_DIR/testcpp1/pools"
        assert_file_exists "testcpp1: square.hpp included" "$EXE_DIR/testcpp1/square.hpp"

        ACTUAL=$("$BIN_DIR/testcpp1" cppsquare '7' 2>&1) || ACTUAL="ERROR: rc=$?"
        assert_test "testcpp1: output correct" "49" "$ACTUAL"

        morloc uninstall --program testcpp1 >/dev/null 2>&1 || true
        assert_not_exists "testcpp1: cleaned up after uninstall" "$BIN_DIR/testcpp1"

        rm -rf "$WORK_DIR"
    fi
    echo ""
fi

# --- testcpp2: source in src/, include whole folder via package.yaml ---
if should_run "testcpp2"; then
    echo "${BOLD}[testcpp2] C++: source in src/, include folder via package.yaml${RESET}"

    TEST_DIR="$SCRIPT_DIR/testcpp2"
    WORK_DIR=$(mktemp -d)
    cp -r "$TEST_DIR"/. "$WORK_DIR"/

    BUILD_ERR=$(cd "$WORK_DIR" && morloc make --install --force -o testcpp2 main.loc 2>&1) || {
        TOTAL=$((TOTAL + 1))
        printf "  %-55s " "testcpp2: build"
        printf "%sFAIL%s\n" "$RED" "$RESET"
        FAILED=$((FAILED + 1))
        FAILURES+=("testcpp2: build")
        echo "$BUILD_ERR" | tail -5 | sed 's/^/      /'
        rm -rf "$WORK_DIR"
    }

    if [[ -d "$WORK_DIR" ]]; then
        assert_file_exists "testcpp2: binary installed" "$BIN_DIR/testcpp2"
        assert_dir_exists  "testcpp2: exe directory created" "$EXE_DIR/testcpp2"
        assert_dir_exists  "testcpp2: src/ folder included" "$EXE_DIR/testcpp2/src"
        assert_file_exists "testcpp2: src/dbl.hpp included" "$EXE_DIR/testcpp2/src/dbl.hpp"

        ACTUAL=$("$BIN_DIR/testcpp2" cppdouble '6' 2>&1) || ACTUAL="ERROR: rc=$?"
        assert_test "testcpp2: output correct" "12" "$ACTUAL"

        morloc uninstall --program testcpp2 >/dev/null 2>&1 || true
        assert_not_exists "testcpp2: cleaned up after uninstall" "$BIN_DIR/testcpp2"

        rm -rf "$WORK_DIR"
    fi
    echo ""
fi

# --- testcpp3: direct source + indirect #include, include via --include CLI ---
if should_run "testcpp3"; then
    echo "${BOLD}[testcpp3] C++: indirect #include, include via --include CLI${RESET}"

    TEST_DIR="$SCRIPT_DIR/testcpp3"
    WORK_DIR=$(mktemp -d)
    cp -r "$TEST_DIR"/. "$WORK_DIR"/

    BUILD_ERR=$(cd "$WORK_DIR" && morloc make --install --force -o testcpp3 --include "inc.hpp" --include "offset.hpp" main.loc 2>&1) || {
        TOTAL=$((TOTAL + 1))
        printf "  %-55s " "testcpp3: build"
        printf "%sFAIL%s\n" "$RED" "$RESET"
        FAILED=$((FAILED + 1))
        FAILURES+=("testcpp3: build")
        echo "$BUILD_ERR" | tail -5 | sed 's/^/      /'
        rm -rf "$WORK_DIR"
    }

    if [[ -d "$WORK_DIR" ]]; then
        assert_file_exists "testcpp3: binary installed" "$BIN_DIR/testcpp3"
        assert_dir_exists  "testcpp3: exe directory created" "$EXE_DIR/testcpp3"
        assert_file_exists "testcpp3: inc.hpp included" "$EXE_DIR/testcpp3/inc.hpp"
        assert_file_exists "testcpp3: offset.hpp included (indirect)" "$EXE_DIR/testcpp3/offset.hpp"

        ACTUAL=$("$BIN_DIR/testcpp3" cppinc '10' 2>&1) || ACTUAL="ERROR: rc=$?"
        assert_test "testcpp3: output correct" "11" "$ACTUAL"

        morloc uninstall --program testcpp3 >/dev/null 2>&1 || true
        assert_not_exists "testcpp3: cleaned up after uninstall" "$BIN_DIR/testcpp3"

        rm -rf "$WORK_DIR"
    fi
    echo ""
fi

# ======================================================================
# R tests
# ======================================================================

# --- testr1: direct source in cwd, include file via package.yaml ---
if should_run "testr1"; then
    echo "${BOLD}[testr1] R: direct source in cwd, include via package.yaml${RESET}"

    TEST_DIR="$SCRIPT_DIR/testr1"
    WORK_DIR=$(mktemp -d)
    cp -r "$TEST_DIR"/. "$WORK_DIR"/

    BUILD_ERR=$(cd "$WORK_DIR" && morloc make --install --force -o testr1 main.loc 2>&1) || {
        TOTAL=$((TOTAL + 1))
        printf "  %-55s " "testr1: build"
        printf "%sFAIL%s\n" "$RED" "$RESET"
        FAILED=$((FAILED + 1))
        FAILURES+=("testr1: build")
        echo "$BUILD_ERR" | tail -5 | sed 's/^/      /'
        rm -rf "$WORK_DIR"
    }

    if [[ -d "$WORK_DIR" ]]; then
        assert_file_exists "testr1: binary installed" "$BIN_DIR/testr1"
        assert_dir_exists  "testr1: exe directory created" "$EXE_DIR/testr1"
        assert_dir_exists  "testr1: pools directory copied" "$EXE_DIR/testr1/pools"
        assert_file_exists "testr1: negate.R included" "$EXE_DIR/testr1/negate.R"

        ACTUAL=$("$BIN_DIR/testr1" rnegate '5.0' 2>&1) || ACTUAL="ERROR: rc=$?"
        assert_test "testr1: output correct" "-5" "$ACTUAL"

        morloc uninstall --program testr1 >/dev/null 2>&1 || true
        assert_not_exists "testr1: cleaned up after uninstall" "$BIN_DIR/testr1"

        rm -rf "$WORK_DIR"
    fi
    echo ""
fi

# --- testr2: source in src/, include whole folder via package.yaml ---
if should_run "testr2"; then
    echo "${BOLD}[testr2] R: source in src/, include folder via package.yaml${RESET}"

    TEST_DIR="$SCRIPT_DIR/testr2"
    WORK_DIR=$(mktemp -d)
    cp -r "$TEST_DIR"/. "$WORK_DIR"/

    BUILD_ERR=$(cd "$WORK_DIR" && morloc make --install --force -o testr2 main.loc 2>&1) || {
        TOTAL=$((TOTAL + 1))
        printf "  %-55s " "testr2: build"
        printf "%sFAIL%s\n" "$RED" "$RESET"
        FAILED=$((FAILED + 1))
        FAILURES+=("testr2: build")
        echo "$BUILD_ERR" | tail -5 | sed 's/^/      /'
        rm -rf "$WORK_DIR"
    }

    if [[ -d "$WORK_DIR" ]]; then
        assert_file_exists "testr2: binary installed" "$BIN_DIR/testr2"
        assert_dir_exists  "testr2: exe directory created" "$EXE_DIR/testr2"
        assert_dir_exists  "testr2: src/ folder included" "$EXE_DIR/testr2/src"
        assert_file_exists "testr2: src/triple.R included" "$EXE_DIR/testr2/src/triple.R"

        ACTUAL=$("$BIN_DIR/testr2" rtriple '4' 2>&1) || ACTUAL="ERROR: rc=$?"
        assert_test "testr2: output correct" "12" "$ACTUAL"

        morloc uninstall --program testr2 >/dev/null 2>&1 || true
        assert_not_exists "testr2: cleaned up after uninstall" "$BIN_DIR/testr2"

        rm -rf "$WORK_DIR"
    fi
    echo ""
fi

# --- testr3: direct source + indirect source(), include via --include CLI ---
if should_run "testr3"; then
    echo "${BOLD}[testr3] R: indirect source(), include via --include CLI${RESET}"

    TEST_DIR="$SCRIPT_DIR/testr3"
    WORK_DIR=$(mktemp -d)
    cp -r "$TEST_DIR"/. "$WORK_DIR"/

    BUILD_ERR=$(cd "$WORK_DIR" && morloc make --install --force -o testr3 --include "glue.R" --include "rutil.R" main.loc 2>&1) || {
        TOTAL=$((TOTAL + 1))
        printf "  %-55s " "testr3: build"
        printf "%sFAIL%s\n" "$RED" "$RESET"
        FAILED=$((FAILED + 1))
        FAILURES+=("testr3: build")
        echo "$BUILD_ERR" | tail -5 | sed 's/^/      /'
        rm -rf "$WORK_DIR"
    }

    if [[ -d "$WORK_DIR" ]]; then
        assert_file_exists "testr3: binary installed" "$BIN_DIR/testr3"
        assert_dir_exists  "testr3: exe directory created" "$EXE_DIR/testr3"
        assert_file_exists "testr3: glue.R included" "$EXE_DIR/testr3/glue.R"
        assert_file_exists "testr3: rutil.R included (indirect)" "$EXE_DIR/testr3/rutil.R"

        ACTUAL=$("$BIN_DIR/testr3" rpaste '"foo"' '"bar"' 2>&1) || ACTUAL="ERROR: rc=$?"
        assert_test "testr3: output correct" '"foobar"' "$ACTUAL"

        morloc uninstall --program testr3 >/dev/null 2>&1 || true
        assert_not_exists "testr3: cleaned up after uninstall" "$BIN_DIR/testr3"

        rm -rf "$WORK_DIR"
    fi
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
