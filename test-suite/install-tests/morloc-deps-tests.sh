#!/usr/bin/env bash
# morloc-deps-tests.sh - Integration tests for the morloc-dependencies
# package.yaml field and the closer-to-install-root-wins resolver.
#
# Each test creates fresh git fixtures in a temp dir, isolates MORLOC_HOME
# to a temp dir so the user's real cache is not touched, then runs `morloc
# install` and asserts on the resulting cache state.
#
# Usage: ./morloc-deps-tests.sh

set -euo pipefail

PASSED=0
FAILED=0
TOTAL=0

if [[ -t 1 ]]; then
    GREEN=$'\033[32m' RED=$'\033[31m' BOLD=$'\033[1m' RESET=$'\033[0m'
else
    GREEN='' RED='' BOLD='' RESET=''
fi

pass() {
    TOTAL=$((TOTAL + 1)); PASSED=$((PASSED + 1))
    printf "  %-65s %sPASS%s\n" "$1" "$GREEN" "$RESET"
}
fail() {
    TOTAL=$((TOTAL + 1)); FAILED=$((FAILED + 1))
    printf "  %-65s %sFAIL%s\n" "$1" "$RED" "$RESET"
    [[ -n "${2:-}" ]] && printf "      %s\n" "$2"
}

assert_file_contains() {
    local label="$1" file="$2" pattern="$3"
    if grep -q "$pattern" "$file" 2>/dev/null; then
        pass "$label"
    else
        fail "$label" "expected '$pattern' in $file"
    fi
}

assert_file_not_contains() {
    local label="$1" file="$2" pattern="$3"
    if grep -q "$pattern" "$file" 2>/dev/null; then
        fail "$label" "did not expect '$pattern' in $file"
    else
        pass "$label"
    fi
}

assert_install_fails() {
    local label="$1" outer_dir="$2"
    if morloc install "./$outer_dir" >/dev/null 2>&1; then
        fail "$label" "expected install to fail"
    else
        pass "$label"
    fi
}

# Set up an isolated MORLOC_HOME for these tests so we never touch the user's
# real cache. Requires the morloc binary to already be built and on PATH.
SETUP_DIR=$(mktemp -d)
export MORLOC_HOME="$SETUP_DIR/morloc-home"
mkdir -p "$MORLOC_HOME"
trap 'rm -rf "$SETUP_DIR"' EXIT

# We expect a morloc binary on PATH built from this repo.
if ! command -v morloc >/dev/null 2>&1; then
    echo "ERROR: morloc binary not found on PATH; build it before running these tests"
    exit 1
fi

echo "${BOLD}morloc-dependencies tests (MORLOC_HOME=$MORLOC_HOME)${RESET}"

# Make a git fixture with two commits of a tiny "inner" package.
# Each commit changes a marker string so we can tell which one was installed.
# Returns the absolute path of the bare repo and the two commit hashes via
# globals: FIXTURE_REPO, FIXTURE_HASH1, FIXTURE_HASH2.
make_fixture_repo() {
    local fixture_root
    fixture_root=$(mktemp -d -p "$SETUP_DIR")
    local inner="$fixture_root/inner-pkg"
    mkdir -p "$inner"
    cd "$inner"
    git init -q -b main
    git config user.email test@example.com
    git config user.name "test"

    cat > package.yaml <<'EOF'
name: inner-pkg
version: 0.1.0
EOF
    cat > main.loc <<'EOF'
module inner-pkg
-- MARKER_VERSION: 1
EOF
    git add . && git commit -q -m "v1"
    FIXTURE_HASH1=$(git rev-parse HEAD)

    sed -i 's/MARKER_VERSION: 1/MARKER_VERSION: 2/' main.loc
    git add . && git commit -q -m "v2"
    FIXTURE_HASH2=$(git rev-parse HEAD)

    FIXTURE_REPO="$inner"
    cd - >/dev/null
}

# ----------------------------------------------------------------------
# Test 1: pinned-dep installs at exact hash, not at HEAD
# ----------------------------------------------------------------------
test_pinned_dep() {
    local label="install-pinned-dep"
    rm -rf "$MORLOC_HOME"/*
    make_fixture_repo

    local outer_dir
    outer_dir=$(mktemp -d -p "$SETUP_DIR")
    cd "$outer_dir"
    cat > package.yaml <<EOF
name: outer-pkg
morloc-dependencies:
  - name: inner-pkg
    git-hash: $FIXTURE_HASH1
EOF
    cat > main.loc <<EOF
module outer-pkg
import inner-pkg
EOF

    # The dep must be installed from a local path; we redirect inner-pkg
    # to the local fixture repo via the user-source mechanism.
    if morloc install "$FIXTURE_REPO" >/dev/null 2>&1; then
        # confirm fixture installed at HEAD (hash2) when no pin overrides it
        :
    fi
    rm -rf "$MORLOC_HOME"/lib

    morloc install "$outer_dir" "$FIXTURE_REPO" >/dev/null 2>&1 || {
        fail "$label: install" "morloc install failed"
        cd - >/dev/null
        return
    }

    local installed_main
    installed_main=$(find "$MORLOC_HOME/lib" -path '*inner-pkg*main.loc' -print -quit 2>/dev/null)
    if [[ -z "$installed_main" ]]; then
        fail "$label: inner-pkg installed" "no main.loc found under $MORLOC_HOME/lib"
        cd - >/dev/null
        return
    fi
    assert_file_contains "$label: pinned hash content selected" "$installed_main" "MARKER_VERSION: 1"
    assert_file_not_contains "$label: HEAD content rejected" "$installed_main" "MARKER_VERSION: 2"

    # The fdb manifest should record the installed_hash field for inner-pkg.
    local fdb="$MORLOC_HOME/fdb/inner-pkg.module"
    if [[ -f "$fdb" ]]; then
        assert_file_contains "$label: fdb records installed_hash" "$fdb" "$FIXTURE_HASH1"
    else
        fail "$label: fdb manifest written" "$fdb missing"
    fi
    cd - >/dev/null
}

# ----------------------------------------------------------------------
# Test 2: stale cache is reconciled on install
# ----------------------------------------------------------------------
test_reconciles_stale_cache() {
    local label="install-reconciles-stale-cache"
    rm -rf "$MORLOC_HOME"/*
    make_fixture_repo

    # First install at hash2 (HEAD)
    local outer_dir
    outer_dir=$(mktemp -d -p "$SETUP_DIR")
    cd "$outer_dir"
    cat > package.yaml <<EOF
name: outer-pkg
morloc-dependencies:
  - name: inner-pkg
    git-hash: $FIXTURE_HASH2
EOF
    cat > main.loc <<EOF
module outer-pkg
import inner-pkg
EOF
    morloc install "$outer_dir" "$FIXTURE_REPO" >/dev/null 2>&1 || true

    # Now bump the pin to hash1 and re-install. The cache currently holds
    # hash2; reconciliation should force a reinstall at hash1.
    cat > package.yaml <<EOF
name: outer-pkg
morloc-dependencies:
  - name: inner-pkg
    git-hash: $FIXTURE_HASH1
EOF
    morloc install "$outer_dir" "$FIXTURE_REPO" >/dev/null 2>&1 || {
        fail "$label: re-install" "morloc install failed"
        cd - >/dev/null
        return
    }

    local installed_main
    installed_main=$(find "$MORLOC_HOME/lib" -path '*inner-pkg*main.loc' -print -quit 2>/dev/null)
    if [[ -z "$installed_main" ]]; then
        fail "$label: inner-pkg installed" "no main.loc found"
        cd - >/dev/null
        return
    fi
    assert_file_contains "$label: cache reconciled to pinned hash" "$installed_main" "MARKER_VERSION: 1"
    assert_file_not_contains "$label: stale hash replaced" "$installed_main" "MARKER_VERSION: 2"
    cd - >/dev/null
}

# ----------------------------------------------------------------------
# Test 3: same-depth conflicting pins error out
# ----------------------------------------------------------------------
test_conflicting_pins() {
    local label="install-conflicting-pins"
    rm -rf "$MORLOC_HOME"/*
    make_fixture_repo

    # Two siblings (sib-a, sib-b) at depth 1 each pin inner-pkg to a
    # different hash. The outer package imports both; resolver should
    # error at install time.
    local outer_dir sib_a_dir sib_b_dir
    outer_dir=$(mktemp -d -p "$SETUP_DIR")

    sib_a_dir="$outer_dir/sib-a"
    mkdir -p "$sib_a_dir"
    cat > "$sib_a_dir/package.yaml" <<EOF
name: sib-a
morloc-dependencies:
  - name: inner-pkg
    git-hash: $FIXTURE_HASH1
EOF
    cat > "$sib_a_dir/main.loc" <<EOF
module sib-a
import inner-pkg
EOF

    sib_b_dir="$outer_dir/sib-b"
    mkdir -p "$sib_b_dir"
    cat > "$sib_b_dir/package.yaml" <<EOF
name: sib-b
morloc-dependencies:
  - name: inner-pkg
    git-hash: $FIXTURE_HASH2
EOF
    cat > "$sib_b_dir/main.loc" <<EOF
module sib-b
import inner-pkg
EOF

    cd "$outer_dir"
    cat > package.yaml <<EOF
name: outer-pkg
EOF
    cat > main.loc <<EOF
module outer-pkg
import sib-a
import sib-b
EOF

    # Run install with both siblings supplied as user sources.
    if morloc install "$outer_dir" "$sib_a_dir" "$sib_b_dir" "$FIXTURE_REPO" >/dev/null 2>&1; then
        fail "$label: conflict raises error" "expected install to fail"
    else
        pass "$label: conflict raises error"
    fi
    cd - >/dev/null
}

# ----------------------------------------------------------------------
# Test 4: local git checkout honors the pinned hash
# ----------------------------------------------------------------------
test_local_git_hash() {
    local label="install-honors-local-git-hash"
    rm -rf "$MORLOC_HOME"/*
    make_fixture_repo

    # The fixture's HEAD is hash2. Pin to hash1; the local install should
    # check out hash1 from the local repo, not just copy the working tree.
    local outer_dir
    outer_dir=$(mktemp -d -p "$SETUP_DIR")
    cd "$outer_dir"
    cat > package.yaml <<EOF
name: outer-pkg
morloc-dependencies:
  - name: inner-pkg
    git-hash: $FIXTURE_HASH1
EOF
    cat > main.loc <<EOF
module outer-pkg
import inner-pkg
EOF
    morloc install "$outer_dir" "$FIXTURE_REPO" >/dev/null 2>&1 || {
        fail "$label: install" "morloc install failed"
        cd - >/dev/null
        return
    }

    local installed_main
    installed_main=$(find "$MORLOC_HOME/lib" -path '*inner-pkg*main.loc' -print -quit 2>/dev/null)
    if [[ -z "$installed_main" ]]; then
        fail "$label: inner-pkg installed" "no main.loc found"
        cd - >/dev/null
        return
    fi
    assert_file_contains "$label: local checkout honors hash" "$installed_main" "MARKER_VERSION: 1"
    cd - >/dev/null
}

# ----------------------------------------------------------------------
# Run
# ----------------------------------------------------------------------

test_pinned_dep
test_reconciles_stale_cache
test_conflicting_pins
test_local_git_hash

echo ""
echo "${BOLD}=== Results ===${RESET}"
echo "  Total:  $TOTAL"
echo "  ${GREEN}Passed: $PASSED${RESET}"
if (( FAILED > 0 )); then
    echo "  ${RED}Failed: $FAILED${RESET}"
    exit 1
fi
echo ""
echo "${GREEN}All morloc-dependencies tests passed${RESET}"
