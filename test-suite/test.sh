#!/usr/bin/env bash
# test.sh - run the shell-based integration/stress/benchmark suites.
#
# These live outside the Haskell golden/unit suite (`stack test`) because they
# drive built executables, the daemon/serving stack, or a container engine.
# Each sub-suite is a self-contained runner that exits 0 on success, nonzero on
# failure, and SKIPs internally when its prerequisites are missing.
#
# Usage: ./test.sh [suite...]
#   No args runs every suite. Filter by name:
#     ./test.sh daemon expose         # just those two
#
# Prerequisites vary by suite; most need a built morloc/nexus (`morloc init -f`).
#
# Note: `manager-tests/` is deliberately NOT aggregated here -- it tests
# mim and needs a container engine, so it is run on its own
# (`./manager-tests/run-tests.sh`), not as part of `./test.sh`.

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

if [[ -t 1 ]]; then
    GREEN=$'\033[32m' RED=$'\033[31m' BOLD=$'\033[1m' RESET=$'\033[0m'
else
    GREEN='' RED='' BOLD='' RESET=''
fi

# name -> runner path. Order is cheapest/most-fundamental first.
SUITE_NAMES=(typecheck-benchmark concurrency daemon packet-io stress expose)
declare -A SUITE_RUNNER=(
    [typecheck-benchmark]="typecheck-benchmark/run-benchmarks.sh"
    [concurrency]="concurrency-tests/run-tests.sh"
    [daemon]="daemon-tests/run-tests.sh"
    [packet-io]="packet-io/run-all.sh"
    [stress]="stress/run-all.sh"
    [expose]="expose-tests/run-tests.sh"
)

# Selection: all suites, or the names passed on the command line.
selected=("$@")
if [[ ${#selected[@]} -eq 0 ]]; then
    selected=("${SUITE_NAMES[@]}")
fi

PASSED=() FAILED=() MISSING=()

for name in "${selected[@]}"; do
    runner="${SUITE_RUNNER[$name]:-}"
    if [[ -z "$runner" ]]; then
        echo "${RED}unknown suite: $name${RESET} (known: ${SUITE_NAMES[*]})"
        MISSING+=("$name")
        continue
    fi
    if [[ ! -x "$SCRIPT_DIR/$runner" ]]; then
        echo "${RED}missing runner: $runner${RESET}"
        MISSING+=("$name")
        continue
    fi
    printf "\n%s========== %s ==========%s\n" "$BOLD" "$name" "$RESET"
    if "$SCRIPT_DIR/$runner"; then
        PASSED+=("$name")
    else
        FAILED+=("$name")
    fi
done

printf "\n%s===== summary =====%s\n" "$BOLD" "$RESET"
printf "  %spassed%s: %s\n" "$GREEN" "$RESET" "${PASSED[*]:-(none)}"
printf "  %sfailed%s: %s\n" "$RED" "$RESET" "${FAILED[*]:-(none)}"
[[ ${#MISSING[@]} -gt 0 ]] && printf "  missing: %s\n" "${MISSING[*]}"

# Suites not yet wired (no runner defined):
#   cmorloc-tests           - libmorloc C-API tests
#   error-message-tests     - compiler diagnostic wording
#   executable-benchmark    - distributed / parallel-interop / serial-interop
#   shm-tests               - ad-hoc /dev/shm fixtures (no runner)

[[ ${#FAILED[@]} -eq 0 && ${#MISSING[@]} -eq 0 ]]
