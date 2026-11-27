#!/usr/bin/env bash
# Every program here is broken on purpose. Two kinds of case:
#
#   build cases (most): the program must fail to build with an ordinary
#   diagnostic. A case with a Makefile builds through it; one without builds
#   its main.loc, or else each of its foo*.loc, and every one must fail
#   (other .loc files are modules they import). It passes on a nonzero exit
#   with no sign that the compiler itself broke (a "compiler bug" or
#   internal error, a Haskell or Rust panic, a signal, or a timeout).
#
#   runtime cases (runtime-error-*, pool-crash-*): the program builds, and
#   its Makefile runs calls that fail, writing to obs.txt one count per call
#   of the lines that carry the expected error. Each count must be >= 1.
#
#   Either kind fails if git asks for credentials or morloc dies of an
#   uncaught exception. The suite runs without network: offline-bin/git
#   answers every remote clone as GitHub answers one for a missing
#   repository.
#
#   run-tests.sh [NAME...]     default: every case directory

set -uo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"
export PATH="$SCRIPT_DIR/offline-bin:$PATH"

if [[ $# -gt 0 ]]; then
    cases=("$@")
else
    cases=()
    for d in */; do
        d="${d%/}"
        if [[ -f "$d/Makefile" || -f "$d/main.loc" ]] || ls "$d"/foo*.loc > /dev/null 2>&1; then
            cases+=("$d")
        fi
    done
fi

broke() {
    grep -q -i -E 'compiler bug|internal (compiler )?error|panicked at|CallStack \(from HasCallStack\)|Prelude\.|^morloc: ' <<< "$1"
}

prompted() {
    grep -q -E "^Username for '" <<< "$1"
}

pass=0
fail=0
for c in ${cases[@]+"${cases[@]}"}; do
    case "$c" in
        runtime-error-*|pool-crash-*) kind=runtime ;;
        *) kind=build ;;
    esac
    if [[ -f "$c/Makefile" ]]; then
        out=$(cd "$c" && timeout 300 make --quiet 2>&1 < /dev/null)
        status=$?
    else
        # every .loc must fail: report the first that does not
        out="" status=1
        if [[ -f "$c/main.loc" ]]; then
            programs="$c/main.loc"
        else
            programs=$(ls "$c"/foo*.loc)
        fi
        for loc in $programs; do
            one=$(cd "$c" && timeout 300 morloc make -o nexus "$(basename "$loc")" 2>&1 < /dev/null)
            st=$?
            out="$out$one"
            if [[ $st -eq 0 || $st -eq 124 || $st -gt 128 ]] || broke "$one"; then
                status=$st
                out="$(basename "$loc"): $one"
                break
            fi
        done
    fi
    why=""
    if [[ $status -eq 124 ]]; then
        why="timed out"
    elif prompted "$out"; then
        why="git asked for credentials"
    elif [[ $kind == build ]]; then
        if [[ $status -eq 0 ]]; then
            why="build succeeded"
        elif [[ $status -gt 128 ]]; then
            why="killed by signal $((status - 128))"
        elif broke "$out"; then
            why="compiler failure"
        fi
    else
        counts=$(cat "$c/obs.txt" 2> /dev/null || true)
        if [[ $status -ne 0 ]]; then
            why="make failed: $(cat "$c/build.err" 2> /dev/null | head -c 300)"
        elif [[ -z "$counts" ]] || grep -q -x '0' <<< "$counts"; then
            why="a call did not report its error (counts: $(echo $counts))"
        fi
    fi
    if [[ -f "$c/Makefile" ]]; then
        (cd "$c" && make --quiet clean > /dev/null 2>&1 || true)
    else
        rm -rf "$c/nexus" "$c"/*-build "$c"/*build.tmp* "$c/__pycache__"
    fi
    if [[ -z "$why" ]]; then
        pass=$((pass + 1))
    else
        fail=$((fail + 1))
        echo "FAIL $c: $why"
        [[ $kind == build ]] && head -c 1500 <<< "$out" | sed 's/^/    /'
    fi
done

echo "errors: $pass passed, $fail failed"
[[ $fail -eq 0 ]]
