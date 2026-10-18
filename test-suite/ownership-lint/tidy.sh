#!/usr/bin/env bash
# clang-tidy's move and dangling checks over the C++ pools of a golden
# snapshot (MORLOC_GOLDEN_SNAPSHOT, see GoldenMakefileTests.hs).
#
#   tidy.sh SNAPDIR [JOBS]
#
# Prints each finding in generated code, and any parse error or clang-tidy
# failure, with its test name; exits 1 if there is any, or if SNAPDIR holds
# no C++ pool. Warnings inside headers are left out. Needs clang-tidy on PATH
# (or CLANG_TIDY set) and g++ for the standard headers. MORLOC_PREFIX and
# MORLOC_STATE name the install (default /opt/morloc and /opt/morloc-state).
# A pool whose test builds in a directory its clean removes, and includes a
# header from there by absolute path (paths-with-spaces), cannot be parsed
# from the snapshot and is reported as an error.
set -euo pipefail
snap=${1:?usage: tidy.sh SNAPDIR [JOBS]}
snap=${snap%/}
jobs=${2:-4}
golden=$(cd "$(dirname "$0")/../golden-tests" && pwd)
tidy=${CLANG_TIDY:-clang-tidy}
prefix=${MORLOC_PREFIX:-/opt/morloc}
state=${MORLOC_STATE:-/opt/morloc-state}
command -v "$tidy" >/dev/null || { echo "tidy.sh: $tidy not found" >&2; exit 2; }
pools=$(find "$snap" -path '*/pools/cpp/pool.cpp' | wc -l)
[ "$pools" -gt 0 ] || { echo "tidy.sh: no C++ pool under $snap" >&2; exit 2; }
gccinc=$(echo | g++ -std=c++20 -xc++ -E -v - 2>&1 \
  | sed -n '/#include <...> search starts here:/,/End of search list./p' \
  | grep '^ ' | sed 's/^ /-isystem/' | tr '\n' ' ')
export snap golden tidy prefix state gccinc
one() {
  local f=$1 t out status=0
  t=${f#"$snap"/}; t=${t%%/*}
  # $gccinc is a list of flags, split on purpose.
  # shellcheck disable=SC2086
  out=$("$tidy" --quiet \
      --checks='-*,bugprone-use-after-move,bugprone-dangling-handle,clang-analyzer-cplusplus.Move' \
      "$f" -- -std=c++20 -nostdinc++ $gccinc -include morloc_pch.hpp \
      -I"$prefix/include" -I"$state/modules/include" \
      -I"$state/modules/include/tensor-cpp" -I"$(dirname "$f")" \
      -iquote "$golden/$t" -idirafter "$golden/$t" \
      -iquote "$state/src/morloc/plane/default/root-cpp" 2>&1) || status=$?
  echo "$out" | grep -E "^$f:[0-9]+:[0-9]+: warning:|error:" | sed "s|^|$t: |" || true
  # clang-tidy exits 1 for errors it printed; anything else is a failure of
  # its own.
  if [ "$status" -gt 1 ]; then echo "$t: clang-tidy exited $status on $f"; fi
}
export -f one
found=$(find "$snap" -path '*/pools/cpp/pool.cpp' -print0 | xargs -0 -P "$jobs" -n 1 bash -c 'one "$0"')
if [ -n "$found" ]; then
  echo "$found"
  exit 1
fi
echo "tidy.sh: $pools pools, no finding"
