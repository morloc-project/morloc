#!/usr/bin/env bash
# Best-of-N wall time (ms) of each stream command per element layout.
# Usage: ./bench.sh [flat_n] [n] [repeats]
set -e
FLAT=${1:-32000000}
N=${2:-500000}
REPS=${3:-3}
cd "$(dirname "$0")"
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
morloc make -o nexus main.loc > /dev/null
# A failed run is reported as FAIL, never timed; its stderr goes to
# $1.err in the working directory.
best() {
  local b=999999999 s e ms
  for _ in $(seq 1 "$REPS"); do
    s=$(python3 -c 'import time; print(time.time_ns())')
    if ! ./nexus "$@" > /dev/null 2> "$1.err"; then
      echo FAIL
      return
    fi
    e=$(python3 -c 'import time; print(time.time_ns())')
    ms=$(( (e - s) / 1000000 )); [ "$ms" -lt "$b" ] && b=$ms
  done
  rm -f "$1.err"
  echo "$b"
}
printf "%-9s %9s %9s %9s %9s %9s\n" layout n write next load slice
for t in bool real str rec shape intShape; do
  n=$N; case $t in bool|real) n=$FLAT ;; esac
  f="$TMP/$t.stream"
  w=$(best "${t}Write" "$f" "$n")
  printf "%-9s %9s %9s %9s %9s %9s\n" "$t" "$n" "$w" \
    "$(best "${t}Next" "$f")" "$(best "${t}Load" "$f")" \
    "$(best "${t}Slice" "$f" $((n / 4)) $((3 * n / 4)))"
done
