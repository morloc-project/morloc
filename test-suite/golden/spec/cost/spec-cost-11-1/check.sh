#!/usr/bin/env bash
# Run the realizer's placement check (`morloc typecheck -v --realize`,
# COST-11) on every golden program and print each one whose built placement
# costs more than the least the realizer found, or whose check hits a
# compiler bug or times out. Each test directory is checked in a private
# copy, since the suite may be running it.
here=$(cd "$(dirname "$0")" && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
check() {
  src=$1
  name=$(basename "$src")
  cp -R "$src" "$CHECK_WORK/$name" 2> /dev/null
  cd "$CHECK_WORK/$name" || return 0
  for loc in main*.loc; do
    [ -e "$loc" ] || continue
    timeout 120 morloc typecheck -v --realize "$loc" > check.out 2> check.err < /dev/null
    status=$?
    if [ "$status" -eq 124 ]; then
      echo "$name/$loc timed out"
    elif grep -q -i -E 'compiler bug|internal compiler error' check.err; then
      echo "$name/$loc compiler bug"
    elif grep -q 'cost check failed' check.out; then
      echo "$name/$loc"
    fi
  done
  cd "$CHECK_WORK" && rm -rf "$name"
}
export -f check
export CHECK_WORK="$work"
# Test dirs are golden/<group>/<test>, and golden/spec/<chapter>/<test>.
golden=$(cd "$here/../../.." && pwd)
{
  find "$golden" -mindepth 3 -maxdepth 3 -name Makefile -not -path "$golden/spec/*"
  find "$golden/spec" -mindepth 3 -maxdepth 3 -name Makefile
} | grep -v '/spec-cost-11-1/Makefile$' | sed 's|/Makefile$||' | sort |
  xargs -P 4 -I{} bash -c 'check {}' | sort
