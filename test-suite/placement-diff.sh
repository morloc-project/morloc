#!/usr/bin/env bash
# Write the realizer's placement listing (`morloc typecheck --realize`) of
# every program under the given directories into OUT, one file per program.
# Run once before and once after a change to the realizer, then compare the
# two directories with `diff -r` to see which placements moved.
#
#   placement-diff.sh OUT [DIR...]
#
# DIR defaults to test-suite/golden-tests. The programs of a directory are
# the .loc files its Makefile passes to `morloc make`, else its main.loc,
# else its main*.loc files, else its only .loc file. Programs that fail to typecheck get a file
# holding the first line of the error, so a new failure shows in the diff
# too. GUARD, if set, prefixes each call (a memory or time limit, e.g.
# GUARD="timeout 120").

set -u

if [ $# -lt 1 ]; then
  echo "usage: $0 OUT [DIR...]" >&2
  exit 2
fi

out=$1
shift
here=$(cd "$(dirname "$0")" && pwd)
if [ $# -eq 0 ]; then
  set -- "$here/golden-tests"
fi
mkdir -p "$out"
GUARD=${GUARD:-}

programs() {
  local dir=$1 listed
  listed=$(grep -o 'morloc make [^;&|>]*\.loc' "$dir/Makefile" 2>/dev/null \
    | grep -o '[^ ]*\.loc$' | sort -u \
    | while read -r loc; do [ -e "$dir/$loc" ] && echo "$loc"; done)
  if [ -n "$listed" ]; then
    echo "$listed"
  elif [ -e "$dir/main.loc" ]; then
    echo main.loc
  elif ls "$dir"/main*.loc > /dev/null 2>&1; then
    (cd "$dir" && ls main*.loc)
  elif [ "$(find "$dir" -maxdepth 1 -name '*.loc' | wc -l)" -eq 1 ]; then
    basename "$(find "$dir" -maxdepth 1 -name '*.loc')"
  fi
}

for root in "$@"; do
  root=$(cd "$root" && pwd)
  find "$root" -name '*.loc' -not -path '*-build/*' -not -path '*/pools/*' -printf '%h\n' \
    | sort -u \
    | while read -r dir; do
        programs "$dir" | while read -r loc; do
          rel=${dir#"$root"}
          name=$(basename "$root")$(echo "$rel/$loc" | tr '/' '_')
          result=$(cd "$dir" && $GUARD morloc typecheck --realize "$loc" 2>&1 < /dev/null)
          listing=$(printf '%s\n' "$result" | sed -n '/^placement:$/,/^$/p')
          if [ -n "$listing" ]; then
            printf '%s\n' "$listing" > "$out/$name"
          else
            printf 'FAILED %s\n' "$(printf '%s\n' "$result" | head -n 1)" > "$out/$name"
          fi
        done
      done
done
