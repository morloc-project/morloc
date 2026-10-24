#!/usr/bin/env bash
# Write the realizer's placement listing (`morloc typecheck --realize`) of
# every program under the given directories into OUT, one file per program.
# Run once before and once after a change to the realizer, then compare the
# two directories with `diff -r` to see which placements moved.
#
#   placement-diff.sh OUT [DIR...]
#
# DIR defaults to test-suite/golden-tests. A program is a main.loc, else
# each main*.loc, else the only .loc file of its directory. Programs that fail to typecheck get a
# file holding the first line of the error, so a new failure shows in the
# diff too. GUARD, if set, prefixes each call (a memory or time limit,
# e.g. GUARD="timeout 120").

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

for root in "$@"; do
  root=$(cd "$root" && pwd)
  find "$root" -name '*.loc' -not -path '*-build/*' -not -path '*/pools/*' -print \
    | sort \
    | while read -r loc; do
        dir=$(dirname "$loc")
        case "$(basename "$loc")" in
          main.loc) ;;
          main*.loc) [ -e "$dir/main.loc" ] && continue ;;
          *) [ "$(find "$dir" -maxdepth 1 -name '*.loc' | wc -l)" -eq 1 ] || continue ;;
        esac
        name=$(basename "$(dirname "$root")")_$(echo "${loc#"$root"/}" | tr '/' '_')
        result=$(cd "$dir" && $GUARD morloc typecheck --realize "$(basename "$loc")" 2>&1 < /dev/null)
        listing=$(printf '%s\n' "$result" | sed -n '/^placement:$/,/^$/p')
        if [ -n "$listing" ]; then
          printf '%s\n' "$listing" > "$out/$name"
        else
          printf 'FAILED %s\n' "$(printf '%s\n' "$result" | head -n 1)" > "$out/$name"
        fi
      done
done
