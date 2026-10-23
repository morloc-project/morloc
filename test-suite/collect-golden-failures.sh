#!/usr/bin/env bash
# Copy the outputs of the golden tests that failed into one directory, and
# print each failure's stderr and output diff.
#
# Usage: collect-golden-failures.sh [OUT_DIR]   (default: golden-failures)
#
# A golden test fails when its obs.txt differs from its exp.txt. The harness
# writes make.err for every test it runs and cleans only the tests that pass,
# so a directory with make.err and a mismatched obs.txt is a failure. Tests
# outside this run's shard never ran and have no make.err.

root="$(cd "$(dirname "$0")" && pwd)/golden-tests"
out="${1:-golden-failures}"
mkdir -p "$out"

failed=()
for dir in "$root"/*/; do
  dir="${dir%/}"
  name="$(basename "$dir")"
  [ -e "$dir/SKIP" ] && continue
  [ -e "$dir/make.err" ] || continue
  if [ -e "$dir/obs.txt" ] && [ -e "$dir/exp.txt" ] && cmp -s "$dir/obs.txt" "$dir/exp.txt"; then
    continue
  fi
  failed+=("$name")
  mkdir -p "$out/$name"
  for f in build.err obs.err make.err obs.txt exp.txt; do
    [ -e "$dir/$f" ] && cp "$dir/$f" "$out/$name/"
  done
  diff -u "$dir/exp.txt" "$dir/obs.txt" > "$out/$name/obs.diff" 2>&1
done

echo "=== ${#failed[@]} failing golden test(s) ==="
for name in "${failed[@]}"; do
  echo "  $name"
done
for name in "${failed[@]}"; do
  for f in build.err obs.err make.err obs.diff; do
    if [ -s "$out/$name/$f" ]; then
      echo "----- $name/$f -----"
      head -c 4000 "$out/$name/$f"
      echo
    fi
  done
done
exit 0
