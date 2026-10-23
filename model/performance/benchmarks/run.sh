#!/usr/bin/env bash
# Run benchmarks under memguard into one run directory, then compare that
# run with the one before it:
#   run.sh [--reps N] [--quick] [--label L] [NAME...]   (default: every benchmark)
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
opts=()
names=()
label=""
quick=""
while [ $# -gt 0 ]; do
  case $1 in
    --reps) opts+=("$1" "$2"); shift 2 ;;
    --quick) opts+=("$1"); quick=1; shift ;;
    --label) label=$2; shift 2 ;;
    *) names+=("$1"); shift ;;
  esac
done
if [ ${#names[@]} -eq 0 ]; then
  for d in "$here"/*/bench.py; do names+=("$(basename "$(dirname "$d")")"); done
fi
run=$(python3 -c "import sys; sys.path.insert(0, '$here'); import perf; print(perf.default_run_name(sys.argv[1] or None))" "$label")
for n in "${names[@]}"; do
  /work/scripts/memguard -m "${PERF_MEM_MIB:-8192}" -t "${PERF_TIMEOUT_S:-3600}" -- \
    python3 "$here/perf.py" "$n" --run "$run" "${opts[@]}"
done
[ -n "$quick" ] || python3 "$here/compare.py"
