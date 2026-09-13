#!/usr/bin/env bash
# Cross-pool table crossing cost by pipeline depth. Prints, per command, the
# best-of-N wall time and the bytes copied into SHM (from MORLOC_ARROW_STATS).
# Usage: ./bench.sh [rows] [repeats]
set -e
ROWS=${1:-10000000}
REPS=${2:-3}
cd "$(dirname "$0")"
morloc make -o nexus main.loc > /dev/null
printf "%-8s %10s %14s\n" command best_ms bytes_copied
for cmd in direct hop1 hop2 hop3; do
  best=999999999
  for _ in $(seq 1 "$REPS"); do
    s=$(date +%s%N); ./nexus "$cmd" "$ROWS" > /dev/null 2>&1; e=$(date +%s%N)
    ms=$(( (e - s) / 1000000 )); [ "$ms" -lt "$best" ] && best=$ms
  done
  bytes=$(MORLOC_ARROW_STATS=1 ./nexus "$cmd" "$ROWS" 2>&1 >/dev/null | awk '/copied/ {s += $3} END {print s + 0}')
  printf "%-8s %10d %14d\n" "$cmd" "$best" "$bytes"
done
