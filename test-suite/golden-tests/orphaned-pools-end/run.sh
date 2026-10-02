#!/bin/sh
# Start a run with all three pools up, SIGKILL the nexus so it cannot clean
# up, and report whether any process of the pools' groups outlives it.
label=$1
mode=$2
rm -f started.txt
MORLOC_PY_POOL=$mode ./nexus nest started.txt > /dev/null 2>&1 &
nx=$!
i=0
while [ ! -s started.txt ] && [ $i -lt 400 ]; do
  sleep 0.05
  i=$((i + 1))
done
pools=$(pgrep -P "$nx")
n=$(echo $pools | wc -w | tr -d ' ')
dirs=""
for p in $pools; do
  dirs="$dirs $(ps -o args= -p "$p" | awk '{print $(NF-1)}')"
done
kill -KILL "$nx"
wait "$nx" 2> /dev/null
i=0
while [ $i -lt 300 ]; do
  left=""
  for g in $pools; do
    pgrep -g "$g" > /dev/null && left="$left $g"
  done
  [ -z "$left" ] && break
  sleep 0.05
  i=$((i + 1))
done
if [ "$n" -ne 3 ]; then
  echo "$label: expected 3 pools, found $n"
elif [ -n "$left" ]; then
  echo "$label: pools outlived the nexus"
else
  echo "$label: pools ended"
fi
for g in $left; do
  kill -KILL -- "-$g" 2> /dev/null
done
for d in $dirs; do
  case "$d" in
    /tmp/morloc.*) rm -rf "$d" ;;
  esac
done
rm -f started.txt
