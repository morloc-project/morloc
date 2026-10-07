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
pools=""
for p in $(pgrep -P "$nx"); do
  case "$(ps -o args= -p "$p")" in
    morloc-pool-pin*) ;;
    *) pools="$pools $p" ;;
  esac
done
n=$(echo $pools | wc -w | tr -d ' ')
groups=""
for p in $pools; do
  ps -o args= -p "$p" | awk '{print $(NF-1)}' >> killed-dirs.txt
  groups="$groups $(ps -o pgid= -p "$p" | tr -d ' ')"
done
kill -KILL "$nx"
wait "$nx" 2> /dev/null
i=0
while [ $i -lt 300 ]; do
  left=""
  for g in $groups; do
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
# The killed nexus's run directory and shared memory are left behind, for
# the next nexus to start to sweep (see swept.sh).
rm -f started.txt
