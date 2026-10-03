#!/usr/bin/env bash
# Start a run that blocks inside its pool, find the run's temporary
# directory from the pool's argv, interrupt the nexus with the given
# signal, and report whether the directory survived.
sig=$1
rm -f started.txt
# Run with SIGHUP at its default even if this harness ignores it (nohup).
python3 -c 'import os, signal, sys; signal.signal(signal.SIGHUP, signal.SIG_DFL); os.execv(sys.argv[1], sys.argv[1:])' \
  ./nexus hang started.txt > /dev/null 2>&1 &
nx=$!
for _ in $(seq 1 200); do
  [ -s started.txt ] && break
  sleep 0.05
done
dir=""
for pid in $(pgrep -P "$nx"); do
  dir=$(ps -o args= -p "$pid" | tr ' ' '\n' | grep -m1 -E '^/tmp/morloc\.[A-Za-z0-9]{6}$')
  [ -n "$dir" ] && break
done
if [ -z "$dir" ] || [ ! -d "$dir" ]; then
  echo "$sig: no tmpdir found"
  kill -KILL "$nx" 2> /dev/null
  exit 0
fi
kill "-$sig" "$nx"
wait "$nx" 2> /dev/null
code=$?
if [ -e "$dir" ]; then
  echo "$sig: tmpdir left behind (exit $code)"
  rm -rf "$dir"
else
  echo "$sig: tmpdir removed (exit $code)"
fi
rm -f started.txt
