#!/bin/sh
# Start a call that blocks inside an R worker, SIGKILL that worker, and report
# whether the call failed within a bound rather than hanging.
rm -f started.txt
./nexus slow started.txt > /dev/null 2>&1 &
nx=$!
i=0
while [ ! -s started.txt ] && [ $i -lt 400 ]; do
  sleep 0.05
  i=$((i + 1))
done
# The R pool is a child of the nexus; its workers are children of the pool.
worker=""
for pool in $(pgrep -P "$nx"); do
  for w in $(pgrep -P "$pool"); do
    worker=$w
  done
done
if [ -z "$worker" ]; then
  echo "no worker found"
  kill -KILL "$nx" 2> /dev/null
  exit 0
fi
kill -KILL "$worker"
i=0
while kill -0 "$nx" 2> /dev/null && [ $i -lt 200 ]; do
  sleep 0.05
  i=$((i + 1))
done
if kill -0 "$nx" 2> /dev/null; then
  echo "the caller hung"
  kill -KILL "$nx"
else
  wait "$nx"
  [ $? -ne 0 ] && echo "the call failed" || echo "the call succeeded"
fi
rm -f started.txt
