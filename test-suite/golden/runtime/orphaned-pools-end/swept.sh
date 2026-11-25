#!/bin/sh
# Start a nexus, whose startup sweep must remove the run directories of the
# nexuses run.sh killed, with the shared memory their markers name.
./nexus pong 1 > /dev/null
left=""
for d in $(sort -u killed-dirs.txt); do
  [ -e "$d" ] && left="$left $d"
done
if [ -n "$left" ]; then
  echo "dead runs left behind:$left"
  for d in $left; do
    case "$d" in /tmp/morloc.*) rm -rf "$d" ;; esac
  done
else
  echo "dead runs swept"
fi
rm -f killed-dirs.txt
