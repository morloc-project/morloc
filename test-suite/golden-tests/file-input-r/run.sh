#!/usr/bin/env bash
echo '"a"' > a.json
echo '"b"' > b.json
./nexus.py foo a.json b.json
./nexus.py foo '"a"' b.json
./nexus.py foo '"a"' '"b"'
./nexus.py foo '"a"' <(echo '"b"')
echo '"b"' | ./nexus.py foo '"a"' /dev/stdin
