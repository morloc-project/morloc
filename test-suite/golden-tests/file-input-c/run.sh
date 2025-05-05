#!/usr/bin/env bash
echo '"a"' > a.json
echo '"b"' > b.json
./nexus foo a.json b.json
./nexus foo '"a"' b.json
./nexus foo '"a"' '"b"'
./nexus foo '"a"' <(echo '"b"')
echo '"b"' | ./nexus foo '"a"' /dev/stdin
