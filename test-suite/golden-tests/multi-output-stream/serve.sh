#!/bin/sh
# Serve the Python build over HTTP and MCP; print each response's gist.
manifest=nexus-py-build/manifest.json
rm -f port.txt
morloc-nexus daemon $manifest --http-port 0 --port-file port.txt > /dev/null 2>&1 &
daemon=$!
i=0
while [ ! -s port.txt ] && [ $i -lt 100 ]; do sleep 0.1; i=$((i + 1)); done
port=$(python3 -c 'import json; print(json.load(open("port.txt"))["http"])')
for q in 'stream?render=all' 'twoSites?render=all'; do
  curl -s -X POST "localhost:$port/call/$q" -d '["log.txt"]' |
    python3 -c 'import json,sys; r=json.load(sys.stdin); print(r["status"], r.get("result", r.get("error")).strip())'
done
kill $daemon
wait $daemon 2> /dev/null
printf '%s\n' \
  '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"t","version":"1"}}}' \
  '{"jsonrpc":"2.0","method":"notifications/initialized"}' \
  '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"twoSites","arguments":{"_1":"log.txt","_render":"all"}}}' |
  morloc-nexus mcp $manifest 2> /dev/null |
  python3 -c 'import json,sys; [print(json.loads(l)["error"]["message"]) for l in sys.stdin if "error" in json.loads(l)]'
