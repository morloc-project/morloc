#!/usr/bin/env python3
"""Minimal MCP-over-HTTP client for the mim serving test suite.

The serving front-end exposes MCP as JSON-RPC over `POST /mcp` with a session
handshake (unlike `morloc-nexus mcp`, which is stdio -- see mcp-tests/mcp_client.py).
This driver performs the initialize -> initialized handshake, carrying the
`Mcp-Session-Id` returned by initialize, then issues one request.

Usage:
  mcp_http.py list  --url URL [--token T]          # print tool names, one per line
  mcp_http.py call  --url URL --tool NAME [--args JSON] [--token T]
  mcp_http.py raw   --url URL --method M [--params JSON] [--token T]   # print raw JSON result

Only the standard library is used (urllib), so no pip install is required.
"""
import argparse
import json
import sys
import urllib.request
import urllib.error

PROTOCOL_VERSION = "2025-06-18"


class McpHttp:
    def __init__(self, url, token=None, timeout=30):
        self.url = url.rstrip("/")
        self.token = token
        self.timeout = timeout
        self.session_id = None

    def _post(self, body):
        data = json.dumps(body).encode("utf-8")
        req = urllib.request.Request(self.url, data=data, method="POST")
        req.add_header("Content-Type", "application/json")
        if self.token:
            req.add_header("Authorization", "Bearer " + self.token)
        if self.session_id:
            req.add_header("Mcp-Session-Id", self.session_id)
        try:
            resp = urllib.request.urlopen(req, timeout=self.timeout)
        except urllib.error.HTTPError as e:
            # Surface the status + body so the test can assert on 401 etc.
            raise SystemExit("HTTP %d: %s" % (e.code, e.read().decode("utf-8", "replace")))
        sid = resp.headers.get("Mcp-Session-Id")
        if sid:
            self.session_id = sid
        raw = resp.read().decode("utf-8", "replace")
        status = resp.getcode()
        return status, raw

    def request(self, method, params=None, req_id=1):
        body = {"jsonrpc": "2.0", "id": req_id, "method": method}
        if params is not None:
            body["params"] = params
        _status, raw = self._post(body)
        return json.loads(raw) if raw.strip() else {}

    def notify(self, method, params=None):
        body = {"jsonrpc": "2.0", "method": method}
        if params is not None:
            body["params"] = params
        self._post(body)

    def handshake(self):
        self.request(
            "initialize",
            {"protocolVersion": PROTOCOL_VERSION, "capabilities": {}, "clientInfo": {"name": "test"}},
        )
        self.notify("notifications/initialized")


def cmd_list(c):
    resp = c.request("tools/list")
    tools = resp.get("result", {}).get("tools", [])
    for t in tools:
        print(t.get("name", ""))


def cmd_call(c, tool, args):
    arguments = json.loads(args) if args else {}
    resp = c.request("tools/call", {"name": tool, "arguments": arguments})
    result = resp.get("result", resp)
    # Prefer human-readable text content when present.
    content = result.get("content") if isinstance(result, dict) else None
    if isinstance(content, list) and content and content[0].get("type") == "text":
        print(content[0].get("text", ""))
    else:
        print(json.dumps(result))


def cmd_raw(c, method, params):
    p = json.loads(params) if params else None
    print(json.dumps(c.request(method, p)))


def main():
    ap = argparse.ArgumentParser()
    sub = ap.add_subparsers(dest="cmd", required=True)
    for name in ("list", "call", "raw"):
        s = sub.add_parser(name)
        s.add_argument("--url", required=True)
        s.add_argument("--token", default=None)
        if name == "call":
            s.add_argument("--tool", required=True)
            s.add_argument("--args", default="")
        if name == "raw":
            s.add_argument("--method", required=True)
            s.add_argument("--params", default="")
    a = ap.parse_args()

    c = McpHttp(a.url, token=a.token)
    c.handshake()
    if a.cmd == "list":
        cmd_list(c)
    elif a.cmd == "call":
        cmd_call(c, a.tool, a.args)
    elif a.cmd == "raw":
        cmd_raw(c, a.method, a.params)


if __name__ == "__main__":
    main()
