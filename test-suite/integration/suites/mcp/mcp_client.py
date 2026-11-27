#!/usr/bin/env python3
"""Minimal MCP stdio client for the morloc MCP test suite.

Speaks newline-delimited JSON-RPC 2.0 to a `morloc-nexus mcp <target>` server
over its stdin/stdout. The server's stderr is captured to a file so pool noise
and diagnostics never mix into assertions -- and so the fd-discipline test can
prove a stray pool `print` landed on stderr rather than the protocol stream.

A PROTOCOL VIOLATION is any non-JSON line on stdout: since the server aliases
fd 1 onto stderr before spawning pools, the protocol stream must be pure JSON.
If a stray byte ever reaches it, `recv` raises and the test fails loudly.

Verbs (each manages one server session unless noted):
  list  <target>                       handshake, then tools/list -> tools array
  call  <target> <tool> [args_json]    handshake, then one tools/call -> result
  raw   <target> [--expect-exit]       send stdin messages verbatim (NO auto
                                        handshake); print a JSON array of the
                                        responses to id-bearing messages
  jget  <expr>                         read JSON on stdin and print eval(expr)
                                        with `d` (the parsed value), plus
                                        `tools`/`t(name)`/`names` when it is a
                                        tools array (assertion helper)

All server-touching verbs print compact JSON to stdout and exit 0 on a
well-formed exchange; they exit non-zero (with a message on stderr) on a
transport/protocol failure so the bash runner can distinguish the two.
"""

import json
import os
import queue
import subprocess
import sys
import tempfile
import threading

PROTOCOL_VERSION = "2025-06-18"
DEFAULT_TIMEOUT = float(os.environ.get("MCP_TEST_TIMEOUT", "20"))


class ProtocolError(RuntimeError):
    pass


class McpServer:
    """A running `morloc-nexus mcp <target>` session."""

    def __init__(self, target, timeout=DEFAULT_TIMEOUT):
        self.timeout = timeout
        self._stderr = tempfile.NamedTemporaryFile(
            prefix="mcp-stderr-", suffix=".log", delete=False, mode="w+b"
        )
        self.stderr_path = self._stderr.name
        self.proc = subprocess.Popen(
            ["morloc-nexus", "mcp", target],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=self._stderr,
            bufsize=0,
        )
        self._q = queue.Queue()
        self._reader = threading.Thread(target=self._read_loop, daemon=True)
        self._reader.start()

    def _read_loop(self):
        # Push each raw stdout line onto the queue; None marks EOF.
        try:
            for raw in self.proc.stdout:
                self._q.put(raw)
        finally:
            self._q.put(None)

    def send(self, obj):
        line = (json.dumps(obj) + "\n").encode("utf-8")
        try:
            self.proc.stdin.write(line)
            self.proc.stdin.flush()
        except BrokenPipeError:
            raise ProtocolError("server closed stdin (process exited early)")

    def recv(self):
        """Read one JSON message. Raises on timeout, EOF, or non-JSON output."""
        try:
            raw = self._q.get(timeout=self.timeout)
        except queue.Empty:
            raise ProtocolError(
                "timed out waiting for a response after %.1fs" % self.timeout
            )
        if raw is None:
            raise ProtocolError("server closed stdout without responding")
        text = raw.decode("utf-8", "replace").strip()
        if text == "":
            return self.recv()  # tolerate blank lines
        try:
            return json.loads(text)
        except json.JSONDecodeError:
            raise ProtocolError(
                "non-JSON bytes on the protocol stream (fd-discipline "
                "violation): %r" % (text[:200],)
            )

    def request(self, method, params=None, req_id=1):
        msg = {"jsonrpc": "2.0", "id": req_id, "method": method}
        if params is not None:
            msg["params"] = params
        self.send(msg)
        # Read until we see the response matching our id (skip any interleaved
        # server-initiated messages, which this server does not send).
        while True:
            resp = self.recv()
            if resp.get("id") == req_id:
                return resp

    def notify(self, method, params=None):
        msg = {"jsonrpc": "2.0", "method": method}
        if params is not None:
            msg["params"] = params
        self.send(msg)

    def initialize(self):
        resp = self.request(
            "initialize",
            {
                "protocolVersion": PROTOCOL_VERSION,
                "capabilities": {},
                "clientInfo": {"name": "mcp-test", "version": "0"},
            },
            req_id="init",
        )
        if "result" not in resp:
            raise ProtocolError("initialize failed: %s" % json.dumps(resp))
        self.notify("notifications/initialized")
        return resp

    def close(self, expect_exit=False):
        try:
            self.proc.stdin.close()
        except Exception:
            pass
        code = None
        try:
            code = self.proc.wait(timeout=self.timeout)
        except subprocess.TimeoutExpired:
            self.proc.kill()
            self.proc.wait()
            if expect_exit:
                raise ProtocolError("server did not exit on stdin EOF")
        # Best-effort cleanup of the captured stderr log (keep it for
        # debugging when MCP_KEEP_STDERR is set).
        if not os.environ.get("MCP_KEEP_STDERR"):
            try:
                os.unlink(self.stderr_path)
            except OSError:
                pass
        return code


def _out(obj):
    sys.stdout.write(json.dumps(obj, separators=(",", ":")) + "\n")


def cmd_list(argv):
    target = argv[0]
    srv = McpServer(target)
    try:
        srv.initialize()
        resp = srv.request("tools/list", {}, req_id="list")
        if "result" not in resp:
            _out(resp)  # surface the error envelope
            return 1
        _out(resp["result"].get("tools", []))
        return 0
    finally:
        srv.close()


def cmd_call(argv):
    target, tool = argv[0], argv[1]
    args = json.loads(argv[2]) if len(argv) > 2 and argv[2] != "" else {}
    srv = McpServer(target)
    try:
        srv.initialize()
        resp = srv.request(
            "tools/call", {"name": tool, "arguments": args}, req_id="call"
        )
        if "error" in resp:
            _out({"error": resp["error"]})
            return 0  # a JSON-RPC error is a valid outcome to assert on
        _out(resp.get("result", {}))
        return 0
    finally:
        srv.close()


def cmd_raw(argv):
    expect_exit = "--expect-exit" in argv
    argv = [a for a in argv if a != "--expect-exit"]
    target = argv[0]
    srv = McpServer(target)
    responses = []
    try:
        for line in sys.stdin:
            line = line.strip()
            if not line:
                continue
            msg = json.loads(line)
            has_id = "id" in msg
            srv.send(msg)
            if has_id:
                while True:
                    resp = srv.recv()
                    if resp.get("id") == msg["id"]:
                        responses.append(resp)
                        break
        if expect_exit:
            code = srv.close(expect_exit=True)
            _out({"responses": responses, "exited": code})
        else:
            _out({"responses": responses})
        return 0
    finally:
        if not expect_exit:
            srv.close()


def cmd_jget(argv):
    expr = argv[0]
    data = json.load(sys.stdin)
    tools = data if isinstance(data, list) else data.get("tools", [])
    by_name = {t.get("name"): t for t in tools}

    def t(name):
        if name not in by_name:
            raise KeyError("no such tool: %s" % name)
        return by_name[name]

    val = eval(expr, {"__builtins__": {}}, {  # noqa: S307 - test helper
        "d": data,
        "tools": tools,
        "t": t,
        "names": sorted(by_name.keys()),
        "sorted": sorted,
        "len": len,
        "list": list,
        "str": str,
        "int": int,
    })
    if isinstance(val, (dict, list)):
        sys.stdout.write(json.dumps(val, separators=(",", ":")) + "\n")
    elif isinstance(val, bool):
        sys.stdout.write(("true" if val else "false") + "\n")
    else:
        sys.stdout.write(str(val) + "\n")
    return 0


def main():
    if len(sys.argv) < 2:
        sys.stderr.write("usage: mcp_client.py <list|call|raw|jget> ...\n")
        return 2
    verb, rest = sys.argv[1], sys.argv[2:]
    handlers = {
        "list": cmd_list,
        "call": cmd_call,
        "raw": cmd_raw,
        "jget": cmd_jget,
    }
    if verb not in handlers:
        sys.stderr.write("unknown verb: %s\n" % verb)
        return 2
    try:
        return handlers[verb](rest)
    except ProtocolError as e:
        sys.stderr.write("PROTOCOL ERROR: %s\n" % e)
        return 3


if __name__ == "__main__":
    sys.exit(main())
