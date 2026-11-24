"""A JSON result served by a daemon over HTTP and by an MCP server over
stdio, against the CLI printing the same value (SERVE-1..3).

The program returns n reals from a C++ pool (`reals`), or a scalar
(`count`). Each point is the best of the repetitions, interleaved over
sizes. The daemon and the MCP server are started once per series, on a
free loopback port and in their own session, and stopped with SIGTERM
(the MCP server by closing its stdin); whatever is left after 20 s is
killed with its process group, so no pool outlives the run.

- SERVE-1: t = c + b*x over x result bytes, for an HTTP call and for
  `./nexus reals n` into /dev/null; `http.over` = http.b - cli.b.
- SERVE-2: wall time of one HTTP call with a scalar result, and of a
  `/health` probe that dispatches nothing, each the mean of CALLS calls
  made one after another.
- SERVE-3: the same fit for an MCP `tools/call`; `mcp.over` = mcp.b -
  http.b, and the fixed cost of a scalar MCP call.
"""

import http.client
import json
import os
import signal
import socket
import subprocess
import tempfile
import time

from perf import UNITS, Timing, fit_linear, timed

SIZES = [0, 1000000, 2000000, 4000000, 8000000]
MCP_SIZES = [0, 500000, 1000000, 2000000]
QUICK_SIZES = [0, 200000]
CALLS = 200
EMPTY = b'{"status":"ok","result":}\n'


def manifest_of(prog_dir, wrapper):
    with open(os.path.join(prog_dir, wrapper)) as f:
        for line in f:
            if line.startswith("# morloc-manifest:"):
                path = line.split(":", 1)[1].strip()
                return os.path.join(prog_dir, path)
    raise RuntimeError(f"{wrapper} names no manifest")


def free_port():
    with socket.socket() as s:
        s.bind(("127.0.0.1", 0))
        return s.getsockname()[1]


def stop_group(proc, term):
    if proc.poll() is None:
        term()
        try:
            proc.wait(timeout=20)
        except subprocess.TimeoutExpired:
            pass
    try:
        os.killpg(proc.pid, signal.SIGKILL)
    except ProcessLookupError:
        pass
    proc.wait()


class Daemon:
    def __init__(self, prog_dir, manifest, log):
        self.port = free_port()
        self.log = open(log, "wb")
        self.proc = subprocess.Popen(
            ["morloc-nexus", "daemon", manifest, "--http-port", str(self.port)],
            cwd=prog_dir, stdin=subprocess.DEVNULL, stdout=self.log,
            stderr=self.log, start_new_session=True)
        deadline = time.monotonic() + 60
        while True:
            if self.proc.poll() is not None:
                raise RuntimeError(f"daemon exited {self.proc.returncode}; see {log}")
            try:
                self.get("/health")
                return
            except OSError:
                if time.monotonic() > deadline:
                    self.stop()
                    raise RuntimeError(f"daemon did not answer; see {log}")
                time.sleep(0.05)

    def request(self, method, path, body=None):
        conn = http.client.HTTPConnection("127.0.0.1", self.port, timeout=600)
        t0 = time.perf_counter()
        conn.request(method, path, body=body)
        r = conn.getresponse()
        data = r.read()
        t1 = time.perf_counter()
        conn.close()
        if r.status != 200:
            raise RuntimeError(f"{method} {path} -> {r.status}: {data[:300]!r}")
        return t1 - t0, data

    def get(self, path):
        return self.request("GET", path)

    def call(self, command, args):
        return self.request("POST", f"/call/{command}", json.dumps(args))

    def stop(self):
        stop_group(self.proc, lambda: os.killpg(self.proc.pid, signal.SIGTERM))
        self.log.close()

    def __enter__(self):
        return self

    def __exit__(self, *_):
        self.stop()


class Mcp:
    def __init__(self, prog_dir, manifest, log):
        self.log = open(log, "wb")
        self.proc = subprocess.Popen(
            ["morloc-nexus", "mcp", manifest], cwd=prog_dir,
            stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=self.log,
            start_new_session=True)
        self.next_id = 0
        self.rpc("initialize", {"protocolVersion": "2025-06-18", "capabilities": {},
                                "clientInfo": {"name": "perf", "version": "0"}})
        self.send({"jsonrpc": "2.0", "method": "notifications/initialized"})
        _, tools = self.rpc("tools/list", {})
        self.params = {t["name"]: list(t["inputSchema"].get("properties", {}))
                       for t in tools["result"]["tools"]}

    def send(self, msg):
        self.proc.stdin.write((json.dumps(msg) + "\n").encode())
        self.proc.stdin.flush()

    def rpc(self, method, params):
        self.next_id += 1
        t0 = time.perf_counter()
        self.send({"jsonrpc": "2.0", "id": self.next_id, "method": method, "params": params})
        line = self.proc.stdout.readline()
        t1 = time.perf_counter()
        if not line:
            raise RuntimeError("the MCP server closed its output")
        return t1 - t0, json.loads(line)

    def call(self, tool, *args):
        names = self.params[tool]
        wall, resp = self.rpc("tools/call", {"name": tool, "arguments": dict(zip(names, args))})
        result = resp.get("result", {})
        if "error" in resp or result.get("isError"):
            raise RuntimeError(f"tools/call {tool} failed: {json.dumps(resp)[:300]}")
        return wall, len(result["content"][0]["text"])

    def stop(self):
        def close():
            self.proc.stdin.close()
        stop_group(self.proc, close)
        self.log.close()

    def __enter__(self):
        return self

    def __exit__(self, *_):
        self.stop()


def best_point(b, series, x, walls, **extra):
    return b.point(series, x, Timing(walls, 0, b""), load=os.getloadavg()[0], **extra)


def fit(b, item, series, xs, ys, unit="ns"):
    c, slope, r2 = fit_linear(xs, ys)
    b.record(item, f"{series}.c", c / UNITS["ms"], "ms", f"r2={r2:.4f}")
    b.record(item, f"{series}.b", slope / UNITS[unit], unit, f"r2={r2:.4f} per result byte")
    return slope


def to_null(cmd):
    return ["sh", "-c", 'exec "$@" > /dev/null', "sh"] + cmd


def measure(b, prog_dir, wrapper="nexus"):
    manifest = manifest_of(prog_dir, wrapper)
    quick = getattr(b, "quick", False)
    sizes = QUICK_SIZES if quick else SIZES
    mcp_sizes = QUICK_SIZES if quick else MCP_SIZES
    calls = 20 if quick else CALLS
    logs = tempfile.mkdtemp(prefix="perf-serve-json-")
    print(f"  daemon and MCP logs in {logs}", flush=True)

    with Daemon(prog_dir, manifest, os.path.join(logs, "daemon.log")) as d:
        http_w = {n: [] for n in sizes}
        cli_w = {n: [] for n in sizes}
        size_of = {}
        for _ in range(b.reps):
            for n in sizes:
                wall, body = d.call("reals", [n])
                http_w[n].append(wall)
                size_of[n] = len(body) - len(EMPTY)
                w, _, _ = timed(to_null([os.path.join(prog_dir, wrapper), "reals", str(n)]), prog_dir)
                cli_w[n].append(w)
        xs = [size_of[n] for n in sizes]
        hy = [best_point(b, "http", size_of[n], http_w[n], n=n) for n in sizes]
        cy = [best_point(b, "cli", size_of[n], cli_w[n], n=n) for n in sizes]
        hb = fit(b, "SERVE-1", "http", xs, hy)
        cb = fit(b, "SERVE-1", "cli", xs, cy)
        b.record("SERVE-1", "http.over", (hb - cb) * 1e9, "ns",
                 f"per result byte; {(hb - cb) * 2**30:.2f} s/GiB")

        call_w, health_w = [], []
        for _ in range(b.reps):
            t0 = time.perf_counter()
            for _ in range(calls):
                d.call("count", [1])
            call_w.append((time.perf_counter() - t0) / calls)
            t0 = time.perf_counter()
            for _ in range(calls):
                d.get("/health")
            health_w.append((time.perf_counter() - t0) / calls)
        best_point(b, "http.call", 1, call_w)
        best_point(b, "http.health", 1, health_w)
        b.record("SERVE-2", "call.c", min(call_w) * 1e6, "us", f"mean of {calls} scalar calls")
        b.record("SERVE-2", "health.c", min(health_w) * 1e6, "us", f"mean of {calls} /health probes")

        mcp_http_w = {n: [] for n in mcp_sizes}
        for _ in range(b.reps):
            for n in mcp_sizes:
                wall, body = d.call("reals", [n])
                mcp_http_w[n].append(wall)
                size_of[n] = len(body) - len(EMPTY)

    with Mcp(prog_dir, manifest, os.path.join(logs, "mcp.log")) as m:
        mcp_w = {n: [] for n in mcp_sizes}
        for _ in range(b.reps):
            for n in mcp_sizes:
                wall, _ = m.call("reals", n)
                mcp_w[n].append(wall)
        xs = [size_of[n] for n in mcp_sizes]
        my = [best_point(b, "mcp", size_of[n], mcp_w[n], n=n) for n in mcp_sizes]
        hy = [best_point(b, "mcp.http", size_of[n], mcp_http_w[n], n=n) for n in mcp_sizes]
        mb = fit(b, "SERVE-3", "mcp", xs, my)
        hb3 = fit(b, "SERVE-3", "mcp.http", xs, hy)
        b.record("SERVE-3", "mcp.over", (mb - hb3) * 1e9, "ns",
                 f"per result byte over HTTP; {(mb - hb3) * 2**30:.2f} s/GiB")
        scalar_w = []
        for _ in range(b.reps):
            t0 = time.perf_counter()
            for _ in range(calls):
                m.call("count", 1)
            scalar_w.append((time.perf_counter() - t0) / calls)
        best_point(b, "mcp.call", 1, scalar_w)
        b.record("SERVE-3", "call.c", min(scalar_w) * 1e6, "us", f"mean of {calls} scalar tools/call")


def run(b):
    b.make()
    measure(b, b.dir)
