"""pfilter-move: a stream of records filtered batch by batch with
pfilterWith. `filt` gives each pulled batch to the filter, which may then
compact it in place; `filtLive` reads the batch again after the call, so it
must copy every survivor. Each run prints the records kept and the
allocations made inside the batch function; the series are interleaved and
the median is used."""

import json
import os
import subprocess

N, LEN = 100000, 4000
QUICK_N = 10000
SERIES = [("hi", 100), ("lo", 1)]
CMDS = [("move", "filt"), ("ctl", "filtLive")]


def run(b):
    b.make(loc="main.loc", out="nexus")
    n = QUICK_N if b.quick else N
    data = b.path("recs.dat")
    with open(data, "wb") as f:
        subprocess.run(["./nexus", "-f", "packet", "gen", str(n), str(LEN)], cwd=b.dir, stdout=f, check=True)
    try:
        xs = [f"{s}.{v}" for s, _ in SERIES for v, _ in CMDS]
        pct = dict(SERIES)
        cmd = dict(CMDS)

        def cmd_of(x):
            s, v = x.split(".")
            return ["./nexus", cmd[v], "1", str(pct[s]), data]

        b.sweep("pfilter", xs, cmd_of)
        med = {p["x"]: p for p in b.points if p["series"] == "pfilter"}
        for s, _ in SERIES:
            out = {}
            for v, _ in CMDS:
                kept, allocs = json.loads(subprocess.run(cmd_of(f"{s}.{v}"), cwd=b.dir, capture_output=True, check=True).stdout)
                out[v] = (kept, allocs)
                b.record("COPY-9", f"{s}.{v}.allocs_per_kept", allocs / max(kept, 1), "allocs", f"{kept} kept of {n}")
            for key in ("median_s", "cpu_s"):
                m = float(med[f"{s}.move"][key])
                c = float(med[f"{s}.ctl"][key])
                name = "wall" if key == "median_s" else "cpu"
                b.record("COPY-9", f"{s}.move.{name}", m, "s", "median")
                b.record("COPY-9", f"{s}.saved.{name}", c - m, "s", "control minus move, median")
    finally:
        os.remove(data)
